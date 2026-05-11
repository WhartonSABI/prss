suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

set.seed(42)

# =============================================================================
# E[TBC] — Expected Time Before Contact with QB
#
# For each pass rusher on each frame, compute the expected number of seconds
# until that rusher physically reaches the QB, given current positions and
# velocity vectors.
#
#   d                = || (x_q, y_q) - (x_r, y_r) ||
#   v_rel            = (vx_r - vx_q, vy_r - vy_q)
#   u_to_qb          = unit vector from rusher to QB
#   closing_speed    = v_rel . u_to_qb              [yards / second]
#   E[TBC]           = d / closing_speed            [seconds]
#
# If closing_speed <= 0 (rusher not approaching), E[TBC] = +Inf.
# If d <= CONTACT_RADIUS, E[TBC] = 0 (contact already made).
#
# Direction convention (NFL Big Data Bowl 2022):
#   `dir` is in degrees, 0 = +y (downfield), measured clockwise.
#   => vx = s * sin(dir_rad), vy = s * cos(dir_rad)
# =============================================================================

root <- getwd()
out_csv <- file.path(root, "data", "processed", "etbc_per_rusher_frame.csv")

# --- Tunable parameters ------------------------------------------------------
CONTACT_RADIUS  <- 1.0    # yards — distance at which we declare "contact"
MAX_ETBC        <- 10.0   # seconds — cap for plotting / aggregation sanity
SAMPLE_N        <- Inf    # Inf = use all valid plays (production run)
FPS             <- 10     # tracking data frames per second
# -----------------------------------------------------------------------------

# --- Load data ----------------------------------------------------------------
cat("Loading tracking data...\n")
tracking <- read_parquet(
  file.path(root, "data", "processed", "bdb-tracking-all.parquet")
) %>%
  filter(!is.na(nflId), !is.na(x), !is.na(y))

players <- read_csv(
  file.path(root, "data", "bdb", "players.csv"),
  show_col_types = FALSE
) %>%
  select(nflId, officialPosition, displayName)

pff <- read_csv(
  file.path(root, "data", "bdb", "pffScoutingData.csv"),
  show_col_types = FALSE
)

tracking <- tracking %>% left_join(players, by = "nflId")

# --- Snap & release event tags (match qb_cell_measurement.R) ------------------
SNAP_EVENTS    <- c("ball_snap", "autoevent_ballsnap")
RELEASE_EVENTS <- c(
  "pass_forward", "autoevent_passforward", "autoevent_passinterrupted",
  "qb_sack", "qb_strip_sack", "qb_spike", "run"
)

# --- Identify pass rushers per play ------------------------------------------
pass_rushers <- pff %>%
  filter(pff_role == "Pass Rush", !is.na(nflId)) %>%
  distinct(gameId, playId, nflId) %>%
  rename(rusher_nflId = nflId)

# --- Identify QB per play (the lone Pass role with QB position) --------------
qb_per_play <- tracking %>%
  filter(officialPosition == "QB") %>%
  distinct(gameId, playId, nflId) %>%
  rename(qb_nflId = nflId)

# --- Helper: convert (s, dir_deg) into velocity components -------------------
to_velocity <- function(s, dir_deg) {
  rad <- dir_deg * pi / 180
  list(vx = s * sin(rad), vy = s * cos(rad))
}

# --- Identify valid plays (have both snap and release frames) ----------------
cat("Identifying valid plays...\n")
event_frames <- tracking %>%
  filter(event %in% c(SNAP_EVENTS, RELEASE_EVENTS)) %>%
  group_by(gameId, playId) %>%
  summarise(
    snap_frame    = suppressWarnings(min(frameId[event %in% SNAP_EVENTS],    na.rm = TRUE)),
    release_frame = suppressWarnings(min(frameId[event %in% RELEASE_EVENTS], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  filter(is.finite(snap_frame), is.finite(release_frame), release_frame >= snap_frame)

valid_plays <- event_frames %>%
  inner_join(qb_per_play,    by = c("gameId", "playId")) %>%
  inner_join(
    pass_rushers %>% group_by(gameId, playId) %>% summarise(n_rushers = n(), .groups = "drop"),
    by = c("gameId", "playId")
  ) %>%
  filter(n_rushers > 0)

cat("Valid plays available:", nrow(valid_plays), "\n")

sampled_plays <- if (is.infinite(SAMPLE_N)) {
  valid_plays
} else {
  valid_plays %>% slice_sample(n = min(SAMPLE_N, nrow(valid_plays)))
}

cat("Plays to process:", nrow(sampled_plays), "\n")

# =============================================================================
# Main computation: per rusher, per frame, compute E[TBC]
# =============================================================================
compute_etbc_for_play <- function(game_id, play_id, snap_f, release_f, qb_id) {

  rusher_ids <- pass_rushers %>%
    filter(gameId == game_id, playId == play_id) %>%
    pull(rusher_nflId)

  if (length(rusher_ids) == 0) return(NULL)

  play_df <- tracking %>%
    filter(
      gameId == game_id, playId == play_id,
      frameId >= snap_f, frameId <= release_f,
      nflId %in% c(qb_id, rusher_ids)
    ) %>%
    select(gameId, playId, frameId, nflId, x, y, s, dir, displayName, officialPosition)

  if (nrow(play_df) == 0) return(NULL)

  qb_df <- play_df %>%
    filter(nflId == qb_id) %>%
    mutate(qb_vx = s * sin(dir * pi / 180),
           qb_vy = s * cos(dir * pi / 180)) %>%
    select(frameId, qb_x = x, qb_y = y, qb_vx, qb_vy)

  rusher_df <- play_df %>%
    filter(nflId %in% rusher_ids) %>%
    mutate(r_vx = s * sin(dir * pi / 180),
           r_vy = s * cos(dir * pi / 180)) %>%
    select(gameId, playId, frameId, rusher_nflId = nflId,
           rusher_name = displayName, rusher_pos = officialPosition,
           r_x = x, r_y = y, r_vx, r_vy)

  joined <- rusher_df %>% inner_join(qb_df, by = "frameId")

  joined %>%
    mutate(
      dx          = qb_x - r_x,
      dy          = qb_y - r_y,
      d           = sqrt(dx * dx + dy * dy),
      ux          = ifelse(d > 0, dx / d, 0),
      uy          = ifelse(d > 0, dy / d, 0),
      rel_vx      = r_vx - qb_vx,
      rel_vy      = r_vy - qb_vy,
      closing_spd = rel_vx * ux + rel_vy * uy,        # yards/sec along rusher->QB
      etbc_raw    = case_when(
        d <= CONTACT_RADIUS ~ 0,
        closing_spd <= 0    ~ Inf,
        TRUE                ~ d / closing_spd
      ),
      etbc        = pmin(etbc_raw, MAX_ETBC),
      frames_since_snap = frameId - snap_f
    ) %>%
    select(gameId, playId, frameId, frames_since_snap,
           rusher_nflId, rusher_name, rusher_pos,
           d, closing_spd, etbc_raw, etbc)
}

cat("Computing E[TBC] per rusher per frame...\n")
etbc_data <- pmap_dfr(
  list(sampled_plays$gameId, sampled_plays$playId,
       sampled_plays$snap_frame, sampled_plays$release_frame,
       sampled_plays$qb_nflId),
  compute_etbc_for_play
)

cat("Total rusher-frame rows:", nrow(etbc_data), "\n")

# =============================================================================
# Per-rusher summary (the player ranking output)
# =============================================================================
rusher_ranking <- etbc_data %>%
  filter(is.finite(etbc_raw)) %>%
  group_by(rusher_nflId, rusher_name, rusher_pos) %>%
  summarise(
    n_frames        = n(),
    n_plays         = n_distinct(paste(gameId, playId)),
    median_etbc     = median(etbc, na.rm = TRUE),
    mean_etbc       = mean(etbc, na.rm = TRUE),
    min_etbc        = min(etbc, na.rm = TRUE),
    pct_under_2sec  = mean(etbc < 2.0, na.rm = TRUE),
    pct_under_1sec  = mean(etbc < 1.0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_plays >= 5) %>%   # require minimum sample size
  arrange(median_etbc)

cat("\nTop 20 rushers by lowest median E[TBC]:\n")
print(rusher_ranking %>% head(20))

# --- Save outputs -------------------------------------------------------------
write_csv(etbc_data,      out_csv)
write_csv(
  rusher_ranking,
  file.path(root, "data", "processed", "etbc_rusher_ranking.csv")
)

cat("\nWrote:\n  ", out_csv, "\n  ",
    file.path(root, "data", "processed", "etbc_rusher_ranking.csv"), "\n")

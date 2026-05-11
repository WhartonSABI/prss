suppressPackageStartupMessages({
  library(tidyverse)
})

# =============================================================================
# E[TBC] — Blocker (OL) Grading
#
# For each pass blocker, attribute the E[TBC] of the rusher they were assigned
# to block (PFF's pff_nflIdBlockedPlayer). Higher average E[TBC] = the blocker
# kept their assigned rusher further from the QB on average = better blocker.
#
# Inputs:
#   data/processed/etbc_per_rusher_frame.csv   (from etbc_measurement.R)
#   data/bdb/pffScoutingData.csv
#   data/bdb/players.csv
#
# Outputs:
#   data/processed/etbc_blocker_ranking.csv
#   data/processed/etbc_blocker_per_play.csv
# =============================================================================

root <- getwd()

cat("Loading inputs...\n")
etbc <- read_csv(
  file.path(root, "data", "processed", "etbc_per_rusher_frame.csv"),
  show_col_types = FALSE
)

pff <- read_csv(
  file.path(root, "data", "bdb", "pffScoutingData.csv"),
  show_col_types = FALSE
)

players <- read_csv(
  file.path(root, "data", "bdb", "players.csv"),
  show_col_types = FALSE
) %>%
  select(nflId, displayName, officialPosition)

# --- Blocker -> Rusher assignment per play -----------------------------------
# Each row: one blocker on one play, with the rusher they were assigned to
blocker_assignments <- pff %>%
  filter(pff_role == "Pass Block",
         !is.na(nflId),
         !is.na(pff_nflIdBlockedPlayer)) %>%
  select(gameId, playId,
         blocker_nflId      = nflId,
         rusher_nflId       = pff_nflIdBlockedPlayer,
         pff_blockType,
         pff_hitAllowed,
         pff_hurryAllowed,
         pff_sackAllowed,
         pff_beatenByDefender)

# Attach blocker identity
blocker_assignments <- blocker_assignments %>%
  left_join(
    players %>% rename(blocker_name = displayName, blocker_pos = officialPosition),
    by = c("blocker_nflId" = "nflId")
  )

cat("Blocker-rusher assignment rows:", nrow(blocker_assignments), "\n")

# --- Join E[TBC] frames onto blocker assignments -----------------------------
# A blocker's E[TBC] for a frame = the E[TBC] of their assigned rusher in that
# frame on that play.
blocker_frames <- blocker_assignments %>%
  inner_join(
    etbc %>% select(gameId, playId, frameId, frames_since_snap,
                    rusher_nflId, etbc, etbc_raw, d, closing_spd),
    by = c("gameId", "playId", "rusher_nflId"),
    relationship = "many-to-many"
  )

cat("Blocker-frame rows:", nrow(blocker_frames), "\n")

# =============================================================================
# Per-play blocker summary
# =============================================================================
blocker_per_play <- blocker_frames %>%
  filter(is.finite(etbc_raw)) %>%
  group_by(gameId, playId, blocker_nflId, blocker_name, blocker_pos,
           rusher_nflId) %>%
  summarise(
    n_frames        = n(),
    median_etbc     = median(etbc, na.rm = TRUE),
    mean_etbc       = mean(etbc,   na.rm = TRUE),
    min_etbc        = min(etbc,    na.rm = TRUE),
    pressure_allowed = as.integer(any(c(pff_hitAllowed, pff_hurryAllowed,
                                        pff_sackAllowed) == 1, na.rm = TRUE)),
    .groups = "drop"
  )

cat("Blocker-play rows:", nrow(blocker_per_play), "\n")

# =============================================================================
# Per-blocker ranking (the OL grade)
# =============================================================================
blocker_ranking <- blocker_per_play %>%
  group_by(blocker_nflId, blocker_name, blocker_pos) %>%
  summarise(
    n_plays            = n(),
    n_frames           = sum(n_frames),
    median_etbc        = median(median_etbc, na.rm = TRUE),
    mean_etbc          = mean(mean_etbc,     na.rm = TRUE),
    pct_pressure_allow = mean(pressure_allowed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_plays >= 5) %>%   # minimum sample size
  arrange(desc(median_etbc))   # highest E[TBC] = best blocker

# Filter to OL positions for the headline ranking
ol_positions <- c("T", "G", "C", "OT", "OG")
ol_ranking <- blocker_ranking %>%
  filter(blocker_pos %in% ol_positions)

cat("\nTop 20 OL by highest median E[TBC] (best protectors):\n")
print(ol_ranking %>% head(20))

cat("\nBottom 10 OL by lowest median E[TBC] (worst protectors):\n")
print(ol_ranking %>% arrange(median_etbc) %>% head(10))

# --- Save outputs -------------------------------------------------------------
write_csv(blocker_per_play,
          file.path(root, "data", "processed", "etbc_blocker_per_play.csv"))
write_csv(blocker_ranking,
          file.path(root, "data", "processed", "etbc_blocker_ranking.csv"))

cat("\nWrote:\n  ",
    file.path(root, "data", "processed", "etbc_blocker_per_play.csv"), "\n  ",
    file.path(root, "data", "processed", "etbc_blocker_ranking.csv"), "\n")

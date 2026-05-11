suppressPackageStartupMessages({
  library(tidyverse)
  library(ggrepel)
})

# =============================================================================
# E[TBC] vs PFF Pressure-Allowed — Head-to-Head Comparison
#
# Question: Does our geometric grade agree with PFF's subjective grade?
#
# Our metric:    median_etbc (higher = better blocker)
# PFF metric:    pct_pressure_allow (lower = better blocker)
# Expected:      strong NEGATIVE correlation if they agree
#
# The interesting cases are the disagreements:
#   - High E[TBC] + high PFF pressure-allowed = blocker who keeps rusher far
#     but still gives up pressure (maybe scheme issue, or PFF over-credits)
#   - Low E[TBC] + low PFF pressure-allowed = blocker who lets rusher close
#     but PFF doesn't ding them (maybe rusher gets re-routed, or PFF is lenient)
#
# Inputs:
#   data/processed/etbc_blocker_ranking.csv
#
# Outputs:
#   data/processed/etbc_vs_pff_comparison.csv
#   data/processed/etbc_vs_pff_scatter.png
# =============================================================================

root <- getwd()

cat("Loading blocker ranking...\n")
blocker_ranking <- read_csv(
  file.path(root, "data", "processed", "etbc_blocker_ranking.csv"),
  show_col_types = FALSE
)

# Filter to OL only with a meaningful sample
ol_positions <- c("T", "G", "C", "OT", "OG")
ol_grades <- blocker_ranking %>%
  filter(blocker_pos %in% ol_positions, n_plays >= 10) %>%
  mutate(
    # Z-score within position to remove positional bias
    etbc_z = ave(median_etbc,        blocker_pos, FUN = function(v) (v - mean(v)) / sd(v)),
    pff_z  = ave(pct_pressure_allow, blocker_pos, FUN = function(v) (v - mean(v)) / sd(v)),
    # Combined "agreement" score: both metrics rank player similarly?
    # Negate pff_z so higher = better on both
    pff_z_inv = -pff_z,
    disagreement = abs(etbc_z - pff_z_inv)
  ) %>%
  arrange(desc(disagreement))

cat("OL with >= 10 plays:", nrow(ol_grades), "\n\n")

# =============================================================================
# Correlation
# =============================================================================
cor_overall <- cor(ol_grades$median_etbc,
                   ol_grades$pct_pressure_allow,
                   method = "spearman", use = "pairwise.complete.obs")

cor_by_pos <- ol_grades %>%
  group_by(blocker_pos) %>%
  summarise(
    n         = n(),
    spearman  = cor(median_etbc, pct_pressure_allow, method = "spearman"),
    pearson   = cor(median_etbc, pct_pressure_allow, method = "pearson"),
    .groups = "drop"
  )

cat("Overall Spearman correlation (E[TBC] vs PFF pressure-allowed):",
    round(cor_overall, 3), "\n")
cat("(Negative correlation = metrics agree)\n\n")

cat("Correlation by position:\n")
print(cor_by_pos)
cat("\n")

# =============================================================================
# Identify the most interesting disagreements
# =============================================================================
cat("Top 10 DISAGREEMENTS (our metric and PFF rank these very differently):\n")
print(
  ol_grades %>%
    select(blocker_name, blocker_pos, n_plays,
           median_etbc, pct_pressure_allow, etbc_z, pff_z_inv, disagreement) %>%
    head(10)
)
cat("\n")

cat("Top 10 AGREEMENTS (both metrics agree this player is good):\n")
print(
  ol_grades %>%
    filter(etbc_z > 0, pff_z_inv > 0) %>%
    arrange(desc(etbc_z + pff_z_inv)) %>%
    select(blocker_name, blocker_pos, n_plays,
           median_etbc, pct_pressure_allow, etbc_z, pff_z_inv) %>%
    head(10)
)
cat("\n")

# =============================================================================
# Scatter plot — labelled
# =============================================================================
# Label the most disagreeing players + the most-extreme on each axis
to_label <- ol_grades %>%
  mutate(extremeness = pmax(abs(etbc_z), abs(pff_z))) %>%
  arrange(desc(disagreement + extremeness)) %>%
  head(15)

p <- ggplot(ol_grades,
            aes(x = pct_pressure_allow, y = median_etbc, color = blocker_pos)) +
  geom_point(aes(size = n_plays), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40",
              linetype = "dashed", linewidth = 0.5,
              inherit.aes = FALSE,
              aes(x = pct_pressure_allow, y = median_etbc)) +
  geom_text_repel(data = to_label,
                  aes(label = blocker_name),
                  size = 3, max.overlaps = Inf, box.padding = 0.35,
                  segment.color = "grey60", segment.size = 0.3) +
  scale_color_manual(values = c("T" = "#1f77b4", "G" = "#2ca02c", "C" = "#d62728")) +
  scale_size_continuous(range = c(2, 7)) +
  labs(
    title    = "E[TBC] vs PFF Pressure-Allowed Rate",
    subtitle = sprintf("Spearman r = %.3f | n = %d offensive linemen with ≥ 10 plays",
                       cor_overall, nrow(ol_grades)),
    x        = "PFF pressure-allowed rate (lower = better)",
    y        = "Median E[TBC], seconds (higher = better)",
    color    = "Position",
    size     = "Plays",
    caption  = "Bottom-right quadrant = both agree player is bad. Top-left = both agree player is good. Off-diagonal = disagreement."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

out_png <- file.path(root, "data", "processed", "etbc_vs_pff_scatter.png")
ggsave(out_png, p, width = 10, height = 7, dpi = 150)

# Save comparison CSV
write_csv(
  ol_grades %>%
    select(blocker_nflId, blocker_name, blocker_pos, n_plays, n_frames,
           median_etbc, pct_pressure_allow, etbc_z, pff_z_inv, disagreement),
  file.path(root, "data", "processed", "etbc_vs_pff_comparison.csv")
)

cat("Wrote:\n  ",
    file.path(root, "data", "processed", "etbc_vs_pff_comparison.csv"), "\n  ",
    out_png, "\n")

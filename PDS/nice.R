# install.packages(c("ggplot2", "tidyr", "dplyr"))

library(ggplot2)
library(tidyr)
library(dplyr)

colors_vec <- c("White","Black","Brown","Pink","Red","Yellow",
                "Green","Orange","Light Blue","Dark Blue")

exp1_survivors <- c(0,16,8,0,4,0,24,12,12,24)
exp2_survivors <- c(12,4,4,16,20,16,4,4,8,12)

# ── Overall chi-square tests ──────────────────
p1_overall <- chisq.test(exp1_survivors)$p.value
p2_overall <- chisq.test(exp2_survivors)$p.value

cat(sprintf("Experiment 1 p-value: %.4f\n", p1_overall))
cat(sprintf("Experiment 2 p-value: %.4f\n\n", p2_overall))

# ── Post hoc console output ───────────────────
posthoc_print <- function(counts, colors, exp_label) {
  n     <- length(counts)
  pairs <- combn(n, 2)
  raw_p <- apply(pairs, 2, function(idx) {
    mat <- matrix(c(counts[idx[1]], counts[idx[2]],
                    sum(counts) - counts[idx[1]],
                    sum(counts) - counts[idx[2]]), nrow = 2)
    suppressWarnings(chisq.test(mat)$p.value)
  })
  adj_p <- p.adjust(raw_p, method = "bonferroni")

  cat(paste0("--- ", exp_label, " Post Hoc (Bonferroni corrected) ---\n"))
  cat(sprintf("%-14s %-14s %10s %10s %s\n", "Color A", "Color B", "raw p", "adj p", "sig"))
  cat(strrep("-", 58), "\n")
  for (i in seq_along(raw_p)) {
    sig <- ifelse(adj_p[i] < 0.001, "***",
           ifelse(adj_p[i] < 0.01,  "**",
           ifelse(adj_p[i] < 0.05,  "*", "")))
    cat(sprintf("%-14s %-14s %10.4f %10.4f %s\n",
                colors[pairs[1,i]], colors[pairs[2,i]],
                raw_p[i], adj_p[i], sig))
  }
  cat("\nSignificance: *** p<0.001  ** p<0.01  * p<0.05\n\n")
}

posthoc_print(exp1_survivors, colors_vec, "Experiment 1")
posthoc_print(exp2_survivors, colors_vec, "Experiment 2")

# ── Post hoc heatmap ─────────────────────────
posthoc_heatmap <- function(counts, colors, title_label, filename) {
  n     <- length(counts)
  pairs <- combn(n, 2)
  raw_p <- apply(pairs, 2, function(idx) {
    mat <- matrix(c(counts[idx[1]], counts[idx[2]],
                    sum(counts) - counts[idx[1]],
                    sum(counts) - counts[idx[2]]), nrow = 2)
    suppressWarnings(chisq.test(mat)$p.value)
  })
  adj_p <- p.adjust(raw_p, method = "bonferroni")

  mat_p <- matrix(NA, n, n, dimnames = list(colors, colors))
  for (i in seq_along(adj_p)) {
    mat_p[pairs[1,i], pairs[2,i]] <- adj_p[i]
    mat_p[pairs[2,i], pairs[1,i]] <- adj_p[i]
  }

  df <- as.data.frame(as.table(mat_p))
  colnames(df) <- c("ColorA", "ColorB", "adj_p")
  df$ColorA <- factor(df$ColorA, levels = colors)
  df$ColorB <- factor(df$ColorB, levels = rev(colors))
  df$sig    <- ifelse(is.na(df$adj_p), "",
               ifelse(df$adj_p < 0.001, "***",
               ifelse(df$adj_p < 0.01,  "**",
               ifelse(df$adj_p < 0.05,  "*", "ns"))))
  df$label  <- ifelse(is.na(df$adj_p), "",
               ifelse(df$adj_p < 0.05,
                      paste0(sprintf("%.3f", df$adj_p), "\n", df$sig),
                      sprintf("%.3f", df$adj_p)))

  ph <- ggplot(df, aes(x = ColorA, y = ColorB, fill = adj_p)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label), size = 2.8, color = "grey20", lineheight = 1.2) +
    scale_fill_gradientn(
      colours  = c("#D94F3D", "#F5C842", "#f0f0f0"),
      values   = c(0, 0.05, 1),
      limits   = c(0, 1),
      na.value = "grey92",
      name     = "Adjusted p",
      guide    = guide_colorbar(barwidth = 8, barheight = 0.6, title.position = "top")
    ) +
    scale_x_discrete(position = "top") +
    labs(
      title    = title_label,
      subtitle = "Pairwise chi-square, Bonferroni corrected  |  * p<0.05  ** p<0.01  *** p<0.001",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", color = "grey85", linewidth = 0.5),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid       = element_blank(),
      axis.text.x      = element_text(angle = 35, hjust = 0, color = "grey30", size = 10),
      axis.text.y      = element_text(color = "grey30", size = 10),
      plot.title       = element_text(size = 15, color = "grey20", margin = margin(b = 4)),
      plot.subtitle    = element_text(size = 9,  color = "grey50", margin = margin(b = 10)),
      legend.position  = "bottom",
      legend.title     = element_text(size = 9, color = "grey40"),
      plot.margin      = margin(16, 16, 16, 16)
    )

  ggsave(filename, plot = ph, width = 8, height = 7.5, dpi = 300, bg = "white")
  message(paste("Saved:", filename))
}

posthoc_heatmap(exp1_survivors, colors_vec,
                "Experiment 1 — Post Hoc Pairwise Comparisons",
                "posthoc_exp1.png")

posthoc_heatmap(exp2_survivors, colors_vec,
                "Experiment 2 — Post Hoc Pairwise Comparisons",
                "posthoc_exp2.png")

# ── Bar charts ────────────────────────────────
trial_colors <- c("Trial 1 Survivors" = "#4472C4",
                  "Trial 2 Survivors" = "#ED7D31",
                  "Trial 3 Survivors" = "#FFC000")

exp2_colors  <- c("Original Population" = "#4472C4",
                  "Trial 1 Survivors"   = "#ED7D31")

theme_clean <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background    = element_rect(fill = "white", color = "grey85", linewidth = 0.5),
      panel.background   = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
      panel.grid.minor   = element_blank(),
      axis.ticks         = element_blank(),
      axis.title.x       = element_text(color = "grey50", size = 11, margin = margin(t = 8)),
      axis.title.y       = element_blank(),
      axis.text          = element_text(color = "grey40", size = 10),
      legend.position      = "top",
      legend.justification = "left",
      legend.title         = element_blank(),
      legend.text          = element_text(size = 10, color = "grey30"),
      legend.key.size      = unit(0.8, "lines"),
      legend.key.spacing.x = unit(12, "pt"),
      legend.margin        = margin(b = 8),
      plot.title           = element_text(size = 16, color = "grey25", margin = margin(b = 6)),
      plot.margin          = margin(16, 20, 16, 16)
    )
}

exp1_long <- data.frame(
  Color = factor(rep(colors_vec, 3), levels = colors_vec),
  Trial = factor(rep(c("Trial 1 Survivors","Trial 2 Survivors","Trial 3 Survivors"), each = 10),
                 levels = names(trial_colors)),
  Survivors = c(0,16,8,0,4,0,24,12,12,24,
                0,24,0,0,16,0,16,8,8,32,
                0,52,0,0,0,0,16,12,0,20)
)

p1 <- ggplot(exp1_long, aes(x = Color, y = Survivors, fill = Trial)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = trial_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Experiment 1", x = "Color") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_clean()

ggsave("experiment1.png", plot = p1, width = 10, height = 5, dpi = 300, bg = "white")
message("Saved: experiment1.png")

exp2_long <- data.frame(
  Color = factor(rep(colors_vec, 2), levels = colors_vec),
  Stage = factor(rep(c("Original Population","Trial 1 Survivors"), each = 10),
                 levels = names(exp2_colors)),
  Count = c(rep(10, 10), 12,4,4,16,20,16,4,4,8,12)
)

p2 <- ggplot(exp2_long, aes(x = Color, y = Count, fill = Stage)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = exp2_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Experiment 2", x = "Color") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_clean()

ggsave("experiment2.png", plot = p2, width = 10, height = 5, dpi = 300, bg = "white")
message("Saved: experiment2.png")
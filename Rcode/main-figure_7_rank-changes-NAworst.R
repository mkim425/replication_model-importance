suppressPackageStartupMessages(library(tidyverse))
library(GGally)
library(ggbump)
library(ggplot2)
library(ggbump)

df_all_scores_NAworst <- readRDS("data/df_all_scores_NAworst.rds")

dat_NAworst <- df_all_scores_NAworst %>%
  group_by(model) %>%
  summarise(
    "-WIS" = mean(-wis),
    "Phi^{lasomo}" = mean(ImpScore_lasomo),
    "Phi^{lomo}" = mean(ImpScore_lomo)
  ) %>%
  ungroup() %>%
  arrange(desc(`-WIS`)) %>%
  mutate_if(is.numeric, ~ round(., 2))


df_rank <- dat_NAworst %>%
  mutate(
    rank_WIS = rank(-`-WIS`, ties.method = "min"),
    rank_lasomo = rank(-`Phi^{lasomo}`, ties.method = "min"),
    rank_lomo = rank(-`Phi^{lomo}`, ties.method = "min")
  ) %>%
  select(model, rank_WIS, rank_lasomo, rank_lomo)

# Plot rank changes between -WIS and Phi^lasomo
p.rank_lasomo <- df_rank %>%
  mutate(line_type = case_when(
    rank_WIS < rank_lasomo ~ "twodash",
    rank_WIS > rank_lasomo ~ "dotted",
    TRUE ~ "solid"
  )) %>%
  select(model, rank_WIS, rank_lasomo, line_type) %>%
  pivot_longer(
    cols = c(rank_WIS, rank_lasomo),
    names_to = "Metric",
    values_to = "Rank"
  ) %>%
  ggplot(aes(x = Metric, y = Rank, group = model, linetype = line_type)) +
  geom_bump(linewidth = 2) +
  geom_point(size = 0, alpha = 0.1) +
  scale_x_discrete(
    limits = rev, labels = c("-WIS", expression(Phi^{
      lasomo
    })),
    expand = c(0.45, 0.45)
  ) +
  scale_y_continuous(trans = "reverse", breaks = 1:10) +
  labs(
    title = bquote(bold("(a) -WIS vs. " * Phi^{
      lasomo
    })),
    x = ""
  ) +
  # Add labels for each line
  geom_text(aes(label = model),
    data = . %>% filter(Metric == "rank_lasomo"), # Place label at the last point
    hjust = "left",
    nudge_x = 0.03,
    size = 6
  ) +
  geom_text(aes(label = model),
    data = . %>% filter(Metric == "rank_WIS"), # Place label at the starting point
    hjust = "right",
    nudge_x = -0.03,
    size = 6
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 23)
  )

p.rank_lomo <- df_rank %>%
  mutate(line_type = case_when(
    rank_WIS < rank_lomo ~ "twodash",
    rank_WIS > rank_lomo ~ "dotted",
    TRUE ~ "solid"
  )) %>%
  select(model, rank_WIS, rank_lomo, line_type) %>%
  pivot_longer(
    cols = c(rank_WIS, rank_lomo),
    names_to = "Metric",
    values_to = "Rank"
  ) %>%
  ggplot(aes(x = Metric, y = Rank, group = model, linetype = line_type)) +
  geom_bump(linewidth = 2) +
  geom_point(size = 0, alpha = 0.1) +
  scale_x_discrete(
    limits = rev, labels = c("-WIS", expression(Phi^{
      lomo
    })),
    expand = c(0.45, 0.45)
  ) +
  scale_y_continuous(trans = "reverse", breaks = 1:10) +
  labs(title = bquote(bold("(b) -WIS vs. " * Phi^{
    lomo
  })), y = " ") +
  # Add labels for each line
  geom_text(aes(label = model),
    data = . %>% filter(Metric == "rank_lomo"), # Place label at the last point
    hjust = "left",
    nudge_x = 0.03,
    size = 6
  ) +
  geom_text(aes(label = model),
    data = . %>% filter(Metric == "rank_WIS"), # Place label at the starting point
    hjust = "right",
    nudge_x = -0.03,
    size = 6
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 23)
  )

# Save plots
pdf(
  file = "plots/main-figure_7a_rank-changes-NAworst.pdf",
  width = 10,
  height = 5
)
p.rank_lasomo
dev.off()

pdf(
  file = "plots/main-figure_7b_rank-changes-NAworst.pdf",
  width = 10,
  height = 5
)
p.rank_lomo
dev.off()

library(tidyverse)
library(ggplot2)
library(ggrepel) # for geom_text_repel()
library(ggalt) # for geom_dumbbell()

# load forecast data with WIS and importance for MA in 2021
forecast_ma2021_full <- readRDS("data-raw/forecast_death_ma2021.rds")

forecast_ma2021 <- forecast_ma2021_full %>%
  rename(f.median = "X0.5") %>%
  select(
    model, importance, wis, target_end_date, location, target,
    dispersion, overpred, underpred, f.median
  )

dat <- forecast_ma2021 %>%
  filter(!grepl("Ens", model)) %>%
  filter(target_end_date == "2021-12-25") %>%
  mutate(neg_wis = -wis)

# (a) create scatter plot on 2021-12-25
p.imp_wis.1225 <- dat %>%
  ggplot(aes(x = neg_wis, y = importance)) +
  geom_point(shape = 21, fill = "black", color = "black", size = 3) +
  geom_text_repel(
    data = subset(dat, model == "CovidAnalytics-DELPHI"),
    aes(label = model),
    size = 6,
    nudge_x = 32,
    nudge_y = -2,
    box.padding = 0.5,
    point.padding = 0.5,
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  xlim(-130, 0) +
  labs(y = "Importance", x = "-WIS") +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 22),
    legend.position = "none"
  )

# save plot
pdf(
  file = "plots/main-figure_5a_wis_imp20211225.pdf",
  width = 8,
  height = 5
)
p.imp_wis.1225
dev.off()

# (b) Plot of prediction intervals of 9 models and ensembles constructed by LOMO

truth.ma.2021 <- read_csv("data-raw/truth_MA_2021.csv") %>%
  select(target_end_date, value) %>%
  rename(truth = value)

y.val <- truth.ma.2021$truth[truth.ma.2021$target_end_date == "2021-12-25"]

# lock in factor level order
forecast_ma2021_full.v2 <- forecast_ma2021_full %>%
  mutate(model_order = factor(model, levels = unique(model))) %>%
  relocate(model_order)

d <- forecast_ma2021_full.v2 %>%
  filter(target_end_date == "2021-12-25") %>%
  select(model_order, 8:30)
model_sorted <- dat %>%
  arrange(desc(importance)) %>%
  select(model)
d1 <- d[c(10, 1:9), ]
d2 <- d[c(10:19), ]

# Prediction intervals of single models
p.PI.model <- d[c(1:9), ] %>%
  ggplot(aes(y = factor(model_order, levels = model_sorted$model))) +
  geom_dumbbell(aes(x = X0.025, xend = X0.975)) +
  geom_point(aes(x = X0.5)) +
  scale_y_discrete(labels = function(x, ...) gsub("-", "\n-", x)) +
  geom_vline(
    xintercept = y.val, linetype = "dashed",
    size = 0.5, colour = "darkred"
  ) +
  labs(
    x = "Value",
    title = "95% Prediction Intervals of Individual Forecasts",
    y = "Model"
  ) +
  theme(
    axis.text.x = element_text(size = 8, angle = 30, hjust = 0.85),
    legend.position = "none"
  ) +
  theme(
    axis.title.x = element_text(size = 13),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 13)
  ) +
  theme(text = element_text(size = 10)) +
  coord_flip()

# Prediction intervals of ensemble models

d2.copy <- d2 %>%
  mutate(model_renamed = model_order) %>%
  relocate(model_renamed)
d2.copy$model_renamed <- str_remove(d2.copy$model_renamed, pattern = "Ens.wo.")
d2.copy <- d2.copy %>%
  mutate(model_renamed = str_replace(
    model_renamed,
    "Ensemble.all",
    "None-(ensemble of all)"
  ))
extended_model_sorted <- c(model_sorted$model, "None-(ensemble of all)")
breaks2 <- c(seq(0, 700, by = 100))
labels2 <- as.character(breaks2)

p.PI.ensemble <- d2.copy %>%
  ggplot(aes(y = factor(model_renamed, levels = extended_model_sorted))) +
  geom_dumbbell(aes(x = X0.025, xend = X0.975)) +
  geom_point(aes(x = X0.5)) +
  scale_x_continuous(limits = c(-10, 500), breaks = breaks2, labels = labels2) +
  scale_y_discrete(labels = function(x, ...) gsub("-", "\n-", x)) +
  geom_vline(
    xintercept = y.val, linetype = "dashed",
    size = 0.5, colour = "darkred"
  ) +
  labs(
    x = "Value", y = "Left-out model",
    title = "95% Prediction Intervals of Ensembles Built Leaving One Model Out"
  ) +
  theme(
    axis.text.x = element_text(size = 8, angle = 30, hjust = 0.85),
    legend.position = "none"
  ) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 16)
  ) +
  theme(text = element_text(size = 12)) +
  coord_flip()


# save plot
pdf(
  file = "plots/main-figure_5b_95PIs-top.pdf",
  width = 8,
  height = 5
)
p.PI.model
dev.off()

pdf(
  file = "plots/main-figure_5b_95PIs-bottom.pdf",
  width = 8,
  height = 5
)
p.PI.ensemble
dev.off()

# NOTE: For the manuscript, p.PI.model and p.PI.ensemble were manually stacked vertically 
# so that their respective models are aligned.

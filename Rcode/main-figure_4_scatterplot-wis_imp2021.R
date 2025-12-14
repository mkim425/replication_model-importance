library(tidyverse)
library(ggplot2)

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
  mutate(
    target_end_date = str_remove(target_end_date, "2021-"),
    neg_wis = -wis
  )

# create scatter plot
p <- dat %>%
  ggplot(aes(x = neg_wis, y = importance)) +
  geom_hline(
    yintercept = 0, linetype = "dashed",
    size = 0.5, colour = "black"
  ) +
  geom_point(shape = 24, color = "grey57", size = 2) +
  geom_point(
    data = dat %>% filter(target_end_date == "12-25"),
    shape = 21, fill = "black", color = "black", size = 2.5
  ) +
  facet_wrap(~model) +
  labs( x = "-WIS", y = "Importance") +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

# save plot
pdf(
  file = "plots/main-figure_4_scatterplot-wis_imp2021.pdf",
  width = 8,
  height = 5
)
p
dev.off()

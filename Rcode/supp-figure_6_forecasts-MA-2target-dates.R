library(dplyr)
library(hubVis)
library(ggplot2)


forecast_ma2021_full <- readRDS("data-raw/forecast_death_ma2021.rds")
forecasts_MA_2021 <- read.csv("data-raw/forecasts_MA_2021_hor1-4.csv")
forecasts_MA_2021$target_end_date <- as.Date(forecasts_MA_2021$target_end_date)

truth <- read.csv("data-raw/truth_MA_2021.csv") |>
  select(target_end_date, location, value) |>
  mutate(
    target_end_date = as.Date(target_end_date),
    target = "wk inc death"
  ) |>
  rename(oracle_value = value)

models2include <- c(
  "CovidAnalytics-DELPHI", "USC-SI_kJalpha",
  "BPagano-RtDriven", "Karlen-pypm",
  "SteveMcConnell-CovidComplete", "UMass-MechBayes",
  "RobertWalraven-ESG", "COVIDhub-baseline",
  "UCSD_NEU-DeepGLEAM"
)

fdat <- forecasts_MA_2021 |>
  filter(
    model %in% models2include,
    location == "25",
    horizon == 4
  ) |>
  mutate(
    target = "wk inc death"
  ) |>
  rename(
    model_id = model,
    reference_date = forecast_date,
    output_type = type,
    output_type_id = quantile
  ) |>
  select(
    model_id, reference_date, location, horizon,
    target_end_date, target, output_type,
    output_type_id, value
  )

# Forecasts of incident weekly deaths in MA as of 2021-06-26
target_data1 <- truth |>
  filter(
    target_end_date <= as.Date("2021-06-26"),
    target_end_date >= as.Date("2021-04-15")
  ) |>
  rename(
    date = target_end_date,
    observation = oracle_value
  )
model_out_tbl1 <- fdat |>
  filter(
    target_end_date >= as.Date("2021-06-20"),
    target_end_date <= as.Date("2021-07-30")
  ) |>
  rename(target_date = target_end_date)
p1 <- plot_step_ahead_model_output(model_out_tbl1,
  target_data1,
  use_median_as_point = TRUE,
  pal_color = "Paired",
  show_legend = FALSE,
  interactive = FALSE,
  facet = "model_id"
) +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    strip.text = element_text(size = 10)
  ) +
  coord_cartesian(ylim = c(0, 150))

# Forecasts of incident weekly deaths in MA as of 2021-07-31

target_data2 <- truth |>
  filter(
    target_end_date <= as.Date("2021-08-05"),
    target_end_date >= as.Date("2021-06-15")
  ) |>
  rename(
    date = target_end_date,
    observation = oracle_value
  )
model_out_tbl2 <- fdat |>
  filter(
    target_end_date >= as.Date("2021-07-26"),
    target_end_date <= as.Date("2021-08-30")
  ) |>
  rename(target_date = target_end_date)
p2 <- plot_step_ahead_model_output(model_out_tbl2, target_data2,
  use_median_as_point = TRUE,
  pal_color = "Paired",
  show_legend = FALSE,
  interactive = FALSE,
  facet = "model_id"
) +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    strip.text = element_text(size = 10)
  ) +
  coord_cartesian(ylim = c(0, 500))

# Save plots
pdf(file="plots/supp-figure_6a_forecasts-MA-2target-dates.pdf",
    width = 8,
    height = 5)
p1
dev.off()


pdf(file="plots/supp-figure_6b_forecasts-MA-2target-dates.pdf",
    width = 8,
    height = 5)
p2
dev.off()
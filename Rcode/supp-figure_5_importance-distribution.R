library(dplyr)
library(modelimportance)
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


# Reference importance scores for the two target end dates
imp_ref <- forecast_ma2021_full |>
  filter(
    !grepl("Ens", model),
    target_end_date %in% c(as.Date("2021-07-24"), as.Date("2021-08-28"))
  ) |>
  rename(model_id = model) |>
  select(model_id, target_end_date)

# forecast data with target end date: 2021-07-24
fdat0724 <- fdat |>
  filter(target_end_date == as.Date("2021-07-24")) |>
  mutate(reference_date = as.Date("2021-06-28"))

models <- unique(fdat$model_id)
results_df <- list()
for (i in 2:9) {
  combo_list <- combn(models, i, simplify = FALSE)
  results_df[[i]] <- furrr::future_map_dfr(
    combo_list,
    function(combo) {
      dat <- fdat0724 |> filter(model_id %in% combo)

      res <- suppressMessages(
        model_importance(
          forecast_data = dat,
          oracle_output_data = truth,
          ensemble_fun = "simple_ensemble",
          importance_algorithm = "lomo"
        )
      ) |>
        model_importance_summary() |>
        mutate(across(where(is.numeric), round, 3)) |>
        right_join(imp_ref |> filter(target_end_date == "2021-07-24"),
          by = "model_id"
        ) |>
        arrange(model_id) |>
        mutate(
          subset_size = length(combo),
          included_models = paste(combo, collapse = ", ")
        )

      res
    }
  )
}

all_dfs0724 <- dplyr::bind_rows(results_df)

model_sorted <- imp_ref |>
  arrange(model_id) |>
  pull(model_id) |>
  unique()

data2plot <- all_dfs0724 |>
  mutate(model_id = factor(model_id, levels = model_sorted)) |>
  filter(!is.na(importance_score_mean))

p0724 <- all_dfs0724 |>
  mutate(model_id = factor(model_id, levels = model_sorted)) |>
  filter(!is.na(importance_score_mean)) |>
  filter(subset_size < 9) |>
  ggplot(aes(x = factor(subset_size), y = importance_score_mean)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.4, size = 1) +
  stat_summary(fun = median, geom = "crossbar", color = "blue", size = 0.25) +
  facet_wrap(~model_id, scales = "free_y") +
  geom_point(
    data = data2plot |>
      filter(subset_size == 9),
    mapping = aes(x = factor(subset_size), y = importance_score_mean), size = 1
  ) +
  labs(x = "Number of models in the subset", y = "Importance") +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    legend.position = "none",
    strip.text = element_text(size = 10)
  )


# forecast data with target end date: 2021-08-28
fdat0828 <- fdat |>
  filter(target_end_date == as.Date("2021-08-28")) |>
  mutate(reference_date = as.Date("2021-08-02"))

models <- unique(fdat$model_id)

results_df <- list()
for (i in 2:9) {
  combo_list <- combn(models, i, simplify = FALSE)
  results_df[[i]] <- furrr::future_map_dfr(
    combo_list,
    function(combo) {
      dat <- fdat0828 |> filter(model_id %in% combo)

      res <- suppressMessages(
        model_importance(
          forecast_data = dat,
          oracle_output_data = truth,
          ensemble_fun = "simple_ensemble",
          importance_algorithm = "lomo"
        )
      ) |>
        model_importance_summary() |>
        mutate(across(where(is.numeric), round, 3)) |>
        right_join(imp_ref |> filter(target_end_date == "2021-07-24"),
          by = "model_id"
        ) |>
        arrange(model_id) |>
        mutate(
          subset_size = length(combo),
          included_models = paste(combo, collapse = ", ")
        )

      res
    }
  )
}

all_dfs0828 <- dplyr::bind_rows(results_df)
model_sorted <- imp_ref |>
  arrange(model_id) |>
  pull(model_id) |>
  unique()

data2plot <- all_dfs0828 |>
  mutate(model_id = factor(model_id, levels = model_sorted)) |>
  filter(!is.na(importance_score_mean))

# Plot importance distributions across different subset sizes
p0828 <- data2plot |>
  filter(subset_size < 9) |>
  ggplot(aes(x = factor(subset_size), y = importance_score_mean)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.4, size = 1) +
  stat_summary(fun = median, geom = "crossbar", color = "blue", size = 0.25) +
  facet_wrap(~model_id, scales = "free_y") +
  geom_point(
    data = data2plot |>
      filter(subset_size == 9),
    mapping = aes(x = factor(subset_size), y = importance_score_mean), size = 1
  ) +
  labs(x = "Number of models in the subset", y = "Importance") +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    legend.position = "none",
    strip.text = element_text(size = 10)
  )



# Save plots
pdf(
  file = "plots/supp-figure_5a_importance-distribution.pdf",
  width = 8,
  height = 5
)
p0724
dev.off()

pdf(
  file = "plots/supp-figure_5b_importance-distribution.pdf",
  width = 8,
  height = 5
)
p0828
dev.off()

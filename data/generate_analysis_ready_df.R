library(tidyverse)
library(scoringutils)

# calculate WIS scores ----------------------------------------------------
forecast_data <- readRDS("data-raw/forecast_data_aligned_death_10models_nov20-nov22.rds") %>%
  select(-c(2)) %>%
  rename(
    forecast_date = reference_date,
    quantile_level = quantile,
    predicted = value
  ) %>% # Change the column names
  arrange(forecast_date, model)
truth_data <- readRDS("data-raw/truth_data_upto2022.rds") |>
  select(-"model") |>
  rename(observed = value)

scores <- forecast_data |>
  left_join(
    truth_data,
    by = c("location", "target_variable", "target_end_date")
  ) |>
  as_forecast_quantile(forecast_unit = c(
    "location", "forecast_date", "target_end_date", "model", "horizon"
  )) |>
  score()

scores_wis <- scores |>
  select(
    model, forecast_date, location, horizon, wis
  )

# load all importance scores and WIS scores -------------------------------
lomo_completed <- readRDS("data/lomo_completed.rds") %>%
  rename(ImpScore_lomo = ImportanceScore) %>%
  transform(
    horizon = as.character(horizon),
    forecast_date = as.Date(forecast_date)
  )
lasomo_completed <- readRDS("data/lasomo_completed.rds") %>%
  rename(ImpScore_lasomo = ImportanceScore) %>%
  transform(
    horizon = as.character(horizon),
    forecast_date = as.Date(forecast_date)
  )

df_all_scores <- lasomo_completed %>%
  left_join(scores_wis, by = c("model", "horizon", "forecast_date", "location")) %>%
  left_join(lomo_completed, by = c("model", "horizon", "forecast_date", "location"))

saveRDS(df_all_scores, "data/df_all_scores.rds")


# replace NAs in df_all_scores with the worst scores ---------------------
df_all_scores_NAworst <- df_all_scores %>%
  dplyr::group_by(horizon, forecast_date, location) %>%
  mutate_at(vars(wis), ~ replace_na(., max(., na.rm = TRUE))) %>%
  mutate_at(vars(ImpScore_lasomo), ~ replace_na(., min(., na.rm = TRUE))) %>%
  mutate_at(vars(ImpScore_lomo), ~ replace_na(., min(., na.rm = TRUE))) %>%
  ungroup()

saveRDS(df_all_scores_NAworst, "data/df_all_scores_NAworst.rds")

# replace NAs in df_all_scores with the average scores ---------------------
df_all_scores_NAavg <- df_all_scores %>%
  dplyr::group_by(horizon, forecast_date, location) %>%
  mutate_at(vars(wis), ~ replace_na(., mean(., na.rm = TRUE))) %>%
  mutate_at(vars(ImpScore_lasomo), ~ replace_na(., mean(., na.rm = TRUE))) %>%
  mutate_at(vars(ImpScore_lomo), ~ replace_na(., mean(., na.rm = TRUE))) %>%
  ungroup()

saveRDS(df_all_scores_NAavg, "data/df_all_scores_NAavg.rds")

# drop NAs in df_all_scores -----------------------------------------------
df_all_scores_NAdrop <- df_all_scores %>% drop_na()
saveRDS(df_all_scores_NAdrop, "data/df_all_scores_NAdrop.rds")

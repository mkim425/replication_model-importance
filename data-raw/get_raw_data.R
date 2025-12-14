library(covidData)
library(covidHubUtils)
library(dplyr)
library(tidyr)

# load all forecasts for MA in 2021 --------------------------------------------
forecasts_MA_2021 <- load_forecasts(
  # models = ,
  # dates = ,    #forecast date
  source = "zoltar",
  date_window_size = 6,
  locations = "25",
  types = c("quantile"),
  verbose = FALSE,
  targets = paste(1:4, "wk ahead inc death")
) |>
  dplyr::filter(target_end_date >= as.Date("2021-01-01") &
    target_end_date < as.Date("2022-01-01"))

write.csv(forecasts_MA_2021, "data-raw/forecasts_MA_2021_hor1-4.csv", row.names = FALSE)

# load truth data for MA in 2021 -----------------------------------------------
truth_data <- load_truth(
  truth_source = "JHU",
  target_variable = "inc death",
  locations = "25"
) %>% filter(target_end_date >= as.Date("2021-01-01") &
  target_end_date < as.Date("2022-01-01"))

write.csv(truth_data, file = "data-raw/truth_MA_2021.csv", row.names = FALSE)


## Data for Cast study -----------------------
# truth data for MA, 2021
truth.ma.2021 <- read.csv("data-raw/truth_MA_2021.csv") %>%
  select(target_end_date, value) %>%
  rename(true_value = value)
# load forecasts for MA, 2021 (horizon 4)
fdat.multi.ma.2021 <- load_forecasts(
  # models = ,
  # dates = ,    #forecast date
  source = "zoltar",
  date_window_size = 6,
  locations = "25",
  types = c("quantile"),
  verbose = FALSE,
  targets = paste(4, "wk ahead inc death")
) %>%
  filter(target_end_date >= as.Date("2021-01-01") &
    target_end_date < as.Date("2022-01-01"))

# select models with all 52 weeks of forecasts in 2021
model.list <- fdat.multi.ma.2021 %>%
  select(model) %>%
  unique()
num.wk <- c()
j <- 0
for (i in model.list$model) {
  j <- j + 1
  num.wk[j] <- fdat.multi.ma.2021 %>%
    filter(model == i) %>%
    select(target_end_date) %>%
    unique() %>%
    nrow()
}
models.52wks <- data.frame(model = model.list$model, num.wk = num.wk) %>%
  filter(num.wk == 52) %>%
  select(model)
models.ma.allwks2021 <- models.52wks$model

# filter data for only models with all 52 weeks of forecasts in 2021
fdat.multi.ma.2021all <- fdat.multi.ma.2021 %>%
  filter(model %in% models.ma.allwks2021) %>%
  filter_at(.vars = vars(model), all_vars(!grepl("ensemble", .))) # exclude ensemble models

# impute missing quantiles of "UMass-MechBayes" with linear interpolation
# impute the value for quantile 0.4 with the mean of values of qt 0.35 and 0.45
row.qt0.4_0815 <- fdat.multi.ma.2021all %>%
  filter(model == "UMass-MechBayes", forecast_date == "2021-08-15", quantile == 0.350)
row.qt0.4_0815$quantile <- 0.400
row.qt0.4_0815$value <- 167

row.qt0.4_0822 <- fdat.multi.ma.2021all %>%
  filter(model == "UMass-MechBayes", forecast_date == "2021-08-22", quantile == 0.350)
row.qt0.4_0822$quantile <- 0.400
row.qt0.4_0822$value <- 62
# complete the forecast dataset by adding the new rows
fdat.ma.2021all <- rbind(fdat.multi.ma.2021all, row.qt0.4_0815, row.qt0.4_0822)

# define function to calculate ensemble forecasts
fdat.mean.ensemble <- function(data, target_date) {
  dat <- data %>%
    filter(target_end_date == target_date)
  # Calculate equally-weighted mean ensemble from all models
  ens.all <- dat %>%
    group_by(quantile) %>%
    summarise(value = mean(value)) %>%
    mutate(model = "Ensemble.all") %>%
    relocate(model)
  fdat.ens <- rbind(dat %>% select(model, quantile, value), ens.all)
  model.names <- unique(data$model)
  for (i in model.names) {
    ens.part <- dat %>%
      filter(model != i) %>%
      group_by(quantile) %>%
      summarise(value = mean(value)) %>%
      mutate(model = paste0("Ens.wo.", i)) %>%
      relocate(model)
    fdat.ens <- rbind(fdat.ens, ens.part)
  }
  # wide data frame
  fdat.ens.wide <- fdat.ens %>%
    pivot_wider(names_from = quantile)
  return(fdat.ens.wide)
}

# calculate WIS and importance scores for all models in MA, 2021 (horizon 4)
location <- fdat.ma.2021all$abbreviation %>% unique()
models <- fdat.ma.2021all$model %>% unique()
n.mod <- length(models)
target <- "4wk ahead inc death"
y.val <- truth.ma.2021$true_value
alpha0 <- 1
alpha <- c(seq(0.9, 0.1, by = -0.1), 0.05, 0.02)
K <- length(alpha)
w0 <- 1 / 2
w1 <- alpha / 2
# weights for over,under prediction#####
w <- c(w0 * alpha0 / 2, w1) * 2 / c(alpha0, alpha)

target.date <- truth.ma.2021$target_end_date
mod.impo.list <- list()
k <- 0
for (i in target.date) {
  k <- k + 1
  y <- y.val[k]
  df <- fdat.mean.ensemble(fdat.ma.2021all, i)

  df.wis <- as.data.frame(
    df %>%
      mutate(
        dispersion = 1 / (K + 1 / 2) * t(w1 %*% t(df[13 + c(1:11)] - df[13 - c(1:11)])),
        overpred = 1 / (K + 1 / 2) * t(w %*% t((df[13 - c(0:11)] - y) * as.numeric(y < df[13 - c(0:11)]))),
        underpred = 1 / (K + 1 / 2) * t(w %*% t((y - df[13 + c(0:11)]) * as.numeric(y > df[13 + c(0:11)])))
      ) %>%
      mutate(wis = dispersion + overpred + underpred) %>%
      relocate(c(model, wis, dispersion, overpred, underpred), .before = "0.01")
  )

  ens.importance <- round(-rep(df.wis[which(df.wis$"model" == "Ensemble.all"), 2], n.mod) +
    df.wis[which(df.wis$"model" == "Ensemble.all") + 1:+n.mod, 2], 2)

  df.wis.importance.all <- df.wis %>%
    mutate(importance = c(ens.importance, rep(NA, 9 + 1))) %>%
    relocate(c(importance), .before = wis) %>%
    mutate(
      target_end_date = i,
      location = location,
      target = target
    )

  mod.impo.list[[k]] <- df.wis.importance.all
}

df.all.ma.2021 <- do.call(rbind.data.frame, mod.impo.list)
original_cols <- colnames(df.all.ma.2021)
colnames(df.all.ma.2021)[7:29] <- paste("X", original_cols[7:29], sep = "")
saveRDS(df.all.ma.2021, "data-raw/forecast_death_ma2021.rds")

## Data for Application
# load truth data for all states up to 2022------------------------------------
incl_locations <- covidData::fips_codes %>%
  dplyr::filter(nchar(location) == 2, location <= "56", location != "11") %>%
  dplyr::pull(location)

truth <- load_truth(
  truth_source = "JHU",
  target_variable = "inc death",
  locations = incl_locations
)

saveRDS(truth, "data-raw/truth_data_upto2022.rds")

# load and align forecast data for 10 models, Nov 2020 - Nov 2022
dates <- seq.Date(
  from = as.Date("2020-10-30"),
  to = as.Date("2022-11-28"),
  by = 7
) %>%
  as.character()

incl_locations <- covidData::fips_codes %>%
  dplyr::filter(nchar(location) == 2, location <= "56", location != "11") %>%
  dplyr::pull(location)

incl_models <- c(
  "BPagano-RtDriven", "COVIDhub-baseline", "CU-select",
  "GT-DeepCOVID", "Karlen-pypm", "MOBS-GLEAM_COVID",
  "PSI-DRAFT", "RobertWalraven-ESG", "UCSD_NEU-DeepGLEAM",
  "USC-SI_kJalpha"
)
death_targets <- paste(1:4, "wk ahead inc death")

forecast_data <- load_forecasts(
  models = incl_models,
  dates = dates,
  date_window_size = 6,
  locations = incl_locations,
  types = "quantile",
  targets = death_targets,
  source = "zoltar",
  verbose = FALSE
)

# align forecasts
d <- forecast_data
colnames <- c(
  "model", "forecast_date", "reference_date", "location", "horizon",
  "relative_horizon", "temporal_resolution", "target_variable",
  "target_end_date", "type", "quantile", "value"
)

forecast_data_aligned <- align_forecasts(d) %>%
  select(all_of(colnames)) 

saveRDS(
  forecast_data_aligned,
  "data-raw/forecast_data_aligned_death_10models_nov20-nov22.rds"
)

# Data for the simulation with 3 point forecasters:
# two fixed forecasts (hat_y1, hat_y2) and one changing forecast (hat_y3)

library(tidyverse)
library("stringr")
source("utils/build_ensemble.R")

# Simulate data for 3 point forecasters ---------------------------------------
simu_data_f3points <- function(b) {
  set.seed(2022)
  T <- 1000
  Y <- rnorm(T, 0, 1)
  # Fixed forecasts
  hat_y1 <- rep(-1, T)
  hat_y2 <- rep(-0.5, T)
  # Changing forecast
  hat_y3 <- rep(b, T)
  # combine 3 forecasts into a data frame
  forecasts <- cbind(hat_y1, hat_y2, hat_y3)
  df <- data.frame(Y, forecasts) %>%
    mutate(
      ens_wo_1 = rowMeans(select(., -Y, -hat_y1)),
      ens_wo_2 = rowMeans(select(., -Y, -hat_y2)),
      ens_wo_3 = rowMeans(select(., -Y, -hat_y3)),
      ens_all = rowMeans(select(., -Y))
    ) %>%
    mutate(
      err1 = Y - hat_y1,
      err2 = Y - hat_y2,
      err3 = Y - hat_y3
    ) %>%
    mutate(
      phi_1 = (Y - ens_wo_1)^2 - (Y - ens_all)^2,
      phi_2 = (Y - ens_wo_2)^2 - (Y - ens_all)^2,
      phi_3 = (Y - ens_wo_3)^2 - (Y - ens_all)^2
    )
  # Calculate the importance scores
  Imp_score <- df %>%
    select(phi_1, phi_2, phi_3) %>%
    colMeans() %>%
    as.data.frame() %>%
    rownames_to_column(var = "Model") %>%
    mutate(Model = str_replace(Model, "phi_", "forecaster")) %>%
    rename(Importance_score = 2) %>%
    dplyr::mutate(f3_pred = b)

  return(Imp_score)
}

b <- seq(-1, 3, 0.05)
list.df <- lapply(b, simu_data_f3points)
list.df.pts <- do.call(rbind, list.df)

saveRDS(list.df.pts, "data-raw/simulation_f3pts.rds")

# Simulate data for 3 quantile forecasters ------------------------------------
simu_data_f3bias <- function(b) {
  set.seed(2022)
  T <- 1000
  Y <- c()
  for (t in 1:T) {
    Y[t] <- rnorm(1, 0, 1)
  }
  mu1 <- rep(-1, T)
  mu2 <- rep(-0.5, T)
  mu3 <- rep(b, T)
  mu <- cbind(mu1, mu2, mu3)
  sd <- cbind(rep(1, T), rep(1, T), rep(1, T))

  probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

  df.list <- list()
  i <- 0
  for (t in 1:T) {
    i <- i + 1
    forecast <- sapply(1:3, function(x) rnorm(100, mean = mu[t, x], sd = 1))
    quantiles <- t(apply(forecast, 2, function(x) quantile(x, probs = probs)))
    df0 <- as.data.frame(quantiles)
    colnames(df0) <- str_c("qt", probs)
    df.list[[i]] <- df0 %>%
      dplyr::mutate(
        model = str_c("forecaster", 1:3),
        target_date = str_c("week", t)
      ) %>%
      dplyr::relocate(model, target_date)
  }

  df.23qt <- do.call(rbind.data.frame, df.list)

  df.23qt.long <- pivot_longer(df.23qt, 3:25, names_to = "quantile")
  models <- df.23qt$model %>% unique()
  target.date <- unique(df.23qt$target_date)
  n.mod <- length(models)
  y.val <- Y
  alpha0 <- 1
  alpha <- c(seq(0.9, 0.1, by = -0.1), 0.05, 0.02)
  K <- length(alpha)
  w0 <- 1 / 2
  w1 <- alpha / 2
  w <- c(w0 * alpha0 / 2, w1) * 2 / c(alpha0, alpha)

  mod.impo.list <- list()
  k <- 0
  for (i in target.date) {
    k <- k + 1
    y <- y.val[k]
    df <- build_mean_ensemble(df.23qt.long, i)

    df.wis <- as.data.frame(
      df %>%
        dplyr::mutate(
          dispersion = 1 / (K + 1 / 2) * t(w1 %*% t(df[13 + c(1:11)] - df[13 - c(1:11)])),
          overpred = 1 / (K + 1 / 2) * t(w %*% t((df[13 - c(0:11)] - y) * as.numeric(y < df[13 - c(0:11)]))),
          underpred = 1 / (K + 1 / 2) * t(w %*% t((y - df[13 + c(0:11)]) * as.numeric(y > df[13 + c(0:11)])))
        ) %>%
        dplyr::mutate(wis = dispersion + overpred + underpred) %>%
        dplyr::relocate(c(model, wis, dispersion, overpred, underpred), .before = qt0.01)
    )

    ens.importance <- -rep(df.wis[which(df.wis$"model" == "Ensemble.all"), 2], n.mod) +
      df.wis[which(df.wis$"model" == "Ensemble.all") + 1:+n.mod, 2]

    df.wis.importance.all <- df.wis %>%
      dplyr::mutate(importance = c(ens.importance, rep(NA, n.mod + 1))) %>%
      dplyr::relocate(c(importance), .before = wis) %>%
      dplyr::mutate(target_end_date = i, .after = wis)

    mod.impo.list[[k]] <- df.wis.importance.all
  }

  df.23qt.full <- do.call(rbind.data.frame, mod.impo.list)

  truth <- rep(Y, rep(3, length(Y)))
  data <- df.23qt.full %>%
    filter(!grepl("Ens", model)) %>%
    dplyr::mutate(t = as.integer(str_remove(target_end_date, "week"))) %>%
    dplyr::mutate(error = qt0.5 - truth, MAE = abs(error)) %>%
    dplyr::mutate(f3_bias = b) %>%
    dplyr::select(model, f3_bias, importance, wis, dispersion, overpred, underpred, error, MAE, t)

  return(data)
}

b <- seq(-1, 3, 0.05)
list.df <- lapply(b, simu_data_f3bias)
list.df.23qt <- do.call(rbind, list.df)

saveRDS(list.df.23qt, "data-raw/simulation_f3bias.rds")

# Simulate data for 3 forecasters with the same bias but different dispersion --
simu_data_f3dispersion <- function(s) {
  set.seed(2022)
  T <- 1000
  Y <- c()
  for (t in 1:T) {
    Y[t] <- rnorm(1, 0, 1)
  }
  mu1 <- mu2 <- mu3 <- rep(0, T)
  mu <- cbind(mu1, mu2, mu3)
  sd <- cbind(rep(0.5, T), rep(0.7, T), rep(s, T))

  probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

  df.list <- list()
  i <- 0
  for (t in 1:T) {
    i <- i + 1
    forecast <- sapply(1:3, function(x) rnorm(100, mean = mu[t, x], sd = sd[t, x]))
    quantiles <- t(apply(forecast, 2, function(x) quantile(x, probs = probs)))
    df0 <- as.data.frame(quantiles)
    colnames(df0) <- str_c("qt", probs)
    df.list[[i]] <- df0 %>%
      dplyr::mutate(
        model = str_c("forecaster", 1:3),
        target_date = str_c("week", t)
      ) %>%
      dplyr::relocate(model, target_date)
  }

  df.23qt <- do.call(rbind.data.frame, df.list)

  df.23qt.long <- pivot_longer(df.23qt, 3:25, names_to = "quantile")
  models <- df.23qt$model %>% unique()
  target.date <- unique(df.23qt$target_date)
  n.mod <- length(models)
  y.val <- Y
  alpha0 <- 1
  alpha <- c(seq(0.9, 0.1, by = -0.1), 0.05, 0.02)
  K <- length(alpha)
  w0 <- 1 / 2
  w1 <- alpha / 2
  w <- c(w0 * alpha0 / 2, w1) * 2 / c(alpha0, alpha)

  mod.impo.list <- list()
  k <- 0
  for (i in target.date) {
    print(paste0("s=", s, ", target date = ", i))
    k <- k + 1
    y <- y.val[k]
    df <- build_mean_ensemble(df.23qt.long, i)

    df.wis <- as.data.frame(
      df %>%
        dplyr::mutate(
          dispersion = 1 / (K + 1 / 2) * t(w1 %*% t(df[13 + c(1:11)] - df[13 - c(1:11)])),
          overpred = 1 / (K + 1 / 2) * t(w %*% t((df[13 - c(0:11)] - y) * as.numeric(y < df[13 - c(0:11)]))),
          underpred = 1 / (K + 1 / 2) * t(w %*% t((y - df[13 + c(0:11)]) * as.numeric(y > df[13 + c(0:11)])))
        ) %>%
        dplyr::mutate(wis = dispersion + overpred + underpred) %>%
        dplyr::relocate(c(model, wis, dispersion, overpred, underpred), .before = qt0.01)
    )

    ens.importance <- round(-rep(df.wis[which(df.wis$"model" == "Ensemble.all"), 2], n.mod) +
      df.wis[which(df.wis$"model" == "Ensemble.all") + 1:+n.mod, 2], 2)

    df.wis.importance.all <- df.wis %>%
      dplyr::mutate(importance = c(ens.importance, rep(NA, n.mod + 1))) %>%
      dplyr::relocate(c(importance), .before = wis) %>%
      dplyr::mutate(target_end_date = i, .after = wis)

    mod.impo.list[[k]] <- df.wis.importance.all
  }

  df.23qt.full <- do.call(rbind.data.frame, mod.impo.list)

  truth <- rep(Y, rep(3, length(Y)))
  data <- df.23qt.full %>%
    filter(!grepl("Ens", model)) %>%
    dplyr::mutate(t = as.integer(str_remove(target_end_date, "week"))) %>%
    dplyr::mutate(error = qt0.5 - truth, MAE = abs(error)) %>%
    dplyr::mutate(f3_sharpness = s) %>%
    dplyr::select(model, f3_sharpness, importance, wis, dispersion, overpred, underpred, error, MAE, t)

  return(data)
}

s <- seq(0.1, 3, 0.05)
list.df2 <- lapply(s, simu_data_f3dispersion)
list.df2.23qt <- do.call(rbind, list.df)

saveRDS(list.df2.23qt, "data-raw/simulation_f3sharp.rds")

# Combine lasomo arrays from different runs into a single list of dataframes

library(stringr)
library(tidyverse)

# horizon1
filenames_h1 <- list.files("data/calc-result/lasomo-output",
  pattern = "array-horizon1", full.names = TRUE
)
ldf <- lapply(filenames_h1, readRDS)

list_lasomo_h1 <- NULL
for (j in 1:10) {
  df <- NULL
  for (i in 1:length(ldf)) {
    arr_ij <- as.data.frame(ldf[[i]][j])
    df <- rbind(df, arr_ij)
  }
  new_names <- str_sub(colnames(df), -2, -1)
  colnames(df) <- new_names
  list_lasomo_h1[[j]] <- df
  names(list_lasomo_h1)[j] <- names(ldf[[1]][j])
}

# horizon2
filenames_h2 <- list.files("data/calc-result/lasomo-output",
  pattern = "array-horizon2", full.names = TRUE
)
ldf <- lapply(filenames_h2, readRDS)

list_lasomo_h2 <- NULL
for (j in 1:10) {
  df <- NULL
  for (i in 1:length(ldf)) {
    arr_ij <- as.data.frame(ldf[[i]][j])
    df <- rbind(df, arr_ij)
  }
  new_names <- str_sub(colnames(df), -2, -1)
  colnames(df) <- new_names
  list_lasomo_h2[[j]] <- df
  names(list_lasomo_h2)[j] <- names(ldf[[1]][j])
}

# horizon3
filenames_h3 <- list.files("data/calc-result/lasomo-output",
  pattern = "array-horizon3", full.names = TRUE
)
ldf <- lapply(filenames_h3, readRDS)

list_lasomo_h3 <- NULL
for (j in 1:10) {
  df <- NULL
  for (i in 1:length(ldf)) {
    arr_ij <- as.data.frame(ldf[[i]][j])
    df <- rbind(df, arr_ij)
  }
  new_names <- str_sub(colnames(df), -2, -1)
  colnames(df) <- new_names
  list_lasomo_h3[[j]] <- df
  names(list_lasomo_h3)[j] <- names(ldf[[1]][j])
}

# horizon4
filenames_h4 <- list.files("data/calc-result/lasomo-output",
  pattern = "array-horizon4", full.names = TRUE
)
ldf <- lapply(filenames_h4, readRDS)

list_lasomo_h4 <- NULL
for (j in 1:10) {
  df <- NULL
  for (i in 1:length(ldf)) {
    arr_ij <- as.data.frame(ldf[[i]][j])
    df <- rbind(df, arr_ij)
  }
  new_names <- str_sub(colnames(df), -2, -1)
  colnames(df) <- new_names
  list_lasomo_h4[[j]] <- df
  names(list_lasomo_h4)[j] <- names(ldf[[1]][j])
}

# Combine all
## function to add date column
create_date_col <- function(df) {
  df$forecast_date <- rownames(df)
  row.names(df) <- NULL
  df2 <- df %>% relocate(forecast_date)
  return(df2)
}

arrays <- c(list_lasomo_h1, list_lasomo_h2, list_lasomo_h3, list_lasomo_h4)
all_arrays <- lapply(arrays, create_date_col)

lasomo_df <- NULL
for (i in 1:40) {
  h <- (i - 1) %/% 10 + 1
  df_long <- all_arrays[[i]] %>%
    mutate(model = names(all_arrays)[i], horizon = h) %>%
    relocate(model, horizon) %>%
    pivot_longer(cols = -c(1:3), names_to = "location", values_to = "ImportanceScore")
  lasomo_df <- rbind(lasomo_df, df_long)
}

# Save the combined dataframe
saveRDS(lasomo_df, "data/lasomo_completed.rds")

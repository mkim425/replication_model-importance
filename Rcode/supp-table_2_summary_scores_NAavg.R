suppressPackageStartupMessages(library(tidyverse))

df_all_scores <- readRDS("data/df_all_scores.rds")
props <- df_all_scores %>%
  group_by(model) %>%
  summarise(
    submission_count = sum(!is.na(wis)),
    submission_proportion = round(sum(!is.na(wis)) / 21800 * 100, 1)
  ) %>%
  ungroup() %>%
  arrange(desc(submission_proportion)) %>%
  rename(
    "Number of predictions" = submission_count,
    "Submission rate (%)" = submission_proportion
  )

df_all_scores_NAavg <- readRDS("data/df_all_scores_NAavg.rds")

table2 <- df_all_scores_NAavg %>%
  group_by(model) %>%
  summarise(
    "-WIS" = mean(-wis),
    "Phi^{lasomo}" = mean(ImpScore_lasomo),
    "Phi^{lomo}" = mean(ImpScore_lomo)
  ) %>%
  ungroup() %>%
  arrange(desc(`-WIS`)) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  left_join(props, by = "model")

cat(
  file = "tables/supp-table_2_summary_scores_NAavg.txt",
  capture.output(table2),
  sep = "\n"
)

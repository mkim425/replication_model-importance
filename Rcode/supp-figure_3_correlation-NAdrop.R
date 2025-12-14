suppressPackageStartupMessages(library(tidyverse))
library(GGally)


df_all_scores_NAavg <- readRDS("data/df_all_scores_NAavg.rds")

dat_NAavg <- df_all_scores_NAavg %>%
  group_by(model) %>%
  summarise(
    "-WIS" = mean(-wis),
    "Phi^{lasomo}" = mean(ImpScore_lasomo),
    "Phi^{lomo}" = mean(ImpScore_lomo)
  ) %>%
  ungroup() %>%
  arrange(desc(`-WIS`)) %>%
  mutate_if(is.numeric, ~ round(., 2))


colnames <- c(
  paste0("-WIS"),
  "Phi^{lasomo}",
  "Phi^{lomo}"
)

p.pairs <- ggpairs(
  dat_NAavg %>% select(-1),
  columnLabels = colnames,
  labeller = "label_parsed"
) +
  theme(
    strip.text = element_text(size = 14),
    panel.spacing = unit(0.65, "lines")
  )


pdf(
  file = "plots/supp-figure_3_correlation-NAavg.pdf",
  width = 10,
  height = 5
)
p.pairs
dev.off()

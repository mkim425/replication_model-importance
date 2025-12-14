library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(ggpubr)

# Association between a component forecaster's point prediction and importance
dat <- readRDS("data-raw/simulation_f3pts.rds")
p1 <- dat %>%
  ggplot(aes(x = f3_pred, y = Importance_score, group = Model)) +
  geom_line(aes(linetype = Model)) +
  scale_linetype_manual(
    values = c("dashed", "twodash", "solid"),
    labels = c("forecaster 1", "forecaster 2", "forecaster 3")
  ) +
  labs(
    x = "b (prediction of forecaster 3)", y = "Average  Importance",
    title = bquote(bold("(a) Average Importance based on -SPE"))
  ) +
  theme(legend.position = "bottom") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )


# Association between a component forecaster's bias and importance
simdat <- readRDS("data-raw/simulation_f3bias.rds")
p2 <- simdat %>%
  rename(Model = model) %>%
  dplyr::select(Model, f3_bias, importance, dispersion) %>%
  dplyr::group_by(f3_bias, Model) %>%
  dplyr::summarise(
    sharpness = mean(dispersion),
    avg_imp = mean(importance),
    "5% qt" = quantile(importance, probs = 0.05),
    # "50% qt"=quantile(importance, probs = 0.50),
    "95% qt" = quantile(importance, probs = 0.95),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = f3_bias, y = avg_imp, group = Model)) +
  geom_line(aes(linetype = Model)) +
  scale_linetype_manual(
    values = c("dashed", "twodash", "solid"),
    labels = c("forecaster 1", "forecaster 2", "forecaster 3")
  ) +
  labs(
    x = "b (bias of forecaster 3)", y = "Average  Importance",
    title = bquote(bold("(b) Average Importance based on -WIS"))
  ) +
  theme(legend.position = "bottom") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

# Combine two plots and save
p <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")
pdf(
  file = "plots/main-figure_2_simulation-settingA.pdf",
  width = 10,
  height = 5
)
p
dev.off()

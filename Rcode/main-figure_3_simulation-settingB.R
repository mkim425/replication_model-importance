library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(ggpubr)

# Association between component forecaster dispersion and importance
simdat <- readRDS("data-raw/simulation_f3sharp.rds")

p3 <- simdat %>% rename(Model = model) %>%  
        dplyr::select(Model, f3_sharpness, importance, dispersion) %>%
        dplyr::group_by(f3_sharpness, Model) %>%
        dplyr::summarise(sharpness=mean(dispersion), 
                         avg_imp=mean(importance), 
                         "5% qt"=quantile(importance, probs = 0.05), 
                         #"50% qt"=quantile(importance, probs = 0.50),
                         "95% qt"=quantile(importance, probs = 0.95), 
                         .groups = 'drop') %>% 
        ggplot(aes(x=f3_sharpness, y =avg_imp, group=Model)) +
        geom_line(aes(linetype=Model)) +
        scale_linetype_manual(values=c("dashed","twodash", "solid"),
                              labels = c("forecaster 1","forecaster 2","forecaster 3")) +
        labs(x="s (standard deviation of forecaster 3)",
             #title="Importance of three forecasters by different dispersion of forecaster 3", 
             y="Average  Importance") + 
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14),
              strip.text = element_text(size = 12),
              legend.title = element_text(size=14),
              legend.text = element_text(size=14))

# Save plot
pdf(file="plots/main-figure_3_simulation-settingB.pdf",
    width = 10,
    height = 5)
p3
dev.off()
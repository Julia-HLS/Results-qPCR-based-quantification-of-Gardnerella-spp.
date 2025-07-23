###########################################################################
# Clear the workspace
rm(list = ls())

# Set the working directory
setwd("D:/USER/Julia/Schule/2021_22Fhnw/BA/Auswertung/publicRepo")
getwd()

# Load necessary libraries
library(tidyverse)
library(readxl)
library(ggpubr)
library(gridExtra)
library(ggplot2)
library(readr)

# Import dataset 

mastertable <- read_excel("_2025_database_JG.xlsx", 
                          sheet = "mastertable", col_types = c("text", "numeric",
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric",
                                                               "text", "text",
                                                               "numeric", "text",
                                                               "text", "text", "text", 
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text"))
#View(mastertable)

# Drop samples with negative GV_status
mastertable <- mastertable %>% filter(GV_status != "negative")

# Set order and labels for body site
mastertable <- mastertable %>%
  mutate(Body_site = factor(Body_site, 
                            levels = c("vaginal_fluid", "penile_skin", "rectal"),
                            labels = c("Vaginal", "Penile skin", "Rectal")))

# Calculate mean and CI per body site
summary_stats <- mastertable %>%
  group_by(Body_site) %>%
  summarise(
    mean = mean(GV_log_copies_swab, na.rm = TRUE),
    sd = sd(GV_log_copies_swab, na.rm = TRUE),
    n = sum(!is.na(GV_log_copies_swab)),
    se = sd / sqrt(n),
    CI_lower = mean - qt(0.975, df = n - 1) * se,
    CI_upper = mean + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )
print(summary_stats)

# Pairwise comparisons: Wilcoxon and mean diff + CI
compare_means <- function(data, group1, group2) {
  g1 <- data$GV_log_copies_swab[data$Body_site == group1]
  g2 <- data$GV_log_copies_swab[data$Body_site == group2]
  
  # Wilcoxon
  test <- wilcox.test(g1, g2)
  p_val <- test$p.value
  
  # Mean diff and 95% CI
  diff_mean <- mean(g1, na.rm = TRUE) - mean(g2, na.rm = TRUE)
  pooled_se <- sqrt(var(g1, na.rm = TRUE)/length(g1) + var(g2, na.rm = TRUE)/length(g2))
  df <- min(length(g1), length(g2)) - 1
  ci_margin <- qt(0.975, df) * pooled_se
  ci_lower <- diff_mean - ci_margin
  ci_upper <- diff_mean + ci_margin
  
  tibble(
    Comparison = paste(group1, "vs", group2),
    Mean_Diff = round(diff_mean, 2),
    CI_Lower = round(ci_lower, 2),
    CI_Upper = round(ci_upper, 2),
    P_Value = signif(p_val, 3)
  )
}

# Apply to all comparisons
pairwise_results <- bind_rows(
  compare_means(mastertable, "Vaginal", "Penile skin"),
  compare_means(mastertable, "Vaginal", "Rectal"),
  compare_means(mastertable, "Penile skin", "Rectal")
)
print(pairwise_results)

# Kruskal–Wallis test
kruskal_result <- kruskal.test(GV_log_copies_swab ~ Body_site, data = mastertable)
print(kruskal_result)

# Define comparisons for plotting
comparisons <- list(
  c("Vaginal", "Penile skin"),
  c("Vaginal", "Rectal"),
  c("Penile skin", "Rectal")
)

# Save plot
png("Plots/Gspp_BodySite_withoutNegative.png", width = 1800, height = 1800, res = 300)

ggplot(mastertable, aes(x = Body_site, y = GV_log_copies_swab, fill = Body_site)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
  scale_fill_manual(values = c("Vaginal" = "palevioletred", 
                               "Penile skin" = "paleturquoise4", 
                               "Rectal" = "gold")) +
  theme_classic() +
  labs(
    title = expression(italic("Gardnerella spp.") * " DNA copies per body site"),
    x = "Body site",
    y = expression("Log10 DNA copy number (" * italic("Gardnerella spp.") * ")")
  ) +
  stat_compare_means(comparisons = comparisons, 
                     method = "wilcox.test", 
                     label = "p.format", 
                     label.y = c(8.8, 9.6, 10.4)) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = 10.6, 
                     label = "p.format") +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

dev.off()
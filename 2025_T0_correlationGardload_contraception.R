###########################################################################
# Clear the workspace
rm(list = ls())

# Set the working directory
setwd("D:/USER/Julia/Schule/2021_22Fhnw/BA/Auswertung")
getwd()

# Load necessary libraries
library(tidyverse)
library(readxl)
library(ggpubr)
library(gridExtra)
library(ggplot2)
library(readr)

# Import dataset 
mastertable <- read_excel("2025_mastertable_adjusted_allTimepoints.xlsx", 
                          sheet = "mastertable", col_types = c("text", "numeric",
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric",
                                                               "numeric",
                                                               "numeric", "numeric", "numeric", "numeric",
                                                               "text", "text",
                                                               "numeric", "text",
                                                               "text", "text", "text", 
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text"))

#Prepare paired data
paired_data <- mastertable %>%
  filter(timepoint == "t0",
         Body_site %in% c("vaginal_fluid", "penile_skin"),
         !is.na(GV_log_copies_swab)) %>%
  mutate(Body_site = factor(Body_site,
                            levels = c("vaginal_fluid", "penile_skin"),
                            labels = c("Vaginal", "Penile skin"))) %>%
  group_by(couple_no, Body_site) %>%
  summarise(mean_log = mean(GV_log_copies_swab), .groups = "drop") %>%
  pivot_wider(names_from = Body_site, values_from = mean_log) %>%
  drop_na()

# Assign protection per couple
protection_per_pair <- mastertable %>%
  filter(!is.na(protection)) %>%
  group_by(couple_no) %>%
  summarise(
    protection = case_when(
      any(protection == "yes") ~ "yes",
      all(protection == "no") ~ "no",
      TRUE ~ "not_available"
    ), .groups = "drop"
  )

paired_data <- paired_data %>%
  left_join(protection_per_pair, by = "couple_no") %>%
  mutate(protection = factor(protection, levels = c("yes", "no", "not_available")))

# Run Spearman correlation
cor_test <- cor.test(paired_data$Vaginal, paired_data$`Penile skin`, method = "spearman")
rho <- round(cor_test$estimate, 2)
p_spear <- format.pval(cor_test$p.value, digits = 3)

# Run linear regression
lm_model <- lm(`Penile skin` ~ Vaginal, data = paired_data)
slope <- round(coef(lm_model)[2], 2)
intercept <- round(coef(lm_model)[1], 2)
p_lm <- format.pval(summary(lm_model)$coefficients[2, 4], digits = 3)
r2 <- round(summary(lm_model)$r.squared, 2)

# Plot
png("Plots/T0_Gspp_load_vaginal_penile_protection.png", width = 2700, height = 1200, res = 300)

ggplot(paired_data, aes(x = Vaginal, y = `Penile skin`)) +
  geom_point(aes(shape = protection), color = "violet", size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_text_repel(aes(label = couple_no), vjust = -0.8, size = 3.5, fontface = "bold", max.overlaps = Inf) +
  annotate("text",
           x = max(paired_data$Vaginal, na.rm = TRUE) * 0.03,
           y = max(paired_data$`Penile skin`, na.rm = TRUE) * 0.9,
           label = paste0("Spearman ρ = ", rho, " (p = ", p_spear, ")\n",
                          "Linear model: y = ", slope, "x + ", intercept,
                          " (p = ", p_lm, ", R² = ", r2, ")"),
           size = 4.5, fontface = "bold",
           hjust = 0, vjust = 1) +
  scale_shape_manual(values = c("yes" = 16, "no" = 17, "not_available" = 8),
                     name = "Protection") +
  labs(
    title = expression(italic("Gardnerella spp.") * " load: vaginal and penile within couples at Timepoint 0"),
    x = "Vaginal (log10 DNA copies/swab)",
    y = "Penile skin (log10 DNA copies/swab)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "right"
  )

dev.off()
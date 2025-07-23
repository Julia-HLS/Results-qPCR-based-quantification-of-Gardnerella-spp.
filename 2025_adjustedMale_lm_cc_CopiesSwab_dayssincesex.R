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
mastertable <- read_excel("2025_mastertable_adjustedMale.xlsx", 
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

# Filter and factor
mastertable <- mastertable %>%
  filter(
    Body_site %in% c("vaginal_fluid", "penile_skin"),
    !is.na(days_since_sex),
    days_since_sex != "not_available"
  ) %>%
  mutate(
    Body_site = factor(Body_site, 
                       levels = c("vaginal_fluid", "penile_skin"),
                       labels = c("Vaginal", "Penile skin")),
    days_grouped = ifelse(as.numeric(days_since_sex) > 14, ">14", as.character(days_since_sex)),
    days_grouped = factor(days_grouped,
                          levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "14", ">14")),
    couple_no = as.factor(couple_no),
    days_since_sex_numeric = as.numeric(days_since_sex)
  )

# Calculate medians
medians <- mastertable %>%
  group_by(Body_site, days_grouped) %>%
  summarise(median_ct = median(GV_log_copies_swab), .groups = "drop") %>%
  mutate(days_numeric = as.numeric(days_grouped))

# Fit linear models
model_vaginal <- lm(GV_log_copies_swab ~ days_since_sex_numeric, data = filter(mastertable, Body_site == "Vaginal"))
model_penile <- lm(GV_log_copies_swab ~ days_since_sex_numeric, data = filter(mastertable, Body_site == "Penile skin"))

# Extract model info
get_model_label <- function(model, label) {
  coef <- coefficients(model)
  slope <- signif(coef[2], 3)
  intercept <- signif(coef[1], 3)
  p_val <- signif(summary(model)$coefficients[2, 4], 3)
  r2 <- signif(summary(model)$r.squared, 3)
  paste0("Linear model: \n y = ", slope, "x + ", intercept, 
         " (p = ", p_val, ", R² = ", r2, ")")
}

label_vaginal <- get_model_label(model_vaginal, "Vaginal")
label_penile <- get_model_label(model_penile, "Penile skin")

# Create tibble for annotations
pval_labels <- tibble(
  Body_site = c("Vaginal", "Penile skin"),
  label = c(label_vaginal, label_penile),
  x = 0.5,
  y = 7
)

# Save plot
png("Plots/adjustedMale_lm_CopiesSwab_dayssincesex.png", width = 2700, height = 1200, res = 300)

# Plot
ggplot(mastertable, aes(x = days_grouped, y = GV_log_copies_swab)) +
  geom_point(aes(color = couple_no), size = 5, alpha = 0.5) +
  geom_smooth(data = medians, aes(x = days_numeric, y = median_ct), 
              method = "lm", se = FALSE, linewidth = 1.2, color = "black") +
  geom_text(data = pval_labels, aes(x = x, y = y, label = label), 
            inherit.aes = FALSE, size = 4, hjust = 0) +
  facet_wrap(~ Body_site, scales = "free_x") +
  theme_minimal() +
  labs(
    title = expression(italic("Gardnerella spp.") * " DNA copy number in relation to days since last sexual intercourse"),
    x = "days since last sexual intercourse",
    y = expression( "Log10 DNA copy number (" *italic("Gardnerella spp.")* ")" ),
    color = "Couple"
  ) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "right"
  )

dev.off()
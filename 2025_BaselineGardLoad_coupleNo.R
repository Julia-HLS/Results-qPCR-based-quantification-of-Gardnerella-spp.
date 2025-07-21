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
mastertable <- read_excel("2025_mastertable_adjusted_allTimepoints.xlsx", 
                          sheet = "mastertable", col_types = c("text", "numeric",
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric",
                                                               "numeric", #SD
                                                               "numeric", "numeric", "numeric", "numeric",
                                                               "text", "text",
                                                               "numeric", "text",
                                                               "text", "text", "text", 
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text"))

# Filter: Only t0 timepoint, non-NA GV_copies_swab, relevant body sites
filtered_data <- mastertable %>%
  filter(
    timepoint == "t0",
    Body_site %in% c("vaginal_fluid", "penile_skin"),
    !is.na(GV_copies_swab)
  ) %>%
  mutate(
    Body_site = factor(Body_site,
                       levels = c("vaginal_fluid", "penile_skin"),
                       labels = c("Vaginal", "Penile skin")),
    couple_no = factor(couple_no),  # ensures categorical x-axis
    ymin = GV_log_copies_swab - SD,
    ymax = GV_log_copies_swab + SD
    )

#open PNG-Grafikgerät
png("Plots/Gspp_coupleID_t0.png", width = 3500, height = 1200, res = 300)

# Vaginal fluid plot
plot_vaginal <- filtered_data %>%
  filter(Body_site == "Vaginal") %>%
  ggplot(aes(x = couple_no, y = GV_log_copies_swab)) +
  geom_point(color = "palevioletred", size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), linewidth = 0.7, width = 0.5, color = "palevioletred", alpha = 0.7) +
  labs(
    title = expression(italic("Gardnerella spp.") * " load at t0: vaginal swabs"),
    x = "Couple ID",
    y = expression( "Log10 DNA copy number (" *italic("Gardnerella spp.")* ")" )
      ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(vjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  )

# Penile skin plot
plot_penile <- filtered_data %>%
  filter(Body_site == "Penile skin") %>%
  ggplot(aes(x = couple_no, y = GV_log_copies_swab)) +
  geom_point(color = "paleturquoise4", size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), linewidth = 0.7, width = 0.5, color = "paleturquoise4", alpha = 0.7) +
  labs(
    title = expression(italic("Gardnerella spp.") * " load at t0: penile skin swabs"),
    x = "Couple ID",
    y =  expression( "Log10 DNA copy number (" *italic("Gardnerella spp.")* ")" )) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(vjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  )

# Arrange both plots side by side
grid.arrange(plot_vaginal, plot_penile, ncol = 2)

dev.off()
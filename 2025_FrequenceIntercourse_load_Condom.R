###########################################################################
# Clear the workspace
rm(list = ls())

# Set working directory
setwd("D:/USER/Julia/Schule/2021_22Fhnw/BA/Auswertung/publicRepo")
getwd()

# Load necessary libraries
library(tidyverse)
library(readxl)
library(ggpubr)
library(gridExtra)
library(ggplot2)
library(readr)
library(FSA)
library(openxlsx)

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

# Create per-couple protection status
protection_per_pair <- mastertable %>%
  filter(!is.na(protection)) %>%
  group_by(couple_no) %>%
  summarise(
    protection_couple = case_when(
      any(protection == "yes") ~ "yes",
      all(protection == "no") ~ "no",
      TRUE ~ "not_available"
    ),
    .groups = "drop"
  )

# Join protection status into main table
mastertable <- mastertable %>%
  left_join(protection_per_pair, by = "couple_no")

# Clean and recode factors
mastertable <- mastertable %>%
  filter(
    Body_site %in% c("vaginal_fluid", "penile_skin"),
    !is.na(frequency_of_sexual_intercourse),
    frequency_of_sexual_intercourse != "not_available",
    protection_couple %in% c("yes", "no")
  ) %>%
  mutate(
    Body_site = factor(Body_site,
                       levels = c("vaginal_fluid", "penile_skin"),
                       labels = c("Vaginal", "Penile skin")),
    frequency_of_sexual_intercourse = factor(frequency_of_sexual_intercourse,
                                             levels = c("never", "rarely", "occasionally", "somewhat_frequent", "frequent", "very_frequent"),
                                             labels = c("Never", "Rarely", "Occasionally", "Somewhat frequent", "Frequent", "Very frequent")),
    frequency_grouped = case_when(
      frequency_of_sexual_intercourse %in% c("Never", "Rarely", "Occasionally") ~ "Never/ Rare",
      TRUE ~ as.character(frequency_of_sexual_intercourse)
    ),
    frequency_grouped = factor(frequency_grouped,
                               levels = c("Never/ Rare", "Somewhat frequent", "Frequent", "Very frequent"))
  )

# Split datasets by protection_couple
data_protected <- mastertable %>% filter(protection_couple == "yes")
data_unprotected <- mastertable %>% filter(protection_couple == "no")

# Function to make the same plot for each dataset
make_boxplot <- function(data, title_suffix) {
  ggplot(data, aes(x = frequency_grouped, y = GV_log_copies_swab, fill = Body_site)) +
    geom_boxplot(position = position_dodge(width = 0.8),
                 outlier.shape = NA, color = "black") +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
                alpha = 0.5, size = 1.5) +
    scale_fill_manual(values = c("Vaginal" = "palevioletred", 
                                 "Penile skin" = "paleturquoise4")) +
    theme_classic() +
    labs(
      title = paste("Gardnerella spp. DNA copies vs. sex frequency\n(", title_suffix, ")"),
      x = "Frequency of sexual intercourse",
      y = expression( "Log10 DNA copy number (" *italic("Gardnerella spp.")* ")" ),
      fill = "Body site"
    ) +
    theme(
      legend.position = "right",
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 12)
    )
}

# Generate plots
plot_protected <- make_boxplot(data_protected, "Protection used")
plot_unprotected <- make_boxplot(data_unprotected, "No protection used")

# Open PNG graphic device
png("Plots/Frequency_load_protection.png", width = 4500, height = 1200, res = 300)

# Plot both boxplots side-by-side into one image
grid.arrange(plot_protected, plot_unprotected, ncol = 2)

# Close the device
dev.off()

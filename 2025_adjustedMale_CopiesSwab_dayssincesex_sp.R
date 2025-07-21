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
#View(mastertable)

# Ensure Body_site is a factor with correct order and labels, rectal swabs are filtered out
mastertable <- mastertable %>%
  filter(
    Body_site %in% c("vaginal_fluid", "penile_skin"),
    
    !is.na(days_since_sex),
    days_since_sex != "not_available"
  ) %>%
  mutate(
    Body_site = factor(Body_site, 
                       levels = c("vaginal_fluid", "penile_skin"),
                       labels = c("Vaginal", "Penile skin")
    ),
    
    # Summarize all data after >14 and ensure correct order of x-axis    
    
    days_grouped = ifelse(as.numeric(days_since_sex) > 14, ">14", as.character(days_since_sex)),
    days_grouped = factor(days_grouped,
                          levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "14", ">14")
    )
  )

# calculate median of days since sex for each body site
medians <- mastertable %>%
  group_by(Body_site, days_grouped) %>%
  summarise(median_ct = median(GV_log_copies_swab), .groups = "drop") %>%
  mutate(days_numeric = as.numeric(days_grouped))

#open PNG-Grafikgerät
png("Plots/adjustedMale_Gspp_last_sex_sp.png", width = 2700, height = 1200, res = 300)

# two Plots with log10 copy number/ swab over time - one for vaginal and one for penile
ggplot(mastertable, aes(x = days_grouped, y = GV_log_copies_swab)) +
  geom_point(aes(color = Body_site), size = 5, alpha = 0.7) +
  geom_smooth(data = medians, aes(x = days_numeric, y = median_ct, color = Body_site), 
              method = "loess", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = c("Vaginal" = "palevioletred", "Penile skin" = "paleturquoise4")) +
  facet_wrap(~ Body_site) +
  
  theme_minimal() +
  
  labs(
    title = expression(italic("Gardnerella spp.") * " DNA copy number in relation to days since last sexual intercourse - adjusted male"),
    
    x = "days since last sexual intercourse",
    y = expression( "Log10 DNA copy number (" *italic("Gardnerella spp.")* ")" ),
    color = "Body site"
  ) +
  
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "none"
  )
dev.off()

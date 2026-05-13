### compare sampling approach 

library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)
library(dplyr)
library(readr)

################################################################################
########################            Define the directory              ##########
################################################################################
dir     <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"

site_number <- "6. SS02_Bowmans-Roberts"
site_name <- "Bowmans-Roberts"

headDir <- paste0(dir, "/work/Output-2/Site-Data/", site_number)

folder <- "/Jackies_working/"
#compiled_folder <- "/2025/"


file <- "compare_Bowmans.xlsx"
worksheet1 <- "Harvest Index jackie"


path_file <- paste0(headDir,folder, file )


df_wide_results <- read_xlsx(path_file, sheet = worksheet1, skip = 0) 

str(df_wide_results)

unique(df_wide_results$`Rows sampled`)


names(df_wide_results)

################################################################################

df_wide_results_long <- df_wide_results %>%
  pivot_longer(
    cols = c(`GS89 Biomass (kg/ha)`,
             `Spike Number (per m²)`,
             TGW,
             `Grain Number (m2)`,
             `Grains per spike`,
             `Hand Cut Yield (g/m2`,
             `Harvest Index`),
    names_to = "trait",
    values_to = "value"
  ) %>%
  filter(!is.na(value))


df_wide_results_long <- df_wide_results_long %>%
  mutate(`Rows sampled` = factor(`Rows sampled`, levels = c("Outside Rows", "All Rows", "Inside Rows")))

str(df_wide_results_long)


df_wide_results_long %>%
  ggplot(aes(x = `Rows sampled`, y = value, fill = `Rows sampled`)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
  geom_jitter(width = 0.15, size = 0.8, alpha = 0.3, colour = "grey30") +
  facet_wrap(~ trait, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c(
      "Outside Rows"               = "#E67A3A",
      "Inside Rows"                = "#4B8BBE",
      "All Rows"                   = "#5BAD72"
    ),
    na.value                       = "grey70"
  ) +
  labs(
    x = "Rows sampled",
    y = NULL,
    fill = "Rows sampled",
    title = "Sampling method at Bowmans"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


plot_name <- "compare_Bowmans_plot.png"

path_plot <- paste0(headDir,folder, plot_name )


ggsave(path_plot, 
       plot = last_plot(), width = 20, height = 15, units = "cm", dpi = 300)

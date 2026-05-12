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

site_number <- "2._SSO2_Copeville-Farley"
site_name <- "Copeville-Farley"

headDir <- paste0(dir, "/work/Output-2/Site-Data/", site_number)

soils_folder <- "/Jackies_working"
compiled_folder <- "/2025/"


file <- "compare.xlsx"
worksheet1 <- "Harvest Index jackie"


path_file <- paste0(headDir, soils_folder, compiled_folder,file )


df_wide_results <- read_xlsx(path_file, sheet = worksheet1, skip = 0) 

str(df_wide_results)

unique(df_wide_results$`Rows sampled`)




################################################################################

df_wide_results_long <- df_wide_results %>%
  pivot_longer(
    cols = c(`Dry Matter GS89(kg/ha)`,
              `Spike Number (per m²)`,
               TGW,
              `Grain Number`, 
             `Grains per spike`, 
             `Hand Cut _Grain yield (g/m2)`,
              `Harvest Index`),
              ,
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
    title = "Sampling method"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

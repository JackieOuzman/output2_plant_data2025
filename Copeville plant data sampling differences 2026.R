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

# file <- "collated Copeville data step 2.xlsx"
# worksheet <- "compare sampling approach"

file <- "compare.xlsx"
worksheet1 <- "compare sampling approach"


path_file <- paste0(headDir, soils_folder, compiled_folder,file )


df_wide_results <- read_xlsx(path_file, sheet = worksheet1, skip = 2) 

str(df_wide_results)

unique(df_wide_results$`rows sampled`)
unique(df_wide_results$`Cals check`)

df_wide_results <- df_wide_results %>%
  mutate(row_group = case_when(
    `rows sampled` == "1, 6"                                        ~ "outside",
    `rows sampled` == "2, 3, 4, 5"                                  ~ "inside",
    `rows sampled` == "1,2,3,4,5,6" & `Cals check` == "No average" ~ "all rows - no average",
    `rows sampled` == "1,2,3,4,5,6" & `Cals check` == "Average"    ~ "all rows - average"
  ))

################################################################################

df_wide_results_long <- df_wide_results %>%
  pivot_longer(
    cols = c(TGW, `Grain Number`, `Spike Number (per m²)`, `Grains per spike`,  `Dry Matter GS89(kg/ha)`),
    names_to = "trait",
    values_to = "value"
  ) %>%
  filter(!is.na(value))



df_wide_results_long %>%
  filter(`rows sampled` == "1,2,3,4,5,6") %>%
  ggplot(aes(x = `Cals check`, y = value, fill = `Cals check`)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
  geom_jitter(width = 0.15, size = 0.8, alpha = 0.3, colour = "grey30") +
  facet_wrap(~ trait, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c(
    "Average"    = "#5BAD72",
    "No average" = "#4B8BBE"
  )) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Cals check",
    title = "All rows: Average vs No Average"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


df_wide_results_long %>%
  ggplot(aes(x = row_group, y = value, fill = row_group)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
  geom_jitter(width = 0.15, size = 0.8, alpha = 0.3, colour = "grey30") +
  facet_wrap(~ trait, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c(
    "outside"               = "#E67A3A",
    "inside"                = "#4B8BBE",
    "all rows - average"    = "#5BAD72",
    "all rows - no average" = "#A8D5A2"
  ), na.value = "grey70") +
  labs(
    x = "Rows sampled",
    y = NULL,
    fill = "Rows sampled",
    title = "Averaged vs No Average"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

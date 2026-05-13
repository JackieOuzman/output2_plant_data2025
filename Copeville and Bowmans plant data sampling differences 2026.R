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
folder <- "/Jackies_working/"


#### Bowmans data #########
site_number1 <- "6. SS02_Bowmans-Roberts"
headDir1 <- paste0(dir, "/work/Output-2/Site-Data/", site_number1)


file1 <- "compare_Bowmans.xlsx"
worksheet1 <- "Harvest Index jackie"

path_file1 <- paste0(headDir1,folder, file1 )
df_wide_results1 <- read_xlsx(path_file1, sheet = worksheet1, skip = 0) 


#### Copeville data #########
site_number <- "2._SSO2_Copeville-Farley"
headDir <- paste0(dir, "/work/Output-2/Site-Data/", site_number)
compiled_folder <- "2025/"
file <- "compare_Copeville.xlsx"

path_file <- paste0(headDir, folder, compiled_folder,file )
df_wide_results <- read_xlsx(path_file, sheet = worksheet1, skip = 0)

Copeville <- df_wide_results
Bowmans <- df_wide_results1

names(Copeville)
names(Bowmans)


###############################################################################
#Work to match clm names

Bowmans <- Bowmans %>%
  rename(
    PlotID = `Plot ID`,
    Row = row,
    `Grain Number` = `Grain Number (m2)`,
    `Dry Matter GS89(kg/ha)` = `GS89 Biomass (kg/ha)`,
    `Hand Cut _Grain yield (g/m2)` = `Hand Cut Yield (g/m2`
  )


##Drop some  clms 
Bowmans <- Bowmans %>%
  select(-c(`Exp`, `Site`, `Year`, 
            `Serpentine Order`, 
            `bay`, `mplots`, 
            `splots`, `block`, 
            `area cut (m2)`, 
            `Ripping_main`,
            `Nutrition_sub`,
            `GS89 Biomass (g/m2)`, `...19`))

Copeville <- Copeville %>%
  select(-c(`Treatment Description`, 
            `RippingTreatment...1`, `Nutrient Treatment...2`,
            `ID_Temp`, `Seeder Rows`, `check name`))

#### Some possible matches 

distinct(Copeville, `Treatment Combo`)
distinct(Bowmans, `Treatments_mainsub`)

distinct(Copeville, `RippingTreatment...4`) #but this has _ and needs removing
distinct(Bowmans, Ripping_main_lab )

distinct(Copeville, `Nutrient Treatment...5`) 
distinct(Bowmans, Nutrition_sub_lab )

#### rename clms
Copeville <- Copeville %>%
  rename(
    TreatmentCombo = `Treatment Combo`,
    RippingTreatmentName = `RippingTreatment...4`,
    NutrientTreatmentName = `Nutrient Treatment...5`
  ) %>%
  mutate(RippingTreatmentName = str_remove_all(RippingTreatmentName, "_"))

Copeville <- Copeville %>%
  mutate(Row = as.numeric(Row))

Bowmans <- Bowmans %>%
  rename(
    TreatmentCombo = `Treatments_mainsub`,
    RippingTreatmentName = Ripping_main_lab,
    NutrientTreatmentName = Nutrition_sub_lab
  ) %>%
  mutate(RippingTreatmentName = str_remove_all(RippingTreatmentName, "_"))


names(Copeville)
names(Bowmans)

### some of Bowmans and Copeville is missing treatment details for the all rows

Bowmans <- Bowmans %>%
  group_by(PlotID) %>%
  fill(TreatmentCombo, RippingTreatmentName, NutrientTreatmentName, .direction = "updown") %>%
  ungroup()

Copeville <- Copeville %>%
  group_by(PlotID) %>%
  fill(TreatmentCombo, RippingTreatmentName, NutrientTreatmentName, .direction = "updown") %>%
  ungroup()

##### Clm with site
Copeville <- Copeville %>%
  mutate(site = "Copeville")

Bowmans <- Bowmans %>%
  mutate(site = "Bowmans")

### bind the df
df_combined <- bind_rows(Copeville, Bowmans)
str(df_combined)


names(Copeville)
names(Bowmans)

## Umm the names are driving me nuts...
#"Tillers density  maturity" =  "Spike Number (per m²)" 
"Biomass GS89" = "Dry Matter GS89(kg/ha)"
"Yield Hand" =  "Hand Cut _Grain yield (g/m2)"
"Harvest Index" = "Harvest Index" 
"TGW" = "TGW"
"Grain Number" =  "Grain Number" 
"Grains per spike" = "Grains per spike"




###long df 
df_combined_long <- df_combined %>%
  pivot_longer(
    cols = c(`Dry Matter GS89(kg/ha)`,
             `Spike Number (per m²)`,
             TGW,
             `Grain Number`,
             `Grains per spike`,
             `Hand Cut _Grain yield (g/m2)`,
             `Harvest Index`),
    names_to = "trait",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(`Rows sampled` = factor(`Rows sampled`, levels = c("Outside Rows", "All Rows", "Inside Rows")))

## Umm the names are driving me nuts...


df_combined_long <- df_combined_long %>%
  mutate(trait = recode(trait,
                        "Spike Number (per m²)"        = "Tillers density maturity",
                        "Dry Matter GS89(kg/ha)"       = "Biomass GS89",
                        "Hand Cut _Grain yield (g/m2)" = "Yield Hand"
  ))



#### 

df_combined_long %>%
  ggplot(aes(x = `Rows sampled`, y = value, fill = `Rows sampled`)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
  geom_jitter(width = 0.15, size = 0.8, alpha = 0.3, colour = "grey30") +
  facet_wrap(~ site + trait, scales = "free_y", ncol = 7)+
  scale_fill_manual(
    values = c(
      "Outside Rows" = "#E67A3A",
      "Inside Rows"  = "#4B8BBE",
      "All Rows"     = "#5BAD72"
    ),
    na.value = "grey70"
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


### I dont like the scale option so lets try this.
library(purrr)
library(cowplot)

sites <- c("Copeville", "Bowmans")
traits <- unique(df_combined_long$trait)

# calculate shared y limits per trait
trait_limits <- df_combined_long %>%
  group_by(trait) %>%
  summarise(ymin = min(value, na.rm = TRUE),
            ymax = max(value, na.rm = TRUE)) %>%
  deframe() %>%  # won't work directly, use as named list below
  { setNames(map2(.$ymin, .$ymax, ~ c(.x, .y)), .$trait) }

# rebuild with shared limits
trait_lims <- df_combined_long %>%
  group_by(trait) %>%
  summarise(ymin = min(value, na.rm = TRUE),
            ymax = max(value, na.rm = TRUE))

plot_list <- map(sites, function(s) {
  map(traits, function(t) {
    ylims <- trait_lims %>% filter(trait == t)
    
    df_combined_long %>%
      filter(trait == t, site == s) %>%
      ggplot(aes(x = `Rows sampled`, y = value, fill = `Rows sampled`)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
      geom_jitter(width = 0.15, size = 0.8, alpha = 0.3, colour = "grey30") +
      scale_y_continuous(limits = c(ylims$ymin, ylims$ymax)) +
      scale_fill_manual(
        values = c(
          "Outside Rows" = "#E67A3A",
          "Inside Rows"  = "#4B8BBE",
          "All Rows"     = "#5BAD72"
        ),
        na.value = "grey70"
      ) +
      labs(x = NULL, y = NULL) +
      ggtitle(if(s == "Copeville") t else NULL) +
      theme_bw() +
      theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 9, face = "bold", hjust = 0.5)
      )
  })
}) %>% flatten()

# assemble
row1 <- plot_grid(plotlist = plot_list[1:7], ncol = 7)
row2 <- plot_grid(plotlist = plot_list[8:14], ncol = 7)

label1 <- ggdraw() + draw_label("Copeville", fontface = "bold", angle = 90)
label2 <- ggdraw() + draw_label("Bowmans", fontface = "bold", angle = 90)

row1_labelled <- plot_grid(label1, row1, ncol = 2, rel_widths = c(0.03, 1))
row2_labelled <- plot_grid(label2, row2, ncol = 2, rel_widths = c(0.03, 1))

legend <- get_legend(
  df_combined_long %>%
    ggplot(aes(x = `Rows sampled`, fill = `Rows sampled`)) +
    geom_boxplot() +
    scale_fill_manual(values = c("Outside Rows" = "#E67A3A", "Inside Rows" = "#4B8BBE", "All Rows" = "#5BAD72")) +
    theme(legend.position = "bottom")
)

plot_grid(row1_labelled, row2_labelled, legend, ncol = 1, rel_heights = c(1, 1, 0.08))


#### save plot and data ####
plot_name <- "Compare_Copeville_and_Bowmans_plot.png"

path_plot <- paste0("H:/Output-2/Site-Data/Jackie_processing_etc/", plot_name )

final_plot <- plot_grid(row1_labelled, row2_labelled, legend, ncol = 1, rel_heights = c(1, 1, 0.08))

ggsave(path_plot, 
       plot = final_plot, width = 40, height = 20, units = "cm", dpi = 300)

data_name <- "Compare_Copeville_and_Bowmans_data.csv"
path_data <- paste0("H:/Output-2/Site-Data/Jackie_processing_etc/", data_name)

write_csv(df_combined_long, path_data)

#Relievers Project

#Data used is from Fangraphs, as of games prior to July 21, 2025

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(ggpubr)
library(paletteer)
library(knitr)
library(gt)
library(tinytex)

starters_25 <- read_csv("starters_7_22.csv")
starters_24 <- read_csv("starters_2024.csv")
starters_23 <- read_csv("starters_2023.csv")
relievers <- read_csv("relievers_7_22.csv")

starters <- rbind(starters_25, starters_24, starters_23)

starters <- starters %>%
  arrange(`ERA-`)

#Getting correlations for starters
starters$`K%` <- as.numeric(sub("%", "", starters$`K%`)) / 100
starters$`O-Swing%` <- as.numeric(sub("%", "", starters$`O-Swing%`)) / 100
starters$`HardHit%` <- as.numeric(sub("%", "", starters$`HardHit%`)) / 100
starters$`Zone%` <- as.numeric(sub("%", "", starters$`Zone%`)) / 100

model1 <- lm(`ERA-` ~ `K%` + `Zone%` + `GB/FB` + `HardHit%`,
            data = starters)
summary(model1)

#model2 <- lm(`ERA-` ~ `K%` + `Zone%` + `GB/FB` + `HardHit%`,
             #data = starters, weights = `Start-IP`)
#summary(model2)

#AIC(model1, model2)

#model2 is just used for adjustments - continue with model1 

starters <- starters %>%
  mutate(`pred_ERA-` = predict(model1))

starters_new <- starters %>%
  mutate(`diff_ERA-` = `pred_ERA-` - `ERA-`)

#changes for relievers
relievers$`K%` <- as.numeric(sub("%", "", relievers$`K%`)) / 100
relievers$`O-Swing%` <- as.numeric(sub("%", "", relievers$`O-Swing%`)) / 100
relievers$`HardHit%` <- as.numeric(sub("%", "", relievers$`HardHit%`)) / 100
relievers$`Zone%` <- as.numeric(sub("%", "", relievers$`Zone%`)) / 100

relievers <- relievers %>%
  mutate(`pred_ERA-` = predict(model1, newdata = relievers))

relievers_new <- relievers %>%
  mutate(`diff_ERA-` = `pred_ERA-` - `ERA-`) %>%
  filter(`pred_ERA-` <= 90 & WHIP <= 1.30 & `GB/FB` >= 1)

#an ERA- of 90 or lower is an above average pitcher per Fangraphs

write_csv(starters, "starters_combined")
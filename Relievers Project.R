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

starters <- read_csv("starters_7_22.csv")
relievers_new <- read_csv("relievers_7_22.csv")

#Getting correlations for starters
starters$`K%` <- as.numeric(sub("%", "", starters$`K%`)) / 100
starters$`O-Swing%` <- as.numeric(sub("%", "", starters$`O-Swing%`)) / 100
starters$`HardHit%` <- as.numeric(sub("%", "", starters$`HardHit%`)) / 100
starters$`Zone%` <- as.numeric(sub("%", "", starters$`Zone%`)) / 100

model1 <- lm(ERA ~ `K%` + `Zone%` + `GB/FB` + `HardHit%`,
            data = starters)
summary(model1)

model2 <- lm(ERA ~ `O-Swing%` + `Zone%` + `GB/FB` + `HardHit%` + `O-Swing%`,
             data = starters)
summary(model2)

AIC(model1, model2)

#model2 is just used for adjustments - continue with model1 

starters$predicted_ERA <- predict(model1, newdata = starters)

starters_new_update <- starters %>%
  mutate(era_diff = predicted_ERA - ERA)

#changes for relievers_new
relievers_new$`K%` <- as.numeric(sub("%", "", relievers_new$`K%`)) / 100
relievers_new$`O-Swing%` <- as.numeric(sub("%", "", relievers_new$`O-Swing%`)) / 100
relievers_new$`HardHit%` <- as.numeric(sub("%", "", relievers_new$`HardHit%`)) / 100
relievers_new$`Zone%` <- as.numeric(sub("%", "", relievers_new$`Zone%`)) / 100

relievers_new$predicted_ERA <- predict(model1, newdata = relievers_new)

relievers_new_update <- relievers_new %>%
  mutate(era_diff = predicted_ERA - ERA) %>%
  filter(era_diff <= 0 & WHIP <= 1.30 & `GB/FB` >= 1)

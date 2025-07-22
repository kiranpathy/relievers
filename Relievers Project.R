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

relievers <- read_csv("relievers_7_21.csv")
starters <- read_csv("starters_7_21.csv")
relievers_new <- read_csv("relievers_new_7_21.csv")

#change percentage columns into decimals
relievers$`GB%` <- as.numeric(sub("%", "", relievers$`GB%`)) / 100
relievers$`FB%` <- as.numeric(sub("%", "", relievers$`FB%`)) / 100
relievers$`HardHit%` <- as.numeric(sub("%", "", relievers$`HardHit%`)) / 100

#Getting correlations for starters
starters$`K%` <- as.numeric(sub("%", "", starters$`K%`)) / 100
starters$`BB%` <- as.numeric(sub("%", "", starters$`BB%`)) / 100
starters$`HardHit%` <- as.numeric(sub("%", "", starters$`HardHit%`)) / 100
starters$`Zone%` <- as.numeric(sub("%", "", starters$`Zone%`)) / 100

model <- lm(ERA ~ `K%` + `Zone%` + `GB/FB` + `HardHit%`,
            data = starters)
summary(model)

#changes for relievers_new
relievers_new$`K%` <- as.numeric(sub("%", "", relievers_new$`K%`)) / 100
relievers_new$`BB%` <- as.numeric(sub("%", "", relievers_new$`BB%`)) / 100
relievers_new$`HardHit%` <- as.numeric(sub("%", "", relievers_new$`HardHit%`)) / 100
relievers_new$`Zone%` <- as.numeric(sub("%", "", relievers_new$`Zone%`)) / 100

relievers_new$predicted_ERA <- predict(model, newdata = relievers_new)

relievers_new_update <- relievers_new %>%
  mutate(era_diff = predicted_ERA - ERA) %>%
  filter(era_diff <= 0 & WHIP <= 1.30 & `GB/FB` >= 1)

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

#change percentage columns into decimals
relievers$`GB%` <- as.numeric(sub("%", "", relievers$`GB%`)) / 100
relievers$`FB%` <- as.numeric(sub("%", "", relievers$`FB%`)) / 100
relievers$`HardHit%` <- as.numeric(sub("%", "", relievers$`HardHit%`)) / 100

#Filtering with some key metrics (general things important for a starter)
new_relievers <- relievers %>%
  filter(`GB%` >= .50 & `BB/9` < 3) %>%
  arrange(`HardHit%`)

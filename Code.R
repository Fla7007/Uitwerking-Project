library(dplyr)
library(tidyr)
library(haven)
library(table1)
library(psych)

#Reading data
raw_data <- read_dta("data.dta")
summary(raw_data)

#Descriptive table (n, mean, sd, min, max)
raw_data %>% 
  select(lnER, lnEnergy:Concentration) %>%
  describe() %>%
  select(n, mean, sd, min, max)

#Descriptive table using table1
dataset_descriptive <- raw_data %>% 
  select(lnER, lnEnergy:Concentration) 
table1(~ lnER + lnEnergy + Coalratio + Oilratio + Gasratio + lnEnergyeff + lnPcca + lnDa + lnSize 
       + lnAge + Own + Export + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
       data=dataset_descriptive, na.rm = T)


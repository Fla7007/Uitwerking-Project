library(dplyr)
library(tidyr)
library(haven)
library(psych)
library(stargazer)
library(plm)
library(AER)
library(huxtable)

#Reading data
raw_data <- read_dta("data.dta")
summary(raw_data)
View(raw_data)

#Descriptive table (n, mean, sd, min, max)
raw_data %>% 
  select(lnER, lnEnergy:Concentration) %>%
  describe() %>%
  select(n, mean, sd, min, max)

#Descriptive table using stargazer (nice layout)
dataset_descriptive <- raw_data %>% 
  select(lnER, lnEnergy:Concentration) 
stargazer(as.data.frame(dataset_descriptive), type = "html", title = "Summary statistics.", digits = 3, out = "Summary_statistics.html")

#Regression models

##Regression model 1 without FE and control variables
model1 <- lm(lnEnergy~lnER, data = raw_data)
summary(model1)
model1_RSE <- coeftest(model1, vcov. = vcovHC, type = "HC1")


##Regression model 2 with control variables and without FE
model2 <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, data = raw_data)
summary(model2)
model2_RSE <- coeftest(model2, vcov. = vcovHC, type = "HC1")

##Regression model 3 with control variables and industry FE
model3 <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = "ind_final",
              model = "within")
summary(model3)
model3_RSE <- coeftest(model3, vcov. = vcovHC, type = "HC1") ###WARNING: difficult to run!

##Regression model 4 with control variables, year FE and firm FE
model4 <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = c("id_in_panel", "year"),
              model = "within")
summary(model4)
model4_RSE <- coeftest(model4, vcov. = vcovHC, type = "HC1")
?plm

##Regression model 5 with control variables and all FE
model5 <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = c("ind_final", "id_in_panel", "year"),
              model = "within")
summary(model5)
model5_RSE <- coeftest(model5, vcov. = vcovHC, type = "HC1") ###WARNING: difficult to run! 

##Overview of all models with RSE
stargazer(model1_RSE, model2_RSE, model3_RSE, model4_RSE, model5_RSE, type = "html", title = "Benchmark regression results.", digits = 3, out = "Benchmark_regression_results.html")

model3 <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + factor(ind_final), 
              data = raw_data)
summary(model3)

library(dplyr)


raw_data_transformed <- raw_data %>%
  group_by(ind_final) %>%
  mutate(across(c(lnEnergy, lnER, lnPcca, lnDa, lnSize, lnAge, Own, Export, 
                  lnOpen, Ind, Endowment, Rail, lnPcgdp, Concentration), 
                ~ . - mean(., na.rm = T), .names = "demeaned_{.col}")) %>%
  ungroup()

model_lm <- lm(demeaned_lnEnergy ~ demeaned_lnER + demeaned_lnPcca + 
                 demeaned_lnDa + demeaned_lnSize + demeaned_lnAge + Own + Export +
                 demeaned_lnOpen + demeaned_Ind + demeaned_Endowment + demeaned_Rail +
                 demeaned_lnPcgdp + demeaned_Concentration, 
               data = raw_data_transformed)
summary(model_lm)

#model 4 via lm
firm_dummies_1 <- model.matrix(~ lnPcca - 1, data = raw_data)
firm_dummies_2 <- model.matrix(~ lnDa - 1, data = raw_data)
firm_dummies_3 <- model.matrix(~ lnSize - 1, data = raw_data)
firm_dummies_4 <- model.matrix(~ lnAge - 1, data = raw_data)
firm_dummies_5 <- model.matrix(~ Own - 1, data = raw_data)
firm_dummies_6 <- model.matrix(~ Export - 1, data = raw_data)
firm_dummies_7 <- model.matrix(~ lnOpen - 1, data = raw_data)
firm_dummies_8 <- model.matrix(~ Ind - 1, data = raw_data)
firm_dummies_9 <- model.matrix(~ Endowment - 1, data = raw_data)
firm_dummies_10 <- model.matrix(~ Rail - 1, data = raw_data)
firm_dummies_11 <- model.matrix(~ lnPcgdp - 1, data = raw_data)
firm_dummies_12 <- model.matrix(~ Concentration - 1, data = raw_data)
year_dummies <- model.matrix(~ year - 1, data = raw_data)

raw_data_transformed_1 <- cbind(raw_data, firm_dummies_1, firm_dummies_2, firm_dummies_3, firm_dummies_4, firm_dummies_5, firm_dummies_6, firm_dummies_7, firm_dummies_8, firm_dummies_9, firm_dummies_10, firm_dummies_11, firm_dummies_12, year_dummies)

model4_lm <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + year, data = raw_data)

summary(model4_lm)







 

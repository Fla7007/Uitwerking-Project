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
model3_RSE <- coeftest(model3, vcov. = vcovHC, type = "HC1")

###Regression model 3 using manually demeaning (not optimal yet)
dataset_model3 <- group_by(raw_data, ind_final)
lnER_means <- summarise(dataset_model3, mean(lnER, na.rm = T))
lnEnergy_means <- summarise(dataset_model3, mean(lnEnergy, na.rm = T))

dataset_model3_demeaned <- raw_data
for (i in 1:dim(lnER_means)[1]) {
  posi <- which(raw_data$ind_final == lnER_means$ind_final[i])
  dataset_model3_demeaned[posi, "lnER"] <- raw_data[posi, "lnER"] - as.numeric(lnER_means[i,2])
  dataset_model3_demeaned[posi, "lnEnergy"] <- raw_data[posi, "lnEnergy"] - as.numeric(lnEnergy_means[i,2])}

coeftest(lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
            + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration
            , dataset_model3_demeaned), vcov. = vcovHC, type = "HC1")

##Regression model 4 with control variables, year FE and firm FE
model4 <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = c("id_in_panel", "year"),
              model = "within")
summary(model4)
model4_RSE <- coeftest(model4, vcov. = vcovHC, type = "HC1")

##Regression model 5 with control variables and all FE
model5 <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = c("ind_final", "id_in_panel", "year"),
              model = "within")
summary(model5)
model5_RSE <- coeftest(model5, vcov. = vcovHC, type = "HC1") 

##Overview of all models with RSE
stargazer(model1_RSE, model2_RSE, model3_RSE, model4_RSE, model5_RSE, type = "html", title = "Benchmark regression results.", digits = 3, out = "Benchmark_regression_results.html")





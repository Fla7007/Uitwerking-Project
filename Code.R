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

##Regression model 3 with control variables and industry FE (using manually demeaning) 
dataset_model3 <- group_by(raw_data, ind_final)
lnER_means <- summarise(dataset_model3, mean_lnER = mean(lnER, na.rm = TRUE))
lnEnergy_means <- summarise(dataset_model3, mean_lnEnergy = mean(lnEnergy, na.rm = TRUE))

dataset_model3_demeaned <- raw_data

for (i in 1:nrow(lnER_means)) {
  posi <- which(raw_data$ind_final == lnER_means$ind_final[i])
  dataset_model3_demeaned[posi, "lnER"] <- raw_data[posi, "lnER"] - lnER_means$mean_lnER[i]
  dataset_model3_demeaned[posi, "lnEnergy"] <- raw_data[posi, "lnEnergy"] - lnEnergy_means$mean_lnEnergy[i]
}

raw_data$ind_final <- factor(raw_data$ind_final)

model <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
            + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
              factor(ind_final), 
            data = raw_data)

robust_se <- vcovHC(model, type = "HC1")
model3_RSE <- coeftest(model, vcov. = robust_se)

##Regression model 4 with control variables, year FE and firm FE
model4 <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = c("id_in_panel", "year"),
              model = "within")
summary(model4)
model4_RSE <- coeftest(model4, vcov. = vcovHC, type = "HC1")

##Regression model 5 with control variables and all FE (industry FE, year FE and firm FE) --> Error: cannot allocate vector of size 44.6 Gb
dataset_model5 <- group_by(raw_data, ind_final)
lnER_means <- summarise(dataset_model5, ind_final = unique(ind_final), mean_lnER = mean(lnER, na.rm = TRUE))
lnEnergy_means <- summarise(dataset_model5, ind_final = unique(ind_final), mean_lnEnergy = mean(lnEnergy, na.rm = TRUE))
dataset_model5_demeaned <- raw_data

for (i in 1:nrow(lnER_means)) {
  posi <- which(raw_data$ind_final == lnER_means$ind_final[i])
  dataset_model5_demeaned$lnER[posi] <- raw_data$lnER[posi] - lnER_means$mean_lnER[i]
  dataset_model5_demeaned$lnEnergy[posi] <- raw_data$lnEnergy[posi] - lnEnergy_means$mean_lnEnergy[i]
}

raw_data$ind_final <- factor(raw_data$ind_final)  
raw_data$year <- factor(raw_data$year)            
raw_data$id_in_panel <- factor(raw_data$id_in_panel)      

model <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
            + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
              factor(ind_final) + factor(year) + factor(id_in_panel), 
            data = raw_data)

robust_se <- vcovHC(model, type = "HC1")
model5_RSE <- coeftest(model, vcov. = robust_se)


##Overview of all models with RSE
stargazer(model1_RSE, model2_RSE, model3_RSE, model4_RSE, type = "html", title = "Benchmark regression results.", digits = 3, out = "Benchmark_regression_results.html")





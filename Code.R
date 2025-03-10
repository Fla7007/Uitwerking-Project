library(dplyr)
library(tidyr)
library(haven)
library(psych)
library(AER)


#Reading data
raw_data <- read_dta("data.dta")
summary(raw_data)
View(raw_data)

#######################
###Descriptive table###
#######################

#using select and describe (n, mean, sd, min, max)
raw_data %>% 
  select(lnER, lnEnergy:Concentration) %>%
  describe() %>%
  select(n, mean, sd, min, max)

#using stargazer
library(stargazer)
dataset_descriptive <- raw_data %>% 
  select(lnER, lnEnergy:Concentration) 
stargazer(as.data.frame(dataset_descriptive), type = "html", title = "Summary statistics.", digits = 3, out = "Summary_statistics.html")

#######################
###Regression models###
#######################

### Model 1 ###
# Model 1 (without FE or control variables)
model1 <- lm(lnEnergy~lnER, data = raw_data)
summary(model1)
model1_RSE <- coeftest(model1, vcov. = vcovHC, type = "HC1")
## Gives the same point estimators and standard errors as in the original paper, no R²

### Model 2 ###
# Model 2 (with control variables, without FE)
model2 <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
             + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, data = raw_data)
summary(model2)
model2_RSE <- coeftest(model2, vcov. = vcovHC, type = "HC1")
## Gives the same point estimators and standard errors as in the original paper, no R²

### Model 3 ###
# Model 3 using plm (with control variables and industry FE)
library(plm)
model3 <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = c("ind_final"),
              model = "within")
summary(model3)
model3_RSE <- coeftest(model3, vcov. = vcovHC, type = "HC1")
## Does NOT give the same results as in the orginal paper: 
## there is no constant, the standard errors aren't the same and despite the values of the point estimators 
## being the same, the significant for some of them differ 

# Model 3 using manually demeaning (with control variables and industry FE) 
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
## Gives the same point estimators and standard errors as in the original paper. 
## Only the intercept is wrong since we've added a factor of ind_final to incorporate the industry FE 

# Model 3 using feols (with control variables and industry FE)
library(fixest)
model3_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | ind_final, 
                    data = raw_data,
                    vcov = "HC1")
## Correct point estimators and SE, no constant (due to FE)

### Model 4 ###
# Model 4 using plm (with control variables, year FE and firm FE) 
model4 <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = c("id_in_panel", "year"),
              model = "within")
summary(model4)
model4_RSE <- coeftest(model4, vcov. = vcovHC, type = "HC1")
## Does NOT give the same results as in the orginal paper: 
## there is no constant and the values of the point estimators and standard errors aren't the same

# Model 4 using manually demeaning (with control variables, year FE and firm FE) 
dataset_model4 <- group_by(raw_data,id_in_panel)
lnER_means <- summarise(dataset_model4, mean_lnER = mean(lnER, na.rm = TRUE))
lnEnergy_means <- summarise(dataset_model4, mean_lnEnergy = mean(lnEnergy, na.rm = TRUE))

dataset_model4_demeaned <- raw_data

for (i in 1:nrow(lnER_means)) {
  posi <- which(raw_data$id_in_panel == lnER_means$id_in_panel[i])
  dataset_model4_demeaned[posi, "lnER"] <- raw_data[posi, "lnER"] - lnER_means$mean_lnER[i]
  dataset_model4_demeaned[posi, "lnEnergy"] <- raw_data[posi, "lnEnergy"] - lnEnergy_means$mean_lnEnergy[i]
}

model4 <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
             + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
               factor(year), factor(id_in_panel), 
             data = dataset_model4_demeaned)

robust_se <- vcovHC(model4, type = "HC1")
model4_RSE <- coeftest(model4, vcov. = robust_se)
## Does NOT give the same results as in the orginal paper: 
## the values of the point estimators and standard errors aren't the same

# Model 4 using feols (with control variables, year FE and firm FE)
model4_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year, 
                data = raw_data,
                vcov = "HC1")
## Correct point estimators, SE sometimes slightly differ, no constant (due to FE), used much more observations (different R²)

### Model 5 ###
# Model 5 using plm (with control variables and all FE)  
model5 <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = c("id_in_panel", "year","ind_final"),
              model = "within")
summary(model5)
model5_RSE <- coeftest(model5, vcov. = vcovHC, type = "HC1")
## Does NOT give the same results as in the orginal paper: 
## there is no constant and the values of the point estimators and standard errors aren't the same
## We also observe that this code gives the exact same output as the plm of model 4

# Model 5 using manually demeaning (with control variables and all FE) 
dataset_model5 <- group_by(raw_data, ind_final)
lnER_means <- summarise(dataset_model5, mean_lnER = mean(lnER, na.rm = TRUE))
lnEnergy_means <- summarise(dataset_model5, mean_lnEnergy = mean(lnEnergy, na.rm = TRUE))
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
## We were unable to run this code since our computers cannot compute it for such a large dataset
## Error: cannot allocate vector of size 44.6 Gb

# Model 5 using feols (with control variables and all FE)
model5_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                    data = raw_data,
                    vcov = "HC1")
## Correct point estimators, SE sometimes slightly differ, no constant (due to FE), used much more observations (different R²) 


### Overview ###
#Using stargazer (Models used: model1, model2, model3 demeaning, model4 plm, model5 plm)
stargazer(model1_RSE, model2_RSE, model3_RSE, model4_RSE, model5_RSE, type = "html", 
          title = "Benchmark regression results.", digits = 3, out = "Benchmark_regression_results.html")
## Does NOT work with the models made by feols

#Using huxreg (Model used: model1, model2, model3 feols, model4 feols, model5 feols)
library(huxtable)
huxreg(model1_RSE, model2_RSE,model3_RSE, model4_RSE, model5_RSE, 
       statistics = c("N. obs." = "nobs", "R squared" = "r.squared"))

#Using modelsummary
library(modelsummary)
library(webshot2)

model_list <- list(
  "model 1" = model1_RSE,
  "model 2" = model2_RSE,
  "model 3" = model3_RSE,
  "model 4" = model4_RSE,
  "model 5" = model5_RSE)

modelsummary(model_list,
             stars = TRUE,
             gof_omit = "IC|Log|Adj|F|RMSE|R2 Within",
             output = "Benchmark regression results (lm + feols).png",
             title = "Benchmark regression results.") 



### Eventueel nuttig bij robustness analyse
# Alternative model 4 with clustered SE
model4 <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year, 
                data = raw_data,
                vcov = "cluster")
# Alternative model 5 with clustered SE
model4 <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                data = raw_data,
                vcov = "cluster")
#Loading necessary packages
library(dplyr)
library(tidyr)
library(haven)
library(psych)
library(AER)
library(stargazer)
library(plm)
library(fixest)
library(lfe)
library(modelsummary)
library(webshot2)
library(huxtable)
library(Hmisc)
library(ggplot2)

#Reading data
raw_data <- read_dta("data.dta")
summary(raw_data)
View(raw_data)

raw_data2 <- read_dta("20012009firm.dta")
summary(raw_data2)
View(raw_data2)

#######################
###Descriptive table###
#######################

#using select and describe (n, mean, sd, min, max)
raw_data %>% 
  select(lnER, lnEnergy:Concentration) %>%
  describe() %>%
  select(n, mean, sd, min, max)

#using stargazer
dataset_descriptive <- raw_data %>% 
  select(lnER, lnEnergy:Concentration) 
stargazer(as.data.frame(dataset_descriptive), type = "html", title = "Summary statistics.", digits = 3, out = "Summary_statistics.html")

#################################
###Regression models (Table 2)###
#################################

### Model 1 ###
# Model 1 (without FE or control variables)
model1 <- lm(lnEnergy~lnER, data = raw_data)
model1_RSE <- coeftest(model1, vcov. = vcovHC, type = "HC1")
## Gives the same point estimators and standard errors as in the original paper, no R²

### Model 2 ###
# Model 2 (with control variables, without FE)
model2 <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
             + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, data = raw_data)
model2_RSE <- coeftest(model2, vcov. = vcovHC, type = "HC1")
## Gives the same point estimators and standard errors as in the original paper, no R²


### Model 3 ###
# Model 3 using feols (with control variables and industry FE)
model3_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | ind_final, 
                    data = raw_data,
                    vcov = "HC1")
## Correct point estimators and SE, no constant (due to FE)


### Model 4 ###
# Model 4 using feols (with control variables, year FE and firm FE)
model4_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year, 
                    data = raw_data,
                    vcov = "HC1")
## Correct point estimators, SE sometimes slightly differ, no constant (due to FE), used much more observations (different R²)


### Model 5 ###
# using feols (with control variables and all FE)
model5_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                    data = raw_data,
                    vcov = "HC1")
## Correct point estimators, SE sometimes slightly differ, no constant (due to FE), used much more observations (different R²) 


### Overview ###
#Using huxreg 
huxreg(model1_RSE, model2_RSE,model3_feols_RSE, model4_feols_RSE, model5_feols_RSE, 
       statistics = c("N. obs." = "nobs", "R squared" = "r.squared"))

#Using modelsummary
models_table2 <- list(
  "model 1" = model1_RSE,
  "model 2" = model2_RSE,
  "model 3" = model3_feols_RSE,
  "model 4" = model4_feols_RSE,
  "model 5" = model5_feols_RSE)

modelsummary(models_table2,
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",
             output = "Table 2. Benchmark regression results.png",
             title = "Table 2. Benchmark regression results") 



### Additional attempts ###
## Model 3 ##
#Using plm (with control variables and industry FE)
model3_plm <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                  + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
                  data = raw_data,
                  index = c("ind_final"),
                  model = "within")
model3_plm_RSE <- coeftest(model3_plm, vcov. = vcovHC, type = "HC1")
## Does NOT give the same results as in the orginal paper: 
## there is no constant, the standard errors aren't the same and despite the values of the point estimators 
## being the same, the significant for some of them differ 

#Using manually demeaning (with control variables and industry FE) 
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

model3_demean <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
                      factor(ind_final), 
                    data = raw_data)

robust_se <- vcovHC(model3_demean, type = "HC1")
model3_demean_RSE <- coeftest(model3_demean, vcov. = robust_se)
## Gives the same point estimators and standard errors as in the original paper. 
## Only the intercept is wrong since we've added a factor of ind_final to incorporate the industry FE 

#Using felm (with control variables, and industry FE)
model3_felm <- felm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | ind_final, 
                    data = raw_data)
model3_felm_RSE <- coeftest(model3_felm, vcov = vcovHC(model3_felm, type="HC1"))
## Correct point estimators and SE, no constant (due to FE)


## Model 4 ##
#Using plm (with control variables, year FE and firm FE) 
model4_plm <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                  + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
                  data = raw_data,
                  index = c("id_in_panel", "year"),
                  model = "within")
summary(model4)
model4_plm_RSE <- coeftest(model4_plm, vcov. = vcovHC, type = "HC1")
## Does NOT give the same results as in the orginal paper: 
## there is no constant and the values of the point estimators and standard errors aren't the same

#Using manually demeaning (with control variables, year FE and firm FE) 
dataset_model4 <- group_by(raw_data,id_in_panel)
lnER_means <- summarise(dataset_model4, mean_lnER = mean(lnER, na.rm = TRUE))
lnEnergy_means <- summarise(dataset_model4, mean_lnEnergy = mean(lnEnergy, na.rm = TRUE))

dataset_model4_demeaned <- raw_data

for (i in 1:nrow(lnER_means)) {
  posi <- which(raw_data$id_in_panel == lnER_means$id_in_panel[i])
  dataset_model4_demeaned[posi, "lnER"] <- raw_data[posi, "lnER"] - lnER_means$mean_lnER[i]
  dataset_model4_demeaned[posi, "lnEnergy"] <- raw_data[posi, "lnEnergy"] - lnEnergy_means$mean_lnEnergy[i]
}

model4_demean <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
                      factor(year), factor(id_in_panel), 
                    data = dataset_model4_demeaned)

robust_se <- vcovHC(model4_demean, type = "HC1")
model4_demean_RSE <- coeftest(model4_demean, vcov. = robust_se)
## Does NOT give the same results as in the original paper: 
## the values of the point estimators and standard errors aren't the same

#Using felm (with control variables, year FE and firm FE)
model4_felm <- felm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year, 
                    data = raw_data)
model4_felm_RSE <- coeftest(model4_felm, vcov = vcovHC(model4_felm, type="HC1"))
## Correct point estimators, SE sometimes slightly differ, no constant (due to FE), used much more observations


## Model 5 ##
#Using plm (with control variables and all FE)  
model5_plm <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                  + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
                  data = raw_data,
                  index = c("id_in_panel", "year","ind_final"),
                  model = "within")
summary(model5_plm)
model5_plm_RSE <- coeftest(model5_plm, vcov. = vcovHC, type = "HC1")
## Does NOT give the same results as in the original paper: 
## there is no constant and the values of the point estimators and standard errors aren't the same
## We also observe that this code gives the exact same output as the plm of model 4

#Using manually demeaning (with control variables and all FE) 
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

model5_demean <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
                      factor(ind_final) + factor(year) + factor(id_in_panel), 
                    data = raw_data)

robust_se <- vcovHC(model5_demean, type = "HC1")
model5_demean_RSE <- coeftest(model5_demean, vcov. = robust_se)
## We were unable to run this code since our computers cannot compute it for such a large dataset
## Error: cannot allocate vector of size 44.6 Gb

#Using felm (with control variables and all FE)
model5_felm <- felm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                    data = raw_data)
model5_felm_RSE <- coeftest(model5_felm, vcov = vcovHC(model5_felm, type="HC1"))
## Correct point estimators, SE sometimes slightly differ, no constant (due to FE), used much more observations

## Overview
#Using stargazer
stargazer(model1_RSE, model2_RSE, model3_demean_RSE, model4_plm_RSE, model5_plm_RSE, type = "html", 
          title = "Benchmark regression results.", digits = 3, out = "Benchmark_regression_results.html")
## Stargazer does NOT work with the models made by feols



##########################
### Robustness analysis###
##########################

### Control variables ###
# Correlation/p-value table
library(writexl)
cor <- round(rcorr(as.matrix(raw_data))$r, 3)
cor[, c("lnEnergy", "lnER")]
cor_selected <- as.data.frame(cor[, c("lnEnergy", "lnER")])
cor_selected <- cbind(Variable = rownames(cor_selected), cor_selected)  # Add row names as a column
write_xlsx(cor_selected, "correlation_table.xlsx")  # Excel file

cor_pvalue <- round(rcorr(as.matrix(raw_data))$P,3)
cor_pvalue[, c("lnEnergy", "lnER")]
cor_pvalue_selected <- as.data.frame(cor_pvalue[, c("lnEnergy", "lnER")])
cor_pvalue_selected <- cbind(Variable = rownames(cor_pvalue_selected), cor_pvalue_selected)  # Add row names as a column
write_xlsx(cor_pvalue_selected, "pvalue_table.xlsx")  # Excel file

## Based on this p-value table, almost all variables in our data set are significantly correlated with lnEnergy (Y) and lnER (X). 
## So actually all variables might be chosen as control variables, except for Gasratio and TargetDummy
## HOWEVER: since we are dealing with a large data set, this method might not be ideal so these findings should not be implemented

# Double Lasso
library(hdm)
lasso_Y <- rlasso(lnEnergy ~ lnER + citycode + age + L + area_final + Coalratio + Oilratio + lnEnergyeff + lnPcca + lnDa +
                    lnSize + lnAge + Own + Export + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration +
                    Lnexport + LnERSO2 + LnERCOD + SO2removalrate + reductionper + codtarget + Lncoalcons +
                    Lnpollutint2005 + Lnenergyint2005 + Lnpollutint2001 + Lnenergyint2001 + HighPollution + Largefirm +
                    energy_intensive + Lnfirmenergypre05 + Gasratio + TargetDummy,
                  data = raw_data)

lasso_X <- rlasso(lnER ~ citycode + age + L + area_final + Coalratio + Oilratio + lnEnergyeff + lnPcca + lnDa +
                    lnSize + lnAge + Own + Export + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration +
                    Lnexport + LnERSO2 + LnERCOD + SO2removalrate + reductionper + codtarget + Lncoalcons +
                    Lnpollutint2005 + Lnenergyint2005 + Lnpollutint2001 + Lnenergyint2001 + HighPollution + Largefirm +
                    energy_intensive + Lnfirmenergypre05 + Gasratio + TargetDummy,
                  data = raw_data)
coef(lasso_Y)
coef(lasso_X)

vars_Y <- names(coef(lasso_Y)[coef(lasso_Y) != 0])
vars_X <- names(coef(lasso_X)[coef(lasso_X) != 0])

vars_Y <- setdiff(vars_Y, "(Intercept)")
vars_X <- setdiff(vars_X, "(Intercept)")

selected_CV <- intersect(vars_Y, vars_X)
print(selected_CV)
## Variables with non-zero coefficients in both models: lnEnergyeff, lnDa, lnSize, Own, lnPcgdp, Concentration, LnERCOD, Lncoalcons, HighPollution, Largefirm, energy_intensive, Gasratio, TargetDummy

# New model with other selection of control variables, with all FE
model5_new_feols_RSE <- feols(as.formula(paste("lnEnergy ~ lnER +", paste(selected_CV, collapse = " + "),"|id_in_panel + year + ind_final")), 
                              data = raw_data,
                              vcov = "HC1")

# Comparison with original model
model_comparison <- list("Original model" = model5_feols_RSE, "Double Lasso selected CVs" = model5_new_feols_RSE)
modelsummary(model_comparison,
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             gof_omit = "Adj|BIC|F|Std.Errors|R2 Within",
             output = "Original vs new model 5.png",
             title = "Model 5 with other CV")
## Compared to the original model, the new model gives a better AIC value and a better RMSE (both lower) 
## So based on that, the new created model is prefered over the original one.
## In the new created model lnER is not significantly related to lnEnergy


                    ## using rlasso with fixed effects
                    NA_obs_deleted <- na.omit(raw_data) #needed so resid_CV can be transformed to a data frame, otherwise the variables wouldn't have the same length
                    
                    # Define dependent variable (Y), independent variable (X), and fixed effects
                    Y <- "lnEnergy"
                    X <- "lnER"
                    FE <- "id_in_panel + year + ind_final"  # Fixed effects
                    CV <- c("citycode", "age", "L", "area_final", "Coalratio", "Oilratio", "lnEnergyeff", "lnPcca", "lnDa", "lnSize",
                            "lnAge", "Own", "Export", "lnOpen", "Ind", "Endowment", "Rail", "lnPcgdp", "Concentration", "Lnexport", "LnERSO2", 
                            "LnERCOD", "SO2removalrate", "reductionper", "codtarget", "Lncoalcons", "Lnpollutint2005", "Lnenergyint2005",
                            "Lnpollutint2001", "Lnenergyint2001", "HighPollution", "Largefirm", "energy_intensive", "Lnfirmenergypre05",
                            "Gasratio", "TargetDummy")
                    
                    # Step 1: Residualize Y (lnEnergy) by removing fixed effects
                    formula_str_Y <- paste(Y, "~ 1 |", FE)
                    resid_Y <- residuals(feols(as.formula(formula_str_Y), data = NA_obs_deleted))
                    
                    # Step 2: Residualize X (lnER) by removing fixed effects
                    formula_str_X <- paste(X, "~ 1 |", FE)
                    resid_X <- residuals(feols(as.formula(formula_str_X), data = NA_obs_deleted))
                  
                    # Step 3: Residualize Control Variables
                    resid_CV <- lapply(CV, function(var) {
                      residuals(feols(as.formula(paste(var, "~ 1 |", FE)), data = NA_obs_deleted))
                    })
                    resid_CV <- do.call(cbind, resid_CV)  # Convert list to matrix
                    colnames(resid_CV) <- CV  # Assign original control variable names
                    
                    
                    # Step 4: Lasso models
                    lasso_Y <- rlasso(resid_Y ~ resid_X + resid_CV)
                    lasso_X <- rlasso(resid_X ~ resid_CV)
                    
                    coef(lasso_X) #non-zero coef: lnAge, Ind, Endowment, Rail, lnPcgdp, Lnexport, LnERSO2, LnERCOD, SO2removalrate, reductionper
                    coef(lasso_Y) #non-zero coef: area_final, Coalratio, lnEnergyeff, lnPcca, lnSize, lnAge, Ind, Lnexport, SO2removalrate, Lncoalcons, Lnenergyint2005, Largefirm, Gasratio
                    #non-zero coef in both: lnAge, Ind, Lnexport, SO2removalrate  
                    
                   
                    
                    ### Code from Github Copilot
                    # Extract the response variable (y) and predictor variables (X)
                    NA_obs_deleted <- na.omit(raw_data)
                    y <- NA_obs_deleted$lnEnergy
                    X <- as.matrix(NA_obs_deleted[, -1])
                    fixed_effects <- as.matrix(NA_obs_deleted[, c("id_in_panel", "year", "ind_final")])
                    
                    # Standardize the predictor variables
                    X <- scale(X)
                  
                    # Combine fixed effects with the original predictors
                    X_with_fe <- cbind(X, fixed_effects)
                    
                    # Perform Lasso regression with fixed effects
                    cv_fit_fe <- cv.glmnet(X_with_fe, y, alpha = 1, nfolds = 10)
                    best_lambda_fe <- cv_fit_fe$lambda.min
                    lasso_model_fe <- glmnet(X_with_fe, y, alpha = 1, lambda = best_lambda_fe)
                    
                    # Print the coefficients of the final Lasso model with fixed effects
                    print(coef(lasso_model_fe)) # all coef are zero
                                 

### Clustered SE ###
# Alternative model 3 with clusterd SE
model3_feols_clustered <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                          + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | ind_final, 
                          data = raw_data,
                          vcov = "cluster")
# Alternative model 4 with clustered SE
model4_feols_clustered <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                          + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year, 
                          data = raw_data,
                          vcov = "cluster")

# Alternative model 5 with clustered SE
model5_feols_clustered <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                          + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                          data = raw_data,
                          vcov = "cluster")

# Comparison with original models
models_SEvariation <- list(
  "Robust SE (3)" = model3_feols_RSE, 
  "Clustered SE (3)" = model3_feols_clustered,
  "Robust SE (4)" = model4_feols_RSE, 
  "Clustered SE (4)" = model4_feols_clustered,
  "Robust SE (5)" = model5_feols_RSE, 
  "Clustered SE (5)" = model5_feols_clustered)

modelsummary(models_SEvariation,
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             add_rows = data.frame(
               rowname = c("Control variables"),
               `Robust SE (3)` = c("X"),
               `Clustered SE (3)` = c("X"),
               `Robust SE (4)` = c("X"),
               `Clustered SE (4)` = c("X"),
               `Robust SE (5)` = c("X"),
               `Clustered SE (5)` = c("X")),
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",
             title = "Robust vs clustered SE",
             output = "Robust vs clustered SE.png")


### Density plots ###

#lnER
plot(density(na.omit(raw_data$lnER)))  #Plots the density estimate of X, showing its distribution
plot(density(na.omit(exp(raw_data$lnER)))) 

#lnEnergy
plot(density(na.omit(raw_data$lnEnergy)))  #Plots the density of Y.
plot(density(na.omit(exp(raw_data$lnEnergy))))

#lnPcca
plot(density(na.omit(raw_data$lnPcca)))
plot(density(na.omit(exp(raw_data$lnPcca)))) 

#lnDa 
plot(density(na.omit(raw_data$lnDa)))
plot(density(na.omit(exp(raw_data$lnDa)))) 

#lnSize 
plot(density(na.omit(raw_data$lnSize)))
plot(density(na.omit(exp(raw_data$lnSize)))) 

#lnAge 
plot(density(na.omit(raw_data$lnAge)))
plot(density(na.omit(exp(raw_data$lnAge)))) 

#Own 
plot(density(na.omit(raw_data$Own)))
plot(density(na.omit(log(raw_data$Own))))
plot(density(log(na.omit(raw_data$Own))))

#Export
plot(density(na.omit(raw_data$Export)))
plot(density(log(na.omit(raw_data$Export))))
plot(density(na.omit(raw_data$lnEnergy)))

#lnOpen
plot(density(na.omit(raw_data$lnOpen)))
plot(density(na.omit(exp(raw_data$lnOpen)))) 

#Ind 
plot(density(na.omit(raw_data$Ind)))
plot(density(log(na.omit(raw_data$Ind))))

#Endowment
plot(density(na.omit(raw_data$Endowment)))
plot(density(log(na.omit(raw_data$Endowment))))
               
#Rail
plot(density(na.omit(raw_data$Rail)))
plot(density(log(na.omit(raw_data$Rail))))
               
#lnPcgdp
plot(density(na.omit(raw_data$lnPcgdp)))
plot(density(na.omit(exp(raw_data$lnPcgdp)))) 

#Concentration
plot(density(na.omit(raw_data$Concentration)))
plot(density(log(na.omit(raw_data$Concentration))))


### New regression models with other combinations of fixed effects ###
#Extra model 1 using feols (with control variables and only year FE)
extramodel1_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                               + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | year, 
                               data = raw_data,
                               vcov = "HC1")

#Extra model 2 using feols (with control variables and only firm FE)
extramodel2_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                               + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel, 
                               data = raw_data,
                               vcov = "HC1")

#Extra model 3 using feols (with control variables, industry FE and year FE)
extramodel3_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                               + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | year + ind_final, 
                               data = raw_data,
                               vcov = "HC1")


#Extra model 4 using feols (with control variables, industry FE and firm FE)
extramodel4_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                          + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + ind_final, 
                          data = raw_data,
                          vcov = "HC1")
# Overview #
models_FE <- list(
  "No FE" = model2_RSE,
  "Year FE" = extramodel1_feols_RSE, 
  "Firm FE" = extramodel2_feols_RSE,
  "Industry FE" = model3_feols_RSE,
  "Year + Firm FE" = model4_feols_RSE,
  "Firm + Industry FE" = extramodel4_feols_RSE,
  "Year + Industry FE" = extramodel3_feols_RSE,
  "All FE" = model5_feols_RSE)

modelsummary(models_FE,
             coef_map = c("lnER" = "lnER"),
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             add_rows = data.frame(
               rowname = c("Control variables"),
               `No FE` = c("X"),
               `Year FE` = c("X"),
               `Firm FE` = c("X"),
               `Industry FE` = c("X"),
               `Year + Firm FE` = c("X"),
               `Firm + Industry FE` = c("X"),
               `Year + Industry FE` = c("X"),
               `All FE` = c("X")),
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",
             title = "Different combinations of FE",
             output = "Different combinations of FE.png")

### Specification curve analysis ###
Y <- "lnEnergy"
X <- "lnER"
FE <- "id_in_panel + year + ind_final"  # Fixed effects
CV <- c("lnPcca", "lnDa", "lnSize", "lnAge", "Own", "Export", "lnOpen", "Endowment", "Rail", "lnPcgdp", "Ind", "Concentration")

# Using specr with parallelisation
library(fixest)
library(specr)
library(furrr)

#Taking a random sample#
data_NAY <- raw_data %>% drop_na(lnEnergy) #remove NA from the Y variable
set.seed(123)
sample_data <- data_NAY %>%
  group_by(ind_final, id_in_panel) %>%
  sample_frac(0.25) %>%  # Take a random sample of 20% of the firms for each industry
  ungroup()  # Remove the grouping to return the full data

#Setting parallelisation#
plan(strategy = multisession, workers = 5)

#LM (without FE)#
#All possible combinations of the 12 selected control variables
specslm <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "lm",
  controls = CV) #4096 different models

plot(specslm) #not readable, so not useful
resultslm <- specr(specslm, .progress = TRUE)
plot(resultslm)

    ###EXTRA: zooming in on the extremes
    bottom <- resultslm$data %>% arrange(estimate) %>% slice(1:200) %>% mutate(extreme = "Most Negative") #5% of the total models with the lowest estimate
    top <- resultslm$data %>% arrange(desc(estimate)) %>% slice(1:200) %>% mutate(extreme = "Most Positive") #5% of the total models with the highest estimate
    extremes <- bind_rows(bottom, top)
    
    freqs <- extremes %>%
      separate_rows(controls, sep = "\\+") %>% # Split 'controls' into multiple rows
      mutate(controls = trimws(controls)) %>% # Trim whitespace
      count(extreme, controls) %>% # Count occurrences
      group_by(extreme) %>% 
      mutate(inclusion_rate = n / 200) %>% # Calculate inclusion rate
      ungroup()
    
    ggplot(freqs, aes(x = reorder(controls, inclusion_rate), y = inclusion_rate, fill = extreme)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(title = paste("Control Inclusion in Top/Bottom", 200, "Specs"),
           x = "Control", y = "Inclusion Rate") +
      theme_minimal()
    #CONCLUSION: 
      ## lnPcca is always included in the most negative models and never in the most positive ones => suggests a negative polarising effect 
      ## lnSize and lnOpen are often included in the most positive models and never in the most negative ones => suggests a positive polarising effect
      ## Endowment and Concentration are included in both groups. However, they are more present in the positive models than in the negative ones
      ## lnPcgdp is only included in the most positive models, but in less than 50%
      ## All other control variables are equaly present in both groups

#Possible combinations of the firm-level CVs with the other CVs always included
specslm1 <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "lm",
  controls = CV[1:6],
  add_to_formula = "lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration") #64 different models

plot(specslm1)
resultslm1 <- specr(specslm1, .progress = TRUE)
plot(resultslm1)

#Possible combinations of the last six CV with the first six CV always included
specslm2 <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "lm",
  controls = CV[7:12],
  add_to_formula = "lnPcca + lnDa + lnSize + lnAge + Own + Export") #64 different models

plot(specslm2)
resultslm2 <- specr(specslm2, .progress = TRUE)
plot(resultslm2)

#FEOLS#
feols_formula <- function(formula, data) {
  formula <- as.formula(paste0(formula, " | ", FE))
  fixest::feols(formula, data)}   #setting needed formula with feols (FE always included)

opts <- furrr_options(
  globals = list(feols_formula = feols_formula, FE = FE),
  seed = TRUE) #needed since we use parallelisation

#All possible combinations of the 12 selected control variables 
specsfeols <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "feols_formula",
  controls = CV) #4096 different models

plot(specsfeols) #not readable, so not useful
resultsfeols <- specr(specsfeols, .options = opts, .progress = TRUE) #takes +/- 2h to run
resultsfeols <- readRDS("resultsfeols.RData") #The specr object was saved after running so it can easily be reloaded without fully running again
plot(resultsfeols)

        ###EXTRA: zooming in on the extremes
        bottom <- resultsfeols$data %>% arrange(estimate) %>% slice(1:200) %>% mutate(extreme = "Most Negative") #5% of the total models with the lowest estimate
        top <- resultsfeols$data %>% arrange(desc(estimate)) %>% slice(1:200) %>% mutate(extreme = "Most Positive") #5% of the total models with the highest estimate
        extremes <- bind_rows(bottom, top)
        
        freqs <- extremes %>%
          separate_rows(controls, sep = "\\+") %>% # Split 'controls' into multiple rows
          mutate(controls = trimws(controls)) %>% # Trim whitespace
          count(extreme, controls) %>% # Count occurrences
          group_by(extreme) %>% 
          mutate(inclusion_rate = n / 200) %>% # Calculate inclusion rate
          ungroup()
        
        ggplot(freqs, aes(x = reorder(controls, inclusion_rate), y = inclusion_rate, fill = extreme)) +
          geom_col(position = "dodge") +
          coord_flip() +
          labs(title = paste("Control Inclusion in Top/Bottom", 200, "Specs"),
               x = "Control", y = "Inclusion Rate") +
          theme_minimal()
        #CONCLUSION: 
    
        
#All possible combinations of the selected CV with Double Lasso
sample_data_changed <- sample_data %>%
  rename(Lnenergyeff = lnEnergyeff) 
selected_CV_changed <- dplyr::recode(selected_CV, "lnEnergyeff" = "Lnenergyeff") #making sure lnEnergyeff is integrated 

specsfeols_DLCV <- setup(
          data = sample_data_changed,
          y = Y,
          x = X,
          model = "feols_formula",
          controls = selected_CV_changed) #8192 different models, selected_CV from Double Lasso
        
plot(specsfeols_DLCV)
resultsfeols_DLCV <- specr(specsfeols_DLCV, .options = opts, .progress = TRUE)
saveRDS(resultsfeols_DLCV, file = "resultsfeols_DLCV.RData")
resultsfeols_DLCV <- readRDS("resultsfeols_DLCV.RData")
plot(resultsfeols_DLCV)
        
#Possible combinations of the firm-level CVs with the other CVs always included
#region-level variables: lnOpen; Endowment; Rail; lnPcgdp
#industry-level variables: Ind; Concentration
#firm-level variables: lnPcca; lnDa; lnSize; lnAge; Own; Export

#Possible combinations of the region-level variables with the industry-level variables and firm-level variables always included.                
specsfeols1 <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "feols_formula",
  controls = CV[7:10],
  add_to_formula = "Ind + Concentration + lnPcca + lnDa + lnSize + lnAge + Own + Export") #16 models

plot(specsfeols1)
resultsfeols1 <- specr(specsfeols1, .options = opts, .progress = TRUE)
plot(resultsfeols1)
#Het effect is consistent niet-significant over alle modelspecificaties. 

#Possible combinations of the industry-level variables with the region-level variables and firm-level variables always included.
specsfeols2 <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "feols_formula",
  controls = CV[11:12],
  add_to_formula = "lnOpen + Endowment + Rail + lnPcgdp + lnPcca + lnDa + lnSize + lnAge + Own + Export") 

plot(specsfeols2)
resultsfeols2 <- specr(specsfeols2, .options = opts, .progress = TRUE)
plot(resultsfeols2)
#Geen significant verband tussen X en Y, ongeacht de gekozen modelspecificatie.

#Possible combinations of the firm-level variables with the region-level variables and industry-level variables always included.
specsfeols3 <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "feols_formula",
  controls = CV[1:6],
  add_to_formula = "lnOpen + Endowment + Rail + lnPcgdp + Ind + Concentration")

plot(specsfeols3)
resultsfeols3 <- specr(specsfeols3, .options = opts, .progress = TRUE)
plot(resultsfeols3)
#Geen significant effect van X op Y, ongeacht de specifieke combinatie van controlevariabelen.

#Different models and SE# (NOT USED, too computational demanding)
feols_formula <- function(formula, data) {
  formula <- as.formula(paste0(formula, " | ", FE))
  fixest::feols(formula, data)} #setting needed formula with feols (FE always included)

feols_formula_clu <- function(formula, data) {
  formula <- as.formula(paste0(formula, " | ", FE))
  fixest::feols(formula, data, vcov = "cluster")} #setting needed formula with feols and clustere SE (FE always included)

opts2 <- furrr_options(
  globals = list(feols_formula = feols_formula, FE = FE, feols_formula_clu = feols_formula_clu),
  seed = TRUE) #needed since we use parallelisation

specsdifmodels <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = c("lm", "feols_formula", "feols_formula_clu"),
  control = CV) #12288 different models

plot(specsdifmodels)
resultsdifmodels <- specr(specsdifmodels, .options = opts2, .progress = TRUE)
plot(resultsdifmodels)

# Using speccurvie
library(speccurvieR)
SCA <- sca(y = Y, 
           x = X,
           controls = CV,
           data = raw_data,
           family = "linear",
           fixedEffects = FE, 
           parallel = TRUE, 
           workers = 11) 
## Unable to run this code: several warnings and an error "cannot allocate vector of size 1.0 Mb". Besides, my computer needs 2 days to run it.


####################
### Extra tables ###
####################
### Table 3 ###
model_coal <- feols(Lncoalcons ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                          + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                          data = raw_data,
                          vcov = "HC1")
model_SO2 <- feols(lnEnergy ~ LnERSO2 + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                    data = raw_data,
                    vcov = "HC1")
model_COD <- feols(lnEnergy ~ LnERCOD + lnPcca + lnDa + lnSize + lnAge + Own + Export
                   + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                   data = raw_data,
                   vcov = "HC1")
model_SO2removalrate <- feols(lnEnergy ~ SO2removalrate + lnPcca + lnDa + lnSize + lnAge + Own + Export
                         + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                         data = raw_data,
                         vcov = "HC1")
model_lnexport <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Lnexport
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                    data = raw_data,
                    vcov = "HC1")
model_extraFE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                        + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final + citycode*ind_final + year*ind_final, 
                        data = raw_data,
                        vcov = "HC1")

pdata <- raw_data %>%
  arrange(id_in_panel, year) %>%
  pdata.frame(index = c("id_in_panel", "year")) #Create pdata.frame (panel data frame)
 
pdata$lag_lnER <- lag(pdata$lnER, k = 1) #Generate lag

model_laggedER <- feols(lnEnergy ~ lag_lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                    data = pdata,
                    vcov = "HC1")

model_list2 <- list(
  "Coal consumption" = model_coal,
  "SO2 mean" = model_SO2, 
  "COD mean" = model_COD, 
  "SO2 removal rate" = model_SO2removalrate, 
  "Export sales" = model_lnexport, 
  "More FE" = model_extraFE,
  "Lagged ER" = model_laggedER)

modelsummary(model_list2,
             coef_map = c("lnER" = "lnER", "LnERSO2" = "LnERSO2", "LnERCOD" = "LnERCOD",
                          "SO2removalrate" = "SO2removalrate", "lag_lnER" = "lag_lnER"),
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",
             add_rows = data.frame(
               rowname = c("Control variables"),
               `Coal consumption` = c("X"),
               `SO2 mean` = c("X"),
               `COD mean` = c("X"),
               `SO2 removal rate` = c("X"),
               `Export sales` = c("X"),
               `More FE` = c("X"),
               `Lagged ER` = c("X")),
             output = "Table 3. Robustness checks' results.png",
             title = "Table 3. Robustness checks' results")

### Table 4 ###
#Case: open after 2001
data_openafter2001 <- raw_data %>%
  filter(year-age <= 2001)
model1_table4 <- model5_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                                           + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                                           data = data_openafter2001,
                                           vcov = "HC1")
#Case: open after 2005
data_openafter2005 <- raw_data %>%
  filter(year-age <= 2005)
model2_table4 <- model5_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                                           + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                                           data = data_openafter2005,
                                           vcov = "HC1")
#Case: exit after 2005
data_exitafter2005 <- raw_data %>%
  group_by(id_in_panel) %>%
  arrange(year, .by_group = T) %>%
  mutate(sign = last(year)) %>%
  ungroup() %>%
  filter(sign > 2005) %>%
  select(-sign)
model3_table4 <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                                           + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                                           data = data_exitafter2005,
                                           vcov = "HC1")
#Case: Change adresses
data_changeaddresses <- raw_data %>%
  group_by(id_in_panel) %>%
  arrange(year, .by_group = T) %>%
  mutate(
    sign = if_else(row_number() == 1 | area_final == lag(area_final), 1, NA_real_),
    sum_na = sum(is.na(sign))
  ) %>%
  ungroup() %>%
  filter(sum_na == 0) %>%
  select(-sign, -sum_na)
model4_table4 <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                       data = data_changeaddresses,
                       vcov = "HC1")

#Case: all previous four
data_combo <- raw_data %>%
  filter(year-age <= 2005) %>%
  group_by(id_in_panel) %>%
  arrange(year, .by_group = T) %>%
  mutate(sign1 = last(year), sign2 = if_else(row_number() == 1 | area_final == lag(area_final), 1, NA_real_),
         sum_na = sum(is.na(sign2))) %>%
  ungroup() %>%
  filter(sign1 > 2005, sum_na == 0) %>%
  select(-sign1, -sign2, -sum_na)
model5_table4 <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                       data = data_combo,
                       vcov = "HC1")

#Case: 2001-2009 existence
model6_table4 <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                       data = raw_data2,
                       vcov = "HC1")

#IV (with additional controls)
data_IV <- raw_data %>%
  mutate(
    reductionper2006 = if_else(year == 2006, reductionper, 0),
    reductionper2007 = if_else(year == 2007, reductionper, 0),
    reductionper2008 = if_else(year == 2008, reductionper, 0),
    reductionper2009 = if_else(year == 2009, reductionper, 0)
  ) %>%
  select(-reductionper)     #This creates four new variables, each equal to reductionper only in a specific year, and 0 elsewhere
                            # = year-specific instruments to avoid multicollinearity and match identification strategies like event studies or staggered treatments

model7_table4 <- feols(lnEnergy ~ 1 + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration| id_in_panel + year + ind_final | 
                      lnER ~ reductionper2006 + reductionper2007 + reductionper2008 + reductionper2009,
                      data = data_IV,
                      vcov = "HC1")
summary(model7_table4)
## According to the F-test the instruments are relevant 
## Sargan test implies that IV might not be valid (p < 0.05 so H0 that the IV are valid may be rejected)
## Wu-Hausman test implies that lnEnergy is exogenous and NOT endogenous (p > 0.05 so H0 that Y is exogenous cannot be rejected) => is IV needed???

model8_table4 <- feols(lnEnergy ~ 1 + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
                         Lnenergyint2001*factor(year) + Lnpollutint2001*factor(year)| id_in_panel + year + ind_final | 
                         lnER ~ reductionper2006 + reductionper2007 + reductionper2008 + reductionper2009,
                       data = data_IV,
                       vcov = "HC1")
summary(model8_table4)

model9_table4 <- feols(lnEnergy ~ 1 + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
                         Lnenergyint2005*factor(year) + Lnpollutint2005*factor(year)| id_in_panel + year + ind_final | 
                         lnER ~ reductionper2006 + reductionper2007 + reductionper2008 + reductionper2009,
                       data = data_IV,
                       vcov = "HC1")
summary(model9_table4)

models_table4 <- list(
  "Open after 2001" = model1_table4,
  "Open after 2005" = model2_table4, 
  "Exit after 2005" = model3_table4, 
  "Changed adresses" = model4_table4, 
  "All four" = model5_table4, 
  "2001-2009 existence" = model6_table4,
  "IV" = model7_table4,
  "IV with additional controls (2001)" = model8_table4,
  "IV with additional controls (2005)" = model9_table4)

modelsummary(models_table4,
             coef_map = c("lnER" = "lnER", "fit_lnER" = "lnER"),
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",
             add_rows = data.frame(
               rowname = c("Lnenergyint2001*year", "Lnpollutint2001*year", "Lnenergyint2005*year", "Lnpollutint2005*year", "Control variables"),
               `Open after 2001` = c("", "", "", "", "X"),
               `Open after 2005` = c("", "", "", "", "X"),
               `Exit after 2005` = c("", "", "", "", "X"),
               `Changed adresses` = c("", "", "", "", "X"),
               `All four` = c("", "", "", "", "X"),
               `2001-2009 existence` = c("", "", "", "", "X"),
               `IV` = c("", "", "", "", "X"),
               `IV with additional controls (2001)` = c("X", "X", "", "", "X"),
               `IV with additional controls (2005)` = c("", "", "X", "X", "X")),
             output = "Table 4. Results of robustness checks.png",
             title = "Table 4. Results of robustness checks")


### Table 5 ###
#Without control variable or FE
model1_table5 <- lm(lnEnergyeff ~ lnER, data = raw_data)
model1_RSE_table5 <- coeftest(model1_table5, vcov. = vcovHC, type = "HC1")

#With control variables, without FE
model2_table5 <- lm(lnEnergyeff ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, data = raw_data)
model2_RSE_table5 <- coeftest(model2_table5, vcov. = vcovHC, type = "HC1")

#With control variables and industry FE
model3_RSE_table5 <- feols(lnEnergyeff ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | ind_final, 
                       data = raw_data,
                       vcov = "HC1")

#With control variables, year FE and firm FE
model4_RSE_table5 <- feols(lnEnergyeff ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year, 
                       data = raw_data,
                       vcov = "HC1")

#With control variables, industry FE, year FE and firm FE
model5_RSE_table5 <- feols(lnEnergyeff ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                           + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                           data = raw_data,
                           vcov = "HC1")

models_table5 <- list(                           #Dit is de manier waarop je een lijst maakt van de verschillende regressiemodellen die je hebt geschat. Elk model is opgeslagen als een object (zoals model1_RSE_table5, model2_RSE_table5, enz.) en deze objecten worden gegroepeerd in de lijst models_list5.
  "model 1" = model1_RSE_table5,      #Door de modellen in een lijst te plaatsen, kun je ze allemaal tegelijkertijd doorgeven aan de modelsummary functie. Dit bespaart tijd en moeite omdat je niet elke regressie afzonderlijk hoeft aan te roepen wanneer je de samenvattende tabel genereert.
  "model 2" = model2_RSE_table5,
  "model 3" = model3_RSE_table5,
  "model 4" = model4_RSE_table5,
  "model 5" = model5_RSE_table5)

modelsummary(models_table5,
             coef_map = c("lnER" = "lnER"),                         #In de coef_map geef je aan welke coëfficiënten je in de tabel wilt hernoemen of weergeven. De reden waarom alleen lnER in de coef_map staat, is omdat je in je oorspronkelijke code specifiek aangeeft dat je de coëfficiënt van lnER wilt weergeven in de samenvattende tabel.
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),      #Dit geeft aan dat je sterretjes wilt gebruiken om de significatieniveaus van je coëfficiënten aan te geven.
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",    #gof_omit verwijdert sommige statistieken zoals Adj, BIC, AIC, etc. en zorgt ervoor dat alleen de belangrijkste statistieken worden weergegeven in de output.
             add_rows = data.frame(                                 #Hier voeg je extra rijen toe aan de tabel. In dit geval voeg je de rij "Control variables" toe, wat aangeeft of control variables in elk model zijn opgenomen ("X" geeft aan of ze aanwezig zijn). Dit helpt de lezer snel te begrijpen of en wanneer bepaalde controlevariabelen zijn opgenomen in de modellen.
               rowname = c("Control variables"),
               `model 1` = c(" "),
               `model 2` = c("X"),
               `model 3` = c("X"),
               `model 4` = c("X"),
               `model 5` = c("X")),
             title = "Table 5. Checks on mechanisms of firms’ energy efficiency",
             output = "Table 5. Checks on mechanisms of firms’ energy efficiency.png")


### Table 6 ###
model1_table6 <- feols(Coalratio ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration |id_in_panel + year + ind_final, data = raw_data, vcov = "HC1")
model2_table6 <- feols(Oilratio ~  lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration |id_in_panel + year + ind_final, data = raw_data, vcov = "HC1")
model3_table6 <- feols(Gasratio ~  lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration |id_in_panel + year + ind_final, data = raw_data, vcov = "HC1")


models_list4 <- list(
  "Coal ratio" = model1_table6,
  "Oil ratio" = model2_table6,
  "Gas ratio" = model3_table6)

modelsummary(models_list4,
             coef_map = c("lnER" = "lnER"),
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
  gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",
  add_rows = data.frame(
    rowname = c("Control variables"),
    `Coal ratio` = c("X"),
    `Oil ratio` = c("X"),
    `Gas ratio` = c("X")),
  title = "Table 6. Checks on mechanisms of firms’ energy structure",
  output = "Table 6. Checks on mechanisms of firms’ energy structure.png")

### Table 7 ###
data_Own <- raw_data %>%    #Drie nieuwe kolommen worden toegevoegd aan de dataset. (Drie binaire variabelen worden aangemaakt op basis van de waarde van Own.) 
  mutate(
    SEO = if_else(Own == 1, 1, 0),           #SEO wordt 1 als de waarde van de kolom Own gelijk is aan 1, anders wordt hij 0. 
    Foreign = if_else(Own == 2, 1, 0),       #Foreign wordt 1 als de waarde van de kolom Own gelijk is aan 2, anders wordt hij 0.
    Private = if_else(Own == 3, 1, 0))       #Private wordt 1 als de waarde van de kolom Own gelijk is aan 3, anders wordt hij 0.

model1_table7 <- feols(lnEnergy ~ lnER:SEO + lnER:Foreign + lnER:Private + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration |id_in_panel + year + ind_final,
                       data = data_Own, 
                       vcov = "HC1")

model2_table7 <- feols(lnEnergyeff ~ lnER:SEO + lnER:Foreign + lnER:Private + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration |id_in_panel + year + ind_final,
                       data = data_Own, 
                       vcov = "HC1")

model3_table7 <- feols(Coalratio ~ lnER:SEO + lnER:Foreign + lnER:Private + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration |id_in_panel + year + ind_final,
                       data = data_Own, 
                       vcov = "HC1")

model4_table7 <- feols(Oilratio ~ lnER:SEO + lnER:Foreign + lnER:Private + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration |id_in_panel + year + ind_final,
                       data = data_Own, 
                       vcov = "HC1")

model5_table7 <- feols(Gasratio ~ lnER:SEO + lnER:Foreign + lnER:Private + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration |id_in_panel + year + ind_final,
                       data = data_Own, 
                       vcov = "HC1")

models_table7 <- list(
  "lnEnergy" = model1_table7,
  "lnEnergyeff" = model2_table7,
  "Coal ratio" = model3_table7,
  "Oil ratio" = model4_table7,
  "Gas ratio" = model5_table7)

modelsummary(models_table7,
             coef_map = c("lnER:SEO" = "lnER:SEO", "lnER:Foreign" = "lnER:Foreign", "lnER:Private" = "lnER:Private"),     #Dit geeft aan dat de coëfficiënten genaamd lnER:SEO, lnER:Foreign, en lnER:Private in de tabel precies dezelfde naam moeten houden als ze in het model voorkomen. Dus, hier wordt geen hernoeming toegepast, omdat de oorspronkelijke en de nieuwe naam hetzelfde zijn. coef_map stelt je in staat om coëfficiëntnamen te herschrijven, zonder dat je de naam van de variabelen in het model zelf hoeft te veranderen.
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",
             add_rows = data.frame(
               rowname = c("Control variables"),
               `lnEnergy` = c("X"),
               `lnEnergyeff` = c("X"),
               `Coal ratio` = c("X"),
               `Oil ratio` = c("X"),
               `Gas ratio` = c("X")),
             title = "Table 7. Results of heterogeneous effects of ownership structure",
             output = "Table 7. Results of heterogeneous effects of ownership structure.png")

### Table 8 ###
data_Size <- raw_data %>%                           #Twee nieuwe kolommen worden aangemaakt in de dataset. 
    mutate(Large = if_else(Largefirm == 1, 1, 0),   #Large krijgt de waarde 1 als de waarde van Largefirm gelijk is aan 1, anders 0.
           Small = if_else(Largefirm == 1, 0, 1))   #Small krijgt de waarde 0 als de waarde van Largefirm gelijk is aan 1, anders 1.

model1_table8 <- feols(lnEnergy ~ lnER:Large + lnER:Small + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + Largefirm |id_in_panel + year + ind_final,
                       data = data_Size, 
                       vcov = "HC1")

model2_table8 <- feols(lnEnergyeff ~ lnER:Large + lnER:Small + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + Largefirm |id_in_panel + year + ind_final,
                       data = data_Size, 
                       vcov = "HC1")

model3_table8 <- feols(Coalratio ~ lnER:Large + lnER:Small + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + Largefirm |id_in_panel + year + ind_final,
                       data = data_Size, 
                       vcov = "HC1")

model4_table8 <- feols(Oilratio ~ lnER:Large + lnER:Small + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + Largefirm |id_in_panel + year + ind_final,
                       data = data_Size, 
                       vcov = "HC1")

model5_table8 <- feols(Gasratio ~ lnER:Large + lnER:Small + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + Largefirm |id_in_panel + year + ind_final,
                       data = data_Size, 
                       vcov = "HC1")

#Note: the variable Largefirm is added as a CV. This is also the variable which is used to form the dummy variables at the beginning.

models_table8 <- list(
  "lnEnergy" = model1_table8,
  "lnEnergyeff" = model2_table8,
  "Coal ratio" = model3_table8,
  "Oil ratio" = model4_table8,
  "Gas ratio" = model5_table8)

modelsummary(models_table8,
             coef_map = c("lnER:Small" = "lnER:Small", "lnER:Large" = "lnER:Large"),
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",
             add_rows = data.frame(
               rowname = c("Control variables"),
               `lnEnergy` = c("X"),
               `lnEnergyeff` = c("X"),
               `Coal ratio` = c("X"),
               `Oil ratio` = c("X"),
               `Gas ratio` = c("X")),
             title = "Table 8. Results of heterogeneous effects of firm scale",
             output = "Table 8. Results of heterogeneous effects of firm scale.png")

### Table 9 ###
data_Pollution <- raw_data %>%                             #Een nieuwe kolom wordt toegevoegd aan de dataset, LowPollution. 
  mutate(LowPollution = if_else(HighPollution == 1, 0, 1)) #Als de waarde van de kolom HighPollution gelijk is aan 1, krijgt de nieuwe kolom LowPollution de waarde 0. Als de waarde van HighPollution niet gelijk is aan 1, krijgt LowPollution de waarde 1.   

model1_table9 <- feols(lnEnergy ~ lnER:HighPollution + lnER:LowPollution + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + HighPollution |id_in_panel + year + ind_final,
                       data = data_Pollution, 
                       vcov = "HC1")

model2_table9 <- feols(lnEnergyeff ~ lnER:HighPollution + lnER:LowPollution + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + HighPollution |id_in_panel + year + ind_final,
                       data = data_Pollution, 
                       vcov = "HC1")

model3_table9 <- feols(Coalratio ~ lnER:HighPollution + lnER:LowPollution + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + HighPollution |id_in_panel + year + ind_final,
                       data = data_Pollution, 
                       vcov = "HC1")

model4_table9 <- feols(Oilratio ~ lnER:HighPollution + lnER:LowPollution + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + HighPollution|id_in_panel + year + ind_final,
                       data = data_Pollution, 
                       vcov = "HC1")

model5_table9 <- feols(Gasratio ~ lnER:HighPollution + lnER:LowPollution + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + HighPollution|id_in_panel + year + ind_final,
                       data = data_Pollution, 
                       vcov = "HC1")

#Note: the variable HighPollution is added as a CV. This is also the variable which is used to form the dummy variables at the beginning.

models_table9 <- list(
  "lnEnergy" = model1_table9,
  "lnEnergyeff" = model2_table9,
  "Coal ratio" = model3_table9,
  "Oil ratio" = model4_table9,
  "Gas ratio" = model5_table9)

modelsummary(models_table9,
             coef_map = c("lnER:LowPollution" = "lnER:LowPollution", "lnER:HighPollution" = "lnER:HighPollution"),
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",
             add_rows = data.frame(
               rowname = c("Control variables"),
               `lnEnergy` = c("X"),
               `lnEnergyeff` = c("X"),
               `Coal ratio` = c("X"),
               `Oil ratio` = c("X"),
               `Gas ratio` = c("X")),
             title = "Table 9. Results of heterogeneous effects of pollution intensity",
             output = "Table 9. Results of heterogeneous effects of pollution intensity.png")

### Table 10 ###
data_EnergyIntensity <- raw_data %>%                       #Twee nieuwe kolommen worden toegevoegd aan de dataset. 
  mutate(LowENINT = if_else(energy_intensive == 1, 0, 1),  #LowENINT krijgt de waarde 0 als de waarde van energy_intensive gelijk is aan 1. Anders krijgt het de waarde 1. 
         HighENINT = if_else(energy_intensive == 1, 1, 0)) #HighENINT krijgt de waarde 1 als energy_intensive gelijk is aan 1. Anders krijgt het de waarde 0.

model1_table10 <- feols(lnEnergy ~ lnER:HighENINT + lnER:LowENINT + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + energy_intensive |id_in_panel + year + ind_final,
                       data = data_EnergyIntensity, 
                       vcov = "HC1")

model2_table10 <- feols(lnEnergyeff ~ lnER:HighENINT + lnER:LowENINT + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + energy_intensive |id_in_panel + year + ind_final,
                       data = data_EnergyIntensity, 
                       vcov = "HC1")

model3_table10 <- feols(Coalratio ~ lnER:HighENINT + lnER:LowENINT + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + energy_intensive |id_in_panel + year + ind_final,
                       data = data_EnergyIntensity, 
                       vcov = "HC1")

model4_table10 <- feols(Oilratio ~ lnER:HighENINT + lnER:LowENINT + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + energy_intensive |id_in_panel + year + ind_final,
                       data = data_EnergyIntensity, 
                       vcov = "HC1")

model5_table10 <- feols(Gasratio ~ lnER:HighENINT + lnER:LowENINT + lnPcca + lnDa + lnSize + lnAge + Own + Export
                       + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + energy_intensive |id_in_panel + year + ind_final,
                       data = data_EnergyIntensity, 
                       vcov = "HC1")

#Note: the variable energy_intensive is added as a CV. This is also the variable which is used to form the dummy variables at the beginning.

models_table10 <- list(
  "lnEnergy" = model1_table10,
  "lnEnergyeff" = model2_table10,
  "Coal ratio" = model3_table10,
  "Oil ratio" = model4_table10,
  "Gas ratio" = model5_table10)

modelsummary(models_table10,
             coef_map = c("lnER:LowENINT" = "lnER:LowENINT", "lnER:HighENINT" = "lnER:HighENINT"),
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within",
             add_rows = data.frame(
               rowname = c("Control variables"),
               `lnEnergy` = c("X"),
               `lnEnergyeff` = c("X"),
               `Coal ratio` = c("X"),
               `Oil ratio` = c("X"),
               `Gas ratio` = c("X")),
             title = "Table 10. Results of heterogeneous effects of energy intensity",
             output = "Table 10. Results of heterogeneous effects of energy intensity.png")



### EXTRA: seperation based on Export 
model1_Export<- raw_data %>%                        
  mutate(Nonexport = if_else(Export == 1, 0, 1)) %>%
  feols(lnEnergy ~ lnER:Export + lnER:Nonexport + lnPcca + lnDa + lnSize + lnAge + Own
        + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final,
        vcov = "HC1")

model2_Export<- raw_data %>%                        
  mutate(Nonexport = if_else(Export == 1, 0, 1)) %>%
  feols(lnEnergyeff ~ lnER:Export + lnER:Nonexport + lnPcca + lnDa + lnSize + lnAge + Own
        + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final,
        vcov = "HC1")

model3_Export<- raw_data %>%                        
  mutate(Nonexport = if_else(Export == 1, 0, 1)) %>%
  feols(Coalratio ~ lnER:Export + lnER:Nonexport + lnPcca + lnDa + lnSize + lnAge + Own
        + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final,
        vcov = "HC1")

model4_Export<- raw_data %>%                        
  mutate(Nonexport = if_else(Export == 1, 0, 1)) %>%
  feols(Oilratio ~ lnER:Export + lnER:Nonexport + lnPcca + lnDa + lnSize + lnAge + Own
        + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final,
        vcov = "HC1")

model5_Export<- raw_data %>%                        
  mutate(Nonexport = if_else(Export == 1, 0, 1)) %>%
  feols(Gasratio ~ lnER:Export + lnER:Nonexport + lnPcca + lnDa + lnSize + lnAge + Own
        + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final,
        vcov = "HC1")

models_Export <- list(
  "lnEnergy" = model1_Export,
  "lnEnergyeff" = model2_Export,
  "Coal ratio" = model3_Export,
  "Oil ratio" = model4_Export,
  "Gas ratio" = model5_Export)

modelsummary(models_Export,
             coef_map = c("lnER:Export" = "lnER:Export", "lnER:Nonexport" = "lnER:Nonexport"),
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             gof_omit = "Adj|BIC|AIC|RMSE|Std.Errors|R2 Within")
#CONCLUSION: effect differs between exporters and non-exporters. Suggest that non-exporters are more sensitive to domestic environmental regulations, 
# whereas exporters, who may already face stricter international environmental standards, show a smaller adjustment. This heterogeneity highlights 
# that firms' exposure to global markets influences how they respond to domestic environmental policies


#EXTRA: Investigating NA's
raw_data_with_na <- raw_data %>%
  select(where(~ any(is.na(.))))
library(mice)
md.pattern(raw_data)
md.pattern(raw_data_with_na)
library(naniar)
gg_miss_upset(raw_data)
gg_miss_upset(raw_data_with_na, nsets = 7)
library(VIM)
aggr(raw_data_with_na, numbers = TRUE, prop = c(TRUE, FALSE), sortVars = TRUE, cex.axis = 0.525)

raw_data_NAfilter <- raw_data %>%
  select(year, ind_final, id_in_panel, Lnfirmenergypre05, Lncoalcons, lnEnergy, 
         Coalratio, Oilratio, Gasratio, lnEnergyeff) #selecting variables with more than 25% NAs + FE

gg_miss_var(raw_data_NAfilter,
            facet = year)

gg_miss_var(raw_data_NAfilter,
            facet = ind_final)


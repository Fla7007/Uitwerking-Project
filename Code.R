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
model3_plm <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = c("ind_final"),
              model = "within")
summary(model3)
model3_plm_RSE <- coeftest(model3_plm, vcov. = vcovHC, type = "HC1")
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

model3_demean <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                  + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
                    factor(ind_final), 
                  data = raw_data)

robust_se <- vcovHC(model3_demean, type = "HC1")
model3_demean_RSE <- coeftest(model3_demean, vcov. = robust_se)
## Gives the same point estimators and standard errors as in the original paper. 
## Only the intercept is wrong since we've added a factor of ind_final to incorporate the industry FE 

# Model 3 using feols (with control variables and industry FE)
model3_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | ind_final, 
                    data = raw_data,
                    vcov = "HC1")
## Correct point estimators and SE, no constant (due to FE)

#Model 3 using felm (with control variables, and industry FE)
model3_felm <- felm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                 + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | ind_final, 
                 data = raw_data)
model3_felm_RSE <- coeftest(model3_felm, vcov = vcovHC(model3_felm, type="HC1"))
## Correct point estimators and SE, no constant (due to FE)

### Model 4 ###
# Model 4 using plm (with control variables, year FE and firm FE) 
model4_plm <- plm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
              + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration, 
              data = raw_data,
              index = c("id_in_panel", "year"),
              model = "within")
summary(model4)
model4_plm_RSE <- coeftest(model4_plm, vcov. = vcovHC, type = "HC1")
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

model4_demean <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                 + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
                   factor(year), factor(id_in_panel), 
                 data = dataset_model4_demeaned)

robust_se <- vcovHC(model4_demean, type = "HC1")
model4_demean_RSE <- coeftest(model4_demean, vcov. = robust_se)
## Does NOT give the same results as in the original paper: 
## the values of the point estimators and standard errors aren't the same

# Model 4 using feols (with control variables, year FE and firm FE)
model4_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year, 
                    data = raw_data,
                    vcov = "HC1")
## Correct point estimators, SE sometimes slightly differ, no constant (due to FE), used much more observations (different R²)

#Model 4 using felm (with control variables, year FE and firm FE)
model4_felm <- felm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                 + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year, 
                 data = raw_data)
model4_felm_RSE <- coeftest(model4_felm, vcov = vcovHC(model4_felm, type="HC1"))
## Correct point estimators, SE sometimes slightly differ, no constant (due to FE), used much more observations

### Model 5 ###
# Model 5 using plm (with control variables and all FE)  
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

model5_demean <- lm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                  + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration + 
                    factor(ind_final) + factor(year) + factor(id_in_panel), 
                  data = raw_data)

robust_se <- vcovHC(model5_demean, type = "HC1")
model5_demean_RSE <- coeftest(model5_demean, vcov. = robust_se)
## We were unable to run this code since our computers cannot compute it for such a large dataset
## Error: cannot allocate vector of size 44.6 Gb

# Model 5 using feols (with control variables and all FE)
model5_feols_RSE <- feols(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                    data = raw_data,
                    vcov = "HC1")
## Correct point estimators, SE sometimes slightly differ, no constant (due to FE), used much more observations (different R²) 

#Model 5 using felm (with control variables and all FE)
model5_felm <- felm(lnEnergy ~ lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                 + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                 data = raw_data)
model5_felm_RSE <- coeftest(model5_felm, vcov = vcovHC(model5_felm, type="HC1"))
## Correct point estimators, SE sometimes slightly differ, no constant (due to FE), used much more observations


### Overview ###
#Using stargazer
stargazer(model1_RSE, model2_RSE, model3_demean_RSE, model4_plm_RSE, model5_plm_RSE, type = "html", 
          title = "Benchmark regression results.", digits = 3, out = "Benchmark_regression_results.html")
## Stargazer does NOT work with the models made by feols

#Using huxreg 
huxreg(model1_RSE, model2_RSE,model3_feols_RSE, model4_feols_RSE, model5_feols_RSE, 
       statistics = c("N. obs." = "nobs", "R squared" = "r.squared"))

#Using modelsummary
model_list <- list(
  "model 1" = model1_RSE,
  "model 2" = model2_RSE,
  "model 3" = model3_feols_RSE,
  "model 4" = model4_feols_RSE,
  "model 5" = model5_feols_RSE)

modelsummary(model_list,
             stars = TRUE,
             gof_omit = "IC|Log|Adj|F|RMSE|R2 Within",
             output = "Benchmark regression results (lm + feols).png",
             title = "Benchmark regression results.") 


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
## Variables with non-zero coefficients in both models: 
## lnEnergyeff, lnDa, lnSize, Own, lnPcgdp, Concentration, LnERCOD, Lncoalcons, HighPollution, Largefirm, energy_intensive, Gasratio, TargetDummy


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
                                 

# New model with other selection of control variables, without FE
model2_new <- lm(lnEnergy ~ lnER + lnEnergyeff + lnDa + lnSize + Own + lnPcgdp + Concentration +
                     LnERCOD + Lncoalcons + HighPollution + Largefirm + energy_intensive + Gasratio + TargetDummy,
                   data = raw_data)
model2_new_RSE <- coeftest(model2_new, vcov. = vcovHC, type = "HC1")


# New model with other selection of control variables, with all FE
model5_new_feols_RSE <- feols(lnEnergy ~ lnER + lnEnergyeff + lnDa + lnSize + Own + lnPcgdp + Concentration +
                        LnERCOD + Lncoalcons + HighPollution + Largefirm + energy_intensive + Gasratio + TargetDummy|
                        id_in_panel + year + ind_final,
                      data = raw_data, 
                      vcov = "HC1")

# Comparison with original models
huxreg(model2_RSE, model2_new_RSE, model5_feols_RSE, model5_new_feols_RSE)
## Compared to the original models, these new models with other control variables gives better logLik and AIC values (higher logLik, lower AIC) 
## So based on that, the new created models are prefered over the original ones.
## In the new created models lnER is not significantly related to lnEnergy


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
huxreg("Robust SE" = model3_feols_RSE, "Clustered SE" = model3_feols_clustered)  # lnER is not anymore significantly related to lnEnergy, the same holds for some of the control variables 
huxreg("Robust SE" = model4_feols_RSE, "Clustered SE" = model4_feols_clustered)  # Despite other standard-errors, the significance of relationships and the estimated coefficients remains the same
huxreg("Robust SE" = model5_feols_RSE, "Clustered SE" = model5_feols_clustered)  # Despite other standard-errors, the significance of relationships and the estimated coefficients remains the same


### Misspecification of the functional form ###

#lnER
matplot(raw_data$lnER,raw_data$lnEnergy, pch=1)   #Plots X vs. Y using points (with pch = 1 as the symbol.) Helps us visually check whether Y has a linear relationship with X or if it appears nonlinear.

?matplot()

plot(density(na.omit(raw_data$lnER)))  #Plots the density estimate of X, showing its distribution
plot(density(na.omit(raw_data$lnEnergy)))   #Plots the density of Y.

matplot(raw_data$lnER,raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ lnER, data = raw_data))  #summary(lm(Y~X)) stores the regression results in res_miss
matplot(raw_data$lnER, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$lnER, t="l", col=2, add=T) #Plots the fitted regression line (red) on top of the scatter plot. Coefficients[1,1] is the intercept and coefficients [2,1] is the slope. add = T ensures the regression line is added to the existing scatter plot.


matplot(exp(raw_data$lnER), raw_data$lnEnergy, pch = 1)
plot(density(na.omit(exp(raw_data$lnER)))) 
plot(density(na.omit(raw_data$lnEnergy)))
matplot(exp(raw_data$lnER),raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ exp(lnER), data = raw_data))  
matplot(exp(raw_data$lnER), res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*exp(raw_data$lnER), t="l", col=2, add=T)


#lnPcca
matplot(raw_data$lnPcca,raw_data$lnEnergy, pch=1)

plot(density(na.omit(raw_data$lnPcca)))
plot(density(na.omit(filtered_raw_data$lnEnergy)))

matplot(raw_data$lnPcca,raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ lnPcca, data = raw_data))
matplot(raw_data$lnPcca, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$lnPcca, t="l", col=2, add=T)


matplot(exp(raw_data$lnPcca), raw_data$lnEnergy, pch = 1)
plot(density(na.omit(exp(raw_data$lnPcca)))) 
plot(density(na.omit(raw_data$lnEnergy)))
matplot(exp(raw_data$lnPcca),raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ exp(lnPcca), data = raw_data))  
matplot(exp(raw_data$lnPcca), res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*exp(raw_data$lnPcca), t="l", col=2, add=T)


#lnDa 
matplot(raw_data$lnDa,raw_data$lnEnergy, pch=1)

plot(density(na.omit(raw_data$lnDa)))
plot(density(na.omit(raw_data$lnEnergy)))

matplot(raw_data$lnDa,raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ lnDa, data = raw_data))
matplot(raw_data$lnDa, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$lnDa, t="l", col=2, add=T)


#lnSize 
matplot(raw_data$lnSize,raw_data$lnEnergy, pch=1)

plot(density(na.omit(raw_data$lnSize)))
plot(density(na.omit(raw_data$lnEnergy)))

matplot(raw_data$lnSize,raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ lnSize, data = raw_data))
matplot(raw_data$lnSize, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$lnSize, t="l", col=2, add=T)


#lnAge 
matplot(raw_data$lnAge,raw_data$lnEnergy, pch=1)

plot(density(na.omit(raw_data$lnAge)))
plot(density(na.omit(raw_data$lnEnergy)))

matplot(raw_data$lnAge,raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ lnAge, data = raw_data))
matplot(raw_data$lnAge, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$lnAge, t="l", col=2, add=T)


#Own 
matplot(raw_data$Own, raw_data$lnEnergy, pch=1)

plot(density(na.omit(raw_data$Own)))
plot(density(na.omit(log(raw_data$Own))))
plot(density(log(filtered_raw_data$Own)))
plot(density(na.omit(raw_data$lnEnergy)))

matplot(raw_data$Own, raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(raw_data$lnEnergy ~ raw_data$Own))
matplot(raw_data$Own, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$Own, t="l", col=2, add=T)


#Export
matplot(raw_data$Export, raw_data$lnEnergy, pch=1)

plot(density(na.omit(raw_data$Export)))
plot(density(log(na.omit(raw_data$Export))))
plot(density(na.omit(raw_data$lnEnergy)))

matplot(raw_data$Export,raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(raw_data$lnEnergy ~ raw_data$Export))
matplot(raw_data$Export, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$Export, t="l", col=2, add=T)

matplot(log(raw_data$Export),raw_data$lnEnergy, pch=1)
res <- summary(lm(raw_data$lnEnergy ~ log(raw_data$Export)))
matplot(log(raw_data$Export), res$coefficients[1,1] + res$coefficients[2,1]*log(raw_data$Export), t="l", col=2, add=T)          
#De code met log() geeft error.
               
#lnOpen
matplot(raw_data$lnOpen,raw_data$lnEnergy, pch=1)

plot(density(na.omit(raw_data$lnOpen)))
plot(density(na.omit(raw_data$lnEnergy)))

matplot(raw_data$lnOpen,raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(raw_data$lnEnergy ~ raw_data$lnOpen))
matplot(raw_data$lnOpen, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$lnOpen, t="l", col=2, add=T)

  
#Ind 
matplot(raw_data$Ind, raw_data$lnEnergy, pch=1)
               
plot(density(na.omit(raw_data$Ind)))
plot(density(log(na.omit(raw_data$Ind))))
plot(density(na.omit(raw_data$lnEnergy)))
               
matplot(raw_data$Ind,raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(raw_data$lnEnergy ~ raw_data$Ind))
matplot(raw_data$Ind, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$Ind, t="l", col=2, add=T)
               
matplot(log(raw_data$Ind),raw_data$lnEnergy, pch=1)
res <- summary(lm(raw_data$lnEnergy ~ log(raw_data$Ind)))
matplot(log(raw_data$Ind), res$coefficients[1,1] + res$coefficients[2,1]*log(raw_data$Ind), t="l", col=2, add=T)
               
#Endowment
matplot(raw_data$Endowment, raw_data$lnEnergy, pch=1)
               
plot(density(na.omit(raw_data$Endowment)))
plot(density(log(na.omit(raw_data$Endowment))))
plot(density(na.omit(raw_data$lnEnergy)))
               
matplot(raw_data$Endowment, raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(raw_data$lnEnergy ~ raw_data$Endowment))
matplot(raw_data$Endowment, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$Endowment, t="l", col=2, add=T)
               
matplot(log(raw_data$Endowment),raw_data$lnEnergy, pch=1)
res <- summary(lm(raw_data$lnEnergy ~ log(raw_data$Endowment)))
matplot(log(raw_data$Endowment), res$coefficients[1,1] + res$coefficients[2,1]*log(raw_data$Endowment), t="l", col=2, add=T)
#De code met log() geeft error.               
               
#Rail
matplot(raw_data$Rail, raw_data$lnEnergy, pch=1)
               
plot(density(na.omit(raw_data$Rail)))
plot(density(log(na.omit(raw_data$Rail))))
plot(density(na.omit(raw_data$lnEnergy)))
               
matplot(raw_data$Rail,raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(raw_data$lnEnergy ~ raw_data$Rail))
matplot(raw_data$Rail, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$Rail, t="l", col=2, add=T)
               
matplot(log(raw_data$Rail),raw_data$lnEnergy, pch=1)
res <- summary(lm(raw_data$lnEnergy ~ log(raw_data$Rail)))
matplot(log(raw_data$Rail), res$coefficients[1,1] + res$coefficients[2,1]*log(raw_data$Rail), t="l", col=2, add=T)
               
               
#lnPcgdp
matplot(raw_data$lnPcgdp,raw_data$lnEnergy, pch=1)
               
plot(density(na.omit(raw_data$lnPcgdp)))
plot(density(na.omit(raw_data$lnEnergy)))

matplot(raw_data$lnPcgdp,raw_data$lnEnergy, pch=1)               
res_miss <- summary(lm(lnEnergy ~ lnPcgdp, data = raw_data))
matplot(raw_data$lnPcgdp, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$lnPcgdp, t="l", col=2, add=T)
               
#Concentration
matplot(raw_data$Concentration, raw_data$lnEnergy, pch=1)
               
plot(density(na.omit(raw_data$Concentration)))
plot(density(log(na.omit(raw_data$Concentration))))
plot(density(na.omit(raw_data$lnEnergy)))
               
matplot(raw_data$Concentration,raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(raw_data$lnEnergy ~ raw_data$Concentration))
matplot(raw_data$Concentration, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*raw_data$Concentration, t="l", col=2, add=T)
               
matplot(log(raw_data$Concentration),raw_data$lnEnergy, pch=1)
res <- summary(lm(raw_data$lnEnergy ~ log(raw_data$Concentration)))
matplot(log(raw_data$Concentration), res$coefficients[1,1] + res$coefficients[2,1]*log(raw_data$Concentration), t="l", col=2, add=T)
#De code met log() geeft error.

#New regression models with other combinations of fixed effects
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
#Using huxreg 
huxreg(extramodel1_feols_RSE, extramodel2_feols_RSE, extramodel3_feols_RSE, extramodel4_feols_RSE, 
       statistics = c("N. obs." = "nobs", "R squared" = "r.squared"))

### Specification curve analysis ###
Y <- "lnEnergy"
X <- "lnER"
FE <- "id_in_panel + year + ind_final"  # Fixed effects
CV <- c("lnPcca", "lnDa", "lnSize", "lnAge", "Own", "Export", "lnOpen", "Ind", "Endowment", "Rail", "lnPcgdp", "Concentration")

# Using speccurvie (DO NOT RUN THIS CODE)
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

# Using specr with parallelisation
library(fixest)
library(specr)
library(furrr)

#Taking a random sample#
data_NAY <- raw_data %>% drop_na(lnEnergy) #remove NA from the Y variable
sample_data <- data_NAY %>%
  group_by(ind_final, id_in_panel) %>%  # Group by industry and firm
  filter(n_distinct(year) > 1) %>%  # Keep only firms with data across multiple years (if applicable)
  sample_frac(0.2) %>%  # Take a random sample of 20% of the firms for each industry
  ungroup()  # Remove the grouping to return the full data

#Setting parallelisation#
plan(strategy = multisession, workers = 6)

#LM (without FE)#
#All possible combinations of the 12 selected control variables
specslm <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "lm",
  controls = CV) #4096 different models

plot(specslm)
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
      ## lnPcgdp is often included in the most positive models and never in the most negative ones => suggests a positive polarising effect
      ## Endowment and Concentration are included in both groups. However, they are more present in the positive models than in the negative ones
      ## lnSize and lnOpen are only included in the most positive models, but in less than 50%
      ## All other control variables are equaly present in both groups

#Possible combinations of the first six CV with the last six CV always included
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

#LM (with all CV and FE)# --> no sure if this is right since we use lm and try to include FE
specslmFE <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "lm",
  controls = c("id_in_panel", "year", "ind_final"),
  add_to_formula = "lnPcca + lnDa + lnSize + lnAge + Own + Export + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration") #8 different models

plot(specslmFE)
resultslmFE <- specr(specslmFE, .progress = TRUE)
plot(resultslmFE)

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

plot(specsfeols)
resultsfeols <- specr(specsfeols, .options = opts, .progress = TRUE) #takes +/- 2h to run
plot(resultsfeols)

#Possible combinations of the first four CV with the last eight CV always included
specsfeols1 <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "feols_formula",
  controls = CV[1:4],
  add_to_formula = "Own + Export + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration") #16 different models

plot(specsfeols1)
resultsfeols1 <- specr(specsfeols1, .options = opts, .progress = TRUE)
plot(resultsfeols1)

#Possible combinations of the middle four CV with the first and last four CV always included
specsfeols2 <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "feols_formula",
  controls = CV[5:8],
  add_to_formula = "lnPcca + lnDa + lnSize + lnAge + Endowment + Rail + lnPcgdp + Concentration") #16 different models

plot(specsfeols2)
resultsfeols2 <- specr(specsfeols2, .options = opts, .progress = TRUE)
plot(resultsfeols2)

#Possible combinations of the last four CV with the first eight CV always included
specsfeols3 <- setup(
  data = sample_data,
  y = Y,
  x = X,
  model = "feols_formula",
  controls = CV[9:12],
  add_to_formula = "lnPcca + lnDa + lnSize + lnAge + Own + Export + lnOpen + Ind") #16 different models

plot(specsfeols3)
resultsfeols3 <- specr(specsfeols3, .options = opts, .progress = TRUE)
plot(resultsfeols3)

#Different models and SE# (NOT RUNNED YET)
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

### Extra tables ###
#Table 3
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

lagged_data <- raw_data %>%
  arrange(id_in_panel, year) %>%                #orders observations within a panel based on year
  group_by(id_in_panel) %>%                     #grouping by panel so lag is calculated within a panel
  mutate(lag_lnER = dplyr::lag(lnER, 1)) %>%    #create 1-period lag
  ungroup()
 
model_laggedER <- feols(lnEnergy ~ lag_lnER + lnPcca + lnDa + lnSize + lnAge + Own + Export
                    + lnOpen + Ind + Endowment + Rail + lnPcgdp + Concentration | id_in_panel + year + ind_final, 
                    data = lagged_data,
                    vcov = "HC1")

#Using modelsummary
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
             stars = TRUE,
             gof_omit = "IC|Log|Adj|F|RMSE|R2 Within")

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


                    ## using gamlr with fixed effects
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
                    formula_str_Y <- paste(Y, "~", paste(CV, collapse = " + "), "|", FE)
                    resid_Y <- residuals(feols(as.formula(formula_str_Y), data = NA_obs_deleted))
                    resid_Y <- matrix(resid_Y, ncol = 1)  # Ensure it's a matrix
                    
                    # Step 2: Residualize X (lnER) by removing fixed effects
                    formula_str_X <- paste(X, "~", paste(CV, collapse = " + "), "|", FE)
                    resid_X <- residuals(feols(as.formula(formula_str_X), data = NA_obs_deleted))
                    resid_X <- matrix(resid_X, ncol = 1)  # Ensure it's a matrix
                    
                    # Step 3: Residualize Control Variables
                    resid_CV_list <- lapply(CV, function(var) {
                      residuals(feols(as.formula(paste(var, "~", FE)), data = NA_obs_deleted))
                    })
                    
                    # Step 4: Combine X and Control Variables & Get Index
                    resid_data <- cbind(resid_X, resid_CV)  # Ensure X is included
                    colnames(resid_data)[1] <- X  # Assign column name to X
                    index_X <- 1  # Since X is the first column in `resid_data`
                    
                    # Step 5: Apply Double LASSO Selection
                    lasso_model <- rlassoEffects(x = resid_data, y = resid_Y, index = index_X, method = "double selection")
                    
                    # Step 6: Output Results
                    summary(lasso_model)



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
filtered_raw_data <- na.omit(raw_data)

#lnER
matplot(filtered_raw_data$lnER,filtered_raw_data$lnEnergy, pch=1)   #Plots X vs. Y using points (with pch = 1 as the symbol.) Helps us visually check whether Y has a linear relationship with X or if it appears nonlinear.

?matplot()

plot(density(filtered_raw_data$lnER))  #Plots the density estimate of X, showing its distribution
plot(density(filtered_raw_data$lnEnergy))   #Plots the density of Y.

matplot(filtered_raw_data$lnER,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ lnER, data = filtered_raw_data))  #summary(lm(Y~X)) stores the regression results in res_miss
matplot(filtered_raw_data$lnER, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$lnER, t="l", col=2, add=T) #Plots the fitted regression line (red) on top of the scatter plot. Coefficients[1,1] is the intercept and coefficients [2,1] is the slope. add = T ensures the regression line is added to the existing scatter plot.

#lnPcca
matplot(filtered_raw_data$lnPcca,filtered_raw_data$lnEnergy, pch=1)

plot(density(filtered_raw_data$lnPcca))
plot(density(filtered_raw_data$lnEnergy))

matplot(filtered_raw_data$lnPcca,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ lnPcca, data = filtered_raw_data))
matplot(filtered_raw_data$lnPcca, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$lnPcca, t="l", col=2, add=T)

#lnDa 
matplot(filtered_raw_data$lnDa,filtered_raw_data$lnEnergy, pch=1)

plot(density(filtered_raw_data$lnDa))
plot(density(filtered_raw_data$lnEnergy))

matplot(filtered_raw_data$lnDa,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ lnDa, data = filtered_raw_data))
matplot(filtered_raw_data$lnDa, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$lnDa, t="l", col=2, add=T)


#lnSize 
matplot(filtered_raw_data$lnSize,filtered_raw_data$lnEnergy, pch=1)

plot(density(filtered_raw_data$lnSize))
plot(density(filtered_raw_data$lnEnergy))

matplot(filtered_raw_data$lnSize,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ lnSize, data = filtered_raw_data))
matplot(filtered_raw_data$lnSize, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$lnSize, t="l", col=2, add=T)


#lnAge 
matplot(filtered_raw_data$lnAge,filtered_raw_data$lnEnergy, pch=1)

plot(density(filtered_raw_data$lnAge))
plot(density(filtered_raw_data$lnEnergy))

matplot(filtered_raw_data$lnAge,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(lnEnergy ~ lnAge, data = filtered_raw_data))
matplot(filtered_raw_data$lnAge, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$lnAge, t="l", col=2, add=T)


#Own 
matplot(filtered_raw_data$Own, filtered_raw_data$lnEnergy, pch=1)

plot(density(filtered_raw_data$Own))
plot(density(log(filtered_raw_data$Own)))
plot(density(filtered_raw_data$lnEnergy))

matplot(filtered_raw_data$Own,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(filtered_raw_data$lnEnergy ~ filtered_raw_data$Own))
matplot(filtered_raw_data$Own, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$Own, t="l", col=2, add=T)

matplot(log(filtered_raw_data$Own),filtered_raw_data$lnEnergy, pch=1)
res <- summary(lm(filtered_raw_data$lnEnergy ~ log(filtered_raw_data$Own)))
matplot(log(filtered_raw_data$Own), res$coefficients[1,1] + res$coefficients[2,1]*log(filtered_raw_data$Own), t="l", col=2, add=T)


#Export
matplot(filtered_raw_data$Export, filtered_raw_data$lnEnergy, pch=1)

plot(density(filtered_raw_data$Export))
plot(density(log(filtered_raw_data$Export)))
plot(density(filtered_raw_data$lnEnergy))

matplot(filtered_raw_data$Export,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(filtered_raw_data$lnEnergy ~ filtered_raw_data$Export))
matplot(filtered_raw_data$Export, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$Export, t="l", col=2, add=T)

matplot(log(filtered_raw_data$Export),filtered_raw_data$lnEnergy, pch=1)
res <- summary(lm(filtered_raw_data$lnEnergy ~ log(filtered_raw_data$Export)))
matplot(log(filtered_raw_data$Export), res$coefficients[1,1] + res$coefficients[2,1]*log(filtered_raw_data$Export), t="l", col=2, add=T)          
#De code met log() geeft error.
               
#lnOpen
matplot(filtered_raw_data$lnOpen,filtered_raw_data$lnEnergy, pch=1)

plot(density(filtered_raw_data$lnOpen))
plot(density(filtered_raw_data$lnEnergy))

matplot(filtered_raw_data$lnOpen,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(filtered_raw_data$lnEnergy ~ filtered_raw_data$lnOpen))
matplot(filtered_raw_data$lnOpen, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$lnOpen, t="l", col=2, add=T)

  
#Ind 
matplot(filtered_raw_data$Ind, filtered_raw_data$lnEnergy, pch=1)
               
plot(density(filtered_raw_data$Ind))
plot(density(log(filtered_raw_data$Ind)))
plot(density(filtered_raw_data$lnEnergy))
               
matplot(filtered_raw_data$Ind,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(filtered_raw_data$lnEnergy ~ filtered_raw_data$Ind))
matplot(filtered_raw_data$Ind, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$Ind, t="l", col=2, add=T)
               
matplot(log(filtered_raw_data$Ind),filtered_raw_data$lnEnergy, pch=1)
res <- summary(lm(filtered_raw_data$lnEnergy ~ log(filtered_raw_data$Ind)))
matplot(log(filtered_raw_data$Ind), res$coefficients[1,1] + res$coefficients[2,1]*log(filtered_raw_data$Ind), t="l", col=2, add=T)
               
#Endowment
matplot(filtered_raw_data$Endowment, filtered_raw_data$lnEnergy, pch=1)
               
plot(density(filtered_raw_data$Endowment))
plot(density(log(filtered_raw_data$Endowment)))
plot(density(filtered_raw_data$lnEnergy))
               
matplot(filtered_raw_data$Endowment,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(filtered_raw_data$lnEnergy ~ filtered_raw_data$Endowment))
matplot(filtered_raw_data$Endowment, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$Endowment, t="l", col=2, add=T)
               
matplot(log(filtered_raw_data$Endowment),filtered_raw_data$lnEnergy, pch=1)
res <- summary(lm(filtered_raw_data$lnEnergy ~ log(filtered_raw_data$Endowment)))
matplot(log(filtered_raw_data$Endowment), res$coefficients[1,1] + res$coefficients[2,1]*log(filtered_raw_data$Endowment), t="l", col=2, add=T)
#De code met log() geeft error.               
               
#Rail
matplot(filtered_raw_data$Rail, filtered_raw_data$lnEnergy, pch=1)
               
plot(density(filtered_raw_data$Rail))
plot(density(log(filtered_raw_data$Rail)))
plot(density(filtered_raw_data$lnEnergy))
               
matplot(filtered_raw_data$Rail,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(filtered_raw_data$lnEnergy ~ filtered_raw_data$Rail))
matplot(filtered_raw_data$Rail, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$Rail, t="l", col=2, add=T)
               
matplot(log(filtered_raw_data$Rail),filtered_raw_data$lnEnergy, pch=1)
res <- summary(lm(filtered_raw_data$lnEnergy ~ log(filtered_raw_data$Rail)))
matplot(log(filtered_raw_data$Rail), res$coefficients[1,1] + res$coefficients[2,1]*log(filtered_raw_data$Rail), t="l", col=2, add=T)
               
               
#lnPcgdp
matplot(filtered_raw_data$lnPcgdp,filtered_raw_data$lnEnergy, pch=1)
               
plot(density(filtered_raw_data$lnPcgdp))
plot(density(filtered_raw_data$lnEnergy))

matplot(filtered_raw_data$lnPcgdp,filtered_raw_data$lnEnergy, pch=1)               
res_miss <- summary(lm(lnEnergy ~ lnPcgdp, data = filtered_raw_data))
matplot(filtered_raw_data$lnPcgdp, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$lnPcgdp, t="l", col=2, add=T)
               
#Concentration
matplot(filtered_raw_data$Concentration, filtered_raw_data$lnEnergy, pch=1)
               
plot(density(filtered_raw_data$Concentration))
plot(density(log(filtered_raw_data$Concentration)))
plot(density(filtered_raw_data$lnEnergy))
               
matplot(filtered_raw_data$Concentration,filtered_raw_data$lnEnergy, pch=1)
res_miss <- summary(lm(filtered_raw_data$lnEnergy ~ filtered_raw_data$Concentration))
matplot(filtered_raw_data$Concentration, res_miss$coefficients[1,1] + res_miss$coefficients[2,1]*filtered_raw_data$Concentration, t="l", col=2, add=T)
               
matplot(log(filtered_raw_data$Concentration),filtered_raw_data$lnEnergy, pch=1)
res <- summary(lm(filtered_raw_data$lnEnergy ~ log(filtered_raw_data$Concentration)))
matplot(log(filtered_raw_data$Concentration), res$coefficients[1,1] + res$coefficients[2,1]*log(filtered_raw_data$Concentration), t="l", col=2, add=T)
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
### Overview ###
#Using huxreg 
huxreg(extramodel1_feols_RSE, extramodel2_feols_RSE, extramodel3_feols_RSE, extramodel4_feols_RSE, 
       statistics = c("N. obs." = "nobs", "R squared" = "r.squared"))



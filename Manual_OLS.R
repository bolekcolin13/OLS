# Load necessary packages
library(readxl) # to read excel files into R
#-------------------------------------------------------------------------------
# Open data file
vacation = read_excel("pathname{vacation.xlsx}")

# Subset data frame to get matrix of independent variables and reponses
vacation_X = as.matrix(vacation[2:5])

# Subset data frame to get vector of dependent responses
vacation_y = as.matrix(vacation[1])

# Create vector of covariates
vacation_XTX_inverse = solve(t(vacation_X) %*% vacation_X) # (X'X)^(-1)
vacation_XTy = t(vacation_X) %*% vacation_y # X'y
vacation_beta_hat = vacation_XTX_inverse %*% vacation_XTy # (X'X)^(-1)X'y 

# Calculate estimate of y vector
vacation_y_hat = vacation_X %*% vacation_beta_hat

# Calculate SSR, SST, and SSE for R^2
vacation_y_mean = mean(vacation_y)
SST = as.numeric(t(vacation_y - vacation_y_mean) %*% (vacation_y - 
                                                        vacation_y_mean))
SSR = as.numeric(t(vacation_y_hat - vacation_y_mean) %*% (vacation_y_hat - 
                                                            vacation_y_mean))
SSE = as.numeric(t(vacation_y - vacation_y_hat) %*% (vacation_y - 
                                                       vacation_y_hat))

# Robustness check: SSE as calculated above should be equal to the squared error
# of the estimate. Return TRUE if R^2-related calculations are accurate to this 
# point.
vacation_errors = vacation_y - vacation_y_hat
SSE == as.numeric(t(vacation_errors) %*% vacation_errors)

# Calculate R^2 and adjusted R^2
R2 = 1 - (SSE/SST)
SSE_Adjusted = SSE/(length(vacation_y) - length(vacation_beta_hat))
SST_Adjusted = SST/(length(vacation_y) - 1)
R2_Adjusted = 1 - (SSE_Adjusted/SST_Adjusted)

# Calculate variance and std. errors of covariate vector \beta
sigma2_hat = SSE/(length(vacation_y) - length(vacation_beta_hat)) 
beta_var = sigma2_hat * vacation_XTX_inverse
se_vacation = as.matrix(sqrt(diag(beta_var)))

# Calculate robust (HC1) std. errors using sandwich method
omega = diag(diag(vacation_errors%*%t(vacation_errors)))
sandwich = vacation_XTX_inverse %*% t(vacation_X) %*% omega %*% vacation_X %*%
  vacation_XTX_inverse
sandwich_hc1 = sandwich * (length(vacation_y)/(length(vacation_y)-
                                                 length(vacation_beta_hat)))
se_vacation_robust = as.matrix(sqrt(diag(sandwich_hc1)))

# Conduct significance testing on \beta and save all values for later table
p_values = c()
t_values = c()
for (i in 1:length(vacation_beta_hat)) {
  t_stat = as.numeric(vacation_beta_hat[i,1]/se_vacation[i,1])
  t_values = c(t_values, t_stat)
  degree = as.numeric(length(vacation_y) - length(vacation_beta_hat))
  t_prob = 2*pt(abs(t_stat), degree, lower.tail = FALSE)
  p_values = c(p_values, t_prob)
}

# Conduct significance testing on \beta using robust standard errors and save
# all values for later table
p_values_robust = c()
t_values_robust = c()
for (i in 1:length(vacation_beta_hat)) {
  t_stat_robust = as.numeric(vacation_beta_hat[i,1]/se_vacation_robust[i,1])
  t_values_robust = c(t_values_robust, t_stat_robust)
  degree_robust = as.numeric(length(vacation_y) - length(vacation_beta_hat))
  t_prob_robust = 2*pt(abs(t_stat_robust), degree, lower.tail = FALSE)
  p_values_robust = c(p_values_robust, t_prob_robust)
}

# Compose a table like R's in-built lm() call:
# for the covariate table
t_values = as.matrix(t_values)
p_values = as.matrix(p_values)
results = cbind(vacation_beta_hat, se_vacation, t_values, p_values)
colnames(results) = c("Covariate", "Std. Error", "T Statistic", "P-Value")
# for the residual standard error = sqrt(MSE)
RSE = sqrt(SSE/(length(vacation_y)-length(vacation_beta_hat)))
# for the F statistic
MSR = SSR/(length(vacation_beta_hat) - 1)
MSE = RSE^2
F = MSR/MSE
f_prob = pf(F, length(vacation_beta_hat) - 1, length(vacation_y) - 
              length(vacation_beta_hat), lower.tail = FALSE)
# put it all together and display to user
table = capture.output(print(results))
details = c(paste0("Residual Standard Error: ", RSE, " on ", degree, 
            " degrees of freedom"), paste0("R^2 = ", R2, " /// Adjusted R^2 = ", 
            R2_Adjusted), paste0("F-Statistic: ", F, " on ", 
            length(vacation_beta_hat)-1, " and ", 
            length(vacation_y) - length(vacation_beta_hat),
            " degrees of freedom, p-value: ", f_prob)) 
full = c(table, "", details)
cat(paste(full, collapse = "\n"))

# Compose a table like R's built-in lm() call with robust std. errors:
# for the covariate table
t_values_robust = as.matrix(t_values_robust)
p_values_robust = as.matrix(p_values_robust)
results = cbind(vacation_beta_hat, se_vacation_robust, t_values_robust, 
                p_values_robust)
colnames(results) = c("Covariate", "Robust Std. Error", "T Statistic", 
                      "P-Value")
# for the residual standard error = sqrt(MSE)
RSE = sqrt(SSE/(length(vacation_y)-length(vacation_beta_hat)))
# for the F statistic
MSR = SSR/(length(vacation_beta_hat) - 1)
MSE = RSE^2
F = MSR/MSE
f_prob = pf(F, length(vacation_beta_hat) - 1, length(vacation_y) - 
              length(vacation_beta_hat), lower.tail = FALSE)
# put it all together and display to user
table = capture.output(print(results))
details = c(paste0("Residual Standard Error: ", RSE, " on ", degree, 
            " degrees of freedom"), paste0("R^2 = ", R2, " /// Adjusted R^2 = ", 
            R2_Adjusted), paste0("F-Statistic: ", F, " on ", 
            length(vacation_beta_hat)-1, " and ", 
            length(vacation_y) - length(vacation_beta_hat),
            " degrees of freedom, p-value: ", f_prob)) 
full = c(table, "", details)
cat(paste(full, collapse = "\n"))

# ------------------------------------------------------------------------------
# The accuracy of the above code can be verified using the following packages
# and calls.
# ------------------------------------------------------------------------------

# To verify the basic homoskedastic linear model:
model = lm(miles ~ income + age + kids, data = vacation)
summary(model)

# To verify the HC1 robust standard errors:
library(sandwich)
library(lmtest)
coeftest(model, vcov. = vcovHC(model, type = "HC1"))

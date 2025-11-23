# Import necessary packages.
import pandas as pd # for basic dataframe manipulation
import numpy as np # for handling arrays and other mathematical operations
import openpyxl as opx # to deal with data inputted from Excel
from scipy import stats # for p-values
import statsmodels.formula.api as smf # to verify accuracy of manual output

# Read in the data
vacation = pd.read_excel("pathname{vacation.xlsx}")

# Subset the data into vector of dependent responses and matrix of independent 
# explanatory variables, then convert to numpy array
vacation_y = vacation["miles"].to_numpy()
vacation_X = vacation[["cons", "income", "age", "kids"]].to_numpy()

# Create vector of predictors, \beta.
vacation_XTX_inverse = np.linalg.inv(vacation_X.T @ vacation_X)
vacation_XTy = vacation_X.T @ vacation_y
vacation_beta_hat = vacation_XTX_inverse @ vacation_XTy

# Calculate the estimate for the y vector and retrieve the error vector.
vacation_y_hat = vacation_X @ vacation_beta_hat
vacation_errors = (vacation_y - vacation_y_hat).reshape(-1,1)

# Calculate the model's SST, SSR, and SSE for R^2. 
vacation_y_mean = np.mean(vacation_y)

SST = (vacation_y - vacation_y_mean).T @ (vacation_y - vacation_y_mean)
SSR = (vacation_y_hat - vacation_y_mean).T @ (vacation_y_hat - vacation_y_mean)
SSE = (vacation_y - vacation_y_hat).T @ (vacation_y - vacation_y_hat)

# Robustness check: SSE as calculated above should be equal to the squared error
# of the estimate. Return TRUE if R^2-related calculations are accurate to this 
# point.
print(SSE == vacation_errors.T @ vacation_errors)

# Calculate R^2 and Adjusted R^2
R2 = 1 - (SSE/SST)
SSE_adjusted = SSE/(len(vacation_y) - len(vacation_beta_hat))
SST_adjusted = SST/(len(vacation_y) - 1)
R2_adjusted = 1-(SSE_adjusted/SST_adjusted)

# Calculate the variance and standard errors of the covariate vector, \beta
sigma2_hat = SSE/(len(vacation_y)-len(vacation_beta_hat))
beta_var = sigma2_hat * vacation_XTX_inverse
beta_se = np.sqrt(np.diag(beta_var))

# Calculate robust (HC1) standard errors of the covariate vector, \beta
omega = np.diag(np.diag(vacation_errors @ vacation_errors.T))
sandwich = (vacation_XTX_inverse @ vacation_X.T @ omega @ vacation_X @ 
            vacation_XTX_inverse)
sandwich_HC1 = sandwich * (len(vacation_y)/(len(vacation_y) - 
                                            len(vacation_beta_hat)))
beta_se_robust = np.sqrt(np.diag(sandwich_HC1))

# Conduct (robust) significance testing on \beta and save all values to be 
# featured in a later table.
degree = len(vacation_y) - len(vacation_beta_hat)
# non-robust errors
t_values = []
p_values = []
for i in range(len(vacation_beta_hat)):
    t_stat = vacation_beta_hat[i]/beta_se[i]
    t_values.append(t_stat)
    t_prob = 2*(1 - stats.t.cdf(abs(t_stat), degree))
    p_values.append(t_prob)
# robust (HC1) errors
t_values_robust = []
p_values_robust = []
for i in range(len(vacation_beta_hat)):
    t_stat_robust = vacation_beta_hat[i]/beta_se_robust[i]
    t_values_robust.append(t_stat_robust)
    t_prob_robust = 2*(1 - stats.t.cdf(abs(t_stat_robust), degree))
    p_values_robust.append(t_prob_robust)
# convert to np array for similarity to vacation_beta_var, beta_se, and 
# beta_se_robust
t_values = np.array(t_values)
p_values = np.array(p_values)
t_values_robust = np.array(t_values_robust)
p_values_robust = np.array(p_values_robust)

# Compose a table to present all relevant information:
# rounding for visual presentation
pd.options.display.float_format = "{:,.4f}".format
# equalize dimensions across arrays of interest
vacation_beta_hat = vacation_beta_hat.reshape(-1, 1)
beta_se = beta_se.reshape(-1, 1)
t_values = t_values.reshape(-1, 1)
p_values = p_values.reshape(-1, 1)
beta_se_robust = beta_se_robust.reshape(-1, 1)
t_values_robust = t_values_robust.reshape(-1, 1)
p_values_robust = p_values_robust.reshape(-1, 1)
# combine associated arrays into two arrays: one standard and one robust
homosk = np.hstack([vacation_beta_hat, beta_se, t_values, p_values])
heterosk = np.hstack([vacation_beta_hat, beta_se_robust, t_values_robust, 
                      p_values_robust])
# prepare row and column names, convert back to pandas data frame, and combine
rownames = ['Intercept', 'Income', 'Age', 'Kids']
colnames = ['Estimate', 'Std. Error', 't Statistic', 'p Value']
colnames_robust = ['Estimate', 'Robust Std. Error', 't Statistic', 'p Value']
homosk = pd.DataFrame(homosk, index = rownames, columns = colnames)
heterosk = pd.DataFrame(heterosk, index = rownames, columns = colnames_robust)

# Prepare supplementary information regarding RSE and F statistic
# for the residual standard error = sqrt(MSE)
RSE = np.sqrt(SSE/(len(vacation_y)-len(vacation_beta_hat)))
# for the F statistic
MSR = SSR/(len(vacation_beta_hat) - 1)
MSE = RSE**2
F = MSR/MSE
f_prob = stats.f.cdf(F, len(vacation_beta_hat) - 1, len(vacation_y) - 
                     len(vacation_beta_hat))
# combine all supplementary information
details = (
    f"\nResidual Standard Error: {RSE:.4f} on "
    f"{len(vacation_y) - len(vacation_beta_hat)} degrees of freedom\n"
    f"Multiple R^2: {R2:.4f},    Adjusted R^2: {R2_adjusted:.4f}\n"
    f"F-statistic: {F:.4f} on {len(vacation_beta_hat) - 1} and "
    f"{len(vacation_y) - len(vacation_beta_hat)} degrees of freedom"
)
# combine all information for both error structures
full_homosk = homosk.to_string() + "\n" + details
full_heterosk = heterosk.to_string() + "\n" + details


# Define and call a function so that the user can choose which model to display
def lm():
    prompt = input("Would you like to display linear regression results under" \
    " assumption of homoskedasticity, heteroskedasticity (with HC1" \
    " correction) or both? Input '1' for homoskedastic assumption, '2' for " \
    "heteroskedasitic assumption, '3' for both, or 'escape' to exit the " \
    "function. ")
    if prompt == str(1):
        print(
        "\033[4mStandard Linear Regression\n\033[0m" 
        + full_homosk
        )
    elif prompt == str(2): 
        print(
        "\033[4mRobust (HC1) Linear Regression\n\033[0m" 
        + full_heterosk
        )
    elif prompt == str(3): 
        print(
        "\033[4mStandard Linear Regression\n\033[0m" 
        + full_homosk
        )
        print(
        "\033[4mRobust (HC1) Linear Regression\n\033[0m" 
        + full_heterosk
        )
    else: 
        return
lm()

"""
The accuracy of the above code can be verified using the following packages and 
calls, specifically from the Statsmodels package.
"""

# To verify the homoskedastic model.
model = smf.ols(formula = 'miles ~ income + age + kids', data = vacation).fit()
print(model.summary())

# To verify the heteroskedastic model.
model_r = smf.ols(formula = 'miles ~ income + age + kids', data = 
                  vacation).fit(cov_type = 'HC1')
print(model_r.summary())

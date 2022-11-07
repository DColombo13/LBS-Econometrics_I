## EXERCISE 1 ==================================================================
rm(list = ls())
library(dplyr)
library(readxl)

#import data
data <- read_excel("C:/Users/ASUS/Desktop/LBS/Courses/Econometrics I - P218/Problem Sets/PS2/SP500Index.xlsx")

SP_t.SP_0 = rep(0L, nrow(data))
for (i in 1:nrow(data)) {
  SP_t.SP_0[i] = data$`Level of the S&P 500 Index`[i]/data$`Level of the S&P 500 Index`[1]
}
data$SP_t.0 = log(SP_t.SP_0)
#d) --- Find numerically \hat\delta_ML and sqrt(\hat\sigma^2_ML)
TT = nrow(data) - 1
delta_hat_ML = last(data$SP_t.0)/TT
sigma2 = (data$SP_t.0[1] - delta_hat_ML)^2
for (i in 3:nrow(data)) {
  sigma2 = sigma2 + (data$SP_t.0[i] - data$SP_t.0[i-1] - delta_hat_ML)^2
}
sigma2_hat = sigma2/TT
sigma_hat_ML = sqrt(sigma2_hat)



## EXERCISE 2 ==================================================================
rm(list = ls())
library(haven)

data <- read_dta("C:/Users/ASUS/Desktop/LBS/Courses/Econometrics I - P218/Problem Sets/PS2/PS1.dta")
#Create variables
data$lwage = log(data$w0)
data$educ = data$ed0
data$age = data$a0
data$exper = data$age-(data$educ+6)
sort(unique(data$educ)) #don't have educ_0 and educ_2
sort(unique(data$exper)) #don't have exper_23 and exper_24

reg_restr = lm(lwage ~ educ + exper, data = data) #model where effects imposed to be linear
summary(reg_restr)
resid_restr = residuals.lm(reg_restr)
SSR_restr = sum(resid_restr^2)

reg_unr = lm(lwage ~ as.factor(educ) + as.factor(exper), data = data) #unrestricted model
summary(reg_unr) 
resid_unr = residuals.lm(reg_unr)
SSR_unr = sum(resid_unr^2)

n = nrow(data) #number of observations
## WRONG F-test
k1 = 1 + 21 + 26 - 2 #number of parameters in unrestricted model
k2 = 3 #number of parameters in restricted model
p = k1 - k2 #number of restrictions
F_stat = ((SSR_restr - SSR_unr)/p)/(SSR_unr/(n-k1)) #distributed as F(p,n-k)
F_crit = qf(0.05, p, n-k1, lower.tail = F) #Rejected if F_stat > critical value  
F_stat > F_crit #Cannot reject

## CORRECT F-test
k1 = 1 + length(unique(data$educ)) + length(unique(data$exper)) - 2 - 2 #number of parameters in unrestricted model
k2 = 3 #number of parameters in restricted model
p = k1 - k2 #number of restrictions
F_stat = ((SSR_restr - SSR_unr)/p)/(SSR_unr/(n-k1)) #distributed as F(p,n-k)
F_crit = qf(0.05, p, n-k1, lower.tail = F) #Rejected if F_stat > critical value  
F_stat > F_crit #Cannot reject



## EXERCISE 3 ==================================================================
# y_i = 1 + 0.5z_i + \e_i, with e uniform [-1,1] and z X^2(3)
rm(list = ls())
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)

n = c(10, 100, 1000, 100000) #number of observations
beta = c(1, 0.5) #coefficients
var_zi = 6 #2 times the d.o.f.
sigma2 = (1+1)^2/12 #variance of uniform distribution: (1/12)*(b-a)^2
k = length(beta)#d.o.f adjustment
MC = 1000 #Monte Carlo iterations
beta_hat = array(NA, c(MC, length(beta), length(n))) #list where store OLS estimated coefficients
sigma2_hat = array(NA, c(MC, 1, length(n))) #list where store estimates of sigma^2

for (nn in n) { #TAKES AROUND ONE MINUTE!
  for (i in 1:MC) {
    set.seed(i)
    e = runif(nn, -1, 1) #structural residuals
    z = rchisq(nn,df=3) #indep variable
    y = beta[1] + beta[2]*z + e #dep variable
    intercept = rep(1, nn) 
    x = cbind(intercept, z) #data matrix
    beta_hat_i = solve(t(x)%*%x)%*%t(x)%*%y #OLS coefficients estimation
    beta_hat[i,,which(nn == n)] = beta_hat_i #store 
    y_hat = x%*%beta_hat_i #predicted values
    e_hat = y - y_hat #estimated residuals
    SSR = t(e_hat)%*%e_hat #SSR
    sigma2_hat[i,,which(nn == n)] = SSR/(nn-k) #estimate of sigma^1
  }
}

#a) and b) --- check unbiasedness and consistency
colMeans(beta_hat[,,1]) #n = 10 
var(beta_hat[,,1])
colMeans(beta_hat[,,2]) #n = 100
var(beta_hat[,,2])
colMeans(beta_hat[,,3]) #n = 1000
var(beta_hat[,,3])
colMeans(beta_hat[,,4]) #n = 100000
var(beta_hat[,,4])

#c) --- check slope asymptotically normal with asymptotic variance sigma^2/Var(zi)
var_beta_hat = var(beta_hat[,2,4])
sigma2/(var_zi*100000)

beta_hat_tidy = data.frame(beta_hat[,2,4]) %>%
  rename(beta = 1)
 
#plot pdfs
plot <- beta_hat_tidy %>% 
  ggplot(aes(x = beta)) +
  geom_density(color = "darkred", fill = "darkred", #plot empirical density (red filled)
               size = 1, alpha = 0.3) +
  geom_function(fun = dnorm, #plot density of normal distribution (black dashed)
                n = 100,
                args = list(mean = beta[2], sd = sqrt((sigma2/var_zi)/100000)),
                alpha = 1, size = 1, linetype = "dashed") +
  labs(title = paste0("Density of beta_hat and density of a N(beta, sigma^2/Var(z_i))"),
       x = "beta_hat",
       y = "Density") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
plot

#plot cdfs
ecdf <- ggplot(beta_hat_tidy, aes(x=beta)) +
  stat_ecdf(geom = "step", aes(colour = "Empirical CDF")) +
  stat_function(fun=pnorm, aes(colour = "Theoretical CDF"), 
                args = list(mean = beta[2], sd = sqrt((sigma2/var_zi)/100000))) +
  labs(title = paste0("Empirical CDF and CDF of a N(beta, sigma^2/Var(z_i))"),
       x = "beta",
       y = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
ecdf

#d) --- check SSR/(n-k) is an unbiased estimator of sigma^2
abs(mean(sigma2_hat[,,1]) - sigma2) #n = 10
abs(mean(sigma2_hat[,,2]) - sigma2) #n = 100
abs(mean(sigma2_hat[,,3]) - sigma2) #n = 1000
abs(mean(sigma2_hat[,,4]) - sigma2) #n = 100000






## EXERCISE 2 ==================================================================
rm(list = ls())
library(dplyr)
library(readxl)

#import data
data <- read_excel("C:/Users/ASUS/Desktop/LBS/Courses/Econometrics I - P218/Problem Sets/PS3/Nerlove1963/Nerlove1963.xlsx")

#create transformed variables
n = nrow(data)
data$logTC <- log(data$Cost)
data$logQ <- log(data$output)
data$logPL <- log(data$Plabor)
data$logPK <- log(data$Pcapital)
data$logPF <- log(data$Pfuel)

#run baseline and Nerlov's regression
regr0 <- lm(logTC ~ logQ + logPL + logPK + logPF, data)
summary(regr0)

# (a) test hypothesis that logQ^2 should be in regression
regr1 <- lm(logTC ~ logQ + logPL + logPK + logPF + I(logQ^2), data)
summary(regr1)  #coefficient for logQ^2 extremely significant, don't reject with t-test

# (b) impose restriction and estimate model by NLS
beta7_range = c(median(data$logQ) - 1*sd(data$logQ) - 1/2, median(data$logQ) + 1*sd(data$logQ) - 1/2) #select value of beta7
beta7_range = c(3.76, 8.67)
sum(data$logQ < beta7_range[1]) #check that many values below lower beta7
sum(data$logQ > beta7_range[2]) #check that many values above higher beta7
interval = (beta7_range[2] - beta7_range[1])/1000
beta7s <- seq(from = beta7_range[1], to = beta7_range[2], by = interval)[1:1000] #vector of all possible values of beta7
SSR = vector("numeric", length(beta7s)) #will store SSRs in this vector

i=1
for(b in beta7s){
  z = data$logQ*(1+exp(b-data$logQ))^(-1)
  model = lm(log(Cost/Pfuel) ~ logQ + log(Plabor/Pfuel) + log(Pcapital/Pfuel) + z, data = data)
  SSR[i] = sum(resid(model)^2)
  i = i+1
}

plot(SSR)
index = which.min(SSR)
beta7_hat = beta7s[index]
beta7_hat

# (c) compute standard errors beta1-beta7
data$zi <- data$logQ*(1 + exp(beta7_hat - data$logQ))^(-1) #rerun model with estimated beta7
regr2. <- lm(log(Cost/Pfuel) ~ logQ + log(Plabor/Pfuel) + log(Pcapital/Pfuel) + zi, data = data)
summary(regr2.) #standard errors under independence assumption
beta6_hat = regr2.$coefficients[5]
SSR = sum(resid(regr2.)^2)
sigma2_hat = SSR/(n-6) #estimate of sigma^2

#derivative of h
data$beta1_d = 1
data$beta2_d = data$logQ
data$beta3_d = data$logPK - data$logPF
data$beta4_d = data$logPL - data$logPF
data$beta6_d = data$zi
data$beta7_d = -beta6_hat*(exp(data$logQ)*exp(beta7_hat)*data$logQ)/(exp(data$logQ) + exp(beta7_hat))^2
der_mat = data %>%
  select(beta1_d, beta2_d, beta3_d, beta4_d, beta6_d, beta7_d)
var_beta_hat = sigma2_hat*solve(t(as.matrix(der_mat))%*%as.matrix(der_mat)) #varcov matrix
se = sqrt(diag(var_beta_hat)) #standard errors
se



## EXERCISE 3 ==================================================================
rm(list = ls())
library(dplyr)
library(readxl)
library(dplyr)

#import data
data <- read_excel("C:/Users/ASUS/Desktop/LBS/Courses/Econometrics I - P218/Problem Sets/PS3/wage.xlsx")

# (d)
n1 = nrow(data) - sum(data$male) #number of females
n2 = nrow(data) - n1 #number of females
data$l_wage = log(data$wage)
f_wage = data %>% #wage of females
  filter(male == 0) %>%
  select(l_wage)
m_wage = data %>% #wage of males
  filter(male == 1) %>%
  select(l_wage)
Ybar1 = mean(unlist(f_wage)) #mean wage for females
Ybar2 = mean(unlist(m_wage)) #mean wage for males
Y_Ybar1.4 = 0
for (i in 1:nrow(f_wage)) {
  Y_Ybar1.4 = Y_Ybar1.4 + (f_wage$l_wage[i] - Ybar1)^4
}
Y_Ybar2.4 = 0
for (i in 1:nrow(m_wage)) {
  Y_Ybar2.4 = Y_Ybar2.4 + (m_wage$l_wage[i] - Ybar2)^4
}
Y_Ybar1.2 = 0
for (i in 1:nrow(f_wage)) {
  Y_Ybar1.2 = Y_Ybar1.2 + (f_wage$l_wage[i] - Ybar1)^2
}
Y_Ybar2.2 = 0
for (i in 1:nrow(m_wage)) {
  Y_Ybar2.2 = Y_Ybar2.2 + (m_wage$l_wage[i] - Ybar2)^2
}
k_hat = ((n1 + n2)*(Y_Ybar1.4 + Y_Ybar2.4))/(Y_Ybar1.2 + Y_Ybar2.2)^2
s1.2 = Y_Ybar1.2/(n1-1)
s2.2 = Y_Ybar2.2/(n2-1)
TT = sqrt((n1*n2)/(n1 + n2))*(log(s1.2) - log(s2.2))
test_stat = TT/sqrt(k_hat-1)

# Conduct robust test
test_stat > 1.64 #do not reject Ho

# Conduct standard test
s1.2/s2.2 > qf(0.05, n1-1, n2-1, lower.tail = F) #reject Ho



## EXERCISE 6 ==================================================================
rm(list = ls())
library(dplyr)
library(readxl)
library(AER)
library(estimatr)
library(car)
library(sandwich)

#import data
data <- read_excel("C:/Users/ASUS/Desktop/LBS/Courses/Econometrics I - P218/Problem Sets/PS3/PS4data.xls")
n = nrow(data) #number of observations

# (a) create pro-capita variables
data$cons_pc <- (data$`real consumption of nondurables` + data$`real consumption of services`)/data$population
data$cons_pc1 <- lag(data$cons_pc)
data$cons_pc2 <- lag(data$cons_pc1)
data$cons_pc3 <- lag(data$cons_pc2)
data$cons_pc4 <- lag(data$cons_pc3)

#run full (unrestricted) regression
reg_unr <- lm(cons_pc ~ cons_pc1 + cons_pc2 + cons_pc3 + cons_pc4, data)
summary(reg_unr)
resid_unr = residuals.lm(reg_unr)
SSR_unr = sum(resid_unr^2)

#run restricted regression
reg_restr <- lm(cons_pc ~ cons_pc1, data)
summary(reg_restr)
resid_restr = residuals.lm(reg_restr)
SSR_restr = sum(resid_restr^2)

#run F-test to test hp that B2=B3=B4=0
p = 3 #number of restrictions imposed in restricted model
k = 5 #number of parameters estimated in unrestricted model
F_stat = ((SSR_restr - SSR_unr)/p)/(SSR_unr/(n-k)) #distributed as F(p,n-k)
F_crit = qf(0.05, p, n-k, lower.tail = F) #rejected if F_stat > critical value  
F_stat > F_crit #reject the null (reject restricted model)

# (c) run 2SLS and compute White's robust se
data$delta_cons <- log(data$cons_pc/data$cons_pc1)
data$instr1 <- lag(lag(data$delta_cons))
data$instr2 <- lag(lag(lag(data$delta_cons)))
data$instr3 <- lag(lag(lag(lag(data$delta_cons))))
data$instr4 <- lag(lag(lag(lag(lag(data$delta_cons)))))
data$delta_income <- log((data$`real disposable income`/data$population)/(lag(data$`real disposable income`/data$population)))
#first stage
first_stage <- lm(delta_income ~ instr1 + instr2 + instr3 + instr4, data = data)
summary(first_stage)
#second stage
reg_iv <- ivreg(delta_cons ~ delta_income | instr1 + instr2 + instr3 + instr4, data = data)
summary(reg_iv) #non-robust se
reg_iv_robust <- iv_robust(delta_cons ~ delta_income | instr1 + instr2 + instr3 + instr4, data = data)
summary(reg_iv_robust) #robust se
#test relevance
instrFtest <- waldtest(first_stage,. ~ . - instr1 - instr2 - instr3 - instr4)
print(instrFtest) #reject Ho that instruments irrelevant, but only slightly
#compare to standard OLS
reg_ols <- lm(delta_cons ~ delta_income, data = data)
summary(reg_ols)

# (d) test exogeneity and overidentifying restrictions
#exogeneity
data$first_stage_res <- c(rep(NA,6), first_stage$residuals)
Hausman_reg <- lm(delta_cons ~ delta_income + first_stage_res, data = data)
print(summary(Hausman_reg))
HausWutest <- waldtest(Hausman_reg,.~.-first_stage_res, test = "Chisq")
print(HausWutest) #fail to reject Ho that instruments exogenous
#overidentifying
data$iv_res <- c(rep(NA,6), reg_iv$residuals)
Sargan_reg <- lm(iv_res ~ instr1 + instr2 + instr3 + instr4, data = data)
Sargan_reg_sm <- summary(Sargan_reg)
Sargan_test <- Sargan_reg_sm$r.squared * (nrow(data) - 6)
print(Sargan_test)
print(1-pchisq(Sargan_test,1))*10  #prints p-value: reject Ho that additional instruments exogenous (don't reject overidentification)
## All tests at once
summary(object = reg_iv, diagnostics = T)

#tests for robust IV
summary(reg_iv, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics = T)


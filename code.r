library(tseries)
library("dplyr")
library(summarytools)
library(xlsx)
library(psych)
library(midasr)
library(broom)
library(ggplot2)
library(lmtest)
library(vars)
library(TSstudio)
library(mFilter)
library(forecast)
library(tidyverse)
library(caret)
library(leaps)
library(car)

#### FINAL DATA FOR ANALYSIS
data <- read.csv(file = 'data.csv', sep=",", dec=".", header=TRUE, stringsAsFactors = FALSE)
#data <- subset (data, select = -X)
data <- select_if(data, is.numeric)
data$exchange_balance. <- NULL
data$pctutxo_in_profit_change <- NULL
data$hodl_5y_7y_change <- NULL
data$hodl_7y_10y_change <- NULL
data$hodl_more_10y_change <- NULL
data$nvt_signal_change <- NULL 
data$nvt_ratio <- NULL

View(data)

## Stationarity tests
# Hypothesis 0 (H0): Assumption of the test holds and is failed to be rejected at some level of significance.
# Hypothesis 1 (H1): Assumption of the test does not hold and is rejected at some level of significance.
# If p-value > alpha: Fail to reject the null hypothesis (i.e. not significant result).
# If p-value <= alpha: Reject the null hypothesis (i.e. significant result).
# If test statistic < critical value: Fail to reject the null hypothesis.
# If test statistic >= critical value: Reject the null hypothesis.
# ADF H0 = non-stationarity H1 = stationarity
# ADF critical values: -3.12 (10%), -3.41 (5%), -3.96 (1%)
# kpss.test null= stationarity
# pp.test null = NON-stationarity
# Test d'hétéroscédasticité (Breusch-Pagan - bptest)
# null hypothesis of homoskedasticity

x <- data$active_addresses_change 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$btc_returns 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$addresses_balance_1k_change 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$coin_days_destroyed 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$exchange_balancepct_change 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$exchange_netflow 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$fee_ratio_multiple_change
x = ts(x, frequency = 1)
adf.test(x)
x <- data$hodl_24h 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$hodl_1d_1w 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$hodl_1w_1m 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$hodl_1m_3m 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$hodl_3m_6m_change 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$hodl_6m_12m_change 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$hodl_1y_2y_change 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$hodl_2y_3y_change 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$hodl_3y_5y_change 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$liveliness_change
x = ts(x, frequency = 1)
adf.test(x)
x <- data$nvt_ratio
x = ts(x, frequency = 1)
adf.test(x)
x <- data$nvt_signal_change
x = ts(x, frequency = 1)
adf.test(x)
x <- data$pctsupply_in_profit_change 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$pctutxo_in_profit_change
x = ts(x, frequency = 1)
adf.test(x)
x <- data$circ_supply_lastactive_1yplus_change
x = ts(x, frequency = 1)
adf.test(x)
x <- data$dormancy 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$hashrate_change 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$Gold_returns 
x = ts(x, frequency = 1)
adf.test(x)
x <- data$m2_change
x = ts(x, frequency = 1)
adf.test(x)
x <- data$deficit_change
x = ts(x, frequency = 1)
adf.test(x)

## Descriptive statistics
summary(data)
StatDes <- summarytools::descr(data)
print(StatDes, short = FALSE)
write.xlsx(StatDes, file="descriptive_statistics.xlsx", sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

## Correlation matrix
out <- corr.test(data, y = NULL, use = "pairwise",method="pearson",adjust="holm",alpha=.05, ci=TRUE)
print(out, short = FALSE)
mat.c.p <- lower.tri(out$r)*out$r 
mat.c.p
write.xlsx(mat.c.p, file="correlation1.xlsx", sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

out <- corr.test(data, y = NULL, use = "pairwise",method="kendall",adjust="holm",alpha=.05, ci=TRUE)
print(out, short = FALSE)
mat.c.p <- lower.tri(out$p)*out$p 
mat.c.p
write.xlsx(mat.c.p, file="correlation2.xlsx", sheetName="Sheet2", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

### LINEAR REGRESSION
## Model auto selection

# forward elimination based on AIC
null<-lm(btc_returns~ 1, data=data) # 1 here means the intercept 
full<-lm(btc_returns~ active_addresses_change + coin_days_destroyed + exchange_netflow + hodl_24h + 
           hodl_1d_1w + hodl_1w_1m + hodl_1m_3m + hodl_3m_6m_change +
           hodl_6m_12m_change + hodl_1y_2y_change + hodl_2y_3y_change +
           hodl_3y_5y_change + dormancy + 
           pctsupply_in_profit_change + 
           addresses_balance_1k_change + hashrate_change + fee_ratio_multiple_change +
           exchange_balancepct_change + liveliness_change +
           circ_supply_lastactive_1yplus_change + Gold_returns + m2_change + deficit_change, data=data)
stepAIC(null, scope=list(lower=null, upper=full), data=data, direction='forward')

# stepwise based on AIC
null<-lm(btc_returns~ 1, data=data) # 1 here means the intercept 
full<-lm(btc_returns~ active_addresses_change + coin_days_destroyed + exchange_netflow + hodl_24h + 
           hodl_1d_1w + hodl_1w_1m + hodl_1m_3m + hodl_3m_6m_change +
           hodl_6m_12m_change + hodl_1y_2y_change + hodl_2y_3y_change +
           hodl_3y_5y_change + dormancy + 
           pctsupply_in_profit_change +
           addresses_balance_1k_change + hashrate_change + fee_ratio_multiple_change +
           exchange_balancepct_change + liveliness_change + 
           circ_supply_lastactive_1yplus_change + Gold_returns + m2_change + deficit_change, data=data)
stepAIC(null, scope=list(lower=null, upper=full), data=data, direction='both')

## General model based on stepwise/forward/backwards elimination          
model = lm(btc_returns ~ pctsupply_in_profit_change + hodl_1d_1w + 
             active_addresses_change + exchange_netflow + fee_ratio_multiple_change + 
             hodl_1y_2y_change + deficit_change + hodl_1m_3m, data=data)
summary(model)
AIC(model)
# Tests on the residuals 
# Residual plot
hist(residuals(model))
# Analysis of normality 
residplot <- ggplot(aes(x=.fitted, y=.resid),data=model)+geom_point()+geom_hline(yintercept=0)+labs(x="Fitted Values",y="Residuals")
residplot
boxplot(residuals(model))
stdresid = rstandard(model)
qqnorm(stdresid, ylab="Standardized Residuals", xlab="Normal Scores", main="Are residuals normally distributed?") 
qqline(stdresid)
# Normality test (an alternative is Jarque-Bera)
shapiro.test(residuals(model))
jarque.bera.test(residuals(model))
# Test d'hétéroscédasticité (Breusch-Pagan - bptest)
bptest(model)
# Breusch Godfrey test for serial correlation
bgtest(model, order = 2, data=data)
# if there is heteroskedasticity -> regression with robust standard errors
coeftest(model, vcov = vcovHAC)

## DEMAND
demand = lm(btc_returns~ active_addresses_change, data=data)
summary(demand)
AIC(demand)
# Tests on the residuals 
# Normality test (an alternative is Jarque-Bera)
shapiro.test(residuals(demand))
# Test d'hétéroscédasticité (Breusch-Pagan - bptest)
bptest(demand)
# Breusch Godfrey test for serial correlation
bgtest(demand, order = 2, data=data)
## DEMAND ROBUST STANDARD ERRORS REGRESSION
coeftest(demand, vcov = vcovHAC)
linearHypothesis(demand, "active_addresses_change", vcov=vcovHAC)

## Demand controlled by security
demandsec = lm(btc_returns~ active_addresses_change + hashrate_change + fee_ratio_multiple_change, data=data)
summary(demandsec)
AIC(demandsec)
coeftest(demandsec, vcov = vcovHAC)
# Normality test (an alternative is Jarque-Bera)
shapiro.test(residuals(demandsec))
# Test d'hétéroscédasticité (Breusch-Pagan - bptest)
bptest(demandsec)
# Breusch Godfrey test for serial correlation
bgtest(demandsec, order = 2, data=data)

## Demand controlled by security and macro
demandmacro = lm(btc_returns~ active_addresses_change + hashrate_change + fee_ratio_multiple_change + Gold_returns + m2_change + deficit_change, data=data)
summary(demandmacro)
AIC(demandmacro)
reg_demandmacro <- tidy(demandmacro)
coeftest(demandmacro, vcov = vcovHAC)
# Normality test (an alternative is Jarque-Bera)
shapiro.test(residuals(demandmacro))
# Test d'hétéroscédasticité (Breusch-Pagan - bptest)
bptest(demandmacro)
# Breusch Godfrey test for serial correlation
bgtest(demandmacro, order = 2, data=data)

## SUPPLY
# stepwise based on AIC
null<-lm(btc_returns~ 1, data=data) # 1 here means the intercept 
full<-lm(btc_returns~ hodl_24h + hodl_1d_1w + hodl_1w_1m + hodl_1m_3m + hodl_3m_6m_change +
           hodl_6m_12m_change + hodl_1y_2y_change + hodl_2y_3y_change + hodl_3y_5y_change + 
           liveliness_change + coin_days_destroyed + dormancy + exchange_netflow + exchange_balancepct_change
         + circ_supply_lastactive_1yplus_change + addresses_balance_1k_change + 
           pctsupply_in_profit_change, data=data, direction='both')
stepAIC(null, scope=list(lower=null, upper=full), data=data, direction='both')

supplyall = lm(btc_returns~ hodl_24h + hodl_1d_1w + hodl_1w_1m + hodl_1m_3m + hodl_3m_6m_change +
                 hodl_6m_12m_change + hodl_1y_2y_change + hodl_2y_3y_change + hodl_3y_5y_change + 
                 liveliness_change + coin_days_destroyed + dormancy + exchange_netflow + exchange_balancepct_change
               + circ_supply_lastactive_1yplus_change + addresses_balance_1k_change + 
                 pctsupply_in_profit_change, data=data) 
summary(supplyall)

supply = lm(btc_returns ~ pctsupply_in_profit_change + hodl_1d_1w + 
              exchange_netflow + hodl_24h + hodl_1y_2y_change + liveliness_change, data = data)
summary(supply)
AIC(supply)
# Normality test (an alternative is Jarque-Bera)
shapiro.test(residuals(supply))
# Test d'hétéroscédasticité (Breusch-Pagan - bptest)
bptest(supply)
# Breusch Godfrey test for serial correlation
bgtest(supply, order = 2, data=data)
coeftest(supply, vcov = vcovHAC)
# supply controlled by security 
supplysec = lm(btc_returns~ pctsupply_in_profit_change + hodl_1d_1w + 
                 exchange_netflow + hodl_24h + hodl_1y_2y_change + liveliness_change
               + hashrate_change + fee_ratio_multiple_change, data=data)
summary(supplysec)
AIC(supplysec)
coeftest(supplysec, vcov = vcovHAC)
# Normality test (an alternative is Jarque-Bera)
shapiro.test(residuals(supplysec))
# Test d'hétéroscédasticité (Breusch-Pagan - bptest)
bptest(supplysec)
# Breusch Godfrey test for serial correlation
bgtest(supplysec, order = 2, data=data)
# supply controlled by security and macro
supplymacro = lm(btc_returns ~ pctsupply_in_profit_change + hodl_1d_1w + 
                   exchange_netflow + hodl_24h + hodl_1y_2y_change + liveliness_change
                 + hashrate_change + fee_ratio_multiple_change
                 + Gold_returns + m2_change + deficit_change, data=data)
summary(supplymacro)
AIC(supplymacro)
coeftest(supplymacro, vcov = vcovHAC)
# Normality test (an alternative is Jarque-Bera)
shapiro.test(residuals(supplymacro))
# Test d'hétéroscédasticité (Breusch-Pagan - bptest)
bptest(supplymacro)
# Breusch Godfrey test for serial correlation
bgtest(supplymacro, order = 2, data=data)

## SECURITY REGRESSION
security = lm(btc_returns ~ hashrate_change + fee_ratio_multiple_change, data=data)
summary(security)
AIC(security)
bptest(security)
coeftest(security, vcov = vcovHAC)
# Normality test (an alternative is Jarque-Bera)
shapiro.test(residuals(security))
# Test d'hétéroscédasticité (Breusch-Pagan - bptest)
bptest(security)
# Breusch Godfrey test for serial correlation
bgtest(security, order = 2, data=data)

## MACRO REGRESSION
macro = lm(btc_returns ~ Gold_returns + m2_change + deficit_change, data=data)
summary(macro)
AIC(macro)
coeftest(macro, vcov = vcovHAC)
# Normality test (an alternative is Jarque-Bera)
shapiro.test(residuals(macro))
# Test d'hétéroscédasticité (Breusch-Pagan - bptest)
bptest(macro)
# Breusch Godfrey test for serial correlation
bgtest(macro, order = 2, data=data)

### VAR Regression
# data
btc_returns <- ts(data$btc_returns, frequency = 365)
active_addresses_change <- ts(data$active_addresses_change, frequency = 365)
hodl_24h <- ts(data$hodl_24h, frequency = 365)
hodl_1d_1w <- ts(data$hodl_1d_1w, frequency = 365)
hodl_1w_1m <- ts(data$hodl_1w_1m, frequency = 365)
hodl_1m_3m <- ts(data$hodl_1m_3m, frequency = 365)
hodl_3m_6m_change <- ts(data$hodl_3m_6m_change, frequency = 365)
hodl_6m_12m_change <- ts(data$hodl_6m_12m_change, frequency = 365)
hodl_1y_2y_change <- ts(data$hodl_1y_2y_change, frequency = 365)
hodl_2y_3y_change <- ts(data$hodl_2y_3y_change, frequency = 365)
hodl_3y_5y_change <- ts(data$hodl_3y_5y_change, frequency = 365)
liveliness_change <- ts(data$liveliness_change, frequency = 365)
coin_days_destroyed <- ts(data$coin_days_destroyed, frequency = 365)
dormancy <- ts(data$dormancy, frequency = 365)
exchange_netflow <- ts(data$exchange_netflow, frequency = 365)
exchange_balancepct_change <- ts(data$exchange_balancepct_change, frequency = 365)
circ_supply_lastactive_1yplus_change <- ts(data$circ_supply_lastactive_1yplus_change, frequency = 365)
addresses_balance_1k_change <- ts(data$addresses_balance_1k_change, frequency = 365)
pctsupply_in_profit_change <- ts(data$pctsupply_in_profit_change, frequency = 365)
hashrate_change <- ts(data$hashrate_change, frequency = 365)
fee_ratio_multiple_change <- ts(data$fee_ratio_multiple_change, frequency = 365)
Gold_returns <- ts(data$Gold_returns, frequency = 365)
m2_change <- ts(data$m2_change, frequency = 365)
deficit_change <- ts(data$deficit_change, frequency = 365)

var_data <- cbind(btc_returns, active_addresses_change, hodl_24h, hodl_1d_1w, hodl_1w_1m, 
                  hodl_1m_3m, hodl_3m_6m_change, 
                  hodl_6m_12m_change, hodl_1y_2y_change, hodl_2y_3y_change, 
                  hodl_3y_5y_change, liveliness_change, coin_days_destroyed, 
                  dormancy, exchange_netflow, exchange_balancepct_change, circ_supply_lastactive_1yplus_change, 
                  addresses_balance_1k_change, pctsupply_in_profit_change,
                  hashrate_change, fee_ratio_multiple_change, Gold_returns, m2_change, deficit_change) 

#1 BEST BTC RETURNS MODEL
var_data <- cbind(btc_returns, active_addresses_change, pctsupply_in_profit_change)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model1 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model1)
# pctsupply_in_profit_change 0.04424,	Adjusted R-squared: 0.03599 
# active_addresses_change 0.4459,	Adjusted R-squared: 0.4411
# btc_returns 0.269,	Adjusted R-squared: 0.2626
# Model diagnostics
# autocorrelation
Serial1 <- serial.test(var_model1, lags.pt = 8, type = "BG")
Serial1
# heteroscedasticity
# ARCH effects are clustered volatility areas in a time series excessive volatility
# changing the variance of the residuals, far from our assumption of constant variance
Arch1 <- arch.test(var_model1, lags.multi = 8, multivariate.only = TRUE)
Arch1
# Normality of residuals
Norm1 <- normality.test(var_model1, multivariate.only = TRUE)
Norm1

## Granger causality
GrangerRRP<- causality(var_model1, cause = "btc_returns")
GrangerRRP
GrangerRRP<- causality(var_model1, cause = "active_addresses_change")
GrangerRRP
GrangerRRP<- causality(var_model1, cause = "pctsupply_in_profit_change")
GrangerRRP

## Impulse Response Functions
btc_irf <- irf(var_model1, impulse = "btc_returns", response = "btc_returns", n.ahead = 20, boot = TRUE)
plot(btc_irf, ylab = "btc_returns", main = "btc_returns shock to btc_returns")
activeadd_irf <- irf(var_model1, impulse = "btc_returns", response = "active_addresses_change", n.ahead = 20, boot = TRUE)
plot(activeadd_irf, ylab = "active_addresses_change", main = "btc_returns shock to active_addresses_change")
pctsupply_irf <- irf(var_model1, impulse = "btc_returns", response = "pctsupply_in_profit_change", n.ahead = 20, boot = TRUE)
plot(pctsupply_irf, ylab = "pctsupply_in_profit_change", main = "btc_returns shock to pctsupply_in_profit_change")
pctsupply_irf <- irf(var_model1, impulse = "pctsupply_in_profit_change", response = "btc_returns", n.ahead = 20, boot = TRUE)
plot(pctsupply_irf, ylab = "pctsupply_in_profit_change", main = "btc_returns shock to pctsupply_in_profit_change")

#2
var_data <- cbind(btc_returns, active_addresses_change, coin_days_destroyed)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model2 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model2)
# error: Error in solve.default(Sigma) : 
# system is computationally singular: reciprocal condition number = 5.31661e-18

#3
var_data <- cbind(btc_returns, active_addresses_change, exchange_netflow)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model3 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model3)
# exchange_netflow 0.0238,	Adjusted R-squared: 0.01537  
# active_addresses_change 0.4464,	Adjusted R-squared: 0.4416
# btc_returns 0.08789,	Adjusted R-squared: 0.08001

#4
var_data <- cbind(btc_returns, active_addresses_change, hodl_24h)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model4 <- VAR(var_data, p = 10, type = "const", season = NULL, exog = NULL) 
summary(var_model4)
# hodl_24h 0.6172,	Adjusted R-squared: 0.613   
# active_addresses_change 0.4572,	Adjusted R-squared: 0.4513
# btc_returns 0.09002,	Adjusted R-squared: 0.08016

#5 
var_data <- cbind(btc_returns, active_addresses_change, hodl_1d_1w)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model5 <- VAR(var_data, p = 10, type = "const", season = NULL, exog = NULL) 
summary(var_model5)
# hodl_1d_1w 0.9046,	Adjusted R-squared: 0.9036  
# active_addresses_change 0.4584,	Adjusted R-squared: 0.4526
# btc_returns 0.08734,	Adjusted R-squared: 0.07746

#6 
var_data <- cbind(btc_returns, active_addresses_change, hodl_1w_1m)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model6 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model6)
# hodl_1w_1m 0.9846,	Adjusted R-squared: 0.9845  
# active_addresses_change 0.4483,	Adjusted R-squared: 0.4436
# btc_returns 0.08068,	Adjusted R-squared: 0.07273 

#7 MEILLEUR MODÈLE OFFRE ET DEMANDE
var_data <- cbind(btc_returns, active_addresses_change, hodl_1m_3m)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model7 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model7)
# hodl_1m_3m 0.996,	Adjusted R-squared: 0.996  
# active_addresses_change 0.4487,	Adjusted R-squared: 0.444
# btc_returns 0.07987,	Adjusted R-squared: 0.07192
# Model diagnostics
# autocorrelation
Serial1 <- serial.test(var_model7, lags.pt = 8, type = "BG")
Serial1
# heteroscedasticity
# ARCH effects are clustered volatility areas in a time series excessive volatility
# changing the variance of the residuals, far from our assumption of constant variance
Arch1 <- arch.test(var_model7, lags.multi = 8, multivariate.only = TRUE)
Arch1
# Normality of residuals
Norm1 <- normality.test(var_model7, multivariate.only = TRUE)
Norm1

## Granger causality
GrangerRRP<- causality(var_model7, cause = "btc_returns")
GrangerRRP
GrangerRRP<- causality(var_model7, cause = "active_addresses_change")
GrangerRRP
GrangerRRP<- causality(var_model7, cause = "hodl_1m_3m")
GrangerRRP

## Impulse Response Functions
btc_irf <- irf(var_model1, impulse = "btc_returns", response = "btc_returns", n.ahead = 20, boot = TRUE)
plot(btc_irf, ylab = "btc_returns", main = "btc_returns shock to btc_returns")
activeadd_irf <- irf(var_model1, impulse = "btc_returns", response = "active_addresses_change", n.ahead = 20, boot = TRUE)
plot(activeadd_irf, ylab = "active_addresses_change", main = "btc_returns shock to active_addresses_change")
pctsupply_irf <- irf(var_model1, impulse = "btc_returns", response = "pctsupply_in_profit_change", n.ahead = 20, boot = TRUE)
plot(pctsupply_irf, ylab = "pctsupply_in_profit_change", main = "btc_returns shock to pctsupply_in_profit_change")
pctsupply_irf <- irf(var_model1, impulse = "pctsupply_in_profit_change", response = "btc_returns", n.ahead = 20, boot = TRUE)
plot(pctsupply_irf, ylab = "pctsupply_in_profit_change", main = "btc_returns shock to pctsupply_in_profit_change")


#8
var_data <- cbind(btc_returns, active_addresses_change, dormancy)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model8 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model8)
# dormancy 0.2043,	Adjusted R-squared: 0.1974  
# active_addresses_change 0.4451,	Adjusted R-squared: 0.4403
# btc_returns 0.081,	Adjusted R-squared: 0.07306

#9
var_data <- cbind(btc_returns, active_addresses_change, addresses_balance_1k_change)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model9 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model9)
# addresses_balance_1k_change 0.008167,	Adjusted R-squared: -0.0004018  
# active_addresses_change 0.4453,	Adjusted R-squared: 0.4405
# btc_returns 0.08005,	Adjusted R-squared: 0.0721

#10
var_data <- cbind(btc_returns, active_addresses_change, exchange_balancepct_change)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model10 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model10)
# exchange_balancepct_change 0.02256,	Adjusted R-squared: 0.01411  
# active_addresses_change 0.4454,	Adjusted R-squared: 0.4406
# btc_returns 0.08636,	Adjusted R-squared: 0.07847 

#11
var_data <- cbind(btc_returns, active_addresses_change, circ_supply_lastactive_1yplus_change)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model11 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model11)
# circ_supply_lastactive_1yplus_change 0.1346,	Adjusted R-squared: 0.1271 
# active_addresses_change 0.4452,	Adjusted R-squared: 0.4404
# btc_returns 0.08246,	Adjusted R-squared: 0.07453 

#12
var_data <- cbind(btc_returns, active_addresses_change, hodl_3m_6m_change)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model12 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model12)
# hodl_3m_6m_change 0.1151,	Adjusted R-squared: 0.1074 
# active_addresses_change 0.446,	Adjusted R-squared: 0.4412
# btc_returns 0.08101,	Adjusted R-squared: 0.07307

#13
var_data <- cbind(btc_returns, active_addresses_change, hodl_6m_12m_change)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model13 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model13)
# hodl_6m_12m_change 0.1422,	Adjusted R-squared: 0.1348
# active_addresses_change 0.4469,	Adjusted R-squared: 0.4421
# btc_returns 0.07828,	Adjusted R-squared: 0.07032

#14
var_data <- cbind(btc_returns, active_addresses_change, hodl_1y_2y_change)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model14 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model14)
# hodl_1y_2y_change  0.1189,	Adjusted R-squared: 0.1113
# active_addresses_change 0.4473,	Adjusted R-squared: 0.4425
# btc_returns 0.09433,	Adjusted R-squared: 0.08651

#15
var_data <- cbind(btc_returns, active_addresses_change, hodl_2y_3y_change)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model15 <- VAR(var_data, p = 10, type = "const", season = NULL, exog = NULL) 
summary(var_model15)
# hodl_2y_3y_change  0.07127,	Adjusted R-squared: 0.06121
# active_addresses_change 0.4466,	Adjusted R-squared: 0.4406 
# btc_returns 0.08559,	Adjusted R-squared: 0.07569

#16
var_data <- cbind(btc_returns, active_addresses_change, hodl_3y_5y_change)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model16 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model16)
# hodl_3y_5y_change  0.07568,	Adjusted R-squared: 0.06769
# active_addresses_change 0.4446,	Adjusted R-squared: 0.4398 
# btc_returns 0.07826,	Adjusted R-squared: 0.0703 

#17
var_data <- cbind(btc_returns, active_addresses_change, liveliness_change)                 
lagselect <- VARselect(var_data, type = "const")
lagselect$selection
var_model16 <- VAR(var_data, p = 8, type = "const", season = NULL, exog = NULL) 
summary(var_model16)
# liveliness_change  0.1952,	Adjusted R-squared: 0.1882
# active_addresses_change 0.4455,	Adjusted R-squared: 0.4407
# btc_returns 0.08278,	Adjusted R-squared: 0.07486 
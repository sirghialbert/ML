library(urca)
library(forecast)
library(vars)
library(tidyverse)
library(tsDyn)
library(dynlm)
library(aTSA)
library(readr)
library(tidyverse)
library(fGarch)
library(forecast)
library(fDMA)
library(lmtest)
library(FinTS)
library(tseries)
library(readxl)
library(lubridate)
library(nortsTest)
library(uroot)
 date<- read.csv("ITX.MC.csv")
plot(date$Close, type = "l")
inditex <- ts(date$Close, start = c(2014, 1), frequency = 12)
View(n)
summary(ur.df(inditex_original_train))


ggtsdisplay(inditex, lag.max = 100)

hegy.test(inditex_original_train)
ch.test(inditex_original_train)
ggsubseriesplot(inditex) +
  ylab("Retail index") +
  ggtitle("Seasonal subseries plot:  inditex stock") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

? ggsubseriesplot



in_none <- ur.df(inditex, type = 'none', selectlags = c("AIC"))
summary(in_none)
#drift
in_t <- ur.df(inditex, type = 'drift', selectlags = c("AIC"))
summary(in_t)
#trend
in_ct <- ur.df(inditex, type = 'trend', selectlags = c("AIC"))
summary(in_ct)

hegy.test(inditex)
ch.test(inditex)



inditex_diff <- diff(log(inditex))
plot(inditex_diff)
log_inditex <- log(inditex)
diff_log_inditex <- diff(log_inditex)
ggtsdisplay(diff_log_inditex)
ch.test(diff_log_inditex)
hegy.test(diff_log_inditex)
diff2_log_inditex <- diff(diff_log_inditex)
ggtsdisplay(diff2_log_inditex)

##Rentabilitatile sunt diff_log_inditex
split_percentage <- 0.75
split_index <- round(length(inditex) * split_percentage)

# Split the data into training and test sets
inditex_train <-
  ts(diff_log_inditex[1:split_index],
     start = c(2014, 01),
     frequency = 12)
inditex_test <-
  ts(diff_log_inditex[(split_index + 1):length(inditex)],
     start = c(2021, 11),
     frequency = 12)



diff_inditex_train <- diff(inditex_train)
diff_inditex_test <- diff(inditex_test)
ggtsdisplay(inditex_train)
ggtsdisplay(diff_inditex_train)

###--------STATIONARITATE---------------------------------
in_none <- ur.df(inditex_train, type = 'none', selectlags = c("AIC"))
summary(in_none)
in_none <-
  ur.df(diff_inditex_train,
        type = 'none',
        selectlags = c("AIC"))
summary(in_none)
#drift
in_t <- ur.df(inditex_train, type = 'drift', selectlags = c("AIC"))
in_t <-
  ur.df(diff_inditex_train,
        type = 'drift',
        selectlags = c("AIC"))
summary(in_t)

in_ct <- ur.df(inditex_train, type = 'trend', selectlags = c("AIC"))
in_ct <-
  ur.df(diff_inditex_train,
        type = 'trend',
        selectlags = c("AIC"))
summary(in_ct)
PP.test(inditex_train)
PP.test(diff_inditex_train)
summary(ur.kpss(inditex_train))
summary(ur.kpss(diff_inditex_train))
hegy.test(inditex_train)
hegy.test(diff_inditex_train)
ch.test(inditex_train)
ch.test(diff_inditex_train)

ggtsdisplay(inditex_train)
ggtsdisplay(diff_inditex_train)


####ARIMA PARAMETER TESTARE-------------------------


p_values <- 0:5
d_values <- 0:1
q_values <- 0:5
best_models <- list()
for (p in p_values) {
  for (d in d_values) {
    for (q in q_values) {
      if ((p  || q)) {
        model <- Arima(inditex_train, order = c(p, d, q))
        aic <- AIC(model)
        
        
        coeft <- coeftest(model)
        aic
        if ("intercept" %in% rownames(coeft)) {
          coeft <- coeft[rownames(coeft) != "intercept", , drop = FALSE]
        }
        
        if (anyNA(coeft)) {
          next
        }
        
        
        coef_significance <- coeft[, "Pr(>|z|)"] < 0.05
        
        
        if (all(coef_significance)) {
          if (length(best_models) < 5 ||
              aic < max(sapply(best_models, function(x)
                x$aic))) {
            current_model <- list(
              aic = aic,
              p = p,
              d = d,
              q = q
            )
            
            
            if (length(best_models) < 5) {
              best_models[[length(best_models) + 1]] <- current_model
            } else {
              worst_model_index <-
                which.max(sapply(best_models, function(x)
                  x$aic))
              best_models[[worst_model_index]] <- current_model
            }
          }
        }
      }
    }
  }
}

print("Cele mai bune 5 modele:\n")
for (i in 1:length(best_models)) {
  cat(
    "Model",
    i,
    "- Parametrii (p, d, q):",
    best_models[[i]]$p,
    best_models[[i]]$d,
    best_models[[i]]$q,
    "- AIC:",
    best_models[[i]]$aic,
    "\n"
  )
}







for (i in 1:length(best_models)) {
  p <- best_models[[i]]$p
  d <- best_models[[i]]$d
  q <- best_models[[i]]$q
  
  arima_model <- Arima(inditex_train, order = c(p, d, q))
  
  box_test_results <- sapply(1:12, function(lag) {
    box_test <-
      Box.test(residuals(arima_model), lag = lag, type = "Ljung-Box")
    box_test$p.value
  })
  jb_test_result <- jarque.bera.test(residuals(arima_model))$p.value
  
  criterii <- character()
  
  if (any(box_test_results <= 0.05)) {
    lag_not_met <- which(box_test_results <= 0.05)
    criterii <-
      c(criterii,
        paste(
          "Testul Box-Ljung nu a fost indeplinit pentru lag-urile:",
          paste(lag_not_met, collapse = ", ")
        ))
  }
  if (jb_test_result <= 0.01) {
    criterii <- c(criterii, "Testul Jarque-Bera nu a fost indeplinit.")
  }
  
  if (length(criterii) == 0) {
    cat("Model ARIMA(p =",
        p,
        ", d =",
        d,
        ", q =",
        q,
        ") respecta toate criteriile.\n")
  } else {
    cat("Model ARIMA(p =",
        p,
        ", d =",
        d,
        ", q =",
        q,
        ") NU respecta urmatoarele criterii:\n")
    cat(paste(criterii, collapse = "\n"), "\n")
  }
}

ggtsdisplay(inditex_train)
ggtsdisplay(diff_inditex_train)



plot(inditex_original_train_log)
best_fit <- Arima(inditex_original_train, order = c(4, 0, 4))
checkresiduals(best_fit)
jarque.bera.test(residuals(best_fit))
AIC(best_fit)
coeftest(best_fit)
checkresiduals(best_fit)
residuals_fit1 <- residuals(best_fit)
ggtsdisplay(residuals_fit1)

jarque.bera.test(residuals_fit1)













####ARIMA PARAMETER TESTARE-------------------------
predictions <- predict(best_fit, 32)

plot(inditex_test)
predictions$pred
lines(predictions$pred,col="red")



erori <- predictions$pred - inditex_test
erori<-erori[-length(erori)]
checkresiduals(erori)
jarque.bera.test(erori)
erori


###########################SARIMA----------------------------------------------------------------------
ggtsdisplay(inditex_train)
SARIMA_best_fit1 <-
  Arima(inditex_train,
        order = c(0, 0, 0),
        seasonal = c(1, 0, 1))
SARIMA_best_fit2 <-
  Arima(inditex_train,
        order = c(1, 1, 0),
        seasonal = c(1, 0, 1))
SARIMA_best_fit3 <-
  Arima(inditex_train,
        order = c(0, 1, 1),
        seasonal = c(1, 0, 1))
SARIMA_best_fit4 <-
  Arima(inditex_train,
        order = c(1, 1, 1),
        seasonal = c(1, 0, 1))


#SARIMA----------------------------------------------------------


ggtsdisplay(inditex_train)

SARIMA_grid_search <-
  function(data,
           p_values,
           d_values,
           q_values,
           P_values,
           D_values,
           Q_values,
           s) {
    best_models <- list()
    top_n <- 5
    
    for (p in p_values) {
      for (d in d_values) {
        for (q in q_values) {
          for (P in P_values) {
            for (D in D_values) {
              for (Q in Q_values) {
                tryCatch({
                  model <-
                    Arima(data,
                          order = c(p, d, q),
                          seasonal = list(order = c(P, D, Q)))
                  aic <- AIC(model)
                  coeft <- coeftest(model)
                  aic
                  if ("intercept" %in% rownames(coeft)) {
                    coeft <- coeft[rownames(coeft) != "intercept", , drop = FALSE]
                  }
                  
                  if (anyNA(coeft)) {
                    next
                  }
                  
                  
                  coef_significance <- coeft[, "Pr(>|z|)"] < 0.05
                  
                  if (length(best_models) < top_n) {
                    if (all(coef_significance))
                    {
                      best_models[[paste("Model", length(best_models) + 1)]] <-
                        list(
                          model = model,
                          AIC = aic,
                          p1 = p,
                          P1 = P,
                          d1 = d,
                          D1 = D,
                          q1 = q,
                          Q1 = q
                        )
                    }
                  } else {
                    if (all(coef_significance))
                    {
                      worst_model <-
                        which.max(unlist(lapply(best_models, function(x)
                          x$AIC)))
                      if (aic < best_models[[worst_model]]$AIC) {
                        best_models[[worst_model]] <-
                          list(
                            model = model,
                            AIC = aic,
                            p1 = p,
                            P1 = P,
                            d1 = d,
                            D1 = D,
                            q1 = q,
                            Q1 = Q
                          )
                      }
                    }
                  }
                }, error = function(e) {
                })
              }
            }
          }
        }
      }
    }
    
    return(best_models)
  }
autoplot(inditex_original_train)

best_models <-
  SARIMA_grid_search(log(inditex_original_train), 0:4, 1:2, 0:4, 0:1, 0:1, 0:1, s)



View(best_models)

for (i in 1:length(best_models)) {
  print(paste("Model", i))
  cat(
    best_models[[i]]$p1,
    best_models[[i]]$d1,
    best_models[[i]]$q1,
    best_models[[i]]$P1,
    best_models[[i]]$D1,
    best_models[[i]]$Q1,
    "\n"
  )
  print(AIC(best_models[[i]]$model))
  
}


for (i in 1:length(best_models)) {
  p <- best_models[[i]]$p1
  d <- best_models[[i]]$d1
  q <- best_models[[i]]$q1
  P <- best_models[[i]]$P1
  D <- best_models[[i]]$D1
  Q <- best_models[[i]]$Q1
  arima_model <-
    Arima(inditex_train,
          order = c(p, d, q),
          seasonal = c(P, D, Q))
  
  box_test_results <- sapply(1:12, function(lag) {
    box_test <-
      Box.test(residuals(arima_model), lag = lag, type = "Ljung-Box")
    box_test$p.value
  })
  jb_test_result <- jarque.bera.test(residuals(arima_model))$p.value
  
  criterii <- character()
  
  if (any(box_test_results <= 0.05)) {
    lag_not_met <- which(box_test_results <= 0.05)
    criterii <-
      c(criterii,
        paste(
          "Testul Box-Ljung nu a fost indeplinit pentru lag-urile:",
          paste(lag_not_met, collapse = ", ")
        ))
  }
  if (jb_test_result <= 0.01) {
    criterii <- c(criterii, "Testul Jarque-Bera nu a fost indeplinit.")
  }
  
  if (length(criterii) == 0) {
    cat(
      "Model SARIMA(p =",
      p,
      ", d =",
      d,
      ",q =",
      q,
      ",P = ",
      P,
      "D= ",
      D,
      "Q= ",
      Q,
      ") respecta toate criteriile.\n"
    )
  } else {
    cat(
      "Model SARIMA(p =",
      p,
      ", d =",
      d,
      ", q =",
      q,
      ", P = ",
      P,
      "D=",
      D,
      "Q= ",
      Q,
      ") NU respecta urmatoarele criterii:\n"
    )
    cat(paste(criterii, collapse = "\n"), "\n")
  }
}


SARIMA_best_fit <-
  Arima(
    inditex_train,
    order = c(0, 0, 1),
    seasonal = c(1, 0, 1),
    include.constant = TRUE
  )
coeftest(SARIMA_best_fit)
jarque.bera.test(residuals(SARIMA_best_fit))
AIC(SARIMA_best_fit)
checkresiduals(residuals(SARIMA_best_fit))

predictions_SARIMA<-predict(SARIMA_best_fit,32)
autoplot(predictions_SARIMA$pred)
plot(predictions_SARIMA$pred,
     main = "SARIMA(0,0,1)(1,0,1)[12] Forecast vs. Test Data",
     xlab = "Data",
     ylab = "Valoare")
lines(inditex_test, col = "red")
legend(
  "topleft",
  legend = c("Date previzionate", "Test Data"),
  col = c("black", "red"),
  lty = 1
)
predictions_SARIMA <- predict(SARIMA_best_fit, 32)
erori_SARIMA <- inditex_test - predictions_SARIMA$pred
checkresiduals(na.omit(erori_SARIMA))
erori_SARIMA
jarque.bera.test(na.omit(erori_SARIMA))
library(stats)


check_arch_effects <-
  function(residuals,
           max_lag = 24,
           significance_level = 0.05) {
    arch_test_results <- vector("numeric", length = max_lag)
    
    for (i in 1:max_lag) {
      arch_test_results[i] <-
        Box.test(residuals ^ 2, lag = i, type = "Ljung-Box")$p.value
    }
    
    significant_lags <- which(arch_test_results < significance_level)
    
    if (length(significant_lags) == 0) {
      cat("NICIUN EFECT ARCH LA LAG-URILE MAI MICI DE", max_lag, "\n")
    } else {
      cat("EFECTE ARCH LA LAG-URILE:", significant_lags, "\n")
    }
    
    return(
      list(
        lags_tested = 1:max_lag,
        p_values = arch_test_results,
        significant_lags = significant_lags
      )
    )
  }
check_arch_effects(residuals(SARIMA_best_fit))
coeftest(SARIMA_best_fit)
residuals <- SARIMA_best_fit$residuals


arch_results <-
  check_arch_effects(residuals, max_lag = 24, significance_level = 0.1)

















inditex_original_train <-
  ts(inditex[1:94], start = c(2014, 01), frequency = 12)
inditex_original_test <-
  ts(inditex[95:126], start = c(2021, 10), frequency = 12)
###SIMPLE EXPONENTIAL SMOOTHING


fit1 <- hw(inditex_original_train, seasonal = "additive") # HW aditiv
summary(fit1)
fit2 <-
  hw(inditex_original_train, seasonal = "multiplicative") # HW multiplicativ
summary(fit2)
autoplot(inditex_original_train) +
  autolayer(fit1, series = "HW additive forecasts", PI = FALSE) +
  autolayer(fit2, series = "HW multiplicative forecasts",
            PI = FALSE) +
  autolayer(inditex_original_test, PI = FALSE) +
  xlab("Year") +
  ylab("Dollars") +
  ggtitle("Inditex Stock") +
  guides(colour = guide_legend(title = "Forecast"))





####INDITEX ORIGINAL

ses_fit2 <- ses(inditex_original_train, 32)
summary(ses_fit2)
holt_fit2 <- holt(inditex_original_train, 32)
SummaryHolt<-summary(holt_fit2)
summaryHoltwinters<-summary(fit11)
SummaryHoltwinters2<-summary(fit22)

summaryHoltwinters

autoplot(inditex_original_train) +
  autolayer(holt_fit2, series = "Holt's method", PI = FALSE) +
  autolayer(ses_fit2, series = "Simple Exponential Smoothing", PI = FALSE) +
  autolayer(inditex_original_test, series = "Actual Test Data", PI = FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Pretul actiunilor INDITEX") +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_bw()

fit33 <- hw(inditex_original_train, seasonal = "additive") # HW aditiv
summary(fit33)
fit44 <-
  hw(inditex_original_train, seasonal = "multiplicative") # HW multiplicativ
summary(fit44)
autoplot(inditex_original_train) +
  autolayer(fit33, series = "HW additive forecasts", PI = FALSE) +
  autolayer(fit44, PI = FALSE, series = "HW multiplicative forecasts") +
  autolayer(inditex_original_test, PI = FALSE) +
  xlab("Year") +
  ylab("Stock price") +
  ggtitle("Holt Winters forecast") +
  guides(colour = guide_legend(title = "Forecast"))
summary(fit11)
fit33$model$aic
fit44$model$aic

ch.test(inditex_train)

hegy.test(inditex_train)


library(forecast)
##SARIMA SI ARIMA PE ORIGINALE--------------------------------------------------------------------------------------
inditex_original_train_log<-(log(inditex_original_train))
inditex_original_test_log<-(log(inditex_original_test))
Arima_original_fit<-Arima(inditex_original_train_log,order=c(4,1,4))

coeftest(Arima_original_fit)
Sarima_original_fit<-Arima(inditex_original_train_log,order=c(0,1,1),seasonal=c(1,0,1))
coeftest(Sarima_original_fit)

# Predicții cu modelul ARIMA
arima_predictions_log <- predict(Arima_original_fit, length(inditex_original_test))
residuals(Sarima_original_fit)%>%jarque.bera.test()
arima_predictions <- exp(arima_predictions_log$pred)
arima_predictions_erori<-inditex_original_test-arima_predictions
plot(arima_predictions_erori)
arima_predictions
# Predicții cu modelul SARIMA
sarima_predictions_log <- predict(Sarima_original_fit, length(inditex_original_test))$pred
sarima_predictions <- exp(sarima_predictions_log)






inditex_all <- c(inditex_original_train, inditex_original_test)
time_index <- 1:length(inditex_all)


data <- data.frame(
  Time = time_index,
  Actual = inditex_all,
  ARIMA_Predicted = c(rep(NA, length(inditex_original_train)), arima_predictions),
  SARIMA_Predicted = c(rep(NA, length(inditex_original_train)), sarima_predictions)
  
)

index(inditex)

p1 <- ggplot(data, aes(x = index(inditex))) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = ARIMA_Predicted, color = "ARIMA Predicted")) +
  labs(title = "ARIMA Model Predictions", y = "Values", x = "Date") +
  scale_color_manual(values = c("Actual" = "blue", "ARIMA Predicted" = "red")) +
  theme_minimal()

# Plot pentru modelul SARIMA
p2 <- ggplot(data, aes(x = index(inditex))) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = SARIMA_Predicted, color = "SARIMA Predicted")) +
  labs(title = "SARIMA Model Predictions", y = "Values", x = "Date") +
  scale_color_manual(values = c("Actual" = "blue", "SARIMA Predicted" = "green")) +
  theme_minimal()
ch.test(diff(inditex_train,12))


fit33 <- hw(inditex_original_train, seasonal = "additive")


autoplot(inditex_original_train) +
  autolayer(fit33, series = "HW additive forecasts", PI = FALSE) +
  autolayer(inditex_original_test, PI = FALSE) +
  xlab("Year") +
  ylab("Stock price") +
  ggtitle("Holt Winters forecast") +
  guides(colour = guide_legend(title = "Forecast"))

coeftest(Arima_original_fit)
print(p1)
print(p2)















library(seasonal)
library(ggplot2)
library(dplyr)

######MULTIVARIATA----------------------------------------------

inditex <- ts(date$Close, start = c(2014, 1), frequency = 12)
autoplot(inditex)
Zara_search<-read_csv("Zara_Search.csv")
Zara_search<-Zara_search$Searches%>% ts(start = c(2014,01),frequency=12)
Zara_search%>%head()
IPC_search
IPC_search<-read_csv("Hoodie_Search.csv")
IPC_search<-IPC_search$Search%>% ts(start = c(2014,01),frequency=12)
IPC_search%>%tail()
ggtsdisplay(Zara_search)
autoplot(IPC_search)
inditex%>%head()
autoplot(Zara_search)
length(IPC_search)
dataset<-cbind(inditex[1:124],IPC_search,Zara_search[1:124])
install.packages("aod")
library(aod)
?ARDL
library(ARDL)
?ARDL::ardl
ardlmodel<-ardl(stock~IPC+Search,data=adjusted_dataset,order=c(2,3,2))
ardlmodel
?ardl

colnames(dataset)<-c("stock","IPC","zara")
dataset%>% head()
autoplot(dataset)
adjusted_dataset<-dataset
colnames(adjusted_dataset)<-c("stock","IPC","Search")

adjusted_dataset
# Aplicarea TRAMO-SEATS pentru desezonalizare
stock_adj <- seas(adjusted_dataset[,"stock"], x11 = "")
IPC_adj <- seas(adjusted_dataset[,"IPC"], x11 = "")
zara_adj <- seas(adjusted_dataset[,"Search"], x11 = "")
stock_adj <- final(stock_adj)
IPC_adj <- final(IPC_adj)
zara_adj <- final(zara_adj)
adjusted_dataset<-cbind(stock_adj,IPC_adj,zara_adj)
adjusted_dataset<-log(adjusted_dataset)
colnames(adjusted_dataset)<-c("stock","IPC","Search")
autoplot(adjusted_dataset)
adjusted_dataset_diff<-diff(adjusted_dataset)


summary(ur.df((IPC_adj%>%log())))

summary(ur.df((zara_adj)%>% log()))



autoplot(adjusted_dataset_diff)
summary(ur.df((adjusted_dataset_diff[,1])))
summary(ur.df((adjusted_dataset_diff[,1]),"trend"))
summary(ur.df((adjusted_dataset_diff[,1]),"drift"))

summary(ur.df((adjusted_dataset_diff[,2])))
summary(ur.df((adjusted_dataset_diff[,2]),"trend"))
summary(ur.df((adjusted_dataset_diff[,2]),"drift"))
ur.kpss(adjusted_dataset_diff[,2])
pp.test(adjusted_dataset_diff[,2])
summary(ur.df(adjusted_dataset_diff[,3]))
summary(ur.df(adjusted_dataset_diff[,3],"trend"))
summary(ur.df(adjusted_dataset_diff[,3],"drift"))
ur.kpss((adjusted_dataset_diff[,1]))
ur.kpss(ts(adjusted_dataset_diff[,3]/deviations_data[,1],start=c(2014,01),frequency=12))
sdd<-sd(adjusted_dataset_diff[,3])
?log
pp.test(adjusted_dataset_diff[,3])




calculate_deviation <- function(series) {
  mean_series <- mean(series)
  deviations <- series - mean_series
  return(deviations)
}


deviations_data <- as.data.frame(lapply(adjusted_dataset_diff[,3], calculate_deviation))
deviations_data







selectvar<-VARselect(adjusted_dataset_diff,lag.max = 12)
selectvar
selectvar$selection



autoplot(adjusted_dataset)
coint.test(adjusted_dataset[,2],adjusted_dataset[,3])
?ca.jo
ctest <- ca.jo(adjusted_dataset, type = 'trace', ecdet = 'const',K=5)
summary(ctest)




model1 <- VAR(adjusted_dataset_diff, p = 5, type = 'const', season = NULL, exog = NULL)
summary(model1)


serial_test <- serial.test(model1, lags.pt = 16, type = "PT.asymptotic")
print(serial_test$serial)
model1
autoplot((inditex))
normality_test <- normality.test(model1)
print(normality_test$jb.mul$JB)
checkresiduals(residuals(model1)[,1])
jarque.bera.test(residuals(model1)[,1])
jarque.bera.test(residuals(model1)[,2])
jarque.bera.test(residuals(model1)[,3])

residuals <- resid(model1)
par(mfrow = c(3, 1))
for (i in 1:ncol(residuals)) {
  ts.plot(residuals[, i], main = colnames(residuals)[i], ylab = "Residuals")
}


par(mfrow = c(3, 1))
for (i in 1:ncol(residuals)) {
  acf(residuals[, i], main = paste("ACF of Residuals -", colnames(residuals)[i]))
}
?VAR


par(mfrow = c(1, 1))

Stability1 <- stability(model1,type = 'OLS-CUSUM')
plot(Stability1) 


granger1<-causality(model1, cause="stock")
granger1
granger2<-causality(model1,cause = "IPC")
granger2
granger3<-causality(model1,cause="Search")
granger3


autoplot(inditex)
autoplot(diff(inditex))
n.ahead <- 20
ci <- 0.90
par(mfrow=c(3,2))
ggtsdisplay (dataset[,2])
stock_IPCirf <- irf(model1, impulse = 'stock', response = 'IPC', n.ahead = n.ahead, boot = TRUE, ci = ci)
plot(stock_IPCirf, ylab = 'IPC value', main = 'Shock from stock to IPC')


stock_Searchirf <- irf(model1, impulse = 'stock', response = 'Search', n.ahead = n.ahead, boot = TRUE, ci = ci)
plot(stock_Searchirf, ylab = 'Search value', main = 'Shock from stock to Search')


IPC_stockirf <- irf(model1, impulse = 'IPC', response = 'stock', n.ahead = n.ahead, boot = TRUE, ci = ci)
plot(IPC_stockirf, ylab = 'Stock value', main = 'Shock from IPC to stock')


IPC_Searchirf <- irf(model1, impulse = 'IPC', response = 'Search', n.ahead = n.ahead, boot = TRUE, ci = ci)
plot(IPC_Searchirf, ylab = 'Search value', main = 'Shock from IPC to Search')


Search_stockirf <- irf(model1, impulse = 'Search', response = 'stock', n.ahead = n.ahead, boot = TRUE, ci = ci)
plot(Search_stockirf, ylab = 'Stock value', main = 'Shock from Search to stock')


Search_IPCirf <- irf(model1, impulse = 'Search', response = 'IPC', n.ahead = n.ahead, boot = TRUE, ci = ci)
plot(Search_IPCirf, ylab = 'IPC value', main = 'Shock from Search to IPC')






summary(model1)

FEVD <- fevd(model1, n.ahead = 10)
plot(FEVD,col=c("skyblue","pink","purple1")) 
(FEVD)
model<-VAR()
forecast <- predict(model1, n.ahead = 12, ci = 0.90) 
plot(forecast, name = 'GDP')
plot(forecast, name = 'Unemployment')

fanchart(forecast, names='Search',colors = c("darksalmon","deeppink"),col.y = "dodgerblue")
fanchart(forecast, names='IPC',colors = c("darksalmon","deeppink"),col.y = "dodgerblue")
fanchart(forecast, names="Stock",colors = c("darksalmon","deeppink"),col.y = "dodgerblue")
?fanchart






var_model_diff <- VAR(adjusted_dataset_diff, p = 5, type = 'const',, season = NULL, exog = NULL)
summary(model1)

summary(var_model_diff)

invert_diff <- function(original_series, differenced_series) {
  cumsum(c(original_series[length(original_series)], differenced_series))
}

n_forecast <- 12  
forecasts_diff <- predict(var_model_diff, n.ahead = n_forecast)
forecasts_diff$fcst$stock

adjusted_dataset%>%head()
forecast_stock <- invert_diff(adjusted_dataset[,1], forecasts_diff$fcst$stock[, "fcst"])
forecast_stock
forecast_IPC <- invert_diff(adjusted_dataset[,2], forecasts_diff$fcst$IPC[, "fcst"])
forecast_Search <- invert_diff(adjusted_dataset[,3], forecasts_diff$fcst$Search[, "fcst"])


forecasts_original_scale <- data.frame(
  time = seq(nrow(adjusted_dataset) + 1, nrow(adjusted_dataset) + n_forecast+1),
  stock = forecast_stock,
  IPC = forecast_IPC,
  Search = forecast_Search
)


adjusted_dataset_df <- as.data.frame(adjusted_dataset)

adjusted_dataset_df$time <- 1:nrow(adjusted_dataset_df)


combined_data <- adjusted_dataset_df %>%
  pivot_longer(cols = -time, names_to = "variable", values_to = "value") %>%
  bind_rows(forecasts_original_scale %>%
              pivot_longer(cols = -time, names_to = "variable", values_to = "value"))



ggplot(combined_data, aes(x = time, y = value, color = variable)) +
  geom_line(size = 1) +  
  geom_point(data = forecasts_original_scale, aes(x = time, y = stock), color = "blue", size = 2, shape = 16) +  # Puncte mai mari și forme pline
  geom_point(data = forecasts_original_scale, aes(x = time, y = IPC), color = "red", size = 2, shape = 16) +
  geom_point(data = forecasts_original_scale, aes(x = time, y = Search), color = "purple", size = 2, shape = 16) +
  labs(title = "Original Series and Forecasts", 
       x = "Time", 
       y = "Value", 
       color = "Variables") +  
  theme_minimal(base_size = 15) + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_text(face = "bold"),  
        legend.title = element_text(face = "bold"))  



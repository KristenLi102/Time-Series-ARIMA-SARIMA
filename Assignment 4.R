library(dplyr)
library(data.table)
library(ggplot2)
library(tseries)
library(forecast)

# This is 5 years of store-item sales data for 50 different items at 10 different stores. 
store <- fread("~/Downloads/store_demand.csv")
head(store)
store$date <- as.Date(store$date, "%m/%d/%Y")
range(store$date)
rownames(store) <- store$Date

ggplot(store, aes(date, sales)) + geom_line() + 
  scale_x_date('time')+ ylab("Daily Sales") + xlab("") +
  facet_grid(store$store)

store1_item36 <- store %>% filter(store==1&item==36)
store1_2items <- rbind(store1_item1,store1_item36)

range(store1_item36$date)
ggplot(store1_2items, aes(date, sales)) + geom_line() + 
  facet_grid(store1_2items$item) +
  ylab("Daily Sales") + xlab("") 

store7_item36 <- store %>% filter(store==7&item==36)
item36_2stores <- rbind(store1_item36,store7_item36)

ggplot(item36_2stores, aes(date, sales)) + geom_line() + 
  facet_grid(item36_2stores$store) +
  ylab("Sales") + xlab("Date") 

##### model for store 1 item 1 #####
store1_item1 <- store %>% filter(store==1&item==1)
rownames(store1_item1) <- store1_item1[,"date"]
store1_item1 <- ts(store1_item1)

cbind("Sales" = store1_item1[, "sales"],
      "Monthly log sales" = log(store1_item1[, "sales"]),
      "Annual change in log sales" = diff(store1_item1[, "sales"],12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("store 1 item 1 sales")


plot(diff(store1_item1), main='Differenced Log-transorm of sales', ylab='', col='brown', lwd=3)

ggplot(store1_item1[,c("date", "sales")], aes(date, sales)) + geom_line() + ylab("Sales") + xlab("Day")

# ACF plots
acf(store1_item1$sales)
pacf(store1_item1$sales)

acf(diff(diff(store1_item1$sales)))
pacf(diff(diff(store1_item1$sales)))

# ARIMA 
fit <- auto.arima(store1_item1[,"sales"], seasonal=FALSE)
summary(fit)
fit2 <- auto.arima(store1_item1[,c("sales")], seasonal=FALSE,stepwise=FALSE, approximation=FALSE)
summary(fit2)
fit3 <- Arima(store1_item1[,c("sales")], order=c(7,1,7))
summary(fit3)

AIC( arima( store1_item1[,c("sales")], order=c(6,1,0) ) ) #AIC =  [1] 11209.36
AIC( arima( store1_item1[,c("sales")], order=c(6,1,1) ) ) #AIC =  [1] 11200.24
AIC( arima( store1_item1[,c("sales")], order=c(6,1,2) ) ) #AIC =  [1] 11199.85
AIC( arima( store1_item1[,c("sales")], order=c(6,1,5) ) ) #AIC =  [1] 10973.21
AIC( arima( store1_item1[,c("sales")], order=c(7,1,0) ) ) #AIC =  [1] 11202.3
AIC( arima( store1_item1[,c("sales")], order=c(7,1,1) ) ) #AIC =  [1] 11163.98
AIC( arima( store1_item1[,c("sales")], order=c(7,1,2) ) ) #AIC =  [1] 11165.98
AIC( arima( store1_item1[,c("sales")], order=c(7,1,3) ) ) #AIC =  [1] 11167.01
AIC( arima( store1_item1[,c("sales")], order=c(7,1,6) ) ) #AIC =  [1] 10935.74
AIC( arima( store1_item1[,c("sales")], order=c(7,1,7) ) ) #AIC =  [1] 10847.98

sum(arima( store1_item1[,c("sales")], order=c(6,1,5) )$residuals^2)
sum(fit$residuals^2)
Box.test(fit$residuals, lag=log(length(fit$residuals)))
Box.test(fit3$residuals, lag=log(length(fit3$residuals)))

tsdisplay(residuals(fit), lag.max=20, main='Seasonal Model Residuals')
checkresiduals(fit)
tsdisplay(residuals(fit3), lag.max=20, main='Seasonal Model Residuals')
checkresiduals(fit3)

pred = forecast(fit3)
plot(forecast(fit3))

# SARIMA Model
d=1
DD=1
per=7

for(p in 1:3){
  for(q in 1:3){
    for(i in 1:3){
      for(j in 1:3){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=store1_item1[,c("sales")], order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

## Final model
sarima <-arima(store1_item1[,c("sales")], order = c(0,1,1), seasonal = list(order = c(1,1,1), period = 7))
summary(sarima)
z.test(sarima)

checkresiduals(sarima)
Box.test(sarima$residuals, lag=log(length(sarima2$residuals)))
pred = forecast(sarima)
plot(forecast(sarima, h=14))

sarima2 <-arima(store1_item1[,c("sales")], order = c(6,1,1), seasonal = list(order = c(1,1,1), period = 7))
summary(sarima2)
tsdisplay(residuals(sarima2), lag.max=20, main='Seasonal Model Residuals')
checkresiduals(sarima2)
Box.test(sarima2$residuals, lag=log(length(sarima2$residuals)))

pred = forecast(sarima2)
plot(forecast(sarima2, h=70))

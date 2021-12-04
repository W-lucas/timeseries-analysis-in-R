library(fpp3)
library(forecast)
library(readabs)
options(warn= -1)
# Question one
set.seed(12345678)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))
#Converting the data to time series object
aus_ret1 <- tsibble(myseries$Turnover,myseries$Month)
# Plot of the time series
autoplot(aus_ret1) +
  labs(title = "Australia retail turnover",
       subtitle = "Time series plot",
       y = "Turnover")
# Exploring the series
gg_season(aus_ret1)
gg_subseries(aus_ret1)
gg_lag(aus_ret1)
#ACF before transformation
ACF(aus_ret1) %>% autoplot()
# Differentiating time series
m <-aus_ret1 %>%
  mutate(diff_turn = difference(difference(aus_ret1$`<dbl>`,12)))
#After transformation of the series
ACF(m) %>% autoplot()
PACF(m) %>% autoplot()
# Question b
# Calculating the seasonal arima model
fit<- Arima(myseries$Turnover,order=c(1,2,6),seasonal = c(0,2,6))
fit
# Checking residuals
checkresiduals(fit,lag=50)
# Forecast of the model
fit %>% forecast(h=12) %>% autoplot()

# Question c
#Downloading the data
retail <- read_abs("8501.0", tables = 11)
#  renaming the variables and assigning the new values
astretail <- retail %>%
  mutate(Month = yearmonth(date)) %>%
  rename(Turnover = value, `Series ID` = series_id) %>%
  select(Month, `Series ID`, series, Turnover)
astretail
# Extracting variables from state and industry
astretail2 <- astretail %>%
  separate(series, c("Category", "State", "Industry"), 
           sep = ";", extra = "drop") %>%
  mutate(
    State = trimws(State),
    Industry = trimws(Industry),
  ) %>%
  select(-Category) %>%
  filter(
    Industry  != "Total (Industry)",
    State != "Total (State)"
  )
# We convert the data into tsibble variable
astretail3 <- astretail2 %>%
  filter(State=="Northern Territory", Industry=="Clothing, footwear and personal accessory retailing") %>%
  as_tsibble(index = Month, key = c(State, Industry)) %>%
  filter(!is.na(Turnover))
astretail3
# The additional rows are due to extended observations to may 2021

# Question two
#Aus arrivals
# question a
 aus_arrival <-  aus_arrivals %>%
           filter(Origin=="US")
 # Time plot
 #Convert the data to tsibble object
 aus_arrival1 <- tsibble(aus_arrival)
 autoplot(aus_arrival1) +
   labs(title = "US arrivals",
        subtitle = "Time series plot",
        y = "Arrivals")
 # Question b
 # Differencing to obtain stationary data.
 ausdiff <- difference(aus_arrival1$Arrivals,4)
 ausdiff
 #Question c 
 #Plotting acf plot
 ggAcf(ausdiff)
 #Question d
 #Plotting pacf of differenced data
 ggPacf(ausdiff)
# model selection
 # I suggest a model of ar(1) and MA(4)
 y <- Arima(aus_arrival1$Arrivals,order=c(1,0,0))# Ar(1)
 y
 # model with MA(4)
 x <- Arima(aus_arrival1$Arrivals,order=c(0,0,4))# MA(4)
 x
 # ARIMA
 fit1<- Arima(aus_arrival1$Arrivals,order=c(1,1,0),seasonal = c(0,1,4))
 fit1
 # Checking residuals
 checkresiduals(fit1,lag=5)
 #Question g
 #Weiting the model in terms of backshift operator
 fit3 <- Arima(aus_arrival1$Arrivals,order=c(1,1,4),seasonal = c(1,1,4))
 fit3
 autoplot(fit3)
 # Without shift operator
 fit4 <- Arima(aus_arrival1$Arrivals,order=c(1,0,4),seasonal = c(1,0,4))
 fit4
 autoplot(fit4)
 
 # Question three
 #Question a
 #Reading data in R
 gdp <- global_economy %>%
        filter(Code=="USA")%>%
        select(GDP,Year)
        
 #finding the best box-cox  transformation of the data
 # extracting year and gdp from the dataset
gdp1 <- tsibble(gdp,index = "Year")
gdp1
autoplot(gdp1)
(lambda <- BoxCox.lambda(gdp$GDP))
 box <- BoxCox(gdp1$GDP,lambda)
 # Question b
 fitt <- Arima(box)
 fitt
 #Checking residuals
 checkresiduals(fitt)
 
 #Question c
 #Trying other models like AR and MA
 fitt3 <- ar(box)
 fitt3
 #Checking residuals
 checkresiduals(fitt3)
 # Forecast of the model
 fitt3 %>% forecast(h=12) %>% autoplot()
 
 #Using ETS
 #Question f
 fit_ets <- gdp %>% model(ETS(GDP))
 report(fit_ets)
 #Residuals
 fit_ets %>%
   gg_tsresiduals(lag_max = 16)
 
 #Forecasts of ETS
 fit_ets %>% forecast(h=12) %>% autoplot()
 #Determining accuracy of the model
 gdp %>%
   stretch_tsibble(.init = 10) %>%
   model(
     ETS(GDP),
     ARIMA(GDP)
   ) %>%
   forecast(h = 12) %>%
   accuracy(gdp) %>%
   select(.model, RMSE:MAPE)
 
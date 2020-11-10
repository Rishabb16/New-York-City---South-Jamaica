#library(odbc)
#library(DBI)
library(tidyverse)
library(lubridate)
#load("~/Documents/Boston University/AD571/R NYC Datasets.RData")
#install.packages("factoextra")
library(factoextra)
#install.packages("forecast")
library(forecast)
#install.packages("fpp2")
library(fpp2)
options(scipen = 999)
#-----Analysis-------

str(Borough)
View(Neighborhood)
view(Building_Class)
str(Nyc)
Borough <- data.frame("BOROUGH_ID" = c("1", "2", "3", "4", "5"),
                      "BOROUGH_NAME" = c("MANHATTAN","BRONX","BROOKLYN",
                                         "QUEENS","STATEN ISLAND"))
head(Historical)
head(Building_Class, 20)
#Joining all the tables togehter
Nyc <- Historical %>%
  left_join(Building_Class, by=c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID")) %>%
  left_join(Neighborhood, by=c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID")) %>%
  left_join(Borough, by=c("BOROUGH_ID" = "BOROUGH_ID")) %>%
  mutate(SALE_YEAR = year(SALE_DATE), QUARTER = quarter(SALE_DATE)) %>%
  select(SALE_PRICE, SALE_YEAR, TYPE, NEIGHBORHOOD_NAME, RESIDENTIAL_UNITS, 
         GROSS_SQUARE_FEET, BOROUGH_NAME, NEIGHBORHOOD_ID, COMMERCIAL_UNITS, 
         YEAR_BUILT, QUARTER, BUILDING_CLASS_FINAL_ROLL, SALE_DATE, ADDRESS)

#View(Nyc)

######### Task 1 #########
Nycts <- Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "SO. JAMAICA-BAISLEY PARK", SALE_PRICE >= 10000, 
         !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019)) %>%
  mutate(Time = SALE_YEAR*4 + QUARTER - 2009*4) %>%
  group_by(Time) %>%
  summarise(TotalSales = sum(SALE_PRICE))

#Convert into a time series
TS <- ts(Nycts$TotalSales, start = c(2009, 1), frequency = 4)
plot(TS, main = "Time Series of Sales", ylab = "Sale Amount")
#Check for individual seasonality by removing trends
Diff <- diff(TS)
autoplot(Diff, main = "Seasonility check")
Season_check <- ggseasonplot(Diff) +
  ggtitle("Checking for seasonility") + 
  ylab("Change in total sales per quarter")
#Plotting each individual year on a line to see how similar the movements of 
#quarters are. 


#ETS model using automatic settings.
ets_TS <- ets(TS, model = "ZZZ")
summary(ets_TS)
Forecast_TS <- forecast(ets_TS, 8)
plot(Forecast_TS)
plot(Forecast_TS, include = 12)

checkresiduals(Forecast_TS) #everything is within the 95% confidence intervals
#AIC = 1625 
?ets
#ARIMA model
arima_TS <- auto.arima(TS, d=1, D=1, approximation = FALSE, stepwise = FALSE)
summary(arima_TS)
checkresiduals(arima_TS)
Forecast_TS1 <- data.frame(forecast(arima_TS, 8))
view(Forecast_TS1)
plot(Forecast_TS1)
plot(Forecast_TS, include = 20)

#AIC 1420

###### Task 2 #######

ggplot(Nycts) + geom_point(mapping = aes(x = Time, y = TotalSales)) +
  geom_smooth(method = "lm", mapping = aes(x = Time, y = TotalSales)) +
  labs(title = "Total Sales over Time") 

Nycts_reg <- lm(data = Nycts, formula = TotalSales ~ Time)
summary(Nycts_reg)
Nycts_pred <- data.frame(Time = c(44, 45, 46, 47, 48, 49, 50, 51), 
                         TotalSales = c(0, 0, 0, 0, 0, 0 , 0, 0),
                         Quarters = c("Q1", "Q2", "Q3", "Q4"))
predict.lm(Nycts_reg, Nycts_pred, interval = "confidence")


Nycts_q <- cbind(Nycts , c("Q1", "Q2", "Q3", "Q4"))
names(Nycts_q)[3] <- "Quarters"


Nycts_reg1 <- lm(data = Nycts_q, formula = TotalSales ~ .)
summary(Nycts_reg1)
Nycts_pred1 <- data.frame(t = c(44, 45, 46, 47, 48, 49, 50, 51), 
                         TotalSales = c(0, 0, 0, 0, 0, 0 , 0, 0),
                         Quarters = c("Q1", "Q2", "Q3", "Q4"))
predict.lm(Nycts_reg, Nycts_pred, interval = "confidence")


###### Task 3 #######

Nyc_reg <-  Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "SO. JAMAICA-BAISLEY PARK", SALE_PRICE >= 10000, 
         !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019), TYPE == "RESIDENTIAL") %>%
  select(SALE_DATE, YEAR_BUILT, SALE_PRICE, GROSS_SQUARE_FEET, BUILDING_CLASS_FINAL_ROLL, RESIDENTIAL_UNITS)

Nyc_reg$SALE_DATE <- ymd(Nyc_reg$SALE_DATE)

Nyc_mlm<- lm(data = Nyc_reg, SALE_PRICE ~ .)
summary(Nyc_mlm)

Nyc_bargain <-  Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "SO. JAMAICA-BAISLEY PARK", SALE_PRICE >= 10000, 
         !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019), TYPE == "RESIDENTIAL") %>%
  mutate(Residuals = Nyc_mlm$residuals) %>%
  arrange(desc(Residuals)) %>%
  select(SALE_DATE, YEAR_BUILT, SALE_PRICE, GROSS_SQUARE_FEET, 
         BUILDING_CLASS_FINAL_ROLL, RESIDENTIAL_UNITS, ADDRESS, Residuals) %>%
  group_by(BUILDING_CLASS_FINAL_ROLL)
View(Nyc_bargain)

undervalued <- filter(.data = Nyc_bargain, Residuals <= 0) %>%
  summarise(Undervalued_properties = n()) %>%
  arrange(Undervalued_properties)

overvalued <- filter(.data = Nyc_bargain, Residuals > 0) %>%
  summarise(Undervalued_properties = n()) %>%
  arrange(Undervalued_properties)

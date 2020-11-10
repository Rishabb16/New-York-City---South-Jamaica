library(odbc)
library(DBI)
library(tidyverse) 
library(lubridate)
library(gapminder)
options(scipen = 999)

str(Borough)
View(Neighborhood)
str(Nyc)
Borough <- data.frame("BOROUGH_ID" = c("1", "2", "3", "4", "5"), "BOROUGH_NAME" = c("MANHATTAN","BRONX","BROOKLYN","QUEENS","STATEN ISLAND"))
head(Historical)
head(Building_Class, 20)
#Joining all the tables togehter
Nyc <- Historical %>%
  left_join(Building_Class, by=c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID")) %>%
  left_join(Neighborhood, by=c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID")) %>%
  left_join(Borough, by=c("BOROUGH_ID" = "BOROUGH_ID")) %>%
  select(SALE_PRICE, SALE_DATE, TYPE, NEIGHBORHOOD_NAME, RESIDENTIAL_UNITS, GROSS_SQUARE_FEET, BOROUGH_NAME)

Nyc2 <- Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "SO. JAMAICA-BAISLEY PARK", !RESIDENTIAL_UNITS == 0) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  group_by(SALE_YEAR)
#view(Nyc2)

YearlySales <- data.frame(summarise(Nyc2, SALE_TOTAL=sum(SALE_PRICE), TOTAL_HOUSES_SOLD = sum(RESIDENTIAL_UNITS)) %>%
                            mutate(AverageYearlySales = SALE_TOTAL/TOTAL_HOUSES_SOLD)) 
#view(YearlySales)

AvgSquareFootage <- summarise(Nyc2, SALE_TOTAL=sum(SALE_PRICE), Total_Square_Footage = sum(GROSS_SQUARE_FEET)) %>%
  mutate(AverageSquareFootage = SALE_TOTAL/Total_Square_Footage)
view(AvgSquareFootage)

forecast_sqft <- lm(AverageSquareFootage ~ SALE_YEAR, data = AvgSquareFootage)
Sqft_pred <- data.frame(SALE_YEAR = c(2020, 2021), 
                        TotalSales = c(0, 0),
                        Quarters = c("Y1", "Y2"))
predict.lm(forecast_sqft, Sqft_pred, interval = "confidence")



#Taking out Sale Price 0 and Square feet = 0
Nyc3 <- Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "SO. JAMAICA-BAISLEY PARK", !SALE_PRICE == 0, !GROSS_SQUARE_FEET == 0, !RESIDENTIAL_UNITS == 0) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  group_by(SALE_YEAR)

YearlySales2 <- data.frame(summarise(Nyc3, SALE_TOTAL=sum(SALE_PRICE), TOTAL_HOUSES_SOLD = sum(RESIDENTIAL_UNITS)) %>%
                             mutate(AverageYearlySales = SALE_TOTAL/TOTAL_HOUSES_SOLD))

Presentation_YS <- data.frame(summarise(Nyc3, SALE_TOTAL=sum(SALE_PRICE), TOTAL_HOUSES_SOLD = sum(RESIDENTIAL_UNITS)) %>%
                                mutate(AverageYearlySales = SALE_TOTAL))

ggplot() + geom_line(data = Presentation_YS, aes(x = SALE_YEAR, y = AverageYearlySales), color = "blue") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "Total Sales Volume", x = "Years", y = "Total Sales Volume") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


AvgSquareFootage2 <- summarise(Nyc3, SALE_TOTAL=sum(SALE_PRICE), Total_Square_Footage = sum(GROSS_SQUARE_FEET)) %>%
  mutate(AverageSquareFootage = SALE_TOTAL/Total_Square_Footage)

#Plots
plot1 <- ggplot() + geom_line(data = YearlySales, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "red") +
  geom_line(data = YearlySales2, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "blue")  

Presentation1 <- ggplot()  + 
  geom_line(data = YearlySales2, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "blue") + 
  labs(title = "Graph of Average Yearly Sales", x = "time", y = "Average Yearly Sales") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Presentation2 <- ggplot() +  
  geom_line(data = AvgSquareFootage2, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "blue") +
  labs(title = "Graph of Average Price Per Square Foot", x = "time", y = "Average Price per square foot") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


plot2 <- ggplot() + geom_line(data = AvgSquareFootage, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "red") +
  geom_line(data = AvgSquareFootage2, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "blue") 

#-------St.Albans----------
Nyc4 <- Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "ST. ALBANS", !RESIDENTIAL_UNITS == 0) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  mutate(QTY = 1) %>%
  group_by(SALE_YEAR)
#view(Nyc4)

YearlySales3 <- data.frame(summarise(Nyc4, SALE_TOTAL=sum(SALE_PRICE), TOTAL_HOUSES_SOLD = sum(RESIDENTIAL_UNITS)) %>%
                             mutate(AverageYearlySales = SALE_TOTAL/TOTAL_HOUSES_SOLD)) 

AvgSquareFootage3 <- summarise(Nyc4, SALE_TOTAL=sum(SALE_PRICE), Total_Square_Footage = sum(GROSS_SQUARE_FEET)) %>%
  mutate(AverageSquareFootage = SALE_TOTAL/Total_Square_Footage)

#Cleaning the date Taking out records where Sale Price = 0 and Square feet = 0
Nyc5 <- Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "ST. ALBANS", !SALE_PRICE == 0, !GROSS_SQUARE_FEET == 0, !RESIDENTIAL_UNITS == 0) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  group_by(SALE_YEAR)
#view(Nyc5)

YearlySales4 <- data.frame(summarise(Nyc5, SALE_TOTAL=sum(SALE_PRICE), TOTAL_HOUSES_SOLD = sum(RESIDENTIAL_UNITS)) %>%
                             mutate(AverageYearlySales = SALE_TOTAL/TOTAL_HOUSES_SOLD))

Presentation_YSA <- data.frame(summarise(Nyc5, SALE_TOTAL=sum(SALE_PRICE), TOTAL_HOUSES_SOLD = sum(RESIDENTIAL_UNITS)) %>%
                                 mutate(AverageYearlySales = SALE_TOTAL))

AvgSquareFootage4 <- summarise(Nyc5, SALE_TOTAL=sum(SALE_PRICE), Total_Square_Footage = sum(GROSS_SQUARE_FEET)) %>%
  mutate(AverageSquareFootage = SALE_TOTAL/Total_Square_Footage)

#Plots
plot3 <- ggplot() + geom_line(data = YearlySales3, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "red") +
  geom_line(data = YearlySales4, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "blue")  

plot4 <- ggplot() + geom_line(data = AvgSquareFootage3, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "red") +
  geom_line(data = AvgSquareFootage4, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "blue") 


#-------Jamaica----------

Nyc6 <- Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "JAMAICA", !RESIDENTIAL_UNITS == 0) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  mutate(QTY = 1) %>%
  group_by(SALE_YEAR)
#view(Nyc6)

YearlySales5 <- data.frame(summarise(Nyc6, SALE_TOTAL=sum(SALE_PRICE), sum(RESIDENTIAL_UNITS)) %>%
                             mutate(AverageYearlySales = SALE_TOTAL/TOTAL_HOUSES_SOLD)) 

AvgSquareFootage5 <- summarise(Nyc6, SALE_TOTAL=sum(SALE_PRICE), Total_Square_Footage = sum(GROSS_SQUARE_FEET)) %>%
  mutate(AverageSquareFootage = SALE_TOTAL/Total_Square_Footage)

#Taking out Sale Price 0 and Square feet = 0
Nyc7 <- Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "JAMAICA", !SALE_PRICE == 0, !GROSS_SQUARE_FEET == 0, !RESIDENTIAL_UNITS == 0) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  group_by(SALE_YEAR)
view(Nyc7)

YearlySales6 <- data.frame(summarise(Nyc7, SALE_TOTAL=sum(SALE_PRICE), sum(RESIDENTIAL_UNITS)) %>%
                             mutate(AverageYearlySales = SALE_TOTAL/TOTAL_HOUSES_SOLD))

AvgSquareFootage6 <- summarise(Nyc7, SALE_TOTAL=sum(SALE_PRICE), Total_Square_Footage = sum(GROSS_SQUARE_FEET)) %>%
  mutate(AverageSquareFootage = SALE_TOTAL/Total_Square_Footage)


#Plots
plot5 <- ggplot() + geom_line(data = YearlySales5, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "red") +
  geom_line(data = YearlySales6, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "blue")  

plot6 <- ggplot() + geom_line(data = AvgSquareFootage5, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "red") +
  geom_line(data = AvgSquareFootage6, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "blue") 


#looking at queens.

Nyc8 <- Nyc %>% 
  filter(BOROUGH_NAME == "QUEENS", !SALE_PRICE == 0, !GROSS_SQUARE_FEET == 0, !RESIDENTIAL_UNITS == 0) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  group_by(SALE_YEAR)
#view(Nyc8)

YearlySales7 <- data.frame(summarise(Nyc8, SALE_TOTAL=sum(SALE_PRICE), sum(RESIDENTIAL_UNITS)) %>%
                             mutate(AverageYearlySales = SALE_TOTAL/TOTAL_HOUSES_SOLD))

AvgSquareFootage7 <- summarise(Nyc8, SALE_TOTAL=sum(SALE_PRICE), Total_Square_Footage = sum(GROSS_SQUARE_FEET)) %>%
  mutate(AverageSquareFootage = SALE_TOTAL/Total_Square_Footage)


#Addtional plots to compare neighborhoods
Colors <- c(red = "South Jamaica", blue = "St. Albans", black = "Jamaica")

Nplot1 <- ggplot() + geom_line(data = YearlySales2, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "red") +
  geom_line(data = YearlySales4, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "blue") +
  geom_line(data = YearlySales6, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "black") +
  geom_line(data = YearlySales7, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "green") +
  labs(title = "Comparing neighborhoods Average Yearly Sales", x = "Years", y = "Average Sale Price")

Presentation3 <-   ggplot() + geom_line(data = YearlySales2, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "red") +
  geom_line(data = YearlySales4, mapping = aes(x = SALE_YEAR, y = AverageYearlySales), color = "blue") +
  labs(title = "Comparing neighborhoods Average Yearly Sales", x = "Years", y = "Average Sale Price")


Nplot2 <- ggplot() + geom_line(data = AvgSquareFootage2, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "red") + 
  geom_line(data = AvgSquareFootage4, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "blue") +
  geom_line(data = AvgSquareFootage6, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "black") +
  geom_line(data = AvgSquareFootage7, mapping = aes(x = SALE_YEAR, y = AverageSquareFootage), color = "green") +
  labs(title = "Comparing neighborhoods Average Yearly Sales", x = "Years" , y = "Average price Per Sqaure Foot")

#library(odbc)
#library(DBI)
library(tidyverse)
library(lubridate)
#library(gapminder)
#load("~/Documents/Boston University/AD571/R NYC Datasets.RData")
install.packages("factoextra")
library(factoextra)
options(scipen = 999)
#-----Analysis-------

str(Borough)
View(Neighborhood)
view(Building_Class)
str(Nyc)
Borough <- data.frame("BOROUGH_ID" = c("1", "2", "3", "4", "5"), "BOROUGH_NAME" = c("MANHATTAN","BRONX","BROOKLYN","QUEENS","STATEN ISLAND"))
head(Historical)
head(Building_Class, 20)
#Joining all the tables togehter
Nyc <- Historical %>%
  left_join(Building_Class, by=c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID")) %>%
  left_join(Neighborhood, by=c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID")) %>%
  left_join(Borough, by=c("BOROUGH_ID" = "BOROUGH_ID")) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  select(SALE_PRICE, SALE_YEAR, TYPE, NEIGHBORHOOD_NAME, RESIDENTIAL_UNITS, GROSS_SQUARE_FEET, BOROUGH_NAME, NEIGHBORHOOD_ID, COMMERCIAL_UNITS)

View(Nyc)

Nycnbh <- Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "SO. JAMAICA-BAISLEY PARK", SALE_PRICE >= 10000, 
         !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019)) %>%
  group_by(SALE_YEAR)

r_Nycnbh <- Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "SO. JAMAICA-BAISLEY PARK", SALE_PRICE >= 10000,
         GROSS_SQUARE_FEET != 0, SALE_YEAR %in% c(2009:2019), 
         TYPE == "RESIDENTIAL") %>%
  group_by(SALE_YEAR)

view(r_Nycnbh)


#Task A Total number of sales in your nbh
Total_nbh <- summarise(Nycnbh, Total_sales = sum(SALE_PRICE), Total_sq_feet = sum(GROSS_SQUARE_FEET))
sum(Total_nbh$Total_sales) 
y1 <- ggplot(Total_nbh) + geom_line(mapping = aes(x = (SALE_YEAR), y = (Total_sales))) + labs(title = "Total Sales Per Year", y = " Total Sales", x = "Year")

#Task B Mean sale Price and gross square footage
Total_r_nbh <- summarise(r_Nycnbh, Total_sales1 = sum(SALE_PRICE), Total_sq_feet1 = sum(GROSS_SQUARE_FEET))
mean_per_year <- summarise(r_Nycnbh, mean_sales = mean(SALE_PRICE), mean_sq_feet = mean(GROSS_SQUARE_FEET)) 
mean((r_Nycnbh$SALE_PRICE))
mean((r_Nycnbh$GROSS_SQUARE_FEET))

y2s <- ggplot(mean_per_year) + geom_line(mapping = aes(x = (SALE_YEAR), y = (mean_sales))) + labs(title = "Average Sale Price Per Year", y = "Avg Sale Price")
y2a <- ggplot(mean_per_year) + geom_line(mapping = aes(x = (SALE_YEAR), y = (mean_sq_feet))) + labs(title = "Average Square Footage Per Year", y = "Avg Sq Foot")



#Task C Five number summary
Summary_of_Sale_Price <- summary(r_Nycnbh$SALE_PRICE)
Summary_Gross_Sq_Feet <- summary(r_Nycnbh$GROSS_SQUARE_FEET)


(filter(.data = r_Nycnbh, SALE_PRICE < 10000000)) %>%
ggplot() + geom_histogram(mapping = aes(x = SALE_PRICE)) + labs(title = "Sale Price Histogram")
#slightly right skewed


#Task D Proportion of residential, commercial, mixed, others
Nycnbh_3 <- Nyc %>% 
  filter(NEIGHBORHOOD_NAME == "SO. JAMAICA-BAISLEY PARK", !SALE_PRICE == 0, !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019)) %>%
  group_by(TYPE) %>%
  tally()
Proportion_df <- data.frame(prop.table(Nycnbh_3$n), Type = (c(Nycnbh_3$TYPE)))
ggplot() + geom_col(data = Proportion_df, aes(x = Type,  y = prop.table.Nycnbh_3.n.), color = "red", fill = "orange") + labs(y = "Proportion", title = "Proportion of Property Types")

#view(Proportion_df)


#Task E and F Standard deviation and correlation
m <- cbind(mean(r_Nycnbh$SALE_PRICE), mean(r_Nycnbh$GROSS_SQUARE_FEET), 
           median(r_Nycnbh$SALE_PRICE), median(r_Nycnbh$GROSS_SQUARE_FEET), 
           sd(r_Nycnbh$SALE_PRICE), sd(r_Nycnbh$GROSS_SQUARE_FEET), 
          cov(r_Nycnbh$SALE_PRICE, r_Nycnbh$GROSS_SQUARE_FEET),
          cor(r_Nycnbh$SALE_PRICE,r_Nycnbh$GROSS_SQUARE_FEET)) %>%
`colnames<-`(c("Mean_SalePrice", "Mean_SqFeet", "Median_SalePrice", "Median_SqFeet", "Sd_SalePrice", "Sd_SqFeet", "Covariance", "Correlation")) %>%
`row.names<-`("So. Jamaica-Balsey Park")

(filter(.data = r_Nycnbh, SALE_PRICE < 10000000)) %>%
ggplot(mapping = aes(y = GROSS_SQUARE_FEET, x = SALE_PRICE)) + geom_point() + labs(title = "Scatterplot of Sale Price and Gross Square Feet")

##### Task 2 K-means Clustering #####

#Clustering based on average Sq foot price, Total Sales, and SD of Sale price

Kl1 <- Nyc %>%
  filter(SALE_PRICE >= 10000, !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019), TYPE == "RESIDENTIAL") %>%
  group_by(NEIGHBORHOOD_NAME) %>%
  summarise(AvgSqFoot = sum(GROSS_SQUARE_FEET)/sum(SALE_PRICE), TotalSales = sum(SALE_PRICE), SD = sd(SALE_PRICE)) %>%
  mutate_if(is.numeric, scale) %>%
  na.omit()

view(Kl1)

Kln1 <- Kl1[-1]

view(Kln1)
#Finding the amount of clusters using the elbow method (Mathematically optimal Cluster size)
set.seed(123)
#function compute total within-cluster sum of square
wss <- function(k) {kmeans(Kln1, k, nstart = 15)$tot.withinss}
k.values <- 1:15
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values, type = "b", pch = 19, frame = FALSE, main = "Elbow Plot")

Cluster_1 <- kmeans(Kln1, 10)

Clustered1 <- mutate(.data = Kl1, Cluster = Cluster_1$cluster)

ggplot(data = Clustered1) + geom_point(mapping = aes(x = AvgSqFoot, y = TotalSales, color = Cluster, size = SD))

#filter some outliers
Clustered1F <- filter(Clustered1, TotalSales < 3.5, AvgSqFoot < 3.5)

ggplot(data = Clustered1F ) + geom_point(mapping = aes(x = AvgSqFoot, y = TotalSales, color = Cluster, size = SD))


#Changing the cluster size to 5. 

Cluster_2 <- kmeans(Kln1, 5)

#view(as.data.frame(Cluster_2$centers))

Clustered2 <- mutate(.data = Kl1, Cluster = Cluster_2$cluster) %>%
  filter(NEIGHBORHOOD_NAME %in%  c("SO. JAMAICA-BAISLEY PARK", "ST. ALBANS", "RICHMOND HILL", "JAMAICA",
                                "WOODHAVEN", "ROSEDALE", "LAURELTON", "OZONE PARK", "SOUTH OZONE PARK", "BRIARWOOD", "FOREST HILLS"))

ggplot(data = Clustered2) + geom_point(mapping = aes(x = AvgSqFoot, y = TotalSales, color = Cluster, size = SD))

#filter some outliers
Clustered2F <- filter(Clustered2, TotalSales < 3.5, AvgSqFoot < 3.5)

ggplot(data = Clustered2F ) + geom_point(mapping = aes(x = AvgSqFoot, y = TotalSales, color = Cluster, size = SD))

view(Clustered2)

# All surrounding nbhds are either in cluster one or five. 
##### Task 3 T-Test #####

Tn1 <- Nyc %>%
  filter(SALE_PRICE >= 10000, !GROSS_SQUARE_FEET == 0, 
         SALE_YEAR %in% c(2009:2019), 
         NEIGHBORHOOD_NAME == c("SO. JAMAICA-BAISLEY PARK", "ST. ALBANS")) %>%
  group_by(NEIGHBORHOOD_NAME) %>%
  summarise(Average_SalePrice = (sum(SALE_PRICE)/(sum(GROSS_SQUARE_FEET))))


t.test(Tn1$Average_SalePrice, conf.level = .95)
?t.test
#under Cluster 5
Nyc_SoJam <- filter(.data = Nyc, NEIGHBORHOOD_NAME == "SO. JAMAICA-BAISLEY PARK", TYPE == "RESIDENTIAL", 
                    SALE_PRICE >= 10000, !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019)) 
Nyc_StAlb <- filter(.data = Nyc, NEIGHBORHOOD_NAME == "ST. ALBANS", TYPE == "RESIDENTIAL", 
                    SALE_PRICE >= 10000, !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019)) 
Nyc_RICH <- filter(.data = Nyc, NEIGHBORHOOD_NAME == "RICHMOND HILL", TYPE == "RESIDENTIAL", 
                    SALE_PRICE >= 10000, !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019))

#Under Cluster 1

Nyc_Jam <- filter(.data = Nyc, NEIGHBORHOOD_NAME == "JAMAICA", TYPE == "RESIDENTIAL", 
                   SALE_PRICE >= 10000, !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019))
Nyc_Wood <- filter(.data = Nyc, NEIGHBORHOOD_NAME == "WOODHAVEN", TYPE == "RESIDENTIAL", 
                   SALE_PRICE >= 10000, !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019))
Nyc_Laur <- filter(.data = Nyc, NEIGHBORHOOD_NAME == "LAURELTON", TYPE == "RESIDENTIAL", 
                   SALE_PRICE >= 10000, !GROSS_SQUARE_FEET == 0, SALE_YEAR %in% c(2009:2019))

volume1 <- select(.data =Nyc_SoJam, SALE_PRICE, SALE_YEAR) %>%
  group_by(SALE_YEAR) %>%
  summarise(Total_Sales_per_year = sum(SALE_PRICE))
volume2 <- select(.data =Nyc_StAlb, SALE_PRICE, SALE_YEAR) %>%
  group_by(SALE_YEAR) %>%
  summarise(Total_Sales_per_year = sum(SALE_PRICE))
volume3 <- select(.data =Nyc_RICH, SALE_PRICE, SALE_YEAR) %>%
  group_by(SALE_YEAR) %>%
  summarise(Total_Sales_per_year = sum(SALE_PRICE))
volume4 <- select(.data =Nyc_Jam, SALE_PRICE, SALE_YEAR) %>%
  group_by(SALE_YEAR) %>%
  summarise(Total_Sales_per_year = sum(SALE_PRICE))
volume5 <- select(.data =Nyc_Wood , SALE_PRICE, SALE_YEAR) %>%
  group_by(SALE_YEAR) %>%
  summarise(Total_Sales_per_year = sum(SALE_PRICE))
volume6 <- select(.data =Nyc_Laur , SALE_PRICE, SALE_YEAR) %>%
  group_by(SALE_YEAR) %>%
  summarise(Total_Sales_per_year = sum(SALE_PRICE))





t.test(x = Nyc_SoJam$SALE_PRICE, y = Nyc_StAlb$SALE_PRICE, conf.level = .95, alternative = "two.sided")
t.test(x = Nyc_SoJam$SALE_PRICE, y = Nyc_StAlb$SALE_PRICE, conf.level = .95, alternative = "less")
#St albans has the higher average sale price. 
t.test(x = Nyc_SoJam$SALE_PRICE, y = Nyc_RICH$SALE_PRICE, conf.level = .95, alternative = "two.sided")
t.test(x = Nyc_SoJam$SALE_PRICE, y = Nyc_RICH$SALE_PRICE, conf.level = .95, alternative = "less")
#Richmond has a much higher sale price on average. 
t.test(x = Nyc_SoJam$SALE_PRICE, y = Nyc_Jam$SALE_PRICE, conf.level = .95, alternative = "two.sided")
t.test(x = Nyc_SoJam$SALE_PRICE, y = Nyc_Jam$SALE_PRICE, conf.level = .95, alternative = "less")
#Jamaica has a much much higher sale price on average however its volume is too less so it is effected greatly by outliers
t.test(x = Nyc_SoJam$SALE_PRICE, y = Nyc_Wood$SALE_PRICE, conf.level = .95, alternative = "two.sided")
t.test(x = Nyc_SoJam$SALE_PRICE, y = Nyc_Wood$SALE_PRICE, conf.level = .95, alternative = "less")
#Woodhaven has a much higher sale price on average. 
t.test(x = Nyc_SoJam$SALE_PRICE, y = Nyc_Laur$SALE_PRICE, conf.level = .95, alternative = "two.sided")
t.test(x = Nyc_SoJam$SALE_PRICE, y = Nyc_Laur$SALE_PRICE, conf.level = .95, alternative = "less")
#Laurelton has a higher average price. 


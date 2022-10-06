library(tidyverse)
library(ggplot2)
library(dplyr)
library(plm)
library(interplot)

rm(list=ls()) # clear environment

# Session -> Set Working Directory -> Choose Directory -> select the folder where you saved you script and data.csv
data <- read.csv('data.csv') # with cwd set to project folder

########## DESCRIPTIVE STATISTICS ##########
names(data)
head(data)
summary(data) # statistical summary for each variable
NROW(na.omit(data)) # to remove rows with null values

pdata <- filter(data, sector==5) # data set filtered for HEALTHCARE sector (5)
summary(pdata)

# sanity checks
pdata <- filter(pdata, assets>0)
pdata <- filter(pdata, sales>0)
pdata <- filter(pdata, mv>0)
pdata <- filter(pdata, fv>0)
pdata <- filter(pdata, mkt>0)
summary(pdata)

require(dplyr)
pdata %>% count(id) # the panel is actually unbalanced, i.e., there are a different number of observations for each firm
pdata %>% count(year)
pdata %>% is.pbalanced() #FALSE

filter(pdata, id==1) # just one example
id <- table(pdata$id)
pdata <- pdata[pdata$id %in% names(id)[id>1],] # remove the companies for which we have less than 3 observations
pdata %>% count(id) # check

# scatter plot of sales over time; the blue line connects the mean values of sales to show the trend
pdata %>%
  group_by(year) %>%
  summarise(sales_mean = mean(sales)) %>%
  left_join(pdata) %>%
  ggplot(data = ., 
         aes(x=year,y=sales)) +
  geom_point() +
  geom_line(aes(x=year,y=sales_mean),col="blue") +
  scale_x_continuous(labels=as.character(pdata$year), 
                     breaks=pdata$year) +
  labs(x="Year", y="Sales") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90))

# sum of sales by year (not very accurate as market size since there might be other firms in the industry for which we do not have data)
sales_by_year = summarise_at(group_by(pdata, year), vars(sales), sum)
colnames(sales_by_year) <- c("year", "tot_sales")

fig1 <- ggplot(data=sales_by_year, aes(x=year, y=tot_sales)) +
  geom_line(color="steelblue") +
  geom_area(fill='steelblue', alpha=0.6) +
  theme_minimal() +
  labs(x="Year", y="Total Sales (million $)") +
  theme(axis.text.y = element_text(size=8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
fig1

data = list(pdata, sales_by_year)
data = data %>% reduce(full_join, by=data$year)
data$mkt_share = (data$sales/data$tot_sales)*100
check = summarise_at(group_by(data, year), vars(mkt_share), sum)

data$mkt_share_sq ='^'(data$mkt_share,2)
hhi_by_year = summarise_at(group_by(data, year), vars(mkt_share_sq), sum)
colnames(hhi_by_year) <- c("year", "hhi_index")

data = list(data, hhi_by_year)
data = data %>% reduce(full_join, by=data$year)

fig2 <- ggplot(data=hhi_by_year, aes(x=year, y=hhi_index)) +
  geom_area(fill="dark green", alpha=0.4) +
  geom_line(color="dark green") +
  geom_point(color="dark green") +
  theme_minimal() +
  labs(x="Year", y="HHI Index") +
  theme(axis.text.y = element_text(size=8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_hline(yintercept = 1500, colour="red", lty="dashed") +
  geom_hline(yintercept = 2500, colour='orange', lty="dashed")
fig2

data$ads = (data$ad/data$sales)
data$rds = (data$rd/data$sales)
data$mkts = (data$mkt/data$sales)

data$ad_intensity = (data$ad/data$assets)

ad_by_year = summarise_at(group_by(data, year), vars(ad), sum)
colnames(ad_by_year) <- c("year", "tot_ad")
data = list(data, ad_by_year)
data = data %>% reduce(full_join, by=data$year)
data$sov = (data$ad/data$tot_ad)

########## MODEL ##########
pdata_frame <- pdata.frame(data, index=c("id","year"))
pdim(pdata_frame)

require(dplyr)

n_distinct(pdata_frame$id)
#pdata_frame <- transform(pdata_frame, lag_mkt=lag(pdata_frame$mkt))

write_csv(pdata_frame, "C:\\Users\\ggatt\\OneDrive\\Desktop\\marketing analytics\\R_project\\regr_data.csv")

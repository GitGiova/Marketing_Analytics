# refine data set
# descriptive analysis eg. plot evolution of variables over time
# check for missing/zero/null values
# select regression
# choose & build variables, eg. HHI
# perform variable selection

# first regression: firm value --> independent variables using HEALTHCARE sector
# then maybe perform regressions comparing different sectors

library(tidyverse)
library(ggplot2)
library(dplyr)
library(plm)

rm(list=ls()) # clear environment

# Session -> Set Working Directory -> Choose Directory -> select the folder where you saved you script and data.csv
data <- read.csv('data.csv') # with cwd set to project folder

########## DESCRIPTIVE STATISTICS ##########
names(data)
head(data)
summary(data) # statistical summary for each variable
NROW(na.omit(data)) # to remove rows with null values

pdata <- filter(data, sector==5) # data set filtered for HEALTHCARE sector (5)

require(dplyr)
pdata %>% count(id) # the panel is actually unbalanced, i.e., there are a different number of observations for each firm
pdata %>% is.pbalanced() #FALSE

filter(pdata, id==1) # just one example

id <- table(pdata$id)
pdata <- pdata[pdata$id %in% names(id)[id>2],] # remove the companies for which we have less than 2 observations
pdata %>% count(id) # check

# this plot looks bad because of: -too many ids; -unbalanced panel
#ggplot(data=pdata, aes(x=year,y=sales)) +
#  geom_line(aes(colour=as.factor(id))) +
#  labs(x="Year", y="Sales") +
#  theme(legend.position="none")

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

# sum of sales by year (not very accurate as market size since there might be other firms in the instustry for which we do not have data)
sales_by_year = summarise_at(group_by(pdata, year), vars(sales), sum)

fig1 <- ggplot(data=sales_by_year, aes(x=year, y=sales)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(x="Year", y="Total Sales (million $)") +
  theme(axis.text.y = element_text(size=8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
fig1

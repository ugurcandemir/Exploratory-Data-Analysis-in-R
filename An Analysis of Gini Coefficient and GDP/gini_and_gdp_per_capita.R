
# In this script we will conduct a socio-economic analysis. We will take a look
# at gini coefficient , gdp growth and gdp per capita. The most recent year all
# three datasets have in common is 2013 so naturally that is our basis year.


# As always we start by importing the libraries and the datasets.

library(data.table)
library(tidyverse)


gini <- fread('gini.csv' , header = TRUE)
gdp_growth <- fread('gdp_total_yearly_growth.csv' , header = TRUE)
gdp_per_capita <- fread('gdppercapita_us_inflation_adjusted.csv' , header = TRUE)


# We wrangle the data as we need it.

gini <- gini[ , c('country' , '2013')]
gdp_growth <- gdp_growth[ , c('country' , '2013')]
gdp_per_capita <- gdp_per_capita[ , c('country' , '2013')]

gini <- na.omit(gini)
gdp_growth <- na.omit(gdp_growth)
gdp_per_capita <- na.omit(gdp_per_capita)

colnames(gini) <- c("country" , "gini")
colnames(gdp_growth) <- c("country" , "gdp_growth")
colnames(gdp_per_capita) <- c("country" , "gdp_per_capita")

dataset <- inner_join(x = gini , y = gdp_growth )
dataset <- inner_join(x = dataset , y = gdp_per_capita )

# Let's look at the correlation coefficients between the variables.

cor(x = dataset$gini , y = dataset$gdp_growth)
cor(x = dataset$gini , y = dataset$gdp_per_capita)
cor(x = dataset$gdp_growth , y = dataset$gdp_per_capita)



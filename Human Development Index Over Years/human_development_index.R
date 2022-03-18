
# This script is written to conduct some analysis on the human development
# index data.



# First , we import the libraries that we are going to use throughout our
# analysis.

library(readr)
library(data.table)
library(tidyverse)

# Here we import the dataset by using two different functions.
# It already is in our working directory but we could also navigate our
# interpreter to the directory that the dataset is in.

hdi1 <- read_csv('Human development index (HDI).csv' , skip = 1)
hdi2 <- fread('Human development index (HDI).csv' , skip = 1 , header = T)


# Both of our functions imported the dataset just as we wanted.
# After importing , we skip to cleaning , wrangling and tidying our dataset.

# The values of the variable year are placed as columns.
# We will make it fit into a tidy format.
# We should keep in mind that we could also want it to stay like that 
# depending on the analysis we run.
# But generally tidy data is the preferred form.

hdi1 <- gather(hdi1 , 'years' , 'hdi' , 3:31 )

# In this dataset NA values are denoted as '..' . We need to detect where they
# occur and change them to NAs and eliminate them if necessary.

hdi1$hdi[ which(hdi1$hdi == '..') ] <- NA

hdi1 <- as.data.frame(hdi1)

class(hdi1$`HDI Rank (2018)`) <- 'integer'
class(hdi1$years) <- 'integer'
class(hdi1$hdi) <- 'numeric'




# We fully tamed our data so it is ready fo be analyzed. Let's dive in.


# Top 5 countries in 2018.

hdi1 %>%
  filter(years == 2018)%>%
  arrange(desc(hdi))%>%
  top_n(5)

# Bottom 5 countries in 2018.

hdi1 %>%
  filter(years == 2018)%>%
  arrange(hdi) %>%
  top_n(-5)
  


hdi1 %>%
  filter(Country %in% c('Turkey' , 'France' , 'Iraq' , 'Pakistan')) %>%
  ggplot(mapping = aes(x = years , y = hdi , color = Country)) + 
  geom_point() + labs(title = "HDI Comparison" ,
                    subtitle = "HDI Values of Four Countries over Years")




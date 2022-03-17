
# I chose this particular dataset because this dataset represents what we call
# panel data in the Statistics/Econometrics terminology. This type of data allow
# us to run both Time Series and Cross-Sectional methods.

# As always we start by importing the libraries and the dataset.

library(tidyverse)
library(readxl)

california_residents <- read_xlsx("co-est2019-annres-06.xlsx" , skip = 3)


# We extract the portion of the data as we will use it.

california_residents <- california_residents[ c(1:59) , ]
california_residents <- california_residents[ , -c(2,3)]


# Regular data wrangling and preparation. It seems like we need to work on the
# county names a little bit.It is going to take a little bit of coding to remedy
# that. Thank god R is excellent at string manipulation.


# Let's take the word California out.

colnames(california_residents)[1] <- 'County'

california_residents <- separate(data = california_residents ,
                                 col = County ,
                                 into = c('County Name' , 'California Word') ,
                                 sep = "," )


# Divide the column into two.

county_names <- c(as.vector(california_residents[ , 1]))[[1]]

county_names1 <- county_names[1]
county_names2 <- county_names[2 : length(county_names)]



# Extract each word until we encounter the word "County".

list_of_where_the_word_county_begins <- gregexpr(pattern = 'County' ,
                                                 text = county_names2)

vector_of_where_the_word_county_begins <- vector(length = 58)

for (each in 1:length(list_of_where_the_word_county_begins)) {
  vector_of_where_the_word_county_begins[each] <- list_of_where_the_word_county_begins[[each]][1]
}

vector_of_where_the_word_county_begins <-  vector_of_where_the_word_county_begins -2 

county_names_without_the_word_county <- vector(length = 58)

for (i in 1:length(vector_of_where_the_word_county_begins)) {
  county_names_without_the_word_county[i] <- 
    substr(x = county_names2[i] , start = 1 , vector_of_where_the_word_county_begins[i])
}


county_names_without_the_word_county

# Now we delete the dot at the beginning of the each county name.

vector_of_county_names <- county_names_without_the_word_county

for (each_county_name_index in 1:length(vector_of_county_names)) {
  vector_of_county_names[each_county_name_index] <- substring(
    text = vector_of_county_names[each_county_name_index] ,
    first = 2 ,
    nchar(county_names[2]))
}

final_county_name_vector <- c(county_names1 , vector_of_county_names)


# Final touches.

california_residents$`County Name` <- final_county_name_vector
california_residents <- california_residents[ , -2]
colnames(california_residents)[1] <- 'county'





# Our data is analysis ready!



# For this analysis we well use our data in two different forms because our
# data is not tidy. What do I mean by that ? Here it is : years , given as columns
# are actually not variables , they are values. Therefore they should be 
# presented under a variable title called 'Years'. However as our data is right 
# now , we can run interesting methods though. This is why we'll keep our data
# in two different forms.

california_residents1 <- california_residents

california_residents2 <- pivot_longer(data = california_residents ,
                                      cols = 2:ncol(california_residents) ,
                                      names_to = "Years")

colnames(california_residents2)[3] <- 'Residents'
california_residents2$Years <- as.integer(california_residents2$Years)

# The most populated 10 counties in the most recent year.

california_residents1 %>%
  select(county , `2019`) %>%
  arrange(desc(`2019`)) %>%
  filter(county != 'California')

# The least populated 10 counties in the most recent year.

california_residents1 %>%
  select(county , `2019`) %>%
  arrange(`2019`) %>%
  filter(county != 'California')


california_residents2 %>%
  select(county , Years , Residents) %>%
  filter(Years == 2019 , county != 'California') %>% 
  arrange(Residents) %>%
  ggplot(aes(x= county, y= Residents , label= Residents)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = county, 
                   yend = Residents, 
                   xend = county), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="The Most Populous Counties", 
       subtitle="LA stands out") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6))
 

california_residents2 %>%
  filter(county == 'California') %>%
  ggplot( aes(x=Years , y = Residents )) + 
  geom_point() + 
  labs(title="California Total Population by Years")



california_residents2 %>%
  filter(county %in% c( 'Los Angeles' , 'San Diego' , 'Colusa' )) %>%
  ggplot( aes(x=Years , y = Residents , color = county)) + 
  geom_point() + 
  labs(title="Total Population of Some Counties")





    
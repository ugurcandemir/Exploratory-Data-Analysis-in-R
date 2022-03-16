
# In this script we analyze the 2016 Presidential Elections in the State of 
# Tennessee , particularly on the basis of county.

# We start by importing the libraries and the dataset.

library(readxl)
library(tidyverse)
library(urbnmapr)


elections <- as.data.frame( read_xlsx( path = 'Tennessee.xlsx') )

# We begin data cleaning by picking nine variables at first.
elections <- elections[ ,c(1 , 7 , 10:17) ]

# Since we are interested in only the presidential elections , not congressional
# elections , we filter by the variable OFFICENAME. 
elections <- elections[elections$OFFICENAME == 'United States President' , ]

# We want to see only Trump and Clinton. 
elections <- elections[elections$COL1HDG == 1 | elections$COL2HDG == 2 , ]

# A little bit more data cleaning.
elections <- elections[ , c(1 , 4:6 , 8:10 )]


# The analysis stage begins.

elections <- elections %>% 
  group_by(COUNTY) %>%
  summarise(TRUMP_VOTE = sum(PVTALLY1) , CLINTON_VOTE = sum(PVTALLY2)) %>%
  mutate(GOP_PERC = TRUMP_VOTE/(TRUMP_VOTE+CLINTON_VOTE) ,
                                DEM_PERC = CLINTON_VOTE/(TRUMP_VOTE+CLINTON_VOTE))

# It is quite improbable but we check if the two candidates got equal vote in
# any county.
any(elections$TRUMP_VOTE == elections$CLINTON_VOTE)


elections$WINNER <- ifelse(test = elections$TRUMP_VOTE > elections$CLINTON_VOTE  ,
                           yes =  'REPUBLICAN' , 
                           no = 'DEMOCRAT')

elections$DIFFERENCE <- as.integer(elections$TRUMP_VOTE - elections$CLINTON_VOTE)



# Visualizations.

# Our first graph.
  
ggplot(data = elections , aes(x = COUNTY, y= DIFFERENCE, label=DIFFERENCE)) + 
  geom_point(mapping = aes(color = DIFFERENCE > 0) ,  stat='identity', fill="black", size=6)  +
    scale_colour_manual(name = 'TRUMP WON?', values = setNames(c('red','blue'),c(T, F))) +
  geom_segment(aes(y = 0, 
                   x = `COUNTY`, 
                   yend = DIFFERENCE, 
                   xend = `COUNTY`), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Which counties did Trump win?", 
       subtitle="Trump won 92 of the 95 counties. But he couldn't beat Hillary in
       any county like she beat him in Shelby and Davidson.") +
    theme(axis.text.x = element_text(angle=90, vjust=0.6))
 

# Our second graph needs a little bit work. We need to merge our data with
# the county data which contains coordinates for geo-spatial analysis.
counties1 <- counties[counties$state_abbv =="TN" , ]
counties2 <- separate(data = counties1 , col = "county_name" , c("COUNTY" , "TRASH"))

# Let's merge!
county_elections <- inner_join(x = counties2 , y = elections , "COUNTY")


countydata %>% 
  left_join(county_elections, by = "county_fips") %>% 
  filter(state_name =="Tennessee") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = DEM_PERC)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradientn(labels = scales::percent, colors = c('red' , 'blue'  )   ,
                       guide = guide_colorbar(title.position = "top")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Homeownership rate") 
  






  
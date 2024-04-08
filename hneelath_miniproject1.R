library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(maps)

Global_YouTube_Statistics <- read_excel("C:/Users/harsh/Desktop/Data Science/mini project 1/Global YouTube Statistics.xlsx", 
                                        na = c("nan", "NaN"))
View(Global_YouTube_Statistics)
glimpse(Global_YouTube_Statistics)


#1-----Finding the number of missing values in each column
naValues<- data.frame('Count_of_NA_Values' = colSums(is.na(Global_YouTube_Statistics))) 
View(naValues)

#2--------Finding the number of Channels per category
channelsPerCategory<-Global_YouTube_Statistics %>% group_by(category) %>% summarise(length(Youtuber)) %>% drop_na()
names(channelsPerCategory) <- c('Category', 'TotalChannels')
channelsPerCategory <- channelsPerCategory %>% arrange(desc(`TotalChannels`))
channelsPerCategory$Category<- factor(channelsPerCategory$Category, levels = rev(channelsPerCategory$Category))

#3--------Visualizing the number of channels for each category
ggplot(data=channelsPerCategory, mapping = aes(Category,TotalChannels, fill=Category)) + 
  geom_bar(stat='identity') + theme(legend.position="none") + 
  geom_text(aes(label= TotalChannels), hjust=-0.1, color="black", size=3.2) + 
  coord_flip()


#4--------Finding average number of subscribers of channels in each country
avgSubsPerCountry <- Global_YouTube_Statistics %>% group_by(Country) %>% 
  summarise('Subscribers(millions)' = mean(subscribers/1000000)) %>% na.omit()
View(avgSubsPerCountry)

#5------Plotting the average number of subscribers of channels in each country on world map
avgSubsPerCountry$Country[avgSubsPerCountry$Country=='United States']<-'USA'
avgSubsPerCountry$Country[avgSubsPerCountry$Country=='United Kingdom']<-'UK'
worldMap <- map_data(map = 'world')
ggplot(data = avgSubsPerCountry) + 
  geom_map(aes(map_id = Country, fill = `Subscribers(millions)`), map = worldMap) + 
  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) + 
  expand_limits(x = worldMap$long, y = worldMap$lat)  + coord_fixed() + theme_void()
  

#6---------Finding most popular category by country
popularCategory <- Global_YouTube_Statistics %>% group_by(Country) %>% count(category) %>% filter(n == max(n)) %>% na.omit()
names(popularCategory)<- c('Country', "Most_Popular_Category", 'Number_of_Channels')
View(popularCategory)

#7---------Plotting the most popular category in each country on a world map
popularCategory$Country[popularCategory$Country == 'United States'] <- 'USA'
popularCategory$Country[popularCategory$Country == 'United Kingdom'] <- 'UK'
ggplot(data = popularCategory) + 
  geom_map(aes(map_id = Country, fill = Most_Popular_Category), map = worldMap) + 
  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) + 
  expand_limits(x = worldMap$long, y = worldMap$lat)  + coord_fixed() + theme_void()
                                     


#8 --- Finding the number of channels per each country and 
#excluding youtube's own channels since they are not user created
#and changing the countries to factors for easy plots
channelsPerCountry<-Global_YouTube_Statistics %>% group_by(Country) %>% summarise('TotalChannels' = length(Youtuber)) %>% drop_na()
channelsPerCountry <- channelsPerCountry %>% arrange(desc(`TotalChannels`))
channelsPerCountry$Country<- factor(channelsPerCountry$Country, levels = rev(channelsPerCountry$Country))
View(channelsPerCountry)

#9-----------Visualizing the number of channels for each country 
#for the top 25 countries with highest numbner of channels
ggplot(data=channelsPerCountry[1:25,], aes(x=Country, y=TotalChannels, fill= Country)) +
  geom_bar(stat="identity") + coord_flip() + theme(legend.position="none") + 
  geom_text(aes(label= TotalChannels), hjust=-0.1, color="black", size=3.2)


#10-----------Finding the number of Views and subscribers per each category
categoryViewsSubscribers <- Global_YouTube_Statistics %>% group_by(category) %>% 
  summarise(sum(`video views`, na.rm = T)/1000000000, sum(subscribers, na.rm = T)/1000000) %>% na.omit()
names(categoryViewsSubscribers)<- c('Category', 'Views(Billions)', 'Subscribers(Millions)')
View(categoryViewsSubscribers)


#11---------Visualizing the relation between Views and Subscribers
ggplot(data=Global_YouTube_Statistics, mapping = aes(x=subscribers, y=`video views`, color = category)) + 
  geom_point(size = 5)

#12---------Visualizing the relation between earnings and subscribers
ggplot(data=Global_YouTube_Statistics %>% na.omit(), mapping = aes(x=subscribers, y=highest_yearly_earnings, color = category)) + 
  geom_point(size = 5)




# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)


# Load Dataset
data <- read.csv("~/Documents/GitHub/SCV_project4/MoviesOnStreamingPlatforms_updated.csv")

# Info about data
summary(data)
str(data)

# Research question: What makes a movie popular depending on the platform?

# Create a table with count values by streaming platform
total_by_platform <- data %>%
  summarise(Netflix = sum(Netflix),
            Prime = sum(Prime.Video),
            Hulu = sum(Hulu),
            Disney = sum(Disney.)
            )
# Transpose the dataframe
total_by_platform <- as.data.frame(t(total_by_platform))
total_by_platform$names <- rownames(total_by_platform)

total_by_platform$names[total_by_platform$names == "Prime"] <-"Prime Video"
total_by_platform$names[total_by_platform$names == "Disney"] <-"Disney+"
# Get the positions
total_by_platform2 <- total_by_platform %>% 
  mutate(csum = rev(cumsum(rev(V1))), 
         pos = V1/2 + lead(csum, 1),
         pos = if_else(is.na(pos), V1/2, pos))

# Pie chart of the proportions of the moviesin the dataset
ggplot(total_by_platform, aes(x = "" , y = V1, fill = fct_inorder(names))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = total_by_platform2,
                   aes(y = pos, label = paste0(V1)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Streaming platform")) +
  theme_void()


# Rating according to the streaming platforms 
# First, levels should be numeric values
data$Rotten.Tomatoes <- as.character(data$Rotten.Tomatoes)
data$IMDb <- as.character(data$IMDb)

data$RT_num <- readr::parse_number(data$Rotten.Tomatoes)
data$IMDb_num <- readr::parse_number(data$IMDb)


# Distribution of the rating for RT  
ggplot(data = subset(data, !is.na(RT_num)), aes(x=RT_num))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 50)


# Distribution of the rating for IMDb 
ggplot(data = subset(data, !is.na(IMDb_num)), aes(x=IMDb_num))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 20)

# Compare the distrubutions by the platforms
RT_Netflix <- data %>%
  filter(Netflix == 1) %>%
  select(RT_num)
RT_Netflix$Platform <- "Netflix"

RT_Prime <- data %>%
  filter(Prime.Video == 1) %>%
  select(RT_num)
RT_Prime$Platform <- "Prime"

RT_Hulu <- data %>%
  filter(Hulu == 1) %>%
  select(RT_num)
RT_Hulu$Platform <- "Hulu"

RT_Disney <- data %>%
  filter(Disney. == 1) %>%
  select(RT_num)
RT_Disney$Platform <- "Disney"

# Dataset
RT_by_platform <- rbind(rbind(RT_Netflix, RT_Prime), rbind(RT_Hulu, RT_Disney))

RT_by_platform <- RT_by_platform %>% 
  drop_na(RT_num) # Drop na's


# Boxplot by categories
ggplot(RT_by_platform, aes(x= Platform, y=RT_num)) + 
  geom_boxplot()


# We can do the same but with the IMDb rating, we can compare 2 ratings by platform in one plot
# Since the scale is different, we should rescale IMDb


IMDb_Netflix <- data %>%
  filter(Netflix == 1) %>%
  select(IMDb_num) %>%
  mutate(IMDb_rescaled = IMDb_num * 10) 
IMDb_Netflix <- select(IMDb_Netflix,-c(IMDb_num))
IMDb_Netflix$Platform <- "Netflix"

IMDb_Prime <- data %>%
  filter(Prime.Video == 1) %>%
  select(IMDb_num) %>%
  mutate(IMDb_rescaled = IMDb_num * 10) 
IMDb_Prime <- select(IMDb_Prime,-c(IMDb_num))
IMDb_Prime$Platform <- "Prime"


IMDb_Hulu <- data %>%
  filter(Hulu == 1) %>%
  select(IMDb_num) %>%
  mutate(IMDb_rescaled = IMDb_num * 10) 
IMDb_Hulu <- select(IMDb_Hulu,-c(IMDb_num))
IMDb_Hulu$Platform <- "Hulu"


IMDb_Disney <- data %>%
  filter(Disney. == 1) %>%
  select(IMDb_num) %>%
  mutate(IMDb_rescaled = IMDb_num * 10) 
IMDb_Disney <- select(IMDb_Disney,-c(IMDb_num))
IMDb_Disney$Platform <- "Disney"

# Dataset
IMDb_by_platform <- rbind(rbind(IMDb_Netflix, IMDb_Prime), rbind(IMDb_Hulu, IMDb_Disney))

IMDb_by_platform <- IMDb_by_platform %>% 
  drop_na(IMDb_rescaled) # Drop na's


# Lets' combine RT and IMDb

RT_by_platform <-  RT_by_platform %>%
  rename(Rating = RT_num)
RT_by_platform$Review <- "Rotten Tomatoes"


IMDb_by_platform <-  IMDb_by_platform %>%
  rename(Rating = IMDb_rescaled)
IMDb_by_platform$Review <- "IMDb"

# Combine the 2
by_platform_review <- rbind(RT_by_platform, IMDb_by_platform)

by_platform_review$Platform[by_platform_review$Platform == "Prime"] <-"Prime Video"
by_platform_review$Platform[by_platform_review$Platform == "Disney"] <-"Disney+"

## GROUPED Boxplots
ggplot(by_platform_review, aes(x= Platform, y=Rating, fill = Review)) + 
  geom_boxplot()


## Time series by platform 
by_year <- data %>%
  count(Year)

# By platform
by_year_Netflix <- subset(data, Netflix == 1) %>%
  select(Year)
by_year_Netflix <- by_year_Netflix %>%
  count(Year)
by_year_Netflix$Platform <- "Netflix"
  
by_year_Prime <- subset(data, Prime.Video == 1) %>%
  select(Year)
by_year_Prime <- by_year_Prime %>%
  count(Year)
by_year_Prime$Platform <- "Prime"  

by_year_Hulu <- subset(data, Hulu == 1) %>%
  select(Year)
by_year_Hulu <- by_year_Hulu %>%
  count(Year)
by_year_Hulu$Platform <- "Hulu"  

by_year_Disney <- subset(data, Disney. == 1) %>%
  select(Year)
by_year_Disney <- by_year_Disney %>%
  count(Year)
by_year_Disney$Platform <- "Disney"  


by_year_platform <- rbind(rbind(by_year_Netflix, by_year_Prime), rbind(by_year_Hulu,by_year_Disney))

by_year_platform$Platform[by_year_platform$Platform == "Prime"] <-"Prime Video"
by_year_platform$Platform[by_year_platform$Platform == "Disney"] <-"Disney+"


## Plot of time series
ggplot(by_year_platform, aes(x = Year, y = n)) + 
  geom_line(aes(color = Platform), size = 1)


# Correlation between the 2 rating platforms
ggplot(data , aes(x = RT_num, y = IMDb_num), alpha = 0.5, size = 3) + geom_point() + geom_smooth(method = "lm") 

# Analysis of by year and rating
ggplot(data , aes(x = Year, y = IMDb_num)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm") 

ggplot(data, aes(x = Year, y = RT_num)) + geom_point() + geom_smooth(method = "lm") 





summary(subset(data, Netflix = 1)$Genres)


library(stringr)

data$genres_categ <- map(strsplit(as.character(data$Genres), split=','),1)

data$genres_categ <- as.factor(as.character(data$genres_categ))


by_year_rating <- data %>%
  group_by(platform)  %>%
  count(genres_categ)

by_year_rating$platform  <- as.character(by_year_rating$platform)

by_year_rating<-subset(by_year_rating , genres_categ !="NULL")

by_year_rating$platform[by_year_rating$platform == "Prime.Video"] <-"Prime Video"
by_year_rating$platform[by_year_rating$platform == "Disney"] <-"Disney+"

ggplot(by_year_rating, aes(x =n, y = reorder(genres_categ, n), fill = platform)) + 
  geom_bar(stat="identity", color='skyblue')


# Would be interesting to discover the popularity by genres ?
ggplot(data, aes(x = Year, y = RT_num, color = genres_categ)) + geom_point( alpha = 0.1, size = 3) 
# too many categories, let's keep the top 10 most common


less_genres_data <- subset(data, genres_categ == "Comedy" | genres_categ == "Drama"|
                             genres_categ == "Action"|genres_categ == "Documentary"|
                             genres_categ=="Animation"| genres_categ=="Crime" | 
                             genres_categ == "Adventure"| genres_categ == "Horror"|
                             genres_categ  == "Biography")

ggplot(less_genres_data, aes(x = Year, y = RT_num, color = genres_categ)) + geom_point( alpha = 0.3, size = 1) 

ggplot(less_genres_data, aes(x = Year, y = IMDb_num, color = genres_categ)) + geom_point( alpha = 0.3, size = 1) 

## By platform
ggplot(subset(less_genres_data, Netflix == 1), aes(x = Year, y = RT_num, color = genres_categ)) + geom_point( alpha = 0.3, size = 1) 
ggplot(subset(less_genres_data, Hulu == 1), aes(x = Year, y = RT_num, color = genres_categ)) + geom_point( alpha = 0.3, size = 1) 
ggplot(subset(less_genres_data, Prime.Video == 1), aes(x = Year, y = RT_num, color = genres_categ)) + geom_point( alpha = 0.3, size = 1) 
ggplot(subset(less_genres_data, Disney. == 1), aes(x = Year, y = RT_num, color = genres_categ)) + geom_point( alpha = 0.3, size = 1) 


# Now let's see only the top rated movies
require("ggrepel")

top_rated <- subset(less_genres_data, RT_num > 90)
rownames(top_rated) <- top_rated$Title

top_rated$platform<- as.character(top_rated$platform)

##   
ggplot(top_rated, aes(x = Year, y = RT_num, color = platform)) + geom_point( alpha = 0.5, size = 1) + 
  geom_text_repel(aes(label = rownames(top_rated)),
                  size = 2)
top_rated$platform<- as.character(top_rated$platform)
top_rated$platform[top_rated$platform == "Prime.Video"] <-"Prime Video"
top_rated$platform[top_rated$platform == "Disney"] <-"Disney+"

### FFAIREPAR PLATEFORME!!!!!!!!

# Best rated movies by countries
ggplot(subset(data, RT_num > 90), aes(x = Year, y = RT_num)) + geom_point( alpha = 0.5, size = 1) + 
  geom_text_repel(aes(label = Country),
                  size = 2)

# Best movies and directors names
ggplot(subset(data, RT_num > 90), aes(x = Year, y = RT_num)) + geom_point( alpha = 0.5, size = 1) + 
  geom_text_repel(aes(label = Directors),
                  size = 2)

# Best movies and titles
ggplot(subset(data, RT_num > 90), aes(x = Year, y = RT_num)) + geom_point( alpha = 0.5, size = 1) + 
  geom_text_repel(aes(label = Title),
                  size = 2)


# Maybe interesting to see by the stremaning platform
data$platform <- as.factor(ifelse(data$Netflix == 1, 'Netflix',
                                 ifelse(data$Prime.Video == 1, 'Prime.Video', 
                                        ifelse(data$Hulu == 1, 'Hulu', 
                                               ifelse(data$Disney. ==1 , "Disney",0)))))


ggplot(subset(data, RT_num > 90), aes(x = Year, y = RT_num, color = platform)) + geom_point( alpha = 0.5, size = 1) + 
  geom_text_repel(aes(label = Title),
                  size = 2)

ggplot(data, aes(x = Year, y = RT_num, color = platform)) + geom_point( alpha = 0.3, size = 1)


### Common Countries
summary(data$Country)


data$main_country <- map(strsplit(as.character(data$Country), split=','),1)
data$main_country <- as.factor(as.character(data$main_country))


top_country <- data %>%
  group_by(platform)%>%
  count(main_country)

top_country <- top_country %>% 
  arrange(desc(n))

top_country <- head(top_country, 20)

ggplot(top_country, aes(x =n ,y = reorder(main_country, -n))) + geom_bar(stat="identity", color='skyblue',fill='steelblue')
### FAIRE PAR PLATEFORME
### Is there dependence between the length of a movie and its rating?
ggplot(data, aes(x = Runtime, y = RT_num)) + geom_point( alpha = 0.5, size = 1) 

### Did the runtime change over time ?

ggplot(data,aes(Year, Runtime)) +
  stat_summary(geom = "line", fun.y = mean) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3)

# Average ratings over time
ggplot(data,aes(Year, RT_num, color = platform)) +
  stat_summary(geom = "line", fun.y = mean) #+
  #stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3)

ggplot(data,aes(Year, IMDb_num)) +
  stat_summary(geom = "line", fun.y = mean) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3)

## Analysis of the age groups
age_groups <- data %>% 
  group_by(Age) %>%
  summarise(freq = length(Age))


ggplot(subset(age_groups, Age!=""), aes(x=Age, y=freq)) +
  geom_bar(stat="identity", fill="blue")
## PAR PLATEFORME

## Analysis of the top movies
top_by_RT <- data %>%                                     
  arrange(desc(RT_num))  

top_by_RT <- top_by_RT[1:20,]

top_by_RT$Title[top_by_RT$Title == "Jim & Andy: The Great Beyond- Featuring a Very Special, Contractually Obligated Mention of Tony Clifton"] <- "Jim & Andy: The Great Beyond"

ggplot(top_by_RT, aes(x =RT_num ,y = reorder(Title, -RT_num))) + geom_bar(stat="identity", color='skyblue',fill='steelblue')
## What is the platform to choose based on the ratings ?
## https://www.businessinsider.com/streaming-comparison-netflix-hulu-disney-plus-hbo-max-prime-2020-6#prime-video-also-has-the-most-tv-shows-but-netflix-isnt-far-behind-5




### Analysis of thebest rated by platform
ggplot(subset(data, RT_num > 80), aes(x = Year, y = RT_num, color =platform)) + geom_point( alpha = 0.5, size = 1) 


# Let's categorize the quality to determine the 

data$RT_quality_categ <- as.factor(ifelse(data$RT_num >= 80, 'Excellent',
                                  ifelse(data$RT_num >= 60 , 'Good', 
                                         ifelse(data$RT_num >= 40 , 'Descent', 
                                                ifelse(data$RT_num >=20 , "Bad", "Terrible")))))
quality_composition <- data%>%
  group_by(platform) %>%
  count(RT_quality_categ)
  
quality_composition <- quality_composition %>% 
  drop_na(RT_quality_categ)
# Reorder factor levels 
levels(quality_composition$RT_quality_categ) <- ordered(c("Excellent", "Good", "Descent", "Bad", "Terrible"))


# Raw
ggplot(quality_composition, aes(fill=RT_quality_categ, y=platform, x=n)) + 
  geom_bar(stat="identity")

#Percentage 
ggplot(quality_composition, aes(fill=RT_quality_categ, y=platform, x=n)) + 
  geom_bar(position="fill", stat="identity")

high_quality_composition <-as.data.frame(table(subset(data,IMDb_num>6)$platform))



# Most genres per platform of best rated movies


# Compareto theprice



top <-subset(data, RT_num > 80 )

genres_composition <- top%>%
  group_by(platform) %>%
  count(genres_categ)


age_composition <- top%>%
  group_by(platform) %>%
  count(Age)

age_composition$Age<- as.factor(age_composition$Age)
age_composition <- subset(age_composition, Age !=  "")
levels(age_composition$Age)

levels(age_composition$Age) <- ordered(c("all","7+", "13+", "16+", "18+", ""))









genres_composition$platform<- as.character(genres_composition$platform)
genres_composition$platform[genres_composition$platform == "Prime.Video"] <-"Prime Video"
genres_composition$platform[genres_composition$platform == "Disney"] <-"Disney+"



age_composition$platform<- as.character(age_composition$platform)
age_composition$platform[age_composition$platform == "Prime.Video"] <-"Prime Video"
age_composition$platform[age_composition$platform == "Disney"] <-"Disney+"


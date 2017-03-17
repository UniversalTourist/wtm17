# WomenTechMakers'17 Istanbul, March 19
# Creating Elegant Graphs with R Programming
# Hazel Kavılı, R-Ladies Istanbul

# 1
A <- 10 
a <- 3
print(paste("A is", A))
print(paste("a is", a))
cat("A and a are equal? = ", A == a)

myNumbers <- c(1:10)
rep(myNumbers, times = 3)
twice <- rep(myNumbers, each = 2)

ls()
rm(a)
print(twice)

# 2
x <- c(1:15)
seq(from = 2, to = 100, by = 2) -> y

sum(x)
min(x) 
max(x)
mean(x)
var(x)
sqrt(x)
sd(x)
length(x)


# 3
library(tidyverse)
library(tufte)

# Read data file
movies <- read.csv("movies.csv",header=TRUE,sep=",")
str(movies)
head(movies)
tail(movies)
dim(movies)
unique(movies$country)
summary(movies)

# dplyr functions
gainORlost <- movies %>% mutate(difference = gross - budget)
head(gainOrlost)
byBudget <- movies %>%  arrange(budget)
head(byBudget)

myData <- movies %>% select(country, title_year, imdb_score) %>% 
  filter (imdb_score >= 6 & country == "UK") 

myDataImdb <- movies %>% select(country, title_year, imdb_score) %>% 
  filter(country == "UK" | country == "USA" | country == "Canada" | country == "Germany")


myDataMean <- movies %>% select(country, imdb_score, title_year) %>% 
  filter(country == "UK" | country == "USA") %>% 
  group_by(country)  %>%  summarise(scoreMean = mean(imdb_score))

top10genres <- movies %>% select(genres)  %>% count(genres) %>% top_n(n = 10, wt = n)

# ggplot  
ggplot(data = myDataImdb) + 
  geom_point(mapping = aes(x = title_year, y = imdb_score, color = country))

ggplot(data = myDataImdb, aes(x = title_year, y = imdb_score)) + 
  geom_point(aes(color = country))

# geom_bar
genresDisplay <- ggplot(data = genres) +
  geom_bar(aes(x = reorder(genres,n), y =n, fill = genres),
           stat = "identity") +
  labs(y = "Frequencies", x = "Genre") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()

# geom_point + geom_smooth 
budgetsONscore <- ggplot(movies, aes(x = budget, y = imdb_score)) + 
  geom_point(shape = 1) + geom_smooth(method = lm, se = FALSE) +
  labs(x = "Budget",
       y = "IMDB Scores",
       title = "The effect of Budget on IMDB scores") +
  theme(legend.position = "top",
        plot.title = element_text(hjust=0.5))   


# IMDB scores of my favourite actresses in years

womenData <- movies %>% select(actor_1_name, imdb_score, genres, title_year) %>% 
  filter(actor_1_name == "Emma Watson" | actor_1_name == "Emma Stone" | actor_1_name == "Anne Hathaway" |
           actor_1_name == "Salma Hayek" | actor_1_name == "Charlize Theron") 

womenDataPlot <- ggplot(data = womenData) + 
  geom_point(aes(x = title_year, y = imdb_score, color = genres,
                 shape = actor_1_name), size = 2) +
  labs(x = "Years",
       y = "IMDB Scores",
       title = "IMDB Scores of Actress in Years",
       color = "Genre",
       shape = "Actresses") +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  guides(shape = guide_legend(nrow = 2))
  


# tufte style
library(ggplot2)
library(ggthemes)
x <- 1967:1977
y <- c(0.5,1.8,4.6,5.3,5.3,5.7,5.4,5,5.5,6,5)
d <- data.frame(x, y)
ggplot(d, aes(x,y)) + geom_line() + geom_point(size=3) + theme_tufte(base_size = 15) +
  theme(axis.title=element_blank()) + geom_hline(yintercept = c(5,6), lty=2) + 
  scale_y_continuous(breaks=seq(1, 6, 1), label=sprintf("$%s",seq(300,400,20))) + 
  scale_x_continuous(breaks=x,label=x) +
  annotate("text", x = c(1977,1977.2), y = c(1.5,5.5), adj=1,  family="serif",
           label = c("Per capita\nbudget expandures\nin constant dollars", "5%"))


ggplot(myDataImdb, aes(x = title_year, y = imdb_score)) + 
  geom_point(size = 2, aes(color = country)) + 
  theme_tufte(base_family = "serif", base_size = 10) 





library(dplyr)
library(ggplot2)
genres <- movies %>% select(genres)  %>% count(genres) %>% top_n(n = 10, wt = n)

genresDisplay <- ggplot(data = genres) +
  geom_bar(aes(x = reorder(genres,n), y =n, fill = genres),
           stat = "identity") +
  labs(y = "Frequencies", x = "Genre") +
  theme_bw()




































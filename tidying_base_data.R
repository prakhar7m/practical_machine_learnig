#import tidyverse
library(tidyverse)
#load the dataset
news_orig = read.csv("fake", na.strings = "")
#introduce an ID
news$ID = seq_len(len(news_orig))
#bring to left
news_orig = select(news, ID, everything())


#get an idea of dist. of dependent variable
table(news_orig$type)

#find out which variables have NAs and how many per column
sum(is.na(news$author))
sum(is.na(news$domain_rank))
sum(is.na(news$thread_title))
sum(is.na(news$title))
sum(is.na(news$country))
sum(is.na(news$main_img_url))

#author, domain_rank, thread_title, title, country, main_img_url have large nos. of NAs

#filter out these variables
news_no_nas = filter(news, !(is.na(news$domain_rank) | is.na(news$author) | is.na(news$thread_title) | 
                              is.na(news$title) | is.na(news$country)))
#drop the main_img_url, uuid variable as we won't be using it
news_clean = news_no_nas  %>% select(-main_img_url)
news_clean = news_clean %>% select(-uuid)

#get an idea of distribution of dep. var. after removing NAs
table(clean_news$type)

#some visualization
#a very heavily right-skewed distribution of likes.
news_clean %>% ggplot(aes(x = likes)) + geom_histogram(binwidth = 100)

#pratically all the observations have comments close to 0.
news_clean %>% ggplot(aes(x = comments)) + geom_histogram(binwidth = 10)

#a pretty skewed distribution of shares
news_clean %>% ggplot(aes(x = shares)) + geom_histogram(binwidth = 90)

# a good, healthy distribution of domain_ranks
news_clean %>% ggplot(aes(x = domain_rank)) + geom_histogram(binwidth = 10000, fill = "blue", color = "yellow")

#a good distribution of authors
#number of unique authors
length(unique(news_clean$author))
#draw a plot with top 50 (to be cont.)
news_clean %>% ggplot(aes(x = author)) + geom_bar()

#a good distribution of countries (US is overrepresented.) 
news_clean %>% ggplot(aes(x = country, color = country)) + geom_bar()

# a quick numerical summary 
summary(news_clean %>% select(-title, -text, -thread_title, -author, -crawled, -language, -site_url, -ID, -ord_in_thread,-published))

#replies_count, participant_counts, likes, comments, shares are quite right skewed (as expected.)


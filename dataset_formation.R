set.seed(1008)
library(tidyverse)
library(GGally)
library(stringr)
library(caTools)
#load the data
base_news = read.csv("base_news.csv", stringsAsFactors = TRUE)
fox_news = read.csv("fox_articles.csv", stringsAsFactors = TRUE)
cnn_news = read.csv("cnn_articles.csv", stringsAsFactors = TRUE)
nyt_news1 = read.csv("NYT_articles (2015-2016).csv", stringsAsFactors = TRUE)
nyt_news2 = read.csv("NYT_articles (2016-2017).csv", stringsAsFactors = TRUE)
alexa = read.csv("alexa_webstats.csv", stringsAsFactors = TRUE)
socialmedia = read.csv("socialmedia_data.csv", stringsAsFactors = TRUE)

#remove bogus extra columns
fox_news = fox_news %>% select(-X)
fox_news = fox_news %>% select(-X)
alexa = alexa %>% select(-X, -X.1, -X.2, -top4_country, -top5_country)
alexa = alexa %>% select(-website_country)

#bind the base_news, cnn_news, fox_news, nyt_news1, nyt_news2 rows together
component1 = bind_rows(base_news, cnn_news, fox_news, nyt_news1, nyt_news2)

#remove the "state" and "fake" observations because they are too rare. 
component1  = component1 %>% filter(!(type == "state" | type == "fake"))

#left join component1 with alexa and socialmedia to create the complete dataset
data = component1 %>% left_join(alexa, by = "site_url") %>% left_join(socialmedia, by = "site_url")

#reorder columns to get type on the right
data = data %>% select(site_url, author, headline, language, country, global_rank, US_rank, top1_country, top2_country, top3_country, bounce_rate, daily_pageviews_per_visitor, daily_time_on_site, percentage_search_visits, num_sites_linking_in, gender, educational_level, browsing_location, stumble_upon, pinterest, linkedin, fb_total, fb_comments, fb_reactions, fb_shares, type)

#convert headline to lowercase
data$headline = tolower(data$headline)

#"bs" type dominates the dataset (~75%). we reduce its presence, by randomly sampling ~1300 "BS" rows.
# we now have 3448 records and this is the dataset we'll use.
reduced_data = bind_rows(filter(data, type != "bs"), sample_n(data %>% filter(type == "bs"), 1300))


#convert type of columns to appropriate type if necessary
reduced_data$type = factor(reduced_data$type)
reduced_data$site_url = factor(reduced_data$site_url)
reduced_data$author = factor(reduced_data$author)
reduced_data$language = factor(reduced_data$language)
reduced_data$country = factor(reduced_data$country)
reduced_data$global_rank = numeric(reduced_data$global_rank)
reduced_data$US_rank = numeric(reduced_data$US_rank)
reduced_data$bounce_rate = numeric(reduced_data$bounce_rate)
reduced_data$daily_time_on_site = numeric(reduced_data$daily_time_on_site)
reduced_data$percentage_search_visits = numeric(reduced_data$percentage_search_visits)
reduced_data$num_sites_linking_in = numeric(reduced_data$num_sites_linking_in)


#original counts of data
table(data$type)

#final counts of data
table(reduced_data$type)
#removing all known 'special punctuation' that won't be removed by tm's removePunctuation method
#which only uses ASCII/Unicode General Punctuation
reduced_data$headline = gsub("???","", reduced_data$headline)
reduced_data$headline = gsub("â","", reduced_data$headline)
reduced_data$headline = gsub("T","", reduced_data$headline)
reduced_data$headline = gsub("Â","", reduced_data$headline)
reduced_data$headline = gsub("»","", reduced_data$headline)
reduced_data$headline = gsub("o","", reduced_data$headline)
reduced_data$headline = gsub("100percentfedUp.com","", reduced_data$headline)
reduced_data$headline = gsub("*", "", reduced_data$headline)
reduced_data$headline = gsub("¦","", reduced_data$headline)
reduced_data$headline = gsub("!", "", reduced_data$headline)
reduced_data$headline = gsub("#", "", reduced_data$headline)

#introducing an ID column
reduced_data$ID = seq_len(3448)
reduced_data = select(reduced_data, ID, everything())

#introduce nlp word indicator variables/columns
reduced_data$gop_nlp = str_detect(reduced_data$headline, "gop") %>% as.numeric()
reduced_data$email_nlp = str_detect(reduced_data$headline, "email") %>% as.numeric()
reduced_data$comei_nlp = str_detect(reduced_data$headline, "comei") %>% as.numeric() 
reduced_data$reveal_nlp = str_detect(reduced_data$headline, "reveal") %>% as.numeric()
reduced_data$comment_nlp = str_detect(reduced_data$headline, "comment") %>% as.numeric()
reduced_data$soro_nlp = str_detect(reduced_data$headline, "soro") %>% as.numeric()
reduced_data$acknowledg_nlp = str_detect(reduced_data$headline, "acknowledg") %>% as.numeric()
reduced_data$emf_nlp = str_detect(reduced_data$headline, "emf") %>% as.numeric()
reduced_data$rig_nlp = str_detect(reduced_data$headline, "rig") %>% as.numeric()
reduced_data$jihad_nlp = str_detect(reduced_data$headline, "jihad") %>% as.numeric()
reduced_data$femin_nlp = str_detect(reduced_data$headline, "femin") %>% as.numeric()
reduced_data$hillary_nlp = str_detect(reduced_data$headline, "hillary") %>% as.numeric()
reduced_data$neomasculin_nlp = str_detect(reduced_data$headline, "neomasculin") %>% as.numeric()
reduced_data$natur_nlp = str_detect(reduced_data$headline, "natur") %>% as.numeric()
reduced_data$weight_nlp = str_detect(reduced_data$headline, "weight") %>% as.numeric()
reduced_data$gmo_nlp = str_detect(reduced_data$headline, "gmo") %>% as.numeric()
reduced_data$trump_nlp = str_detect(reduced_data$headline, "trump") %>% as.numeric()
reduced_data$brief_nlp = str_detect(reduced_data$headline, "brief") %>% as.numeric()
reduced_data$roi_nlp = str_detect(reduced_data$headline, "roi") %>% as.numeric()
reduced_data$tax_nlp = str_detect(reduced_data$headline, "tax") %>% as.numeric()

#now split into training/test
split = sample.split(reduced_data$type, SplitRatio = 0.7)
train = filter(reduced_data, split == TRUE)
test = filter(reduced_data, split == FALSE)





#select specific nlp, socialmedia, webstats columns to form training, test data for underlying models
socialmedia.train = train %>% select(ID, site_url, stumble_upon, pinterest, linkedin, fb_total, fb_comments, fb_reactions, fb_shares, type)
socialmedia.test = test %>% select(ID, site_url, stumble_upon, pinterest, linkedin, fb_total, fb_comments, fb_reactions, fb_shares, type)
webstats.train = train %>% select(ID, site_url, author, language, country, global_rank, US_rank, top1_country, top2_country, top3_country, bounce_rate, daily_pageviews_per_visitor, daily_time_on_site, percentage_search_visits, num_sites_linking_in, gender, educational_level, browsing_location, type)
webstats.test = test %>% select(ID, site_url, author, language, country, global_rank, US_rank, top1_country, top2_country, top3_country, bounce_rate, daily_pageviews_per_visitor, daily_time_on_site, percentage_search_visits, num_sites_linking_in, gender, educational_level, browsing_location, type)
nlp.train = train %>% select(ID, site_url, headline, sentiment, gop_nlp, email_nlp, comei_nlp, reveal_nlp, comment_nlp, soro_nlp, acknowledg_nlp, emf_nlp, rig_nlp, jihad_nlp, femin_nlp, hillary_nlp, neomasculin_nlp, natur_nlp, weight_nlp, gmo_nlp, trump_nlp, brief_nlp, roi_nlp, tax_nlp,type)
nlp.test = test %>% select(ID, site_url, headline, sentiment, gop_nlp, email_nlp, comei_nlp, reveal_nlp, comment_nlp, soro_nlp, acknowledg_nlp, emf_nlp, rig_nlp, jihad_nlp, femin_nlp, hillary_nlp, neomasculin_nlp, natur_nlp, weight_nlp, gmo_nlp, trump_nlp, brief_nlp, roi_nlp, tax_nlp,type)



                         
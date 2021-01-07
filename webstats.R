library(randomForest)
library(rpart)
library(MASS)
library(e1071)

webstats.train$type = factor(webstats.train$type)
webstats.test$type = factor(webstats.test$type)

#accuracy:
#CART: 0.998
#rf: 1
#svm: 1
#lda: n/a

#n
cart_ws_model = rpart(type ~ language + country + global_rank + US_rank + top1_country + top2_country + top3_country + bounce_rate + daily_pageviews_per_visitor + daily_time_on_site + percentage_search_visits + num_sites_linking_in + gender + educational_level + browsing_location, data = webstats.train, method = "class", minbucket = 5, cp = 0.001)
rf_ws_model = randomForest(type ~ language + country + global_rank + US_rank + top1_country + top2_country + top3_country + bounce_rate + daily_pageviews_per_visitor + daily_time_on_site + percentage_search_visits + num_sites_linking_in + gender + educational_level + browsing_location, data = webstats.train, mtry = 5, nodesize = 5, ntree = 500)
svm_ws_model = svm(type ~ language + country + global_rank + US_rank + top1_country + top2_country + top3_country + bounce_rate + daily_pageviews_per_visitor + daily_time_on_site + percentage_search_visits + num_sites_linking_in + gender + educational_level + browsing_location, data = webstats.train, cost = 100, gamma = 1)

#predict
rf_ws_pred = predict(rf_ws_model, newdata = webstats.test)
cart_ws_pred = predict(cart_ws_model, newdata = webstats.test, type = "class")
svm_ws_pred = predict(svm_ws_model, newdata = webstats.test)


#ACCURACY
table(webstats.test$type, cart_ws_pred)
table(webstats.test$type, rf_ws_pred)
table(webstats.test$type, svm_ws_pred)




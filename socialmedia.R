library(randomForest)
library(rpart)
library(MASS)
library(e1071)
socialmedia.train$type = factor(socialmedia.train$type)
socialmedia.test$type = factor(socialmedia.test$type)

cart_sm_model = rpart(type ~ stumble_upon + pinterest + linkedin + fb_total + fb_comments + fb_reactions + fb_shares, data = socialmedia.train, method = "class", minbucket = 5, cp = 0.001)
rf_sm_model = randomForest(type ~ stumble_upon + pinterest + linkedin + fb_total + fb_comments + fb_reactions + fb_shares, data = socialmedia.train, mtry = 5, nodesize = 5, ntree = 500)
svm_sm_model = svm(type ~ stumble_upon + pinterest + linkedin + fb_total + fb_comments + fb_reactions + fb_shares, data = socialmedia.train, cost = 100, gamma = 1)
lda_sm_model = lda(type ~ stumble_upon + pinterest + linkedin + fb_total + fb_comments + fb_reactions + fb_shares, data = socialmedia.train)


cart_sm_pred = predict(cart_sm_model, newdata = socialmedia.test, type = "class")
rf_sm_pred = predict(rf_sm_model, newdata = socialmedia.test)
svm_sm_pred = predict(svm_sm_model, newdata = socialmedia.test)
lda_sm_pred = predict(lda_sm_model, newdata = socialmedia.test)


#accuracy of cart model: 1
#accuracy of rf model: 1
#accuracy of svm model: 0.882
#accuracy of lda model: 0.799
table(socialmedia.test$type, cart_sm_pred)
table(socialmedia.test$type, rf_sm_pred)
table(socialmedia.test$type, svm_sm_pred)
table(socialmedia.test$type, lda_sm_pred$class)


#using the following models:
#socialmedia: gbm(n.trees = 100)
#webstats: randomForest()
#nlp: glm (multinomial logistic)


#importing h2o
library(h2o)

#initializing h2o
h2o.init()

#putting our training, testing data into h2o frames
as.h2o(train, "train_h2o")
test_h2o = as.h2o(test, 'test_h2o')

#defining the underlying models
my_gbm = h2o.gbm(c("stumble_upon", "pinterest", "linkedin", "fb_total", "fb_comments", "fb_reactions", "fb_shares"), "type", training_frame = train_h2o,ntrees = 100, max_depth = 3, min_rows = 10, learn_rate = 0.001, nfolds = 5, fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE, seed = 108)
my_rf = h2o.randomForest(c("language", "country", "global_rank", "US_rank", "top1_country", "top2_country", "top3_country", "bounce_rate", "daily_pageviews_per_visitor", "daily_time_on_site", "percentage_search_visits", "num_sites_linking_in", "gender", "educational_level", "browsing_location"), "type", training_frame = train_h2o, ntrees = 500, nfolds = 5, fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE, seed = 108)
my_glm = h2o.glm(c("sentiment", "gop_nlp", "comei_nlp", "reveal_nlp", "comment_nlp", "soro_nlp", "acknowledg_nlp", "emf_nlp", "rig_nlp", "jihad_nlp","femin_nlp", "hillary_nlp", "neomasculin_nlp", "natur_nlp", "weight_nlp", "gmo_nlp","trump_nlp", "brief_nlp", "roi_nlp", "tax_nlp"), "type", training_frame = train_h2o, nfolds = 5, seed = 108, fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE, family = "multinomial")

#defining the stacked ensemble model
my_ensemble = h2o.stackedEnsemble(c("language", "country", "global_rank", "US_rank", "top1_country", "top2_country", "top3_country", "bounce_rate", "daily_pageviews_per_visitor", "daily_time_on_site", "percentage_search_visits", "num_sites_linking_in", "gender", "educational_level", "browsing_location", "stumble_upon","pinterest", "linkedin", "fb_total", "fb_comments", "fb_reactions", "fb_shares", "sentiment", "gop_nlp", "email_nlp", "reveal_nlp", "comment_nlp", "soro_nlp", "acknowledg_nlp", "emf_nlp", "rig_nlp", "jihad_nlp", "femin_nlp", "hillary_nlp", "neomasculin_nlp", "natur_nlp", "weight_nlp", "gmo_nlp", "brief_nlp", "roi_nlp", "tax_nlp"), "type", training_frame = train_h2o, base_models = list(my_glm, my_rf, my_gbm))

#make some predictions
pred = h2o.predict(my_ensemble, newdata = test_h2o)

#convert the h2o data frame to r dataframe
ensemble_pred_r = as.data.frame(pred)

#get final accuracies
table(test$type, ensemble_pred_r$predict)

#compare ensemble performance to underlying learner performance

#ensemble performance
perf_ensemble_test <- h2o.performance(my_ensemble, newdata = test_h2o)

#component model performance
perf_gbm_test <- h2o.performance(my_gbm, newdata = test_h2o)
perf_rf_test <- h2o.performance(my_rf, newdata = test_h2o)
perf_glm_test <- h2o.performance(my_glm, newdata = test_h2o)
baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test), h2o.auc(perf_glm_test))
ensemble_auc_test <- h2o.auc(perf_ensemble_test)

print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))


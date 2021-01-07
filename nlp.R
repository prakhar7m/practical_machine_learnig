library(tidytext)
library(stringr)
#convert headline to lowercase
nlp.train$headline = tolower(nlp.train$headline)
nlp.test$headline = tolower(nlp.test$headline)
#tokenize headline column and get in a tidytext format
tidyheadlines = nlp.train %>% unnest_tokens(word, headline)

#testheadlines =  nlp.test %>% unnest_tokens(word, headline) %>% inner_join(get_sentiments("afinn"), by = "word") %>% group_by(ID) %>% mutate(sentiment = sum(score))
#nlp.test = nlp.test %>% left_join(tidyheadlines, by = "ID")
#nlp.train = unique(nlp.train)
#nlp.train = nlp.train %>% replace_na(replace = list(sentiment = 0))

#sentiment-analysis
tidyheadlines = tidyheadlines %>% inner_join(get_sentiments("afinn"), by = "word")
tidyheadlines = tidyheadlines %>% group_by(ID) %>% mutate(sentiment = sum(score))


nlp.train = nlp.train %>% left_join(tidyheadlines, by = "ID")
nlp.train = unique(nlp.train)
nlp.train = nlp.train %>% replace_na(replace = list(sentiment = 0))
nlp.train = nlp.train %>% select(-site_url.y, -type.y, - word, -score)

#get characteristic words through tf-idf
#obtain a fresh tokenzation of training data
type_words = nlp.train %>% unnest_tokens(word, headline) %>% count(type, word)

#stem words 
type_words$word_stem = wordStem(type_words$word, language = "porter")

#obtain tf_idfs
type_words = count(type_words, ID, word_stem)
type_words = type_words %>% bind_tf_idf(word_stem, type, n)

#plot to see most characteristic words of each class
type_plot %>% filter(nchar(type_plot$word_stem) >= 3) %>% arrange(desc(tf_idf)) %>% group_by(type) %>% top_n(5, tf_idf) %>% ungroup() %>% ggplot(aes(word_stem, tf_idf, fill = type)) + geom_col(show.legend = FALSE) + labs(x = NULL, y = "tf-idf") + facet_wrap(~type, ncol = 3, scales = "free") + coord_flip()


#words selected: 
#bias: gop, email, comei
#bs: reveal, comment, soro
#conspiracy: acknowledg, emf, rig
#hate: jihad, femin, hillary, neomasculin
#junksci: natur, weight, gmo
#legitimate: trump, brief, roi, tax

#introduce variables into nlp.train indicate if a particular stemword is contained in headline
nlp.train$gop_nlp = str_detect(nlp.train$headline, "gop") %>% as.numeric()
nlp.train$email_nlp = str_detect(nlp.train$headline, "email") %>% as.numeric()
nlp.train$comei_nlp = str_detect(nlp.train$headline, "comei") %>% as.numeric() 
nlp.train$reveal_nlp = str_detect(nlp.train$headline, "reveal") %>% as.numeric()
nlp.train$comment_nlp = str_detect(nlp.train$headline, "comment") %>% as.numeric()
nlp.train$soro_nlp = str_detect(nlp.train$headline, "soro") %>% as.numeric()
nlp.train$acknowledg_nlp = str_detect(nlp.train$headline, "acknowledg") %>% as.numeric()
nlp.train$emf_nlp = str_detect(nlp.train$headline, "emf") %>% as.numeric()
nlp.train$rig_nlp = str_detect(nlp.train$headline, "rig") %>% as.numeric()
nlp.train$jihad_nlp = str_detect(nlp.train$headline, "jihad") %>% as.numeric()
nlp.train$femin_nlp = str_detect(nlp.train$headline, "femin") %>% as.numeric()
nlp.train$hillary_nlp = str_detect(nlp.train$headline, "hillary") %>% as.numeric()
nlp.train$neomasculin_nlp = str_detect(nlp.train$headline, "neomasculin") %>% as.numeric()
nlp.train$natur_nlp = str_detect(nlp.train$headline, "natur") %>% as.numeric()
nlp.train$weight_nlp = str_detect(nlp.train$headline, "weight") %>% as.numeric()
nlp.train$gmo_nlp = str_detect(nlp.train$headline, "gmo") %>% as.numeric()
nlp.train$trump_nlp = str_detect(nlp.train$headline, "trump") %>% as.numeric()
nlp.train$brief_nlp = str_detect(nlp.train$headline, "brief") %>% as.numeric()
nlp.train$roi_nlp = str_detect(nlp.train$headline, "roi") %>% as.numeric()
nlp.train$tax_nlp = str_detect(nlp.train$headline, "tax") %>% as.numeric()

#also introduce into nlp.test
nlp.test$gop_nlp = str_detect(nlp.test$headline, "gop") %>% as.numeric()
nlp.test$email_nlp = str_detect(nlp.test$headline, "email") %>% as.numeric()
nlp.test$comei_nlp = str_detect(nlp.test$headline, "comei") %>% as.numeric() 
nlp.test$reveal_nlp = str_detect(nlp.test$headline, "reveal") %>% as.numeric()
nlp.test$comment_nlp = str_detect(nlp.test$headline, "comment") %>% as.numeric()
nlp.test$soro_nlp = str_detect(nlp.test$headline, "soro") %>% as.numeric()
nlp.test$acknowledg_nlp = str_detect(nlp.test$headline, "acknowledg") %>% as.numeric()
nlp.test$emf_nlp = str_detect(nlp.test$headline, "emf") %>% as.numeric()
nlp.test$rig_nlp = str_detect(nlp.test$headline, "rig") %>% as.numeric()
nlp.test$jihad_nlp = str_detect(nlp.test$headline, "jihad") %>% as.numeric()
nlp.test$femin_nlp = str_detect(nlp.test$headline, "femin") %>% as.numeric()
nlp.test$hillary_nlp = str_detect(nlp.test$headline, "hillary") %>% as.numeric()
nlp.test$neomasculin_nlp = str_detect(nlp.test$headline, "neomasculin") %>% as.numeric()
nlp.test$natur_nlp = str_detect(nlp.test$headline, "natur") %>% as.numeric()
nlp.test$weight_nlp = str_detect(nlp.test$headline, "weight") %>% as.numeric()
nlp.test$gmo_nlp = str_detect(nlp.test$headline, "gmo") %>% as.numeric()
nlp.test$trump_nlp = str_detect(nlp.test$headline, "trump") %>% as.numeric()
nlp.test$brief_nlp = str_detect(nlp.test$headline, "brief") %>% as.numeric()
nlp.test$roi_nlp = str_detect(nlp.test$headline, "roi") %>% as.numeric()
nlp.test$tax_nlp = str_detect(nlp.test$headline, "tax") %>% as.numeric()

#try random forest, boosting, multinomial logistic, CART
#CART: untuned: 0.449. 
#random forest: 0.450
#svm model: 0.440
#lda model: 0.459
cart_nlp_model = rpart(type ~ sentiment + gop_nlp + email_nlp + comei_nlp + reveal_nlp + comment_nlp + soro_nlp + acknowledg_nlp + emf_nlp + rig_nlp + jihad_nlp + femin_nlp + hillary_nlp + neomasculin_nlp + natur_nlp + weight_nlp + gmo_nlp + trump_nlp + brief_nlp + roi_nlp + tax_nlp, data = nlp.train, method = "class", minbucket = 5, cp = 0.001)
rf_nlp_model = randomForest(type ~ sentiment + gop_nlp + email_nlp + comei_nlp + reveal_nlp + comment_nlp + soro_nlp + acknowledg_nlp + emf_nlp + rig_nlp + jihad_nlp + femin_nlp + hillary_nlp + neomasculin_nlp + natur_nlp + weight_nlp + gmo_nlp + trump_nlp + brief_nlp + roi_nlp + tax_nlp, data = nlp.train, mtry = 5, nodesize = 5, ntree = 500)
svm_nlp_model = svm(type ~ sentiment + gop_nlp + email_nlp + reveal_nlp + comment_nlp + soro_nlp + acknowledg_nlp + emf_nlp + rig_nlp + jihad_nlp + femin_nlp + hillary_nlp + neomasculin_nlp + natur_nlp + weight_nlp + gmo_nlp + trump_nlp + brief_nlp + roi_nlp + tax_nlp, data = nlp.train,cost = 100, gamma = 1)
lda_nlp_model = lda(type ~ sentiment + gop_nlp + email_nlp + reveal_nlp + comment_nlp + soro_nlp + acknowledg_nlp + emf_nlp + rig_nlp + jihad_nlp + femin_nlp + hillary_nlp + neomasculin_nlp + natur_nlp + weight_nlp + gmo_nlp + trump_nlp + brief_nlp + roi_nlp + tax_nlp, data = nlp.train)
nba_nlp_model = naiveBayes(type ~ sentiment + gop_nlp + email_nlp + reveal_nlp + comment_nlp + soro_nlp + acknowledg_nlp + emf_nlp + rig_nlp + jihad_nlp + femin_nlp + hillary_nlp + neomasculin_nlp + natur_nlp + weight_nlp + gmo_nlp + trump_nlp + brief_nlp + roi_nlp + tax_nlp, data = nlp.train)
log_nlp_model = multinom(type ~ sentiment + gop_nlp + email_nlp + reveal_nlp + comment_nlp + soro_nlp + acknowledg_nlp + emf_nlp + rig_nlp + jihad_nlp + femin_nlp + hillary_nlp + neomasculin_nlp + natur_nlp + weight_nlp + gmo_nlp + trump_nlp + brief_nlp + roi_nlp + tax_nlp, data = nlp.train)

cart_nlp_pred = predict(cart_nlp_model, newdata = nlp.test, type = "class")
rf_nlp_pred = predict(rf_nlp_model, newdata = nlp.test)
svm_nlp_pred = predict(svm_nlp_model, newdata = nlp.test)
lda_nlp_pred = predict(lda_nlp_model, newdata = nlp.test)
nba_nlp_pred = predict(nba_nlp_model, newdata = nlp.test)
log_nlp_pred = predict(log_nlp_model, newdata = nlp.test)

#get accuracy of cart model: 0.449 
table(nlp.test$type, cart_nlp_pred)
#get accuracy of rf model: 0.450
table(nlp.test$type, rf_nlp_pred)
#get accuracy of svm model: 0.448 
table(nlp.test$type, svm_nlp_pred)
#get accuracy of lda model: 0.459
table(nlp.test$type, lda_nlp_pred$class)
#get accuracy of nba model: 0.097
table(nlp.test$type,nba_nlp_pred)
#get accuracy of log model: 0.462
table(nlp.test$type, log_nlp_pred)






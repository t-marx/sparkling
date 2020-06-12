# Loading the required libraries

if(!require(pander)) install.packages("pander", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tm)) install.packages("tm", repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud", repos = "http://cran.us.r-project.org")
if(!require(text2vec)) install.packages("text2vec", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")

# This script is based on web-scraped data (cleaned); these data can be obtained
# either by executing the wine_scrap.R script (should take 8 to 12 hours in total to retrieve the data)
# or alternatively by executing the following set of commands

wineURL <- ("https://github.com/t-marx/sparkling/raw/master/wines.RDS")
wines <- readRDS(url(wineURL))

pander(summary(wines))

# Part I : Exploratory data analysis

# Plotting ratings for all wines

wines %>%
  ggplot(aes(x=points)) +
  geom_histogram(binwidth=1, color="black") +
  xlab("Rating points") +
  ylab("Number of wines") +
  ggtitle("Distribution of wines by rating")

# Plotting ratings for actual Champagnes only

wines %>%
  filter(appellation == 'Champagne') %>%
  ggplot(aes(x=points)) +
  geom_histogram(binwidth=1, color="black") +
  xlab("Rating points") +
  ylab("Number of Champagnes") +
  ggtitle("Distribution of Champagnes by rating")

# Plotting ratings for vintage Champagnes only

wines %>%
  filter(appellation == 'Champagne') %>%
  filter(!wine_year == 'None') %>%
  ggplot(aes(x=points)) +
  geom_histogram(binwidth=1, color="black") +
  xlab("Rating points") +
  ylab("Number of Champagnes") +
  ggtitle("Distribution of Vintage Champagnes by rating")

# Plotting the relationship between price and rating
wines %>%
  ggplot(aes(x=price,y=points)) +
  geom_point() +
  xlab("Price") +
  ylab("Points") +
  ggtitle("Relationship between price and rating")

# Plotting the relationship between price and rating (Champagnes only)

wines %>%
  filter(appellation == 'Champagne') %>%
  ggplot(aes(x=price,y=points)) +
  geom_point() +
  xlab("Price") +
  ylab("Points") +
  ggtitle("Relationship between price and rating \n (Champagnes only)")

# Plotting the relationship between review length and rating

wines %>%
  ggplot(aes(x=rev_length,y=points)) +
  geom_point() +
  xlab("Length of review (characters)") +
  ylab("Points") +
  ggtitle("Relationship between review length and rating")

# Plotting the relationship between alcohol content and rating

wines %>%
  ggplot(aes(x=alcohol,y=points)) +
  geom_point() +
  xlab("Alcohol content (%)") +
  ylab("Points") +
  ggtitle("Relationship between \n alcohol content and rating")

# Plotting the relationship between badge and rating

wines %>%
  group_by(badge) %>%
  ggplot(aes(x=badge,y=points)) +
  geom_violin() +
  xlab("Badge") +
  ylab("Points") +
  ggtitle("Relationship between \n badge and rating") +
  theme(axis.text.x = element_text(angle = 45))

# Plotting the relationship between country and rating (countries with more than 20 wines reviewed for legibility))

wines %>%
  group_by(country) %>%
  filter(n() > 20) %>%
  ggplot(aes(x=country,y=points)) +
  geom_violin() +
  xlab("Country") +
  ylab("Points") +
  ggtitle("Distribution of ratings by country") +
  theme(axis.text.x = element_text(angle = 45))

# Plotting the relationship between appellation and rating (only for appellations covering more than 250 wines, for legibility)

wines %>%
  group_by(appellation) %>%
  filter(n() > 250) %>%
  ggplot(aes(x=str_trunc(appellation, 20),y=points)) +
  geom_violin() +
  xlab("Appellation") +
  ylab("Points") +
  ggtitle("Relationship between main appellations and rating") +
  theme(axis.text.x = element_text(angle = 45))

# Plotting the relationship between country and price (price < 300 USD and countries with more than 20 wines reviewed for legibility)

wines %>%
  group_by(country) %>%
  filter(n() > 20) %>%
  filter(price < 300) %>%
  ggplot(aes(x=country,y=price)) +
  geom_violin() +
  xlab("Country") +
  ylab("Price") +
  ggtitle("Distribution of prices (< 300 USD) by country (> 20 wines reviewed)") +
  theme(axis.text.x = element_text(angle = 45))

# Plotting the relationship between appellation and price (< 300 USD and appellations with more than 250 wines for legibility)

wines %>%
  filter(price < 300) %>%
  group_by(appellation) %>%
  filter(n() > 250) %>%
  ggplot(aes(x=str_trunc(appellation, 20),y=price)) +
  geom_violin() +
  xlab("") +
  ylab("Price") +
  ggtitle("Distribution of prices (< 300 USD) by main appellation (> 250 wines)") +
  theme(axis.text.x = element_text(angle = 45))


# Part II : modelling

# First model : comparison of different classification algorithms among wine qualities

wines_class <- wines

# We transform the gradings in three categories of roughly equivalent population

wines_class$quality[wines_class$points %in% c(80, 81, 82, 83, 84, 85, 86, 87)] <- 'Low'
wines_class$quality[wines_class$points %in% c(88, 89, 90)] <- 'Average'
wines_class$quality[wines_class$points %in% c(91, 92, 93, 94, 95, 96, 97, 98, 99, 100)] <- 'High'

# We need to transform this new variable in factor for the modelling

wines_class$quality <- as.factor(wines_class$quality)

wines_class %>%
  group_by(Quality = quality) %>%
  summarise(Population = n()) %>%
  pander(.)

# We construct the train and test sets

set.seed(2020)
test_index_class <- createDataPartition(y = wines_class$quality, times = 1, p = 0.9, list = FALSE)
wines_class_train <- wines_class[test_index_class,]
wines_class_test <- wines_class[-test_index_class,]

# We define the resampling options

trainControl <- trainControl(method="cv", number=5, classProbs=TRUE)

# First classification algorithm: Random Forest with 1000 trees (as suggested by the litterature) and nodesize at 1 (better for classification);
# we use only price, rev_length and country as variables, as the EDA part exhibited a stronger points-relation compared to the others
# We tried the three different possible values of mtry (1, 2 and 3), and a value of 3 gives the best accuracy
# We set a new seed to ensure consistent results

set.seed(2020)
rf_model <- randomForest(quality ~ price + rev_length + country, data = wines_class_train, importance = TRUE, ntree = 1000, nodesize = 1, mtry = 3, trControl = trainControl)

varImpPlot(rf_model)

# We can now run the RF prediction on the test set

rf_pred <- predict(rf_model, wines_class_test)

confusionMatrix(rf_pred, wines_class_test$quality)

# We start building a table presenting the accuracy of the three different models

rf_acc <- data.frame(Method = "Random Forest (1000 trees, node size = 1, mtry = 3)", Accuracy = confusionMatrix(rf_pred, wines_class_test$quality)$overall["Accuracy"])

pander(rf_acc)


# Second classification algorithm: CART with bagging

set.seed(2020)

tree_model <- train(quality ~ price + rev_length + country, data = wines_class_train, method = "treebag", trControl = trainControl)

tree_pred <- predict(tree_model, wines_class_test)
confusionMatrix(tree_pred, wines_class_test$quality)

tree_acc <- data.frame(Method = "Bagged CART (no parameters)", Accuracy = confusionMatrix(tree_pred, wines_class_test$quality)$overall["Accuracy"])

pander(bind_rows(rf_acc, tree_acc))


# Third classification algorithm: K nearest neighbors, with a grid of k parameter ranging from 1 to 10

set.seed(2020)

knn_model <- train(quality ~ price + rev_length + country, data = wines_class_train, method = "knn",  trControl = trainControl, tuneGrid = expand.grid(k = 1:10))

knn_model

# The model retains a parameter of k = 1, which will be used for the prediction

knn_pred <- predict(knn_model, wines_class_test)
confusionMatrix(knn_pred, wines_class_test$quality)

knn_acc <- data.frame(Method = "K nearest neighbors (grid for k: 1 to 10, value of 1 selected)", Accuracy = confusionMatrix(knn_pred, wines_class_test$quality)$overall["Accuracy"])

pander(bind_rows(rf_acc, tree_acc, knn_acc))


# Conclusion: we obtain pretty similar accuracies for two of the three models, with a decent 0.79 overall accuracy
# (obviously higher for the "high" and "low" categories), for a simple model with just three explicative variables


#Second model: predicting if the wine appellation is a Champagne based on the text of its review

wines_t2v <- wines %>% select(review, appellation)

wines_t2v$isChamp[wines_t2v$appellation =="Champagne"] <- 1

wines_t2v$isChamp[wines_t2v$appellation !="Champagne"] <- 0

wines_t2v <- wines_t2v %>% select(-appellation)

summary(wines_t2v)

# Let's graph the most frequent words (aside from "Champagne") used for Champagnes and non-Champagnes
# We use the Corpus functions of the 'tm' package for this

wines_t2v_champ <- wines_t2v %>% filter(isChamp == 1)

corpus_champ <- Corpus(VectorSource(wines_t2v_champ$review))

corpus_champ <- tm_map(corpus_champ, removePunctuation)
corpus_champ <- tm_map(corpus_champ, removeNumbers)
corpus_champ <- tm_map(corpus_champ, tolower)
corpus_champ <- tm_map(corpus_champ, removeWords, c("champagne", "wine", "drink", stopwords("english")))
corpus_champ <- tm_map(corpus_champ, stripWhitespace)
corpus_champ <- tm_map(corpus_champ, stemDocument)

# Drawing the word cloud of the 50 most frequent words (aside from stopwords) for Champagnes 
# We set a new seed to ensure consistent presentation results

set.seed(2020)

wordcloud(corpus_champ, scale = c(5, 0.5), min.freq = 15, max.words = 60, random.order = FALSE, rot.per = 0.33,
          colors = brewer.pal(4,"Set2"))


# We proceed similarily for non-Champagnes

wines_t2v_nochamp <- wines_t2v %>% filter(isChamp == 0)

corpus_nochamp <- Corpus(VectorSource(wines_t2v_nochamp$review))

corpus_nochamp <- tm_map(corpus_nochamp, removePunctuation)
corpus_nochamp <- tm_map(corpus_nochamp, removeNumbers)
corpus_nochamp <- tm_map(corpus_nochamp, tolower)
corpus_nochamp <- tm_map(corpus_nochamp, removeWords, c("champagne", "wine", "drink", stopwords("english")))
corpus_nochamp <- tm_map(corpus_nochamp, stripWhitespace)
corpus_nochamp <- tm_map(corpus_nochamp, stemDocument)

set.seed(2020)

wordcloud(corpus_nochamp, scale = c(5, 0.5), min.freq = 15, max.words = 60, random.order = FALSE, rot.per = 0.33,
          colors = brewer.pal(4,"Set2"))

# We construct the train and test sets

set.seed(2020)
test_index_t2v <- createDataPartition(y = wines_t2v$isChamp, times = 1, p = 0.9, list = FALSE)
wines_t2v_train <- wines_t2v[test_index_t2v,]
wines_t2v_test <- wines_t2v[-test_index_t2v,]

# Starting the process of creating the respective Document Term Matrices, with the text2vec package
# We first define the tokens

token_train <- itoken(wines_t2v_train$review, 
                      preprocessor = tolower,
                      tokenizer = word_tokenizer)

# Creating the vocabulary vectors, removing the obvious "champagne" stop word

vectors <- create_vocabulary(token_train, stopwords = "champagne") %>% vocab_vectorizer(.)

# Finally, putting the result in DTM format

dtm_train <- create_dtm(token_train, vectors)

# We proceed similarily for the test set, using the same vectors as the training set ones

token_test <- itoken(wines_t2v_test$review, 
                     preprocessor = tolower,
                     tokenizer = word_tokenizer)

dtm_test <- create_dtm(token_test, vectors)


# We will use the 'glmnet' package for the regressions, as it appears particularily efficient in a text mining context, according to litterature

# First algorithm : Standard GLM with 5-fold cross-validation (minimum is 3-fold, default is 10-fold), area-under-curve as measure and lasso penalty (alpha = 1)

set.seed(2020)

glmnet_train <- cv.glmnet(x = dtm_train, y = wines_t2v_train$isChamp, 
                          family = "binomial",
                          alpha = 1,
                          type.measure = "auc",
                          nfolds = 5)

# Plotting the AUC as function of log(lambda) 

plot(glmnet_train)

# Identifying the maximum value of AUC

max(glmnet_train$cvm)

# Computing the prediction as to the expected values of the test set

glmnet_pred <- predict(glmnet_train, dtm_test, type = 'response')[,1]

# Transforming the prediction in binary value, and putting it in a datframe along with the test set actual values

glmnet_res <- data.frame(cbind(test = wines_t2v_test$isChamp, pred = ifelse(glmnet_pred>0.5,1,0)))

confusionMatrix(as.factor(glmnet_res$pred), as.factor(glmnet_res$test))

# Building the table comparing the accuracies of the three algorithms

glmnet_acc <- data.frame(Method = "Standard GLM with 5-fold cross-validation", Accuracy = confusionMatrix(as.factor(glmnet_res$pred), as.factor(glmnet_res$test))$overall["Accuracy"])

pander(glmnet_acc)


# Second algorithm: N-grams with pruning

pruned_vectors <- prune_vocabulary(create_vocabulary(token_train, stopwords = "champagne", ngram = c(1L, 2L)),
                                   term_count_min = 10, doc_proportion_max = 0.5) %>%
  vocab_vectorizer(.)

dtm_png_train <- create_dtm(token_train, pruned_vectors)
dtm_png_test <- create_dtm(token_test, pruned_vectors)

set.seed(2020)

glmnet_png_train <- cv.glmnet(x = dtm_png_train, y = wines_t2v_train$isChamp, 
                              family = "binomial",
                              alpha = 1,
                              type.measure = "auc",
                              nfolds = 5)

plot(glmnet_png_train)

max(glmnet_png_train$cvm)

glmnet_png_pred <- predict(glmnet_png_train, dtm_png_test, type = 'response')[,1]

glmnet_png_res <- data.frame(cbind(test = wines_t2v_test$isChamp, pred = ifelse(glmnet_png_pred>0.5,1,0)))

confusionMatrix(as.factor(glmnet_png_res$pred), as.factor(glmnet_png_res$test))

glmnet_png_acc <- data.frame(Method = "GLM with 5-fold cross-validation, N-grams with pruning", Accuracy = confusionMatrix(as.factor(glmnet_png_res$pred), as.factor(glmnet_png_res$test))$overall["Accuracy"])

pander(bind_rows(glmnet_acc, glmnet_png_acc))


# Third algorithm: TF-IDF  transformation

# Initializing the class

tfidf <- TfIdf$new()

# Fitting and transforming the training set

dtm_tfidf_train <- dtm_train %>% fit_transform(., tfidf)

# We need to create dtm_test once again then transform it, as using 'fit_transformer' would involve fitting first (which we should not do on the test set)

dtm_tfidf_test <- create_dtm(token_test, vectors) %>%
  transform(tfidf)

# We can now run the modelling process on the newly transformed DTM

set.seed(2020)

glmnet_tfidf_train <- cv.glmnet(x = dtm_tfidf_train, y = wines_t2v_train$isChamp, 
                                family = "binomial",
                                alpha = 1,
                                type.measure = "auc",
                                nfolds = 5)

plot(glmnet_tfidf_train)

max(glmnet_tfidf_train$cvm)

glmnet_tfidf_pred <- predict(glmnet_tfidf_train, dtm_tfidf_test, type = 'response')[,1]

glmnet_tfidf_res <- data.frame(cbind(test = wines_t2v_test$isChamp, pred = ifelse(glmnet_tfidf_pred>0.5,1,0)))

confusionMatrix(as.factor(glmnet_tfidf_res$pred), as.factor(glmnet_tfidf_res$test))

glmnet_tfidf_acc <- data.frame(Method = "GLM with 5-fold cross-validation and TF-IDF transformation", Accuracy = confusionMatrix(as.factor(glmnet_tfidf_res$pred), as.factor(glmnet_tfidf_res$test))$overall["Accuracy"])

pander(bind_rows(glmnet_acc, glmnet_png_acc, glmnet_tfidf_acc))

# We save the image after the last step, for the Rmarkdown report

save.image()

# END
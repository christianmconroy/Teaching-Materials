tweets <- readr::read_csv("https://raw.githubusercontent.com/kolaveridi/kaggle-Twitter-US-Airline-Sentiment-/master/Tweets.csv")

tweets_clean <- dplyr::select(tweets, text, airline_sentiment)
tweets_clean <-  dplyr::mutate(tweets_clean,
                               airline_sentiment = factor(airline_sentiment))

tweet_corpus <- tm::Corpus(tm::VectorSource(tweets_clean$text))

tweet_corpus[1:6]

tm::inspect(tweet_corpus[1:6])

corpus_clean <- tm::tm_map(tweet_corpus, tolower)
corpus_clean <- tm::tm_map(corpus_clean, tm::removeNumbers)
corpus_clean <- tm::tm_map(corpus_clean, tm::removeWords, tm::stopwords())
corpus_clean <- tm::tm_map(corpus_clean, tm::removePunctuation)
corpus_clean <- tm::tm_map(corpus_clean, tm::stripWhitespace)

tm::inspect(corpus_clean[1:3])

tweet_dtm <- tm::DocumentTermMatrix(corpus_clean)

perc75 <- sample(1:nrow(tweets_clean), floor(.75 * nrow(tweets_clean)))

tweet_raw_train <-tweets_clean[perc75,]
tweet_raw_test <- tweets_clean[-perc75,]

tweets_dtm_train <- tweet_dtm[perc75,]
tweets_dtm_test <- tweet_dtm[-perc75,]

tweets_corpus_train <- corpus_clean[perc75]
tweets_corpus_test <- corpus_clean[-perc75]

prop.table(table(tweet_raw_train$airline_sentiment))
prop.table(table(tweet_raw_test$airline_sentiment))

tweets_dict <- tm::findFreqTerms(tweets_dtm_train, 5)
tweets_train <- tm::DocumentTermMatrix(tweets_corpus_train, list(dictionary = tweets_dict))
tweets_test <- tm::DocumentTermMatrix(tweets_corpus_test, list(dictionary = tweets_dict))

# convert_counts <- function(x) {
#   x <- ifelse(x > 0, 1, 0)
#   x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
# }

labeled_tweets_train <- apply(tweets_train, MARGIN = 2, function(x){ifelse(x > 0, 1, 0)})#convert_counts)
labeled_tweets_test  <- apply(tweets_test, MARGIN = 2, function(x){ifelse(x > 0, 1, 0)})#convert_counts)

# tweets_classifier <- e1071::naiveBayes(labeled_tweets_train,
#                                        tweet_raw_train$airline_sentiment)

fast_classifier <- fastNaiveBayes::fastNaiveBayes(labeled_tweets_train,
                                                  tweet_raw_train$airline_sentiment)

fast_classifier_sparse <- fastNaiveBayes::fastNaiveBayes(Matrix::Matrix(labeled_tweets_train, sparse = T),
                                                  tweet_raw_train$airline_sentiment)

# tweets_test_pred <- predict(tweets_classifier, labeled_tweets_test)
fast_pred <- predict(fast_classifier, labeled_tweets_test)
fast_pred_sparse <- predict(fast_classifier_sparse, labeled_tweets_test)
# table(tweets_test_pred, tweet_raw_test$airline_sentiment)
#
# gmodels::CrossTable(tweets_test_pred, tweet_raw_test$airline_sentiment,
#            prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
#            dnn = c('predicted', 'actual'))

gmodels::CrossTable(fast_pred, tweet_raw_test$airline_sentiment,
                    prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
                    dnn = c('predicted', 'actual'))

gmodels::CrossTable(fast_pred_sparse, tweet_raw_test$airline_sentiment,
                    prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
                    dnn = c('predicted', 'actual'))

library(magrittr)

tidy_tweets <- tweets %>%
  dplyr::mutate(id = 1:NROW(.)) %>%
  dplyr::select(id, text) %>%
  dplyr::distinct() %>%
  tidytext::unnest_tokens(word, text ) %>%
  dplyr::anti_join(tidytext::stop_words) %>%
  dplyr::count(id, word)
%>%
  dplyr::mutate(n = ifelse(n > 0, 1, 0))

tidy_dtm <- tidytext::cast_dtm(tidy_tweets, id, word, n)

train <- sample(1:nrow(tidy_dtm), .75 * nrow(tidy_dtm))

tidy_train <- tidy_dtm[train,]
tidy_test <- tidy_dtm[-train,]

y_train <- tweets$airline_sentiment[as.numeric(train)]

y_test <- tweets$airline_sentiment[-as.numeric(train)]

tidy_fast <- fastNaiveBayes::fastNaiveBayes(tidy_train, y_train)
tidy_fast <- tidy_train %>%
  as.matrix() %>%
  Matrix::Matrix(sparse = T)%>%
  fastNaiveBayes::fastNaiveBayes( y_train)
tidy_pred <- predict(tidy_fast, tidy_test)

gmodels::CrossTable(tidy_pred, y_test,
                    prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
                    dnn = c('predicted', 'actual'))

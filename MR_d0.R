setwd("~/Desktop/MyR/RShiny/MovieRecommender")

#### Data Prepare ####

## Libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(stringi)
library(tibble)
library(ggplot2)
library(caret)
library(recommenderlab)
library(tidytext)
library(tokenizers)
#library(text2vec)

movie0 <- read.csv('movies_metadata.csv')
head(movie0)
m1 <-na.omit(movie0)
summary(m1$vote_count)
summary(m1$vote_average)
m1$release_year <- year(as.Date(m1$release_date))
summary(m1$release_year)
m2 <- m1[complete.cases(m1[,c('id', 'title', 'release_year', 'genres')]),]
# m2 <- m2[m2$genres != '[]',]
head(m2$genres)
m2$genres1 <- stringi::stri_extract_all(m2$genres, regex = "[A-Z][a-z]+")
#m2$companies1 <- stringi::stri_extract_all(m2$production_companies, regex = "[A-Z][a-z]*[ [A-Z][a-z]*]*")
#m3 <- m2[!is.na(m2$genres1) & !is.na(m2$companies1),]
m3 <- m2[!is.na(m2$genres1),]
genres <- levels(as.factor(unlist(m3$genres1)))
# companies <- levels(as.factor(unlist(m3$companies1)))


genre_df <- tibble::enframe(m3$genres1)%>%
  unnest(value)%>%
  mutate(temp=1)%>%
  pivot_wider(names_from=value, values_from=temp, values_fill=list(temp=0))

# company_df <- tibble::enframe(m3$companies1)%>%
#   unnest(value)%>%
#   mutate(temp=1)%>%
#   pivot_wider(names_from=value, values_from=temp, values_fill=list(temp=0))
# 
# movie1 <- cbind(m3, genre_df, company_df)

movie1 <- cbind(m3, genre_df)

# simple_recommend <- function(data, genre, rec_num, by_vote_avg=F, by_popularity=T, year_after=NULL){
#   if (is.null(company)) {
#     if (is.null(year_after)) {generic_df <- data[data[,genre]==1 & data$vote_count>=50, ]}
#     else {generic_df <- data[data[,genre]==1 & data$vote_count>=50 & data$release_year >= year_after, ]}}
# 
#   else {if (is.null(year_after)) {generic_df <- data[data[,genre]==1 & data$company==1 & data$vote_count>=50, ]}
#     else {generic_df <- data[data[,genre]==1 & data[,company]==1, data$vote_count>=50 & data$release_year >= year_after, ]}}
# 
#   if (by_vote_avg==T) {generic_df <-  generic_df[order(generic_df$vote_average,as.numeric(generic_df$popularity) , decreasing = T),]}
#   else  {generic_df <-  generic_df[order(as.numeric(generic_df$popularity), decreasing = T),]}
#   rec <- generic_df[1:rec_num, c('title', 'release_year', 'genres1','vote_average', 'popularity', 'companies1')]
#   print(rec)
# }
# simple_recommend(movie1, 'Action', 'Walt Disney Pictures',10, T, T, 2010)

#### Simple Recommender ####

simple_recommend1 <- function(data, genre, rec_num, by_vote_avg=F, by_popularity=T, year_after=NULL){
  
  if (is.null(year_after)) {generic_df <- data[data[,genre]==1 & data$vote_count>=50, ]}
  else {generic_df <- data[data[,genre]==1 & data$vote_count>=50 & data$release_year >= year_after, ]}
  
  
  if (by_vote_avg==T) {generic_df <-  generic_df[order(generic_df$vote_average,as.numeric(generic_df$popularity) , decreasing = T),]}
  else  {generic_df <-  generic_df[order(as.numeric(generic_df$popularity, generic_df$vote_average), decreasing = T),]}
  rec <- generic_df[1:rec_num, c('title', 'release_year', 'genres1','vote_average', 'popularity')]
  return(rec)
}

simple_recommend1(movie1, 'Action', 10, 2010)

# simple_recommend2 <- function(data, genre, rec_num, by_vote_avg=F, by_popularity=T, year_after=NULL, company=NULL){
#   if (is.null(company)) {
#     if (is.null(year_after)) {generic_df <- data[data[,genre]==1 & data$vote_count>=50, ]}
#     else {generic_df <- data[data[,genre]==1 & data$vote_count>=50 & data$release_year >= year_after, ]}}
#   
#   else {if (is.null(year_after)) {generic_df <- data[data[,genre]==1 & data$company==1 & data$vote_count>=50, ]}
#     else {generic_df <- data[data[,genre]==1 & data[,company]==1, data$vote_count>=50 & data$release_year >= year_after, ]}}
#   
#   if (by_vote_avg==T) {generic_df <-  generic_df[order(generic_df$vote_average,as.numeric(generic_df$popularity) , decreasing = T),]}
#   else  {generic_df <-  generic_df[order(as.numeric(generic_df$popularity), decreasing = T),]}
#   rec <- generic_df[1:rec_num, c('title', 'release_year', 'genres1','vote_average', 'popularity', 'companies1')]
#   print(rec)
# }

#### Content-Based ####
# link0 <- read.csv('links_small.csv')
# credit0 <- read.csv('credits.csv')
# keyword0 <- read.csv('keywords.csv')
# 
# keyword0$key <- stringi::stri_extract_all(keyword0$keywords, regex = '[a-z]+[ \\-a-z]*')
# for (i in 1:nrow(keyword0)) {
#   keyword0$key[[i]] <- keyword0$key[[i]][!keyword0$key %in% c('id', 'name')]
# }
# 
# keyword

movie1$description <- paste(movie1$overview, movie1$tagline)
m_temp <- movie1[,c('title','description')]
temp <- m_temp%>%unnest_tokens(word, description)%>%anti_join(stop_words)
freq_temp <- temp%>%count(word, sort = T)
# wordcloud::wordcloud(words=freq_temp$word,freq = freq_temp$n, max.words = 200, colors = RColorBrewer::brewer.pal(8,'Dark2'),random.order=FALSE, rot.per=0.35)
wordcloud2::wordcloud2(freq_temp)

library(superml)
tfv <- superml::TfIdfVectorizer$new(max_features = 1000, remove_stopwords = T)
tf_mat <- tfv$fit_transform(m_temp$description)

cos_sim <- function(m1,m2){
  mat <- tcrossprod(m1,m2)
  t1 <- sqrt(apply(m1,1,crossprod))
  t2 <- sqrt(apply(m2, 1, crossprod))
  mat/outer(t1,t2)
}

cos_sim_mat <- cos_sim(tf_mat, tf_mat)

titles <- movie1$title
content_recommender <- function(title, rec_num){
  idx <- which(titles==title)
  sim_scores <- tf_mat[idx,]
  orders <- order(sim_socres, decreasing = T) 
  sim_df <- as.data.frame(cbind(sim_scores,orders))
  rec_idx <- which(sim_scores %in% sim_df[sim_df[orders %in% 1:20, 'sim_scores']])
  return(head(titles[rec_idx], rec_num))
}

#### Collaborative-Filtering (Item-Based vs User-Based) ####




#### Hybrid Recommender (ContentBased + CollaborativeFiltering)####

rating0 <- read.csv('ratings.csv')
movie1$movieId <- movie1$id
r_m0 <- rating0%>%join(movie1, by='movieId')

hybrid_recommender <- function(userId, title, rec_num){
  idx <- which(titles==title)
  sim_scores <- tf_mat[idx,]
  order0 <- order(sim_socres, decreasing = T) 
  sim_df <- as.data.frame(cbind(sim_scores,order0))
  rec_idx0 <- which(sim_scores %in% sim_df[order0 %in% 1:30, 'sim_scores'])
  
  cf_est <- movie1[rec_idx0, 'est']
  order1 <- order(rec_est,decreasing = T)
  cf_df <- as.data.frame(cbind(cf_est, order1))
  rec_idx1 <- which(cf_est %in% cf_df[order1 %in% 1:10, 'cf_est'])
  
  return(movie1[rec_idx1, c('title', 'est')])
}


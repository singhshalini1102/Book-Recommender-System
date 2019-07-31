library(data.table)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(DT)
library(corrplot)
library(tidyr)
library(hexbin)
library(stringr)
library(qgraph)
library(recommenderlab)


## read the data
books <- fread('/Users/shalinisingh/Desktop/Projects/BookRecommender/books.csv')
ratings <- fread('/Users/shalinisingh/Desktop/Projects/BookRecommender/ratings.csv')
book_tags <- fread('/Users/shalinisingh/Desktop/Projects/BookRecommender/book_tags.csv')
tags <- fread('/Users/shalinisingh/Desktop/Projects/BookRecommender/tags.csv')


## remove the duplicate ratings.
ratings[, N := .N, .(user_id, book_id)]
cat('Number of duplicate ratings: ', nrow(ratings[N > 1]))
## Number of duplicate ratings:  4487
ratings <- ratings[N == 1]

## keep users who rated more than 2 books
ratings[, N := .N, .(user_id)]
cat('Number of users who rated more than 2 books: ', uniqueN(ratings[N <= 2, user_id]))
## Number of users who rated more than 2 books:  8364
ratings <- ratings[N > 2]

## Data Exploration
## i) Distribution of ratings

ratings %>% 
  ggplot(aes(x = rating, fill = factor(rating))) +
  ggtitle("Distribution of Ratings") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(color = "grey20") + scale_fill_brewer(palette = "Oranges") + guides(fill = FALSE)

## ii) Number of ratings per user

ratings %>% 
  group_by(user_id) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  xlab("Ratings per User") + ylab("Rating count") +
  ggtitle("Number of ratings per user") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(fill = "cadetblue3", color = "grey20") + coord_cartesian(c(3, 50))

## iii) Distribution of mean user rating

ratings %>% 
  group_by(user_id) %>% 
  summarize(mean_user_rating = mean(rating)) %>% 
  ggplot(aes(mean_user_rating)) +
  xlab("Mean of User ratings") + ylab("Rating count") +
  ggtitle("Distribution of mean user rating") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(bins = 30, fill = "cyan", color = "grey20")

## iv) 10 highly Rated books

books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(-average_rating) %>% 
  top_n(10,wt = average_rating) %>% 
  select(image, title, ratings_count, average_rating) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

## v) 10 most popular books

books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(-ratings_count) %>% 
  top_n(10,wt = ratings_count) %>% 
  select(image, title, ratings_count, average_rating) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

## vi) Relationship between number of rating and average rating

get_cor <- function(df){
  m <- cor(df$x,df$y, use="pairwise.complete.obs");
  eq <- substitute(italic(r) == cor, list(cor = format(m, digits = 2)))
  as.character(as.expression(eq));                 
}

books %>% 
  filter(ratings_count < 1e+5) %>% 
  ggplot(aes(ratings_count, average_rating)) + stat_bin_hex(bins = 50) + scale_fill_distiller(palette = "Spectral") + 
  stat_smooth(method = "lm", color = "orchid", size = 2) +
  annotate("text", x = 85000, y = 2.7, label = get_cor(data.frame(x = books$ratings_count, y = books$average_rating)), parse = TRUE, color = "orchid", size = 7)

## vii) Frequent raters

tmp <- ratings %>% 
  group_by(user_id) %>% 
  summarize(mean_rating = mean(rating), number_of_rated_books = n())

tmp %>% filter(number_of_rated_books <= 100) %>% 
  ggplot(aes(number_of_rated_books, mean_rating)) + stat_bin_hex(bins = 50) + scale_fill_distiller(palette = "Spectral") + stat_smooth(method = "lm", color = "orchid", size = 2, se = FALSE) +
  annotate("text", x = 80, y = 1.9, label = get_cor(data.frame(x = tmp$number_of_rated_books, y = tmp$mean_rating)), color = "orchid", size = 7, parse = TRUE)

### Collaborative Filtering

dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))
ratingmat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>% select(-user_id)

ratingmat <- as.matrix(ratingmat)
dimnames(ratingmat) <- dimension_names
ratingmat[1:5, 1:5]
dim(ratingmat)

## Find similar users

current_user <- "17329"
rated_items <- which(!is.na((as.data.frame(ratingmat[current_user, ]))))
selected_users <- names(which(apply(!is.na(ratingmat[ ,rated_items]), 1, sum) >= 2))
head(selected_users, 40)

user1 <- data.frame(item=colnames(ratingmat),rating=ratingmat[current_user,]) %>% filter(!is.na(rating))
user2 <- data.frame(item=colnames(ratingmat),rating=ratingmat["1339",]) %>% filter(!is.na(rating))
tmp<-merge(user1, user2, by="item")
tmp
cor(tmp$rating.x, tmp$rating.y, use="pairwise.complete.obs")

user2 <- data.frame(item = colnames(ratingmat), rating = ratingmat["21877", ]) %>% filter(!is.na(rating))
tmp <- merge(user1, user2, by="item")
tmp

cor(tmp$rating.x, tmp$rating.y, use="pairwise.complete.obs")

# normalizing the user ratings
rmat <- ratingmat[selected_users, ]
user_mean_ratings <- rowMeans(rmat,na.rm=T)
rmat <- rmat - user_mean_ratings

similarities <- cor(t(rmat[rownames(rmat)!=current_user, ]), rmat[current_user, ], use = 'pairwise.complete.obs')
sim <- as.vector(similarities)
names(sim) <- rownames(similarities)
res <- sort(sim, decreasing = TRUE)
head(res, 40)

## Visualizing similarities

sim_mat <- cor(t(rmat), use = 'pairwise.complete.obs')
random_users <- selected_users[1:20]
qgraph(sim_mat[c(current_user, random_users), c(current_user, random_users)], layout = "spring", vsize = 5, theme = "TeamFortress", labels = c(current_user, random_users))

## Get predictions for other books.

similar_users <- names(res[1:4])

similar_users_ratings <- data.frame(item = rep(colnames(rmat), length(similar_users)), rating = c(t(as.data.frame(rmat[similar_users,])))) %>% filter(!is.na(rating))

current_user_ratings <- data.frame(item = colnames(rmat), rating = rmat[current_user,]) %>% filter(!is.na(rating))

predictions <- similar_users_ratings %>% 
  filter(!(item %in% current_user_ratings$item)) %>% 
  group_by(item) %>% summarize(mean_rating = mean(rating))

predictions %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
# Get the best 5 predictions

predictions %>% 
  arrange(-mean_rating) %>% 
  top_n(5, wt = mean_rating) %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

## Using recommenderlab

ratingmat0 <- ratingmat
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
rm(ratingmat0)
gc()

real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

# model

model <- Recommender(real_ratings, method = "UBCF", param = list(method = "pearson", nn = 4))

prediction <- predict(model, real_ratings[current_user, ], type = "ratings")

as(prediction, 'data.frame') %>% 
  arrange(-rating) %>% .[1:5,] %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE)) 

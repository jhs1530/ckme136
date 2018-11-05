## 1. Using genre similarity

#recreate genremat (excluding movies that were never rated)
genremat <- movie_data[,11:34]
genremat <- as.matrix(genremat)

dim(genremat) #9724 movies, 24 genres

#initialize userprofile
userprofile <- matrix(nrow = nrow(ratingmat), ncol = ncol(genremat))

#for each row representing a user, calculate preference towards certain genre(col)
for (i in 1:nrow(ratingmat)) {
  y <- train_set[train_set$userId == i,]
  x <- genremat[match(y$movieId, movie_index),]
  z <- x * y$rating
  userprofile[i,] <- apply(z, 2, mean, na.rm = TRUE)
}

userprofile[1:10,1:10]

# rating is based on how similar a movie is to user's preferred genre
genre_based <- function(a) {
  # a = c(userId, movieId)
  x <- userprofile[a[1], ] * genremat[match(a[2], movie_index), ]
  return(mean(x[x>0], na.rm = TRUE))
}

content1 <- apply(as.matrix(test_set[,c(1,2)]), 1, genre_based)

sum(is.na(content1)) # instances where same genre was never rated in a train set
na_content1 <- sum(is.na(content1))

#RMSE
rmse_content1 <- sum((actual_ratings[!is.na(content1)] - content1[!is.na(content1)])^2)/length(content1[!is.na(content1)])
mae_content1 <- sum(abs(actual_ratings[!is.na(content1)] - content1[!is.na(content1)]))/length(content1[!is.na(content1)])

#RMSE
rmse_content1 <- sum((actual_ratings[!is.na(content1)] - content1[!is.na(content1)])^2)/length(content1[!is.na(content1)])
mae_content1 <- sum(abs(actual_ratings[!is.na(content1)] - content1[!is.na(content1)]))/length(content1[!is.na(content1)])

## 2. Using Linear Reression
coeffmat <- matrix(ncol = 25, nrow = 610)

for (i in 1:nrow(ratingmat)) {
  y <- train_set[train_set$userId == i,]
  x <- genremat[match(y$movieId, movie_index),]
  x[is.na(x)] <- 0 # treat NAs as 0s for regression
  fit <- lm(y$rating~x)
  coeffmat[i,] <- fit$coefficients
}

reg_rating <- function(a) {
  # a = c(userId, movieId)
  intercept <- coeffmat[a[1], 1] 
  beta <- coeffmat[a[1], -1]
  indep_var <- genremat[match(a[2], movie_index),]
  exp_rating <- intercept + sum(beta * indep_var, na.rm = TRUE)
  #Limit scale from 0.5 to 5
  exp_rating <- max(0.5, exp_rating)
  exp_rating <- min(5, exp_rating)
  return(exp_rating)
}

content2 <- apply(as.matrix(test_set[,c(1,2)]), 1, reg_rating)

sum(is.na(content2))
na_content2 <- sum(is.na(content2))

rmse_content2 <- sum((actual_ratings[!is.na(content2)] - content2[!is.na(content2)])^2)/length(content2[!is.na(content1)])
mae_content2 <- sum(abs(actual_ratings[!is.na(content2)] - content2[!is.na(content2)]))/length(content2[!is.na(content1)])


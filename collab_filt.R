##Concerns
#number of ratings per movies
num_rated <- apply(ratingmat, 2, function(x) sum(!is.na(x)))
summary(num_rated)
hist(num_rated, xlim = c(0,10) ,breaks = max(num_rated))
sum(num_rated==1) # instances where a movie was only rated once
sum(num_rated==2) # instances where a movie was only rated twice
#I will try to come up with different validating method
#Or, use subset of larger dataset

ratingmat[1:10,1:10] # check
dim(ratingmat) # 610 users, 9724 movies

#Normalizing
ratingmat_norm_mean <- apply(ratingmat, 1, mean, na.rm = TRUE) #average rating for each user, used for norm/denormalizing
ratingmat_norm_stdev <- apply(ratingmat, 1, sd, na.rm = TRUE) #stdev of rating for each user, used for norm/denormalizing
ratingmat_norm <- (ratingmat - ratingmat_norm_mean)/ratingmat_norm_stdev

mean(ratingmat_norm[1,],na.rm = TRUE) #check
sd(ratingmat_norm[1,],na.rm = TRUE) #chek

watchedmat <- ratingmat
watchedmat[watchedmat >0] <- 1 #logical matrix indicating with 1 if a user watched a movie 0 if not

#Calculating cosine similarity (removing NAs)
cosine_sim <- function(x, y) {
  A = sum(x*x, na.rm = TRUE)
  B = sum(y*y, na.rm = TRUE)
  C = sum(x*y, na.rm = TRUE)
  return (C/(sqrt(A)*sqrt(B)))
}

n <- nrow(ratingmat)

#Initialize similarity matrix
similaritymat <- matrix(NA, n, n)

#Fill in similarity matrix - may take a while to run
for (i in 1:n) {
  for (j in 1:n) {
    similaritymat[i, j] = cosine_sim(ratingmat_norm[i, ], ratingmat_norm[j, ])
  }
}

cosine_cf <- function(a, nearest = 3) {
  # a = c(userId, movieId)
  #Find top n similar users who have rated the movie
  sim_vec <- watchedmat[,match(a[2], movie_index)] * similaritymat[a[1], ] #Find users who already watched the movie
  testrating <- sort(sim_vec, decreasing = TRUE, index.return = TRUE, na.last = TRUE) #sort similarity
  rating_index <- testrating[[2]][1:nearest] #n nearest users
  #find weighted mean of the ratings, then denormalize
  n_ratings <- ratingmat_norm[rating_index, match(a[2], movie_index)]
  n_sim <- similaritymat[a[1],rating_index]
  return(weighted.mean(n_ratings, n_sim, na.rm = TRUE) * ratingmat_norm_stdev[a[1]] + ratingmat_norm_mean[a[1]])
}

##Find expected rating
cf1 <- apply(as.matrix(test_set[,1:2]), 1, cosine_cf)

sum(is.na(cf1)) # instances where model could not predict because the movie was never rated by another user
na_cf1 <- sum(is.na(cf1))

#RMSE
rmse_cf1 <- sum((actual_ratings[!is.na(cf1)] - cf1[!is.na(cf1)])^2)/length(cf1[!is.na(cf1)])
mae_cf1 <- sum(abs(actual_ratings[!is.na(cf1)] - cf1[!is.na(cf1)]))/length(cf1[!is.na(cf1)])

#consolidating results
{result <- data.frame(matrix(nrow = 3, ncol = 5))
rownames(result) <- c("rmse", "mae", "num_na")
colnames(result) <- c("base1", "base2", "genre_based", "genre_reg","cosine_cf")
result[1,] <- c(rmse_base1,rmse_base2,rmse_content1,rmse_content2,rmse_cf1)
result[2,] <- c(mae_base1,mae_base2,mae_content1,mae_content2,mae_cf1)
result[3,] <- c(na_base1,na_base2,na_content1,na_content2,na_cf1)
result}



# Package used: "reshape2"

library("reshape2")

# Please place rating.dat and movies.dat file on the working directory

## 1. Data prep

## Importing data

# Ratings format - userId::movieId::rating::timestamp
raw_ratings <- read.csv("ratings.dat", sep = ":", header = FALSE)
raw_ratings <- raw_ratings[,c(1,3,5,7)]
colnames(raw_ratings) <- c("userId", "movieId", "rating", "timestamp")

# Movies format - movieId::title::genre1|genre2|gen re3
raw_movies <- read.csv("movies.dat", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
raw_movies$V1 <- gsub("::", "\t", raw_movies$V1)
raw_movies <- read.csv(textConnection(raw_movies$V1), sep = '\t', header = FALSE, stringsAsFactors = FALSE)
colnames(raw_movies) <- c("movieId", "title", "genres")

# Separating genres
raw_movies$genres[1]<- "Animation|Children's|Comedy|||" # Maximum number of genres one movie has 5,
genres <- read.csv(textConnection(raw_movies$genres), sep="|", header = FALSE, stringsAsFactors = FALSE)

genre_list <- unique(unlist(as.matrix(apply(genres,2,unique))))

genre_matrix <- data.frame(matrix(ncol = length(genre_list),nrow = 0))
colnames(genre_matrix) <- genre_list

for (i in 1:nrow(genres)) {
  for (j in 1:ncol(genres)) {
    x <- match(genres[i,j], genre_list)
    genre_matrix[i,x] <- 1
  }
}
genre_matrix <- genre_matrix[,-which(colnames(genre_matrix) == '')]

raw_movies <- cbind(raw_movies, genre_matrix)


# Binarize ratings
ratings <- raw_ratings
ratings$rating <- 1
ratings$rating[raw_ratings$rating<4] <- -1




#1.1 Data Filtering

# Original dataset: ratings
movie_index <- unique(ratings$movieId)
movies <- raw_movies[raw_movies$movieId %in% movie_index,] # remove movies that are never rated in ratings.dat
movies$movieRow <- 1:nrow(movies) # Indexing movieId to correspond with the rating matrix
ratings$movieRow <- match(ratings$movieId, movies$movieId)

# Secondary dataset: ratings100
movie_index100 <- dimnames(table(ratings$movieId)[table(ratings$movieId)>=100])[[1]]
user_index100 <- dimnames(table(ratings$userId)[table(ratings$userId)>=100])[[1]]
ratings100 <- ratings[ratings$movieId %in% movie_index100 & ratings$userId %in% user_index100,]
movies100 <- raw_movies[raw_movies$movieId %in% movie_index100,]

ratings100$userRow <- match(ratings100$userId,user_index100)

movies100$movieRow <- 1:nrow(movies100)
ratings100$movieRow <- match(ratings100$movieId, movies100$movieId)

### Any object related to the secondary dataset would be labeled with 100 from now on


## Subsetting Train/test Set
seed <- 3
set.seed(seed)
p = 0.6

test_index <- sample(nrow(ratings), nrow(ratings)*p) #test index for ratings data
test_set <- ratings[test_index,] #create test rating set
actual_ratings <- test_set$rating #actual ratings values
test_set <- test_set[,c("userId","movieRow")] #userId, movieId for testing

train_set <- ratings # Initializing train_set
train_set$rating[test_index] <- NA #remove rating values assigned to test set

test_index100 <- sample(nrow(ratings100), nrow(ratings100)*p)
test_set100 <- ratings100[test_index100,]
actual_ratings100 <- test_set100$rating
test_set100 <- test_set100[,c("userRow", "movieRow")]

train_set100 <- ratings100
train_set100$rating[test_index100] <- NA




# Create rating matrix
ratingmat <- dcast(train_set, userId~movieId, value.var = "rating", na.rm = FALSE)
ratingmat100 <- dcast(train_set100, userRow~movieId, value.var = "rating", na.rm = FALSE)
# Each rows represent users, each column represents movies
ratingmat <- as.matrix(ratingmat[,-1]) #removing userIds
ratingmat100 <- as.matrix(ratingmat100[,-1])


#Evaluation scheme
performance <- function(x) {
  num_na <- sum(x==0) # Number of instances where model could not make a prediction
  # True Positive
  tp <- length(which(x+actual_ratings == 2))
  # True Negative
  tn <- length(which(x+actual_ratings == -2))
  # False Positive (acutal is negative)
  fp <- length(which(x-actual_ratings == 2))
  # False Negative
  fn <- length(which(x-actual_ratings == -2))
  accuracy <- (tp+fp) / (tp+tn+fp+fn+num_na)
  precision <- tp / (tp+fp+num_na)
  recall <- tp / (tp+fn+num_na)
  return(c("accuracy" = accuracy, "precision" = precision, "recall" = recall, "num_na" = num_na))
}

performance100 <- function(x) {
  num_na <- sum(x==0)
  # True Positive
  tp <- length(which(x+actual_ratings100 == 2))
  # True Negative
  tn <- length(which(x+actual_ratings100 == -2))
  # False Positive (acutal is negative)
  fp <- length(which(x-actual_ratings100 == 2))
  # False Negative
  fn <- length(which(x-actual_ratings100 == -2))
  accuracy <- (tp+fp) / (tp+tn+fp+fn+num_na)
  precision <- tp / (tp+fp+num_na)
  recall <- tp / (tp+fn+num_na)
  return(c("accuracy" = accuracy, "precision" = precision, "recall" = recall, "num_na" = num_na))
}

## 2. Populairty-based
popularity <- function (x) {
  movieRow <- as.integer(x[2])
  avg <- mean(ratingmat[,movieRow], na.rm = TRUE)
  if (is.na(avg)) {
    return(0)
  }
  return(sign(avg))
}

popularity_result <- apply(test_set, 1, popularity)

popularity100 <- function(x) {
  movieRow <- as.integer(x[2])
  avg <- mean(ratingmat100[,movieRow], na.rm = TRUE)
  if (is.na(avg)) {
    return(0)
  }
  return(sign(avg))
}

popularity_result100 <- apply(test_set100, 1, popularity100)

#Results
print(popularity_table <- performance(popularity_result))
print(popularity_table100 <- performance100(popularity_result100))


## 3. Content-based

genre_matrix <- as.matrix(movies[,4:21]) # Creating genre_matrix

tf_matrix <- genre_matrix/rowSums(genre_matrix, na.rm = TRUE) # TF
idf <- log(nrow(genre_matrix)/colSums(genre_matrix, na.rm = TRUE)) #IDF
tfidf <- tf_matrix * idf
tfidf <- as.matrix(tfidf)

tfidf[is.na(tfidf)] <- 0 
ratingmat[is.na(ratingmat)] <- 0 # Replacing NA's with 0 for easier calculation
tfidf_profile <- crossprod(t(ratingmat), tfidf)

tfidf[tfidf==0] <- NA 

tfidf_calc <- function (x) {
  movieRow <- as.integer(x[2])
  userId <- as.integer(x[1])
  sumprod <- sum(tfidf_profile[userId,]*tfidf[movieRow,], na.rm = TRUE)
  return(sign(sumprod))
}

tfidf_result <- apply(test_set, 1, tfidf_calc) # Predictions

#Same steps are repeated for the secondary dataset

genre_matrix100 <- as.matrix(movies100[,4:21])
tf_matrix100 <- genre_matrix100/rowSums(genre_matrix100, na.rm = TRUE)
idf100 <- log(nrow(genre_matrix100)/colSums(genre_matrix100, na.rm = TRUE))
tfidf100 <- tf_matrix100 * idf100
tfidf100 <- as.matrix(tfidf100)
tfidf100[is.na(tfidf100)] <- 0
ratingmat100[is.na(ratingmat100)] <- 0
tfidf_profile100 <- crossprod(t(ratingmat100), tfidf100)
tfidf100[tfidf100==0] <- NA

tfidf_calc100 <- function (x) {
  movieRow <- as.integer(x[2])
  userId <- as.integer(x[1])
  sumprod <- sum(tfidf_profile100[userId,]*tfidf100[movieRow,], na.rm = TRUE)
  return(sign(sumprod))
}

tfidf_result100 <- apply(test_set100, 1, tfidf_calc100)


# Results
print(tfidf_table <- performance(tfidf_result))
print(tfidf_table100 <- performance100(tfidf_result100))



## Collaborative Filtering
# Creating similarity matrix
ratingmat_norm <- ratingmat/sqrt(rowSums(ratingmat^2,na.rm = TRUE)) # Make length of each user_vector 1

ratingmat_norm[is.na(ratingmat_norm)] <- 0

# Since all vectors have length 1, cosine sim can be calculated by dot product of vectors
user_simmat <- crossprod(t(ratingmat_norm),t(ratingmat_norm))
item_simmat <- crossprod(ratingmat_norm,ratingmat_norm)


# For secondary dataset
ratingmat_norm100 <- ratingmat100/sqrt(rowSums(ratingmat100^2,na.rm = TRUE))

ratingmat_norm100[is.na(ratingmat_norm100)] <- 0
user_simmat100 <- crossprod(t(ratingmat_norm100),t(ratingmat_norm100))
item_simmat100 <- crossprod(ratingmat_norm100,ratingmat_norm100)

# k tested = (5, 10, 20, 30, 50, 100, 300)

## 4. UBCF

ubcf_calc <- function (x) {
  movieRow <- as.integer(x[2])
  userId <- as.integer(x[1])
  sim_vec <- abs(ratingmat[,movieRow])*user_simmat[userId,] # Search similar users who already have rated the given movie
  sim_vec[userId] <- NA # Remove one's own similarity
  simrank <- sort(sim_vec, decreasing = TRUE, index.return = TRUE, na.last = TRUE) # Rank users by similarity
  # Predict using k = (5, 10, 20, 30, 50, 100, 300) neighbors
  sumprod5 <- sum(simrank$x[1:5]*ratingmat[simrank$ix[1:5],movieRow], na.rm = TRUE)
  sumprod10 <- sum(simrank$x[1:10]*ratingmat[simrank$ix[1:10],movieRow], na.rm = TRUE)
  sumprod20 <- sum(simrank$x[1:20]*ratingmat[simrank$ix[1:20],movieRow], na.rm = TRUE)
  sumprod30 <- sum(simrank$x[1:30]*ratingmat[simrank$ix[1:30],movieRow], na.rm = TRUE)
  sumprod50 <- sum(simrank$x[1:50]*ratingmat[simrank$ix[1:50],movieRow], na.rm = TRUE)
  sumprod100 <- sum(simrank$x[1:100]*ratingmat[simrank$ix[1:100],movieRow], na.rm = TRUE)
  sumprod300 <- sum(simrank$x[1:300]*ratingmat[simrank$ix[1:300],movieRow], na.rm = TRUE)
  x <- c(sumprod5, sumprod10, sumprod20, sumprod30, sumprod50,sumprod100, sumprod300)
  return(sign(x))
}

ubcf_calc100 <- function (x) {
  movieRow <- as.integer(x[2])
  userId <- as.integer(x[1])
  sim_vec <- abs(ratingmat100[,movieRow])*user_simmat100[userId,]
  sim_vec[userId] <- NA
  simrank <- sort(sim_vec, decreasing = TRUE, index.return = TRUE, na.last = TRUE)
  sumprod5 <- sum(simrank$x[1:5]*ratingmat100[simrank$ix[1:5],movieRow], na.rm = TRUE)
  sumprod10 <- sum(simrank$x[1:10]*ratingmat100[simrank$ix[1:10],movieRow], na.rm = TRUE)
  sumprod20 <- sum(simrank$x[1:20]*ratingmat100[simrank$ix[1:20],movieRow], na.rm = TRUE)
  sumprod30 <- sum(simrank$x[1:30]*ratingmat100[simrank$ix[1:30],movieRow], na.rm = TRUE)
  sumprod50 <- sum(simrank$x[1:50]*ratingmat100[simrank$ix[1:50],movieRow], na.rm = TRUE)
  sumprod100 <- sum(simrank$x[1:100]*ratingmat100[simrank$ix[1:100],movieRow], na.rm = TRUE)
  sumprod300 <- sum(simrank$x[1:300]*ratingmat100[simrank$ix[1:300],movieRow], na.rm = TRUE)
  x <- c(sumprod5, sumprod10, sumprod20, sumprod30, sumprod50,sumprod100,sumprod300)
  return(sign(x))
}


ubcf_result <- apply(test_set, 1, ubcf_calc)
ubcf_result100 <- apply(test_set100, 1, ubcf_calc100)

# Results
ubcf_table <- t(apply(ubcf_result, 1, performance))
rownames(ubcf_table) <- c(5,10,20,30,50,100,300)

ubcf_table100 <- t(apply(ubcf_result100, 1, performance100))
rownames(ubcf_table100) <- c(5,10,20,30,50,100,300)

print(ubcf_table)
print(ubcf_table100)

## 5. IBCF

ibcf_calc <- function (x) {
  movieRow <- as.integer(x[2])
  userId <- as.integer(x[1])
  sim_vec <- abs(ratingmat[userId,])*item_simmat[movieRow,]
  sim_vec[movieRow] <- NA
  if (sum(!is.na(sim_vec))==0) {
    return(c(0,0,0,0,0,0,0))
  }
  simrank <- sort(sim_vec, decreasing = TRUE, index.return = TRUE, na.last = TRUE)
  sumprod5 <- sum(simrank$x[1:5]*ratingmat[userId,simrank$ix[1:5]], na.rm = TRUE)
  sumprod10 <- sum(simrank$x[1:10]*ratingmat[userId,simrank$ix[1:10]], na.rm = TRUE)
  sumprod20 <- sum(simrank$x[1:20]*ratingmat[userId,simrank$ix[1:20]], na.rm = TRUE)
  sumprod30 <- sum(simrank$x[1:30]*ratingmat[userId,simrank$ix[1:30]], na.rm = TRUE)
  sumprod50 <- sum(simrank$x[1:50]*ratingmat[userId,simrank$ix[1:50]], na.rm = TRUE)
  sumprod100 <- sum(simrank$x[1:100]*ratingmat[userId,simrank$ix[1:100]], na.rm = TRUE)
  sumprod300 <- sum(simrank$x[1:300]*ratingmat[userId,simrank$ix[1:300]], na.rm = TRUE)
  x <- c(sumprod5, sumprod10, sumprod20, sumprod30, sumprod50, sumprod100, sumprod300)
  return(sign(x))
}

ibcf_calc100 <- function (x) {
  movieRow <- as.integer(x[2])
  userId <- as.integer(x[1])
  sim_vec <- abs(ratingmat100[userId,])*item_simmat100[movieRow,]
  sim_vec[movieRow] <- NA
  if (sum(!is.na(sim_vec))==0) {
    return(c(0,0,0,0,0,0,0))
  }
  simrank <- sort(sim_vec, decreasing = TRUE, index.return = TRUE, na.last = TRUE)
  sumprod5 <- sum(simrank$x[1:5]*ratingmat100[userId,simrank$ix[1:5]], na.rm = TRUE)
  sumprod10 <- sum(simrank$x[1:10]*ratingmat100[userId,simrank$ix[1:10]], na.rm = TRUE)
  sumprod20 <- sum(simrank$x[1:20]*ratingmat100[userId,simrank$ix[1:20]], na.rm = TRUE)
  sumprod30 <- sum(simrank$x[1:30]*ratingmat100[userId,simrank$ix[1:30]], na.rm = TRUE)
  sumprod50 <- sum(simrank$x[1:50]*ratingmat100[userId,simrank$ix[1:50]], na.rm = TRUE)
  sumprod100 <- sum(simrank$x[1:100]*ratingmat100[userId,simrank$ix[1:100]], na.rm = TRUE)
  sumprod300 <- sum(simrank$x[1:300]*ratingmat100[userId,simrank$ix[1:300]], na.rm = TRUE)
  x <- c(sumprod5, sumprod10, sumprod20, sumprod30, sumprod50, sumprod100, sumprod300)
  return(sign(x))
}

ibcf_result <- apply(test_set, 1, ibcf_calc)
ibcf_result100 <- apply(test_set100, 1, ibcf_calc100)

# Results
ibcf_table <- t(apply(ibcf_result, 1, performance))
rownames(ibcf_table) <- c(5,10,20,30,50,100,300)

ibcf_table100 <- t(apply(ibcf_result100, 1, performance100))
rownames(ibcf_table100) <- c(5,10,20,30,50,100,300)

print(ibcf_table)
print(ibcf_table100)

## 6. Results

# Original dataset
popularity_table
tfidf_table
ubcf_table
ibcf_table

# Secondary dataset
popularity_table100
tfidf_table100
ubcf_table100
ibcf_table100




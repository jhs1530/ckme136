
##Importing data
movies <- read.csv("movies-small.csv") #movie data from movielens dataset
ratings <- read.csv("ratings-small.csv") #ratings data from movielens dataset
links <- read.csv("links-small.csv") #used to link movies to metadata
metadata <- read.csv("data.tsv", sep = '\t') #metadata from imdb (loading may take a while)

##Linking imdb dataset to movielens dataset
##links.csv has integer value code whereas imdb dataset uses tt + 7digit integer code
links$imdbId <- links$imdbId + 10000000 
links$imdbId <- as.character(links$imdbId) 
links$imdbId <- paste("tt",links$imdbId, sep = '')
# I added 10000000, paste "tt" then removed 1 to match two formats
links$imdbId <- paste(substr(links$imdbId, 1,2),substr(links$imdbId, 4,10), sep = '') #Remove extra '1' after 'tt'

head(metadata$tconst)
head(links$imdbId)

## I found that 16 of the links are outdated. Searching with older code on imdb redirected me to the correct code. 
#Updating outdated links to current one
links$imdbId[links$imdbId == "tt0056600"] <- "tt0054333"
links$imdbId[links$imdbId == "tt0081454"] <- "tt0079285"
links$imdbId[links$imdbId == "tt0250305"] <- "tt0157472"
links$imdbId[links$imdbId == "tt0266860"] <- "tt0235679"
links$imdbId[links$imdbId == "tt0282674"] <- "tt0169102"
links$imdbId[links$imdbId == "tt0285036"] <- "tt0142239"
links$imdbId[links$imdbId == "tt0290538"] <- "tt0270288"
links$imdbId[links$imdbId == "tt0313487"] <- "tt0287635"
links$imdbId[links$imdbId == "tt0377059"] <- "tt0343663"
links$imdbId[links$imdbId == "tt1213019"] <- "tt0211946"
links$imdbId[links$imdbId == "tt1347439"] <- "tt1543317"
links$imdbId[links$imdbId == "tt1522863"] <- "tt1493943"
links$imdbId[links$imdbId == "tt2227830"] <- "tt0173714"
links$imdbId[links$imdbId == "tt3534602"] <- "tt2690138"
links$imdbId[links$imdbId == "tt7807952"] <- "tt7806998"
links$imdbId[links$imdbId == "tt7808620"] <- "tt7807000"

#update column name for join
colnames(metadata)[1] <- "imdbId"

#join links to metadata, so that metadata also has movieIds
movie_data <- merge(links, metadata, by = "imdbId", all.x = TRUE)
summary(movie_data)

#check empty rows
sum(is.na(movie_data$originalTitle))


#configure genre list as matrix
genres <- as.character(movie_data$genres)
genres <- read.csv(textConnection(genres), header = FALSE, stringsAsFactors = FALSE) #genres are comma separated
head(genres) #Checking

#Create genre list
genre_list <- as.list(unique(c(genres$V1,genres$V2,genres$V3)))
length(genre_list) # total 24 genres + 1 empty (23rd col) 

#Creating genre matrix
genremat <- data.frame(matrix(ncol = 25, nrow = 0))
colnames(genremat) <- genre_list

for (i in 1:nrow(genres)) {
  for (j in 1:3) {
    x <- match(genres[i,j], genre_list)
    genremat[i,x] <- 1
  }
}

genremat[1:10, 1:25] #Check

#Drop empty array
genremat <- genremat[, !names(genremat) == ""]

genremat[1:10, 1:24] #Check

#Replace genre column with genre matrix
movie_data <- cbind(movie_data[,!names(movie_data) == "genres"], genremat)

nrow(movie_data) #9742 unique movies
length(unique(ratings$movieId)) #9724 unique movies rated

movie_data <- movie_data[which(movie_data$movieId %in% ratings$movieId), ] #removie movies that were never rated

## Subsetting Train/test Set
seed <- 3
set.seed(seed)
p = 0.2

test_index <- sample(nrow(ratings), nrow(ratings)*p) #test index for ratings data
test_set <- ratings[test_index,] #create test rating set
actual_ratings <- test_set$rating #actual ratings values
test_set <- test_set[,1:2] #userId, movieId for testing

train_set <- ratings # Initializing train_set
train_set$rating[test_index] <- NA #remove rating values assigned to test set

##Create rating matrix with train set

library("reshape2")
ratingmat <- dcast(train_set, userId~movieId, value.var = "rating", na.rm = FALSE)
# Each rows represent users, each column represents movies
movie_index <- colnames(ratingmat)[-1] # For matching ratingmat to movies
ratingmat <- as.matrix(ratingmat[,-1]) #removing userIds

dim(ratingmat) # 610 users, 9724 movies

##baseline methods
# 1. return average rating of a given movie

mean_movie <- function(a) {
  #a represent a movieId
  x <- ratingmat[,match(a,movie_index)]
  mean(x[x>0], na.rm = TRUE)
}

base1 <- apply(as.matrix(test_set$movieId), 1, mean_movie)

sum(is.na(base1)) # instances where movie was never rated in the test set
na_base1 <- sum(is.na(base1))

rmse_base1 <- sum((actual_ratings[!is.na(base1)] - base1[!is.na(base1)])^2)/length(base1[!is.na(base1)])
mae_base1 <- sum(abs(actual_ratings[!is.na(base1)] - base1[!is.na(base1)]))/length(base1[!is.na(base1)])

# 2. return average rating of a given user

mean_user <- function(a) {
  #a represent an user
  x <- ratingmat[a,]
  mean(x[x>0], na.rm = TRUE)
}

base2 <- apply(as.matrix(test_set$userId), 1, mean_user)

sum(is.na(base2)) 
na_base2 <- sum(is.na(base2))

rmse_base2 <- sum((actual_ratings[!is.na(base2)] - base2[!is.na(base2)])^2)/length(base2[!is.na(base2)])
mae_base2 <- sum(abs(actual_ratings[!is.na(base2)] - base2[!is.na(base2)]))/length(base2[!is.na(base2)])

#Please download the dataset first:

ml-latest-small.zip (size: 1 MB) from https://grouplens.org/datasets/movielens/

title.basics.tsv.gz from https://datasets.imdbws.com/

#Order of files - Please run the files in the following order
1. data_prep.R - Includes data preparation and baseline performance benchmark using average rating methods
2. content_based.R - Two approaches using genres preferences, and linear regression
3. collab_filt.R - cosine similarity

#Initial Result

| |base1|base2|genre_based|genre_reg|cosine_cf|
|---------|---------|---------|---------|---------|---------|
|rmse|0.9242157|0.8791282|0.9071452|1.0841915|2.2944107|
|mae|0.7447494|0.7303772|0.7386630|0.7954896|0.7255709|
|num_na|767|0|19|0|772|

num_na represent number of instances model could not make a prediction

#More work to be done on:
  1. Scale to larger Dataset
  2. Work with sparsity - Remove ratings with movies that were rated less than n times
  3. Build more models combining content-based and cf method

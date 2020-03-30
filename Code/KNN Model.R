library(reshape2)
library(plyr)
library(dplyr)
library(knitr)
library(stringr)
library(spatstat)

## Load the data from CSV's
ratings_df = read.csv("C:/Users/DELL/Desktop/Projects/Movie/ml-latest/ratings.csv")
movies_df = read.csv("C:/Users/DELL/Desktop/Projects/Movie/ml-latest/movies.csv")
genome_scores_df = read.csv("C:/Users/DELL/Desktop/Projects/Movie/ml-latest/genome-scores.csv")


## Count of Movies having less than 500 reviews/ratings
movies_count = data.frame(table(ratings_df$movieId))
colnames(movies_count) = c("movieId", "Freq")
frequently_rated_movies = subset(movies_count,movies_count$Freq > 100)

### Converting to numeric
frequently_rated_movies = as.numeric(as.character(frequently_rated_movies$movieId))

### Filtering out movies which are not in frequently_rated_movies
movies_filtered = movies_df[movies_df$movieId %in% frequently_rated_movies,]

## Filtering out movies from genome score df
genome_scores_filtered = genome_scores_df[genome_scores_df$movieId %in% frequently_rated_movies,]

## Converting to matrix
genome_scores_matrx = acast(genome_scores_filtered, movieId ~ tagId, 
                            value.var = "relevance")

## Creating a Movie_df Matrix mapping
movie_mapping = data.frame(movieId=row.names(genome_scores_matrx), matrixId=1:nrow(genome_scores_matrx))
movie_mapping = merge(movie_mapping, movies_df, by = 'movieId', all.x = TRUE)

### Resetting rownames for matrix
row.names(genome_scores_matrx) = 1:nrow(genome_scores_matrx)


## Liked Movies index

liked_movies = c("Toy Story (1995)", "Kung Fu Panda (2008)", "Shrek (2001)")
# liked_movies = c("Interstellar (2014)", "Apollo 13 (1995)", "The Martian (2015)")
liked_matrx_indx = c()

### Getting indices
for (i in liked_movies){
  liked_matrx_indx = c(liked_matrx_indx, subset(movie_mapping, movie_mapping$title == i, "matrixId")[,'matrixId'])
}

liked_movies_scores = genome_scores_matrx[liked_matrx_indx,]
centroid = colMeans(liked_movies_scores)


## Finding 5 similar movies to liked movies

### Calculating nearest neighbours 
sim_movies = FNN::get.knnx(genome_scores_matrx, rbind(centroid), 8)
sim_movies_index = sim_movies$nn.index
sim_movies_index = sim_movies_index[!(sim_movies_index %in% liked_matrx_indx)][1:5]

### Similar Movies are
similar_movies = as.character(subset(movie_mapping, movie_mapping$matrixId %in% sim_movies_index, "title")[,'title'])

print("Similar movies are: ")
print(similar_movies)


library(reshape2)
library(knitr)
library(Matrix)
library(proxyC)
library(RcppParallel)

## Load the data from CSV's
ratings_df = read.csv("C:/Users/DELL/Desktop/Projects/Movie/ml-latest/ratings.csv")
movies_df = read.csv("C:/Users/DELL/Desktop/Projects/Movie/ml-latest/movies.csv")


## Count of Movies having less than 500 reviews/ratings
movies_count = data.frame(table(ratings_df$movieId))
colnames(movies_count) = c("movieId", "Freq")
frequently_rated_movies = subset(movies_count,movies_count$Freq > 500)

### Converting to numeric
frequently_rated_movies = as.numeric(as.character(frequently_rated_movies$movieId))

### Filtering out movies which are not in frequently_rated_movies
ratings_filtered = ratings_df[ratings_df$movieId %in% frequently_rated_movies,]

## Count of ratings per user
user_ratings_count = data.frame(table(ratings_filtered$userId))
colnames(user_ratings_count) = c("userId", "Freq")
frequent_raters = subset(user_ratings_count, user_ratings_count$Freq > 400)

### Converting to numeric
frequent_raters = as.numeric(as.character(frequent_raters$userId))

### Filtering out users who have rated less than 300 times
ratings_filtered = ratings_filtered[ratings_filtered$userId %in% frequent_raters,]

## Creating user-movies ratings matrix
ratings_matrix = acast(ratings_filtered, userId ~ movieId, value.var = "rating", 
                                 fun.aggregate = mean, na.rm = TRUE,)

### Demeaning each row using only the available ratings
mean_user_rating = data.frame(userId=rownames(ratings_matrix), Means=rowMeans(ratings_matrix, na.rm = TRUE))

for (index in 1:nrow(mean_user_rating)){
  ratings_matrix[index,] = ratings_matrix[index,] - mean_user_rating[index,2]
}

### Filling in the NaN values with 0
ratings_matrix[is.na(ratings_matrix)] = 0

## Finding similarity between users

### Creating a sparse matrix to be compatible with the package
t_ratings_matrix = as(ratings_matrix, "dgCMatrix")

## Calculating Similarity
setThreadOptions(8)

user_similarity_matrix = simil(t_ratings_matrix,margin = 1, method = "cosine")

### Converting back to matrix
final_user_sim_matrix = as.matrix(user_similarity_matrix)

##Write matrix ro csv
write.table(final_user_sim_matrix, "C:/Users/DELL/Desktop/Projects/Movie/ml-latest/similarity_matrix.csv")

## End


## Old cosine code
# user_similarity_matrix = lsa::cosine(x = t_ratings_matrix) # Computes column similarity. So transpose

# ### Writing a function to identify common non-NA values between 2 rows 
# Common <- function(X) {if (is.na(X)) return(FALSE) else return(TRUE)}
# Common = Vectorize(Common)
# 
# ### Cosine Similarity calculation function
# cos.sim <- function(ix) 
# {
#   A = ratings_matrix[ix[1],]
#   B = ratings_matrix[ix[2],]
#   A_norm = A[Common(A) & Common(B)]
#   B_norm = B[Common(A) & Common(B)]
#   return( sum(A*B, na.rm = TRUE)/sqrt(sum(A_norm^2)*sum(B_norm^2)) )
# }
# 
# ## Generating the Matrix
# n = nrow(ratings_matrix)
# Row_ID_Iter = expand.grid(i=1:n, j=1:n) 
# user_similarity_matrix = matrix(apply(Row_ID_Iter, 1, cos.sim), n, n)



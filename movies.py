# Import Pandas
import pandas as pd
import csv
# Load Movies Metadata
metadata = pd.read_csv('movies_metadata.csv', low_memory=False)

# Print the first three rows
print(metadata.head(3))

#Weighted Rating (WR) = (v/(v+m)*R)+(m/(v+m)*C)
#where,
#v is the number of votes for the movie;
#m is the minimum votes required to be listed in the chart;
#R is the average rating of the movie;
#C is the mean vote across the whole report

# Calculate C
C = metadata['vote_average'].mean()
print(C)

# Calculate the minimum number of votes required to be in the chart, m
m = metadata['vote_count'].quantile(0.90)
print(m)

# Filter out all qualified movies into a new DataFrame
q_movies = metadata.copy().loc[metadata['vote_count'] >= m]
print(q_movies.shape)

# Function that computes the weighted rating of each movie
def weighted_rating(x, m=m, C=C):
    v = x['vote_count']
    R = x['vote_average']
    # Calculation based on the IMDB formula
    return (v/(v+m) * R) + (m/(m+v) * C)

# Define a new feature 'score' and calculate its value with `weighted_rating()`
q_movies['score'] = q_movies.apply(weighted_rating, axis=1)

#Sort movies based on score calculated above
q_movies = q_movies.sort_values('score', ascending=False)

#Print the top 20 movies
print(q_movies[['title', 'vote_count', 'vote_average', 'score']].head(20))

#Print plot overviews of the first 5 movies.
print(metadata['overview'].head())


#Import TfIdfVectorizer from scikit-learn
from sklearn.feature_extraction.text import TfidfVectorizer

#Define a TF-IDF Vectorizer Object. Remove all english stop words such as 'the', 'a'
tfidf = TfidfVectorizer(stop_words='english')

#Replace NaN with an empty string
metadata['overview'] = metadata['overview'].fillna('')

#Construct the required TF-IDF matrix by fitting and transforming the data
tfidf_matrix = tfidf.fit_transform(metadata['overview'])

#Output the shape of tfidf_matrix
print(tfidf_matrix.shape)


#using the cosine similarity to calculate a numeric quantity that denotes the similarity between two movies.
# formula: cosine(X,Y)=(X)%$%(Y⊺) /(||X|| * ||Y||)
# cosine_similarities(X, Y)

#Since the use of TF-IDF vectorizer, calculating the dot product will directly give you the cosine similarity score.
#use sklearn's linear_kernel() instead of cosine_similarities() since it is faster.
#linear_kernel(X,Y) = Xt . Y
# Import linear_kernel
from sklearn.metrics.pairwise import linear_kernel

# Compute the cosine similarity matrix
cosine_sim = linear_kernel(tfidf_matrix, tfidf_matrix)

#Construct a reverse map of indices and movie titles
indices = pd.Series(metadata.index, index=metadata['title']).drop_duplicates()

# FUNCTION that takes in movie title as input and outputs most similar movies
def get_recommendations(title, cosine_sim=cosine_sim):
    # Get the index of the movie that matches the title
    idx = indices[title]

    # Get the pairwsie similarity scores of all movies with that movie
    sim_scores = list(enumerate(cosine_sim[idx]))

    # Sort the movies based on the similarity scores
    sim_scores = sorted(sim_scores, key=lambda x: x[1], reverse=True)

    # Get the scores of the 10 most similar movies
    sim_scores = sim_scores[1:11]

    # Get the movie indices
    movie_indices = [i[0] for i in sim_scores]

    # Return the top 10 most similar movies
    return metadata['title'].iloc[movie_indices]

print(get_recommendations('...name..'))

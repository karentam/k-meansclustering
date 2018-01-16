# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

# Remove the first column: Type

df <- wine[-1]

# Use scale() function on the dataset df
df <- scale(df)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 
# nc is the maximum number of clusters to consider. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest? 
#   * 3 clusters because the Within Groups Sum of Square has a larger drop from cluster 1 to cluster 3. After cluster 3, it drops a lot less. 

#   * Why does this method work? What's the intuition behind it?
#   * The plot can clearly show the suggestion of how many clusters. It is based on the difference of Within Groups Sum of Square between clusters. 

#   * Look at the code for wssplot() and figure out how it works
#   * The code is to create a plot of the total within-groups sums of squares against the number of clusters in a K-means solution. A bend in the graph suggests the appropriate number of clusters. 

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# It suggests 3 clusters. 

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, 3, nstart=25)  
fit.km$size
fit.km$centers
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

ct.km <- table(wine$Type, fit.km$cluster)
ct.km

install.packages("flexclust")
library(flexclust)
randIndex(ct.km)

# The adjusted Rand index provides a measure of the agreement between two partitions, adjusted for chance. It ranges from -1 (no agreement) to 1 (perfect agreement). 
# Agreement between the wine varietal type and the cluster solution is 0.897495, meaning that it is a good clustering. 


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

install.packages("cluster")
library(cluster) 
clusplot(df, fit.km$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# The 3 clusters have clear boundaries, meaning that this is a good clustering. 

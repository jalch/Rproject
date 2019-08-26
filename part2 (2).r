library(fastcluster)
library(fpc)
#Examining responses

binary <- read.table('binary-ling-data.data', header = T)
extract.states <- c('AK', 'AL', 'AR', 'AZ', 
'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'IA', 'ID', 'IL',
 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME', 'MI', 'MN', 'MO', 
 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 
 'OK', 'OR', 'PA', 'RI', 'SC' , 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 
 'WA', 'WI', 'WV', 'WY', 'DC')
extract.states <- sort(extract.states)
bin.reduce <- binary[binary[,3] %in% extract.states,]
state.q <- bin.reduce[, -c(1,2,4,5,6)]
#factor(state.q[,1])
states.list <- split(state.q, state.q[,1])
states <- as.character(extract.states)
states.list <- states.list[which(names(states.list) %in% states)]
state.response.means <-lapply(states.list, function(df){
							colMeans(df[,2:ncol(df)])})
#Response Correlations
R <- cor(state.q[,-c(1)])
							

#PCA

prcomp.ling <- princomp(state.q[,-c(1)], cor = TRUE)
#how many components the first four components account for
percent.variance.4 <- (prcomp.ling$sdev[1])^2 + (prcomp.ling$sdev[2])^2 + (prcomp.ling$sdev[3])^2 + (prcomp.ling$sdev[4])^2
percent.variance.6 <- percent.variance.4 + (prcomp.ling$sdev[5])^2 + (prcomp.ling$sdev[6])^2
percent.variance.9 <- percent.variance.6 + (prcomp.ling$sdev[7])^2 + (prcomp.ling$sdev[8])^2 +(prcomp.ling$sdev[9])^2
percent.variance.11 <- percent.variance.9 + (prcomp.ling$sdev[10])^2 + (prcomp.ling$sdev[11])^2
pca <- prcomp(state.q[,-c(1)])
pc.r <- pca$rotation
first.comp <- pc.r[,1]
second.comp <- pc.r[,2]
third.comp <- pc.r[,3]
fourth.comp <- pc.r[,4]

first.comp.sorted <- sort(abs(first.comp),decreasing = TRUE)
second.comp.sorted <- sort(abs(second.comp), decreasing = TRUE)
third.comp.sorted <- sort(abs(third.comp), decreasing = TRUE)

first.comp.1.73.1 <- lapply(state.response.means, "[[", "Q073.1")
first.comp.2.73.6 <- lapply(state.response.means, "[[", "Q073.6")
first.comp.3.105.1 <- lapply(state.response.means, "[[", "Q105.1")
first.comp.4.80.1 <- lapply(state.response.means, "[[", "Q080.1")
first.comp.5.80.8 <- lapply(state.response.means, "[[", "Q080.8")

sec.comp.1.76.1 <- lapply(state.response.means, "[[", "Q076.1")
sec.comp.2.76.4 <- lapply(state.response.means, "[[", "Q076.4")
sec.comp.3.103.4 <- lapply(state.response.means, "[[", "Q103.4")
sec.comp.4.50.9 <- lapply(state.response.means, "[[", "Q050.9")
sec.comp.5.103.3 <- lapply(state.response.means, "[[", "Q103.3")

third.comp.1.59.2 <- lapply(state.response.means, "[[", "Q059.2")
third.comp.2.120.2 <- lapply(state.response.means, "[[", "Q120.2")
third.comp.3.50.4 <- lapply(state.response.means, "[[", "Q050.4")
third.comp.4.97.5 <- lapply(state.response.means, "[[", "Q097.5")
third.comp.5.86.3 <- lapply(state.response.means, "[[", "Q086.3")

#pca.plot of variances
par(mfrow=c(2, 2))
plot(pca)
cum.proportions <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(cum.proportions)
plot(prcomp.ling, type = 'lines') #better visualization of pca

#dimension reduction
set.seed(47)
pca.reduced.4 <- predict(pca)[,1:4]
pca.reduced.6 <- predict(pca) [,1:6]
pca.reduced.9 <- predict(pca)[,1:9]
pca.reduced.11 <- predict(pca)[,1:11]
means.df <- data.frame(matrix(unlist(state.response.means), nrow = 51, byrow = T))
pca.means <- prcomp(means.df)
pca.reduced.means.4 <- predict(pca.means)[,1:4] #4 components
pca.reduced.means.6 <- predict(pca.means)[,1:6] #6 components
pca.reduced.means.9 <- predict(pca.means)[,1:9] #9 components
pca.reduced.means.11 <- predict(pca.means)[,1:11] #11 components

dist.matrix.4 <- dist(pca.reduced.means.4)
dist.matrix.6 <- dist(pca.reduced.means.6)
dist.matrix.9 <- dist(pca.reduced.means.9)
dist.matrix.11 <- dist(pca.reduced.means.11)

#hclust for 4,6,9,11 components
hclusters.4 <- hclust(dist.matrix.4)
hclusters.6 <- hclust(dist.matrix.6)
hclusters.9 <- hclust(dist.matrix.9)
hclusters.11 <- hclust(dist.matrix.11)
#get all plots in same window??
par(mfrow=c(2, 2))
plot(hclusters.4, labels = names(state.response.means), main = "Mean State Responses")
rect.hclust(hclusters.4,3)
rect.hclust(hclusters.4,4)
rect.hclust(hclusters.4,5)
plot(hclusters.6, labels = names(state.response.means), main = "Mean State Responses")
rect.hclust(hclusters.6,3)
rect.hclust(hclusters.6,4)
rect.hclust(hclusters.6,5)
plot(hclusters.9, labels = names(state.response.means), main = "Mean State Responses")
rect.hclust(hclusters.9,3)
rect.hclust(hclusters.9,4)
rect.hclust(hclusters.9,5)
plot(hclusters.11, labels = names(state.response.means), main = "Mean State Responses")
rect.hclust(hclusters.11,3)
rect.hclust(hclusters.11,4)
rect.hclust(hclusters.11,5)


#hclust: 3 clusters on 4,6,9,11 components
groups.3.4 <- cutree(hclusters.4,3)
table.3.4 <- table(groups.3.4)
groups.3.6 <- cutree(hclusters.6,3)
table.3.6 <- table(groups.3.6)
groups.3.9 <- cutree(hclusters.9,3)
table.3.9 <- table(groups.3.9)
groups.3.11 <- cutree(hclusters.11,3)
table.3.11 <- table(groups.3.11)

#hclust: 4 clusters on 4,6,9,11
groups.4.4 <- cutree(hclusters.4,4)
table.4.4 <- table(groups.4.4)
groups.4.6 <- cutree(hclusters.6,4)
table.4.6 <- table(groups.4.6)
groups.4.9 <- cutree(hclusters.9,4)
table.4.9 <- table(groups.4.9)
groups.4.11 <- cutree(hclusters.11,4)
table.4.11 <- table(groups.4.11)

#hclust: 5 clusters on 4,6,9,11 components
groups.5.4 <- cutree(hclusters.4,5)
table.5.4 <- table(groups.5.4)
groups.5.6 <- cutree(hclusters.6,5)
table.5.6 <- table(groups.5.6)
groups.5.9 <- cutree(hclusters.9,5)
table.5.9 <- table(groups.5.9)
groups.5.11 <- cutree(hclusters.11,5)
table.5.11 <- table(groups.5.11)


#rect.hclust(hclusters,3)
#rect.hclust(hclusters,4)
#rect.hclust(hclusters,5)
#all different groupings from 2:5
counts = sapply(2:5, function(ncl)table(cutree(hclusters,ncl)))
names(counts) = 2:5
#[4 components] 3,4,5 hclust in dataframe. precursor to maps
states.clust3.4 <- sapply(unique(groups.3.4), function(g) names(state.response.means)
					[groups.3.4 == g])
states.clust4.4 <- sapply(unique(groups.4.4), function(g) names(state.response.means)
					[groups.4.4 == g])
states.clust.5.4 <- sapply(unique(groups.5.4), function(g) names(state.response.means)
					[groups.5.4 == g])
clust3.states.4 <- c(states.clust3.4[[1]],states.clust3.4[[2]],states.clust3.4[[3]])
clust3.cluster.4 <- c(rep(1,24),rep(2,14),rep(3,13))
clust4.states.4 <- c(states.clust4.4[[1]], states.clust4.4[[2]], states.clust4.4[[3]],states.clust4.4[[4]])
clust4.cluster.4 <- c(rep(1,19), rep(2,14), rep(3,13), rep(4,5))
clust5.states.4 <- c(states.clust.5.4[[1]],states.clust.5.4[[2]],states.clust.5.4[[3]], 
					states.clust.5.4[[4]], states.clust.5.4[[5]])
clust5.cluster.4<-c(rep(1,19),rep(2,9),rep(3,13),rep(4,5),rep(5,5))

clust3.df.4 <- data.frame(clust3.states.4,clust3.cluster.4)
clust4.df.4 <- data.frame(clust4.states.4, clust4.cluster.4)
clust5.df.4 <- data.frame(clust5.states.4,clust5.cluster.4)

#[6 components] 3,4,5 hclust in dataframe. precursor to maps
states.clust3.6 <- sapply(unique(groups.3.6), function(g) names(state.response.means)
					[groups.3.6 == g])
states.clust4.6 <- sapply(unique(groups.4.6), function(g) names(state.response.means)
					[groups.4.6 == g])
states.clust5.6 <- sapply(unique(groups.5.6), function(g) names(state.response.means)
					[groups.5.6 == g])
clust3.states.6 <- c(states.clust3.6[[1]],states.clust3.6[[2]],states.clust3.6[[3]])
clust3.cluster.6 <- c(rep(1,length(states.clust3.6[[1]])),rep(2,length(states.clust3.6[[2]])),rep(3,length(states.clust3.6[[3]])))
clust4.states.6 <- c(states.clust4.6[[1]], states.clust4.6[[2]], states.clust4.6[[3]],states.clust4.6[[4]])
clust4.cluster.6 <- c(rep(1,length(states.clust4.6[[1]])),rep(2,length(states.clust4.6[[2]])),rep(3,length(states.clust4.6[[3]])), rep(4,length(states.clust4.6[[4]])))
clust5.states.6<- c(states.clust.5.6[[1]],states.clust.5.6[[2]],states.clust.5.6[[3]], 
					states.clust.5.6[[4]], states.clust.5.6[[5]])
clust5.cluster.6 <- c(rep(1,length(states.clust5.6[[1]])),rep(2,length(states.clust5.6[[2]])),rep(3,length(states.clust5.6[[3]])), 
					rep(4,length(states.clust5.6[[4]])), rep(5, length(states.clust5.6[[5]])))

clust3.df.6 <- data.frame(clust3.states.6,clust3.cluster.6)
clust4.df.6 <- data.frame(clust4.states.6, clust4.cluster.6)
clust5.df.6 <- data.frame(clust5.states.6,clust5.cluster.6)
#left off
#[9 components] 3,4,5 hclust in dataframe. precursor to maps
states.clust3.9 <- sapply(unique(groups.3.9), function(g) names(state.response.means)
					[groups.3.9 == g])
states.clust4.9 <- sapply(unique(groups.4.9), function(g) names(state.response.means)
					[groups.4.9 == g])
states.clust.5.9 <- sapply(unique(groups.5.9), function(g) names(state.response.means)
					[groups.5.9 == g])
clust3.states.9 <- c(states.clust3.9[[1]],states.clust3.9[[2]],states.clust3.9[[3]])
clust3.cluster.9 <- c(rep(1,length(states.clust3.9[[1]])),rep(2,length(states.clust3.9[[2]])),rep(3,length(states.clust3.9[[3]])))
clust4.states.9 <- c(states.clust4.9[[1]],states.clust4.9[[2]],states.clust4.9[[3]],stfates.clust4.9[[4]])
clust4.cluster.9 <- c(rep(1,length(states.clust4.9[[1]])),rep(2,length(states.clust4.9[[2]])),rep(3,length(states.clust4.9[[3]])), rep(4,length(states.clust4.9[[4]])))
clust5.states.9<- c(states.clust.5.9[[1]],states.clust.5.9[[2]],states.clust.5.9[[3]], 
					states.clust.5.9[[4]], states.clust.5.9[[5]])
clust5.cluster.9<-c(rep(1,length(states.clust.5.9[[1]])),rep(2,length(states.clust.5.9[[2]])),rep(3,length(states.clust.5.9[[3]])), 
					rep(4,length(states.clust.5.9[[4]])), rep(5, length(states.clust.5.9[[5]])))

clust3.df.9<- data.frame(clust3.states.9,clust3.cluster.9)
clust4.df.9 <- data.frame(clust4.states.9, clust4.cluster.9)
clust5.df.9 <- data.frame(clust5.states.9,clust5.cluster.9)

#[11 components] 3,4,5 hclust in dataframe. precursor to maps
states.clust3.11 <- sapply(unique(groups.3.11), function(g) names(state.response.means)
					[groups.3.11 == g])
states.clust4.11 <- sapply(unique(groups.4.11), function(g) names(state.response.means)
					[groups.4.11 == g])
states.clust.5.11 <- sapply(unique(groups.5.11), function(g) names(state.response.means)
					[groups.5.11 == g])
clust3.states.11 <- c(states.clust3.11[[1]],states.clust3.11[[2]],states.clust3.11[[3]])
clust3.cluster.11 <- c(rep(1,length(states.clust3.11[[1]])),rep(2,length(states.clust3.11[[2]])),rep(3,length(states.clust3.11[[3]])))
clust4.states.11 <- c(states.clust4.11[[1]], states.clust4.11[[2]], states.clust4.11[[3]],states.clust4.11[[4]])
clust4.cluster.11 <- c(rep(1,length(states.clust4.11[[1]])),rep(2,length(states.clust4.11[[2]])),rep(3,length(states.clust4.11[[3]])), rep(4,length(states.clust4.11[[4]])))
clust5.states.11<- c(states.clust.5.11[[1]],states.clust.5.11[[2]],states.clust.5.11[[3]], 
					states.clust.5.11[[4]], states.clust.5.11[[5]])
clust5.cluster.11<-c(rep(1,length(states.clust.5.11[[1]])),rep(2,length(states.clust.5.11[[2]])),rep(3,length(states.clust.5.11[[3]])), 
					rep(4,length(states.clust.5.11[[4]])), rep(5, length(states.clust.5.11[[5]])))

clust3.df.11<- data.frame(clust3.states.11,clust3.cluster.11)
clust4.df.11 <- data.frame(clust4.states.11, clust4.cluster.11)
clust5.df.11 <- data.frame(clust5.states.11,clust5.cluster.11)






#Silhouette analysis for determining number of clusters

par(mfrow=c(1, 2))

asw <- numeric(20)
for (k in 2:20)
	asw[[k]] <- pam(dist.matrix.4,k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")
plot(pam(dist.matrix.4,k.best))

#silhoutte analysis is the same for 11
asw <- numeric(20)
for (k in 2:20)
	asw[[k]] <- pam(dist.matrix.11,k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")
plot(pam(dist.matrix.11,k.best))




#kmeans: 4, 6, 9, 11 components

#4
kmeans.data.4 <- kmeans(pca.reduced.means.4, centers = k.best, iter.max = 10)
kmeans.cluster.groups.4 <- kmeans.data.4$cluster
kmeans.cluster.4 <- sapply(unique(kmeans.cluster.groups.4), function(g) names(state.response.means)
					[kmeans.cluster.groups.4 ==g])
kmeans.clust.states <- c(kmeans.cluster.4[[1]],kmeans.cluster.4[[2]],kmeans.cluster.4[[3]])
kmeans.id <- c(rep(1,24), rep(2,14), rep(3,13))
kmeans.df.4 <- data.frame(kmeans.clust.states, kmeans.id) 
#get cluster means 
aggregate(pca.reduced.means.4, by = list(kmeans.data.4$cluster), FUN = mean)

par(mfrow=c(2,2))
plot(pca.reduced.means.4, col = kmeans.data.4$cluster)
plotcluster(pca.reduced.means.4, kmeans.data.4$cluster) #discriminant projection plot. one dimensional data is plotted against number
#discriminant projections for visualization of separate groupings. optimize separation of clusters and btw single cluster and rest of coordinates
clusplot(pca.reduced.means.4,kmeans.data.4$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
#visualizes clusters in a map. composition of a single cluster.
#how many iterations are needed? 


#maps 

#install.packages('RColorBrewer')
#install.packages('maps')
library(RColorBrewer)
library(maps)
data(state)
data(state.fips)

#kmeans : 3 clusters
state.order <- match(state.fips$abb, kmeans.df.4[,1])
state.clusts <- kmeans.df.4[state.order,2]
cols <- c('red', 'green', 'blue')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("Kmeans clustering: 3 clusters, 4 principal components")



# hclust: 3 clusters: 4,6,9,11 principal components
par(mfrow=c(2, 2))


# 4 components
state.order <- match(state.fips$abb, clust3.df.4[,1])
state.clusts <- clust3.df.4[state.order, 2]
cols <- c('red', 'green', 'blue')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 3 clusters, 4 pc")

# 6 components
state.order <- match(state.fips$abb, clust3.df.6[,1])
state.clusts <- clust3.df.6[state.order, 2]
cols <- c('red', 'green', 'blue')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 3 clusters, 6 pc")

# 9 components 
state.order <- match(state.fips$abb, clust3.df.9[,1])
state.clusts <- clust3.df.9[state.order, 2]
cols <- c('red', 'green', 'blue')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 3 clusters, 9 pc")

#11 components
state.order <- match(state.fips$abb, clust3.df.11[,1])
state.clusts <- clust3.df.11[state.order, 2]
cols <- c('red', 'green', 'blue')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 3 clusters, 11 pc")

#hclust: 4 clusters: 4, 6, 9, 11
par(mfrow=c(2, 2))
#4 components
state.order <- match(state.fips$abb, clust4.df.4[,1])
state.clusts <- clust4.df.4[state.order,2]
cols <- c('red', 'green', 'blue', 'purple')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 4 clusters, 4 pc")

#6 components
state.order <- match(state.fips$abb, clust4.df.6[,1])
state.clusts <- clust4.df.6[state.order,2]
cols <- c('red', 'green', 'blue', 'purple')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 4 clusters, 6 pc")
#9 components
state.order <- match(state.fips$abb, clust4.df.9[,1])
state.clusts <- clust4.df.9[state.order,2]
cols <- c('red', 'green', 'blue', 'purple')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 4 clusters, 9 pc")
#11 components
state.order <- match(state.fips$abb, clust4.df.11[,1])
state.clusts <- clust4.df.11[state.order,2]
cols <- c('red', 'green', 'blue', 'purple')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 4 clusters, 11 pc")


#hclust: 5 clusters: 4, 6, 9, 11

par(mfrow=c(2, 2))

#4 components
state.order <- match(state.fips$abb, clust5.df.4[,1])
state.clusts <- clust5.df.4[state.order, 2]
cols <- c('red', 'green', 'blue', 'purple', 'cyan')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 5 clusters, 4 pc")

#6 components
state.order <- match(state.fips$abb, clust5.df.6[,1])
state.clusts <- clust5.df.6[state.order, 2]
cols <- c('red', 'green', 'blue', 'purple', 'cyan')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 5 clusters, 6 pc")
#9 components 
state.order <- match(state.fips$abb, clust5.df.9[,1])
state.clusts <- clust5.df.9[state.order, 2]
cols <- c('red', 'green', 'blue', 'purple', 'cyan')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 5 clusters, 9 pc")
#11 components
state.order <- match(state.fips$abb, clust5.df.11[,1])
state.clusts <- clust5.df.11[state.order, 2]
cols <- c('red', 'green', 'blue', 'purple', 'cyan')
map.cols <- cols[state.clusts]
names(map.cols) <- state.fips$polyname
map("state", fill=T, lty=1, lwd=1, col=map.cols)
title("hclust: 5 clusters, 11 pc")








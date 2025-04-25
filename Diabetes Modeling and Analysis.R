# Libraries needed
library(factoextra)
library(cluster)
library(ggplot2)
library(corrplot)
library(mclust)

# Load and view dataset
Obesity = read.csv("ObesityDataSet_raw_and_data.csv")
View(Obesity)

# Preprocessing dataset
colSums(is.na(Obesity)) # Data is good 


# Exploratory Data Analysis

summary(Obesity)


# Bar Charts
ggplot(Obesity, aes(x = Gender, fill = Gender)) + 
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Frequency") +
  scale_fill_manual(values = c("pink", "skyblue")) 

ggplot(Obesity, aes(x = SMOKE, fill = SMOKE)) + 
  geom_bar() +
  labs(title = "Smoking Status", x = "Smoking Status", y = "Frequency") +
  scale_fill_manual(values = c("salmon", "gray"))


# Histograms
ggplot(Obesity, aes(x = Age)) + 
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

ggplot(Obesity, aes(x = Weight)) + 
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Weight Distribution", x = "Weight", y = "Frequency")

ggplot(Obesity, aes(x = FCVC)) + 
  geom_histogram(bins = 30, fill = "lightyellow", color = "black") +
  labs(title = "FCVC Distribution", x = "FCVC", y = "Frequency")

ggplot(Obesity, aes(x = NCP)) + 
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  labs(title = "NCP Distribution", x = "NCP", y = "Frequency")

ggplot(Obesity, aes(x = FAF)) + 
  geom_histogram(bins = 30, fill = "green", color = "black") +
  labs(title = "FAF Distribution", x = "FAF", y = "Frequency")


# Scatterplot Matrix
Obesity_numeric <- Obesity[, sapply(Obesity, is.numeric)]
plot(Obesity_numeric)


# Convert categorical to numeric
Obesity$Gender <- ifelse(Obesity$Gender == "Male", 0, 1)
Obesity$SMOKE <- ifelse(Obesity$SMOKE == "yes", 1, 0)
Obesity$FAVC <- ifelse(Obesity$FAVC == "yes", 1, 0)
Obesity$SCC <- ifelse(Obesity$SCC == "yes", 1, 0)
Obesity$family_history_with_overweight <- ifelse(Obesity$family_history_with_overweight == "yes", 1, 0)
Obesity$CAEC <- as.integer(factor(Obesity$CAEC, levels = c("no", "Sometimes", "Frequently", "Always")))
Obesity$CALC <- as.integer(factor(Obesity$CALC, levels = c("no", "Sometimes", "Frequently", "Always")))
Obesity$MTRANS <- as.numeric(factor(Obesity$MTRANS,levels = c("Walking", "Bike", "Motorbike", "Public_Transportation", "Automobile")))

# Select numeric features
ObesityNumeric <- Obesity[, sapply(Obesity, is.numeric)]
ObesityNumeric

# Scale data
ObesityScaled <- scale(ObesityNumeric)


# K-Means Clustering (Subhan and Julio)

# K-Mean clustering with full dataset
# Finding optimal K value via gap statistic 
set.seed(4323)
km.out = eclust(ObesityScaled, FUNcluster = "kmeans", nstart=50, nboot=50) 
km.out
# Plot Gap Statistic
fviz_nbclust(ObesityScaled, kmeans, nstart = 50, nboot = 50, method = "gap_stat") # K = 10 optimal K


# Finding optimal k via elbow method
set.seed(4323)
wss.list = numeric(15)

for(m in 1:15){
  km.out = eclust(x = data.frame(ObesityScaled), FUNcluster = "kmeans", 
                 k = m, nstart = 50)
  wss.list[m] = km.out$tot.withinss
}

plot(1:15, wss.list, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters", ylab = "Total WSS") # K = 5 optimal k 


# Finding optimal K via largest silhouette coefficient
set.seed(4323)
k.max = 15
silh.coef = numeric(k.max)
for(k in 2:15){
  silh.coef[k] = eclust(ObesityScaled, FUNcluster = "kmeans", k = k, graph = 0, nstart = 50)$silinfo$avg.width
}
plot(silh.coef, type = "b", pch = 19, col = 4)
which.max(silh.coef) # K = 2 optimal K 


# K-Mean clustering with K = 10
set.seed(4323)
km.out=eclust(ObesityScaled, FUNcluster = "kmeans", k = 10, nstart=50) # K = 10 Optimal K
km.out

# Plot clusters
fviz_cluster(km.out, data = ObesityScaled, main = "K-Means Clustering of Obesity Features with K=10")

# Silhouette Coefficient
km.out$silinfo

# Total WSS
km.out$tot.withinss

# Between WSS
km.out$betweenss

# Silhouette Visuals
sil = silhouette(km.out$cluster, dist(ObesityScaled))
fviz_silhouette(sil)


# K-Mean clustering with K = 5
set.seed(4323)
km.out=eclust(ObesityScaled, FUNcluster = "kmeans", k = 5, nstart=50) # K = 5 Optimal K
km.out

# Plot clusters
fviz_cluster(km.out, data = ObesityScaled, main = "K-Means Clustering of Obesity Features with K=5")

# Silhouette Coefficient
km.out$silinfo

# Total WSS
km.out$tot.withinss

# Between WSS
km.out$betweenss

# Silhouette Visuals
sil = silhouette(km.out$cluster, dist(ObesityScaled))
fviz_silhouette(sil) +
  theme(
    text = element_text(size = 20),        
    axis.title = element_text(size = 20),  
    axis.text = element_text(size = 20),   
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    plot.title = element_text(size = 40)
  )


# K-Mean clustering with K = 2
set.seed(4323)
km.out=eclust(ObesityScaled, FUNcluster = "kmeans", k = 2, nstart=50) # K = 2 Optimal K
km.out

# Plot clusters
fviz_cluster(km.out, data = ObesityScaled, main = "K-Means Clustering of Obesity Features with K=10")

# Silhouette Coefficient
km.out$silinfo

# Total WSS
km.out$tot.withinss

# Between WSS
km.out$betweenss

# Silhouette Visuals
sil = silhouette(km.out$cluster, dist(ObesityScaled))
fviz_silhouette(sil) +
  theme(
    text = element_text(size = 20),        
    axis.title = element_text(size = 20),  
    axis.text = element_text(size = 20),   
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    plot.title = element_text(size = 40)
  )


# Optimal clustering
# K-Mean clustering with K = 10
set.seed(4323)
km.optimal=eclust(ObesityScaled, FUNcluster = "kmeans", k = 10, nstart=50) # K = 10 Optimal K
km.optimal

# Plot clusters
fviz_cluster(km.optimal, data = ObesityScaled, main = "K-Means Clustering of Obesity Features with K=10")



# Specific Clustering

# Choosing K = 10 as optimal k

# Lifestyle & Dietary Habits Clustering
lifestyle_features = subset(ObesityNumeric, select = c(FAVC, FCVC, NCP, CAEC, CH2O, FAF, TUE, SCC, MTRANS))
lifestyle_scaled = scale(lifestyle_features)

# K-Mean clustering with K = 10
set.seed(4323)
km_lifestyle=eclust(lifestyle_scaled, FUNcluster = "kmeans", k = 10, nstart=50)
km_lifestyle

# Plot clusters
fviz_cluster(km_lifestyle, data = lifestyle_scaled, main = "K-Means Clustering of Lifestyle Features with K=10")

# Silhouette Coefficient
km_lifestyle$silinfo

# Silhouette Visuals
sil = silhouette(km_lifestyle$cluster, dist(lifestyle_scaled))
fviz_silhouette(sil)


# BMI & Health Risk Clustering
bmi_features = subset(ObesityNumeric, select = c(Age, Height, Weight, family_history_with_overweight))
bmi_scaled = scale(bmi_features)

# K-Mean clustering with K = 10
set.seed(4323)
km_bmi=eclust(bmi_scaled, FUNcluster = "kmeans", k = 10, nstart=50) 
km_bmi

# Plot clusters
fviz_cluster(km_bmi, data = bmi_scaled, main = "K-Means Clustering of BMI Features with K=10")

# Silhouette Coefficient
km_bmi$silinfo

# Silhouette Visuals
sil = silhouette(km_bmi$cluster, dist(bmi_scaled))
fviz_silhouette(sil)


# Combined Lifestyle + BMI Clustering
combined_features = subset(ObesityNumeric, select = c(Age, Height, Weight, FCVC, NCP, FAF, TUE, FAVC, CAEC, CH2O))
combined_scaled = scale(combined_features)

# K-Mean clustering with K = 10
set.seed(4323)
km_combined=eclust(combined_scaled, FUNcluster = "kmeans", k = 10, nstart=50)
km_combined

# Plot clusters
fviz_cluster(km_combined, data = combined_scaled, main = "K-Means Clustering of Combined Features with K=10")

# Silhouette Coefficient
km_combined$silinfo

# Silhouette Visuals
sil = silhouette(km_combined$cluster, dist(combined_scaled))
fviz_silhouette(sil)



# Hierarchical Clustering (Tony and Zach)

# Single linkage
hc_single = hclust(dist(ObesityScaled), method = "single")
plot(hc_single, labels=FALSE, main="Single Linkage")
single = cutree(hc_single, k = 10)

# Complete linkage
hc_complete = hclust(dist(ObesityScaled), method = "complete")
plot(hc_complete, labels=FALSE, main="Complete Linkage")
complete = cutree(hc_complete, k = 10)

# Average linkage
hc_average = hclust(dist(ObesityScaled), method = "average")
plot(hc_average, labels=FALSE, main="Average Linkage")
average = cutree(hc_average, k = 10)

# Centroid Linkage
hc_centroid = hclust(dist(ObesityScaled), method = "centroid")
plot(hc_centroid, labels=FALSE, main="Centroid Linkage")
centroid = cutree(hc_centroid, k = 10)


# Silhouette for single linkage (Best Linkage)
sil_single = silhouette(single, dist(ObesityScaled))
fviz_silhouette(sil_single)
mean(sil_single[,3])

# Silhouette for complete linkage
sil_complete = silhouette(complete, dist(ObesityScaled))
fviz_silhouette(sil_complete)
mean(sil_complete[,3])

# Silhouette for average linkage
sil_average = silhouette(average, dist(ObesityScaled))
fviz_silhouette(sil_average)
mean(sil_average[,3])

# Silhouette for centroid linkage
sil_centroid = silhouette(centroid, dist(ObesityScaled))
fviz_silhouette(sil_centroid)
mean(sil_centroid[,3])


# Single linkage (Best Linkage)
hc_single_optimal = hclust(dist(ObesityScaled), method = "single")
plot(hc_single_optimal, labels=FALSE, main="Single Linkage")
single_optimal = cutree(hc_single_optimal, k = 10)

# Silhouette for single linkage (Best Linkage)
sil_single_optimal = silhouette(single_optimal, dist(ObesityScaled))
fviz_silhouette(sil_single_optimal)
mean(sil_single_optimal[,3])


# Posthoc Analyis

# Better Model?

# K-Means Clustering
sil = silhouette(km.optimal$cluster, dist(ObesityScaled))
mean(sil[,3])

# Hierarchical Clustering
sil_single_optimal = silhouette(single_optimal, dist(ObesityScaled))
mean(sil_single_optimal[,3])


# External Validation (External Labels)
external_labels = Obesity$NObeyesdad

table(km.optimal$cluster, external_labels)
adjustedRandIndex(km.optimal$cluster, external_labels)

table(single_optimal, external_labels)
adjustedRandIndex(single_optimal, external_labels)







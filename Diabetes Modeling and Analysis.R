# Libraries needed
library(factoextra)
library(cluster)

# Load and view dataset
Obesity = read.csv("ObesityDataSet_raw_and_data.csv")
View(Obesity)


# Preprocessing dataset
colSums(is.na(Obesity)) # Data is good 


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


# K-Means Clustering 


# Finding optimal K value
km.out = eclust(ObesityScaled, FUNcluster = "kmeans", nstart=50, nboot=50) # K = 10 Optimal K
km.out

# Plot Gap Statistic
fviz_nbclust(ObesityScaled, kmeans, nstart = 50, nboot = 50, method = "gap_stat")


# Finding optimal K via largetst silhouette coefficient
k.max = 15
silh.coef = numeric(k.max)
for(k in 2:15){
  silh.coef[k] = eclust(ObesityScaled, FUNcluster = "kmeans", k = k, graph = 0, nstart = 50)$silinfo$avg.width
}

plot(silh.coef, type = "b", pch = 19, col = 4)

which.max(silh.coef)


# K-Mean clustering with K = 10
km.out=eclust(ObesityScaled, FUNcluster = "kmeans", k = 10, nstart=50) # K = 10 Optimal K
km.out

# Plot clusters
fviz_cluster(km.out, data = ObesityScaled, main = "K-Means Clustering of Obesity Features with K=10")

# Silhouette Coefficient
km.out$silinfo

# Silhouette Visuals
sil = silhouette(km.out$cluster, dist(ObesityScaled))
fviz_silhouette(sil)


# K-Mean clustering with K = 2
km.out=eclust(ObesityScaled, FUNcluster = "kmeans", k = 2, nstart=50) # K = 2 Optimal K
km.out

# Plot clusters
fviz_cluster(km.out, data = ObesityScaled, main = "K-Means Clustering of Obesity Features with K=10")

# Silhouette Coefficient
km.out$silinfo

# Silhouette Visuals
sil = silhouette(km.out$cluster, dist(ObesityScaled))
fviz_silhouette(sil)




# Specific Clustering


# Lifestyle & Dietary Habits Clustering
lifestyle_features = subset(ObesityNumeric, select = c(FAVC, FCVC, NCP, CAEC, CH2O, FAF, TUE, SCC, MTRANS))
lifestyle_scaled = scale(lifestyle_features)

# Finding optimal K value
km_lifestyle = eclust(lifestyle_scaled, FUNcluster = "kmeans", nstart=50, nboot=50) # K = 10 Optimal K
km_lifestyle

# Plot Gap Statistic
fviz_nbclust(lifestyle_scaled, kmeans, nstart = 50, nboot = 50, method = "gap_stat")


# K-Mean clustering with K = 10
km_lifestyle=eclust(lifestyle_scaled, FUNcluster = "kmeans", k = 10, nstart=50) # K = 10 Optimal K
km_lifestyle

# Plot clusters
fviz_cluster(km_lifestyle, data = lifestyle_scaled, main = "K-Means Clustering of Lifestyle Features with K=10")

# Silhouette Coefficient
km_lifestyle$silinfo

# Silhouette Visuals
sil = silhouette(km_lifestyle$cluster, dist(lifestyle_scaled))
fviz_silhouette(sil)


# Finding optimal K via largest silhouette coefficient
k.max = 15
silh.coef = numeric(k.max)
for(k in 2:15){
  silh.coef[k] = eclust(lifestyle_scaled, FUNcluster = "kmeans", k = k, graph = 0, nstart = 50)$silinfo$avg.width
}

plot(silh.coef, type = "b", pch = 19, col = 4)

which.max(silh.coef)


# K-Mean clustering with K = 2
km_lifestyle=eclust(lifestyle_scaled, FUNcluster = "kmeans", k = 2, nstart=50) # K = 2 Optimal K
km_lifestyle

# Plot clusters
fviz_cluster(km_lifestyle, data = lifestyle_scaled, main = "K-Means Clustering of Lifestyle Features with K=2")

# Silhouette Coefficient
km_lifestyle$silinfo

# Silhouette Visuals
sil = silhouette(km_lifestyle$cluster, dist(lifestyle_scaled))
fviz_silhouette(sil)



# BMI & Health Risk Clustering
bmi_features = subset(ObesityNumeric, select = c(Age, Height, Weight, family_history_with_overweight))
bmi_scaled = scale(bmi_features)

# Finding optimal K value
km_bmi = eclust(bmi_scaled, FUNcluster = "kmeans", nstart=50, nboot=50) # K = 9 Optimal K
km_bmi

# Plot Gap Statistic
fviz_nbclust(bmi_scaled, kmeans, nstart = 50, nboot = 50, method = "gap_stat")


# K-Mean clustering with K = 9
km_bmi=eclust(bmi_scaled, FUNcluster = "kmeans", k = 9, nstart=50) # K = 9 Optimal K
km_bmi

# Plot clusters
fviz_cluster(km_bmi, data = bmi_scaled, main = "K-Means Clustering of BMI Features with K=9")

# Silhouette Coefficient
km_bmi$silinfo

# Silhouette Visuals
sil = silhouette(km_bmi$cluster, dist(bmi_scaled))
fviz_silhouette(sil)


# Finding optimal K via largest silhouette coefficient
k.max = 15
silh.coef = numeric(k.max)
for(k in 2:15){
  silh.coef[k] = eclust(bmi_scaled, FUNcluster = "kmeans", k = k, graph = 0, nstart = 50)$silinfo$avg.width
}

plot(silh.coef, type = "b", pch = 19, col = 4)

which.max(silh.coef)


# K-Mean clustering with K = 2
km_bmi=eclust(bmi_scaled, FUNcluster = "kmeans", k = 2, nstart=50) # K = 2 Optimal K
km_bmi

# Plot clusters
fviz_cluster(km_bmi, data = bmi_scaled, main = "K-Means Clustering of BMI Features with K=2")

# Silhouette Coefficient
km_bmi$silinfo

# Silhouette Visuals
sil = silhouette(km_bmi$cluster, dist(bmi_scaled))
fviz_silhouette(sil)



# Combined Lifestyle + BMI Clustering
combined_features = subset(ObesityNumeric, select = c(Age, Height, Weight, FCVC, NCP, FAF, TUE, FAVC, CAEC, CH2O))
combined_scaled = scale(combined_features)

# Finding optimal K value
km_combined = eclust(combined_scaled, FUNcluster = "kmeans", nstart=50, nboot=50) # K = 10 Optimal K
km_combined

# Plot Gap Statistic
fviz_nbclust(combined_scaled, kmeans, nstart = 50, nboot = 50, method = "gap_stat")


# K-Mean clustering with K = 10
km_combined=eclust(combined_scaled, FUNcluster = "kmeans", k = 10, nstart=50) # K = 10 Optimal K
km_combined

# Plot clusters
fviz_cluster(km_combined, data = combined_scaled, main = "K-Means Clustering of Combined Features with K=10")

# Silhouette Coefficient
km_combined$silinfo

# Silhouette Visuals
sil = silhouette(km_combined$cluster, dist(combined_scaled))
fviz_silhouette(sil)


# Finding optimal K via largest silhouette coefficient
k.max = 15
silh.coef = numeric(k.max)
for(k in 2:15){
  silh.coef[k] = eclust(combined_scaled, FUNcluster = "kmeans", k = k, graph = 0, nstart = 50)$silinfo$avg.width
}

plot(silh.coef, type = "b", pch = 19, col = 4)

which.max(silh.coef)


# K-Mean clustering with K = 14
km_combined=eclust(combined_scaled, FUNcluster = "kmeans", k = 14, nstart=50) # K = 14 Optimal K
km_combined

# Plot clusters
fviz_cluster(km_combined, data = combined_scaled, main = "K-Means Clustering of Combined Features with K=14")

# Silhouette Coefficient
km_combined$silinfo

# Silhouette Visuals
sil = silhouette(km_combined$cluster, dist(combined_scaled))
fviz_silhouette(sil)
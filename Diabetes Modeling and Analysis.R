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
library(factoextra)


# Finding optimal K value
km.out=eclust(ObesityScaled, FUNcluster = "kmeans", nstart=20, nboot=50) # K = 10 Optimal K
km.out


# Plot Gap Statistic
fviz_nbclust(ObesityScaled, kmeans, nstart = 20, nboot = 50, method = "gap_stat")


# Plot clusters
fviz_cluster(km.out, data = ObesityScaled, main = "K-Means Clustering of Obesity Features with K=10")

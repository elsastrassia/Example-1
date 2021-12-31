#Packages
library(pastecs)
library(e1071)
library(vcd)
library(fitdistrplus)
library(tseries)
library(dendextend)
library(colorspace) 
library(dplyr)
library(clValid)
library(factoextra)
library(mltools)

data = read.csv(file ="/Users/elsastrassia/Downloads/DBD RANDOM DATA WEST JAVA - Sheet1 (2).csv") # Input data to be processed in the next stage
data <- data$BODETABEK                             # $ sign is used to select a spesific column from the data table. BODETABEK just an example column. 


# Using functions in R for data analysis
skewness(data)           #it can be used to look at the skewness of the data so that we can see where the data set tends to converge. A positive value indicates that most of the values in the data are gathered on the left
boxplot(data)            #It is used to see the distribution of data and detect outliers  
plot(data, type="l")     # Visualize data in the form of a lines plot

# Create descriptive statistics of data
summary(data)      # Show minimum, first until third quartile, and maximum value from the data
stat.desc(data)    # More completely than summary() 

# Determine the distribution that fits the data
g1 <- goodfit(data,type= 'nbinomial',method= "ML")        # This function means testing the negative binomial distribution on the chi square test using the maximum likelihood method
summary(g1)                                               # Shows the P(> X^2) or p-value of the chi square test where the data fits the negative binomial distribution if the p-value is greater than 0.05
fp1 <- fitdist(data,distr='nbinom',method="mle")          # This function is used to determine the parameter value in the negative binomial distribution using the maximum likelihood method
summary(fp1)                                              # Output of the fitdist function
plot(fp1)                                                 # Visualize the comparison of the probability distribution and the cumulative distribution function between the original data and the data using the negative binomial distribution
legend("bottomright",legend=c("Distribusi Data Asli","Distribusi Binomial Negatif"),cex=0.6,lty=1,  # Added an explanation of black and red colors meaning in the plot
       col=c("black","dark  red"),pch=c(9,NA))


# Generalized Linear Model with predictor variables using Time Series analysis namely Autoregressive (AR)
# Time Series analysis (we use the same data to modelling data with GLM)
adf.test(data)              # Data stationarity test
ts(data)                    # Convert a numeric vector into an time series object                                                                           
acf(data)                   # This function is used to determine the autocorrelation between certain time lags
pacf(data)                  # This function is used to determine the partial autocorrelation between certain time lags

#if the PACF plot shows a cut off at a certain time lag and the ACF plot shows a sinusoidal or exponential pattern, then we can continue with this code
data_GLM = read.csv(file ="/Users/elsastrassia/Downloads/BODETABEK DATA - Sheet1.csv",sep=',')                                       # Input data to be processed in the next stage

summary(nbinom_reg_BODETABEK <- glm.nb(data_GLM$BODETABEK~data_GLM$x, data = data_GLM))                         # It shows parameters of negatif binomial regression that used in BODETABEK data with x is predictor variabel that obtained by determining the PACF at a certain lag
respons <- predict(nbinom_reg_BODETABEK,type="response")                                                        # Predict value of BODETABEK data using negative binomial regression
plot(respons, main = NA,xlab = "Week", ylab = "Jumlah Penderita DBD di Kabupaten Subang",ylim=c(0,600),type="l") # Visualize predictive value of BODETABEK data with negative binomial regression in lines plot
lines(data, col = "red")                                                                                        # Add BODETABEK original data plot at the same frame with predictive value
legend("topleft",legend=c("Nilai aktual kasus DBD di BODETABEK","Nilai kasus DBD dengan regresi binomial negatif"),cex=0.75,lty=1,  # Added an explanation of black and red colors meaning in the plot
       col=c("dark red","black"),pch=c(19,NA))

# Mean Squared Error and Root Mean Squared Error between predictive values and original value of BODETABEK data
mse(respons,data_GLM$BODETABEK)
rmse(respons,data_GLM$BODETABEK)

# Clustering Data with Hierarchical and K-Means Method
data_Cluster = read.csv(file = "/Users/elsastrassia/Downloads/Clustering - Sheet1 (2).csv",sep=",")      # Input data from excel
data_edit_Cluster <- scale(data_Cluster[,-1])                   # Standardize data so that each variable is in the same range
dist_data <- dist(data_edit_Cluster)                            # Calculate Euclidean distance   


# Clustering data with Hierarchical Method
dist_data<-na.omit(dist_data)                              # Removes all incomplete cases of a data object
hc1 <- hclust(dist_data, method="ward.D" )                 # Hierarchical clustering namely Ward method
dend <- as.dendrogram(hc1)                                 # Create dendogram of Ward method
plot(dend)                                                 # Visualize the dendogram
dend <- color_branches(dend, k=3)                          # Create 3 different groups by differentiating the color of each group
species_labels <- data_Cluster[,1]                         
labels(dend) <- paste(as.character(species_labels)[order.dendrogram(dend)], # Create name of each branch according to the name in the first column of table 
                      "(",labels(dend),")", 
                      sep = "")
dend <- set(dend, "labels_cex", 0.4)    # Reduce the size of the labels
plot(dend,horiz=TRUE,cex.axis=0.6)      # Visualize new dendogram horizontally

# Divide 3 groups on the ward method based on the average value
cut_point = cutree(dend, k = 3)  # Cut branches into 3 group
as.data.frame(data_Cluster %>%   # Create a new data frame containing the average value of each group
                mutate(Klaster = cut_point) %>%
                group_by(Klaster) %>% 
                summarise(Mean_Week1 = round(mean(Week1),2), Mean_Week2 = round(mean(Week2),2), Mean_Week3 = round(mean(Week3),2), Mean_Week4 = round(mean(Week4),2),Mean_Week5 = round(mean(Week5),2),Mean_Week6 = round(mean(Week6),2),Mean_Week7 = round(mean(Week7),2),Mean_Week8 = round(mean(Week8),2),Mean_Week9 = round(mean(Week9),2),Mean_Week10 = round(mean(Week10),2),
                          Mean_Week11 = round(mean(Week11),2),Mean_Week12 = round(mean(Week12),2),Mean_Week13 = round(mean(Week13),2),Mean_Week14 = round(mean(Week14),2),Mean_Week15 = round(mean(Week15),2),Mean_Week16 = round(mean(Week16),2),Mean_Week17 = round(mean(Week17),2),Mean_Week18 = round(mean(Week18),2),Mean_Week19 = round(mean(Week19),2),Mean_Week20 = round(mean(Week20),2),
                          Mean_Week21 = round(mean(Week21),2),Mean_Week22 = round(mean(Week22),2),Mean_Week23 = round(mean(Week23),2),Mean_Week24 = round(mean(Week24),2),Mean_Week25 = round(mean(Week25),2),Mean_Week26 = round(mean(Week26),2),Mean_Week27 = round(mean(Week27),2),Mean_Week28 = round(mean(Week28),2),Mean_Week29 = round(mean(Week29),2),Mean_Week30 = round(mean(Week30),2),
                          Mean_Week31 = round(mean(Week31),2),Mean_Week32 = round(mean(Week32),2),Mean_Week33 = round(mean(Week33),2),Mean_Week34 = round(mean(Week34),2),Mean_Week35 = round(mean(Week35),2),Mean_Week36 = round(mean(Week36),2),Mean_Week37 = round(mean(Week37),2),Mean_Week38 = round(mean(Week38),2),Mean_Week39 = round(mean(Week39),2),Mean_Week40 = round(mean(Week40),2),
                          Mean_Week41 = round(mean(Week41),2),Mean_Week42 = round(mean(Week42),2),Mean_Week43 = round(mean(Week43),2),Mean_Week44 = round(mean(Week44),2),Mean_Week45 = round(mean(Week45),2),Mean_Week46 = round(mean(Week46),2),Mean_Week47 = round(mean(Week47),2),Mean_Week48 = round(mean(Week48),2),Mean_Week49 = round(mean(Week49),2),Mean_Week50 = round(mean(Week50),2),
                          Mean_Week51 = round(mean(Week51),2),Mean_Week52 = round(mean(Week52),2),Mean_Week53 = round(mean(Week53),2),Mean_Week54 = round(mean(Week54),2),Mean_Week55 = round(mean(Week55),2),Mean_Week56 = round(mean(Week56),2),Mean_Week57 = round(mean(Week57),2),Mean_Week58 = round(mean(Week58),2),Mean_Week59 = round(mean(Week59),2),Mean_Week60 = round(mean(Week60),2),
                          Mean_Week61 = round(mean(Week61),2),Mean_Week62 = round(mean(Week62),2),Mean_Week63 = round(mean(Week63),2),Mean_Week64 = round(mean(Week64),2),Mean_Week65 = round(mean(Week65),2),Mean_Week66 = round(mean(Week66),2),Mean_Week67 = round(mean(Week67),2),Mean_Week68 = round(mean(Week68),2),Mean_Week69 = round(mean(Week69),2),Mean_Week70 = round(mean(Week70),2),
                          Mean_Week71 = round(mean(Week71),2),Mean_Week72 = round(mean(Week72),2),Mean_Week73 = round(mean(Week73),2),Mean_Week74 = round(mean(Week74),2),Mean_Week75 = round(mean(Week75),2),Mean_Week76 = round(mean(Week76),2),Mean_Week77 = round(mean(Week77),2),Mean_Week78 = round(mean(Week78),2),Mean_Week79 = round(mean(Week79),2),Mean_Week80 = round(mean(Week80),2)))
cut_point                     # Output average value of each group
dunn(dist_data,cut_point)     # Measure how well the data are grouped

# Measure how well the data are grouped with silhouette coefficient
hc.res <- eclust(data_edit_Cluster, "hclust", k = 3, hc_metric = "euclidean",  
                 hc_method = "ward.D", graph = FALSE)
fviz_silhouette(hc.res, palette = "jco", ggtheme = theme_classic())  # Visualize the silhouette coefficient graph of each group member
silinfo<-hc.res$silinfo                                              # Average value of the silhouette coefficient for each group 
silinfo                                                              # Silhouette coefficient for each group member


# Clustering data with K-Means Method
km.res<-kmeans(dist_data,3,nstart=25)                                        # K-means method
km.res
fviz_cluster(km.res, data_edit_Cluster,ellipse.type = "convex",labelsize = 8) # Visualize K-means clustering
rownames(data_edit_Cluster) <- data_Cluster$Kabupaten.Kota                   # Label the chart
dunn(dist_data,km.res$cluster)                                               # Measure how well the data are grouped
sil <- silhouette(km.res$cluster, dist_data)
sil                                                                          # Visualize the silhouette coefficient graph of each group member 
fviz_silhouette(sil)                                                         # Average value of the silhouette coefficient for each group 


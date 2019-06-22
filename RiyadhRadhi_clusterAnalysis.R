#loading required libraries 

library(dplyr)
library(ggplot2)

rm(list = ls())


#1)Loading our data and filtering based on region and selecting our required columns 

countries_df<-read.csv("Countries.csv")

europe_df<-countries_df %>%
  filter(Region == "Europe") %>%
  select(Country.Name,Abbr,Business.Freedom,Labor.Freedom,Monetary.Freedom,Trade.Freedom,Investment.Freedom,Financial.Freedom)


head(europe_df)

#2) Standardize indicators with Z scores 

standarized_europe_df <-data.frame(scale(europe_df[,3:ncol(europe_df)],center = TRUE,scale = TRUE))

head(standarized_europe_df)

#3)distances between countries using "euclidian distance"


distance_europe<- dist(standarized_europe_df, method = "euclidian")

#4)running hierarchical cluster analysis with "complete distance" + Constructing  a dendrogram + drawing rectangles around the clusters 


clustered_europe<- hclust(distance_europe, method = "complete")
clustered_europe

plot(clustered_europe,hang=1)
rect.hclust(clustered_europe,2, border = "blue")


#5)Adding New membership variable to our original europe dataframe 


europe_df$cluster_membership <- cutree(clustered_europe, 2)
head(europe_df)


#6) Perfomring Kmeans clustering + adding new membership variable  

kmeans_europe_cluster <-  kmeans(distance_europe,2)
kmeans_europe_cluster

europe_df$kmeans_cluster_membership <- kmeans_europe_cluster$cluster
head(europe_df)



#7) Compare the two clusterings by creating three different tables 

table(europe_df$cluster_membership)
table(europe_df$kmeans_cluster_membership)
table(hierarchical=europe_df$cluster_membership,Kmeans=europe_df$kmeans_cluster_membership)

#From the tables above, we can see that the hierarchical method are doing better in my opinoin, because 
#it divided our groups in more details and found more distinct features 

#8)Scatter plot of  between Business.Freedom and Investment.Freedom.  
ggplot(europe_df, aes(x= Business.Freedom,y=Investment.Freedom))+
  geom_point(aes(col=europe_df$cluster_membership))+
  geom_text(label=europe_df$Abbr, nudge_x = 1, nudge_y = 1)

#9) Discuss the position of Armenia on the map


armenia_coordination <-data.frame(x_Business.Freedom= europe_df[europe_df$Abbr == "ARM","Business.Freedom"],
                         y_Investment.Freedom=europe_df[europe_df$Abbr == "ARM","Investment.Freedom"])
armenia_coordination


Business.Freedom_mean <-summary(europe_df$Business.Freedom)["Mean"]
Investment.Freedom_mean <-summary(europe_df$Investment.Freedom)["Mean"]

table_comparision<-data.frame(rbind(Business.Freedom_mean,Investment.Freedom_mean,
      Armenia_Business.Freedom = armenia_coordination[,1],
      Armenia_Investment.Freedom =armenia_coordination[,2] ))
colnames(table_comparision) <- "percentage_statistics"

table_comparision
#from the table above, we can actually conclude that Armenia is in good place in terms of both Business freedom
# and investment freedom, as it is above average in both cases and this is good indications.
library(rio)
#import the new customer data
customer <- read.csv("New Customers.csv")

#choose only relevant columns of the data
customer <- customer[ ,c(3,4,6,8,9,15)]
summary(customer)

#change n/a values to NA 
customer[customer == "n/a"] <- NA

#get rid of rows with missing values (NA)
customer <- customer[complete.cases(customer), ]
 
#seperate numerical from character variables
customer_numeric <- customer %>%
  select_if (is.numeric)
customer_character <- customer %>%
  select_if (is.character)

#transform character to numerical with dummy
install.packages("fastDummies")
library (fastDummies)
customer_character <- dummy_cols(customer_character, remove_most_frequent_dummy = TRUE)

#finalize the dataset
customer_numeric <- cbind(customer_numeric, customer_character[, 5:17])

#rename a column
names(customer_numeric)[1:2] <- c("bike_purchases", "age")

#scale the dataset
customer_numeric[ ,1:15] <- scale(customer_numeric[ , 1:15])

#using the elbow rule (determining the number of clusters)
install.packages("factoextra")
library(factoextra)
fviz_nbclust(customer_numeric, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")

#clusters
clusters <- kmeans(customer_numeric, centers = 9,iter.max = 10)
clusters$centers

ggplot(data = customer_numeric) +
  geom_point(aes(x = age, y = bike_purchases))

#export clusters
write.csv(clusters$centers, file = "customer clusters.csv")

#merge cluster with customer data
customer_final <- cbind(customer, clusters$cluster)

#### export
write.csv(customer_final, file = "customers final.csv")

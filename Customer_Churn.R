#--First importing the new churn rfm data----

library(readxl)
sf_rfm_churn_new_data <- read_excel("Desktop/sf_rfm_churn_new_data.xlsx")
View(sf_rfm_churn_new_data)

#IMPORTING THE PRINT_FS_DASHBOARD_CHURN_DATA
library(readxl)
print_FS_dashboard_churn_data <- read_excel("Desktop/print_FS_dashboard_churn_data.xlsx")
View(print_FS_dashboard_churn_data)


#JOINING BOTH THESE DATA FIRST
data1 <- merge(sf_rfm_churn_new_data, print_FS_dashboard_churn_data, by = "sf_account_name",all.x = TRUE)


#Trying a full Join on the data
data_full <- merge(sf_rfm_churn_new_data, print_FS_dashboard_churn_data, by = "sf_account_name", all = TRUE)
colSums(is.na(data_full))

# use 
#all.x = TRUE for left join
#all.y = TRUE for right join
#all = TRUE for full join


#------------------------------------------------
#Checking for missing values in the data

colSums(is.na(data1))
View(data1)

#Exploring the data that contains NAs in Upper Serial Name FS to understand how the rest of the data looks like where we have missing values in Printer Name
data_printer_na = data1[is.na(data1$`Upper Serial Name_FS`),]
View(data_printer_na)
str(data_printer_na)
summary(data_printer_na)


#Dropping the rows with NA's in the printer name


data_new = data1[!(is.na(data1$`Upper Serial Name_FS`)),]
View(data_new)
colSums(is.na(data_new))



#Now exploring the data that still contains NA 

data_na_volume = data_new[is.na(data_new$`Avg. Volume Ml`),]
View(data_na_volume)

#All the NAs in avg volume ml are printers which have not printed,thus replacing the NAs in avg Volume ml and days since last print 

data_new[,14:15][is.na(data_new[,14:15])] = 0
View(data_new)
colSums(is.na(data_new))


#Thus, data_new is our final data set with 0 NAs , 10409 obs and 16 variables


#-------INSTALLING ALL THE REQUIRED PACKAGES AND LIBRARIES BEFORE MOVING AHEAD-----------
install.packages("dplyr")
library(dplyr)
install.packages("corrplot")
library(corrplot)
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
install.packages("ggthemes")
library(ggthemes)
install.packages("caret")
library(caret)
library(MASS)
library(randomForest)
install.packages("party")
library(party)
library(cluster)
library(Rtsne)


#First converting all character data columns into factors

data_final <- data_new%>%
  mutate_if(is.character,as.factor)

str(data_final)


#Now creating the churn column assigning 1 to customers who are likely to churn
#Defining the customers with purchase rate < 0.005 as "churned"

data_churn =  mutate(data_final, churn = ifelse(data_final$`purchase rate` < 0.005, "1", "0"))
View(data_churn)

colSums(is.na(data_churn))

str(data_churn)
#Converting the churn column into factors
data_churn$churn <- as.factor(data_churn$churn)


#Exploring the data


#Finding correlation between the numerical variables 

#First creating a data frame with only numerical variables
data_numerical <- data_churn[-c(1,2,9:12,17)]
View(data_numerical)

#Now plotting the correlation of the numerical variables
install.packages("corrgram")
library(corrgram)
corrgram(data_numerical, order = NULL, panel= panel.shade, text.panel = panel.txt, main = "Correlogram")
cor(data_numerical)

#We could also plot the correlation with this code. Using either of them would be fine
library(GGally)
ggcorr(data_numerical, method = c("everything","pearson"))


#Creating data visualizations for various categorical variables against churn

#Plot1 : count of customers in different rfm categories vs the churn
attach(data_churn)
ggplot(data_churn) + geom_bar(aes(x = rfm_category, fill = churn), position = "dodge")

#The count of accounts with respect to the different rfm categories and churn
table(rfm_category,churn)

#Plot 2 : Printed vs churn

ggplot(data_churn) + geom_bar(aes(x = `printed?`, fill = churn), position = "dodge")


data_churn%>%
  group_by(churn, `printed?`)%>%
  summarise(count= n())

#Plot 3 : Onboarded vs churn

ggplot(data_churn) + geom_bar(aes(x = `Onboarded?`, fill = churn), position = "dodge")
data_churn%>%
  group_by(churn, `Onboarded?`)%>%
  summarise(count= n())

#Plot4 : dashboard registered vs churn

ggplot(data_churn) + geom_bar(aes(x = dashboard_registered, fill = churn), position = "dodge")

data_churn%>%
  group_by(churn, dashboard_registered)%>%
  summarise(count= n())


#Exploring the numerical data across different rfm categories and churn
df <- data_churn%>%
  group_by(rfm_category, churn)%>%
  summarise_at(vars(-sf_account_name,-`Upper Serial Name_FS`,-`printed?`,-`Onboarded?`,-dashboard_registered,-rfm_category,-churn), funs(mean))
View(df)


#Exploring the mean of the numerical variables w.r.t churn
df1 <- data_churn%>%
  group_by(churn)%>%
  summarise_at(vars(-sf_account_name,-`Upper Serial Name_FS`,-`printed?`,-`Onboarded?`,-dashboard_registered,-rfm_category,-churn), funs(mean))
View(df1)



#Clustering the data to get more insights on user characteristics. As we have mixed data type(containing both numerical and categorical variables), we will use PAM method, gower distance for clustering
data_clustering <- data_churn[-c(1,9)]
View(data_clustering)


gower_dist <- daisy(data_clustering,
                    metric = "gower",
                    type = list(logratio = 5))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

#Output the most similar pair
data_clustering[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


#Now output the most dissimilar pair

data_clustering[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

#Thus, based on silhouette width, 3 clusters yield the highest value
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

pam_results <- data_clustering %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

data_clustering[pam_fit$medoids,]

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))



#The clustering with k=3, is not looking that intuitive, so trying to create 4 clusters

pam_fit1 <- pam(gower_dist, diss = TRUE, k = 4)
pam_results1 <- data_clustering %>%
mutate(cluster = pam_fit1$clustering) %>%
group_by(cluster) %>%
do(the_summary = summary(.))

pam_results1$the_summary

data_clustering[pam_fit1$medoids,]

#Plotting the clusters
tsne_obj1 <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data1 <- tsne_obj1$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster1 = factor(pam_fit1$clustering))


ggplot(aes(x = X, y = Y), data = tsne_data1) + geom_point(aes(color = cluster1))

#This looks more intuitive. As it can be seen there are a lot of small clusters , but clustering them into a larger number is not intuitive

#Now implementing the classification model

#First splitting the data set
install.packages("caret")
library(caret)
intrain<- createDataPartition(data_churn$churn,p=0.7,list=FALSE)
set.seed(2018)
training<- data_churn[intrain,]
testing<- data_churn[-intrain,]

#Confirming the split
dim(training); dim(testing)

#It is important that the character data types are converted into factors before modeling the data. Thus, checking the structure of the training set before going ahead

str(training)
detach(data_churn)
attach(training)

#Implementing the random Forest
set.seed(2018)
rf4 <- randomForest(churn ~ `account average order size` + `account days since last order` + `account sales` + `days since last print` + `Avg. Volume Ml` + COUNTOFPRINTS+ rfm_category + `printed?` + dashboard_registered + `Onboarded?`, data = training, importance = TRUE)
print(rf4)
importance(rf4)
varImpPlot(rf4)


#Making predictions with the random forest
predrf4 <- predict(rf4, testing)
caret::confusionMatrix(predrf4 , testing$churn)


#Calculating the AUC for the RF4
library(ROCR)
predictions = as.vector(rf4$votes[,2])
pred = prediction(predictions, churn)
perf = performance(pred, "auc")
AUC = perf@y.values[[1]]
perf_ROC = performance(pred, "tpr", "fpr")
plot(perf_ROC, main = "ROC plot")
text(0.5,0.5, paste("AUC = ", format(AUC, digits = 5, scientific = FALSE)))



#Trying decision tree model with the same predictors
CART_model2=rpart(training$churn ~ `account average order size`+`account days since last order` + `account sales`+ `Avg. Volume Ml` + COUNTOFPRINTS + `days since last print` + `printed?` + `Onboarded?`+dashboard_registered + rfm_category , data= training, method="class")
prp(CART_model2)
summary(CART_model2)


predict_CART_test2=predict(CART_model2,newdata=testing, type="class")
table(testing$churn,predict_CART_test2)

#The Logistic regression model

logistic = glm(churn ~ `account average order size` + `account days since last order` + `account sales` + `days since last print` + `Avg. Volume Ml` + COUNTOFPRINTS + rfm_category + `printed?` + dashboard_registered + `Onboarded?`, family = "binomial", data = training)
summary(logistic)

anova(logistic, test = "Chisq")

fitted.results = predict(logistic, newdata = testing, type = 'response')
fitted.results = ifelse(fitted.results >0.5,1,0)
misclasserror = mean(fitted.results != testing$churn)
1 - misclasserror

table(testing$churn, fitted.results>0.5)


#Now finding the Odds ratio, which would give us the odds of an event happening

library(MASS)

exp(cbind(OR = coef(logistic), confint(logistic)))



#Implementing RF5 (a random forest model) with only the print and dashboard data

rf5 = randomForest(churn ~ `days since last print` + `Avg. Volume Ml` + COUNTOFPRINTS+ rfm_category + `printed?` + dashboard_registered + `Onboarded?`, data = training, importance = TRUE)
print(rf5)
importance(rf5)
varImpPlot(rf5)




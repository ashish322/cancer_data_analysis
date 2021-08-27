#Libraries
library(datasets) 
library(caTools)
library(party)
library(dplyr)
library(magrittr)
library(caret)

#Load the data and create factor
data = read.csv('tumors_csv_transposed_updated.csv')
head(data)
attach(data)
data$Cancer_Type = as.factor(data$Cancer_Type)

#Divide the data into train and test
set.seed(123)
t = createDataPartition(Cancer_Type, p=0.8, list = FALSE)
train = na.omit(data[t, ])
test = na.omit(data[-t, ])

#Scale the data except the last column
test[, -c(2309)] <- scale(test[, -c(2309)])
train[, -c(2309)] <- scale(train[, -c(2309)])


#The true y
y_true = test$Cancer_Type

#Apply the model on train data
model<- ctree(Cancer_Type ~ ., train)

plot(model)

#Predict the test set
y_pred = predict(model, newdata = test)


#Confusion matrix
conf_matrix=confusionMatrix(y_true, y_pred)
conf_matrix
conf_matrix$byClas


#Model Accuracy
mtx <- table(y_true, y_pred)
ac_Test <- sum(diag(mtx)) / sum(mtx)
print(paste('Accuracy for test using Decision Tree is found to be', ac_Test*100 , '%'))

#Apply PCA
df_pca=prcomp(data[,-2309])
df_pca
scores=df_pca$x
summary(df_pca)


#Screen Plot
screeplot(df_pca, type = "l", npcs = 40, main = "Screeplot of the first 40 PCs")
abline(h = 18, col="red", lty=5)
cumulative_pro = cumsum(df_pca$sdev^2 / sum(df_pca$sdev^2))
plot(cumulative_pro[0:30], xlab = "PCs", ylab = "Explained variance", main = "Cumulative variance plot")
abline(v = 30, col="blue", lty=5)
abline(h = 0.91026, col="blue", lty=50)
legend("bottomright", legend=c("Cut-off @ PC30"),col=c("red"))

#Apply pca on training and test data
pca_scores=scores[,1:30] 
smp_size=floor(0.80 * nrow(pca_scores)) 
set.seed(1) 
ind=sample(seq_len(nrow(pca_scores)), size = smp_size) 
y=data[ind, ]$Cancer_Type 
y_actual=data[-ind, ]$Cancer_Type 
train=pca_scores[ind, ] 
test=pca_scores[-ind, ]

test = as.data.frame(test)
train = as.data.frame(train)

#Apply the model
model<- ctree(y ~ ., train)
plot(model)

#Predict the output
y_predicted = predict(model, newdata = test)


#Confusion matrix
conf_matrix_pca=confusionMatrix(y_predicted,y_actual)
conf_matrix_pca
conf_matrix_pca$byClas

#Model Accuracy adter pca
mtx_pca <- table(y_predicted, y_actual) 
ac_Test_pca <- sum(diag(mtx_pca)) / sum(mtx_pca)
print(paste('Accuracy for test using Decision Tree after applying pca is found to be', ac_Test_pca*100 , '%'))
print(paste('Accuracy for test using Decision Tree is found to be', ac_Test*100 , '%'))





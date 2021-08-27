# Random Forest Classification

# Importing the dataset
dataset = read.csv('tumors_csv_updated.csv')
dataset = dataset[2:65]

# Encoding the target feature as factor
#dataset$Cancer_Type = factor(dataset$Cancer_Type, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Cancer_Type, SplitRatio = 0.80)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

training_set = dataset[1:63,]
test_set = dataset[64:2307,]




# Feature Scaling
training_set[-64] = scale(training_set[-64])
test_set[-64] = scale(test_set[-64])

# Fitting Random Forest Classification to the Training set
install.packages('randomForest')
library(randomForest)
set.seed(123)
#install.packages('rpart')
#library(rpart)



classifier = randomForest(x = training_set[-64],
                          y = training_set$Cancer_Type,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-64])

# Making the Confusion Matrix
cm = table(test_set[, 64], y_pred)

# Visualising the Training set results(To be updated)
install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('C1', 'C2')
y_grid = predict(classifier, grid_set)
plot(set[, -3],
     main = 'Random Forest Classification (Training set)',
     xlab = 'C1', ylab = 'C2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results(To be updated)
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('C1', 'C2')
y_grid = predict(classifier, grid_set)
plot(set[, -3], main = 'Random Forest Classification (Test set)',
     xlab = 'C1', ylab = 'C2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Choosing the number of trees
plot(classifier)

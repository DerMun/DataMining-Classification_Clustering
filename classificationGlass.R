rm(list = ls()) 

library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)

glass <- read.csv("C:/dev/glass.csv")

#convert target variable to factor
glass$Type <- as.factor(glass$Type)


#pre processing
#glass <- glass[glass$Type != 6, ]
#glass$Type <- droplevels(glass$Type)


###Exploratory Data Analysis (EDA)

dim(glass)
str(glass)
summary(glass)
levels(glass$Type)

#histograms for each numeric feature
n_numeric <- ncol(glass) - 1
rows <- ceiling(sqrt(n_numeric))
cols <- ceiling(n_numeric/rows)

#reduced margins (left, bottom, top, right)
par(mfrow = c(rows, cols), mar = c(2, 2, 1, 1), mgp = c(1, 0.5, 0))

for (i in 1:n_numeric) {
  hist(glass[, i],
       main = "",
       xlab = names(glass)[i],
       col = "skyblue",
       breaks = 15,
       cex.axis = 0.8,
       cex.lab = 0.8)
}

#boxplots by Type for some variables
boxplot(glass$RI ~ glass$Type, xlab = "Glass Type", ylab = "RI")
boxplot(glass$Na ~ glass$Type, xlab = "Glass Type", ylab = "Sodium (Na)")
boxplot(glass$Mg ~ glass$Type, xlab = "Glass Type", ylab = "Magnesium (Mg)")

dev.off()#closes the active graphics device and clears all plots


#train/test
set.seed(1)
index <- createDataPartition(glass$Type, p = 0.8, list = FALSE)
trainset <- glass[index, ]
testset <- glass[-index, ]

#data dimensions
cat("Train set:", dim(trainset), "\n")
cat("Test set:", dim(testset), "\n")


#sample division
table(glass$Type)
table(trainset$Type)
table(testset$Type)


#model training
fitControl <- trainControl(method = "cv", number = 10)

model <- train(Type ~ ., data = trainset,
               method = "rpart",
               trControl = fitControl,
               tuneLength = 10)


print(model)

#show tree
tree_model <- model$finalModel

rpart.plot(tree_model,
           type = 2,
           extra = 102,
           fallen.leaves = TRUE,
           box.palette = "Blues",
           tweak = 1.2,
           branch = 0.3,
           shadow.col = "transparent")


#confusion matrices
pred_train <- predict(model, trainset)
pred_test <- predict(model, testset)

cat("Confusion Matrix - Train:\n")
print(confusionMatrix(pred_train, trainset$Type))

cat("Confusion Matrix - Test:\n")
print(confusionMatrix(pred_test, testset$Type))


source("model.R")

library(pROC)
library(caret)

train = read_csv("train.csv")
test = read_csv("test.csv")


# ROC analysis
test$y = test$subtype
pred = predict(model, newdata=test, type="probs")
r = multiclass.roc(test$y, pred)
auc(r)


# Confusion matrix
test$y = test$subtype
pred = predict(model, newdata=test, type="class")
cm = confusionMatrix(as.factor(pred), as.factor(test$y))
cm

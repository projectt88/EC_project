library(nnet)
library(readr)
library(tidyr)
library(dplyr)

formula = y ~ cg25837710 + cg11224603 + cg19651132 + cg06633615 + cg07616879 + 
  cg06590608 + cg21759280 + cg05705018 + cg08339497 + cg18746831 +
  cg06108510 + cg20392607 + cg09493063


res_data = read_csv("https://raw.githubusercontent.com/projectt88/EC_project/main/resampled_data.csv")
beta = res_data[,!colnames(res_data) %in% c("...1", "subtype")]
model_CpG = colnames(beta) # call selected CpG for later uses
y = res_data$subtype # call ground-truth classes

set.seed(4) # Noise regularization
x = as.matrix(beta) + matrix(rnorm(length(beta),0,sd=0.1), nrow=nrow(beta), ncol=ncol(beta))
x = as.data.frame(x)
x$y = y

# fit the model
model = multinom(formula, x)


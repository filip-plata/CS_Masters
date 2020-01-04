library(glmnet)
library(car)
library(randomForest)
library(FNN)
library(MASS)
library(doParallel)
library(caret)
library(lmvar)

# protein
load("protein.RData")

protein_y = 2001
protein.train.X <- data.train[, -c(protein_y)]
protein.train.Y <- data.train[, protein_y]
protein.test.X <- data.test

# linear dependence
protein.lm <- lm(protein.train.Y ~ protein.train.X)
protein.ld.vars <- attributes(alias(protein.lm)$Complete)$dimnames[[1]]

protein.cv.glmnet <- cv.glmnet(protein.train.X, protein.train.Y)
protein.lambda_opt <- protein.cv.glmnet$lambda.1se
# Done by looking at data by hand. R is the worst programming language I have seen.
# Even Smalltalk is better.
protein.lambda_opt_idx <- 55
protein.lasso.mse <- protein.cv.glmnet$cvm[protein.lambda_opt_idx]

protein.lasso <- glmnet(protein.train.X,
                        protein.train.Y,
                        family="gaussian")
protein.lasso.pred <- predict(protein.lasso, protein.test.X)[,1]
# Source for protein important variables, comment out lambda_opt above
#library(plotmo)
#plot_glmnet(protein.lasso)

# Top 5 predictors in random order
predictors.protein <- c("x821",  "x1395", "x860", 
                        "x848", "x1982")

protein.five.train.X <- data.train[, predictors.protein]
protein.five.test.X <- data.test[, predictors.protein]

# Linear model for protein
protein.lm.df <- data.frame(
    x = protein.five.train.X, 
    y = protein.train.Y)
protein.lm <- lm(y~ ., data = protein.lm.df, x = TRUE, y = TRUE)

protein.lm.mse <- cv.lm(protein.lm)$MSE$mean

protein.lm.prediction <- predict(
  protein.lm, data.frame(x = protein.five.test.X))

# Random forest for protein
protein.rf.ntree <- 5000
protein.rf <- randomForest(protein.five.train.X,
                           protein.train.Y,
                           mtry = 3, maxnodes = 100, ntree = protein.rf.ntree)
protein.rf.pred <- predict(protein.rf, protein.five.test.X)
protein.rf.mse <- protein.rf$mse[protein.rf.ntree]

# Predictions
pred.protein <- protein.lm.prediction

# VIF. Method from car package needs linear model built from dataframe,
# so thats what we do
protein.df <- data.frame(x = protein.five.train.X, y = protein.train.Y)
protein.lm <- lm(y ~ ., data = protein.df)
protein.vif <- vif(protein.lm)
hist(protein.vif)

# cancer
load("cancer.RData")

cancer_y = 17738
cancer.col = cancer_y - 1
cancer.train.list <- data.train
cancer.train.X <- matrix(unlist(data.train[, -c(cancer_y)]), ncol = cancer.col, byrow = TRUE)
cancer.train.Y <- matrix(unlist(data.train[, cancer_y]), ncol = 1, byrow = TRUE)[,1]
cancer.test.X <- matrix(unlist(data.test), ncol = cancer.col, byrow = TRUE)

cancer.scaled.train.X <- scale(cancer.train.X)
cancer.scaled.train.Y <- scale(cancer.train.Y)

# Selecting redictors using separate linear models

cancer.pred.split <- 1000
cancer.lm.preds <- c()
cancer.df <- data.frame(x = cancer.train.X, y = cancer.train.Y)

cancer.do_aic <- function(df, steps = steps) {
  cancer.lm.start <- lm(y ~ 1, data = cancer.lm.df, y = TRUE, x = TRUE)
  cancer.lm.end <- lm(y ~ ., data = cancer.lm.df)
  return (stepAIC(cancer.lm.start, steps = steps, scope = list(
    upper=cancer.lm.end, lower=cancer.lm.start), trace = FALSE))
}

for (start in seq(from = 1, to = cancer.col, by = cancer.pred.split)) {
  end = min(start + cancer.pred.split, cancer.col)
  cancer.lm.df <- cancer.df[c(start:end, cancer_y)]
  cancer.lm.first <- cancer.do_aic(cancer.lm.df, steps = 30)
  cancer.lm.preds <- c(cancer.lm.preds, names(cancer.lm.first$coefficients)[-(1:1)])
}

cancer.pred.trim <- function(preds) {
  pmin(pmax(preds, rep(c(0), times = length(preds))), rep(c(1), times = length(preds)))
}

cancer.df <- data.frame(x = cancer.train.X, y = cancer.train.Y)
cancer.lm.df <- cancer.df[c(cancer.lm.preds, "y")]
cancer.lm <- cancer.do_aic(cancer.lm.df, steps = 100)
cancer.lm.mse <- cv.lm(cancer.lm)$MSE$mean
cancer.lm.pred <- predict(
    cancer.lm, data.frame(x = cancer.test.X)[cancer.lm.preds])

# VIF
cancer.vif <-  vif(cancer.lm)
hist(cancer.vif)
  
# Calculating most important predictors set. Because of that, we use parallelization
registerDoParallel(makeCluster(detectCores() - 1))
cancer.rf.importance <- foreach(ntree=rep(300, 3), .combine=randomForest::combine,
              .multicombine=TRUE, .packages='randomForest') %dopar% {
                randomForest(cancer.train.X,
                             cancer.train.Y,
                             ntree = ntree,
                             importance = TRUE)
}

# Most important predictors
cancer.predictors.count <- 100
cancer.predictors_idx <- head(order(-importance(
    cancer.rf.importance, type = 1)), n=cancer.predictors.count)
cancer.predictors <- colnames(cancer.train.list)[cancer.predictors_idx]

cancer.main_predictors.train.X <- cancer.train.X[,cancer.predictors_idx]
cancer.main_predictors.test.X <- cancer.test.X[,cancer.predictors_idx]

cancer.rf.train <- sample(1:629, 500, replace=FALSE)
cancer.rf.trees <- 1000
cancer.rf <- randomForest(cancer.main_predictors.train.X[cancer.rf.train,],
                          cancer.train.Y[cancer.rf.train],
                          ntree = cancer.rf.trees)
cancer.rf.pred <- predict(cancer.rf, cancer.main_predictors.test.X)
cancer.rf.mse <- mean((predict(
    cancer.rf, cancer.main_predictors.train.X[-cancer.rf.train,]) - 
      cancer.train.Y[-cancer.rf.train]) ^ 2) # or cancer.rf$mse[cancer.rf.trees]

# This create knnRegCv object. Docs are vague, but it should
# construct predictions by cross validation, because there
# is no other way for knn
cancer.knn <- knn.reg(scale(cancer.main_predictors.train.X), 
                      y = cancer.train.Y, k = 15)
cancer.knn.pred <- cancer.knn$pred
cancer.knn.mse <- cancer.knn$PRESS / length(cancer.train.Y)

# Predictions
pred.cancer <- cancer.pred.trim(cancer.lm.pred)

# common

load(file = "plata.RData")
save(pred.cancer, pred.protein, predictors.protein, 
     file = "plata.RData")

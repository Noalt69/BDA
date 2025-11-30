library(corrplot)
library(VIM)
library(caret)
library(ggplot2)
library(dplyr)
library(rstan)
library(gridExtra)

setwd("C:/Users/An/OneDrive/Desktop/Master/BDA/Project")

data_pooled<- read.csv("data_cleaned.csv")


pred_pooled <- setdiff(names(data), c("Risk"))
data_pooled[pred_pooled] <- scale(data[pred_pooled])

split_data <- function(df, target_cols, proportion = 0.5) {
  n <- nrow(df)
  train_size <- round(n * proportion)
  train <- head(df, train_size)
  test <- tail(df, n - train_size)
  
  list(
    train = train,
    test = test,
    y_train = train[[target_cols]],
    y_test = test[[target_cols]]
  )
}

# Pooled Model----
pooled_split <- split_data(data_pooled, "Risk")

X_train_pooled <- subset(pooled_split$train, select = -Risk)
X_test_pooled  <- subset(pooled_split$test, select = -Risk)

credit_data_pooled <- list(
  N_train = nrow(X_train_pooled),
  K = ncol(X_train_pooled),
  X_train = X_train_pooled,
  y_train = pooled_split$y_train,
  N_test = nrow(X_test_pooled),
  X_test = X_test_pooled
)

pooled_fit <- stan(
  file = "pooled.stan",
  data = credit_data_pooled,
  chains = 4, iter = 2000, warmup = 1000
)

fit_pooled <- summary(pooled_fit)$summary

saveRDS(pooled_fit, file = "pooled_fit.rds")


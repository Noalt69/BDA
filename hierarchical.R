
library(corrplot)
library(VIM)
library(caret)
library(ggplot2)
library(dplyr)
library(rstan)
library(gridExtra)

setwd("C:/Users/An/OneDrive/Desktop/Master/BDA/Project")

data_hierarchical <- read.csv("data_cleaned.csv")

pred_hierarchical <- setdiff(names(data), c("Risk", "Job"))
data_hierarchical["Job"] <- lapply(data_hierarchical["Job"], function(x) x +1)
data_hierarchical[pred_hierarchical] <- scale(data[pred_hierarchical])

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

# Hierarchical Model----
hier_split <- split_data(data_hierarchical, "Risk")

X_train <- subset(hier_split$train, select = -c(Risk, Job))
X_test  <- subset(hier_split$test, select = -c(Risk, Job))
ll_train <- hier_split$train$Job
ll_test  <- hier_split$test$Job

credit_data_hierarchical <- list(
  N_train = nrow(X_train),
  K = ncol(X_train),
  L_train = length(unique(ll_train)),
  X_train = as.matrix(X_train),
  ll_train = ll_train,
  y_train = hier_split$y_train,
  N_test = nrow(X_test),
  L_test = length(unique(ll_test)),
  X_test = as.matrix(X_test),
  ll_test = ll_test
)

hierarchical_fit <- stan(
  file = "C:/Users/An/OneDrive/Desktop/Master/BDA/Project/hierarchical.stan",
  data = credit_data_hierarchical,
  chains = 4, iter = 3000, warmup = 1000
)

# Diagnostics----
fit_hierarchical <- summary(hierarchical_fit)$summary

#Save 
saveRDS(hierarchical_fit, file = " hierarchical_fit.rds")


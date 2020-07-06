# Training the chained classification and regression models with subjective splits
# Goal - use folCount split labels as categories for classification,
# then perform regression on each class to determine numeric folCount

# Split by powers of 10
breaks <- c(0,1e2,1e3,1e4,1e5,1e9)
splits <- cut(df$folCount, breaks, labels=c(1,2,3,4,5), right=FALSE)
nsplit <-length(levels(splits))
summary(splits)

# Add labels to working df
df.cat <- cbind(df,splits)

# Build RF regression model on train data to predict folCount within each split class
rmse.rf.splt <- vector(length = nsplit, mode = 'numeric')
rf.splt <- list()
formula.full <- formula(folCount~frCount+statCount+favCount+createdDate+verifiedBool+protectedBool)
df.cat.train <- df.cat[train,]
df.cat.test <- df.cat[test,]
for (i in 1:nsplit){
  split.rows.train <- which(df.cat.train$splits==i)
  split.rows.test <- which(df.cat.test$splits==i)
  rf.splt[[i]] <- randomForest(formula.full, data = df.cat.train[split.rows.train,], ntree=1000)
  rf.splt.pred <- predict(rf.splt[[i]], newdata = df.cat.test[split.rows.test,])
  rmse.rf.splt[i] <- rmse(df.cat.test$folCount[split.rows.test], rf.splt.pred)
}

# Build RF classification model with train data to predict split class
formula.split <- formula(splits~frCount+statCount+favCount+createdDate+verifiedBool+protectedBool)
rf.cat <- randomForest(formula.split, data=df.cat[train,], ntree=4000)
rf.cat.pred <- predict(rf.cat, newdata=df.cat[test,])

cm <- table(df.cat$splits[test], rf.cat.pred)
acc.rf <- sum(diag(cm))/sum(cm)
cm


# Predict numeric folCount using regression model associated with predicted split class (RF Reg)
df <- df.base[test,]
df <- df[order(df$folCount),]
rf.cat.pred <- predict(rf.cat, newdata=df[test,])
final.pred <- vector()
df.cat.pred <- cbind(df[test,],splitPred=rf.cat.pred)
rmse.rf.predcat <- vector('numeric', nsplit)
for (i in 1:nsplit){
  split.rows <- which(df.cat.pred$splitPred==i)
  rf.splt.pred <- predict(rf.splt[[i]], newdata = df.cat.pred[split.rows,])
  rmse.rf.predcat[i] <- rmse(df.cat.pred$folCount[split.rows], rf.splt.pred)
  final.pred <- c(final.pred,rf.splt.pred)
}
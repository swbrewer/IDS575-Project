load(file='/Users/cymorene/Documents/UIC/2018Spring/IDS575/Project/rf.splt')
load(file='/Users/cymorene/Documents/UIC/2018Spring/IDS575/Project/rf.cat')

# rf.cat predicts category for new data
# rf.splt conducts regression on new data within each predicted category

nrow(df.base[train,])
nrow(df.base[test,])
length(train)
#df.new <- df

# Order 
df <- df.base[test,]
df.ord <- df[order(df$folCount),]

ord.pred <- predict(rf.cat, newdata=df.ord)
df.ord <- cbind(df.ord,splitPred=ord.pred)

chain.pred <- vector()
rmse.chain <- vector('numeric', nsplit)
for (i in 1:nsplit){
  split.rows <- which(df.ord$splitPred==i)
  rf.splt.pred <- predict(rf.splt[[i]], newdata = df.ord[split.rows,])
  rmse.chain[i] <- rmse(df.ord$folCount[split.rows], rf.splt.pred)
  chain.pred <- c(chain.pred,rf.splt.pred)
}
#rmse.newdata <- rmse.rf.predcat
#rmse.newdata1 <- rmse.rf.predcat

hist(as.numeric(df.cat.pred$splitPred))
summary(df)
nrow(df)

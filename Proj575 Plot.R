df.plot <- data.frame(orig.tf=log.plus1(df.ord$folCount),
                      chain.pred.tf=log.plus1(chain.pred),
                      rf.pred.tf=log.plus1(rf.pred))

plot(1:nrow(df.plot), df.plot$orig.tf, type='l',xlab='Observations',ylab='Log(FollowerCount)')
title('Log Transform of Follower Count and Predictions',cex=.8)
colors <- c('black','blue','red')
legNames <- c('Original','RF','RF Class + RF Reg')
legend("topleft", lty=1, col=colors,legend=legNames, bty='y', cex=.8)
lines(1:nrow(df.plot), df.plot$rf.pred.tf, type='l',col=colors[2])
lines(1:nrow(df.plot), df.plot$chain.pred.tf, type='l',col=colors[3])
lines(1:nrow(df.plot), df.plot$orig.tf, type='l', col=colors[1])



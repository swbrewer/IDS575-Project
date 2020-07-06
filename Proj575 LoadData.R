# Reload data
file_friend <- '3blue1brown_friends.csv'
file_follower <- '3blue1brown_followers.csv'
data_fr <- read.csv(paste0('/Users/cymorene/Documents/Share.nosync/',file_friend))
data_fo <- read.csv(paste0('/Users/cymorene/Documents/Share.nosync/',file_follower))

# Tag source before combine (probably not important, but just in case)
data_fr$file_status <- 'friend'
data_fo$file_status <- 'follower'

# Combine and write
df.input <- rbind(data_fr, data_fo)
write.csv(df.input, file=paste0('/Users/cymorene/Documents/Share.nosync/',userName,'_combo.csv'), row.names = FALSE)

# Input data
data <- read.csv(file=paste0('/Users/cymorene/Documents/Share.nosync/',userName,'_combo.csv'))

# Check for duplicates by screen name
dupes <- duplicated(data$screenName)
sum(dupes)

# Strip out dupes by screen name
# It flags occurences after first with TRUE
data <- data[-which(dupes),]

# Remove id, sceen name, and friend/follower tag.
# Convert dates from seconds since 1/1/70 to days since 1/1/70
# Transfrom large range field by log(x+1) (to account fo zeros)
df <- data[,3:9]
df$createdDate <- as.numeric(as.Date(as.POSIXct(df$createdDate, origin='1970-1-1')))
#df.base <- df # Storing df before transforming it
summary(df)

# Declare functions for transformations and error calcs 
log.plus1 <- function(baseval) log(baseval+1) 

exp.minus1 <- function(logxplus1val) exp(logxplus1val)-1

rmse <- function(actual_val, pred_val) mean(sqrt((actual_val-pred_val)^2))


#df$createdDate <- log(df$createdDate+1)
df$frCount <- log.plus1(df$frCount)
df$folCount <- log.plus1(df$folCount)
df$statCount <- log.plus1(df$statCount)
df$favCount <- log.plus1(df$favCount)

df.tf <- df # Store transformed df for later
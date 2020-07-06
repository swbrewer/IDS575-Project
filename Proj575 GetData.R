library(twitteR)

# Log in as 'swbrewer'
api_key <- "c2eiHpCAjrTQ0jX8AasPfzUoG"
api_secret <- "RqMjyMMTg84ktM1JLlz14TUhbPMsb49tv3l8UWxDzgovTol6BH"
access_token <- "15043068-tzRAOCaQQrgz0gfZeOFbANDGJ0dG0EknHftXKlUvA"
access_token_secret <- "y43mE6qZOd6DUa7jetlXreWbevCcUS4OEwzCEbe1VI95D"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

userName <- 'BigDataGal'
user <- getUser(userName)
friends <- user$getFriends()
followers <- user$getFollowers()

n <- length(friends)
# Initialize columns of user data
id <- rep(NA, n)
screenName <- rep(NA, n)
frCount <- rep(NA, n)
statCount <- rep(NA, n)
favCount <- rep(NA, n)
createdDate <- rep(NA, n)
folCount <- rep(NA, n)
verifiedBool <- rep(NA, n)
protectedBool <- rep(NA, n)

# Build user data frame of friends
for (i in 1:n) {
  #afriend <- getUser(friends[i]) # Don't use - calls twitter api resulting in rate limit after ~1000 calls
  id[i] <- friends[[i]]$getId()
  screenName[i] <- friends[[i]]$screenName
  frCount[i] <- friends[[i]]$friendsCount
  folCount[i] <- friends[[i]]$followersCount
  statCount[i] <- friends[[i]]$statusesCount
  favCount[i] <- friends[[i]]$favoritesCount
  createdDate[i] <- friends[[i]]$created
  verifiedBool[i] <- friends[[i]]$verified
  protectedBool[i] <- friends[[i]]$protected
}

# Combine columns into data frame. Note: cbind was specifically avoided due to being bad practice
df.input <- data.frame(id=id,
                 screenName=screenName,
                 frCount=frCount,
                 folCount=folCount,
                 statCount=statCount,
                 favCount=favCount,
                 createdDate=createdDate,
                 verifiedBool=verifiedBool,
                 protectedBool=protectedBool,
                 stringsAsFactors = FALSE)
write.csv(df.input, file=paste0('/Users/cymorene/Documents/Share.nosync/',userName,'_friends.csv'), row.names = FALSE)


# Build user data frame of followers
n <- length(followers)
for (i in 1:n) {
  #afollower <- getUser(followers[i])  # Don't use - calls twitter api resulting in rate limit after ~1000 calls
  id[i] <- followers[[i]]$id
  screenName[i] <- followers[[i]]$screenName
  frCount[i] <- followers[[i]]$friendsCount
  folCount[i] <- followers[[i]]$followersCount
  statCount[i] <- followers[[i]]$statusesCount
  favCount[i] <- followers[[i]]$favoritesCount
  createdDate[i] <- followers[[i]]$created
  verifiedBool[i] <- followers[[i]]$verified
  protectedBool[i] <- followers[[i]]$protected
}
# Combine columns into data frame. Note: cbind was specifically avoided due to being bad practice
df.input <- data.frame(id=id,
                 screenName=screenName,
                 frCount=frCount,
                 folCount=folCount,
                 statCount=statCount,
                 favCount=favCount,
                 createdDate=createdDate,
                 verifiedBool=verifiedBool,
                 protectedBool=protectedBool,
                 stringsAsFactors = FALSE)
write.csv(df.input, file=paste0('/Users/cymorene/Documents/Share.nosync/',userName,'_followers.csv'), row.names = FALSE)

#BIOL933
#Lab 2
#Example 1
# use other data set , same method
ques1_dat <- read.table('oats.txt', header = TRUE)
#import, then inspect the data
str(ques1_dat, give.attr=F)

#convert the data into a dataframe, then reinspect
ques1_dat<-as.data.frame(ques1_dat)
str(ques1_dat, give.attr=F)

# Tell R that "Sample" is a FACTOR
ques1_dat$Block<-as.factor(ques1_dat$Block)
str(ques1_dat, give.attr=F)

# Find the requested means
means <- aggregate(ques1_dat$y, list(ques1_dat$Block), mean)

# Find other useful info...
ids <- unique(ques1_dat$Block)
ns <- aggregate(ques1_dat$y, list(ques1_dat$Block), length)

# Compile the results into a summary dataframe
summary <- data.frame(ids,means$x,ns$x)
names(summary)[1:3] <- c("Block","Mean","n")
summary

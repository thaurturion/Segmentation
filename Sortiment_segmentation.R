####Generate synthetic dataset

numOfProds = 30
numOfCategories = 15

data = data.frame(seq(1:numOfProds))

for (i in 1:numOfCategories) {
  set.seed(i)
  #data = cbind(data, as.factor(as.logical(sample(0:1, numOfProds, replace=T))))
  data = cbind(data, as.logical(sample(0:1, numOfProds, replace=T)))
}

#generate random product revenue shares
revenueShare = runif(numOfProds, 0, 1)
revenueShare = revenueShare/sum(revenueShare)
data = cbind(data, revenueShare)

#generate proper column names
colnames(data) <- c("Produkt_ID", paste("Category_", letters[1:numOfCategories]), "Revenue_Share")

###Finished generating data, now proceed with converting relative revenue share
View(data)

hist(data$Revenue_Share)
#manual threshold after looking at histogram
poor = 0.02
medium = 0.05
#top is > medium
splitData <- function (dat) {
  if (dat <= poor) {
    return("poor")
  } else if(dat <= medium) {
    return("medium")
  }
  else {
    return("strong")
  }
}

#data_y (revenue share) must be as factor
data$Revenue_Share = as.factor(unlist(lapply(data$Revenue_Share, splitData)))

dataWithoutID = data[,-1]


library(rpart)

fit <- rpart(Revenue_Share~., data=dataWithoutID, method="class", control=rpart.control(minsplit=2, cp=0))

#print out most important variables
fit$variable.importance
printcp(fit)
#http://gormanalysis.com/decision-trees-in-r-using-rpart/
#As a rule of thumb, it’s best to prune a decision tree using the
#cp of smallest tree that is within one standard deviation of the 
#tree with the smallest xerror. In this example, the best xerror 
#is 0.4 with standard deviation .25298. So, we want the smallest tree 
#with xerror less than 0.65298. This is the tree with cp = 0.2, so 
#we’ll want to prune our tree with a cp slightly greater than than 0.2.

mytree <- prune(fit, cp=0.071)

library(rattle)
library(rpart.plot)
library(RColorBrewer)


fancyRpartPlot(fit)
#text(data$Revenue_Share)

print(fit)


#library(C50)

# fit model
#fit <- C5.0(Revenue_Share~., data=data, trials=1)
# summarize the fit
#print(fit)
# make predictions
#predictions <- predict(fit, data)
# summarize accuracy
#table(predictions, data$Revenue_Share)
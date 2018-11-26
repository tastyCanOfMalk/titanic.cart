if (!require(ISLR)) install.packages("ISLR")
library(ISLR)
if (!require(tree)) install.packages("tree")
library(tree)
if (!require(MASS)) install.packages("MASS")
library(MASS)
if (!require(randomForest)) install.packages("randomForest")
library(randomForest)
if (!require(gbm)) install.packages("gbm")
library(gbm)
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# regression tree on Hitters dataset 

head(Hitters)
attach(Hitters)
nrow(Hitters)
Hitters <- na.omit(Hitters)
Salary <- na.omit(Salary)
nrow(Hitters)

salary.log <- log(Salary)

##tree on all data
tree.hitters <- tree(salary.log ~ Years + Hits, data = Hitters)
summary(tree.hitters)

plot(tree.hitters)
text(tree.hitters,cex=0.75)


# second regression tree example 
### Regression Tree, using Boston data set in MASS library

set.seed (1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston ,subset=train)
summary(tree.boston)

plot(tree.boston )
text(tree.boston,pretty =0, cex=0.8)

# use the cv.tree() function to see if pruning will improve performance

cv.boston =cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

# However, if we wish to prune the tree, we could do so as follows, 
# using the prune.tree() function:

prune.boston =prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty =0)

# In keeping with the cross-validation results, we use the unpruned tree to
# make predictions on the test set.
yhat=predict(tree.boston, newdata=Boston[-train,])
boston.test=Boston[-train ,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat - boston.test)^2)
# [1] 25.04559





attach(Carseats)


# creating a binary variable
High <- ifelse(Sales <= 8, "No", "Yes")
# merge High with the rest of the Carseats
Carseats <- data.frame(Carseats, High)
summary(Carseats)
a <- tree(formula = High ~ . - Sales, data = Carseats)
summary(a)
plot(a)
text(a,cex=0.5, pretty=0)

#type the name of the tree and you get the output of each branch of the tree
a  

# prediction performance on validation set
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]  # actual High values on the observations in the test set
tree.carseats=tree(High ~ . - Sales, data = Carseats, subset=train)
tree.pred=predict(tree.carseats, Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200  # this is the prediction accuracy for the test data set

# cross validation 
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats

# dev corresponds to the cross-validation error rate in this example
# size corresponds to # of terminal nodes
# tree with 9 terminal node has lowest cv error rate of 50

# plot the error rate as a function of both size and k
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev,type="b")
plot(cv.carseats$k, cv.carseats$dev,type="b")

# 9-node tree from pruning
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0,cex=0.6)
# prediction performance of this 9 node tree
tree.pred=predict(tree.carseats, Carseats.test,type="class")
table(tree.pred,High.test)
(87+58)/200

# 15-node tree from pruning
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0,cex=0.6)
# prediction performance of this 15 node tree
tree.pred=predict(tree.carseats, Carseats.test,type="class")
table(tree.pred,High.test)
(87+56)/200


### Bagging, Random Forests, and Boosting Examples using Boston data set in MASS library


boston.test=Boston[-train ,"medv"]
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

# how does this bagged model perform on the test set?

yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

# randomForest on Boston dataset
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train, mtry=4)  # sqrt(12) rounded up to 4 
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)
# [1] 11.44016

par(mfrow=c(1,1))
importance(rf.boston, type=2)
varImpPlot(rf.boston)


# boosting 
set.seed (1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=5000, interaction.depth = 4)
summary(boost.boston)

# partial dependence plots

par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

# test MSE from the boosted model
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)

#set shrinkage parameter to 0.02 (default is .001)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=5000, interaction.depth = 4, shrinkage=0.02,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)

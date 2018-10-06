## Author: Rajesh Jakhotia
## Company Name: K2 Analytics Finishing School Pvt. Ltd
## Email : ar.jakhotia@k2analytics.co.in
## Website : k2analytics.co.in

#### Amended and Customized by Santhosh Murali
##  for Greatlakes


## Let us first set the working directory path

setwd ("Data/")
getwd()

## Data Import
CTDF.dev <- read.table("DEV_SAMPLE.csv", sep = ",", header = T)
CTDF.dev.skewed <- read.table("DEV_SAMPLE_skewed.csv", sep = ",", header = T)
CTDF.holdout <- read.table("HOLDOUT_SAMPLE.csv", sep = ",", header = T)
c(nrow(CTDF.dev), nrow(CTDF.holdout))
str(CTDF.dev)



## installing rpart package for CART
#install.packages("rpart")
#install.packages("rpart.plot")


## loading the library
library(rpart)
library(rpart.plot)


## Target Rate 
sum(CTDF.dev$Target)/14000

## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 5)
r.ctrl

## calling the rpart function to build the tree
##m1 <- rpart(formula = Target ~ ., data = CTDF.dev[which(CTDF.dev$Holding_Period>10),-1], method = "class", control = r.ctrl)
m1 <- rpart(formula = Target ~ ., data = CTDF.dev[,-1], method = "class", control = r.ctrl)
m1

m2 <- rpart(formula = Target ~ ., data = CTDF.dev.skewed[,-1], method = "class", control = r.ctrl)
m2
#install.packages("rattle")
#install.packages("RColorBrewer")
library(rattle)
library(RColorBrewer)
fancyRpartPlot(m1)
fancyRpartPlot(m2)

## to find how the tree performs
printcp(m1) # will give cost complexity parameter table
plotcp(m1)

##rattle()
## Pruning Code
ptree<- prune(m1, cp= 0.0021 ,"CP")
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")


## Let's use rattle to see various model evaluation measures
##rattle()

View(CTDF.dev)
## Scoring syntax
CTDF.dev$predict.class <- predict(m1, CTDF.dev, type="class")
CTDF.dev$predict.score <- predict(m1, CTDF.dev)

View(CTDF.dev)
head(CTDF.dev)


## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

class(CTDF.dev$predict.score)
## deciling
CTDF.dev$deciles <- decile(CTDF.dev$predict.score[,2])
View(CTDF.dev)

## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(CTDF.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);

View(rank)



##install.packages("ROCR")
library(ROCR)
pred <- prediction(CTDF.dev$predict.score[,2], CTDF.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

##install.packages("ineq")
library(ineq)
gini = ineq(CTDF.dev$predict.score[,2], type="Gini")

with(CTDF.dev, table(Target, predict.class))
auc
KS
gini

View(rank)
## Syntax to get the node path
tree.path <- path.rpart(ptree, node = c(26, 27))

nrow(CTDF.holdout)

## Scoring Holdout sample
CTDF.holdout$predict.class <- predict(m1, CTDF.holdout, type="class")
CTDF.holdout$predict.score <- predict(m1, CTDF.holdout)


CTDF.holdout$deciles <- decile(CTDF.holdout$predict.score[,2])
View(CTDF.holdout)

## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(CTDF.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round(h_rank$cnt_resp * 100 / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_perct_resp <- round(h_rank$cum_resp * 100 / sum(h_rank$cnt_resp),2);
h_rank$cum_perct_non_resp <- round(h_rank$cum_non_resp * 100 / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_perct_resp - h_rank$cum_perct_non_resp);

View(h_rank)


with(CTDF.holdout, table(Target, predict.class))


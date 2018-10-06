## Author: Rajesh Jakhotia
## Company Name: K2 Analytics Finishing School Pvt. Ltd
## Email : ar.jakhotia@k2analytics.co.in
## Website : k2analytics.co.in

## Santhosh
## Residency - 3


## Let us first set the working directory path
#setwd ("D:/K2Analytics/Datafile/")
getwd()


## Ideally for any modeling you should have Training & Testing dataset
## Typically you would use sampling strategy
## However for the Neural Net training I am supplying the Training & Testing data separately


nn.dev <- read.table("data/DEV_SAMPLE.csv", sep = ",", header = T)
nn.holdout <- read.table("data/HOLDOUT_SAMPLE.csv", sep = ",", header = T)

View(nn.dev)
##occ.matrix <- model.matrix(~ Occupation - 1, data = nn.dev)
##nn.dev <- data.frame(nn.dev, occ.matrix)

##Gender.matrix <- model.matrix(~ Gender - 1, data = nn.dev)
##nn.dev <- data.frame(nn.dev, Gender.matrix)


##occ.matrix <- model.matrix(~ Occupation - 1, data = nn.holdout)
##nn.holdout <- data.frame(nn.holdout, occ.matrix)

##Gender.matrix <- model.matrix(~ Gender - 1, data = nn.holdout)
##nn.holdout <- data.frame(nn.holdout, Gender.matrix)

View(nn.holdout)
c(nrow(nn.dev), nrow(nn.holdout))
str(nn.dev)

## Response Rate
sum(nn.dev$Target) / nrow(nn.dev)
sum(nn.holdout$Target) / nrow(nn.holdout)


## Installing the Neural Net package; 

## If already installed do not run the below step
##install.packages("neuralnet")


library(neuralnet)
?"neuralnet"


nn1 <- neuralnet(formula = Target ~  Age +  Balance  + SCR +  No_OF_CR_TXNS + Holding_Period , 
                 data = nn.dev, 
                 hidden = 2,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000
                 ##startweights = startweightsObj
)

plot (nn1)

table(nn1$net.result)

## Assigning the Probabilities to Dev Sample
nn.dev$Prob = nn1$net.result[[1]] 

## The distribution of the estimated probabilities
quantile(nn.dev$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hist(nn.dev$Prob)



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

## deciling
nn.dev$deciles <- decile(nn.dev$Prob)


## Ranking code
##install.packages("data.table")
library(data.table)
library(scales)

tmp_DT = data.table(nn.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)



## Rebuilding the model by Scaling the Independent Variables

nn.dev <- read.table("data/DEV_SAMPLE.csv", sep = ",", header = T)
## build the neural net model by scaling the variables
x <- subset(nn.dev, 
            select = c("Age","Balance", "SCR", "No_OF_CR_TXNS", "Holding_Period"
##,"OccupationPROF", "OccupationSAL", "OccupationSELF.EMP", "OccupationSENP","GenderF", "GenderM", "GenderO"
                       )
)
View(nn.dev)
nn.devscaled <- scale(x)
nn.devscaled <- cbind(nn.dev[2], nn.devscaled)
View(nn.devscaled)

nn2 <- neuralnet(formula = Target ~  Age + Balance  + SCR +  No_OF_CR_TXNS + Holding_Period ,
      ## + OccupationPROF + OccupationSAL + OccupationSELF.EMP + OccupationSENP + GenderF + GenderM + GenderO,
                      data = nn.devscaled, 
                      hidden = 3,
                      err.fct = "sse",
                      linear.output = FALSE,
                      lifesign = "full",
                      lifesign.step = 1,
                      threshold = 0.1,
                      stepmax = 2000
                    )
#https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf

#number of hidden layers is number of times we reduce the variables through squire root.


plot(nn2)

nn2$net.result

## Assigning the Probabilities to Dev Sample
nn.dev$Prob = nn2$net.result[[1]] 


## The distribution of the estimated probabilities
quantile(nn.dev$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hist(nn.dev$Prob)
View(nn.dev)

## deciling
nn.dev$deciles <- decile(nn.dev$Prob)


## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(nn.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)





## Assgining 0 / 1 class based on certain threshold
nn.dev$Class = ifelse(nn.dev$Prob>0.21,1,0)
with( nn.dev, table(Target, as.factor(Class)  ))

## We can use the confusionMatrix function of the caret package 
##install.packages("caret")
library(caret)
confusionMatrix(nn.dev$Target, nn.dev$Class)


## Error Computation
sum((nn.dev$Target - nn.dev$Prob)^2)/2




## Other Model Performance Measures
##install.packages("ROCR")
library(ROCR)
nn.dev$Prob
pred <- prediction(nn.dev$Prob, nn.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

##install.packages("ineq")
library(ineq)
gini = ineq(nn.dev$Prob, type="Gini")


auc
KS
gini



## Scoring another dataset using the Neural Net Model Object
## To score we will use the compute function
?compute
x <- subset(nn.holdout, 
            select = c("Age","Balance", "SCR", "No_OF_CR_TXNS", "Holding_Period")
          )
x.scaled <- scale(x)
compute.output = compute(nn2, x.scaled)
nn.holdout$Predict.score = compute.output$net.result
View(nn.holdout)


quantile(nn.holdout$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)
nn.holdout$deciles <- decile(nn.holdout$Predict.score)

library(data.table)
tmp_DT = data.table(nn.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)


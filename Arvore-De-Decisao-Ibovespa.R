require(ggplot2)
library(data.table)
library(tidyr)
require(reshape2)
library(stats)
library(rattle)
library(caret)
library(ROCR)
library(party)
library(dplyr)
library(mlbench)
library(randomForest)
library(flexclust)
library(cluster)
library(Hmisc)
library(boot)
library(fpc)
library(xgboost)
library(readr)
library(stringr)
library(car)
library(pROC)
require(Matrix)
library(fTrading)
library(rpart.plot)
library(RColorBrewer)

ibov <- fread("IBOV_TREES.csv", header=T, sep=";")
ibov <- as.data.frame(ibov)
names(ibov)

ibovModelDB <- ibov

ibov_validacao <- ibovModelDB[1960:dim(ibovModelDB)[1],]

ibovModelDB <- ibovModelDB[1:1959,]

seed = 19
set.seed(seed)
inTrain <- createDataPartition(y=ibovModelDB$TARGET,p = 0.8, list=FALSE)
training <- ibovModelDB[inTrain,]
testing <- ibovModelDB[-inTrain,]
dim(training); dim(testing)

fit <- rpart(TARGET ~ .,
               data=training[,8:21],
               method="class", 
               control=rpart.control(minsplit=2, cp=0.007))
fancyRpartPlot(fit)

prediction_testing <- predict(fit, testing, type = "class")
summary(prediction_testing)
table(testing$TARGET,prediction_testing)
table(testing$TARGET,prediction_testing)[4]/(table(testing$TARGET,prediction_testing)[3]+table(testing$TARGET,prediction_testing)[4])*100
#59.5%

prediction_2017 <- predict(fit, ibov_validacao, type = "class")
ibov_validacao$prediction_2017 <- prediction_2017
buy <- subset(ibov_validacao,ibov_validacao$prediction_2017 == 1)[,1:7]
names(buy)
cumulative_buy_pontos <- cumsum(buy$Pontos)
plot(cumulative_buy_pontos,type="l",
     ylab = "Pontos Acumulados",
     xlab = "ExecuÃ§ao em 2017")
title(main = list("Resultado Ibovespa - Arvore de Decisao D1", cex = 1.5,
                  col = "red", font = 2))
text(15,500,"www.outspokenmarket.com", cex=1.1, pos=4, col="black")

sum(buy[6])#5426
SHARPE <- mean(buy$Pontos, na.rm = T)/sd(buy$Pontos, na.rm = T)
SHARPE #0.4428
maxDrawDown(cumulative_buy_pontos)
maxDrawDown(cumulative_buy_pontos)$maxdrawdown[1]/sum(buy[6])*100
#26.28%
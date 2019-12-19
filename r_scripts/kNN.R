#install.packages("class")
#install.packages("gmodels")
#install.packages("ggplot2")
library(class)
library(gmodels)
library(ggplot2)

#Read the data
wholesaleData <- read.csv("./data/knn/Wholesale.csv", sep = ",")
wholesaleData$Channel=factor(ifelse(wholesaleData$Channel==1,'Horeca','Retail'))
summary(wholesaleData)

#Plot Graphs
PlotFactor<- function(column) {
  path = paste("./diagrams/knn/graph_", column, ".png", sep = "", collapse = NULL)
  plot <- ggplot(data.frame(wholesaleData), aes(x=wholesaleData[, column]), col="lightblue") + geom_bar(fill="lightblue")
  ggsave(path, plot, width = 4, height = 4)
}

PlotNumeric<- function(column) {
  path = paste("./diagrams/knn/graph_", column, ".png", sep = "", collapse = NULL)
  png(path, width = 500, height = 500)
  hist(wholesaleData[, column], col="lightblue", main = column, xlab = column)
  dev.off()
}
#Plots
plot(wholesaleData)
PlotFactor("Channel")
PlotNumeric("Fresh")
PlotNumeric("Milk")
PlotNumeric("Grocery")
PlotNumeric("Frozen")
PlotNumeric("Detergents_Paper")
PlotNumeric("Delicassen")

#Split Training and Testing data/create a random sample for training and test data (2/3)
set.seed(1)
wholesaleDataRandom <- wholesaleData[order(runif(440)), ]

wholesaleDataTrainLables <- wholesaleDataRandom[1:290, "Channel"]
wholesaleDataTestLables <- wholesaleDataRandom[291:440, "Channel"]

wholesaleDataRandom = wholesaleDataRandom[-1]
wholesaleDataRandomZ <- scale(wholesaleDataRandom[,1:7])

wholesaleDataTrain <- wholesaleDataRandom[1:290, ]
wholesaleDataTest  <- wholesaleDataRandom[291:440, ]
wholesaleDataTrainZ <- wholesaleDataRandomZ[1:290, ]
wholesaleDataTestZ  <- wholesaleDataRandomZ[291:440, ]

prop.table(table(wholesaleDataTrainLables))
prop.table(table(wholesaleDataTestLables))


#Create and Evaluate the models with different k Value (No Scaling)
for (val in c(1:15)) {
  predictions <- knn(train = wholesaleDataTrain, test = wholesaleDataTest, 
                     cl = wholesaleDataTrainLables, k=val)
  
  CrossTable(predictions, wholesaleDataTestLables,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  accuracy <- sum(diag(table(predictions, wholesaleDataTestLables))) / sum(table(predictions, wholesaleDataTestLables))
  print(paste('(No Scaling) Accuracy for k', val, "is", accuracy))
}

#Create and Evaluate the models with different k Value (Z scaling)
for (val in c(1:15)) {
  predictions <- knn(train = wholesaleDataTrainZ, test = wholesaleDataTestZ, 
                   cl = wholesaleDataTrainLables, k=val)
  
  CrossTable(predictions, wholesaleDataTestLables,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  accuracy <- sum(diag(table(predictions, wholesaleDataTestLables))) / sum(table(predictions, wholesaleDataTestLables))
  print(paste('(Z Scaling) Accuracy for k', val, "is", accuracy))
}


#install.packages("C50")
#install.packages("gmodels")
#install.packages(ggplot2)
library(C50)
library(gmodels)
library(ggplot2)
set.seed(1)

adult <- read.csv("./data/dicision_tree/adult.data", header = F, sep = ",")
colnames(adult) <- c("age", "workclass", "fnlwgt", "education.str", "education.num", "marital.status", "occupation", "relationship", "race", "sex", "capital.gain", "capital.loss", "hours.per.week", "native.country", "salary")
summary(adult)

#Plot Graphs
PlotFactor<- function(column, width) {
  path = paste("./diagrams/dicision_trees/graph_", column, ".png", sep = "", collapse = NULL)
  plot <- ggplot(data.frame(adult), aes(x=adult[, column]), col="lightblue") + geom_bar(fill="lightblue") + xlab(column)
  ggsave(path, plot, width = width, height = 10)
}

PlotNumeric<- function(column) {
  path = paste("./diagrams/dicision_trees/graph_", column, ".png", sep = "", collapse = NULL)
  png(path, width = 500, height = 500)
  hist(adult[, column], col="lightblue", main = column, xlab = column)
  dev.off()
}
Plots
PlotNumeric("age")
PlotFactor("workclass", 15)
PlotNumeric("fnlwgt")
PlotFactor("education.str", 15)
PlotNumeric("education.num")
PlotFactor("marital.status", 15)
PlotFactor("occupation", 18)
PlotFactor("relationship", 15)
PlotFactor("race", 15)
PlotFactor("sex", 15)
PlotNumeric("capital.gain")
PlotNumeric("capital.loss")
PlotNumeric("hours.per.week")
PlotFactor("native.country", 35)
PlotFactor("salary", 15)

#Randomise Data Set
adultRandom <- adult[order(runif(32558)), ]

#Using 1/3 data for validation
adultTrain <- adultRandom[1:21705, ]
adultTest  <- adultRandom[21706:32558, ]

#Check the proportion of column for prediction
prop.table(table(adultTrain$salary))
prop.table(table(adultTest$salary))

#function to plot models as png files and export
PlotDicisonTree <- function(path, model, trial)
{
  png(path, width = 5000, height = 5000)
  plot(model) #To make plot work had to replace all spaces with nothing in the csv
  dev.off() 
}

#create model
model <- C5.0(salary ~ ., data = adultTrain)
model

#plot model
PlotDicisonTree("./diagrams/dicision_trees/dt_no_boost.png", model, 0)

#print summary
summary(model)

#Make Predictions
predictions <- predict(model, adultTest)
CrossTable(predictions, adultTest$salary,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
accuracy <- sum(diag(table(predictions, adultTest$salary))) / sum(table(predictions, adultTest$salary))
print(paste('Accuracy for Discision Tree', accuracy))

#Create + Plot Model with different boosting values
for (trial in c(3, 7, 15, 45)) {#chosen randomly to see effects
  #create model
  model <- C5.0(salary ~ ., data = adultTrain, trials = trial)
  print(model)
  
  #plot model
  png_path = paste("./diagrams/dicision_trees/dt_boost", trial, ".png", sep = "", collapse = NULL)
  PlotDicisonTree(png_path, model, trial)
  
  #print summary
  print(summary(model))

  #Make Predictions
  predictions <- predict(model, adultTest)
  CrossTable(predictions, adultTest$salary,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
  accuracy <- sum(diag(table(predictions, adultTest$salary))) / sum(table(predictions, adultTest$salary))
  print(paste('Accuracy for Discision Tree', accuracy))
}


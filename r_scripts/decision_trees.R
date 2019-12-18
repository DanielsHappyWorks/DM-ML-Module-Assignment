#install.packages("C50")
#install.packages("gmodels")
library(C50)
library(gmodels)
set.seed(1)

adult <- read.csv("./data/dicision_tree/adult.data", header = F, sep = ",")
colnames(adult) <- c("age", "workclass", "fnlwgt", "education.str", "education.num", "marital.status", "occupation", "relationship", "race", "sex", "capital.gain", "capital.loss", "hours.per.week", "native.country", "salary")
summary(adult)

#Plot Graphs
PlotFactor<- function(column) {
  path = paste("./diagrams/dicision_trees/graph_", column, ".png", sep = "", collapse = NULL)
  plot <- ggplot(data.frame(adult), aes(x=adult[, column]), col="lightblue") + geom_bar(fill="lightblue")
  ggsave(path, plot, width = 4, height = 4)
}

PlotNumeric<- function(column) {
  path = paste("./diagrams/dicision_trees/graph_", column, ".png", sep = "", collapse = NULL)
  png(path, width = 500, height = 500)
  hist(adult[, column], col="lightblue", main = column, xlab = column)
  dev.off()
}
#Plots
PlotNumeric("age")
PlotFactor("workclass")
PlotNumeric("fnlwgt")
PlotFactor("education.str")
PlotNumeric("education.num")
PlotFactor("marital.status")
PlotFactor("occupation")
PlotFactor("relationship")
PlotFactor("race")
PlotFactor("sex")
PlotNumeric("capital.gain")
PlotNumeric("capital.loss")
PlotNumeric("hours.per.week")
PlotFactor("native.country")
PlotFactor("salary")

#Randomise Data Set
adultRandom <- adult[order(runif(32558)), ]

#Using 1/3 data for validation
adultTrain <- adultRandom[1:21705, ]
adultTest  <- adultRandom[21706:32558, ]

#Check the proportion of column for prediction
prop.table(table(adultTrain$salary))
prop.table(table(adultTest$salary))

#function to plot models as png files and export
PlotDicisonTree <- function(path, model)
{
  png(path, width = 5000, height = 5000)
  plot(model) #To make plot work had to replace all spaces with nothing in the csv
  dev.off() 
}

#create model
model <- C5.0(salary ~ ., data = adultTrain)
model

#plot model
PlotDicisonTree("./diagrams/dicision_trees/dt_boost.png", model)

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
for (trial in c(3, 5, 7, 10, 15)) {#chosen randomly to see effects
  #create model
  model <- C5.0(salary ~ ., data = adultTrain, trials = trial)
  model
  
  #plot model
  png_path = paste("./diagrams/dicision_trees/dt_boost", trial, ".png", sep = "", collapse = NULL)
  PlotDicisonTree(png_path, model)
  
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

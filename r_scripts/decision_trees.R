#install.packages("C50")
#install.packages("gmodels")
library(C50)
library(gmodels)
set.seed(1)

adult <- read.csv("./data/dicision_tree/adult.data", sep = ",")
summary(adult)

#Randomise Data Set
adultRandom <- adult[order(runif(4898)), ]

#Using 1/3 data for validation
adultTrain <- adultRandom[1:2000, ]
adultTest  <- adultRandom[2001:3000, ]

#Check the proportion of column for prediction
prop.table(table(adultTrain$X..50K))
prop.table(table(adultTest$X..50K))

#Create + Plot Model
model <- C5.0(X..50K ~ ., data = adultTrain)
model
#plot(model)
summary(model)

#Make Predictions
predictions <- predict(model, adultTest)
CrossTable(predictions, adultTest$X..50K,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

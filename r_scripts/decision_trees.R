#install.packages("ggplot2")
library('ggplot2')
set.seed(1)
wineData <- read.csv("data/regression/winequality-white.csv", sep = ";")

#Randomise Data Set
wineDataRand <- wineData[order(runif(4898)), ]

#Using 1/3 data for validation
wineDataTrain <- wineDataRand[1:2000, ]
wineDataTest  <- wineDataRand[2001:3000, ]

# define the functions
RootMeanSquareError <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

GetPerfForColTraining <- function(performance, poly.fit, col)
{
  performance <- rbind(performance, data.frame(Degree = d,Column = col,Data = 'Training Set', RootMeanSquareError = RootMeanSquareError(wineDataTrain$quality, predict(poly.fit))))
  performance <- rbind(performance, data.frame(Degree = d, Column = col, Data = 'validation Set', RootMeanSquareError = RootMeanSquareError(wineDataTest$quality, predict(poly.fit, newdata = wineDataTest))))
  return(performance)
}

PlotPerfForColTraining <- function(performance, title)
{
  ggplot(performance, aes(x = Degree, y = RootMeanSquareError, linetype = Data)) + ggtitle(title) + geom_point() +geom_line()
}

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(fixed.acidity, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "fixed.acidity")
}
PlotPerfForColTraining(performance, "fixed.acidity")

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(volatile.acidity, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "volatile.acidity")
  
}
PlotPerfForColTraining(performance, "volatile.acidity")

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(citric.acid, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "citric.acid")
  
}
PlotPerfForColTraining(performance, "citric.acid")

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(residual.sugar, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "residual.sugar")
  
}
PlotPerfForColTraining(performance, "residual.sugar")

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(chlorides, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "chlorides")
  
}
PlotPerfForColTraining(performance, "chlorides")

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(free.sulfur.dioxide, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "free.sulfur.dioxide")
  
}
PlotPerfForColTraining(performance, "free.sulfur.dioxide")

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(total.sulfur.dioxide, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "total.sulfur.dioxide")
  
}
PlotPerfForColTraining(performance, "total.sulfur.dioxide")

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(density, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "density")
  
}
PlotPerfForColTraining(performance, "density")

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(pH, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "pH")
  
}
PlotPerfForColTraining(performance, "pH")

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(sulphates, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "sulphates")
  
}
PlotPerfForColTraining(performance, "sulphates")

performance <- data.frame()
for (d in 1:15)
{
  poly.fit <- lm(quality ~ poly(alcohol, degree=d), data=wineDataTrain)
  performance <- GetPerfForColTraining(performance, poly.fit, "alcohol")
  
}
PlotPerfForColTraining(performance, "alcohol")

fitMulti <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data=wineDataTrain)
summary(fitMulti)
fitPolyDeg2 <- lm(quality ~ polym(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol, degree=2), data=wineDataTrain)
summary(fitPolyDeg2)
fitPolyDeg3 <- lm(quality ~ polym(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol, degree=3), data=wineDataTrain)
summary(fitPolyDeg3)
fitPloyDegCustom <- lm(quality ~ poly(fixed.acidity, degree=4) + poly(volatile.acidity, degree=7) + poly(citric.acid, degree=1) + poly(residual.sugar, degree=1) + poly(chlorides, degree=6) + poly(free.sulfur.dioxide, degree=1) + poly(total.sulfur.dioxide, degree=1) + poly(density, degree=3) + poly(pH, degree=2) + poly(sulphates, degree=3) + poly(alcohol, degree=3), data=wineDataTrain)
summary(fitPloyDegCustom)

anova(fitMulti, fitPolyDeg2, fitPolyDeg3, fitPloyDegCustom)

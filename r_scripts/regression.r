#load data set
wineData <- read.csv("./data/regression/winequality-white.csv", sep = ";")
#plot(wineData)

# define the function to create diagarms for single col lr
PlotSingleLR <- function(lr, yLable, xLable)
{
  pdf(paste("diagrams/regression/wine_", xLable, "_vs_", yLable, "_scatter.pdf", sep = "", collapse = NULL))
  plot(wineData[,xLable], wineData[,yLable], pch = 16, cex = 1.3, col = "blue", main = paste(xLable, "VS", yLable, sep = " ", collapse = NULL), xlab = xLable, ylab = yLable)
  abline(lr, col="red", lty=2, lwd=3)
  dev.off()
  pdf(paste("diagrams/regression/wine_", xLable, "_hist.pdf", sep = "", collapse = NULL))
  hist(wineData[,yLable], col="lightblue", main = paste("Histogram of", xLable, sep = " ", collapse = NULL), xlab = xLable)
  dev.off()
  return(lr)
}
#use function to draw single column linear regression
a <- PlotSingleLR(lm(quality ~ fixed.acidity, data=wineData), 'quality', 'fixed.acidity')
#summary(a)
b <- PlotSingleLR(lm(quality ~ volatile.acidity, data=wineData), 'quality', 'volatile.acidity')
#summary(b)
c <- PlotSingleLR(lm(quality ~ citric.acid, data=wineData), 'quality', 'citric.acid')
#summary(c)
d <- PlotSingleLR(lm(quality ~ residual.sugar, data=wineData), 'quality', 'residual.sugar')
#summary(d)
e <- PlotSingleLR(lm(quality ~ chlorides, data=wineData), 'quality', 'chlorides')
#summary(e)
f <- PlotSingleLR(lm(quality ~ free.sulfur.dioxide, data=wineData), 'quality', 'free.sulfur.dioxide')
#summary(f)
g <- PlotSingleLR(lm(quality ~ total.sulfur.dioxide, data=wineData), 'quality', 'total.sulfur.dioxide')
#summary(g)
h <- PlotSingleLR(lm(quality ~ density, data=wineData), 'quality', 'density')
#summary(h)
i <- PlotSingleLR(lm(quality ~ pH, data=wineData), 'quality', 'pH')
#summary(i)
j <- PlotSingleLR(lm(quality ~ sulphates, data=wineData), 'quality', 'sulphates')
#summary(j)
k <- PlotSingleLR(lm(quality ~ alcohol, data=wineData), 'quality', 'alcohol')
#summary(k)
anova(a,b,c,d,e,f,g,h,i,j,k)

#All element linear regression
fitMulti <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data=wineData)
#summary(fitMulti)

#Polinomial regression deg 2
fitPolyDeg2 <- lm(quality ~ polym(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol, degree=2), data=wineData)
#summary(fitPolyDeg2)

#Polinomial regression deg 3
fitPolyDeg3 <- lm(quality ~ polym(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol, degree=3), data=wineData)
#summary(fitPolyDeg3)

anova(fitMulti, fitPolyDeg2, fitPolyDeg3)

#test data for evaluation
testData <- data.frame(
  fixed.acidity = c(8.5,10.3,5.5,6.7,5.9,6.8,7.6,6.8,7.2,7.9,7.3,5.8,9.1,6.9),
  volatile.acidity = c(0.26,0.17,0.485,0.31,0.36,0.37,0.34,0.3,0.23,0.41,0.19,0.28,0.27,0.36),
  citric.acid = c(0.21,0.47,0,0.31,0.04,0.51,0.39,0.27,0.39,0.37,0.27,0.34,0.45,0.34),
  residual.sugar = c(16.2,1.4,1.5,9.9,5.7,11.8,7.6,11.6,2.3,4.5,13.9,2.2,10.6,4.2),
  chlorides = c(0.074,0.037,0.065,0.04,0.046,0.044,0.04,0.028,0.033,0.03,0.057,0.037,0.035,0.018),
  free.sulfur.dioxide = c(41,5,8,10,21,62,45,22,29,40,45,24,28,57),
  total.sulfur.dioxide = c(197,33,103,175,87,163,215,97,102,114,155,125,124,119),
  density = c(0.998,0.9939,0.994,0.9953,0.9934,0.9976,0.9965,0.99314,0.9908,0.992,0.99807,0.98986,0.997,0.9898),
  pH = c(3.02,2.89,3.63,3.46,3.22,3.19,3.11,2.96,3.26,3.17,2.94,3.36,3.2,3.28),
  sulphates = c(0.5,0.28,0.4,0.55,0.51,0.44,0.53,0.38,0.54,0.54,0.41,0.33,0.46,0.36),
  alcohol = c(9.8,9.6,9.7,11.4,10.2,8.8,9.2,11.7,12.3,12.4,8.8,12.8,10.4,12.7)
  #3,3,4,4,5,5,6,6,7,7,8,8,9,9 Expected quality
)

#predictions
predict(fitMulti, testData, interval="predict")
predict(fitPolyDeg2, testData, interval="predict")
predict(fitPolyDeg3, testData, interval="predict")


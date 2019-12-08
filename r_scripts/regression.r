wineData <- read.csv("data/regression/winequality-white.csv", sep = ";")

# define the function to create diagarms for single col lr
PlotSingleLR <- function(lr, xLable, yLable)
{
  pdf(paste("diagrams/regression/", xLable, "_vs_", yLable, "_scatter.pdf", sep = "", collapse = NULL))
  plot(wineData[,xLable], wineData[,yLable], pch = 16, cex = 1.3, col = "blue", main = paste(xLable, "VS", yLable, sep = " ", collapse = NULL), xlab = xLable, ylab = yLable)
  abline(lr, col="red", lty=2, lwd=3)
  dev.off()
  pdf(paste("diagrams/regression/", xLable, "_vs_", yLable, "_hist.pdf", sep = "", collapse = NULL))
  hist(wineData[,yLable], col="lightblue", main = paste("Histogram of", yLable, sep = " ", collapse = NULL), xlab = xLable)
  dev.off()
  summary(lr)
  return(lr)
}
#use function to draw single column linear regression
a <- PlotSingleLR(lm(quality ~ fixed.acidity, data=wineData), 'quality', 'fixed.acidity')
b <- PlotSingleLR(lm(quality ~ volatile.acidity, data=wineData), 'quality', 'volatile.acidity')
c <- PlotSingleLR(lm(quality ~ citric.acid, data=wineData), 'quality', 'citric.acid')
d <- PlotSingleLR(lm(quality ~ residual.sugar, data=wineData), 'quality', 'residual.sugar')
e <- PlotSingleLR(lm(quality ~ chlorides, data=wineData), 'quality', 'chlorides')
f <- PlotSingleLR(lm(quality ~ free.sulfur.dioxide, data=wineData), 'quality', 'free.sulfur.dioxide')
g <- PlotSingleLR(lm(quality ~ total.sulfur.dioxide, data=wineData), 'quality', 'total.sulfur.dioxide')
h <- PlotSingleLR(lm(quality ~ density, data=wineData), 'quality', 'density')
i <- PlotSingleLR(lm(quality ~ pH, data=wineData), 'quality', 'pH')
j <- PlotSingleLR(lm(quality ~ sulphates, data=wineData), 'quality', 'sulphates')
k <- PlotSingleLR(lm(quality ~ alcohol, data=wineData), 'quality', 'alcohol')
anova(a,b,c,d,e,f,g,h,i,j,k)

#All element linear regression
fitMulti <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data=wineData)
summary(fitMulti)

#Polinomial regression deg 2
fitPolyDeg2 <- lm(quality ~ polym(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol, degree=2), data=wineData)
summary(fitPolyDeg2)

#Polinomial regression deg 3
fitPolyDeg3 <- lm(quality ~ polym(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol, degree=3), data=wineData)
summary(fitPolyDeg3)

anova(fitMulti, fitPolyDeg2, fitPolyDeg3)

require(graphics)
require(lattice)
require(ggplot2)
require(zoo) #used for time series on daily measurements rather than monthly
require(car)
data(airquality)
attributes(airquality)
summary(airquality[,1:4])
airquality.transformed <- transform(airquality, Date = as.Date(paste(1973, Month, Day, sep="-")))
#could have also interpolated missing data for the ts, particularly for the ozone measurement which has a lot of missingness,
# but doesn't make much sense, 
# and time series is not terribly useful anyways. command is 
#>airquality.transformed.interpolated <- na.approx(airquality.transformed)
airquality.transformed.ts <- zoo(airquality.transformed[,1:4], as.Date(airquality.transformed[,7]))
plot(airquality.transformed.ts, plot.type = "single", col= c("red","blue","green","purple"), lwd = 2, ylim=c(0,350))
legend("topright", legend=c("Ozone","Rad","Wind","Temp"), col=c("red","blue","green","purple"),lty=1,lwd=2, cex=0.5)
#nothing is seeming terribly seasonal. gonna make another and leave off ozone and rad in order to have better y axis for temp/wind
airquality.transformed.noRad.noO.ts <- zoo(airquality.transformed[,c(3,4)], as.Date(airquality.transformed[,7]))
plot(airquality.transformed.noRad.noO.ts, plot.type = "single", col= c("green","purple"), lwd = 2, ylim=c(0,100))
legend("topright", legend=c("Wind","Temp"), col=c("green","purple"),lty=1,lwd=2,cex=0.5)

boxplot(airquality[,1:4])
#a few upper outliers in ozone measurements (ozone and rad are skewed) and a few in wind. temp and wind look fairly normally distributed
# you can see this with histograms or qq plots as well

#par(mfcol=c(1,2),ps=6.5)
#hist(airquality$Ozone,breaks=30, cex.caption = 3, cex.lab = 3)
#qqnorm(log(airquality$Ozone))
# decided to loop these, but so far could only figure out how to loop the hists together, and then the # qqs together

par(mfrow=c(2, 2))
colnames <- dimnames(airquality[c("Ozone","Solar.R","Temp","Wind")])[[2]]
for (i in 1:4) {
  hist(airquality[,i],  breaks=30, main=colnames[i], probability=TRUE, col="gray", border="white") 
}

par(mfrow=c(2, 2))
colnames <- dimnames(airquality[c("Ozone","Solar.R","Temp","Wind")])[[2]]
for (i in 1:4) {
  qqnorm(airquality[,i], main=colnames[i])
}

#ozone is looking a bit non-normal, going to try a log transform
par(mfcol=c(1,2),ps=6.5)
hist(log(airquality$Ozone),breaks=30, cex.caption = 3, cex.lab = 3)
qqnorm(log(airquality$Ozone))

#i'm not sure if the fact that the log of ozone is more normal distribution means that I have to use the log ozone
#from now on in all the models. but..

shapiro.test(airquality$Ozone) #tests normality of distrubution. null hypothesis is non-normality.
#ozone measurements look normal enough?

pairs(airquality[c("Ozone","Solar.R","Wind","Temp")]) 
#pairs works fine for a scatterplot, but the one in the car library includes more features
library(car)
scatterplotMatrix(~Ozone+Solar.R+Wind+Temp, data=airquality, main="Airquality Scatterplot")
# from this, see a correlation between temp and wind (not surprised), but possible can use temp and wind to predict ozone


cor(airquality[c("Ozone","Temp")], use="pairwise.complete.obs", method=(c("pearson")))
cor(airquality[c("Ozone","Wind")], use="pairwise.complete.obs", method=(c("pearson")))
cor(airquality[c("Ozone","Solar.R")], use="pairwise.complete.obs", method=(c("pearson")))

cor(airquality[c("Temp","Wind")], use="pairwise.complete.obs", method=(c("pearson")))

fit <- lm(airquality$Wind~airquality$Temp)
fit
ci <- confint(fit)
ci
plot(airquality$Wind~airquality$Temp, data=airquality)
abline(fit, col="black")
abline(ci[1, 2], ci[2, 2], col="gray")
abline(ci[1, 1], ci[2, 1], col="gray")
summary(fit)
#R^2 shows ~20% of the variation in wind can be explained with a linear model in the temp variable

fit2 <- lm(airquality$Ozone~airquality$Temp)
fit2
ci2 <- confint(fit2)
ci2
plot(airquality$Ozone~airquality$Temp, data=airquality)
abline(fit2, col="black")
abline(ci2[1, 2], ci2[2, 2], col="gray")
abline(ci2[1, 1], ci2[2, 1], col="gray")
summary(fit2)
#R^2 shows almost 50% of the variation in Ozone can be explained with a linear model in the temp variable

fit3 <- lm(airquality$Ozone~airquality$Wind)
fit3
ci3 <- confint(fit3)
ci3
plot(airquality$Ozone~airquality$Wind, data=airquality)
abline(fit3, col="black")
abline(ci3[1, 2], ci3[2, 2], col="gray")
abline(ci3[1, 1], ci3[2, 1], col="gray")
summary(fit3)

xyplot(Solar.R~Ozone|Month, data=airquality, panel=function(...){
    panel.abline(lm(airquality$Solar.R~airquality$Ozone), col="black")
    panel.xyplot(...)
   }
)

xyplot(Solar.R~Temp|Month, data=airquality, panel=function(...){
  panel.abline(lm(airquality$Solar.R~airquality$Temp), col="black")
  panel.xyplot(...)
}
)



xyplot(Ozone~Temp|Month, data=airquality)
xyplot(Ozone~Wind|Month, data=airquality)
xyplot(Temp~Wind|Month, data=airquality)
#need to look up how to do legends with lattice xyplots on a conditioning variable, and how to 
# fit separate lines for conditioning variables


cor(airquality[c("Ozone","Temp")], use="pairwise.complete.obs", method=(c("pearson")))
cor.test(airquality$Ozone,airquality$Temp)

cor(airquality[c("Ozone","Solar.R")], use="pairwise.complete.obs", method=(c("pearson")))
cor.test(airquality$Ozone,airquality$Solar.R)  
cor(airquality[c("Temp","Wind")], use="pairwise.complete.obs", method=(c("pearson")))
cor.test(airquality$Temp,airquality$Wind)


ozone.model <- lm(formula=Ozone~Solar.R+Temp+Wind, data=airquality)
ozone.model
summary(ozone.model)
#temp and wind appear to be significantly correlated with ozone
ci <- confint(ozone.model)
ci
#probably should remove intercept from the model, as not sure that an intercept makes any kind of sense
# in terms of this model, since I somewhat doubt that ozone ever measures at 0. 

#thought about making linear model without Solar.R, but need to figure out how to handle missingness
#ozone.model2 <- lm(formula=Ozone~Temp+Wind, data=airquality, na.action=na.exclude)
#ozone.model2
#summary(ozone.model2)
#ci2 <- confint(ozone.model2)
#ci2
#anova(ozone.model,ozone.model2)

influence.measures(ozone.model)
ozone.fitted <- fitted(ozone.model,na.action=na.exclude)
ozone.fitted
ozone.residuals <- residuals(ozone.model, na.action=na.exclude)
ozone.residuals

rfs(ozone.model, aspect=1, layout=c(2,1), distribution = qnorm, main = list("Residual-Fit Spread Plot", cex=2),) 

outlierTest(ozone.model)
# thought about leaving out obs 117, as it is highly influential in the model, which is likely 
# undue to it. Depends on what exactly we are trying to model though..  

par(mfcol=c(1,2),ps=8)
plot(ozone.fitted, ozone.residuals, ylab="Residuals", xlab="Ozone", main="airquality ozone", cex.caption=3, cex.lab=3) 
abline(0, 0)# the horizon
qqnorm(ozone.residuals)
# there seems to be increased residuals at higher ozone levels

par(mfcol=c(3,2),ps=6.5)

# thrown in for fun. need to look into the meaning of some of this more (I understand residuals vs fitted, and QQ but not the others)
plot(ozone.model, which=c(1:6), 
     caption = list("Residuals vs fitted", "Normal Q-Q", "Scale-Location", "Cook's Distance",
                    "Residuals vs Leverage", expression("Cooks dist vs Leverage " * h[ii] / (1-h[ii])) ),
     sub.caption = NULL, main="",
     ask = prod(par("mfcol")) < length(which) && dev.interactive(),
     id.n = 3, labels.id = names(residuals(ozone.model)), cex.id=3,
     qqline = TRUE, cook.levels = c(0.5,1.0),
     cex.caption=3,
     cex.lab=3
)






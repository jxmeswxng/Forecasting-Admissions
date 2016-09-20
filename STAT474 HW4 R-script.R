data <- Admissions

dim(data)
str(data)
summary(data) # lots of NAs
attach(data)

###### Level 1 Analysis
### Univariate
admitBarplot <- barplot(c(length(admit[admit==1])[1]/8700,
                          length(admit[admit==0])[1]/8700),
                        main="Frequency of Admission to Elite University",
                        names.arg=c("Admitted","Rejected"),
                        ylim=c(0,1))
text(x=admitBarplot,y=c(0.31,0.69),label=c(2686,6014),pos=3,col="red")

raceBarplot <- barplot(c(sum(complete.cases(anglo[anglo==1]))/8700,
                         sum(complete.cases(asian[asian==1]))/8700,
                         sum(complete.cases(black[black==1]))/8700,
                         2117/8700),
                       main="Frequency of Race in Applications to Elite University",
                       names.arg=c("Anglo","Asian","Black","Unreported"),
                       ylim=c(0,1))
text(x=raceBarplot,y=c(0.32,0.39,0.04,0.24),label=c(2810,3417,356,2117),pos=3,col="red")

boxplot(gpa.wtd,main="Boxplot of Weighted GPA")
axis(side=1,at=c(1),labels=c("Weighted GPA"))

satBoxplot <- boxplot(sati.verb,sati.math,main="Boxplots of SAT Scores")
axis(side=1,at=c(1,2),labels=c("Verbal","Math"))

boxplot(income,main="Boxplot of Applicant Income")
summary(income)

sexBarplot <- barplot(c(dim(data[data$sex==1,])[1]/8700,
                        dim(data[data$sex==0,])[1]/8700,
                        24/8700),
                        main="Frequency of Gender Applicants to Elite University",
                        names.arg=c("Male","Female","Unreported"),
                        ylim=c(0,1))
text(x=sexBarplot,y=c(0.46,0.53,0.01),label=c(4043,4633,24),pos=3,col="red")

### Bivariate
admitYes <- data[data$admit==1,]
admitNo <- data[data$admit==0,]

raceBarplot <- barplot(c(sum(complete.cases(admitYes$anglo[admitYes$anglo==1]))/2686,
                         sum(complete.cases(admitYes$asian[admitYes$asian==1]))/2686,
                         sum(complete.cases(admitYes$black[admitYes$black==1]))/2686,
                         594/2696),
                       main="Admitted Frequency of Race in Applications to Elite University",
                       names.arg=c("Anglo","Asian","Black","Did not report"),
                       ylim=c(0,1))
text(x=raceBarplot,y=c(0.34,0.41,0.04,0.22),label=c(912,1093,87,594),pos=3,col="red")

raceBarplot <- barplot(c(sum(complete.cases(admitNo$anglo[admitNo$anglo==1]))/6014,
                         sum(complete.cases(admitNo$asian[admitNo$asian==1]))/6014,
                         sum(complete.cases(admitNo$black[admitNo$black==1]))/6014,
                         1523/6014),
                       main="Rejected Frequency of Race in Applications to Elite University",
                       names.arg=c("Anglo","Asian","Black","Did not report"),
                       ylim=c(0,1))
text(x=raceBarplot,y=c(0.32,0.39,0.05,0.25),label=c(1898,2324,269,1523),pos=3,col="red")

boxplot(admitYes$gpa.wtd,admitNo$gpa.wtd,main="Admitted vs Rejected Boxplot of Weighted GPA")
axis(side=1,at=c(1,2),labels=c("Admitted","Rejected"))

hist(admitYes$gpa.wtd,col=rgb(1,0,0,0.5),breaks=seq(0,5,by=.1),ylim=c(0,600),main="Admitted vs Rejected Histogram of Weighted GPA",xlab="Weighted GPA")
hist(admitNo$gpa.wtd,add=T,col=rgb(0,0,1,0.5),breaks=seq(0,5,by=.1))

satBoxplot <- boxplot(admitYes$sati.verb,admitNo$sati.verb,main="Admitted vs Rejected Boxplots of SAT Verbal Scores")
axis(side=1,at=c(1,2),labels=c("Admitted","Rejected"))

hist(admitYes$sati.verb,col=rgb(1,0,0,0.5),breaks=seq(0,800,by=100),ylim=c(0,2500),main="Admitted vs Rejected Histogram of SAT Verbal Scores",xlab="SAT Verbal Scores")
hist(admitNo$sati.verb,add=T,col=rgb(0,0,1,0.5),breaks=seq(0,800,by=100))

satBoxplot <- boxplot(admitYes$sati.math,admitNo$sati.math,main="Admitted vs Rejected Boxplots of SAT Math Scores")
axis(side=1,at=c(1,2),labels=c("Admitted","Rejected"))

hist(admitYes$sati.math,col=rgb(1,0,0,0.5),breaks=seq(0,800,by=100),ylim=c(0,2500),main="Admitted vs Rejected Histogram of SAT Math Scores",xlab="SAT Math Scores")
hist(admitNo$sati.math,add=T,col=rgb(0,0,1,0.5),breaks=seq(0,800,by=100))

boxplot(admitYes$income,admitNo$income,main="Admitted vs Rejected Boxplot of Applicant Income")
axis(side=1,at=c(1,2),labels=c("Admitted","Rejected"))
summary(admitYes$income)
summary(admitNo$income)

sexBarplot <- barplot(c(sum(admitYes$sex==1,na.rm=TRUE)/2686,
                        sum(admitYes$sex==0,na.rm=TRUE)/2686,
                        3/2686),
                      main="Admitted Frequency of Gender Applicants to Elite University",
                      names.arg=c("Male","Female","Did not report"),
                      ylim=c(0,1))
text(x=sexBarplot,y=c(0.45,0.55,0.01),label=c(1218,1465,3),pos=3,col="red")

sexBarplot <- barplot(c(sum(admitNo$sex==1,na.rm=TRUE)/6014,
                        sum(admitNo$sex==0,na.rm=TRUE)/6014,
                        21/6014),
                      main="Rejected Frequency of Gender Applicants to Elite University",
                      names.arg=c("Male","Female","Did not report"),
                      ylim=c(0,1))
text(x=sexBarplot,y=c(0.47,0.53,0.01),label=c(2825,3168,21),pos=3,col="red")

###### Level II Analysis
install.packages("randomForest")
library(randomForest)

### Remove missing values
dataNA <- na.omit(data)
summary(dataNA)
dataZero <- dataNA[dataNA$gpa.wtd!=0,]
summary(dataZero)
dataMath <- dataZero[dataZero$sati.math!=0,]
summary(dataMath)
dataRead <- dataMath[dataMath$sati.verb!=0,]
summary(dataRead)
dataFinal <- dataRead
dim(dataFinal) # n = 6175
dataFinal$admit <- as.factor(dataFinal$admit)

### Random Forest Classifier
# No Misclassification Costs
rf1 <- randomForest(admit~.,
                   data=dataFinal,
                   ntree=500,
                   importance=TRUE) 

# With Misclassification Costs
rf2 <- randomForest(admit~.,
                   data=dataFinal,
                   ntree=500,
                   importance=TRUE,
                   sampsize=c(450,1284)) # 450,1284

### Variable Importance Plot
par(mfrow=c(2,2))
varImpPlot(rf2,type=1,scale=F,class=1,
           main="Forecasting Variable Importance Plot for Admittance (Unstd)",
           pch=19)
varImpPlot(rf2,type=1,scale=T,class=1,
           main="Forecasting Variable Importance Plot for Admittance (Std)",
           pch=19)
varImpPlot(rf2,type=1,scale=F,class=0,
           main="Forecasting Variable Importance Plot for Rejection (Unstd)",
           pch=19)
varImpPlot(rf2,type=1,scale=T,class=0,
           main="Forecasting Variable Importance Plot for Rejection (Std)",
           pch=19)

### Partial Dependence Plots
# race
partial_anglo <- partialPlot(rf2,pred.data=dataFinal,x.var=anglo,which.class=1)
partial_asian <- partialPlot(rf2,pred.data=dataFinal,x.var=asian,which.class=1)
partial_black <- partialPlot(rf2,pred.data=dataFinal,x.var=black,which.class=1)

par(mfrow=c(1,3))
scatter.smooth(partial_anglo$x,partial_anglo$y,span=.5,
               main="Partial Dependence Plot: Admittance on Anglo",
               pch=19,col="blue",
               ylim=c(0.40,0.49))
scatter.smooth(partial_asian$x,partial_asian$y,span=.5,
               main="Partial Dependence Plot: Admittance on Asian",
               pch=19,col="red",
               ylim=c(0.40,0.49))
scatter.smooth(partial_black$x,partial_black$y,span=.5,
               main="Partial Dependence Plot: Admittance on Black",
               pch=19,col="green",
               ylim=c(0.40,0.49))

# GPA
partial_gpa <- partialPlot(rf2,pred.data=dataFinal,x.var=gpa.wtd,which.class=1)

par(mfrow=c(1,1))
scatter.smooth(partial_gpa$x,partial_gpa$y,span=.25,
               main="Partial Dependence Plot: Admittance on GPA",
               pch=19,col="black",
               ylim=c(-1,2))

# SAT
partial_satVerb <- partialPlot(rf2,pred.data=dataFinal,x.var=sati.verb,which.class=1)
partial_satMath <- partialPlot(rf2,pred.data=dataFinal,x.var=sati.math,which.class=1)

par(mfrow=c(1,2))
scatter.smooth(partial_satVerb$x,partial_satVerb$y,span=.25,
               main="Partial Dependence Plot: Admittance on SAT Verbal",
               pch=19,col="blue",
               ylim=c(-0.5,1.5))
scatter.smooth(partial_satMath$x,partial_satMath$y,span=.25,
               main="Partial Dependence Plot: Admittance on SAT Math",
               pch=19,col="red",
               ylim=c(-0.5,1.5))

# income
partial_income <- partialPlot(rf2,pred.data=dataFinal,x.var=income,which.class=1)

par(mfrow=c(1,1))
scatter.smooth(partial_income$x,partial_income$y,span=.25,
               main="Partial Dependence Plot: Admittance on Income",
               pch=19,col="black",
               ylim=c(0.1,0.6))

### Empirical Margins
mr <- margin(rf2)
summary(mr)
sum(mr>0)/6175 # 0.7397571
hist(mr,
     main="Histogram of Empirical Margins",
     xlab="Empirical Margin")

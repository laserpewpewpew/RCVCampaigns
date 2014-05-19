########################
# 2 Candidate Election #
########################
library(foreign)
myData2 <- read.csv("simdata2cands.csv")
myData2$lambda <- ((myData2$b*(myData2$y+myData2$d)/(1+myData2$z))-(myData2$c*myData2$k))
myData2$delta <- ((myData2$b*((myData2$y)/(1+myData2$z)))-(myData2$c*myData2$k))

myData2$lambdapos[myData2$lambda>0] <- 1
myData2$lambdapos[myData2$lambda<=0] <- 0

myData2$deltapos[myData2$delta>0] <- 1
myData2$deltapos[myData2$delta<=0] <- 0
summary(myData2$lambdapos)
summary(myData2$deltapos)
# These summaries show a 3% reduction in positive utility possibilities

require(ggplot2)
lambda.myData2 <- qplot(myData2$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 2 Candidate Race") +
  theme_bw()

lambda.myData2

require(ggplot2)
delta.myData2 <- qplot(myData2$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 2 Candidate Race") +
  theme_bw()

delta.myData2

library(doBy)
lambdaavgs.myData2 <- summaryBy(lambda ~ d, data=myData2, FUN=c(mean), na.rm=T)


########################
# 3 Candidate Election #
########################
library(foreign)
myData3 <- read.csv("simdata3cands.csv")
myData3$lambda <- ((myData3$b*(myData3$y+myData3$d)/(1+myData3$z))-(myData3$c*myData3$k))
myData3$delta <- ((myData3$b*((myData3$y)/(1+myData3$z)))-(myData3$c*myData3$k))

myData3$lambdapos[myData3$lambda>0] <- 1
myData3$lambdapos[myData3$lambda<=0] <- 0

myData3$deltapos[myData3$delta>0] <- 1
myData3$deltapos[myData3$delta<=0] <- 0
summary(myData3$lambdapos)
summary(myData3$deltapos)
# These summaries show a 3% reduction in positive utility possibilities

require(ggplot2)
lambda.myData3 <- qplot(myData3$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 3 Candidate Race") +
  theme_bw()

lambda.myData3

require(ggplot2)
delta.myData3 <- qplot(myData3$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 3 Candidate Race") +
  theme_bw()

delta.myData3

library(doBy)
lambdaavgs.myData3 <- summaryBy(lambda ~ d, data=myData3, FUN=c(mean), na.rm=T)


########################
# 4 Candidate Election #
########################
library(foreign)
myData4 <- read.csv("simdata4cands.csv")
myData4$lambda <- ((myData4$b*(myData4$y+myData4$d)/(1+myData4$z))-(myData4$c*myData4$k))
myData4$delta <- ((myData4$b*((myData4$y)/(1+myData4$z)))-(myData4$c*myData4$k))

myData4$lambdapos[myData4$lambda>0] <- 1
myData4$lambdapos[myData4$lambda<=0] <- 0

myData4$deltapos[myData4$delta>0] <- 1
myData4$deltapos[myData4$delta<=0] <- 0
summary(myData4$lambdapos)
summary(myData4$deltapos)
# These summaries show a 4% reduction in positive utility possibilities

require(ggplot2)
lambda.myData4 <- qplot(myData4$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Election - 4 Candidate Race") +
  theme_bw()

lambda.myData4

require(ggplot2)
delta.myData4 <- qplot(myData4$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 4 Candidate Race") +
  theme_bw()

delta.myData4

library(doBy)
lambdaavgs.myData4 <- summaryBy(lambda ~ d, data=myData4, FUN=c(mean), na.rm=T)

########################
# 5 Candidate Election #
########################
library(foreign)
myData5 <- read.csv("simdata5cands.csv")
myData5$lambda <- ((myData5$b*(myData5$y+myData5$d)/(1+myData5$z))-(myData5$c*myData5$k))
myData5$delta <- ((myData5$b*((myData5$y)/(1+myData5$z)))-(myData5$c*myData5$k))

myData5$lambdapos[myData5$lambda>0] <- 1
myData5$lambdapos[myData5$lambda<=0] <- 0

myData5$deltapos[myData5$delta>0] <- 1
myData5$deltapos[myData5$delta<=0] <- 0
summary(myData5$lambdapos)
summary(myData5$deltapos)
# These summaries show a 5.4% reduction in positive utility possibilities

require(ggplot2)
lambda.myData5<- qplot(myData5$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 5 Candidate Race") +
  theme_bw()

lambda.myData5

require(ggplot2)
delta.myData5 <- qplot(myData5$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 5 Candidate Race") +
  theme_bw()

delta.myData5

library(doBy)
lambdaavgs.myData5 <- summaryBy(lambda ~ d, data=myData5, FUN=c(mean), na.rm=T)

########################
# 6 Candidate Election #
########################

library(foreign)
myData6 <- read.csv("simdata6cands.csv")
myData6$lambda <- ((myData6$b*(myData6$y+myData6$d)/(1+myData6$z))-(myData6$c*myData6$k))
myData6$delta <- ((myData6$b*((myData6$y)/(1+myData6$z)))-(myData6$c*myData6$k))

myData6$lambdapos[myData6$lambda>0] <- 1
myData6$lambdapos[myData6$lambda<=0] <- 0

myData6$deltapos[myData6$delta>0] <- 1
myData6$deltapos[myData6$delta<=0] <- 0
summary(myData6$lambdapos)
summary(myData6$deltapos)
# These summaries show us a roughly 15% reduction in positive utility possibilities

require(ggplot2)
lambda.myData6 <- qplot(myData6$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 6 Candidate Race") +
  theme_bw()

lambda.myData6

require(ggplot2)
delta.myData6 <- qplot(myData6$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 6 Candidate Race") +
  theme_bw()

delta.myData6

library(doBy)
lambdaavgs.myData6 <- summaryBy(lambda ~ d, data=myData6, FUN=c(mean), na.rm=T)

#############################################
# Rerun all functions without b and c terms #
#############################################

########################
# 2 Candidate Election #
########################
library(foreign)
myData2 <- read.csv("simdata2cands.csv")
myData2$lambda.n <- (((myData2$y+myData2$d)/(1+myData2$z))-(myData2$k))
myData2$delta.n <- ((((myData2$y)/(1+myData2$z)))-(myData2$k))

myData2$lambdapos.n[myData2$lambda.n>0] <- 1
myData2$lambdapos.n[myData2$lambda.n<=0] <- 0

myData2$deltapos.n[myData2$delta.n>0] <- 1
myData2$deltapos.n[myData2$delta.n<=0] <- 0
summary(myData2$lambdapos.n)
summary(myData2$deltapos.n)
# These summaries show a 3% reduction in positive utility possibilities

require(ggplot2)
lambda.myData2.n <- qplot(myData2$lambda.n, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 2 Candidate Race") +
  theme_bw()

lambda.myData2.n

require(ggplot2)
delta.myData2.n <- qplot(myData2$delta.n, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 2 Candidate Race") +
  theme_bw()

delta.myData2.n

library(doBy)
lambdaavgs.myData2.n <- summaryBy(lambda.n ~ d, data=myData2, FUN=c(mean), na.rm=T)


########################
# 3 Candidate Election #
########################
library(foreign)
myData3 <- read.csv("simdata3cands.csv")
myData3$lambda.n <- (((myData3$y+myData3$d)/(1+myData3$z))-(myData3$k))
myData3$delta.n <- ((((myData3$y)/(1+myData3$z)))-(myData3$k))

myData3$lambdapos.n[myData3$lambda.n>0] <- 1
myData3$lambdapos.n[myData3$lambda.n<=0] <- 0

myData3$deltapos.n[myData3$delta.n>0] <- 1
myData3$deltapos.n[myData3$delta.n<=0] <- 0
summary(myData3$lambdapos.n)
summary(myData3$deltapos.n)
# These summaries show a 3% reduction in positive utility possibilities

require(ggplot2)
lambda.myData3.n <- qplot(myData3$lambda.n, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 3 Candidate Race") +
  theme_bw()

lambda.myData3.n

require(ggplot2)
delta.myData3.n <- qplot(myData3$delta.n, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 3 Candidate Race") +
  theme_bw()

delta.myData3.n

library(doBy)
lambdaavgs.myData3.n <- summaryBy(lambda.n ~ d, data=myData3, FUN=c(mean), na.rm=T)


########################
# 4 Candidate Election #
########################
library(foreign)
myData4 <- read.csv("simdata4cands.csv")
myData4$lambda.n <- (((myData4$y+myData4$d)/(1+myData4$z))-(myData4$k))
myData4$delta.n <- ((((myData4$y)/(1+myData4$z)))-(myData4$k))

myData4$lambdapos.n[myData4$lambda.n>0] <- 1
myData4$lambdapos.n[myData4$lambda.n<=0] <- 0

myData4$deltapos.n[myData4$delta.n>0] <- 1
myData4$deltapos.n[myData4$delta.n<=0] <- 0
summary(myData4$lambdapos.n)
summary(myData4$deltapos.n)
# These summaries show a 4% reduction in positive utility possibilities

require(ggplot2)
lambda.myData4.n <- qplot(myData4$lambda.n, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Election - 4 Candidate Race") +
  theme_bw()

lambda.myData4.n

require(ggplot2)
delta.myData4.n <- qplot(myData4$delta.n, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 4 Candidate Race") +
  theme_bw()

delta.myData4.n

library(doBy)
lambdaavgs.myData4.n <- summaryBy(lambda.n ~ d, data=myData4, FUN=c(mean), na.rm=T)

########################
# 5 Candidate Election #
########################
library(foreign)
myData5 <- read.csv("simdata5cands.csv")
myData5$lambda.n <- (((myData5$y+myData5$d)/(1+myData5$z))-(myData5$k))
myData5$delta.n <- ((((myData5$y)/(1+myData5$z)))-(myData5$k))

myData5$lambdapos.n[myData5$lambda.n>0] <- 1
myData5$lambdapos.n[myData5$lambda.n<=0] <- 0

myData5$deltapos.n[myData5$delta.n>0] <- 1
myData5$deltapos.n[myData5$delta.n<=0] <- 0
summary(myData5$lambdapos.n)
summary(myData5$deltapos.n)
# These summaries show a 5.4% reduction in positive utility possibilities

require(ggplot2)
lambda.myData5.n <- qplot(myData5$lambda.n, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 5 Candidate Race") +
  theme_bw()

lambda.myData5.n

require(ggplot2)
delta.myData5.n <- qplot(myData5$delta.n, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 5 Candidate Race") +
  theme_bw()

delta.myData5.n

library(doBy)
lambdaavgs.myData5.n <- summaryBy(lambda.n ~ d, data=myData5, FUN=c(mean), na.rm=T)

########################
# 6 Candidate Election #
########################

library(foreign)
myData6 <- read.csv("simdata6cands.csv")
myData6$lambda.n <- (((myData6$y+myData6$d)/(1+myData6$z))-(myData6$k))
myData6$delta.n <- ((((myData6$y)/(1+myData6$z)))-(myData6$k))

myData6$lambdapos.n[myData6$lambda.n>0] <- 1
myData6$lambdapos.n[myData6$lambda.n<=0] <- 0

myData6$deltapos.n[myData6$delta.n>0] <- 1
myData6$deltapos.n[myData6$delta.n<=0] <- 0
summary(myData6$lambdapos.n)
summary(myData6$deltapos.n)
# These summaries show us a roughly 15% reduction in positive utility possibilities

require(ggplot2)
lambda.myData6.n <- qplot(myData6$lambda.n, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 6 Candidate Race") +
  theme_bw()

lambda.myData6.n

require(ggplot2)
delta.myData6.n <- qplot(myData6$delta.n, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 6 Candidate Race") +
  theme_bw()

delta.myData6.n

library(doBy)
lambdaavgs.myData6.n <- summaryBy(lambda.n ~ d, data=myData6, FUN=c(mean), na.rm=T)


##################
# Candidate Data #
##################
library(foreign)
canddat <- read.csv("candidatercv.csv")
names(canddat)[65] <- "spent"

canddat$neg[canddat$B8rival==6] <- 0
canddat$neg[canddat$B8rival==1] <- 0
canddat$neg[canddat$B8rival==2] <- 0
canddat$neg[canddat$B8rival==3] <- 1
canddat$neg[canddat$B8rival==4] <- 2
canddat$neg[canddat$B8rival==5] <- 2

canddat$neg.0[canddat$neg==0] <- 1
canddat$neg.0[canddat$neg!=0] <- 0

canddat$neg.1[canddat$neg==1] <- 1
canddat$neg.1[canddat$neg!=1] <- 0

canddat$neg.2[canddat$neg==2] <- 1
canddat$neg.2[canddat$neg!=2] <- 0

canddat$neg.f <- factor(canddat$neg, labels=c("Mostly/Somewhat Positive", "Neither Positive/Negative", "Somewhat/Mostly Negative"))
canddat$candvoteshare <- as.numeric(canddat$candvoteshare)
canddat$age <- as.numeric(canddat$age)
canddat$B1interest <- as.numeric(canddat$B1interest)
canddat$C3educ <- as.numeric(canddat$C3educ)
canddat$C6female <- as.numeric(canddat$C6female)
canddat$spent <- as.numeric(canddat$spent)

canddat$neglogit[canddat$B8rival==6] <- 0
canddat$neglogit[canddat$B8rival==1] <- 0
canddat$neglogit[canddat$B8rival==2] <- 0
canddat$neglogit[canddat$B8rival==3] <- 0
canddat$neglogit[canddat$B8rival==4] <- 1
canddat$neglogit[canddat$B8rival==5] <- 1

canddat$info[canddat$B3info==1] <- 4
canddat$info[canddat$B3info==2] <- 3
canddat$info[canddat$B3info==3] <- 2
canddat$info[canddat$B3info==4] <- 1

canddat$rivalneg[canddat$B7you==6] <- 0
canddat$rivalneg[canddat$B7you==5] <- 1
canddat$rivalneg[canddat$B7you==4] <- 1
canddat$rivalneg[canddat$B7you==3] <- 0
canddat$rivalneg[canddat$B7you==2] <- 0
canddat$rivalneg[canddat$B7you==1] <- 0

canddat$emp[canddat$C1emp==1] <- 1
canddat$emp[canddat$C1emp==2] <- 1
canddat$emp[canddat$C1emp==3] <- 0
canddat$emp[canddat$C1emp==4] <- 0
canddat$emp[canddat$C1emp==5] <- 0
canddat$emp[canddat$C1emp==6] <- 0
canddat$emp[canddat$C1emp==7] <- 0

canddat$white[canddat$C4race==1] <- 1
canddat$white[canddat$C4race==0] <- 0
canddat$white[canddat$C4race==2] <- 0
canddat$white[canddat$C4race==3] <- 0
canddat$white[canddat$C4race==4] <- 0
canddat$white[canddat$C4race==5] <- 0
canddat$white[canddat$C4race==6] <- 0

# remove missing cases
# remove missing values of the weight variable
myDat0 <- canddat[complete.cases(canddat[, c("B8rival")]), ]
myDat1 <- myDat0[complete.cases(myDat0[, c("spent")]), ]
myDat2 <- myDat1[complete.cases(myDat1[, c("info")]), ]
myDat3 <- myDat2[complete.cases(myDat2[, c("B1interest")]), ]
myDat4 <- myDat3[complete.cases(myDat3[, c("rivalneg")]), ]
myDat5 <- myDat4[complete.cases(myDat4[, c("recentwin")]), ]
myDat6 <- myDat5[complete.cases(myDat5[, c("candvoteshare")]), ]
myDat7 <- myDat6[complete.cases(myDat6[, c("emp")]), ]
myDat8 <- myDat7[complete.cases(myDat7[, c("C3educ")]), ]
myDat9 <- myDat8[complete.cases(myDat8[, c("white")]), ]
myDat10 <- myDat9[complete.cases(myDat9[, c("C6female")]), ]
canddata <- myDat10[complete.cases(myDat10[, c("age")]), ]
canddata <- canddata[canddata$B8rival!=6, ]

canddata$spent <- log(canddata$spent)

require(MASS)
olog.neg <- polr(neg.f ~ rcv + recentwin + rivalneg + candvoteshare + spent, data=canddata, Hess = TRUE)
olog.neg2 <- polr(neg.f ~ rcv + recentwin + candvoteshare + spent, data=canddata, Hess = TRUE)


require(stargazer)
stargazer(olog.neg, olog.neg2,
          title=c("Effect of Electoral System Type on Decision to go Negative"),
          dep.var.labels=c("Negativity"),
          covariate.labels=c("RCV",
                             "Recent Winner",
                             "Rival Negativity",
                             "Vote Share",
                             "Spending(log)"),
          notes=c("Unstandardized ordered logistic coefficients with standard errors below in parantheses.  Source: Candidate Survey")
          )



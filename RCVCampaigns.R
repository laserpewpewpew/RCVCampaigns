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
lambdaavgs.myData4 <- summaryBy(lambda ~ d, data=myData3, FUN=c(mean), na.rm=T)

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
lambdaavgs.myData5 <- summaryBy(lambda ~ d, data=myData4, FUN=c(mean), na.rm=T)

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

########################
# 7 Candidate Election #
########################

library(foreign)
myData7 <- read.csv("simdata7cands.csv")
myData7$lambda <- ((myData7$b*(myData7$y+myData7$d)/(1+myData7$z))-(myData7$c*myData7$k))
myData7$delta <- ((myData7$b*((myData7$y)/(1+myData7$z)))-(myData7$c*myData7$k))

myData7$lambdapos[myData7$lambda>0] <- 1
myData7$lambdapos[myData7$lambda<=0] <- 0

myData7$deltapos[myData7$delta>0] <- 1
myData7$deltapos[myData7$delta<=0] <- 0
summary(myData7$lambdapos)
summary(myData7$deltapos)
# These summaries show us a roughly 15% reduction in positive utility possibilities

require(ggplot2)
lambda.myData7 <- qplot(myData7$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 7 Candidate Race") +
  theme_bw()

lambda.myData7

require(ggplot2)
delta.myData7 <- qplot(myData7$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 7 Candidate Race") +
  theme_bw()

delta.myData7

library(doBy)
lambdaavgs.myData7 <- summaryBy(lambda ~ d, data=myData7, FUN=c(mean), na.rm=T)

########################
# 8 Candidate Election #
########################

library(foreign)
myData8 <- read.csv("simdata8cands.csv")
myData8$lambda <- ((myData8$b*(myData8$y+myData8$d)/(1+myData8$z))-(myData8$c*myData8$k))
myData8$delta <- ((myData8$b*((myData8$y)/(1+myData8$z)))-(myData8$c*myData8$k))

myData8$lambdapos[myData8$lambda>0] <- 1
myData8$lambdapos[myData8$lambda<=0] <- 0

myData8$deltapos[myData8$delta>0] <- 1
myData8$deltapos[myData8$delta<=0] <- 0
summary(myData8$lambdapos)
summary(myData8$deltapos)
# These summaries show us a roughly 15% reduction in positive utility possibilities

require(ggplot2)
lambda.myData8 <- qplot(myData8$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 8 Candidate Race") +
  theme_bw()

lambda.myData8

require(ggplot2)
delta.myData8 <- qplot(myData8$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 8 Candidate Race") +
  theme_bw()

delta.myData8

library(doBy)
lambdaavgs.myData8 <- summaryBy(lambda ~ d, data=myData8, FUN=c(mean), na.rm=T)


########################
# 8 Candidate Election #
########################

library(foreign)
myData9 <- read.csv("simdata9cands.csv")
myData9$lambda <- ((myData9$b*(myData9$y+myData9$d)/(1+myData9$z))-(myData9$c*myData9$k))
myData9$delta <- ((myData9$b*((myData9$y)/(1+myData9$z)))-(myData9$c*myData9$k))

myData9$lambdapos[myData9$lambda>0] <- 1
myData9$lambdapos[myData9$lambda<=0] <- 0

myData9$deltapos[myData9$delta>0] <- 1
myData9$deltapos[myData9$delta<=0] <- 0
summary(myData9$lambdapos)
summary(myData9$deltapos)
# These summaries show us a roughly 15% reduction in positive utility possibilities

require(ggplot2)
lambda.myData9 <- qplot(myData9$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections - 9 Candidate Race") +
  theme_bw()

lambda.myData9

require(ggplot2)
delta.myData9 <- qplot(myData9$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections - 9 Candidate Race") +
  theme_bw()

delta.myData9

library(doBy)
lambdaavgs.myData9 <- summaryBy(lambda ~ d, data=myData9, FUN=c(mean), na.rm=T)


######################################
# Bayesian Linear Regression in Stan #
######################################


require(rstan)

# First we have to define the model
regdays.code <- '
data {
int<lower=0> N;
vector[N] regdays;
vector[N] stategini;
vector[N] stdiversity;
vector[N] over64;
vector[N] college;
vector[N] stincpc;
vector[N] south;
}
parameters {                
real beta1;             // coef for constant (default prior is uniform, i.e., noninformative)
real beta2;             // coef for stategini
real beta3;
real beta4;
real beta5;
real beta6;
real beta7;
real<lower=0> sigma;
}
model {
regdays ~ normal(beta1 + beta2 * stategini + beta3 * stdiversity +
beta4 * over64 + beta5 * college +
beta6 * stincpc + beta7 * south, sigma);
}
'

# Then put the data into the expected format
states.data <- list(N = nrow(states), regdays = states$regdays, stategini = states$stategini,
                    stdiversity = states$stdiversity, over64 = states$over64, 
                    college = states$college, stincpc = states$stincpc, south = states$south)

# Now we can run it
set.seed(324)
m1.stan <- stan(model_code = regdays.code, data = states.data, 
                iter = 10000, chains = 3)

print(m1.stan)
m1.stan.sim <- as.data.frame(m1.stan)

b.gini.plot <- qplot(m1.stan.sim$beta2, geom="density") + 
  xlab("Coefficient of State Income Inequality") + 
  ylab("Density of Posterior Distribution") +
  theme_bw()

b.gini.plot

# Graph regression results
states2 <- states

# Standardize continuous IVs by dividing by 2 s.d.s (per Gelman (2008))
for (iv in 4:8) {
  states2[,iv] <- states2[,iv]/(2*sd(states2[,iv]))
}

states2.data <- list(N = nrow(states2), regdays = states2$regdays, stategini = states2$stategini,
                     stdiversity = states2$stdiversity, over64 = states2$over64, 
                     college = states2$college, stincpc = states2$stincpc, south = states2$south)

set.seed(324)
m1.stan2 <- stan(fit=m1.stan, data=states2.data, iter = 10000, chains = 3)
m1.stan2.sim <- as.data.frame(m1.stan2)

# HDI.posterior is based, in part, on Kruschke (2011, 628-29)
HDI.posterior <- function(data = NULL, mass = .95) {
  n.var <- dim(data)[2]-2
  results.HDI <- matrix(rep(NA,3*n.vars), nrow=n.vars, ncol=3)
  for (var in 1:n.var) {
    post <- data[,var]
    sorted.post <- sort(post)
    ci.idx <- floor(mass * length(sorted.post))
    n.ci <- length(sorted.post) - ci.idx
    ci.width <- rep(0, n.ci)
    for (i in 1:n.ci) {
      ci.width[i] <- sorted.post[i+ci.idx] - sorted.post[i]
    }
    HDI.min <- sorted.post[which.min(ci.width)]
    HDI.max <- sorted.post[which.min(ci.width)+ci.idx]
    mean.post <- mean(post)
    results.HDI[var,] <- c(mean.post, HDI.min, HDI.max)
  }  
  results.HDI <- as.data.frame(results.HDI)
  names(results.HDI) <- c("b", "lb", "ub")
  return(results.HDI)
}

reg.results <- HDI.posterior(m1.stan2.sim)   
reg.results <- reg.results[-1,]             # exclude constant (not interesting)
reg.results$no <- 1:dim(reg.results)[1]     # an index to order the variables
reg.results$var <- c("Income Inequality", "Ethnic Diversity", "Senior Population",
                     "College-Educated Population", "GDP/pc", "South") # variable names

reg.plot <- ggplot(data = reg.results, aes(y = no, x = b)) +
  geom_point() + geom_errorbarh(aes(xmin = lb, xmax = ub, height=0)) +
  ylab("") + xlab("") + theme_bw() + 
  scale_y_reverse(breaks = 1:dim(reg.results)[1], 
                  labels = reg.results[1:dim(reg.results)[1],"var"]) +
  geom_vline(xintercept=c(0), linetype="dotted")

reg.plot



regdays.code2 <- '
data {
int<lower=0> N;
vector[N] regdays;
vector[N] stategini;
vector[N] stdiversity;
vector[N] over64;
vector[N] college;
vector[N] stincpc;
vector[N] south;
}
parameters {                
real beta1;             // coef for constant (default prior is uniform, i.e., noninformative)
real beta2;             // coef for stategini
real beta3;
real beta4;
real beta5;
real beta6;
real beta7;
real<lower=0> sigma;
}
model {
beta1 ~ cauchy(0, 2.5);
beta2 ~ cauchy(0, 2.5);
beta3 ~ cauchy(0, 2.5);
beta4 ~ cauchy(0, 2.5);
beta5 ~ cauchy(0, 2.5);
beta6 ~ cauchy(0, 2.5);
beta7 ~ cauchy(0, 2.5);

regdays ~ normal(beta1 + beta2 * stategini + beta3 * stdiversity +
beta4 * over64 + beta5 * college +
beta6 * stincpc + beta7 * south, sigma);
}
'

set.seed(324)
m1.stan2.s2 <- stan(model_code = regdays.code2, data = states2.data, 
                    iter = 10000, chains = 3)

print(m1.stan2)
print(m1.stan2.s2)
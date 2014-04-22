########################
# 6 Candidate Election #
########################

library(foreign)
myData <- read.csv("simdata.csv")
myData$lambda <- ((myData$b*(myData$y+myData$d)/(1+myData$z))-(myData$c*myData$k))
myData$delta <- ((myData$b*((myData$y)/(1+myData$z)))-(myData$c*myData$k))

myData$lambdapos[myData$lambda>0] <- 1
myData$lambdapos[myData$lambda<=0] <- 0

myData$deltapos[myData$delta>0] <- 1
myData$deltapos[myData$delta<=0] <- 0
summary(myData$lambdapos)
summary(myData$deltapos)
# These summaries show us a roughly 15% reduction in positive utilitity possibilities

require(ggplot2)
lambda.plot <- qplot(myData$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections") +
  theme_bw()

lambda.plot

require(ggplot2)
delta.plot <- qplot(myData$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections") +
  theme_bw()

delta.plot

library(doBy)
lambdaavgs <- summaryBy(lambda ~ d, data=myData, FUN=c(mean), na.rm=T)

require(ggplot2)
lambdaavgs.plot <- qplot(lambdaavgs$lambda.mean, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections") +
  theme_bw()

lambdaavgs.plot

########################
# 3 Candidate Election #
########################
library(foreign)
myData2 <- read.csv("simdata3cands.csv")
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
lambda.plot2 <- qplot(myData2$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections") +
  theme_bw()

lambda.plot2

require(ggplot2)
delta.plot2 <- qplot(myData2$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections") +
  theme_bw()

delta.plot2

library(doBy)
lambdaavgs2 <- summaryBy(lambda ~ d, data=myData2, FUN=c(mean), na.rm=T)

require(ggplot2)
lambdaavgs.plot2 <- qplot(lambdaavgs2$lambda.mean, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections") +
  theme_bw()

lambdaavgs.plot2



########################
# 4 Candidate Election #
########################
library(foreign)
myData3 <- read.csv("simdata4cands.csv")
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
lambda.plot3 <- qplot(myData3$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections") +
  theme_bw()

lambda.plot3

require(ggplot2)
delta.plot3 <- qplot(myData3$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections") +
  theme_bw()

delta.plot3

library(doBy)
lambdaavgs3 <- summaryBy(lambda ~ d, data=myData3, FUN=c(mean), na.rm=T)

require(ggplot2)
lambdaavgs.plot3 <- qplot(lambdaavgs3$lambda.mean, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections") +
  theme_bw()

lambdaavgs.plot3



########################
# 5 Candidate Election #
########################
library(foreign)
myData4 <- read.csv("simdata5cands.csv")
myData4$lambda <- ((myData4$b*(myData4$y+myData4$d)/(1+myData4$z))-(myData4$c*myData4$k))
myData4$delta <- ((myData4$b*((myData4$y)/(1+myData4$z)))-(myData4$c*myData4$k))

myData4$lambdapos[myData4$lambda>0] <- 1
myData4$lambdapos[myData4$lambda<=0] <- 0

myData4$deltapos[myData4$delta>0] <- 1
myData4$deltapos[myData4$delta<=0] <- 0
summary(myData4$lambdapos)
summary(myData4$deltapos)
# These summaries show a 3% reduction in positive utility possibilities

require(ggplot2)
lambda.plot4 <- qplot(myData4$lambda, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for RCV Elections") +
  theme_bw()

lambda.plot4

require(ggplot2)
delta.plot4 <- qplot(myData4$delta, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections") +
  theme_bw()

delta.plot4

library(doBy)
lambdaavgs4 <- summaryBy(lambda ~ d, data=myData4, FUN=c(mean), na.rm=T)

require(ggplot2)
lambdaavgs.plot4 <- qplot(lambdaavgs4$lambda.mean, geom="density") + 
  xlab("Negative Campaigning Utility") + 
  ylab("Density for non-RCV Elections") +
  theme_bw()

lambdaavgs.plot4


#####################################
# Bayesian Linear Regression in Stan
# Install by following the directions at <https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started>
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
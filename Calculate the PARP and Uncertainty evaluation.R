library(reshape2)
##Taking Western Africa as an example for calculating PARP
##Calculate the average temperature during pregnancy simulated by six DIMIP models
##based on the latitude, longitude, birthday, and gestational age information in the DHS data.
##Each model has two scenarios: "hist" and "hist-nat"
load("C:/Users/朱钲宏/Desktop/返修0731/figdata1/figdata/Example data for calculating PARP.RData")

##Calculate the exposure frequency of the study population to heat (>90th) in different scenarios
##and organize the data into an appropriate format
dat_p <- as.data.frame(table(dat$Country))

for(i in 15:26){
  dat[,i] <- ifelse(dat[,i]>dat$Q90,1,0)
  message(i)
  x <- data.frame(table(dat$Country,dat[,i]))
  x[,3] <- x[,3]/dat_p$Freq
  x <- subset(x,x$Var2==1)
  dat_p <- cbind(dat_p,x[,3])
  message(i)
}

colnames(dat_p)[3:14] <- colnames(dat)[15:26]

dat_p1 <- data.frame(Country = dat_p$Var1)
dat_p1$model <- "history"
dat_p1 <- cbind(dat_p1,dat_p[,grepl("history",colnames(dat_p))])
colnames(dat_p1) <- gsub("history","",colnames(dat_p1))

dat_p2 <- data.frame(Country = dat_p$Var1)
dat_p2$model <- "hist.nat"
dat_p2 <- cbind(dat_p2,dat_p[,grepl("hist.nat",colnames(dat_p))])
colnames(dat_p2) <- gsub("hist.nat","",colnames(dat_p2))

dat_p <- rbind(dat_p1,dat_p2)
dat_p <- melt(dat_p)
dat_p <- dcast(dat_p,Country+variable~model)

LBWrate <- dat[!duplicated(dat$Country),c("Country","rate")]
dat_p <- merge(dat_p,LBWrate)

##
set.seed(123)
##We quantified the uncertainty of the estimates by generating 1000 samples
##of the coefficients of the heat (representing the association) through Monte Carlo simulations,
##assuming a multivariate normal distribution for the estimated GEE model coefficients 
effect <- rnorm(1000, mean = 0.113, sd = 	0.052)
n_sim=1000
sample <- dat_p[sample(1:nrow(dat_p),  n_sim, replace = TRUE), ]
sample$effect <- sample(effect,  n_sim, replace = TRUE)

##Adjust RR based on low birth weight rates in various countries
sample$RR <- exp(sample$effect)/((1-sample$rate/100)+sample$rate*exp(sample$effect)/100)

##Calculating PARP and 95% CI;See the article 'methods'for more details
sample$PARP_hist <- 1-1/(sample$history*(sample$RR-1)+1)
sample$PARP_hist_nat <- 1-1/(sample$hist.nat*(sample$RR-1)+1)
sample$AP <- (sample$PARP_hist-sample$PARP_hist_nat)/sample$PARP_hist

lower <- function(x) {quantile(x,c(0.025),na.rm=TRUE)}
upper <- function(x) {quantile(x,c(0.975),na.rm=TRUE)}

f1 <- round(lower(sample$PARP_hist)*100,2)
f2 <- round(mean(sample$PARP_hist)*100,2)
f3 <- round(upper(sample$PARP_hist)*100,2)

cat("PARP_hist, 95%CI", paste0(f2,"(",f1,",",f3,")"), "\n")

f1 <- round(lower(sample$PARP_hist_nat)*100,2)
f2 <- round(mean(sample$PARP_hist_nat)*100,2)
f3 <- round(upper(sample$PARP_hist_nat)*100,2)

cat("PARP_hist_nat, 95%CI", paste0(f2,"(",f1,",",f3,")"), "\n")

f1 <- round(lower(sample$AP)*100,2)
f2 <- round(mean(sample$AP)*100,2)
f3 <- round(upper(sample$AP)*100,2)

cat("AP, 95%CI", paste0(f2,"(",f1,",",f3,")"), "\n")

##Uncertainty evaluation
library(sensitivity)
set.seed(123)
# 生成1000条均值为1，标准差为0.2的正态分布数据
effect <- rnorm(1000, mean = 0.113, sd = 	0.052)
n_sim=10000
sample <- dat_p[sample(1:nrow(dat_p),  n_sim, replace = TRUE), ]
sample$effect <- sample(effect,  n_sim, replace = TRUE)
sample$RR <- exp(sample$effect)/((1-sample$rate/100)+sample$rate*exp(sample$effect)/100)
sample <- sample[,c(3:4,7)]

sample1 <- dat_p[sample(1:nrow(dat_p),  n_sim, replace = TRUE), ]
sample1$effect <- sample(effect,  n_sim, replace = TRUE)
sample1$RR <- exp(sample1$effect)/((1-sample1$rate/100)+sample1$rate*exp(sample1$effect)/100)
sample1 <- sample1[,c(3:4,7)]

calculate <- function(x) {
  y <- 1/(x[,1]*(x[,3]-1)+1)-1/(x[,2]*(x[,3]-1)+1)
  y
}

# Sobol' sensitivity analysis
sobol_results <- sobol2007(model = calculate, X1 = sample, X2 = sample1, order = 1, nboot = 1000)

# Extract the Sobol' indices
sobol_indices <- sobol_results$S
print(sobol_indices)
plot(sobol_results)









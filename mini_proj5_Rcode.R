setwd("F:/Academic/Fall 18@UTD/Statistical Methods for DS/Mini Projects")
temp_hrate <-  read.csv(file = "bodytemp-heartrate.csv", header=TRUE, sep=",")
# 1.a)
 male_temp <- temp_hrate$body_temperature[temp_hrate$gender == 1]
 female_temp <- temp_hrate$body_temperature[temp_hrate$gender == 2]
 boxplot(male_temp, female_temp)
 var(male_temp)
 var(female_temp)
 t.test(male_temp, female_temp, alternative = "two.sided", conf.level = 0.95,var.equal = FALSE) 
# 1.b)
 male_hrate <- temp_hrate$heart_rate[temp_hrate$gender == 1]
 female_hrate <- temp_hrate$heart_rate[temp_hrate$gender == 2]
 boxplot(male_hrate, female_hrate)
 var(male_hrate)
 var(female_hrate)
 t.test(male_hrate, female_hrate, alternative = "two.sided", conf.level = 0.95,var.equal = FALSE)
# 1.c)
 plot(temp_hrate$body_temperature, temp_hrate$heart_rate)
 abline(lm(temp_hrate$heart_rate ~ temp_hrate$body_temperature))
 plot(male_temp, male_hrate)
 abline(lm(male_hrate ~ male_temp))
 plot(female_temp, female_hrate)
 abline(lm(female_hrate ~ female_temp))
-------------------------------------------------------------
# Q2.
mean.star <- function(x) {
  n <- length(x)
  xbar <- mean(x)
  # For resample, lambda = 1/xbar
  xstar <- rexp(n, 1/xbar)
  meanstar <- mean(xstar)
  return(meanstar)
}
 
 conf_intervals <- function(n, lambda) {
   x <- rexp(n, lambda)
   ## 1st Conf interval - large z-sample CI(aplha = 0.05)
   conf_int1 <- mean(x) + c(-1, 1)*qnorm(1 - (0.05/2))*sd(x)/sqrt(n)
   
   ##2nd Conf interval - parametric percentile bootstrap CI(aplha = 0.05)
   nboot <- 1000
   mean.pboot.dist <- replicate(nboot, mean.star(x))
   conf_int2 <- sort(mean.pboot.dist)[c(25, 975)]
   
   return(c(conf_int1,conf_int2))
 }

 n_sim <- 5000
 #Case 1)
  n <- 1000
  lambda <- 10
  mean_exp <- 1/lambda
  conf_int_mc <- replicate(n_sim, conf_intervals(n,lambda))
  dim(conf_int_mc)
  
  prop_conf_int1 <- mean((mean_exp >= conf_int_mc[1,])*(mean_exp <= conf_int_mc[2,]))
  prop_conf_int2 <- mean((mean_exp >= conf_int_mc[3,])*(mean_exp <= conf_int_mc[4,]))
  
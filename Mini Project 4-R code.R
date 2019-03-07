#Q1.
gpa <- read.csv(file = "gpa.csv", header=TRUE, sep=",")
#to find scatter plot:
plot(gpa$gpa, gpa$act, main="gpa & act", xlab = "gpa", ylab = "act")
cor(gpa$gpa, gpa$act)
library(boot)
cor.npar <- function(x,indices){
  result <- cor(x$gpa[indices],x$act[indices])
  return (result)
}
set.seed(123)
cor.npar.boot <- boot(gpa,cor.npar,R=999,sim="ordinary", stype="i")
plot(cor.npar.boot)
boot.ci(cor.npar.boot)
----------------------------------------------------------------
#Q2. 
vol <- read.csv(file = "VOLTAGE.csv", header=TRUE, sep=",")
vol_remote <- vol$voltage[vol$location == 0]
vol_local <- vol$voltage[vol$location == 1]
boxplot(vol_remote,vol_local)
qqnorm(vol_remote,main="QQ-norm for remote voltage")
qqline(vol_remote)
qqnorm(vol_local,main="QQ-norm for local voltage")
qqline(vol_local)
hist(vol_local)
hist(vol_remote)
#see if variances are equal or not
var(vol_local)
var(vol_remote)
# Unequal variances,hence apply formula for CI from 9 OCT
mean_vol_local <- mean(vol_local)
mean_vol_remote <- mean(vol_remote)
t.test(vol_remote, vol_local, alternative = "two.sided", conf.level = 0.95, 
               var.equal = FALSE)
#verify by manual calculation
ci <- (mean_vol_remote - mean_vol_local) +c(-1,1)*qt(0.025,58)*sqrt((var(vol_local) + var(vol_remote))/30)
--------------------------------------
  #Q3
vaporData <- read.csv(file="vapor.csv", header=TRUE, sep=",")
diff <- vaporData$theoretical - vaporData$experimental
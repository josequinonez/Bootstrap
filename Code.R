

bootstrap_fun  <- function (arg_vector) {
  NumLoops <- 1000
  bootstrap <- numeric(NumLoops)
  for (i in 1:NumLoops) {
    bootsample <- sample(arg_vector, size=length(arg_vector), replace=TRUE)
    bootstrap[i] <- mean(bootsample)
  }
  bootstrap
}




ProposedMean <- 100
ProposedSD <- 15


SampleSize1 <- 100
SampleSize2 <- 200
SampleSize3 <- 20
SampleSize4 <- 50

VectorX1 <- rnorm(SampleSize1, ProposedMean, ProposedSD)
VectorX2 <- rnorm(SampleSize2, ProposedMean, ProposedSD)

VectorX3 <- rexp(n=SampleSize3, rate=1)
VectorX4 <- rexp(n=SampleSize4, rate=1)

OriginalMean1 <- mean(VectorX1)
OriginalMean2 <- mean(VectorX2)
OriginalMean3 <- mean(VectorX3)
OriginalMean4 <- mean(VectorX4)

OriginalSD1 <- sd(VectorX1)
OriginalSD2 <- sd(VectorX2)
OriginalSD3 <- sd(VectorX3)
OriginalSD4 <- sd(VectorX4)

vbootstrap1 <- bootstrap_fun(VectorX1)
vbootstrap2 <- bootstrap_fun(VectorX2)
vbootstrap3 <- bootstrap_fun(VectorX3)
vbootstrap4 <- bootstrap_fun(VectorX4)



hist(vbootstrap1, main = "Histogram of Bootstrap 1 Normal", xlab="Bootstrap 1 Normal")
vbootstrap_mean1 <- mean(vbootstrap1)
vbootstrap_SD1 <- sd(vbootstrap1)

abline(v=ProposedMean, col="red", lwd=3, lty=1)
abline(v=OriginalMean1, col="brown", lwd=2, lty=2)
abline(v=vbootstrap_mean1, col="cyan", lwd=2, lty=3)

Final_SD1 <- OriginalSD1/sqrt(SampleSize1)
est_bias1 <- OriginalMean1-vbootstrap_mean1

c("Bootstrap 1 Normal")
c("Bottstrap sd: ", round(vbootstrap_SD1,4))
c("Final sd: ", round(Final_SD1,4))
c("Bias: ", round(est_bias1,4))



hist(vbootstrap2, main = "Histogram of Bootstrap 2 Normal", xlab="Bootstrap 2 Normal")
vbootstrap_mean2 <- mean(vbootstrap2)
vbootstrap_SD2 <- sd(vbootstrap2)

abline(v=ProposedMean, col="red", lwd=3, lty=1)
abline(v=OriginalMean2, col="brown", lwd=2, lty=2)
abline(v=vbootstrap_mean2, col="cyan", lwd=2, lty=3)

Final_SD2 <- OriginalSD2/sqrt(SampleSize2)
est_bias2 <- OriginalMean2-vbootstrap_mean2

c("Bootstrap 2 Normal")
c("Bottstrap sd: ", round(vbootstrap_SD2,4))
c("Final sd: ", round(Final_SD2,4))
c("Bias: ", round(est_bias2,4))



hist(vbootstrap3, main = "Histogram of Bootstrap 1 Exp", xlab="Bootstrap 1 Exp")
vbootstrap_mean3 <- mean(vbootstrap3)
vbootstrap_SD3 <- sd(vbootstrap3)

abline(v=ProposedMean, col="red", lwd=3, lty=1)
abline(v=OriginalMean3, col="brown", lwd=2, lty=2)
abline(v=vbootstrap_mean3, col="cyan", lwd=2, lty=3)

Final_SD3 <- OriginalSD3/sqrt(SampleSize3)
est_bias3 <- OriginalMean3-vbootstrap_mean3

c("Bootstrap 1 Exp")
c("Bottstrap sd: ", round(vbootstrap_SD3,4))
c("Final sd: ", round(Final_SD3,4))
c("Bias: ", round(est_bias3,4))



hist(vbootstrap4, main = "Histogram of Bootstrap 2 Exp", xlab="Bootstrap 2 Exp")
vbootstrap_mean4 <- mean(vbootstrap4)
vbootstrap_SD4 <- sd(vbootstrap4)

abline(v=ProposedMean, col="red", lwd=3, lty=1)
abline(v=OriginalMean4, col="brown", lwd=2, lty=2)
abline(v=vbootstrap_mean4, col="cyan", lwd=2, lty=3)

Final_SD4 <- OriginalSD3/sqrt(SampleSize4)
est_bias4 <- OriginalMean3-vbootstrap_mean4

c("Bootstrap 2 Exp")
c("Bottstrap sd: ", round(vbootstrap_SD1,4))
c("Final sd: ", round(Final_SD4,4))
c("Bias: ", round(est_bias4,4))

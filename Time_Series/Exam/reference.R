library(astsa)


# generate  dataset
set.seed(1)
num = 50
w = rnorm(num+1,0,1)
v = rnorm(num ,0,1)
mu = cumsum(w) # state: mu[0], mu[1],..., mu[50]
y = mu[-1] + v # obs: y[1],..., y[50]
# filter  and  smooth (Ksmooth0 does  both)
ks = Ksmooth0(num , y, A=1, mu0=0, Sigma0=1, Phi=1, cQ=1, cR=1)
# start  figurepar(mfrow=c(3,1))
Time = 1:num
plot(Time , mu[-1], main="Predict", ylim=c(-5,1))
lines(Time ,y,col="green")
lines(ks$xp)
lines(ks$xp+2*sqrt(ks$Pp), lty=2, col=4)
lines(ks$xp -2*sqrt(ks$Pp), lty=2, col=4)
plot(Time , mu[-1], main="Filter", ylim=c(-5,10))
lines(Time ,y,col="green")
lines(ks$xf)
lines(ks$xf+2*sqrt(ks$Pf), lty=2, col=4)
lines(ks$xf -2*sqrt(ks$Pf), lty=2, col=4)
plot(Time , mu[-1], main="Smooth", ylim=c(-5,10))
lines(Time ,y,col="green")
lines(ks$xs)
lines(ks$xs+2*sqrt(ks$Ps), lty=2, col=4)
lines(ks$xs-2*sqrt(ks$Ps), lty=2, col=4)
mu[1]
ks$x0n
sqrt(ks$P0n) # initial  value  info

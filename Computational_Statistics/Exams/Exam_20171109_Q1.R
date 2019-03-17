## 1.1

fsimul_norm<-function(){
    u1<-runif(1)*2*pi;u2<-runif(1)
    sqrt(-2*log(u2))*cos(u1)
}

fsimul_geom<-function(p=1/3){
    X<-0 ## or X<-1 depending on definition
    U<-0
    while (U==0){
	U<-sample(c(0,1),1,replace=FALSE,c(1-p,p))
	X<-X+1
    }
    X    
}


## 1.2

calc_Frechet_mean<-function(vX,dist="abs"){
    par0<-apply(vX,1,mean)
    res<-optim(par=par0,fn=function(par,vX,dist){
	fval<-NA	
	if (dist=="abs"){
	    fval<-sum(apply(vX,2,function(x,par){sum(abs(x-par))},par=par))
	}
	if (dist=="d2"){
	    fval<-sum(apply(vX,2,function(x,par){sum((x-par)^2)},par=par))
	}
	fval
    },vX=vX,dist=dist)
    res$par
}

Sample_10<-sapply(1:10,function(i){c(fsimul_norm(),fsimul_geom())},simplify=TRUE)
Sample_30<-sapply(1:30,function(i){c(fsimul_norm(),fsimul_geom())},simplify=TRUE)
Sample_50<-sapply(1:50,function(i){c(fsimul_norm(),fsimul_geom())},simplify=TRUE)


s10a<-calc_Frechet_mean(Sample_10,"abs")
s30a<-calc_Frechet_mean(Sample_30,"abs")
s50a<-calc_Frechet_mean(Sample_50,"abs")

s10d2<-calc_Frechet_mean(Sample_10,"d2")
s30d2<-calc_Frechet_mean(Sample_30,"d2")
s50d2<-calc_Frechet_mean(Sample_50,"d2")

## 1.3

pdf("s10a.pdf")
plot(Sample_10[1,],Sample_10[2,],pch=19,cex=1,main="",xlab="",ylab="",col="black")
points(s10a[1],s10a[2],pch=17,cex=3,col=gray(0.5))
points(mean(Sample_10[1,]),mean(Sample_10[2,]),pch=15,cex=3,col=gray(0.7))
points(median(Sample_10[1,]),median(Sample_10[2,]),pch=13,cex=3,col=gray(0.3))
dev.off()

pdf("s30a.pdf")
plot(Sample_30[1,],Sample_30[2,],pch=19,cex=1,main="",xlab="",ylab="",col="black")
points(s30a[1],s30a[2],pch=17,cex=3,col=gray(0.5))
points(mean(Sample_30[1,]),mean(Sample_30[2,]),pch=15,cex=3,col=gray(0.7))
points(median(Sample_30[1,]),median(Sample_30[2,]),pch=13,cex=3,col=gray(0.3))
dev.off()

pdf("s50a.pdf")
plot(Sample_50[1,],Sample_50[2,],pch=19,cex=1,main="",xlab="",ylab="",col="black")
points(s50a[1],s50a[2],pch=17,cex=3,col=gray(0.5))
points(mean(Sample_50[1,]),mean(Sample_50[2,]),pch=15,cex=3,col=gray(0.7))
points(median(Sample_50[1,]),median(Sample_50[2,]),pch=13,cex=3,col=gray(0.3))
dev.off()

pdf("s10d2.pdf")
plot(Sample_10[1,],Sample_10[2,],pch=19,cex=1,main="",xlab="",ylab="",col="black")
points(s10d2[1],s10d2[2],pch=17,cex=3,col=gray(0.5))
points(mean(Sample_10[1,]),mean(Sample_10[2,]),pch=15,cex=3,col=gray(0.7))
points(median(Sample_10[1,]),median(Sample_10[2,]),pch=13,cex=3,col=gray(0.3))
dev.off()

pdf("s30d2.pdf")
plot(Sample_30[1,],Sample_30[2,],pch=19,cex=1,main="",xlab="",ylab="",col="black")
points(s30d2[1],s30d2[2],pch=17,cex=3,col=gray(0.5))
points(mean(Sample_30[1,]),mean(Sample_30[2,]),pch=15,cex=3,col=gray(0.7))
points(median(Sample_30[1,]),median(Sample_30[2,]),pch=13,cex=3,col=gray(0.3))
dev.off()

pdf("s50d2.pdf")
plot(Sample_50[1,],Sample_50[2,],pch=19,cex=1,main="",xlab="",ylab="",col="black")
points(s50d2[1],s50d2[2],pch=17,cex=3,col=gray(0.5))
points(mean(Sample_50[1,]),mean(Sample_50[2,]),pch=15,cex=3,col=gray(0.7))
points(median(Sample_50[1,]),median(Sample_50[2,]),pch=13,cex=3,col=gray(0.3))
dev.off()


# install.packages("signal")
# library(signal)
# install.packages("seewave")
# library(seewave)
# library(ggplot2)
# library(stats)

a<-ftwindow(512,wn="rectangle")
b<-ftwindow(512,wn="bartlett")
c<-ftwindow(512,wn="hamming")
d<-ftwindow(512,wn="hanning")
e<-ftwindow(512,wn="blackman")
f<-ftwindow(512,wn="flattop")

all<-cbind(a,b,c,d,e,f)
# matplot(all,type="l",col=1:6,lty=1:6)
# legend(legend=c("rectangle","bartlett","hamming","hanning","blackman","flattop"),
#        x=380,y=0.95,col=1:6,lty=1:6,cex=0.75)

p1<- plot(a,xaxt='n',yaxt='n',xlab="Rectangle",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
p2<- plot(b,xaxt='n',yaxt='n',xlab="Bartlett",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
p3<- plot(c,xaxt='n',yaxt='n',xlab="Hamming",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
p4<- plot(d,xaxt='n',yaxt='n',xlab="Hanning",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
p5<- plot(e,xaxt='n',yaxt='n',xlab="Blackman",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
p6<- plot(f,xaxt='n',yaxt='n',xlab="Flattop",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")

spectrum(a)
spectrum(b)
spectrum(c)
spectrum(d)
spectrum(e)
spectrum(f)

spectrum(all)

tempall<-cbind(b,c,d,e,f)
spectrum(tempall)
spectrum(tempall,ylim=c(1e-14,1e+02))
legend(legend=c("rectangle","bartlett","hamming","hanning","blackman","flattop"),
       x=380,y=0.95,col=1:6,lty=1:6,cex=0.75)

# look at the function spec, it will definitly help if we can figure it out!





#### Window Function Bubble Plot ####
library(seewave)
a<-ftwindow(512,wn="rectangle")
b<-ftwindow(512,wn="bartlett")
c<-ftwindow(512,wn="hamming")
d<-ftwindow(512,wn="hanning")
e<-ftwindow(512,wn="blackman")
f<-ftwindow(512,wn="flattop")

layout(matrix(c(1, 2, 3, 7, 7, 7, 4, 5, 6, 7, 7, 7), nrow = 2, ncol = 6, byrow = TRUE))
plot(a,xaxt='n',yaxt='n',xlab="Rectangle",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
plot(b,xaxt='n',yaxt='n',xlab="Bartlett",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
plot(c,xaxt='n',yaxt='n',xlab="Hamming",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
plot(d,xaxt='n',yaxt='n',xlab="Hanning",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
plot(e,xaxt='n',yaxt='n',xlab="Blackman",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
plot(f,xaxt='n',yaxt='n',xlab="Flattop",ylab="",type="l",ylim=c(-0.25,1.25),col="blue")
all<-cbind(a,b,c,d,e,f)
spectrum <- spectrum(all, main=NA, xaxt='n')
axis(side = 1, at = c(0, 0.1, 0.2, 0.3, 0.4), tck = -.025)
axis(side = 1, at = c(0.4999), tck = -.025)
legend(0, 10^(-25), legend=c("Rectangle", "Bartlett", "Hamming", "Hanning",
                       "Blackman", "Flattop"),
       col=c("black", "red", "green", "blue", "cyan", "magenta"), lty=1:1, cex=0.8)

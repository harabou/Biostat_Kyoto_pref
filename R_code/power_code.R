#�Q�W�{���ϒl
#n�ݒ聨power
power.t.test(delta=3,sd=4, sig.level=0.05, n=20, type="two.sample")

#power�ݒ聨n
power.t.test(delta=3,sd=4, sig.level=0.05, power=0.8, type="two.sample")


#�Q�W�{���ϒl(Simulation�j
mypower <- function(n) {
count <- 0
for(i in 1:n) {
	MYDATA <- data.frame(
		GROUP 	=c( rep("A",20), rep("B",20)),
		QOL	=c( rnorm(20, mean=6, sd=4.0),
			    rnorm(20, mean=3, sd=4.0))
)
	result <- t.test(QOL~GROUP, var=T, data=MYDATA)
	if ((result$estimate[1]-result$estimate[2]>0)&&
	(result$p.value <0.05)) count <-count+1
}
return(count/n)
}
mypower(10000)



#�p�b�P�[�W���p
install.packages(pwr)
library(pwr)
pwr.p.test(h=ES.h(p1=0.75,p2=0.50),
           sig.level=0.05,
           power=0.8,
           alternative="two.side")

p.out <-pwr.p.test(h=ES.h(p1=0.75,p2=0.50),
                   sig.level=0.05,
                   power=0.8,
                   alternative="two.side")
plot(p.out)
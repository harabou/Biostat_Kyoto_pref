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
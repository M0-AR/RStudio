#E2.9
#a
pnorm(2)

curve(dnorm, xlim=c(-4,4))
xseq <- seq(-4, 2, len=1000)
polygon(x=c(xseq,2,xseq[1]),
        y=c(dnorm(xseq),0,dnorm(xseq[1])),
        col="pink")

#b
qnorm(pnorm(2))

#c
## Plot the standard normal distribution
curve(dnorm, xlim=c(-4,4))
## Add a vertical line at the 0.975 quantile
abline(v=qnorm(0.975))


#2.10
#b
#p(x<=20)
pnorm(20, mean =24, sd=4)
#p(x>29.5)
1 - pnorm(29.5, mean=24, sd=4)


#2.12
2*pnorm(2993, mean=3000, sd=3)

#2.13
#a
lambda30min <- 110000/(365*24*2)
dpois(x=0, lambda=lambda30min)
#The correct answer is 0.002

#b
lambda15min <- lambda30min/2
dpois(x=0, lambda=lambda15min)
# Answer is 0.043
rm(list=ls())
#
#inst=500
#feat=10
#data=matrix(NaN, nrow=inst, ncol=feat+1)
#for (i in seq(1,inst)){
#	data[i,]=sample(c(1,-1), feat+1, replace=T)
#}
#
#labs=data[,1]
#W=c(0,0,0,0,0,0,0,0,0,0)
#errors=0
#while(errors!=-1){
#	for (i in seq(1, inst)){
#		#the ith example is not separated
#		if( (t(W)%*%(data[i, 2:(feat)])) * labs[i] <= 0 ){
#		}
#	}
#	
#}

N=1000

line=matrix(0, nrow=(N/2), ncol=1)
ns=seq(2,N,2)
i=1
for (n in ns){
	for (j in seq((n/2), n)){
		line[i] = line[i]+dbinom(j, n, 1/2)
	}
	i=i+1
}

pdf('bFig.pdf')
plot(seq(2,N,2), line,
	'l',
	ylab = 'Pr(t=+1)',
	xlab = 'n',
	main = 'Probability of a Positive Label as a\n Function of the Number of Instances',
	ylim = c(0.45,0.8)
	)
lines(c(1,2),c(0.5,0.75),'l',
	col='black'
	)

lines(c(1,N),c(0.5,0.5),
	col='red'
	)

dev.off()

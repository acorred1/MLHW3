rm(list=ls())

a_w1000<-0
a_avg<-0
a_vote<-0

FOO=10
fluff=1000
S=100
Es=matrix(NaN, nrow=1,ncol=1)
Ncount=1
for(N in seq(2, fluff, FOO)){
	E=matrix(NaN, nrow=S, ncol=1)
	for(q in seq(1,S)){
		#Creating samples
		#N<-4
		feat<-11
		samples<-matrix(sample(c(1,-1),N*feat,replace=T),feat,N)

		#Part b
		#Generating Labels
		t<-matrix(0,1,N)
		for(i in 1:N){
  			if(sum(samples[,i]) > 0){ t[i]<-1 } else{ t[i]<- -1}
		}

		w<-matrix(0,1,feat)
		status<-matrix(1,1,N)  
		epochs<-0
		prediction_errors<-0

		while(sum(status) != 0){
  			epochs<-epochs+1
  			for(i in 1:N){
    				if (w%*%samples[,i]*t[i] <= 0){ #wrong
      					w = w + t[i]*samples[,i]
      					prediction_errors<-prediction_errors + 1
    				} else{ status[i]<-0  } #correct
			}
		}

		E[q]=epochs
		}

	Es[Ncount]=mean(E)
	Ncount=Ncount+1
}

pdf('epochsB.pdf')
plot(seq(2, fluff, FOO), Es,
	'l',
	ylab='Average Number of Epochs',
	xlab='Number of Instances'
	)
dev.off()

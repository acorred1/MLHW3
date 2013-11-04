#Hw3 Prob5: Perceptron with Noise
#Part b
this.dir<-dirname(parent.frame(2)$ofile)
setwd(this.dir)

#Creating samples
N<-500
feat<-11
samples<-matrix(sample(c(1,-1),N*feat,replace=T),feat,N)

#Initial weights
w<-matrix(0,1,feat)
status<-matrix(1,1,N)  #sentinel for correctness of each sample's predicted label. All 0 all correct
epochs<-0
prediction_errors<-0

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
      status[i]<-1
    } else{ status[i]<-0  } #correct
  }
}

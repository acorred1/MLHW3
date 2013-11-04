#Hw3 Prob5: Perceptron with Noise
#Part c 

this.dir<-dirname(parent.frame(2)$ofile)
setwd(this.dir)

#Accuracies
a_w1000<-0 
a_avg<-0
a_vote<-0


#Repeat testing 10 times to get avg accuracies
for(q in 1:10){
  #Creating samples
  N<-500
  feat<-11
  samples<-matrix(sample(c(1,-1),N*feat,replace=T),feat,N)

  #Generating lables
  t<-matrix(0,1,N)
  for(i in 1:N){ t[i]<-sign(sum(samples[,i]) + runif(1,-3,3)) }
  
  w<-matrix(0,1001,feat)
  prediction_errors<-0
  
  for(j in 1:2){
    for(i in 1:N){
      index<-i+N*(j-1)
      if (w[index,]%*%samples[,i]*t[i] <= 0){ #wrong
        w[(index +1),] = w[index,] + t[i]*samples[,i]
        prediction_errors<-prediction_errors + 1
      } else{ w[(index +1) ,] = w[index,] } #correct
    }
  }
  
  w<-w[2:1001,] #remove original guess (0,..,0)T
  w_avg<-apply(w,2,mean)
  
  
  #Generate test set
  test<-matrix(sample(c(1,-1),N*feat,replace=T),feat,N)
  ttest<-matrix(0,1,N)
  for(i in 1:N){ ttest[i]<-sign(sum(test[,i]) + runif(1,-3,3)) }
  
  #Counters for correct labeling
  r_w1000<-0
  r_avg<-0
  r_vote<-0
  
  
  for(i in 1:N){
    
    #Predict labels and compare to true label
    if( sign(w[1000,]%*%test[,i]) == ttest[i] ){
      r_w1000<-r_w1000 + 1
    }
    if( sign(w_avg%*%test[,i]) == ttest[i] ){
      r_avg<-r_avg + 1
    }
    
    vote<-0
    for(k in 1:1000){
      vote<-vote+sign(w[k,]%*%test[,i])
    }
    
    if( sign(vote)  == ttest[i] ){
      r_vote<-r_vote + 1
    }
    
  }
  
  
  a_w1000<-r_w1000/N + a_w1000
  a_avg<-r_avg/N + a_avg
  a_vote<-r_vote/N + a_vote
  
}

mean_a_w1000<-a_w1000/10
mean_a_avg<-a_avg/10
mean_a_vote<-a_vote/10
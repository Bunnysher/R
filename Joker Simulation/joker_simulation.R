###########
# A' Case #
###########

#In the first scenario, we will consider that people play randomly.

#We will repeat the following procedure 8 times, therefore we will have 104 lotteries.

start.time<-Sys.time()

winning<-matrix(,nrow=13,ncol=6)
winners<-matrix(,nrow=13,ncol=1)
jackpot<-matrix(,nrow=13,ncol=1)
for(k in 1:13){
  nums<-replicate(5000000,sample(1:45,5))
  nums<-t(nums)
  nums<-t(apply(nums,1,sort))
  joker<-replicate(5000000,sample(1:20,1))
  joker<-as.matrix(joker)
  columns<-cbind(nums,joker)
  
  won_nums<-replicate(1,sample(1:45,5))
  won_nums<-sort(won_nums)
  won_joker<-replicate(1,sample(1:20,1))
  won_column<-c(won_nums,won_joker)
  won_column<-t(won_column)
  winning[k,]<-won_column
  
  count<-0
  count2<-0
  for (i in 1:5000000){
    if (sum(as.numeric(won_column==columns[i,]))==6){
      count<-count+1
    }else{
      count2<-count2+1
    }
  }
  if (count2==5000000){
    jackpot[k]<-1
  }else{
    winners[k]<-1
  }
}
jackpot[is.na(jackpot)]<-0
winners[is.na(winners)]<-0

end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken


#Diagram
jack<-c(rep(1,22),0,1,0,rep(1,8),0,0,1,1,0,1,1,1,0,0,0,1,1,1,1,0,rep(1,5),
        0,rep(1,10),0,1,0,1,1,1,0,1,1,0,1,0,rep(1,6),0,1,1,1,0,1,0,0,1,1,0,1,0,1,1,0,1,1,0,1,1)

barplot(table(jack),main="Jackpot Distribution",xlab="Jackpot",col=c("red","royalblue"))
legend(0.2,80,legend=c("Winner","Jackpot"),fill=c("red","royalblue"))


#Count lengths of runs
leng<-rle(jack)
table(leng)


#Probability of getting jackpot in each lottery
prob<-sum(jack)/length(jack)



###########
# B' Case #
###########

#In the second scenario, we will consider that people don't play randomly.

#We will repeat the following procedure 8 times, therefore we will have 104 lotteries.

setwd("C:\\Users\\Stergina\\R\\Joker")

y2015<-read.table("Joker_2015.txt",sep = "",blank.lines.skip =FALSE)
colnames(y2015)<-c("Lottery","Date","1st","2nd","3rd","4th","5th","Joker",
                   "5+1 Success","5+1 Profit","5 Success","5 Profit","4+1 Success","4+1 Profit",
                   "4 Success","4 Profit","3+1 Success","3+1 Profit","3 Success","3 Profit",
                   "2+1 Success","2+1 Profit","1+1 Success","1+1 Profit")

number<-y2015[,3:7]
numbers<-number<=30
probs<-apply(numbers,1,sum)/ncol(number)
probs2<-sum(prob)/nrow(number)

#We observe that people choose numbers from 1-30 with probability 0.65.

start.time<-Sys.time()

winning<-matrix(,nrow=13,ncol=6)
winners<-matrix(,nrow=13,ncol=1)
jackpot<-matrix(,nrow=13,ncol=1)
for(k in 1:13){
  nums<-replicate(5000000,sample(c(1:30,31:45),5,prob=c(rep(0.65,30),rep(0.35,15))))
  nums<-t(nums)
  nums<-t(apply(nums,1,sort))
  joker<-replicate(5000000,sample(1:20,1))
  joker<-as.matrix(joker)
  columns<-cbind(nums,joker)
  
  won_nums<-replicate(1,sample(1:45,5))
  won_nums<-sort(won_nums)
  won_joker<-replicate(1,sample(1:20,1))
  won_column<-c(won_nums,won_joker)
  won_column<-t(won_column)
  winning[k,]<-won_column
  
  count<-0
  count2<-0
  for (i in 1:5000000){
    if (sum(as.numeric(won_column==columns[i,]))==6){
      count<-count+1
    }else{
      count2<-count2+1
    }
  }
  if (count2==5000000){
    jackpot[k]<-1
  }else{
    winners[k]<-1
  }
}
jackpot[is.na(jackpot)]<-0
winners[is.na(winners)]<-0

end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken


#Diagram
jack2<-c(rep(1,18),0,1,1,0,rep(1,7),0,0,rep(1,4),0,1,1,0,rep(1,4),0,rep(1,6),0,0,1,
         0,rep(1,6),0,1,0,1,1,1,0,rep(1,7),0,1,1,0,rep(1,6),0,rep(1,5),0,rep(1,4),0,rep(1,8))

barplot(table(jack2),main="Jackpot Distribution",xlab="Jackpot",col=c("deeppink2","cyan3"),ylim=c(0,100))
legend(0.2,80,legend=c("Winner","Jackpot"),fill=c("deeppink2","cyan3"))


#Count lengths of runs
leng2<-rle(jack2)
table(leng2)


#Probability of getting jackpot in each lottery
prob2<-sum(jack2)/length(jack2)



###########
# C' Case #
###########

#In the third scenario, we will consider that people play randomly and additionally that 
#the number of columns played is changing and it is between 3000000-8000000 columns.

start.time<-Sys.time()

winning<-matrix(,nrow=13,ncol=6)
winners<-matrix(,nrow=13,ncol=1)
jackpot<-matrix(,nrow=13,ncol=1)
played<-matrix(,nrow=13,ncol=1)
for(k in 1:13){
  played[k]<-sample(seq(3000000,8000000),1,replace=TRUE)
  nums<-replicate(played[k],sample(1:45,5))
  nums<-t(nums)
  nums<-t(apply(nums,1,sort))
  joker<-replicate(played[k],sample(1:20,1))
  joker<-as.matrix(joker)
  columns<-cbind(nums,joker)
  
  won_nums<-replicate(1,sample(1:45,5))
  won_nums<-sort(won_nums)
  won_joker<-replicate(1,sample(1:20,1))
  won_column<-c(won_nums,won_joker)
  won_column<-t(won_column)
  winning[k,]<-won_column
  
  count<-0
  count2<-0
  for (i in 1:played[k]){
    if (sum(as.numeric(won_column==columns[i,]))==6){
      count<-count+1
    }else{
      count2<-count2+1
    }
  }
  if (count2==played[k]){
    jackpot[k]<-1
  }else{
    winners[k]<-1
  }
}
jackpot[is.na(jackpot)]<-0
winners[is.na(winners)]<-0

end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken


#Diagram
jack3<-c(1,0,1,0,0,1,0,rep(1,4),0,0,1,0,rep(1,11),0,1,0,1,0,0,0,1,0,1,0,rep(1,4),0,0,1,0,
         rep(1,11),0,rep(1,9),0,1,1,1,0,1,0,1,0,rep(1,6),0,rep(1,14),0,1,0,1,1,0,0,0)

barplot(table(jack3),main="Jackpot Distribution",xlab="Jackpot",col=c("darkorange","blueviolet"),ylim=c(0,80))
legend(0.2,80,legend=c("Winner","Jackpot"),fill=c("darkorange","blueviolet"))


#Count lengths of runs
leng3<-rle(jack3)
table(leng3)


#Probability of getting jackpot in each lottery
prob3<-sum(jack3)/length(jack3)



###########
# D' Case #
###########

#In the forth scenario, we will consider that people don't play randomly and additionally 
#the number of columns played is changing based on the previous lottery's outcome.

start.time<-Sys.time()

winning<-matrix(,nrow=13,ncol=6)
winners<-matrix(,nrow=13,ncol=1)
jackpot<-matrix(,nrow=13,ncol=1)
played<-matrix(,nrow=13,ncol=1)
played[1]<-3000000
for(k in 1:13){
  nums<-replicate(played[k],sample(c(1:30,31:45),5,prob=c(rep(0.65,30),rep(0.35,15))))
  nums<-t(nums)
  nums<-t(apply(nums,1,sort))
  joker<-replicate(played[k],sample(1:20,1))
  joker<-as.matrix(joker)
  columns<-cbind(nums,joker)
  
  won_nums<-replicate(1,sample(1:45,5))
  won_nums<-sort(won_nums)
  won_joker<-replicate(1,sample(1:20,1))
  won_column<-c(won_nums,won_joker)
  won_column<-t(won_column)
  winning[k,]<-won_column
  
  count<-0
  count2<-0
  for (i in 1:played[k]){
    if (sum(as.numeric(won_column==columns[i,]))==6){
      count<-count+1
    }else{
      count2<-count2+1
    }
  }
  if (count2==played[k]){
    jackpot[k]<-1
    played[k+1]<-played[k]+1000000
  }else{
    winners[k]<-1
    played[k+1]<-played[k]
  }
}
jackpot[is.na(jackpot)]<-0
winners[is.na(winners)]<-0

end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken

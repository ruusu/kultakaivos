# kultakaivos
kultakaivos


main

#main

#Call DF

#Observe result

#Act

#Visualize

#Call DF(t+1)
result=double(length(Price)

for (t in 1000:length(Price)){

result[t]=DecisionFunction(Price,t,15,15)

}
Decision=double(1)

portfolio=matrix(0,1500,4)

for (i in 1:550){
  portfolio[i,1]=1
  portfolio[i,2]=as.double(Price[1,1])
  portfolio[i,3]=2
  portfolio[i,4]=as.double(Price[i,1])
}


BUY_PARAMETER=15
SELL_PARAMETER=15

Price.array=Price[,1]

for (t in 15:length(Price.array)-1){

Decision=DecisionFunction(Price.array,t,BUY_PARAMETER,SELL_PARAMETER)

#print(c(t,Decision))

if (sign(Decision)==1){  #Buy btc
  portfolio[t,1]=portfolio[t-1,1]+Decision*portfolio[t-1,2]/as.double(Price.array[t])
  portfolio[t,2]=portfolio[t-1,2]-Decision*portfolio[t-1,2]
  portfolio[t,3]=portfolio[t,1]+portfolio[t,2]/as.double(Price.array[t])
  portfolio[t,4]=as.double(Price.array[t])

}
if (sign(Decision)==-1){ #Sell btc
  portfolio[t,1]=portfolio[t-1,1]+Decision*portfolio[t-1,1]
  portfolio[t,2]=portfolio[t-1,2]-Decision*portfolio[t-1,1]*as.double(Price.array[t])
  portfolio[t,3]=portfolio[t,1]+portfolio[t,2]/as.double(Price.array[t])
  portfolio[t,4]=as.double(Price.array[t])
  
}
if (sign(Decision)==0){  #No action
  portfolio[t,1]=portfolio[t-1,1]
  portfolio[t,2]=portfolio[t-1,2]
  portfolio[t,3]=portfolio[t,1]+portfolio[t,2]/as.double(Price.array[t])
  portfolio[t,4]=as.double(Price.array[t])
  
}
}
plot(portfolio[,2])






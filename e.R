setwd("~/Desktop/knn")
#read.table('val.csv', header=T, sep=',')

val<- read.csv('val.csv', header=T, sep=',',stringsAsFactors = FALSE)
train <- read.csv('train.csv', header=T, sep=',',stringsAsFactors = FALSE)
test <- read.csv('test.csv', header=T, sep=',',stringsAsFactors = FALSE)

#val_class<-c()
#val_class<-val[11]
#test_class<-c()
#test_class<-val[11]
#train_class<-c()
#train_class<train[11]



val <- val[-1]
#train<-train[-1]
#test<-test[-1]



normalize <- function(x) 
  {
  return ((x - min(x)) / (max(x) - min(x))) 
  }

# val 1-10
#val<-as.data.frame(lapply(val[1:10], normalize))
#train<-as.data.frame(lapply(train[1:10], normalize))
#test<-as.data.frame(lapply(test[1:10], normalize))


##eucleain funticon to calculate the distance 
euclideanDist <- function(x, y){
  eu_distance = 0
  for(i in c(1:(length(x)-1) ))
  {
    eu_distance = eu_distance + (x[[i]]-y[[i]])^2
  }
  eu_distance = sqrt(eu_distance)
  return(eu_distance)
}



##
knn <- function(test, train, k_value){ #order does matter!!!!
  result <- c() #empty vector
  for(i in c(1:nrow(test))){   #for(i = 1; i<test_data.length; i++)
    eu_dist =c() 
    inverse_dist = c()
    eu_class = c()
    class2 = 0 #classcification
    class4 = 0 
    count = 0
    for(j in c(1:nrow(train))){ #for training set
      eu_dist <- c(eu_dist, euclideanDist(test[i,], train[j,])) #fill in eu_dist 
      #vector of euclideanDist of every test point with training point
      eu_class <- c(eu_class, as.integer(train[j,][[11]])) #clomn 11 is the class number 2,4
      }


    # print(inverse_dist)
    eu <- data.frame(eu_class, eu_dist)
    eu <- eu[order(eu$eu_dist),]  #sort the eu data frame
    eu <- eu[1:k,]               #eu dataframe with top K neighbors
    for(k in c(1:nrow(eu))){
      if(as.integer(eu[k,"eu_class"]) == 2){
        #  class2 = inverse_function(eu_dist,class2)
        class2 = class2+1/nrow(eu) #weighted knn
      }
      else
        #  class4 = inverse_function(eu_dist,class4)
        class4 = class4+1/nrow(eu)
    }
    if(class2 > class4){        
      result <- c(result, 2)
    }
    else if(class2 < class4){
      result <- c(result, 4)
    }
  }
  return(result)
}



#K=13
#count = 0
#predictions <- knn(test, train, K) 
#test[,9]<-predictions
#for(i in c(1:nrow(val_class))){
#if(predictions == val_class[i,1])
#{
 #  count = count+1
#}
#  accu = count/nrow(val_class)
#  print(accu)
#}



k=11
predictions <- knn(test, train, k) 
test[,7]<-predictions
count = 0
for(j in c(1:nrow(test)))
{
if(test[j,7] == test[j,11])
{
  count = count+1
}
acc = count/nrow(test)
print(acc)
}

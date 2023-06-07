#Build  lhs sample

#Design the experiment
 library(lhs)
 set.seed(5000)
 sample.size<-200
 number.of.Xs<-10
 lhs.sample<-data.frame(randomLHS(sample.size,number.of.Xs))

#define ranges of each x
x1.min<-0.5
x1.max<-1.5

x2.min<-0.3
x2.max<-1.1

x3.min<-0.1
x3.max<-2.0

x4.min<-0.5
x4.max<-1.6

x5.min<-0.5
x5.max<-1.5

x6.min<-0.3
x6.max<-1.3

x7.min<-0.8
x7.max<-1.8

x8.min<-0.8
x8.max<-1.2

x9.min<-0.5
x9.max<-1.5

x10.min<-0.5
x10.max<-1.5


#Define futures table
lhs.sample[,1]<-qunif(lhs.sample[,1],x1.min,x1.max)
lhs.sample[,2]<-qunif(lhs.sample[,2],x2.min,x2.max)
lhs.sample[,3]<-qunif(lhs.sample[,3],x3.min,x3.max)
lhs.sample[,4]<-qunif(lhs.sample[,4],x4.min,x4.max)
lhs.sample[,5]<-qunif(lhs.sample[,5],x5.min,x5.max)
lhs.sample[,6]<-qunif(lhs.sample[,6],x6.min,x6.max)
lhs.sample[,7]<-qunif(lhs.sample[,7],x7.min,x7.max)
lhs.sample[,8]<-qunif(lhs.sample[,8],x8.min,x8.max)
lhs.sample[,9]<-qunif(lhs.sample[,9],x9.min,x9.max)
lhs.sample[,10]<-qunif(lhs.sample[,10],x10.min,x10.max)

#write
write.csv(lhs.sample,"lhs_sample.csv",row.names=FALSE)

#to find out where is gets written?
getwd()

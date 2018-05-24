#install.packages('mosaic')
#install.packages('sciplot')
#install.packages("colorspace")

library(mosaic)
library(colorspace)
library(sciplot)

#d0<-read.csv(file="GuillaumeOperantData.csv")
d0<-read.csv(file.choose(),header=TRUE)
d<-d0[,c(1:8)]

#Things you need to set everytime you run script!
#Ids of flies being analyzed, day1time, day2time, fps, ids of flies that were forced trained on day 1: lines 17,18,19,25,199 respectively

#Set times for analysis in seconds and fps
day1time=120
day2time=900
fps=5
pools=5




#Picking out which flies you want via subject id
#Can do equal, smaller, bigger etc.
#Right now only analyze flies that were in same experiment
#Example: day 1 time has to match day 2 time and fps of course.
d<-filter(d,id<67&id>59)
ids<-unique(d$id)
ids


#Declaring empty categories for storage
etoh90Ids<-character()
controlIds<-character()
etoh80Ids<-character()
etoh70Ids<-character()


#Getting rid of any flies that don't have 2 sessions
for(i in ids){
  di<-filter(d,id==i)
  if(length(unique(di$session))!=2){
    ids<-ids[!ids==i]
    d<-filter(d,id!=i)
  }
}

ids<-unique(d$id)

#These are the flies that you're analyzing
ids

#Creating vectors of different conditions from ids to sub categories
#, right now just EtOH (90%) and control
for(i in ids){
  dtemp<-filter(d,id==i)
  print(c(unique(dtemp$EtOH),unique(dtemp$id)))
  if(unique(dtemp$EtOH==70)){
    etoh70Ids<-c(etoh70Ids,unique(dtemp$id))
  } else if(unique(dtemp$EtOH==80)){
    etoh80Ids<-c(etoh80Ids,unique(dtemp$id))
  } else if(unique(dtemp$EtOH==90)){
    etoh90Ids<-c(etoh90Ids,unique(dtemp$id))
  } else if(unique(dtemp$EtOH==0)){
    controlIds<-c(controlIds,unique(dtemp$id))
  }
}


###The following makes graphs for each condition. Right now coded to 90% and control but just replace etoh90ids with whatever 
##vector of ids you want to make a graph
#This loop makes a line for every individual fly in the 90% EtoH condition; number of seconds spent in rewarding region
plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Time spent in rewarding region (s)",main=paste("IAA+90%EtoH; ",day1time/60,"minutes on day one;",day2time/60,"minutes on day two"),xlab="Day")
axis(side=1,at=c(1,2))
for(i in etoh90Ids){
  di<-filter(d,id==i)
  dsub<-di %>% 
    group_by(session) %>% 
    summarise(countAlcohol=round(length(Fly.in.rewd.region.[Fly.in.rewd.region.==TRUE])/fps))
  lines(dsub$countAlcohol,type="o",xaxt="n",col=sample(rainbow_hcl(100)))
}

#This code block makes a new plot for every fly in the etoh90 condition, uncomment it out if you want to run it

#for(i in etoh90Ids){
#  di<-filter(d,id==i)
#  plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Time spent in rewarding region (s)",main=paste("IAA+90%EtoH; Fly number: ",i,"; ",day1time/60,"minutes on day one;",day2time/60,"minutes on day two"),xlab="Day")
#    dsub<-di %>% 
#    group_by(session) %>% 
#    summarise(countAlcohol=round(length(Fly.in.rewd.region.[Fly.in.rewd.region.==TRUE])/fps))
#  lines(dsub$countAlcohol,type="o",xaxt="n",col="black")
#}


#This loop makes a graph for every individual fly in the control condition; number of seconds spent in rewarding region
plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Time spent in rewarding region (s)",main=paste("IAA only; ",day1time/60,"minutes on day one;",day2time/60,"minutes on day two"),xlab="Day")
axis(side=1,at=c(1,2))
for(i in controlIds){
  di<-filter(d,id==i)
  dsub<-di %>% 
    group_by(session) %>% 
    summarise(countAlcohol=round(length(Fly.in.rewd.region.[Fly.in.rewd.region.==TRUE])/fps))
  lines(dsub$countAlcohol,type="o",xaxt="n",col=sample(rainbow_hcl(100)))
}

#This code block makes a new plot for every fly in the control condition, uncomment it out if you want to run it

#for(i in controlIds){
#  di<-filter(d,id==i)
#  plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Time spent in rewarding region (s)",main=paste("IAA only; Fly number: ",i,"; ",day1time/60,"minutes on day one;",day2time/60,"minutes on day two"),xlab="Day")
#    dsub<-di %>% 
#    group_by(session) %>% 
#    summarise(countAlcohol=round(length(Fly.in.rewd.region.[Fly.in.rewd.region.==TRUE])/fps))
#  lines(dsub$countAlcohol,type="o",xaxt="n",col="black")
#}

#Seperating into 4 data frames: one for day1Etoh, one for day2etoh, one for day1control, one for day2control
d1Etoh90<-filter(d,session==1,EtOH==90)
d2Etoh90<-filter(d,session==2,EtOH==90)
d1control<-filter(d,session==1,EtOH==0)
d2control<-filter(d,session==2,EtOH==0)

#Setting up for summary graphs for number of seconds in rewarding region
d1subEtoh90<-d1Etoh90 %>% 
  group_by(id) %>% 
  summarise(countAlcohol=round(length(Fly.in.rewd.region.[Fly.in.rewd.region.==TRUE])/fps))

d2subEtoh90<-d2Etoh90 %>% 
  group_by(id) %>% 
  summarise(countAlcohol=round(length(Fly.in.rewd.region.[Fly.in.rewd.region.==TRUE])/fps))

d1subcontrol<-d1control %>% 
  group_by(id) %>% 
  summarise(countAlcohol=round(length(Fly.in.rewd.region.[Fly.in.rewd.region.==TRUE])/fps))

d2subcontrol<-d2control %>% 
  group_by(id) %>% 
  summarise(countAlcohol=round(length(Fly.in.rewd.region.[Fly.in.rewd.region.==TRUE])/fps))

sd1<-sd(d1subEtoh90$countAlcohol)
sd2<-sd(d2subEtoh90$countAlcohol)
sd3<-sd(d1subcontrol$countAlcohol)
sd4<-sd(d2subcontrol$countAlcohol)


#Summary graph for 90% + EtOH
plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Time spent in rewarding region (s)",main=paste("Summary; Time spent in rewarding region per fly: IAA+90%EtoH; n=",length(etoh90Ids)),xlab="Day")
axis(side=1,at=c(1,2))
points(1,mean(d1subEtoh90$countAlcohol),pch=19)
arrows(1, mean(d1subEtoh90$countAlcohol)+sd1, 1, mean(d1subEtoh90$countAlcohol)-sd1, length=0.05, angle=90, code=3)
points(2,mean(d2subEtoh90$countAlcohol),pch=19)
arrows(2, mean(d2subEtoh90$countAlcohol)+sd2, 2, mean(d2subEtoh90$countAlcohol)-sd2, length=0.05, angle=90, code=3)
segments(1,mean(d1subEtoh90$countAlcohol),2,mean(d2subEtoh90$countAlcohol))
#Summary graph for control
plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Time spent in rewarding region (s)",main=paste("Summary: Time spent in rewarding region per fly: IAA; n=",length(controlIds)),xlab="Day")
axis(side=1,at=c(1,2))
points(1,mean(d1subcontrol$countAlcohol),pch=19)
arrows(1, mean(d1subcontrol$countAlcohol)+sd3, 1, mean(d1subcontrol$countAlcohol)-sd3, length=0.05, angle=90, code=3)
points(2,mean(d2subcontrol$countAlcohol),pch=19)
arrows(2, mean(d2subcontrol$countAlcohol)+sd4, 2, mean(d2subcontrol$countAlcohol)-sd4, length=0.05, angle=90, code=3)
segments(1,mean(d1subcontrol$countAlcohol),2,mean(d2subcontrol$countAlcohol))

#Note: if there are errors of indeterminate lengths than that's fine. Just means that sd equals 0 which makes sense 
#for forced trainings





###############Number of entries and mean time in rewarding region per entry

#Whenever you're making the next graphs, always run this following block otherwise it adds on and messes up
etoh90Entries1=NULL
etoh90Entries2=NULL
etoh90MeanTime1=NULL
etoh90MeanTime2=NULL

#Making each fly its own line for IAA plus 90% EtoH flies # of entries in rewarding region
plot(NULL,type="p",xaxt="n",ylim=c(0,20),xlim=c(1,2),xlab="Day",ylab = "Number of entries in rewarding region",main=paste("IAA+90%EtOH;","Number of entries n= ",length(etoh90Ids)))
axis(side=1,at=c(1,2))
for(i in etoh90Ids){
  di<-filter(d,id==i)
  for(h in 1:2){
    dsubi<-filter(di,session==h)
    if(unique(dsubi$session==1)){
      Tcounts1<-rle(dsubi$Fly.in.rewd.region.)$lengths[rle(dsubi$Fly.in.rewd.region.)$values]
    } else if (unique(dsubi$session==2)){
      Tcounts2<-rle(dsubi$Fly.in.rewd.region.)$lengths[rle(dsubi$Fly.in.rewd.region.)$values]
    }
  }
  #SUPER IMPORTANT
  #Coercing 1 entry WHEN flies were forced trained. MAKE SURE ids are accurate in subsequent line
  #If you're not analyzing any flies that were forced trained, comment out subsequent line
  if(unique(di$id)>=50){Tcounts1<-day1time}
  TcountsSum<-c(length(Tcounts1),length(Tcounts2))
  McountsMean<-c(mean(Tcounts1),mean(Tcounts2))
  print(i)
  #Always have one of the two lines commented out. Comment out lines if you want every fly to have its own plot.
  lines(TcountsSum,type="o",xaxt="n",ylim=c(0,20),xlim=c(1,2),ylab = "Number of entries",col=sample(rainbow_hcl(100)))
  #plot(TcountsSum,type="o",xaxt="n",col="black",ylim=c(0,20),xlim=c(1,2),ylab = "Number of entries",main=paste("IAA+90%EtOH; Fly number:",i,"; "))
  
  #Storing data to be able to create summarized graphs
  etoh90Entries1<-c(etoh90Entries1,length(Tcounts1))
  etoh90Entries2<-c(etoh90Entries2,length(Tcounts2))
  etoh90MeanTime1<-c(etoh90MeanTime1,mean(Tcounts1))
  etoh90MeanTime2<-c(etoh90MeanTime2,mean(Tcounts2))
}



#Making each fly its own line for IAA plus 90% EtoH flies Mean number of time spent per entry
plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Mean Time spent in rewarding region (s)",main=paste("IAA+90% Etoh; Time spent in rewarding region per entry; n= ",length(etoh90Ids)),xlab="Day")
for(i in etoh90Ids){
  di<-filter(d,id==i)
  for(h in 1:2){
    dsubi<-filter(di,session==h)
    if(unique(dsubi$session==1)){
      Tcounts1<-rle(dsubi$Fly.in.rewd.region.)$lengths[rle(dsubi$Fly.in.rewd.region.)$values]
    } else if (unique(dsubi$session==2)){
      Tcounts2<-rle(dsubi$Fly.in.rewd.region.)$lengths[rle(dsubi$Fly.in.rewd.region.)$values]
    }
  }
  #SUPER IMPORTANT
  #Coercing 1 entry WHEN flies were forced trained. MAKE SURE ids are accurate in subsequent line
  #If you're not analyzing any flies that were forced trained, comment out subsequent line
  if(unique(di$id)>=50){Tcounts1<-day1time}
  McountsMean<-c(mean(Tcounts1),mean(Tcounts2))
  
  #Always have one of the two lines commented out. Comment out lines if you want every fly to have its own plot.
  lines(McountsMean,type="o",xaxt="n",ylim=c(0,900),xlim=c(1,2),col=sample(rainbow_hcl(100)))
  #plot(McountsMean,type="o",xaxt="n",col="black",ylim=c(0,900),xlim=c(1,2),ylab = "Average time per entry",main=paste("IAA only; Fly number:",i,"; "))
  print(i)
}

#Whenever you're making the next graphs, always run this following block
cEntries1=NULL
cEntries2=NULL
cMeanTime1=NULL
cMeanTime2=NULL

#Making each fly its own line for IAA control flies # of entries in rewarding region
plot(NULL,type="p",xaxt="n",ylim=c(0,50),xlim=c(1,2),xlab="Day",ylab = "Number of entries in rewarding region",main=paste("IAA only;","Number of entries n= ",length(controlIds)))
axis(side=1,at=c(1,2))
for(i in controlIds){
  di<-filter(d,id==i)
  for(h in 1:2){
    dsubi<-filter(di,session==h)
    if(unique(dsubi$session==1)){
      Tcounts1<-rle(dsubi$Fly.in.rewd.region.)$lengths[rle(dsubi$Fly.in.rewd.region.)$values]
    } else if (unique(dsubi$session==2)){
      Tcounts2<-rle(dsubi$Fly.in.rewd.region.)$lengths[rle(dsubi$Fly.in.rewd.region.)$values]
    }
  }
  #SUPER IMPORTANT
  #Coercing 1 entry WHEN flies were forced trained. MAKE SURE ids are accurate in subsequent line
  #If you're not analyzing any flies that were forced trained, comment out subsequent line
  if(unique(di$id)>=50){Tcounts1<-day1time}
  TcountsSum<-c(length(Tcounts1),length(Tcounts2))
  McountsMean<-c(mean(Tcounts1),mean(Tcounts2))
  print(i)
  #Always have one of the two lines commented out. Comment out lines if you want every fly to have its own plot.
  lines(TcountsSum,type="o",xaxt="n",ylab = "Number of entries",col=sample(rainbow_hcl(100)))
  #plot(TcountsSum,type="o",xaxt="n",col="black",ylim=c(0,50),xlim=c(1,2),ylab = "Number of entries",main=paste("IAA; Fly number:",i,"; "))
  
  #Storing data to be able to create summarized graphs
  cEntries1<-c(cEntries1,length(Tcounts1))
  cEntries2<-c(cEntries2,length(Tcounts2))
  cMeanTime1<-c(cMeanTime1,mean(Tcounts1))
  cMeanTime2<-c(cMeanTime2,mean(Tcounts2))
}


#Making each fly its own line for IAA only flies Mean number of time spent per entry
plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Mean Time spent in rewarding region (s)",main=paste("IAA only; Time spent in rewarding region per entry; n= ",length(controlIds)),xlab="Day")
for(i in controlIds){
  di<-filter(d,id==i)
  for(h in 1:2){
    dsubi<-filter(di,session==h)
    if(unique(dsubi$session==1)){
      Tcounts1<-rle(dsubi$Fly.in.rewd.region.)$lengths[rle(dsubi$Fly.in.rewd.region.)$values]
    } else if (unique(dsubi$session==2)){
      Tcounts2<-rle(dsubi$Fly.in.rewd.region.)$lengths[rle(dsubi$Fly.in.rewd.region.)$values]
    }
  }
  #SUPER IMPORTANT
  #Coercing 1 entry WHEN flies were forced trained. MAKE SURE ids are accurate in subsequent line
  #If you're not analyzing any flies that were forced trained, comment out subsequent line
  if(unique(di$id)>=50){Tcounts1<-day1time}
  McountsMean<-c(mean(Tcounts1),mean(Tcounts2))
  
  #Always have one of the two lines commented out. Comment out lines if you want every fly to have its own plot.
  lines(McountsMean,type="o",xaxt="n",ylim=c(0,900),xlim=c(1,2),col=sample(rainbow_hcl(100)))
  #plot(McountsMean,type="o",xaxt="n",col="black",ylim=c(0,900),xlim=c(1,2),ylab = "Average time per entry",main=paste("IAA only; Fly number:",i,"; "))
  print(i)
}


#Storing data to be able to create summarized graphs
cEntries1<-c(etoh90Entries1,length(Tcounts1))
cEntries2<-c(etoh90Entries2,length(Tcounts2))
cMeanTime1<-c(etoh90MeanTime1,mean(Tcounts1))
cMeanTime2<-c(etoh90MeanTime2,mean(Tcounts2))


#Making summary graphs for number of seconds per entry and number of entries for 90%EtoH
sd1e<-sd(etoh90Entries1)
sd2e<-sd(etoh90Entries2)
sd3e<-sd(etoh90MeanTime1)
sd4e<-sd(etoh90MeanTime2)  


#Summary graph for 90% + EtOH for mean number of entries
plot(NULL,type="p",xaxt="n",ylim=c(0,20),xlim=c(1,2),ylab = "Number of entries",main=paste("Summary: number of entries in rewarding region: IAA+90%Etoh; n=",length(etoh90Ids)),xlab="Day")
axis(side=1,at=c(1,2))
points(1,mean(etoh90Entries1),pch=19)
arrows(1, mean(etoh90Entries1)+sd1e, 1, mean(etoh90Entries1)-sd1e, length=0.05, angle=90, code=3)
points(2,mean(etoh90Entries2),pch=19)
arrows(2, mean(etoh90Entries2)+sd2e, 2, mean(etoh90Entries2)-sd2e, length=0.05, angle=90, code=3)
segments(1,mean(etoh90Entries1),2,mean(etoh90Entries2))
#Summary graph for 90% + EtOH for mean time in rewarding region
plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Time spent in rewarding region (s)",main=paste("Summary: Mean time spent per entry: IAA+90%Etoh; n=",length(etoh90Ids)),xlab="Day")
axis(side=1,at=c(1,2))
points(1,mean(etoh90MeanTime1),pch=19)
arrows(1, mean(etoh90MeanTime1)+sd3e, 1, mean(etoh90MeanTime1)-sd3e, length=0.05, angle=90, code=3)
points(2,mean(etoh90MeanTime2),pch=19)
arrows(2, mean(etoh90MeanTime2)+sd4e, 2, mean(etoh90MeanTime2)-sd4e, length=0.05, angle=90, code=3)
segments(1,mean(etoh90MeanTime1),2,mean(etoh90MeanTime2))



#Making summary graphs for number of seconds per entry and number of entries for control flies
sd1c<-sd(cEntries1)
sd2c<-sd(cEntries2)
sd3c<-sd(cMeanTime1)
sd4c<-sd(cMeanTime2)  

#Summary graph for control for mean number of entries
plot(NULL,type="p",xaxt="n",ylim=c(0,50),xlim=c(1,2),ylab = "Number of entries",main=paste("Summary: number of entries: IAA; n=",length(controlIds)),xlab="Day")
axis(side=1,at=c(1,2))
points(1,mean(cEntries1),pch=19)
arrows(1, mean(cEntries1)+sd1e, 1, mean(cEntries1)-sd1e, length=0.05, angle=90, code=3)
points(2,mean(cEntries2),pch=19)
arrows(2, mean(cEntries2)+sd2e, 2, mean(cEntries2)-sd2e, length=0.05, angle=90, code=3)
segments(1,mean(cEntries1),2,mean(cEntries2))
#Summary graph for control for mean time in rewarding region
plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Time spent in rewarding region (s)",main=paste("Summary: Mean time per entry: IAA; n=",length(controlIds)),xlab="Day")
axis(side=1,at=c(1,2))
points(1,mean(cMeanTime1),pch=19)
arrows(1, mean(cMeanTime1)+sd3e, 1, mean(cMeanTime1)-sd3e, length=0.05, angle=90, code=3)
points(2,mean(cMeanTime2),pch=19)
arrows(2, mean(cMeanTime2)+sd4e, 2, mean(cMeanTime2)-sd4e, length=0.05, angle=90, code=3)
segments(1,mean(cMeanTime1),2,mean(cMeanTime2))

#Summary barplot
ntime<-c(mean(cMeanTime2),mean(etoh90MeanTime2))
barplot(ntime,xlab="IAA                                                      IAA+90%EtOH",
        ylim=c(0,300),ylab="Number of seconds in rewarding region",main="Number of seconds in rewarding region")





#Making barplot, pooling by time day 2
plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Time spent in rewarding region (s)",main=paste("IAA+90%EtoH; ",day1time/60,"minutes on day one;",day2time/60,"minutes on day two"),xlab="Day")
axis(side=1,at=c(1,2))
for(i in etoh90Ids){
  for(k in 1:pools){
    di<-filter(d,id==i,session==2)
    tempr<-floor(nrow(di)/pools)
    di<-di[1+(tempr*(k-1)):(k*tempr),]
    dsub1<-di %>% 
      group_by(session) %>% 
      summarise(countAlcohol=round(length(Fly.in.rewd.region.[Fly.in.rewd.region.==TRUE])/fps))
    if(k==1){
      pool1<-c(pool1,dsub1$countAlcohol[1])
    } else if(k==2){
      pool2<-c(pool2,dsub1$countAlcohol[1])
    }else if(k==2){
      pool2<-c(pool2,dsub1$countAlcohol[1])
    } else if(k==3){
      pool3<-c(pool3,dsub1$countAlcohol[1])
    }else if(k==4){
      pool4<-c(pool4,dsub1$countAlcohol[1])
    }else if(k==5){
      pool5<-c(pool5,dsub1$countAlcohol[1])
    }else if(k==6){
      pool6<-c(pool6,dsub1$countAlcohol[1])
    }else if(k==7){
      pool7<-c(pool7,dsub1$countAlcohol[1])
    }else if(k==8){
      pool8<-c(pool8,dsub1$countAlcohol[1])
    }else if(k==9){
      pool9<-c(pool9,dsub1$countAlcohol[1])
    }else if(k==10){
      pool10<-c(pool10,dsub1$countAlcohol[1])
    }else if(k==11){
      pool11<-c(pool11,dsub1$countAlcohol[1])
    }else if(k==12){
      pool12<-c(pool12,dsub1$countAlcohol[1])
    }else if(k==13){
      pool13<-c(pool13,dsub1$countAlcohol[1])
    }else if(k==14){
      pool14<-c(pool14,dsub1$countAlcohol[1])
    }else if(k==15){
      pool15<-c(pool15,dsub1$countAlcohol[1])
    }
 }
}

##Making actual graph
#lines(dsub$countAlcohol,type="o",xaxt="n",col=sample(rainbow_hcl(100)))
#Have to hard code how many pools you want....Annyong but saves time in long run
Means<-c(mean(pool1),mean(pool2),mean(pool3),mean(pool4),mean(pool5),mean(pool6),mean(pool7),mean(pool8),mean(pool9),mean(pool10),mean(pool11),mean(pool12),mean(pool13),mean(pool14),mean(pool15))
sds<-c(sd(pool1),sd(pool2),sd(pool3),sd(pool4),sd(pool5),sd(pool6),sd(pool7),sd(pool8),sd(pool9),sd(pool10),sd(pool11),sd(pool12),sd(pool13),sd(pool14),sd(pool15))
g1<-barplot(height=Means,ylim=c(0,50),beside=T,xlab=paste("Time. Each bar is",day2time/pools,"seconds"))
arrows(g1, Means - sds * 2, g1,
       Means + sds * 2, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

pool1=NULL
pool2=NULL
pool3=NULL
pool4=NULL
pool5=NULL
pool6=NULL
pool7=NULL
pool8=NULL
pool9=NULL
pool10=NULL
pool11=NULL
pool12=NULL
pool13=NULL
pool14=NULL
pool15=NULL

##MEAN TIME SPENT PER ENTRY IN REWARDING REGION AND NUMBER OF ENTRIES
#Making barplot, pooling by time day 2 
#Reminder: Tcounts2 is a vector of the lengths of all the entries in rewarding region
#Right now it's collapsing across all flies to increase n.
plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Mean Time spent in rewarding region (s)",xlab="Day",main="Time spent in rewarding region on day 2")
axis(side=1,at=c(1,2))
for(i in etoh90Ids){
  for(k in 1:pools){
    di<-filter(d,id==i,session==2)
    tempr<-floor(nrow(di)/pools)
    di<-di[1+(tempr*(k-1)):(k*tempr),]
    Tcounts2<-rle(di$Fly.in.rewd.region.)$lengths[rle(di$Fly.in.rewd.region.)$values]
   # if(Tcounts2==0){k==15}

    if(k==1){
      pool1<-c(pool1,Tcounts2)
    } else if(k==2){
      pool2<-c(pool2,Tcounts2)
    } else if(k==3){
      pool3<-c(pool3,Tcounts2)
    }else if(k==4){
      pool4<-c(pool4,Tcounts2)
    }else if(k==5){
      pool5<-c(pool5,Tcounts2)
    }else if(k==6){
      pool6<-c(pool6,Tcounts2)
    }else if(k==7){
      pool7<-c(pool7,Tcounts2)
    }else if(k==8){
      pool8<-c(pool8,Tcounts2)
    }else if(k==9){
      pool9<-c(pool9,Tcounts2)
    }else if(k==10){
      pool10<-c(pool10,Tcounts2)
    }else if(k==11){
      pool11<-c(pool11,Tcounts2)
    }else if(k==12){
      pool12<-c(pool12,Tcounts2)
    }else if(k==13){
      pool13<-c(pool13,Tcounts2)
    }else if(k==14){
      pool14<-c(pool14,Tcounts2)
    }else if(k==15){
      pool15<-c(pool15,Tcounts2)
    }
  print(Tcounts2)
  print(class(Tcounts2))}
}
##Making actual graph for mean time spent in rewarding region
#Have to hard code how many pools you want....Annyong but saves time in long run
Means<-c(mean(pool1),mean(pool2),mean(pool3),mean(pool4),mean(pool5),mean(pool6),mean(pool7),mean(pool8),mean(pool9),mean(pool10),mean(pool11),mean(pool12),mean(pool13),mean(pool14),mean(pool15))
sds<-c(sd(pool1),sd(pool2),sd(pool3),sd(pool4),sd(pool5),sd(pool6),sd(pool7),sd(pool8),sd(pool9),sd(pool10),sd(pool11),sd(pool12),sd(pool13),sd(pool14),sd(pool15))
g1<-barplot(height=Means,ylim=c(0,50),beside=T,xlab=paste("Time. Each bar is",day2time/pools,"seconds into experiment"),main=paste("IAA+90%EtoH; Mean time spent per rewarding region"))
arrows(g1, Means - sds * 2, g1,
       Means + sds * 2, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

#Using the same pools to make NUMBER OF ENTRIES
#plot(NULL,type="p",xaxt="n",ylim=c(0,350),xlim=c(1,2),ylab = "Number of entries",xlab="Day",main="Time spent in rewarding region on day 2")
Lengths<-c(length(pool1),length(pool2),length(pool3),length(pool4),length(pool5),length(pool6),length(pool7),length(pool8),length(pool9),length(pool10),length(pool11),length(pool12),length(pool13),length(pool14),length(pool15))
g1<-barplot(height=Lengths,ylim=c(0,20),beside=T,xlab=paste("Time. Each bar is",day2time/pools,"seconds into experiment"),main=paste("IAA+90%EtoH; Total number of entries in rewarding region"))


######################################################################










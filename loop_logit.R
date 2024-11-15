cross<-read.csv('msa.csv')
write.csv(EN.Central,'region/EN.Central.csv')
write.csv(ES.Central,'region/ES.Central.csv')
write.csv(Middle.Atlantic,'region/Middle.Atlantic.csv')
write.csv(Mountain,'region/Mountain.csv')
write.csv(New.England,'region/New.England.csv')
write.csv(Pacific,'region/Pacific.csv')
write.csv(S.Atlantic,'region/S.Atlantic.csv')
write.csv(WN.Central,'region/WN.Central.csv')
write.csv(WS.Central,'region/WS.Central.csv')
gc()

df = read.csv('region/ES.Central.csv')
df$id<-rownames(df)
df<-merge(df,cross, by = 'MET2013',all.x=T)
df<-subset(df,!MET=='Not.in.identifiable.area')
Train <- df %>%
  group_by(occupation,bpl,INDNAICS,decade,MET) %>%
  sample_frac(0.8) 
Test <- anti_join(df, Train, by = 'id')

gc()
state<-ddply(df,.(MET),summarize,pop=sum(PERWT))
rownames(state)<-state$MET
Test$bpl <- Test$bpl[,drop=TRUE]
Test$occupation <- Test$occupation[,drop=TRUE]
gc()
#state$MET
for (i in rownames(state)) {
  print(i)
  Train[,i]<-ifelse(Train$MET==i,1,0)
  Test[,i]<-ifelse(Test$MET==i,1,0)
  model<-glm(as.formula(paste0(i,'~',' major + occupation +  bpl*decade+ log(yr.in.us)+
              race + hispanic + log(AGE) + edu + log(INCWAGE) + bpl*occ.broad + factor(INDNAICS)')),
             data = Train, family = 'binomial'(link = 'probit'), weights = PERWT,na.action = na.exclude )
  gc()
  Test[,paste('prob',i,sep = '.')] <- predict(model, Test, type = "response")
  Test[,paste('in',i,sep = '.')]<-ifelse(Test[,paste('prob',i,sep = '.')]>=0.6,1,0)
  Test[,paste('accurate',i,sep = '.')]<-ifelse(Test[,paste('in',i,sep = '.')]==Test[,i],1,0) 
  state[i,'accuracy']<-sum(Test[,paste('accurate',i,sep = '.')]*Test$PERWT)/sum(Test$PERWT)
}
gc()
region<-rbind(region,state)
write.csv(region,'msa/region_pred2.csv')


df = read.csv('region/Middle.Atlantic.csv')
df$id<-rownames(df)
df<-merge(df,cross, by = 'MET2013',all.x=T)
df<-subset(df,!MET=='Not.in.identifiable.area')
Train <- df %>%
  group_by(occupation,bpl,INDNAICS,decade,MET) %>%
  sample_frac(0.8) 
Test <- anti_join(df, Train, by = 'id')
gc()
state<-ddply(df,.(MET),summarize,pop=sum(PERWT))
rownames(state)<-state$MET
Test$bpl <- Test$bpl[,drop=TRUE]
Test$occupation <- Test$occupation[,drop=TRUE]
gc()
#state$MET
for (i in rownames(state)) {
  print(i)
  Train[,i]<-ifelse(Train$MET==i,1,0)
  Test[,i]<-ifelse(Test$MET==i,1,0)
  model<-glm(as.formula(paste0(i,'~',' major + occupation +  bpl*decade+ log(yr.in.us)+
              race + hispanic + log(AGE) + edu + log(INCWAGE) + bpl*occ.broad + factor(INDNAICS)')),
             data = Train, family = 'binomial'(link = 'probit'), weights = PERWT,na.action = na.exclude )
  gc()
  Test[,paste('prob',i,sep = '.')] <- predict(model, Test, type = "response")
  Test[,paste('in',i,sep = '.')]<-ifelse(Test[,paste('prob',i,sep = '.')]>=0.6,1,0)
  Test[,paste('accurate',i,sep = '.')]<-ifelse(Test[,paste('in',i,sep = '.')]==Test[,i],1,0) 
  state[i,'accuracy']<-sum(Test[,paste('accurate',i,sep = '.')]*Test$PERWT)/sum(Test$PERWT)
}
gc()
region<-rbind(region,state)
write.csv(region,'msa/region_pred2.csv')




df = read.csv('region/Mountain.csv')
df$id<-rownames(df)
df<-merge(df,cross, by = 'MET2013',all.x=T)
df<-subset(df,!MET=='Not.in.identifiable.area')
Train <- df %>%
  group_by(occupation,bpl,INDNAICS,decade,MET) %>%
  sample_frac(0.8) 
Test <- anti_join(df, Train, by = 'id')
gc()
state<-ddply(df,.(MET),summarize,pop=sum(PERWT))
rownames(state)<-state$MET
Test$bpl <- Test$bpl[,drop=TRUE]
Test$occupation <- Test$occupation[,drop=TRUE]
gc()
#state$MET
for (i in rownames(state)) {
  print(i)
  Train[,i]<-ifelse(Train$MET==i,1,0)
  Test[,i]<-ifelse(Test$MET==i,1,0)
  model<-glm(as.formula(paste0(i,'~',' major + occupation +  bpl*decade+ log(yr.in.us)+
              race + hispanic + log(AGE) + edu + log(INCWAGE) + bpl*occ.broad + factor(INDNAICS)')),
             data = Train, family = 'binomial'(link = 'probit'), weights = PERWT,na.action = na.exclude )
  gc()
  Test[,paste('prob',i,sep = '.')] <- predict(model, Test, type = "response")
  Test[,paste('in',i,sep = '.')]<-ifelse(Test[,paste('prob',i,sep = '.')]>=0.6,1,0)
  Test[,paste('accurate',i,sep = '.')]<-ifelse(Test[,paste('in',i,sep = '.')]==Test[,i],1,0) 
  state[i,'accuracy']<-sum(Test[,paste('accurate',i,sep = '.')]*Test$PERWT)/sum(Test$PERWT)
}
gc()
region<-rbind(region,state)
write.csv(region,'msa/region_pred2.csv')




df = read.csv('region/New.England.csv')
df$id<-rownames(df)
df<-merge(df,cross, by = 'MET2013',all.x=T)
df<-subset(df,!MET=='Not.in.identifiable.area')
Train <- df %>%
  group_by(occupation,bpl,INDNAICS,decade,MET) %>%
  sample_frac(0.8) 
Test <- anti_join(df, Train, by = 'id')
gc()
state<-ddply(df,.(MET),summarize,pop=sum(PERWT))
rownames(state)<-state$MET
Test$bpl <- Test$bpl[,drop=TRUE]
Test$occupation <- Test$occupation[,drop=TRUE]
gc()
#state$MET
for (i in rownames(state)) {
  print(i)
  Train[,i]<-ifelse(Train$MET==i,1,0)
  Test[,i]<-ifelse(Test$MET==i,1,0)
  model<-glm(as.formula(paste0(i,'~',' major + occupation +  bpl*decade+ log(yr.in.us)+
              race + hispanic + log(AGE) + edu + log(INCWAGE) + bpl*occ.broad + factor(INDNAICS)')),
             data = Train, family = 'binomial'(link = 'probit'), weights = PERWT,na.action = na.exclude )
  gc()
  Test[,paste('prob',i,sep = '.')] <- predict(model, Test, type = "response")
  Test[,paste('in',i,sep = '.')]<-ifelse(Test[,paste('prob',i,sep = '.')]>=0.6,1,0)
  Test[,paste('accurate',i,sep = '.')]<-ifelse(Test[,paste('in',i,sep = '.')]==Test[,i],1,0) 
  state[i,'accuracy']<-sum(Test[,paste('accurate',i,sep = '.')]*Test$PERWT)/sum(Test$PERWT)
}
gc()
region<-rbind(region,state)
write.csv(region,'msa/region_pred2.csv')







df = read.csv('region/S.Atlantic.csv')
df$id<-rownames(df)
df<-merge(df,cross, by = 'MET2013',all.x=T)
df<-subset(df,!MET=='Not.in.identifiable.area')
Train <- df %>%
  group_by(occupation,bpl,INDNAICS,decade,MET) %>%
  sample_frac(0.8) 
Test <- anti_join(df, Train, by = 'id')
gc()
state<-ddply(df,.(MET),summarize,pop=sum(PERWT))
rownames(state)<-state$MET
Test$bpl <- Test$bpl[,drop=TRUE]
Test$occupation <- Test$occupation[,drop=TRUE]
gc()
#state$MET
for (i in rownames(state)) {
  print(i)
  Train[,i]<-ifelse(Train$MET==i,1,0)
  Test[,i]<-ifelse(Test$MET==i,1,0)
  model<-glm(as.formula(paste0(i,'~',' major + occupation +  bpl*decade+ log(yr.in.us)+
              race + hispanic + log(AGE) + edu + log(INCWAGE) + bpl*occ.broad + factor(INDNAICS)')),
             data = Train, family = 'binomial'(link = 'probit'), weights = PERWT,na.action = na.exclude )
  gc()
  Test[,paste('prob',i,sep = '.')] <- predict(model, Test, type = "response")
  Test[,paste('in',i,sep = '.')]<-ifelse(Test[,paste('prob',i,sep = '.')]>=0.6,1,0)
  Test[,paste('accurate',i,sep = '.')]<-ifelse(Test[,paste('in',i,sep = '.')]==Test[,i],1,0) 
  state[i,'accuracy']<-sum(Test[,paste('accurate',i,sep = '.')]*Test$PERWT)/sum(Test$PERWT)
}
gc()
region<-rbind(region,state)
write.csv(region,'msa/region_pred2.csv')

df = read.csv('region/Pacific.csv')
df$id<-rownames(df)
df<-merge(df,cross, by = 'MET2013',all.x=T)
df<-subset(df,!MET=='Not.in.identifiable.area')
Train <- df %>%
  group_by(occupation,bpl,INDNAICS,decade,MET) %>%
  sample_frac(0.8) 
Test <- anti_join(df, Train, by = 'id')
gc()
state<-ddply(df,.(MET),summarize,pop=sum(PERWT))
rownames(state)<-state$MET
Test$bpl <- Test$bpl[,drop=TRUE]
Test$occupation <- Test$occupation[,drop=TRUE]
gc()

#state$MET
for (i in rownames(state[19:29,])) {
  print(i)
  Train[,i]<-ifelse(Train$MET==i,1,0)
  Test[,i]<-ifelse(Test$MET==i,1,0)
  model<-glm(as.formula(paste0(i,'~',' major + occupation +  bpl*decade+ log(yr.in.us)+
              race + hispanic + log(AGE) + edu + log(INCWAGE) + bpl*occ.broad + factor(INDNAICS)')),
             data = Train, family = 'binomial'(link = 'probit'), na.action = na.exclude )
  gc()
  Test[,paste('prob',i,sep = '.')] <- predict(model, Test, type = "response")
  Test[,paste('in',i,sep = '.')]<-ifelse(Test[,paste('prob',i,sep = '.')]>=0.6,1,0)
  Test[,paste('accurate',i,sep = '.')]<-ifelse(Test[,paste('in',i,sep = '.')]==Test[,i],1,0) 
  state[i,'accuracy']<-sum(Test[,paste('accurate',i,sep = '.')]*Test$PERWT)/sum(Test$PERWT)
  write.csv(state,'msa/pacific_msa.csv')
  }
gc()
region<-rbind(region,state)
write.csv(region,'msa/region_pred2.csv')



df = read.csv('region/WN.Central.csv')
df$id<-rownames(df)
df<-merge(df,cross, by = 'MET2013',all.x=T)
df<-subset(df,!MET=='Not.in.identifiable.area')
Train <- df %>%
  group_by(occupation,bpl,INDNAICS,decade,MET) %>%
  sample_frac(0.8) 
Test <- anti_join(df, Train, by = 'id')
gc()
state<-ddply(df,.(MET),summarize,pop=sum(PERWT))
rownames(state)<-state$MET
Test$bpl <- Test$bpl[,drop=TRUE]
Test$occupation <- Test$occupation[,drop=TRUE]
gc()
#state$MET
for (i in rownames(state)) {
  print(i)
  Train[,i]<-ifelse(Train$MET==i,1,0)
  Test[,i]<-ifelse(Test$MET==i,1,0)
  model<-glm(as.formula(paste0(i,'~',' major + occupation +  bpl*decade+ log(yr.in.us)+
              race + hispanic + log(AGE) + edu + log(INCWAGE) + bpl*occ.broad + factor(INDNAICS)')),
             data = Train, family = 'binomial'(link = 'probit'), weights = PERWT,na.action = na.exclude )
  gc()
  Test[,paste('prob',i,sep = '.')] <- predict(model, Test, type = "response")
  Test[,paste('in',i,sep = '.')]<-ifelse(Test[,paste('prob',i,sep = '.')]>=0.6,1,0)
  Test[,paste('accurate',i,sep = '.')]<-ifelse(Test[,paste('in',i,sep = '.')]==Test[,i],1,0) 
  state[i,'accuracy']<-sum(Test[,paste('accurate',i,sep = '.')]*Test$PERWT)/sum(Test$PERWT)
}
gc()
region<-rbind(region,state)
write.csv(region,'msa/region_pred2.csv')




df = read.csv('region/WS.Central.csv')
df$id<-rownames(df)
df<-merge(df,cross, by = 'MET2013',all.x=T)
df<-subset(df,!MET=='Not.in.identifiable.area')
Train <- df %>%
  group_by(occupation,bpl,INDNAICS,decade,MET) %>%
  sample_frac(0.8) 
Test <- anti_join(df, Train, by = 'id')
gc()
state<-ddply(df,.(MET),summarize,pop=sum(PERWT))
rownames(state)<-state$MET
Test$bpl <- Test$bpl[,drop=TRUE]
Test$occupation <- Test$occupation[,drop=TRUE]
gc()
#state$MET
for (i in rownames(state)) {
  print(i)
  Train[,i]<-ifelse(Train$MET==i,1,0)
  Test[,i]<-ifelse(Test$MET==i,1,0)
  model<-glm(as.formula(paste0(i,'~',' major + occupation +  bpl*decade+ log(yr.in.us)+
              race + hispanic + log(AGE) + edu + log(INCWAGE) + bpl*occ.broad + factor(INDNAICS)')),
             data = Train, family = 'binomial'(link = 'probit'), weights = PERWT,na.action = na.exclude )
  gc()
  Test[,paste('prob',i,sep = '.')] <- predict(model, Test, type = "response")
  Test[,paste('in',i,sep = '.')]<-ifelse(Test[,paste('prob',i,sep = '.')]>=0.6,1,0)
  Test[,paste('accurate',i,sep = '.')]<-ifelse(Test[,paste('in',i,sep = '.')]==Test[,i],1,0) 
  state[i,'accuracy']<-sum(Test[,paste('accurate',i,sep = '.')]*Test$PERWT)/sum(Test$PERWT)
}
gc()
region<-rbind(region,state)
write.csv(region,'msa/region_pred2.csv')
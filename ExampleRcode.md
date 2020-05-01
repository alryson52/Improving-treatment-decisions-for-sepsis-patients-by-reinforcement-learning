# Improving-treatment-decisions-for-sepsis-patients-by-reinforcement-learning
The repository include the example R code for the thesis, 'Improving treatment decisions for sepsis patients by reinforcement learning'.

library(writexl)
library(mice)
library(ReinforcementLearning)
library(Rfast)
library(data.table)
library(gplots)
library(ggplot2)
library(ggpubr)
library(tidyr)

setwd()#set up the working directory
data<-read.csv()#read the dataset file #18014 
#delete data hospital dischargestatus 144
data<-data[data$hospitaldischargestatus!="",]#17870
data<-data[complete.cases(data$day),]#17825
data<-data.table(data)

data[, Cat1NE := (Norepinephrine | Epinephrine) & (!Phenylephrine & !Vasopressin & !Other)]
data[, Cat3PVO := (!Norepinephrine & !Epinephrine) & (Phenylephrine | Vasopressin | Other)] # this group is small
data[, Cat4None := !Norepinephrine & !Epinephrine & !Phenylephrine.1 & !Vasopressin & !Other]
data[, Cat2NEPVO := !(Cat1NE|Cat3PVO|Cat4None)] # these groups is small
sum(data$Cat1NE) + sum(data$Cat2NEPVO) + sum(data$Cat3PVO) + sum(data$Cat4None)
nrow(data)
data<-as.data.frame(data)

#Step 1: Summarize treatment combination and frequency, data check and creating table 1 for all covariates. 
#convert factor into number
for (i in 6:30) {
  data[,i]<-as.numeric(as.character(data[,i]))
}

#Conversion
#temperature
data$temperature<-ifelse(89<data$temperature,(data$temperature-32)/1.8,data$temperature)
matrix<-matrix(,nrow = 25,ncol=7)
for (i in 6:30) {
  for (j in 1:7) {
    matrix[i-5,j]=summary(data[,i])[j]
  }
}

#divide the characters into categorical and continuous
#continuous variables: the other
matrix<-matrix(,nrow = 25,ncol=7)
for (i in 6:30) {
  for (j in 1:7) {
    matrix[i-5,j]=summary(data[,i])[j]
  }
}
#change the colname and the rowname of the matrix
matrix<-as.data.frame(matrix)
rownames(matrix)[1:25]=colnames(data)[6:30]
colnames(matrix)[1:7]=rownames(as.data.frame(as.matrix(summary(data[,6]))))
for (i in 6:30) {
  matrix$SD[i-5]<-sd(data[,i],na.rm = TRUE)
}
matrix$miss<-matrix$`NA's`/17825
write.csv(matrix,file="summary_descriptives statistics.csv")

#graph the histogram for each variable
pdf("Normality of variables.pdf",width = 6,height = 3) 
for (i in 6:30) {
  par(mfrow=c(1,2))
  hist((data[,i]),
       main=colnames(data[i]),
       xlab = "")
  box(bty="o")
  qqnorm(data[,i], pch = 1, frame = FALSE,main=paste0("Normal Q-Q Plot for ",colnames(data)[i]))
  qqline(data[,i], col = "steelblue", lwd = 2)
}
dev.off() 

#categorical variable: gender, angVP, VP.dopamine, VP.epinephrine, VP.phenylephrine, VP.vasopressin
data<-data[,-c(31:36)]
for (i in c(5,31:34)) {
  data[,i]<-as.factor(data[,i])
}
data$treatment<-ifelse(data$Cat4None=="TRUE","None",ifelse(data$Cat3PVO=="TRUE","PVO",
                                                       ifelse(data$Cat1NE=="TRUE","NE",
                                                              ifelse(data$Cat2NEPVO=="TRUE","NEPVO","FALSE"))))
data$treatment<-as.factor(data$treatment)
# NE NEPVO  None   PVO 
# 1113  4377  9150  3185 

#frequency table for categorical data
list2<-list()
for (i in 31:35) {
  list2[[i-29]]<-as.data.frame(t(summary(data[,i])))
}
list2[[1]]<-as.data.frame(t(summary(data[,5])))

for (i in 7:12) {
  list2[[i]]<-as.data.frame(prop.table(list2[[i-6]]))
}
names(list2)<-c("gender",colnames(data[,31:34]),"Treatment","gender_p","NE_p","PVO_p","None_p","NEPVO_p","Treatment_p")
write_xlsx(list2,path = "frequency of descriptive statistics (categorical).xlsx",col_names = TRUE)

#transformation
data2<-data
data2[,7]<-log(data2[,7]+1)
data2[,8]<-log(data2[,8])
data2[,9]<-log(data2[,9])
data2[,10]<-log(data2[,10])
data2[,12]<-log(data2[,12])
data2[,14]<-log(data2[,14])
data2[,17]<-log(data2[,17])
data2[,18]<-log(data2[,18])
data2[,19]<-log(data2[,19])
data2[,20]<-log(102-data2[,20])
data2[,21]<-log(data2[,21])
data2[,22]<-data2[,22]^(1/2)
data2[,23]<-log(data2[,23])
data2[,28]<-log(data2[,28])
colnames(data2)

#standardization
data3<-data2[,-c(15,16,27)]
for (i in 6:27) {
  data3[,i]<-(data2[,i]-mean(data2[,i],na.rm = TRUE))/sd(data2[,i],na.rm = TRUE)
}

#Imputation
data3$gender<- as.factor(data3$gender)
tobelogged <- c("age","albumin", "alt..sgpt.", "ast..sgot.", "bands","bicarbonate", "bun", "chloride","creatinine", "glucose", "hgb", "lactate", "o2.sat....", "paco2", "platelets.x.1000", "pt...inr", "respiratory.rate","sodium","temperature","wbc.x.1000","heartratemax","systemicsystolicmax")
set.seed(20191210)
data_mice <- mice(data3[, c(5:27)], m = 5) # include gender, m = 5 means 5 multiple imputation
save(data_mice, file = "imputedData.RData")
load("imputedData.RData")
data4 <- data3
data4[,c(5:27)] <- mice::complete(data_mice, 1) # complete(, 1), 1 means using the first imputed data set. We imputed 5 copies, so it can be any number between 1 and 5.

#treatment for each day
trtd<-as.data.frame(matrix(NA,ncol=4))
for (i in 1:7) {
  cluster<-data4[data4$day==i,]
  trtd<-rbind(trtd,table(cluster$treatment))
}
colnames(trtd)<-c('NE','NEPVO','None','PVO')
write.csv(trtd,file="trtd.csv")

#in-hospital mortality
unique1<-unique(data4$patientunitstayid)
outcome<-as.data.frame(matrix(NA,nrow=3726,ncol=2))
outcome$V1<-unique1
for (i in 1:length(unique1)) {
  outcome$V2[i]<-ifelse(data[data$patientunitstayid==unique1[i],]$hospitaldischargestatus[1]=='Alive',1,0)
}
sum(outcome$V2)/3726

#continuous variables after imputation
matrix<-matrix(,nrow = 22,ncol=7)
for (i in 6:27) {
  for (j in 1:6) {
    matrix[i-5,j]=summary(data4[,i])[j]
  }
}
#change the colname and the rowname of the matrix
matrix<-as.data.frame(matrix)
rownames(matrix)[1:22]=colnames(data4)[6:27]
colnames(matrix)[1:6]=rownames(as.data.frame(as.matrix(summary(data4[,6]))))
for (i in 6:27) {
  matrix$SD[i-5]<-sd(data4[,i],na.rm = TRUE)
}
write.csv(matrix,file="summary_descriptives statistics after imputations.csv")


#cluster
K = 100
data4[, c(6:27)] # the data columns used for clustering
clusterdata<-data4
clusterdata$gender<-ifelse(clusterdata$gender=="Female",0,1)
clustout <- kmeans(clusterdata[, c(5:27)], centers = K, nstart = 1000, iter.max = 5000) # impose mixture
clust <- clustout$centers

# size of clusters
clustout$size
# [1] 217 204 234 150 107 201 194 276 264 229 171 181 271 185  76 257 112 252 149 194 169 188 173  97
# [25] 225 112 181 206 205 179 185 217 205 175 146 170 165 206  44 111 233 153 189 145 122 179 146 225
# [49] 207 178 175 231 189 200  65 212 184 127 249 148 172 163 149 318 183 175 183 224 182 230 143  54
# [73] 212 172 219 180 188 186 242  85 230 237 224 186  86 112 179 139 218 202 153 205 189 176 136 197
# [97] 125  76 171  84

#reward and survival ranks
clusterdata$cluster<-clustout$cluster
clusterdata$outcome<-ifelse(clusterdata$hospitaldischargestatus=="Alive",1,0)

reward<-as.data.frame(matrix(NA,nrow=100,ncol=1))
reward$V1<-c(1:100)
colnames(reward)[1]<-"cluster"
for (i in 1:100) {
  cluster<-clusterdata[clusterdata$cluster==i,]
  cluster<-cluster[!duplicated(cluster$patientunitstayid),]
  numerator<-sum(cluster$outcome)
  denomenator<-nrow(cluster)
  reward$probability[i]<-1-numerator/denomenator #hospital mortality
  reward$size[i]<-denomenator
}
reward$rank<-rank(1-reward$probability)
reward<-reward[order(reward$rank),]
reward$rank1<-1:100
write.csv(reward,file="reward.csv")

#Table 1 for cluster with ranks
#label the cluster on original table
data5<-data
data5$cluster<-clustout$cluster
matching<-match(data5$cluster,reward$cluster)
data5$rank<-reward$rank1[matching]

#clusterdata and reward
matching1<-match(clusterdata$cluster,reward$cluster)
clusterdata$rank<-reward$rank1[matching1]
clusterdata$reward<-reward$probability[matching1]
clusterdata1<-clusterdata

data$rank<-clusterdata1$rank
data$cluster<-clusterdata1$cluster
#mean of the medical vitals
vitalsmean<-as.data.frame(matrix(NA,nrow=100,ncol=25))
for (i in 1:100) {
  subset<-data[data$cluster==i,]
  for (j in 1:25) {
    vitalsmean[i,j]<-mean(subset[,j+5],na.rm=TRUE)
  }
}
colnames(vitalsmean)<-colnames(data)[c(6:30)]
vitalsmean$state=rownames(vitalsmean)
vitalsmean<-format(vitalsmean,digits=3)
matching1<-match(vitalsmean$state,reward$cluster)
vitalsmean$rank<-reward$rank1[matching1]
vitalsmean<-vitalsmean[,-c(10,11,22)]
colnames(vitalsmean)<-c('Age','Bands','Albumin','ALT','AST','Bicarbonate','BUN','Chloride','Creatinine','Glucose','Hgb','Lactate','O2 sat','PaCO2','Platelets','INR','RR','Sodium','Temperature','WBC','Heart rate','Systolic BP','State','Rank')
write.csv(vitalsmean,file="means of medical vitals.csv")

vitalsmean[,c(1:22)]<-lapply(vitalsmean[,c(1:22)],as.character)
vitalsmean[,c(1:22)]<-lapply(vitalsmean[,c(1:22)],as.numeric)
#heatmap of medical vitals mean
df<-vitalsmean[,c(1:22)]
matching3<-match(rownames(df),reward$cluster)
df$rank<-reward$rank1[matching3]
rownames(df)<-df$rank
df<-df[order(df$rank),]
df<-as.matrix(df[,c(1:22)])
heatmap.2(df, scale='column',Rowv=FALSE,Colv=TRUE,dendrogram="col",trace="none",srtCol=52, adjCol = c(0.8,0),col=bluered,  tracecol="#303030")

lattice<-vitalsmean[,c(23,24,1:11)]
lattice2<-vitalsmean[,c(23,24,12:22)]

pdf("Means of Different Variables 2.pdf")
age<-ggplot(lattice, aes(x=Rank, y=Age)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
bands<-ggplot(lattice, aes(x=Rank, y=Bands)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
albumin<-ggplot(lattice, aes(x=Rank, y=Albumin)) +
  geom_point() + geom_smooth(method='loess')+
  theme(axis.title.x = element_blank())
alt<-ggplot(lattice, aes(x=Rank, y=ALT)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
ast<-ggplot(lattice, aes(x=Rank, y=AST)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
bicarbonate<-ggplot(lattice, aes(x=Rank, y=Bicarbonate)) +
  geom_point() + geom_smooth(method='loess')+
  theme(axis.title.x = element_blank())
bun<-ggplot(lattice, aes(x=Rank, y=BUN)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
creatinine<-ggplot(lattice, aes(x=Rank, y=Creatinine)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
chloride<-ggplot(lattice, aes(x=Rank, y=Chloride)) +
  geom_point() + geom_smooth(method='loess')
glucose<-ggplot(lattice, aes(x=Rank, y=Glucose)) +
  geom_point() + geom_smooth(method='loess') 
hgb<-ggplot(lattice, aes(x=Rank, y=Hgb)) +
  geom_point() + geom_smooth(method='loess') 
#ggarrange(age,bands,albumin,alt,ast,bicarbonate,bun,creatinine,chloride,glucose,hgb + rremove("x.text"), 
#          ncol = 4, nrow = 3)

lactate<-ggplot(lattice2, aes(x=Rank, y=Lactate)) +
  geom_point() + geom_smooth(method='loess')+
  theme(axis.title.x = element_blank())
o2<-ggplot(lattice2, aes(x=Rank, y=`O2 sat`)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
paco2<-ggplot(lattice2, aes(x=Rank, y=PaCO2)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
platelets<-ggplot(lattice2, aes(x=Rank, y=Platelets)) +
  geom_point() + geom_smooth(method='loess')+
  theme(axis.title.x = element_blank())
inr<-ggplot(lattice2, aes(x=Rank, y=INR)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
rr<-ggplot(lattice2, aes(x=Rank, y=RR)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
sodium<-ggplot(lattice2, aes(x=Rank, y=Sodium)) +
  geom_point() + geom_smooth(method='loess')+
  theme(axis.title.x = element_blank())
temperature<-ggplot(lattice2, aes(x=Rank, y=Temperature)) +
  geom_point() + geom_smooth(method='loess') +
  theme(axis.title.x = element_blank())
wbc<-ggplot(lattice2, aes(x=Rank, y=WBC)) +
  geom_point() + geom_smooth(method='loess') 
heartrate<-ggplot(lattice2, aes(x=Rank, y=`Heart rate`)) +
  geom_point() + geom_smooth(method='loess') 
ss<-ggplot(lattice2, aes(x=Rank, y=`Systolic BP`)) +
  geom_point() + geom_smooth(method='loess') 
 
ggarrange(age,bands,albumin,alt,ast,bicarbonate,bun,creatinine,chloride,glucose,hgb,lactate,o2,paco2,platelets,inr,rr,sodium,temperature,wbc,heartrate,ss + rremove("x.text"), 
          ncol = 5, nrow = 5)
dev.off()

#treatment for each state
trt<-as.data.frame(matrix(NA))
for (i in 1:100) {
  cluster<-clusterdata[clusterdata$cluster==i,]
  trt[c(1:4),i]<-as.data.frame((as.matrix(table(cluster$treatment))))$V1
  trt[5,i]<-nrow(cluster)
  trt[c(6:9),i]<-trt[c(1:4),i]/nrow(cluster)
}
rownames(trt)<-c(rownames(as.data.frame((as.matrix(table(cluster$treatment))))),"Size",paste0(rownames(as.data.frame((as.matrix(table(cluster$treatment))))),' Probability'))
trt<-round(trt,digits=2)
colnames(trt)<-c(1:100)
write.csv(trt,file = "Treatment of each cluster.csv")
trt<-as.data.frame(t(trt))
trt$state=rownames(trt)

matching1<-match(clusterdata$cluster,reward$cluster)
clusterdata$rank1<-reward$rank1[matching1]
clusterdata1$rank1<-reward$rank1[matching1]

#treatment for each rank
trt2<-as.data.frame(matrix(NA))
for (i in 1:100) {
  cluster<-clusterdata[clusterdata$rank1==i,]
  trt2[c(1:4),i]<-as.data.frame((as.matrix(table(cluster$treatment))))$V1
  trt2[5,i]<-nrow(cluster)
  trt2[c(6:9),i]<-trt2[c(1:4),i]/nrow(cluster)
  
}
rownames(trt2)<-c(rownames(as.data.frame((as.matrix(table(cluster$treatment))))),"Size",paste0(rownames(as.data.frame((as.matrix(table(cluster$treatment))))),'Probability'))
trt2<-round(trt2,digits=2)
colnames(trt2)<-reward$rank1
for (i in 1:9) {
  trt2$total[i]<-sum(trt2[i,1:100])
}
write.csv(trt2,file = "Treatment of each rank.csv")

trt2<-as.data.frame(t(trt2))
trt2<-trt2[-101,]

#heatmap
library("gplots")
library("ggplot2")
df=(as.matrix(trt2[,c(1:4)]))
heatmap.2(df, Rowv=FALSE,Colv=TRUE,dendrogram="col",trace="none",srtCol=360, adjCol = c(0.5,1) )


#data6<-clusterdata1
data6<-clusterdata1[,c(1,2,4,32,33,34,36,37)]
for (i in 1:(nrow(data6)-1)) {
  data6$nextcluster[i]<-data6$cluster[i+1]
}

#clusterdata1%>%group_by(patientunitstayid)%>%filter(day!= max(day))%>%mutate(reward = if_else(day=max(day), 0, reward))

#delete dead patients' last record
uni<-unique(data6[as.character(data6$hospitaldischargestatus)=="Expired",]$patientunitstayid)
data6$sequence=1:nrow(data6)
for (i in uni) {
  a<-data6[data6$patientunitstayid==i& data6$day==max(data6[data6$patientunitstayid==i,]$day),]$sequence
  data6<-data6[data6$sequence!=a,]
  data6$reward<-ifelse(data6$sequence==a-1,-100,data6$reward)
}
#delete alive patients with full record no.7
uni2<-unique(data6[as.character(data6$hospitaldischargestatus)=="Alive",]$patientunitstayid)
data6$sequence=1:nrow(data6)
for (i in uni2) {
  a<-data6[data6$patientunitstayid==i& data6$day==max(data6[data6$patientunitstayid==i,]$day),]$sequence
  data6<-data6[data6$sequence!=a,]
  data6$reward<-ifelse(data6$sequence==a-1,100,data6$reward)
}
data6$reward<-ifelse(data6$reward==100,100,ifelse(data6$reward==-100,-100,0))
#data 6 is ready for the reinforcement learning

data7<-data6
data7<-data7[,c(4,5,7,9)]
data7$cluster<-as.character(data7$cluster)
data7$nextcluster<-as.character(data7$nextcluster)
data7$treatment<-as.character(data7$treatment)
data7$reward<-as.numeric(data7$reward)
# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.99, epsilon = 0.1)

# Perform reinforcement learning
model3 <- ReinforcementLearning(data7, s = "cluster", a = "treatment", r = "reward", 
                                s_new = "nextcluster", iter = 5000, control = control,verbose=TRUE)
View(model3$Q)
# Calculate optimal policy
pol <- computePolicy(model3)

# Print policy
head(pol)
# X39    X40    X41    X42    X43    X44 
# "PVO"   "NE"  "PVO" "None" "None"   "NE"

#organize the table with Q-value and the states
table2<-as.data.frame(cbind(model3$Q,model3$Policy))
table2$state<-as.numeric(substring(rownames(table2),2))
rownames(table2)<-table2$state
table2<-table2[order(table2$state),]
colnames(table2)[5]<-'AI'

#combine with treatment with each state
trt2$state=rownames(trt2)
matching2<-match(table2$state,trt$state)
for (i in 1:4) {
  table2[,6+i]=trt[,5+i][matching2]
}
colnames(table2)[7:10]<-colnames(trt)[6:9]
colnames(table2)[7:10]<-gsub('Probability','2',colnames(table2)[7:10])

table2<-table2[,c(6,1,2,3,4,8,10,7,9,5)]
treatmentlist1 <- c("NEPVO","PVO","NE","None")
table2$Clinician<- treatmentlist1[apply(table2[c(6,7,8,9)], 1, which.max)]
table2[,c(2,3,4,5)]<-lapply(table2[,c(2,3,4,5)],as.character)
table2[,c(2,3,4,5)]<-lapply(table2[,c(2,3,4,5)],as.numeric)
write.csv(table2,file="table1 with 4 treatments.csv")

pi0 <- table2[,c(6,7,8,9)]
colnames(pi0)<-gsub(" *2","",colnames(pi0))
pi0 # this is the clinician policy

pi1 <- pi0
pi1 <- (pi1 < 0) + (1 - 0.9) / 3
colnames(pi1)<-gsub(" *2","",colnames(pi1))
for(j in 1:length(model3$Policy)) {
  pi1[rownames(pi1) == substr(names(model3$Policy)[j],2,5), colnames(pi1) == model3$Policy[j]] <- 0.9
}
rowSums(pi1) # this is the AI epsilon (0.1) greedy policy

pi2 <- pi0
pi2 <- (pi2 < 0) + (1 / 4)

ratios0 <- pi0 / pi0
ratios0[is.na(ratios0)] <- 0
ratios1 <- pi1 / pi0
ratios1[ratios1 == Inf] <- 0
ratios2 <- pi2 / pi0
ratios2[ratios2 == Inf] <- 0

#heatmap
df<-as.data.frame(exp(t(scale(t(table2[,c(2,3,4,5)]),center = TRUE,scale=TRUE))))
table2[,c(2,3,4,5)]<-df/outer(rowSums(df), rep(1, 4))
df<-table2[,c(2,3,4,5)]
matching3<-match(rownames(df),reward$cluster)
df$rank<-reward$rank1[matching3]
rownames(df)<-df$rank
df<-df[order(df$rank),]
df<-as.matrix(df[,c(1:4)])
heatmap.2(df, Rowv=FALSE,Colv=TRUE,dendrogram="col",trace="none",srtCol=360, adjCol = c(0.5,1),col=bluered,  tracecol="#303030")

df=table2[,c(6:9)]
colnames(df)<-colnames(table2)[2:5]
matching3<-match(rownames(df),reward$cluster)
df$rank<-reward$rank1[matching3]
rownames(df)<-df$rank
df<-df[order(df$rank),]
df<-as.matrix(df[,c(1:4)])
heatmap.2(df, Rowv=FALSE,Colv=TRUE,dendrogram="col",trace="none",srtCol=360, adjCol = c(0.5,1),col=bluered, tracecol="#303030")

data_long <- gather(table2,Q,probability, `NEPVO`:`None`, factor_key=TRUE)
theme_set(theme_pubclean())
ggplot(data_long, aes(Clinician,probability)) +
  geom_boxplot(aes(color = Q))+
  scale_color_manual(values = c("#00AFBB", "#E7B800","#56B4E9","#CC79A4")) +
  stat_compare_means(aes(group = Q), label = "p.signif")
data_long1<-table2[table2$Clinician=="NEPVO",]
long1w<-gather(data_long1,trt,value,'NEPVO':'None')
aov1<-aov(long1w$value~long1w$trt,data=long1w)
summary(aov1)
pairwise.t.test(long1w$value,long1w$trt,p.adj="bonferroni")

data_long2<-table2[table2$Clinician=="None",]
long2w<-gather(data_long2,trt,value,'NEPVO':'None')
aov2<-aov(long2w$value~long2w$trt,data=long2w)
summary(aov2)
pairwise.t.test(long2w$value,long2w$trt,p.adj="bonferroni")

ratios1$cluster<-rownames(ratios1)
table6<-ratios1

table7<-reshape(table6, 
        direction = "long",
        varying = list(names(table6)[1:4]),
        v.names = "Ratio",
        idvar = "cluster",
        timevar = "treatment",
        times = c("NEPVO","PVO","NE","None"))
table8<-merge(table7,data6,by=c("cluster","treatment"),all.x=TRUE,sort=FALSE)
table8<-table8[order(table8$patientunitstayid,table8$day),]
unique<-unique(table8$patientunitstayid)
matrix1<-as.data.frame(matrix(NA,nrow=length(unique),ncol=2))
matrix1$V1<-unique
for (i in 1:length(unique)) {
  subset=table8[table8$patientunitstayid==unique[i],]
  matrix1$V2[i]<-prod(subset$Ratio)
}
colnames(matrix1)<-c("patientunitstayid","Ratio")

#duplicate number to group
for (i in 1:length(unique)) {
  matrix1$duplicated[i]<-sum(duplicated(table8[table8$patientunitstayid==matrix1$patientunitstayid[i],]$patientunitstayid))
  matrix1$duplicated[i]<-matrix1$duplicated[i]+1
}

#denote the weights
matrix1$w<-NA
for (i in 1:6) {
  subset=matrix1[matrix1$duplicated==i,]
  matrix1[matrix1$duplicated==i,]$w=sum(subset$Ratio)/length(subset$patientunitstayid)
}

#calculate trajectory-wise WIS
matrix1$v<-NA
for (i in 1:length(unique)) {
  subset<-table8[table8$patientunitstayid==matrix1$patientunitstayid[i],]
  matrix1$v[i]<-sum(subset$reward)*matrix1$Ratio[i]/matrix1$w[i]
}
mean(matrix1$v)
#89.05968

ratios0$cluster<-rownames(ratios0)
table6<-ratios0

table7<-reshape(table6, 
                direction = "long",
                varying = list(names(table6)[1:4]),
                v.names = "Ratio",
                idvar = "cluster",
                timevar = "treatment",
                times = c("NEPVO","PVO","NE","None"))
table8<-merge(table7,data6,by=c("cluster","treatment"),all.x=TRUE,sort=FALSE)
table8<-table8[order(table8$patientunitstayid,table8$day),]
unique<-unique(table8$patientunitstayid)
matrix1<-as.data.frame(matrix(NA,nrow=length(unique),ncol=2))
matrix1$V1<-unique
for (i in 1:length(unique)) {
  subset=table8[table8$patientunitstayid==unique[i],]
  matrix1$V2[i]<-prod(subset$Ratio)
}
colnames(matrix1)<-c("patientunitstayid","Ratio")

#duplicate number to group
for (i in 1:length(unique)) {
  matrix1$duplicated[i]<-sum(duplicated(table8[table8$patientunitstayid==matrix1$patientunitstayid[i],]$patientunitstayid))
  matrix1$duplicated[i]<-matrix1$duplicated[i]+1
}

#denote the weights
matrix1$w<-NA
for (i in 1:6) {
  subset=matrix1[matrix1$duplicated==i,]
  matrix1[matrix1$duplicated==i,]$w=sum(subset$Ratio)/length(subset$patientunitstayid)
}

#calculate trajectory-wise WIS
matrix1$v<-NA
for (i in 1:length(unique)) {
  subset<-table8[table8$patientunitstayid==matrix1$patientunitstayid[i],]
  matrix1$v[i]<-sum(subset$reward)*matrix1$Ratio[i]/matrix1$w[i]
}
mean(matrix1$v)
#59.90538



#random
ratios2$cluster<-rownames(ratios2)
table6<-ratios2

table7<-reshape(table6, 
                direction = "long",
                varying = list(names(table6)[1:4]),
                v.names = "Ratio",
                idvar = "cluster",
                timevar = "treatment",
                times = c("NEPVO","PVO","NE","None"))
table8<-merge(table7,data6,by=c("cluster","treatment"),all.x=TRUE,sort=FALSE)
table8<-table8[order(table8$patientunitstayid,table8$day),]
unique<-unique(table8$patientunitstayid)
matrix1<-as.data.frame(matrix(NA,nrow=length(unique),ncol=2))
matrix1$V1<-unique
for (i in 1:length(unique)) {
  subset=table8[table8$patientunitstayid==unique[i],]
  matrix1$V2[i]<-prod(subset$Ratio)
}
colnames(matrix1)<-c("patientunitstayid","Ratio")

#duplicate number to group
for (i in 1:length(unique)) {
  matrix1$duplicated[i]<-sum(duplicated(table8[table8$patientunitstayid==matrix1$patientunitstayid[i],]$patientunitstayid))
  matrix1$duplicated[i]<-matrix1$duplicated[i]+1
}

#denote the weights
matrix1$w<-NA
for (i in 1:6) {
  subset=matrix1[matrix1$duplicated==i,]
  matrix1[matrix1$duplicated==i,]$w=sum(subset$Ratio)/length(subset$patientunitstayid)
}

#calculate trajectory-wise WIS
matrix1$v<-NA
for (i in 1:length(unique)) {
  subset<-table8[table8$patientunitstayid==matrix1$patientunitstayid[i],]
  matrix1$v[i]<-sum(subset$reward)*matrix1$Ratio[i]/matrix1$w[i]
}
mean(matrix1$v)
#68.39275
sum(table8$reward)/length(unique)
#59.90538

#crossvalidation
policyreward0<-as.data.frame(matrix(NA,ncol=3))
for (k in 1:1298) {
  trainID<-sample(unique(data6$patientunitstayid),round(length(unique(data6$patientunitstayid))),replace=TRUE)
  train<-data6[which(data6$patientunitstayid %in% trainID),]
  traindata<-data6[which(data6$patientunitstayid %in% trainID),]
  
  trttest<-as.data.frame(matrix(NA))
  for (i in 1:100) {
    cluster<-traindata[traindata$cluster==i,]
    trttest[c(1:4),i]<-as.data.frame((as.matrix(table(cluster$treatment))))$V1
    trttest[5,i]<-nrow(cluster)
    trttest[c(6:9),i]<-trttest[c(1:4),i]/nrow(cluster)
  }
  rownames(trttest)<-c(rownames(as.data.frame((as.matrix(table(cluster$treatment))))),"Size",paste0(rownames(as.data.frame((as.matrix(table(cluster$treatment))))),' Probability'))
  trttest<-round(trttest,digits=2)
  colnames(trttest)<-c(1:100)
  trttest<-as.data.frame(t(trttest))
  trttest$state=rownames(trttest)
   if (sum(as.data.frame(lapply(trttest[,c(6,7,8,9)],function(x){length(which(x==0))})))){
     next
   }
  
train<-train[,c(4,5,7,9)]
train$cluster<-as.character(train$cluster)
train$nextcluster<-as.character(train$nextcluster)
train$treatment<-as.character(train$treatment)
train$reward<-as.numeric(train$reward)
# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.99, epsilon = 0.1)
model3 <- ReinforcementLearning(train, s = "cluster", a = "treatment", r = "reward", 
                                s_new = "nextcluster", iter = 5000, control = control,verbose=TRUE)
pol <- as.data.frame(computePolicy(model3))
pol$state<-as.numeric(substring(rownames(pol),2))
names(pol)[1]<-"AItreatment"

tablet<-as.data.frame(cbind(model3$Q,model3$Policy))
tablet$state<-as.numeric(substring(rownames(tablet),2))
rownames(tablet)<-tablet$state
tablet<-tablet[order(tablet$state),]
colnames(tablet)[5]<-'AI'

#combine with treatment with each state
matching2<-match(tablet$state,trttest$state)
for (i in 1:4) {
  tablet[,6+i]=trttest[,5+i][matching2]
}
colnames(tablet)[7:10]<-colnames(trttest)[6:9]
colnames(tablet)[7:10]<-gsub('Probability','2',colnames(tablet)[7:10])

tablet<-tablet[,c(6,1,2,3,4,8,10,7,9,5)]
treatmentlist1 <- c("NEPVO","PVO","NE","None")
tablet$Clinician<- treatmentlist1[apply(tablet[c(6,7,8,9)], 1, which.max)]
tablet[,c(2,3,4,5)]<-lapply(tablet[,c(2,3,4,5)],as.character)
tablet[,c(2,3,4,5)]<-lapply(tablet[,c(2,3,4,5)],as.numeric)

pi0 <- tablet[,c(6,7,8,9)]
colnames(pi0)<-gsub(" *2","",colnames(pi0))
pi0 # this is the clinician policy

pi1 <- pi0
pi1 <- (pi1 < 0) + (1 - 0.9) / 3
colnames(pi1)<-gsub(" *2","",colnames(pi1))
for(j in 1:length(model3$Policy)) {
  pi1[rownames(pi1) == substr(names(model3$Policy)[j],2,5), colnames(pi1) == model3$Policy[j]] <- 0.9
}
rowSums(pi1) # this is the AI epsilon (0.1) greedy policy

pi2 <- pi0
pi2 <- (pi2 < 0) + (1 / 4)

ratios0 <- pi0 / pi0
ratios0[is.na(ratios0)] <- 0
ratios1 <- pi1 / pi0
ratios1[ratios1 == Inf] <- 0
ratios2 <- pi2 / pi0
ratios2[ratios2 == Inf] <- 0

ratios1$cluster<-rownames(ratios1)
table6<-ratios1

table7<-reshape(table6, 
                direction = "long",
                varying = list(names(table6)[1:4]),
                v.names = "Ratio",
                idvar = "cluster",
                timevar = "treatment",
                times = c("NEPVO","PVO","NE","None"))
table8<-merge(table7,traindata,by=c("cluster","treatment"),all.x=TRUE,sort=FALSE)
table8<-table8[order(table8$patientunitstayid,table8$day),]
unique<-unique(table8$patientunitstayid)
matrix1<-as.data.frame(matrix(NA,nrow=length(unique),ncol=2))
matrix1$V1<-unique
for (i in 1:length(unique)) {
  subset=table8[table8$patientunitstayid==unique[i],]
  matrix1$V2[i]<-prod(subset$Ratio)
}
colnames(matrix1)<-c("patientunitstayid","Ratio")

#duplicate number to group
for (i in 1:length(unique)) {
  matrix1$duplicated[i]<-sum(duplicated(table8[table8$patientunitstayid==matrix1$patientunitstayid[i],]$patientunitstayid))
  matrix1$duplicated[i]<-matrix1$duplicated[i]+1
}

#denote the weights
matrix1$w<-NA
for (i in 1:6) {
  subset=matrix1[matrix1$duplicated==i,]
  matrix1[matrix1$duplicated==i,]$w=sum(subset$Ratio)/length(subset$patientunitstayid)
}

#calculate trajectory-wise WIS
matrix1$v<-NA
for (i in 1:length(unique)) {
  subset<-table8[table8$patientunitstayid==matrix1$patientunitstayid[i],]
  matrix1$v[i]<-sum(subset$reward)*matrix1$Ratio[i]/matrix1$w[i]
}
aipolicy<-mean(matrix1$v)
cpolicy<-sum(table8$reward)/length(unique)

#random
ratios2$cluster<-rownames(ratios2)
table6<-ratios2

table7<-reshape(table6, 
                direction = "long",
                varying = list(names(table6)[1:4]),
                v.names = "Ratio",
                idvar = "cluster",
                timevar = "treatment",
                times = c("NEPVO","PVO","NE","None"))
table8<-merge(table7,traindata,by=c("cluster","treatment"),all.x=TRUE,sort=FALSE)
table8<-table8[order(table8$patientunitstayid,table8$day),]
unique<-unique(table8$patientunitstayid)
matrix1<-as.data.frame(matrix(NA,nrow=length(unique),ncol=2))
matrix1$V1<-unique
for (i in 1:length(unique)) {
  subset=table8[table8$patientunitstayid==unique[i],]
  matrix1$V2[i]<-prod(subset$Ratio)
}
colnames(matrix1)<-c("patientunitstayid","Ratio")

#duplicate number to group
for (i in 1:length(unique)) {
  matrix1$duplicated[i]<-sum(duplicated(table8[table8$patientunitstayid==matrix1$patientunitstayid[i],]$patientunitstayid))
  matrix1$duplicated[i]<-matrix1$duplicated[i]+1
}

#denote the weights
matrix1$w<-NA
for (i in 1:6) {
  subset=matrix1[matrix1$duplicated==i,]
  matrix1[matrix1$duplicated==i,]$w=sum(subset$Ratio)/length(subset$patientunitstayid)
}

#calculate trajectory-wise WIS
matrix1$v<-NA
for (i in 1:length(unique)) {
  subset<-table8[table8$patientunitstayid==matrix1$patientunitstayid[i],]
  matrix1$v[i]<-sum(subset$reward)*matrix1$Ratio[i]/matrix1$w[i]
}
rpolicy<-mean(matrix1$v)

policyreward<-cbind(cpolicy,aipolicy)
policyreward<-cbind(policyreward,rpolicy)
colnames(policyreward)<-colnames(policyreward0)
policyreward0<-rbind(policyreward,policyreward0)

}
policyvalue<-policyreward0
colnames(policyvalue)=c("Clinician","Q-learning","Random")
pvsummary<-as.data.frame(matrix(NA,ncol=3,nrow=2))
for (i in 1:3) {
  pvsummary[1,i]<-mean(policyvalue[,i])
  pvsummary[2,i]<-sd(policyvalue[,i])
}
rownames(pvsummary)<-c("mean","sd")
colnames(pvsummary)<-c("Clinician","Q-learning","Random")

boxplot(policyvalue,xlab="Policy",ylab="Value",ylim=c(50,100),na.rm=TRUE)
pvlong <- gather(policyvalue, policy, value, Clinician:Random, factor_key=TRUE,na.rm = TRUE)
pvlong
fit<-aov(value~policy,data=pvlong)
summary(fit)
pairwise.t.test(pvlong$value,pvlong$policy,p.adj="bonferroni")

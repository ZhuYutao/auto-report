library("data.table")
library("tidyr")
library("plyr")

data1<- fread("D:/work/暑期活动/每日数据/day5.csv", header=TRUE, sep=",", encoding="UTF-8")
sum1<-fread("D:/work/暑期活动/每日数据/summer1.csv", header=TRUE, sep=",", encoding="UTF-8")
sum2<-fread("D:/work/暑期活动/每日数据/summer2.csv", header=TRUE, sep=",", encoding="UTF-8")
sdata<-fread("D:/work/暑期活动/每日数据/数据汇总.csv", header=TRUE, sep=",", encoding="UTF-8")
sumdata<-fread("D:/work/暑期活动/每日数据/sumdata.csv", header=TRUE, sep=",", encoding="UTF-8")

data1$支付金额<-data1$支付金额/100
data2<-separate(separate(separate(data1,2,into = c("name","supplier"),sep = "_"),7,into = c("year","month","day","hour"),sep = "-"),9,into = c("day","hour"),sep = " ")
data2[is.na(data2)] <- "T2"

for (i in 1:nrow(data2)) {
  if (data2$name[i]=="T2游戏平台") {
    data2$name[i]<-data2$商品名称[i]
  }
}
gamepad<-data2[c(grep("49.9",data2$name),grep("北通",data2$name),grep("小鸡",data2$name),grep("运动加加",data2$name))]#筛选附件
data3<-data2[-c(grep("49.9",data2$name),grep("北通",data2$name),grep("小鸡",data2$name),grep("虚拟",data2$name),grep("实物",data2$name),grep("测试",data2$name),grep("买配件",data2$name))]
datas<-data2[-c(grep("虚拟",data2$name),grep("实物",data2$name),grep("测试",data2$name),grep("买配件",data2$name))]
if (nrow(data3)==0) {
  data3<-data2
}
if (nrow(datas)==0) {
  datas<-data2
}
data3$level<-0
datas$level<-0
#计算单笔支付等级
for (i in 1:nrow(data3)) {
  if (data3$支付金额[i]>=1000) {
    data3$level[i]<-5
  }else if (data3$支付金额[i]>=500) {
    data3$level[i]<-4
  }else if (data3$支付金额[i]>=100) {
    data3$level[i]<-3
  }else if (data3$支付金额[i]>=30) {
    data3$level[i]<-2
  }else{
    data3$level[i]<-1
  }
}

k=5    #第K天
dau=311692
data3<-data3[as.numeric(data3$day)==k,]
payper<-as.numeric(nrow(data3[!duplicated(data3$用户id),]))
#第一页数据 活动游戏单项数据
for (i in 1:nrow(datas)) {
  for (j in 2:length(sum1)) {
    if (datas$name[i]==names(sum1)[j]) {
      sum1[k,j]<-as.numeric(sum(datas[which(datas$name==names(sum1)[j]),]$支付金额))
    } 
  }
}

#第二页数据 汇总数据
for (i in 2:6) {
  j=i-1
  sum2[k,i]<-sum(data3$level==j)
}
sum2$付费次数[k]<-sum(sum2[k,c(2:6)])  #订单数
sum2$CPS流水[k]<-sum(data3$支付金额)  #当日流水
sum2$配件数量[k]<-nrow(gamepad)
sum2$配件流水[k]<-sum(gamepad$支付金额)
sum2$`总流水（元）`[k]<-sum2$CPS流水[k]+sum2$配件流水[k]
sum2$活跃付费转化率[k]<-paste(round(sum2[k,7]*100,2)/ as.numeric(dau),"%",sep = "")      #付费转换率
sum2$`DAU（万）`[k]<- round(as.numeric(dau)/10000,4)  #DAU
sum2$ARPPU[k]<-sum2$CPS流水[k]/sum2$付费次数[k] #人均ARPPU

#第三页 用户数据（新增、累计）
sumdata<-rbind(sumdata,datas) 
dsdata<-sumdata[!duplicated(sumdata$用户id),]
dsdata<-dsdata[,14]
dsdata$金额<-0
dsdata$等级<-0
for (i in 1:nrow(dsdata)) {
  iddata<-sumdata[sumdata$用户id==dsdata$用户id[i],]
  dsdata$金额[i]<-sum(iddata$支付金额)
}

for (i in 1:nrow(dsdata)) {
  if (dsdata$金额[i]>=1000) {
    dsdata$等级[i]<-5
  }else if (dsdata$金额[i]>=500) {
    dsdata$等级[i]<-4
  }else if (dsdata$金额[i]>=100) {
    dsdata$等级[i]<-3
  }else if (dsdata$金额[i]>=30) {
    dsdata$等级[i]<-2
  }else{
    dsdata$等级[i]<-1
  }
}
for (i in 6:10) {
  j=i-5
  sdata[k,i]<-sum(dsdata$等级==j)
}

sdata[k,2]<-payper
sdata[k,4]<-sum(sdata[k,c(6:10)])
sdata[k,3]<-sdata$付费人数累计[k]-sdata$付费人数累计[k-1]
sdata[k,5]<-paste(round((sdata$单日付费人数[k]/sdata$单日付费人数[k-1]-1)*100,2),"%",sep = "")


write.csv(sum1,"D:/work/暑期活动/每日数据/summer1.csv", row.names = F)
write.csv(sum2,"D:/work/暑期活动/每日数据/summer2.csv", row.names = F)
write.csv(sdata,"D:/work/暑期活动/每日数据/数据汇总.csv", row.names = F)
write.csv(sumdata,"D:/work/暑期活动/每日数据/sumdata.csv", row.names = F)

####筛选返现名单
sumdata<-data2
sumdata$level<-0

list<-names(sum1)[2:24]
sf<-sumdata[-c(1:10000000),]
for (i in 1:26) {
  fanxian<-sumdata[grep(list[i],sumdata$name)]
  sf<-rbind(sf,fanxian) 
}
df<-sf[!duplicated(sf$用户id)]
###抽奖
sample(sf[!duplicated(sf$用户id)]$用户id, 20, replace=F)
write.csv(sf,"D:/work/暑期活动/每日数据/fanxianlist7+.csv", row.names = F)

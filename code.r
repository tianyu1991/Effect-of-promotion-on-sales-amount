a<-read.csv("Sheet 4.csv")
library(dplyr)
a2<-group_by(a,ï..Month.of.Order.Date)
a3<-summarise(a2,sum=sum(Sales.Amount))
a4<-merge(a,a3,by="ï..Month.of.Order.Date")
Sales.percent<-a4.Sales.Amount/a4.sum
a5<-cbind(a4,Sales.percent)
New Product Promotion

prom<-c(1497010.911,1580440.858,2182655.998,1544526.03,1941087.024,3142115.042,
1518542.162,1665460.671,2790434.159,1609573.69,
2073132.924,3131966.24)

without_prom<-c(839807.4364,1239546.207,1254863.389,800944.8734,
1071563.711,1052060.586,591295.9602,879581.3591,
803324.8698,715634.3876,761885.1353,644591.6767)

t.test(prom,without_prom,paired=TRUE)

   Paired t-test

data:  prom and without_prom
t = 6.1064, df = 11, p-value = 7.667e-05
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  747317.4 1589656.9

According to the t test result, 0 is contented in 95 percent confidence interval, so the hypothesis that the

distribution of means is same as the normal distribution cannot be denied. Thus, the distribution of 

means is same as the normal distribution.

nosim<-12

mean<-mean(prom)-mean(without_prom)
s_mean<-sqrt(((nosim-1)*sd(prom)+(nosim-1)*sd(without_prom))/(nosim*2-2))
mean+c(-1,1)*qnorm(.05)*s_mean/sqrt(nosim)


excess_reseller<-read.csv("excess_reseller.csv",stringsAsFactors=FALSE)
excess_reseller2<-cbind(sales_amount=excess_reseller$Sales.Amount,reseller_key=excess_reseller$Reseller.Key,order_date=as.character(excess_reseller$Order.Date))
excess_reseller2<-as.data.frame(excess_reseller2)
excess_reseller2$sales_amount<-as.numeric(as.character(excess_reseller2$sales_amount))
group_id<-paste(excess_reseller2$reseller_key,excess_reseller2$order_date)
excess_reseller3<-cbind(excess_reseller2,group_id)

excess_reseller_c<-read.csv("excess_reseller_control.csv",stringsAsFactors=FALSE)
excess_reseller_c2<-cbind(sales_amount=excess_reseller_c$Sales.Amount,reseller_key=excess_reseller_c$Reseller.Key,order_date=excess_reseller_c$Order.Date)
excess_reseller_c2<-as.data.frame(excess_reseller_c2)
excess_reseller_c2$sales_amount<-as.numeric(as.character(excess_reseller_c2$sales_amount))

group_id<-paste(as.character(excess_reseller_c2$reseller_key),excess_reseller_c2$order_date)
excess_reseller_c3<-cbind(excess_reseller_c2,group_id)
excess_reseller_c4<-group_by(excess_reseller_c3,group_id)
excess_reseller_c5<-summarise(excess_reseller_c4,sales_amount=mean(sales_amount),reseller_key=mean(as.numeric(as.character(reseller_key))),order_date=mean(as.numeric(as.factor(order_date))))
excess_reseller_c6<-as.data.frame(excess_reseller_c5)

result<-merge(excess_reseller3, excess_reseller_c6, by="group_id", all = FALSE)

adjusted<-result[,2]/result[,5]
result<-cbind(result,adjusted)

a<-result[,7]==2|result[,7]==4|result[,7]==6|result[,7]==8|result[,7]==10|result[,7]==12|result[,7]==16|result[,7]==13

t.test(result[a,8],result[!a,8])

******
        Welch Two Sample t-test

data:  result[a, 8] and result[!a, 8]
t = 2.3136, df = 241.803, p-value = 0.02153
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.02193344 0.27329845
sample estimates:
mean of x mean of y 
0.7899453 0.6423294 

write.csv(result, file = "excess_result.csv")

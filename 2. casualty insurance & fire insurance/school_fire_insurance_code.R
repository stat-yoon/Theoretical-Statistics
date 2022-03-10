#### part3. 학교화재보험료 

# (a)QQplot


data3<-read.csv("학교화재.csv")
names(data3)<-c("x","n08","n09","n10","n11","n12","n13","n14","n15","n16","n17","n1317")
data3<-data3[1:6,1:12]
data3<-data3%>%mutate(x=as.numeric(x))
n<-sum(data3$n1317)+2

qq.pareto1 <- function(x,y,lambda){
  r = cumsum(y) 
  pr <- r/(n+1)
  lnxr <- -log(1-pr)
  yr <- log(1+x/lambda)
  fit <- lm(yr~lnxr)
  return(summary(fit)$r.squared)
}
x <- data3$x
y <- data3$n1317
r2 <- c()
lambda <- seq(1,120,1)
for(i in lambda){
  r2 <- c(r2,qq.pareto1(x, y, i))
}

lambda <- lambda[which.max(r2)]

x <-data3$x
count <- data3$n1317
pr<- cumsum(count)/(n+1)
pareto <- -log(1-pr)
y<-log(1+x/lambda)

alpha <- 1/summary(lm(y~pareto-1))$coef[[1]]

#파레토 최종 QQplot 
pareto.qq<-data3 %>% mutate(pr = cumsum(n1317)/(n+1),
                           pareto = -log(1-pr)/alpha)%>%
  ggplot(aes(x=pareto,y=log(1+x/lambda))) +
  geom_point(size=2) + geom_smooth(method="lm", se=F)+
  geom_smooth(method="lm", se=F)+stat_regline_equation(aes(label = ..rr.label..))

#R-squared 값 자세히 뽑아보기. 

x <-data3$x
count <- data3$n1317
pr<- cumsum(count)/(n+1)
library(jmuOutlier)
loglaplace<-qlaplace(pr)
i.weibull<- -log(-log(pr))
lognormal <- qnorm(pr)
loglogistic<-log(cumsum(count)/(n+1-cumsum(count)))

summary(lm(log(x)~loglaplace))$r.squared -> r2.1
summary(lm(log(x)~i.weibull))$r.squared -> r2.2
summary(lm(log(x)~loglogistic))$r.squared -> r2.3 
summary(lm(log(x)~lognormal))$r.squared -> r2.4
pareto <- -log(1-pr)/alpha 
y<-log(1+x/lambda)
summary(lm(y~pareto-1))$r.squared-> r2.5

#loglogistic & pareto 선택

kable(data.frame(Dist = c("loglaplace","Frechet","loglogistic","lognormal","pareto"), 
                 R_square= round(c(r2.1,r2.2,r2.3,r2.4,r2.5),3) ),align=c("rclcr")) %>% 
  kable_styling(full_width=F)




# (b) lognormal 모형 사용. 화재보험료 산정 


total.school<-18898+ 1863

En<-(sum(data3$n1317)+2)/(5*total.school)

Ey.norm<-function(x,a,b){
  mu <- round(estimator(data3$x,data3$n1317,"lognormal")[[1]],3)
  sigma <- round(estimator(data3$x,data3$n1317,"lognormal")[[2]],3)
  
  f<-function(x) {dlnorm(x,mu,sigma)*x}
  
  #f<-function(x) ((1/(x*sqrt(2*pi)*sigma))*(exp((-1/2)*((log(x)-mu)/sigma)^2)))*x
  Fab<-ifelse(b==Inf,0,plnorm(a+b,mu,sigma))
  Fa <-plnorm(a,mu,sigma)
  EX<-ifelse(b==Inf,integrate(f,a,a+b)$value+0*(1-Fab)-a*(Fab-Fa),
             integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa))
  #print(c(integrate(f,a,a+b)$value, b*(1-Fab), -a*(Fab-Fa)))
  return(EX)
}

data.frame(`한도` = c("inf",100000),
           `평균사고심도` = c(Ey.norm(data3$x,0,Inf),Ey.norm(data3$x,0,100000)),
           `보험료` = c(round(Ey.norm(data3$x,0,Inf)*En,3)*10000,round(Ey.norm(data3$x,0,100000)*En,3)*10000)
           ) -> table3b

kable(table3b,align=c("rclcr")) %>% kable_styling(full_width=F)






# (c) 보상한도에 따른 화재보험료 산정 

#loglogistic 
Ey.loglogi<-function(x,a,b){
  lambda <- estimator(data3$x,data3$n1317,"loglogistic")[[1]]
  alpha <- estimator(data3$x,data3$n1317,"loglogistic")[[2]]

  f<-function(x) (alpha*x^(alpha-1)*lambda^(-alpha)/(1+(x/lambda)^alpha)^2)*x
  Fab<-ifelse(b==Inf,0,d.loglogi(a+b,lambda,alpha))
  Fa<-d.loglogi(a,lambda,alpha)
  EX<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(EX)
}


Ey.pareto<-function(x,a,b){
  lamda <- estimator(data3$x,data3$n1317,"pareto")[[1]]
  alpha <- round(estimator(data3$x,data3$n1317,"pareto")[[2]],3)
  f<- function(x) {(alpha*(lamda^alpha)*((lamda+x)^(-alpha-1)))*x}
  Fab<-d.pareto(a+b,lamda,alpha)
  Fa<-d.pareto(a,lamda,alpha)
  Ey<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(Ey)
}



#loglogistic 

df_price1<-data.frame(`보상한도`= NA, `평균사고심도`= NA, `보험료`=NA )

i<-1
for(b in c(10000,50000,100000)){
    Ey1 <- Ey.loglogi(data3$x,0,b)
    price1 <- Ey1*En

    df_price1[i,] <- c(b,round(Ey1,3),
                      paste(format(round(price1,3)*10000,big.mark = ","),"원"))
    i <- i+1
}

kable(df_price1,align=c("rclcr"),caption="[log_logistic]") %>% kable_styling(full_width=F)




# pareto 
df_price2<-data.frame(`보상한도`= NA, `평균사고심도`= NA, `보험료`=NA )

i<-1
for(b in c(10000,50000,100000)){

    Ey2 <- Ey.pareto(data3$x,0,b)
    price2 <- Ey2*En
    
    df_price2[i,] <- c(b, round(Ey2,3) ,
                      paste(format(round(price2,3)*10000,big.mark = ","),"원"))
    i <- i+1
}

kable(df_price2,align=c("rclcr"),caption="[Pareto]") %>% kable_styling(full_width=F)










# (d) 2008-2012년도 손해율 

#demage : 각 연도별 총 손실액 

f.price <- function(demage,b,num){
    price1 <- Ey.loglogi(data3$x,0,b)*En
    price2 <- Ey.pareto(data3$x,0,b)*En
    total.school <-num
    result1 <- round(demage/(num*price1)*100,3)
    result2 <- round(demage/(num*price2)*100,3)
    
    return(c(result1,result2))
}

demage <- c(176163,89734,139708,81843,115335)
num <-c(19780,19893,19989,20366,20527)
year <- seq(2008,2012,1)

loss_ratio <-data.frame(year = NA , `보상한도` = NA, `loglogistic_손해율`=NA, `Pareto_손해율` = NA)
j<-1
for (i in 1:5){
  for(b in c(10000,50000,100000)){

      loss_ratio[j,] <- c(year[i] , b, paste(f.price(demage[i],b,num[[i]])[[1]],"%"),
                     paste(f.price(demage[i],b,num[[i]])[[2]], "%"))
      j <- j+1
  }
}

kable(loss_ratio,align=c("rclcr")) %>% kable_styling(full_width=F)
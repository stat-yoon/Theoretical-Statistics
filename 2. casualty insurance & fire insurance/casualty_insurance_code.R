

#### 1 [상해보험] 

data<- read_excel('상해보험자료2.xls',sheet=1)
data<-data[1:50,]
names(data)<-c("x","count")
data<-data%>% mutate(x=as.numeric(x))

# (1) histogram
ggplot(data[1:50,],aes(as.numeric(x),count))+
  geom_bar(stat="identity")+xlab("사고치료비(미만)")+ylab("건수")


# (2) QQplot 

n<-8443
#로그노말
lognorm.qq<-data%>% mutate(pr=cumsum(count)/(n+1), lognormal = qnorm(pr))%>%
  ggplot( aes(lognormal, log(as.numeric(data$x)))) + geom_point(size=2) +
  geom_smooth(method="lm", se=F)+stat_regline_equation(aes(label = ..rr.label..))

#파레토
## 모수추정 - 초기값 구하기 
####lambda 
qq.pareto1 <- function(x,y,lambda){
  r <- cumsum(y) 
  pr <- r/(n+1)
  lnxr <- -log(1-pr)
  yr <- log(1+x/lambda)
  fit <- lm(yr~lnxr-1)
  return(summary(fit)$r.squared)
}
x <- data$x
y <- data$count
r2 <- c()
lambda <- seq(1,200,1)
for(i in lambda){
  r2 <- c(r2,qq.pareto1(x, y, i))
}
lambda <- lambda[which.max(r2)]
x <- data$x
y <- data$count
r <- cumsum(y) 
pr <- r/(n+1)

pareto <- -log(1-pr)
alpha <- 1/summary(lm(log(1+x/lambda)~pareto-1))$coef[[1]]


#파레토 최종 QQplot 
pareto.qq<-data %>% mutate(pr = cumsum(count)/(n+1),
                           pareto = -log(1-pr)/alpha)%>%
  ggplot(aes(x=pareto,y=log(1+x/lambda))) +
  geom_point(size=2) + geom_smooth(method="lm", se=F)+
  geom_smooth(method="lm", se=F)+
  stat_regline_equation(aes(label = ..rr.label..))


#와이블
w.qq<- data%>% mutate( pr=cumsum(count)/(n+1), weibull=log(-log(1-pr)))%>% ggplot(aes(weibull, log(x))) +
  geom_point(size=2) + 
  geom_smooth(method="lm", se=F)+
  stat_regline_equation(aes(label = ..rr.label..))


#역와이블
iw.qq<- data%>% mutate( pr=cumsum(count)/(n+1),
                        i.weibull=-log(-log(pr)))%>%
  ggplot(aes(i.weibull, log(x))) + geom_point(size=2) + 
  geom_smooth(method="lm", se=F)+stat_regline_equation(aes(label = ..rr.label..))

#로그로지스틱
loglogi.qq<-data%>% 
  mutate(loglogistic=log(cumsum(count)/(n+1-cumsum(count))))%>%
  ggplot(aes(loglogistic, log(x))) + geom_point(size=2) +
  geom_smooth(method="lm", se=F)+stat_regline_equation(aes(label = ..rr.label..))

grid.arrange(lognorm.qq,pareto.qq, w.qq,iw.qq,loglogi.qq, ncol=3)


#R-squared 값 자세히 뽑아보기. 
x <-data$x
count <- data$count
pr<- cumsum(count)/(n+1)
weibull<-log(-log(1-pr))
i.weibull<- -log(-log(pr))
lognormal <- qnorm(pr)
loglogistic<-log(cumsum(count)/(n+1-cumsum(count)))
summary(lm(log(x)~weibull))$r.squared -> r2.1
summary(lm(log(x)~i.weibull))$r.squared ->r2.2
summary(lm(log(x)~loglogistic))$r.squared -> r2.3
summary(lm(log(x)~lognormal))$r.squared -> r2.4
pareto <- -log(1-pr)/alpha
y<-log(1+x/lambda)
summary(lm(y~pareto-1))$r.squared -> r2.5


kable(data.frame(Dist = c("weibull","Frechet","loglogistic","lognormal","pareto"), 
                 R_square= round(c(r2.1,r2.2,r2.3,r2.4,r2.5),3) ),align=c("rclcr")) %>% 
  kable_styling(full_width=F)



#모수계산함수 

w.coef <- function(coef){
    mu <- coef[[1]]
    sigma<- coef[[2]]
    tau <- 1/sigma
    c<- exp(-mu)
    return(list(tau=tau,c=c)) 
}

iw.coef <- function(coef){
    mu <- coef[[1]]
    sigma <- coef[[2]]
    tau<- 1/sigma 
    c<- exp(mu*tau)
    return(list(tau=tau,c=c))
}

loglogi.coef <- function(coef){
    mu <- coef[[1]]
    sigma <- coef[[2]]
    lambda<-exp(mu)
    alpha<-1/sigma
    return(list(lambda=lambda,alpha=alpha))
}


###estimate function 

estimator <- function(x,count,dist){
  pr<-cumsum(count)/(n+1)
  lognormal <- qnorm(pr)
  pareto.y<-log(1+x/lambda)
  pareto <- -log(1-pr)/alpha
  weibull <- log(-log(1-pr))
  i.weibull <- -log(-log(pr))
  loglogistic <- log(cumsum(count)/(n+1-cumsum(count)))
  
  if(dist=="lognormal"){
    model<-lm(log(x)~lognormal)
    mu<-model$coef[[1]]
    sigma<-model$coef[[2]]
    result<- list(mu=mu,sigma=sigma)
    
  } else if(dist=="pareto"){
    model<-lm(pareto.y~pareto)
    result<-list(lambda=lambda,alpha=alpha) ### 
    
  } else if(dist=="weibull"){
    model<-lm(log(x)~weibull)
    tau <- w.coef(model$coef)[[1]]
    c <- w.coef(model$coef)[[2]]
    result <- list(tau=tau,c=c)
    
  } else if(dist=="i.weibull"){
    model<-lm(log(x)~i.weibull)
    tau <- iw.coef(model$coef)[[1]]
    c <- iw.coef(model$coef)[[2]]
    result <- list(tau=tau,c=c)
    
  } else {
    model<-lm(log(x)~loglogistic)
    lambda<-loglogi.coef(model$coef)[[1]]
    alpha<-loglogi.coef(model$coef)[[2]]
    result <- list(lambda=lambda,alpha=alpha)
  }
    
    return(result) 
}

#각 모델의 모수 추정치
estimator(data$x,data$count,"lognormal") -> e1
estimator(data$x,data$count,"weibull") -> e2
estimator(data$x,data$count,"i.weibull") ->e3 
estimator(data$x,data$count,"pareto") -> e4
estimator(data$x,data$count,"loglogistic") ->e5

kable(data.frame(estimator = c("lognormal","weibull","Frechet","Pareto","Log-logistic"),
           `par_1` = round(c(e1[[1]],e2[[1]],e3[[1]],e4[[1]],e5[[1]]),3),
           `par_2` = round(c(e1[[2]],e2[[2]],e3[[2]],e4[[2]],e5[[2]]),3)
           ),align=c("rclcr")) %>% kable_styling(full_width=F)


# (3) 적정 보험료 계산

d.lognormal<- function(x,mu,sigma){
  value <- pnorm((log(x)-mu)/sigma)
  return(value)
}

d.pareto<- function(x,lambda,alpha){
  value <- 1-(lambda/(lambda+x))^alpha
  return(value)
}

d.weibull<- function(x,tau,c){
  value <- 1-exp(-c*(x^tau))
  return(value)
}

d.iweibull<- function(x,tau,c){
  value <- exp(-c/(x^tau))
  return(value)
}

d.loglogi<- function(x,lambda,alpha){
  value <- 1-(1/(1+(x/lambda)^alpha))
  return(value)
}


Ey.norm<-function(x,a,b){
  mu <- estimator(data$x,data$count,"lognormal")[[1]]
  sigma <- estimator(data$x,data$count,"lognormal")[[2]]
  f<-function(x) (1/(x*sqrt(2*pi)*sigma)*exp((-1/2)*((log(x)-mu)/sigma)^2))*x
  Fab<-pnorm(a+b,mu,sigma)
  Fa <-pnorm(a,mu,sigma)
  EX<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(EX)
}

Ey.w<-function(x,a,b){
  tau <- estimator(data$x,data$count,"i.weibull")[[1]]
  c <- estimator(data$x,data$count,"i.weibull")[[2]]
  f<- function(x) (c*tau*x^(tau-1)*exp(-c*x^tau))*x
  Fab<-d.iweibull(a+b,tau,c)
  Fa<-d.iweibull(a,tau,c)
  EX<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(EX)
}

Ey.iw<-function(x,a,b){
  tau <- estimator(data$x,data$count,"i.weibull")[[1]]
  c <- estimator(data$x,data$count,"i.weibull")[[2]]
  f<- function(x) {((c*tau*exp(-c/x^tau))/x^(tau+1))*x}
  Fab<-d.iweibull(a+b,tau,c)
  Fa<-d.iweibull(a,tau,c)
  EX<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(EX)
}

Ey.loglogi<-function(x,a,b){
  lambda <- estimator(data$x,data$count,"loglogistic")[[1]]
  alpha <- estimator(data$x,data$count,"loglogistic")[[2]]

  f<-function(x) {(alpha*x^(alpha-1)*lambda^(-alpha)/(1+(x/lambda)^alpha)^2)*x}
  Fab<-d.loglogi(a+b,lambda,alpha)
  Fa<-d.loglogi(a,lambda,alpha)
  EX<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(EX)
}


Ey.pareto<-function(x,a,b){
  lambda <- estimator(data$x,data$count,"pareto")[[1]]
  alpha <- round(estimator(data$x,data$count,"pareto")[[2]],3)
  f<- function(x) {(alpha*(lambda^alpha)*((lambda+x)^(-alpha-1)))*x}
  Fab<-d.pareto(a+b,lambda,alpha)
  Fa<-d.pareto(a,lambda,alpha)
  Ey<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(Ey)
}

En<-8443/271306

df_price<-data.frame(a = NA, b= NA, `p1_prechet` =NA , `p2_pareto` = NA)
i<-1
for(a in c(0,10,20)){
  for(b in c(50,100,200,500,1000)){
    price1 <- Ey.iw(data$x,a,b)*En
    price2 <- Ey.pareto(data$x,a,b)*En

    df_price[i,] <- c(a,b,paste(format(round(price1,3)*10000,big.mark = ","),"원"),
                      paste(format(round(price2,3)*10000,big.mark = ","),"원"))
    i <- i+1
  }
}
kable(df_price,align=c("rclcr")) %>% kable_styling(full_width=F)


# (4) 손해율 비교 

kable(data.frame(`손해율` = c("frechet","Pareto"), 
                 Value= c(paste(round(259699/(271306*0.9570 )*100,3),"%"),paste(round(259699/(271306*0.9540)*100,3), "%"))),align=c("rclcr")) %>% kable_styling(full_width=F)


# (5) R-shiny

# (6) 5만원 이하의 사고자료가 없는 경우(절단된 자료)를 가지고 손해액 분포 추정 

data2<- read_excel('상해보험자료2.xls',sheet=2)
names(data2)<-c("x","count")
data2.qq<-data2[6:50,]%>% mutate(x=as.numeric(x))

#rsquare를 크게하는 n추정 
search.n <- function(x,y,n){
  r = cumsum(y) 
  pr <- r/(n+1)
  i.weibull <- -log(-log(pr))
  fit <- lm(log(x)~i.weibull)
  return(summary(fit)$r.squared)
}
x <- data2.qq$x
y <- data2.qq$count
r2 <- c()
n <- seq(6000,8000,1)
for(i in n){
  r2 <- c(r2, search.n(x, y, i))
}
n <- n[which.max(r2)]
n

#Q-Q
lognorm.qq<-data2.qq%>% mutate(pr=cumsum(count)/(n+1), lognormal = qnorm(pr))%>%
  ggplot( aes(lognormal, log(as.numeric(data2.qq$x)))) + geom_point(size=2) +
  geom_smooth(method="lm", se=F)+stat_regline_equation(aes(label = ..rr.label..))

#와이블
w.qq<- data2.qq%>% mutate( pr=cumsum(count)/(n+1), weibull=log(-log(1-pr)))%>% ggplot(aes(weibull, log(x))) +
  geom_point(size=2) + 
  geom_smooth(method="lm", se=F)+stat_regline_equation(aes(label = ..rr.label..))

#역와이블
iw.qq<- data2.qq%>% mutate( pr=cumsum(count)/(n+1),
                        i.weibull=-log(-log(pr)))%>%
  ggplot(aes(i.weibull, log(x))) + geom_point(size=2) + 
  geom_smooth(method="lm", se=F)+stat_regline_equation(aes(label = ..rr.label..))

#로그로지스틱
loglogi.qq<-data2.qq%>% 
  mutate(loglogistic=log(cumsum(count)/(n+1-cumsum(count))))%>%
  ggplot(aes(loglogistic, log(x))) + geom_point(size=2) +
  geom_smooth(method="lm", se=F)+stat_regline_equation(aes(label = ..rr.label..))


#파레토
## 모수추정 - 초기값 구하기 
####lambda 
qq.pareto1 <- function(x,y,lambda){
  r = cumsum(y) 
  pr <- r/(n+1)
  lnxr <- -log(1-pr)
  yr <- log(1+x/lambda)
  fit <- lm(yr~lnxr-1)
  return(summary(fit)$r.squared)
}
x <- data2.qq$x
y <- data2.qq$count
r2 <- c()
lambda <- seq(1,200,1)
for(i in lambda){
  r2 <- c(r2,qq.pareto1(x, y, i))
}
lambda <- lambda[which.max(r2)]
pareto <- -log(1-cumsum(y)/(n+1))
y<-log(1+x/lambda)
alpha <- 1/summary(lm(y~pareto-1))$coef[[1]]


#파레토 최종 QQplot 
pareto.qq<-data2.qq %>% mutate(pr = cumsum(count)/(n+1),
                           pareto = -log(1-pr)/alpha)%>%
  ggplot(aes(x=pareto,y=log(1+x/lambda))) +
  geom_point(size=2) + geom_smooth(method="lm", se=F)+
  geom_smooth(method="lm", se=F)+stat_regline_equation(aes(label = ..rr.label..))

grid.arrange(lognorm.qq,pareto.qq, w.qq,iw.qq,loglogi.qq, ncol=5)


#R squared
x <- data2.qq$x
y <- data2.qq$count
pr<- cumsum(y)/(n+1)
weibull<-log(-log(1-pr))
i.weibull<- -log(-log(pr))
lognormal <- qnorm(pr)
loglogistic<-log(cumsum(y)/(n+1-cumsum(y)))
summary(lm(log(x)~weibull))$r.squared ->r2.1
summary(lm(log(x)~i.weibull))$r.squared -> r2.2 
summary(lm(log(x)~loglogistic))$r.squared-> r2.3
summary(lm(log(x)~lognormal))$r.squared -> r2.4
pareto <- -log(1-pr)/alpha
y<-log(1+x/lambda)
summary(lm(y~pareto-1))$r.squared -> r2.5

#Pareto 선택 
kable(data.frame(Dist = c("weibull","Frechet","loglogistic","lognormal","pareto"), 
                 R_square= round(c(r2.1,r2.2,r2.3,r2.4,r2.5),3) ),align=c("rclcr")) %>% kable_styling(full_width=F)


#초기값
loglik <-function(par){
  m<-par[1]; lambda<-par[2]; alpha<-par[3]
  data <- data2.qq
  data["Ft"] <- 1-(lambda/(lambda+x))^alpha
  data[2:45,"prob"] <- diff(data$Ft)
  data[1,"prob"] <- data[1,"Ft"] - 0 
  data[46,"prob"] <- 1 - data[45,"Ft"]
  data[46,"count"] <- m - sum(data$count[1:45])
  data <- data %>% mutate(v1 = lgamma(count+1) ,
                          v2 = count*log(prob))
  loglik <- lgamma(m+1) - sum(data$v1) + sum(data$v2)
  return(-loglik)
}

optim3 <- optim(par=c(8500,lambda,alpha), 
                loglik, hessian=T, method= "BFGS")
mle <- optim3$par
n1<-round(mle)[[1]]-n

kable(data.frame(Estimator = c("m","n1","lambda","alpha"), 
                 Value= c(round(mle[1]),n1,round(mle[2],3),round(mle[3],3)) ),align=c("rclcr")) %>% kable_styling(full_width=F)




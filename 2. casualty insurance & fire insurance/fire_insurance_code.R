#### part2. 특수건물(아파트) 화재보험료 계산 


# 단위: 천원
apart <- read.csv("아파트 규모별 피해.csv", encoding="cp949")
apart <- apart[1:20,]
apart["x"] <- as.integer(apart$x)

# 단위 -> 만원
head(apart)


# (1) QQplot


# quantile function
# x = r/(n+1)
g_lognorm <- function(x){
  return(qnorm(x))
}

g_pareto <- function(x){
  return(-log(1-x))
}

g_weibull <- function(x){
  return(log(-log(1-x)))
}

g_frechet <- function(x){
  return(-log(-log(x)))
}

g_llogis <- function(x){
  return(log(x/(1-x)))
}

# loss Q-Q plot을 위한 dataframe
apart <- apart %>% select(x, n) %>%  arrange(x) %>% mutate(lnx = log(x), r = cumsum(n), pr = r/(sum(n)+1), lognorm = g_lognorm(pr), pareto = g_pareto(pr), weibull = g_weibull(pr), frechet = g_frechet(pr), llogis = g_llogis(pr))

# n=0제외
apart <- apart[1:15,]

# pareto Q-Q plot
## lambda select
lambda = seq(2000,3000,10)
r2 <- c()
for(k in lambda){
  y = log(1+apart$x/k)
  x = apart$pareto
  fit = lm(y~x-1)
  r2 <- c(r2,summary(fit)$r.squared)
}
print(round(c(lambda = lambda[which.max(r2)], r2 = max(r2)),3))

best_lam = lambda[which.max(r2)] # 2070

## alpha estimate
pareto_x <- log(1+apart$x/best_lam)
pareto_g <- apart$pareto
lm.pareto <- lm(pareto_x ~ pareto_g-1)
best_alpha <- 1/lm.pareto$coefficients[[1]] # 1.621283


# Q-Q plot
apart_qq <- cbind(xr = apart$lnx, stack(apart[,6:10]))
apart_qq[apart_qq$ind=="pareto","xr"] <- pareto_x

label = c(lognorm = "Lognormal", pareto="Pareto", weibull="Weibull", frechet = "Inverse Weibull", llogis = "Log-logistic")
ggplot(apart_qq,aes(x=values, y=xr)) + geom_point() + geom_smooth(method="lm", col="cornflowerblue", se=FALSE) + facet_wrap(~ind,labeller = labeller(ind=label),scales = "free", nrow=1) + ylab("X(r)") + xlab("quantile")

ggplot(apart_qq[apart_qq$ind=="pareto",], aes(x=values, y=xr))+geom_point()+geom_abline(intercept = 0, slope = 1/best_alpha)




# (2) 가장 적절한 확률 분포 찾기 

# r square check
est <- function(x, y){
  fit <- lm(y~x)
  rr <- summary(fit)$r.squared
  coef <- fit$coefficients
  return(list(coef = coef, r2=rr))
}

r2_total <- c()
for(i in c(6,8,9,10)){
  label <- colnames(apart)[i]
  r2_total[label] <- est(apart[,i], apart$lnx)$r2
}
r2_total["pareto"] <- summary(lm.pareto)$r.squared
round(r2_total,3)


## weibull과 pareto가 가장 적합


weibull_coef <- est(apart$weibull, apart$lnx)$coef
names(weibull_coef) <- c("mu", "sigma")
pareto_coef <- c("lambda"=best_lam, "alpha"=best_alpha)
round(weibull_coef,3)
round(pareto_coef,3)


# (3) 적정 보험료 계산 

# weibull pdf
## if d=TRUE, calculate pdf
## par1 = mu, par2 = sigma
f_weibull <-function(x, par1, par2, d=TRUE){
  a <- 1/par2 #shape
  b <- exp(par1) #scale
  if(d==TRUE){
    x*dweibull(x, a, b)}
  else{
   pweibull(x, a, b)}
}

# pareto pdf
## par1 = lambda, par2 = alpha
f_pareto <- function(x, par1, par2, d=TRUE){
  pdf <- par2*(par1^par2)*(par1+x)^(-par2-1)
  cdf <- 1-(par1/(par1+x))^par2
  if(d==TRUE){
    x*pdf
  }
  else{
    cdf
  }
}

# E(N) = 사고건수/계약건수
EN <- 617/5777

a <- rep(c(0,1000,5000),rep(5,3))
b <- rep(c(10000, 20000, 50000, 100000, 200000),3)
premium <- data.frame(A=a, B=b)


EY <- function(f, par1, par2, A, B){
  part1 <- integrate(f, A, (A+B), par1=par1, par2=par2, d=TRUE)
  part2 <- ifelse(B==Inf, 0, B*(1-f((A+B), par1=par1, par2=par2, d=FALSE)))
  part3 <- -A*(f((A+B), par1=par1, par2=par2, d=FALSE) - f(A, par1=par1, par2=par2, d=FALSE))
  return(part1$value+part2+part3)
}

for(i in 1:nrow(premium)){
  a <- premium$A[i]
  b <- premium$B[i]
  premium[i,"P1"] <- EN*EY(f_weibull, weibull_coef[["mu"]], weibull_coef[["sigma"]], A=a, B=b)
  premium[i,"P2"] <- EN*EY(f_pareto, pareto_coef[["lambda"]], pareto_coef[["alpha"]], A=a, B=b)
}

premium




# (4) 자기부담금 없고, 보상 한도 없는 보험의 적정보험료 


Pa<-c("weibull"=EN*EY(f_weibull, weibull_coef[["mu"]], weibull_coef[["sigma"]], A=0, B=Inf),
      "pareto"=EN*EY(f_pareto, pareto_coef[["lambda"]], pareto_coef[["alpha"]], A=0, B=Inf))

Pb <- 1883508/5777
c(Pa, Pb=Pb)






# (5) 


# 표 2-10(2005년~2009년)
premium5 <- data.frame(year = c(2005:2010),
                       total_n = c(12010, 11512, 6597, 3307, 4451, 5777),
                       event_n = c(1617, 2427, 1893, 579, 782, 617),
                       loss = c(c(8899, 11291, 10506, 4212, 8216)*10000,1883508) # 단위: 천원
                       )

## (가)                       
premium5["Pa_weibull"] <- (premium5$event_n/premium5$total_n)*EY(f_weibull, weibull_coef[["mu"]], weibull_coef[["sigma"]], A=0, B=Inf)
premium5["Pa_pareto"] <- (premium5$event_n/premium5$total_n)*EY(f_pareto, pareto_coef[["lambda"]], pareto_coef[["alpha"]], A=0, B=Inf)
premium5["Pb"] <- 1883508/5777


## (나)
premium5["r_Pa_weibull"] <- round(with(premium5, loss/(total_n*Pa_weibull)*100),1)
premium5["r_Pa_pareto"] <- round(with(premium5, loss/(total_n*Pa_pareto)*100),1)
premium5["r_Pb"] <- round(with(premium5, loss/(total_n*Pb)*100),1)

## (다)
premium5["r_actual"] <- c(83.3, 71.5, 68.5, 67, 118.4, NA)
premium5


x <- seq(0,10000,1)
fw <- c()
fp <- c()
for(i in 1:length(x)){
  fw[i] <- f_weibull(x[i],weibull_coef[["mu"]], weibull_coef[["sigma"]],d=FALSE)
  fp[i] <- f_pareto(x[i], pareto_coef[["lambda"]], pareto_coef[["alpha"]], d=FALSE)
}
df <- data.frame(x=x, weibull=fw, pareto=fp)
df <- cbind(x=df$x, stack(df[,2:3]))
ggplot(df)+geom_line(aes(x,values,col=ind),lwd=1)








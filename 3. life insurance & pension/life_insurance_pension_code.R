####  Part (1) : (생명표 만들기) 

# (1)
pop <- read.csv('data1_pop.csv')
death <- read.csv('data1_death.csv')

# pop 데이터 reshape 
pop <- melt(pop, id.vars=c('sex'), measure.vars = colnames(pop[,2:102]))
colnames(pop) <- c('sex', 'age', 'pop')
pop$age <- as.integer(substr(pop$age, 2,4))
death$age <- as.integer(death$age)

# 성별에 따라 데이터 분리
#to_sex <- function(data){
#  paste0('male_', data) <- data %>% filter(sex=='남자')
#  paste0('fem_', data) <- data %>% filter(sex=='여자')
#}
#to_sex(pop); to_sex(death)

male_pop <- pop %>% filter(sex=='남자'); fem_pop <- pop %>% filter(sex=='여자')
male_death <- death %>% filter(sex=='남자'); fem_death <- death %>% filter(sex=='여자')

# 성별에 따라 데이터 병합(변수: sex, age, pop, death)
male <- merge(male_pop, male_death, by=c('sex','age'))
male <- male[order(male$age),]
female <- merge(fem_pop, fem_death, by=c('sex','age'))
female <- female[order(female$age),]

# 변수 추가
male <- male %>% mutate(m = death/pop, q = m/(1+m/2), 
                        l = 100000*(cumprod(1-q)/(1-q)), 
                        L = (l+lead(l, 1))/2) %>% filter(age < 100) %>% 
                 mutate(e = revcumsum(L)/l)

female <- female %>% mutate(m = death/pop, q = m/(1+m/2), 
                        l = 100000*(cumprod(1-q)/(1-q)), 
                        L = (l+lead(l, 1))/2) %>% filter(age < 100) %>% 
                 mutate(e = revcumsum(L)/l)

life_tb <- cbind(female, male)
life_tb <- subset(life_tb, select = c(2, 7, 8, 6, 9, 16, 17, 15, 18))
life_tb


breaks = seq(-500000, 500000, 100000)
labels = paste0(as.character(c(seq(50, 0,-10), seq(10, 50,10))), '만')

pop_total <- rbind(fem_pop, male_pop)
ggplot(data=pop_total, aes(x=age, y=pop, fill=sex)) + 
  geom_bar(stat='identity') + scale_y_continuous(breaks=breaks, labels=labels) + 
  coord_flip() + ggtitle('남녀 연령별 인구수 추이')



# (2) 


table <- read.csv('data2_lifetb.csv')
colnames(table) <- c('age', 'e_m', 'e_f', 'q_m', 'q_f', 'l_m', 'l_f', 'L_m', 'L_f',
                     'y_m', 'y_f', 'd_m', 'd_f')
male_tb <- table %>% select('age', 'e_m', 'q_m', 'l_m', 'L_m', 'd_m')
fem_tb <- table %>% select('age', 'e_f', 'q_f', 'l_f', 'L_f', 'd_f')

# 생존자수(l)
gg_l <-  ggplot() + geom_line(data= fem_tb, aes(x=age, y=l_f, col='female'))+ geom_line(data= male_tb, aes(x=age, y=l_m, col='male')) + ggtitle('생존자수') + ylab('lx')
# 사망자수(d)
gg_d <- ggplot() + geom_line(data= fem_tb[-nrow(fem_tb),], aes(x=age, y=d_f, col='female'))+ geom_line(data= male_tb, aes(x=age, y=d_m, col='male')) + ggtitle('사망자수') + ylab('dx')
# 사망확률(q)
gg_q <- ggplot() + geom_line(data= fem_tb[-nrow(fem_tb),], aes(x=age, y=q_f, col='female'))+ geom_line(data= male_tb, aes(x=age, y=q_m, col='male')) + ggtitle('사망확률') + ylab('qx')
# 기대여명(e)
gg_e <- ggplot() + geom_line(data= fem_tb, aes(x=age, y=e_f, col='female'))+ geom_line(data= male_tb, aes(x=age, y=e_m, col='male')) + ggtitle('기대여명') + ylab('ex')

grid.arrange(gg_l, gg_d, gg_q, gg_e)



#### Part (2) : (생명표 의미 분석) 

# (a)
table <- read.csv('data2_lifetb.csv')
colnames(table) <- c('age', 'e_m', 'e_f', 'q_m', 'q_f', 'l_m', 'l_f', 'L_m', 'L_f',
                     'y_m', 'y_f', 'd_m', 'd_f')
male_tb <- table %>% select('age', 'e_m', 'q_m', 'l_m', 'L_m', 'd_m')
fem_tb <- table %>% select('age', 'e_f', 'q_f', 'l_f', 'L_f', 'd_f')

# 생존자수(l)
gg_l <-  ggplot() + geom_line(data= fem_tb, aes(x=age, y=l_f, col='female'))+ geom_line(data= male_tb, aes(x=age, y=l_m, col='male')) + ggtitle('생존자수') + ylab('lx')
# 사망자수(d)
gg_d <- ggplot() + geom_line(data= fem_tb[-nrow(fem_tb),], aes(x=age, y=d_f, col='female'))+ geom_line(data= male_tb, aes(x=age, y=d_m, col='male')) + ggtitle('사망자수') + ylab('dx')
# 사망확률(q)
gg_q <- ggplot() + geom_line(data= fem_tb[-nrow(fem_tb),], aes(x=age, y=q_f, col='female'))+ geom_line(data= male_tb, aes(x=age, y=q_m, col='male')) + ggtitle('사망확률') + ylab('qx')
# 기대여명(e)
gg_e <- ggplot() + geom_line(data= fem_tb, aes(x=age, y=e_f, col='female'))+ geom_line(data= male_tb, aes(x=age, y=e_m, col='male')) + ggtitle('기대여명') + ylab('ex')

grid.arrange(gg_l, gg_d, gg_q, gg_e)


# (b)
table <- table %>% mutate(q_ratio = q_m/q_f)

ggplot(table, aes(age, q_ratio)) + geom_line() + ggtitle('남녀 간 사망확률 비')


# (c)
male_tb <- male_tb %>% mutate(log_q = log(q_m), log_mort_rate = log(d_m/L_m))
fem_tb <- fem_tb %>% mutate(log_q = log(q_f), log_mort_rate = log(d_f/L_f))

# log-사망확률
gg1 <- ggplot() + geom_line(data= fem_tb[-nrow(fem_tb),], aes(x=age, y=log_q, col='female'))+ geom_line(data= male_tb, aes(x=age, y=log_q, col='male'), lwd=1) + ggtitle('log-사망확률') + ylab('') + theme(aspect.ratio = 1)

# log-사망율
gg2 <- ggplot() + geom_line(data= fem_tb[-nrow(fem_tb),], aes(x=age, y=log_mort_rate, col='female'))+ geom_line(data= male_tb, aes(x=age, y=log_mort_rate, col='male'), lwd=1) + ggtitle('log-사망율') + ylab('') + theme(aspect.ratio = 1)

grid.arrange(gg1, gg2, ncol=2)



# (d)
# log-사망확률(2001 vs 2010)
table_2001 <- read.csv('data2_lifetb2001.csv')
male_tb01 <- table_2001 %>% mutate(log_q = log(q_m))
fem_tb01 <- table_2001 %>% mutate(log_q = log(q_f))

ggplot() + geom_line(data= fem_tb01[-nrow(fem_tb01)], 
                     aes(x=age, y=log_q, col='female')) + 
  geom_line(data= male_tb01, aes(x=age, y=log_q, col='male')) + 
  geom_line(data= fem_tb, aes(x=age, y=log_q, col='female'), lwd=2) + 
  geom_line(data= male_tb, aes(x=age, y=log_q, col='male'), lwd=2) + 
  labs(y='log-사망확률', title='2001년 vs 2010년 log-사망확률', 
       subtitle='얇은선: 2001년 / 굵은선: 2010년')


# (e)
# 남성
male_tb_20 <- male_tb %>% mutate(m_m = d_m/L_m) %>% filter(age > 20)
male_lm <- lm(log(m_m) ~ age, male_tb_20)
a_m <- male_lm$coef[[1]]; b_m <- male_lm$coef[[2]]
male_tb_20 <- male_tb_20 %>% mutate(gom_m_m = exp(a_m + age*b_m))

# 여성
fem_tb_20 <- fem_tb %>% mutate(m_f = d_f/L_f) %>% filter(age > 20)
fem_lm <- lm(log(m_f) ~ age, fem_tb_20)
a_f <- fem_lm$coef[[1]]; b_f <- fem_lm$coef[[2]]
fem_tb_20 <- fem_tb_20 %>% mutate(gom_m_f = exp(a_f + age*b_f))

data.frame(sex=c('male', 'female'), a=c(a_m, a_f), b=c(b_m, b_f))
g1 <- ggplot(male_tb_20, aes(age, log(m_m))) + geom_line() + geom_smooth(method = 'lm', se=FALSE) + labs(x='log(m)', title='age vs log(m) : male')
g2 <- ggplot(fem_tb_20, aes(age, log(m_f))) + geom_line() + geom_smooth(method = 'lm', se=FALSE) + labs(x='log(m)', title='age vs log(m) : female')

grid.arrange(g1, g2, ncol=2)


# (f)
male_tb <- male_tb %>% mutate(Sx = l_m/100000)
fem_tb <- fem_tb %>% mutate(Sx = l_f/100000)
# Q-Q Plot
g1 <- ggplot(male_tb[male_tb$age > 40 & male_tb$age < 100,], aes(age, log(-log(Sx)))) + geom_line() + geom_smooth(method = 'lm', se=FALSE) + labs(title='Gompertz Q-Q Plot (male)')
g2 <- ggplot(fem_tb[fem_tb$age > 40 & fem_tb$age < 100,], aes(age, log(-log(Sx)))) + geom_line() + geom_smooth(method = 'lm', se=FALSE) + labs(title='Gompertz Q-Q Plot (female)')

grid.arrange(g1,g2,ncol=2)


# x > 40 직선이라고 할 수 있음(?)
fit1 <- lm(age ~ log(-log(Sx)), data=male_tb[male_tb$age > 40 & male_tb$age < 100,])
mu_m <- fit1$coef[[1]]; sigma_m <- fit1$coef[[2]]
b_m2 <- 1/sigma_m; a_m2 <- log(b_m2) - mu_m/sigma_m

fit2 <- lm(age ~ log(-log(Sx)), data=fem_tb[fem_tb$age > 40 & fem_tb$age < 100,])
mu_f <- fit2$coef[[1]]; sigma_f <- fit2$coef[[2]]
b_f2 <- 1/sigma_f; a_f2 <- log(b_f2) - mu_f/sigma_f

data.frame(sex=c('male', 'female'), mu=c(mu_m, mu_f), sigma=c(sigma_m, sigma_f))


#(g)
# 한계수명
fit1 <- lm(log_q ~ age, male_tb[male_tb$age > 20,])
a_m2 <- fit1$coef[[1]]; b_m2 <- fit1$coef[[2]]
x_bar_m <- round(-a_m2/b_m2)

fit2 <- lm(log_q ~ age, fem_tb[fem_tb$age > 20,])
a_f2 <- fit2$coef[[1]]; b_f2 <- fit2$coef[[2]]
x_bar_f <- round(-a_f2/b_f2)

c(a_m2, b_m2, a_f2, b_f2)
c('남성 한계수명: ', x_bar_m, '여성 한계수명: ', x_bar_f)

# 최대수명
max <- data.frame(n =c(100000, 500000, 1000000, 100000000, 6000000000))
max_m <- max %>% mutate(x_bar_m = mu_m + sigma_m*log(log(n+1)))
max_f <- max %>% mutate(x_bar_f = mu_f + sigma_f*log(log(n+1)))

data.frame('n' = max$n, 'x_bar(male)'= max_m$x_bar_m, 'x_bar(female)' = max_f$x_bar_f)






#### Part (3) : (생명보험료 계산) 

#(a)
df <- data.frame(age=seq(20, 65, by=5))

# 현재가 기대값 계산
r <- log(1.05)
v <- exp(-r)
male_tb <- male_tb %>% mutate(C = v^(age+1)*d_m, D = (v^age)*l_m, 
                                  M = revcumsum(C), A = M/D, 
                                  A_bar = A*(0.05/log(1.05)))
fem_tb <- fem_tb %>% mutate(C = v^(age+1)*d_f, D = (v^age)*l_f, 
                                  M = revcumsum(C), A = M/D, 
                                  A_bar = A*(0.05/log(1.05)))

a_df <- inner_join(df, male_tb[c('age', 'A_bar')], by='age') %>% left_join(.,  fem_tb[c('age', 'A_bar')], by='age')
colnames(a_df) <- c('age', 'male', 'female')
a_df


#(b)
m <- 12
i_m <- m*exp(r/m) - m
d_m <- m-m*exp(-r/m)

male_tb <- male_tb %>% mutate(A_m = A*(0.05/i_m),
                            a_m = (1-A_m)/(m*(1-exp(-r/m))))
fem_tb <- fem_tb %>% mutate(A_m = A*(0.05/i_m),
                            a_m = (1-A_m)/(m*(1-exp(-r/m))))

male_tb['a_m']; fem_tb['a_m']


#(c)
male_tb <- male_tb %>% mutate('premium(male)' = A_bar/ a_m * 100000000 * 1/12)
fem_tb <- fem_tb %>% mutate('premium(female)' = A_bar/ a_m * 100000000 * 1/12)

c_df <- inner_join(df, male_tb[c('age', 'premium(male)')], by='age') %>% left_join(.,  fem_tb[c('age', 'premium(female)')], by='age')

c_df













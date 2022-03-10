library(shiny)
library(shinydashboard)
library(tidyverse)
options("scipen" = 100)
library(readxl)
library(MASS)
library(survival)
library(randomForest)
library(data.table)
library(mltools)
library(xgboost)
library(nnet)
library(NeuralNetTools)
library(ROCR)
library(DT)
library(caret)
library(e1071)
library(kknn)
library(class)
library(gam)

header <- dashboardHeader(title = "Life Insurance Cancellation Probability")


sidebar <- dashboardSidebar(
  
  
  sidebarMenu(id="type",
              menuItem("Life Insurance Cancellation Probability", tabName = "acc", icon = icon("heart")),
              
              tags$hr(),
              
              fileInput('file', label=h6('Upload Data File'),
                        accept=c('text/csv', 
                                 'text/comma-separated-values,text/plain', 
                                 '.csv')),
              
              textInput("name", h3("Name"), value = "So"),
              tags$hr(),
              
              selectInput("a.method", h3("Analysis method"), 
                          choices = c("GLM","GAM","CoxPHM","LDA","KNN",
                                      "RandomForest","SVM","XGBoost","NeuralNetwork"))
              
  )
)



body <- dashboardBody(
  
  fluidRow(
    box(sliderInput("age", h3("Age"), value=40, min = 0, max = 100),
        sliderInput("period", h3("Payment period(year)"), value=20 ,min = 0,max = 100),
        sliderInput("final.number", h3("Final number of payments"), value=50, min = 0, max = 200),
        numericInput("premium",h4("Insuarance Premium"),value = 10000),
        radioButtons("renew", h3("Revival or Not"), 
                     choices = list("Yes"= 1,
                                    "No" = 0),  selected = 1),
        width = 4, height=650, status = "warning", solidHeader = T),
    
    
    box ( radioButtons("type1", h3("Product categories"), 
                       choices = list("Cancer"=1,
                                      "The sick"=2,
                                      "Newborn baby"=3,
                                      "Medical expenses"=4,
                                      "Regular"=5,
                                      "Lifetime"=0)),
          
          radioButtons("type2", h3("Product Subcategories"), 
                       choices = list("Renewal type"=1,
                                      "Non-renewal type"=2,
                                      "Dementia/care"=3,
                                      "Regular"=4,
                                      "Lifetime"=5 ,
                                      "Adult"=6,
                                      "Child"=7,
                                      "Infant child"=8,
                                      "Property"=9,
                                      "etc"=10)),
          
          width = 4, height=650, status = "warning", solidHeader = T),
    
    
    box ( dateInput("start_date", h3("Contract date"), value = "2001-01-01", min = NULL, max = NULL,
                    format = 'yyyy-mm-dd', startview = 'month', weekstart = 0),
          
          dateInput("end_date", h3("Expiration date"), value = "2020-11-01", min = NULL, max = NULL,
                    format = 'yyyy-mm-dd', startview = 'month', weekstart = 0),
          
          radioButtons("p.period", h3("Payment method"), 
                       choices = list("1 month"= 1,
                                      "3 month" = 2,
                                      "6 month"=3,
                                      "1 year" = 4),  selected = 1),
          radioButtons("c.method", h3("Collection Method"), 
                       choices = list("Visit"=1,
                                      "Automatic withdrawal"=2,
                                      "Giro"=3,
                                      "Direct"=4,
                                      "Card"=5)),
          
          width = 4, height=650, status = "warning", solidHeader = T)
    
    
    
    
  ),
  
  fluidRow(
    box(title = "Information of Customer", 
        column(11, align='center', tableOutput('table')),
        status = 'warning', width = 12, height=150, solidHeader = T
        
    )),
  
  fluidRow(valueBoxOutput("p")))








ui <- dashboardPage(header, sidebar, body)


## function
data <- read_xls("insuarance.xls",sheet=2)

## data setting
data.setting <- function(df){
  names(df) <- c('y','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11')
  data<-df%>% 
    mutate(x4=as.factor(x4),
           x6=as.factor(x6), x7=as.Date(x7,format = c("%Y%m%d")), x8=as.Date(x8,format = c("%Y%m%d")),
           x10=as.factor(x10), x11=as.factor(x11), y=as.factor(y))
  
  is.na(data$x8) -> na.list  
  data$x8[na.list] <- as.Date(gsub("0229", "0301", df$x8[na.list]),
                              format = c("%Y%m%d"))
  
  data %>% mutate(
    x2 = ifelse(x2=="1",1,ifelse(x2=="2",3,ifelse(x2=="3",6,12))),
    x14 = x9 * x2,      ##final payment period
    x15 = round(x14/(x3*12)*100),   ##rate
    x16 = ((12*year(x8)+month(x8)) - (12*2001+6)),   ##Expiration period
    x17 = floor(((12*2001+6)-(12*year(x7)+month(x7))+1)/x2) - x9,   ##Delinquency
    x5 = log(x5/x2)
    
  )  -> result
  
  result[result$x15==Inf, "x15"] <- 0
  result[result$x17<0, 'x17'] <- 0
  ind <- which(result$x16 > 9000)  
  result[ind, "x16"] <- with(result[ind,], (150-((2001-year(x7))+x1))*12)
  result[result$x15==100,'x17'] <-0
  result <- result %>% filter(x8>="2001-10-01")
  
  print(head(result))
  return(result)
}

data.setting(data) -> data 

data %>% dplyr::select(-x9,-x7,-x8) -> data
print(str(data))
fac.level <- list(x4=levels(data$x4), x6=levels(data$x6), 
                  x10=levels(data$x10), x11=levels(data$x11))


data.setting2 <- function(df){
  data<-df%>% 
    mutate(x4=factor(x4, levels = fac.level[[1]]), x6=factor(x6, levels = fac.level[[2]]), 
           x7=as.Date(x7,format = c("%Y-%m-%d")), x8=as.Date(x8,format = c("%Y-%m-%d")),
           x10=factor(x10, levels = fac.level[[3]]), x11=factor(x11, levels = fac.level[[4]]))
  
  data %>% mutate(
    x2 = ifelse(x2==1,1,ifelse(x2==2,3,ifelse(x2==3,6,12))),
    x14 = x9 * x2, 
    x15 = ifelse(round(x14/(x3*12)*100)==Inf, 0, round(x14/(x3*12)*100)),
    x16 = ((12*year(x8)+month(x8)) - (12*2001+6)),
    x17 = max(floor(((12*2001+6)-(12*year(x7)+month(x7))+1)/x2) - x9,0)
    
  )  -> result
  
  return(result)
}




# model fitting
# glm
glm_gompit <- glm(y ~ x1 + x4 + x6 + x10 + x14 + x15 + x16 + x17 + x4:x17 + x10:x17 + x14:x15 + x1:x4 + x14:x17 + x6:x15, 
                  family = binomial(link = cloglog), data = data)

# gam
gam <- gam(y ~ x1 + x3 + x5 + s(x15, 8) + s(x17, 4) , data, family=binomial)
print(summary(gam))
print(head(gam$fitted.values))
print(head(predict(gam, newdata=data, type = 'response')))
# cox
cox <- coxph(Surv(x14,as.numeric(as.character(y))) ~x1+x3+x5+x15, data = data)
print(summary(cox))
S <- basehaz(cox)
lm <- lm(log(hazard)~log(time), data = S)
alpha <- summary(lm)$coefficients[1]
beta <- summary(lm)$coefficients[2]

# lda
ld <- lda(y~., data=data)

# scaling data
coln <- which(colnames(data) %in% c("y","x4","x6","x10","x11"))
data.scale <- data
max.train <- apply(data[,-coln], 2, max)
min.train <- apply(data[,-coln], 2, min)
print("max")
print(max.train)
print("min")
print(min.train)
min_max <- function(x){ return((x-min(x))/(max(x)-min(x)))}
data.scale[,-coln] <- as.data.frame(lapply(data.scale[,-coln], min_max))

# knn data
data2 <- data.scale %>% mutate(y=ifelse(y==1, "yes", "no"))
best_k <- 9
set.seed(123)
kn_fit <- knn(train=data2[,-1], cl=data2$y, test=data2[,-1], k=best_k, prob=TRUE)

# svm
svm_fit <- svm(y~., data=data, gamma=0.1, cost=5, probability=TRUE)
print(summary(svm_fit))


# one-hot encoding
data.one<-one_hot(as.data.table(data[,-1]))
data.xgb <- xgb.DMatrix(data=as.matrix(data.one),label=as.numeric(as.character(data$y)))
# xgb
xgb_model =xgb.train(
  data=data.xgb,
  max.depth = 5, eta = 0.01, nthread = 2, nrounds = 2, objective = "binary:logistic",
  subsample = 0.8, min_child_weight = 1, verbose=0
)


# rf
rf_model <- randomForest(y ~ ., data=data, mtry = floor(sqrt(7)), ntree = 500, importance = T)

# nn
nn_model <- nnet(y ~. , data=data.scale, size = 2, decay = 5e-04)



p.function<-function(newdata){
  coln <- which(colnames(newdata) %in% c("x4","x6","x10","x11"))
  newdata.scale <- newdata
  newdata.scale[,-coln] <- (newdata.scale[,-coln]-min.train)/(max.train-min.train)
  
  newdata.xgb <- one_hot(as.data.table(rbind(newdata, data[,-1])))
  newdata.xgb <- newdata.xgb[1,]
  newdata.xgb <- as.matrix(newdata.xgb)
  
  # glm
  glm_pred <- predict(glm_gompit, newdata=newdata, type = 'response')
  print(glm_pred)
  
  # gam
  print(predict(gam, newdata=head(data), type="response"))
  gam_pred <- predict(gam, newdata=newdata, type = 'response')
  print(gam_pred)
  
  # cox
  Ht <- exp(alpha+beta*log(newdata$x14))
  Ht.delta <- exp(alpha+beta*log((newdata$x14+3)))
  St<- exp(-Ht)
  St.delta<-exp(-Ht.delta)
  mu.cox <- exp(predict(cox, newdata, type = 'lp'))
  cox_pred <- 1-(St.delta/St)^mu.cox
  print(paste("cox: ",cox_pred))
  
  # lda
  ld_hat <- predict(ld, newdata)
  lda_pred <- ld_hat$posterior[,2]
  print(lda_pred)
  
  # knn
  set.seed(123)
  kn_fit <- knn(train=data2[,-1], cl=data2$y, test=newdata.scale, k=best_k, prob=TRUE)
  knn_pred <- 1-attr(kn_fit,"prob")
  print(paste("knn: ",knn_pred))
  
  # svm
  svm_hat <- predict(svm_fit, newdata, probability = TRUE)
  svm_pred <- attr(svm_hat, "prob")[,2]
  print(paste("svm: ", svm_pred))
  
  #xgboost
  xgb_pred = predict(xgb_model, newdata = newdata.xgb, reshape=T)
  
  #rf
  rf_pred <- predict(rf_model, newdata, type="prob")[,2]
  print(paste("rf: ",rf_pred))
  
  #nn
  nn_pred<-predict(nn_model,newdata=newdata.scale)
  print(paste("nn: ", nn_pred))
  
  
  p <- list(GLM=glm_pred, GAM=gam_pred, CoxPHM=cox_pred, 
            LDA=lda_pred, KNN=knn_pred, SVM=svm_pred, 
            RandomForest=rf_pred, NeuralNetwork=nn_pred, XGBoost=xgb_pred)
  return(p)
}


server <- function(input, output, session){
  out <- reactive({
    df<-data.frame(x1 = input$age, x2 = input$p.period,x3=input$period,x4=input$c.method,
                   x5=input$premium, x6=input$renew, x7=input$start_date,
                   x8=input$end_date, x9=input$final.number, x10=input$type1, x11=input$type2)
    data.setting2(df) -> df 
    
    df %>% dplyr::select(-x9,-x7,-x8) -> df2
    print(df2)
    
    res <- p.function(df2)
    return(list(data=df, p=res))
  }
  )
  
  
  
  output$table <- renderTable({
    df<-data.frame(name = input$name,
                   x1 = input$age, x2 = paste0(ifelse(input$p.period==4,12,ifelse(input$p.period == 3,6,ifelse(input$p.period == 2,3,1)))," month"), x3=paste0(input$period, " year"),
                   x4=ifelse(input$c.method==1,"Visit",ifelse(input$c.method==2,"Automatic withdrawal",ifelse(input$c.method==3,"Giro",ifelse(input$c.method==4,"Direct","Card")
                   ))), x5=input$premium, x6=ifelse(input$renew==1,"Yes","No"),
                   x7=as.character(input$start_date),
                   x8=as.character(input$end_date),
                   x9=input$final.number, x10=ifelse(input$type1==1,"Cancer",ifelse(input$type1==2,"The sick",ifelse(input$type1==3,"Newborn baby",
                                                                                                                     ifelse(input$type1 ==4, "Medical expenses",
                                                                                                                            ifelse(input$type1 ==5,"Regular", "Lifetime"))))),
                   
                   x11=ifelse(input$type2==1,"Renewal type", ifelse(input$type2==2,"Non-renewal type",
                                                                    ifelse(input$type2==3,"Dementia/care",
                                                                           ifelse(input$type2==4,"Regular",
                                                                                  ifelse(input$type2==5,"LifeTime", ifelse(input$type2==6,"Adult",
                                                                                                                           ifelse(input$type2==8,"Infant_Child",ifelse(input$type2==9,"Property",
                                                                                                                                                                       ifelse(input$type2==7,"Child","etc"))) ))))))
    )
    colnames(df)<-c("Name","Age",'Payment Term','Payment period','Collection method','Insuarance Premium','Revival','Contract date','Expiration date',
                    'Final number of payments','Product categories','Product Subcategories')
    
    print(df)
  })
  
  output$p <- renderValueBox({
    valueBox({out <- out()
    round(out$p[[input$a.method]],3)},
    paste0(input$name,"'s Cancellation Probability"),color = "red"
    )})
  
}


shinyApp(ui=ui, server = server)

library(shiny)
library(shinydashboard)
library(tidyverse)
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

header <- dashboardHeader(title = "Corporation bankruptcy")


sidebar <- dashboardSidebar(
    
    
    sidebarMenu(id="type",
                
                fileInput('file', label=h4('Upload Data File'),
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                textInput("name", h3("Name"), value = "Samsung"),
                selectInput("method", h3("Analysis method"), 
                            choices = c("GLM","GAM","CoxPHM","LDA","KNN",
                                        "RandomForest","SVM","NeuralNetwork")),
                numericInput("x40",h3("Corporate age"),value = 5000),
                radioButtons("x43", h3("Business type"), 
                             choices = list("light industry"= 'a',
                                            "Heavy industry" = 'b',
                                            "Construction industry"='c',
                                            "Wholesale and Retail"='d',
                                            "Service"='e'),  selected = 'a'),
                radioButtons("x44", h3("Scale"), 
                             choices = list("External audit"= 'a',
                                            "Non-External audit1" = 'b',
                                            "Non-External audit2"='c',
                                            "SOHO"='d',
                                            "individual"='e'),  selected = 'a')
                
    ))





body <- dashboardBody(
    fluidRow(valueBoxOutput("p",width=6), valueBoxOutput("z.score",width=6)),
    fluidRow(
        tabBox(
            tabPanel("Productivity and growth",
                     numericInput("x21", h4("Total capital investment efficiency"), value=40),
                     numericInput("x22", h4("Sales receivables growth rate"), value=10 ),
                     numericInput("x23", h4("Inventory asset growth rate"), value=10)
            ),
            tabPanel("Profitability",
                     numericInput("x24", h4("Net return on management capital"), value=5),
                     numericInput("x25", h4("Financial ratio/Total debt ratio"), value=5),
                     numericInput("x1", h4("Financial ratio/Total cost ratio"), value=5),
                     numericInput("x26", h4("Net return on equity capital"), value=10),
                     numericInput("x27", h4("Net return on capital"), value=20),
                     numericInput("x28", h4("Total Capital Current Return Rate"), value=5),
                     numericInput("x29", h4("Gross return on capital"), value=5),
                     numericInput("x30", h4("Total capital operating profit ratio"), value=30),
                     numericInput("x31", h4("Total return on asset business"), value=5)
            ),
            tabPanel("Stability-1",
                     numericInput("x2", h4("Fixed debt ratio"), value=30),
                     numericInput("x3", h4("Fixed ratio"), value=100),
                     numericInput("x4", h4("Debt ratio"), value=200),
                     numericInput("x5", h4("Total Debt/Total Asset Ratio"), value=50),
                     numericInput("x6", h4("Net debt/total asset ratio"), value=50),
                     numericInput("x7", h4("Current debt ratio"), value=100)),
            tabPanel("Stability-2",
                     numericInput("x8", h4("Flow ratio"), value=100),
                     numericInput("x9", h4("Reserved amount/total asset ratio"), value=60),
                     numericInput("x10", h4("Equity capital ratio"), value=50),
                     numericInput("x11", h4("Depend on borrowings"), value=50),
                     numericInput("x12", h4("Fixed assets/borrowing ratio"), value=100),
                     numericInput("x13", h4("Loan/ Equity Capital Ratio"), value=100)),
            
            
            width=4),
        tabBox(
            tabPanel("Debt repayment ability",
                     numericInput("x14", h4("Fixed financial expenses compensation ratio"), value=5),
                     numericInput("x15", h4("Total loan/(total loan + equity capital) ratio"), value=50),
                     numericInput("x16", h4("Total CF/Borrowing ratio"), value=20),
                     numericInput("x17", h4("CF/borrowing ratio"), value=5)
            ),
            tabPanel("Liquidity",
                     numericInput("x18", h4("Net operating capital/total capital ratio"), value=50),
                     numericInput("x19", h4("Current debt composition ratio"), value=30),
                     numericInput("x20", h4("Cash ratio"), value=5)
            ),
            tabPanel("Exchangeability",
                     numericInput("x32", h4("Turnover rate of business capital"), value=1),
                     numericInput("x33", h4("Turnover rate of Fixed asset"), value=5),
                     numericInput("x34", h4("Turnover rate of purchase debt"), value=2000),
                     numericInput("x35", h4("Turnover rate of accounts receivable"), value=10),
                     numericInput("x36", h4("Turnover rate of equity capital"), value=5),
                     numericInput("x37", h4("Turnover rate of capital"), value=5),
                     numericInput("x38", h4("Turnover rate of Inventory asset"), value=20),
                     numericInput("x39", h4("Turnover rate of Total capital"), value=5)
            ),
            tabPanel("Additional",
                     numericInput("x41",h4("log(Sales)"),value = 10),
                     numericInput("x42",h4("log(Asset)"),value = 10)),
            
            width=4),
  
            box(
                title = "Probability of bankruptcy",plotOutput('Plot.p'),
                plotOutput('Plot.p2'), solidHeader=T ,width = 4,height=700)
         
    )
)


ui <- dashboardPage(header, sidebar, body , skin = "purple")

data <- read.csv("bankruptcy_data.csv")

train<-data[1:3168,]
#test<-data[3169:6336,-c(46,47)]

# 결측치(9999.99) NA로 대체
## train
for (i in 2:43){
    train[[i]] <- replace(train[[i]], train[[i]]==9999.99, NA)
    train[[i]] <- replace(train[[i]], train[[i]]==-9999.99, NA)
}


outlier <- read_excel('halfnorm_result.xlsx', sheet=2, col_names = F)
outlier <- c(outlier)$...1
train <- train %>% filter(!(ID %in% outlier))


# outlier drop
ind <- c( which(train$x2>20000), which(train$x4>100000),
          which(train$x7>50000), which(train$x8>500000), which(train$x12>200000), which(train$x13>50000), which(train$x15>30000), which(train$x16>30000), which(train$x17>300000), which(train$x20>50000),
          which(train$x22>500000000), which(train$x23>2000000000), which(train$x27>50000), which(train$x37>4000))

train <- train[-ind,]

# drop variables with na more than 500
## 34,35,38
na <- apply(train, 2, function(x) sum(is.na(x)))
train <- train %>% dplyr::select(-names(na[na>500]))

# train 변수 변환
train$x40 <- 1/train$x40

# factor 변환
train[,c('x43','x44')] <- lapply(train[,c('x43','x44','delta')], factor)

x43_level <- levels(train$x43)
x44_level <- levels(train$x44)

# 다중공선성 있는 변수 제거
# x2, 3, 4, 5, 10, 28, 30, 32 삭제
drop_var <- which(colnames(train) %in% c('x2', 'x3', 'x4', 'x5', 'x10', 'x28', 'x30', 'x32'))
train <- train[,-drop_var]

print(head(train))

train_median <- apply(train[,2:32], 2, median, na.rm=TRUE)

# na값 median 대체
for (i in 2:32){
    train[[i]][is.na(train[[i]])] <- median(train[[i]],na.rm=TRUE)
}

#train <- train[,-1]

### test 변수 전처리

# 변수 drop
make.test <- function(test) {
    
    drop_var <- which(colnames(test) %in% c('x2', 'x3', 'x4', 'x5', 'x10', 'x28', 'x30', 'x32', 'x34', 'x35', 'x38'))
    test <- test[,-drop_var]
    
    # missing value imputation with median(train data)
    
    # for (i in 2:32){
    #     test[[i]][is.na(test[[i]])] <- train_median[colnames(test[,i])]
    # }
    # 
    
    # log 변환
    for (i in c(1, 7, 8, 12, 13, 15, 20, 33, 36, 37, 39)){
        name <- paste0("x",i)
        test[name] <- log(test[[name]]+1)
    }
    
    # sqrt 변환
    for(i in c(11, 19)){
        name <- paste0("x",i)
        test[name] <- sqrt(test[[name]])
    }
    
    # inverse 변환
    test$x40 <- 1/(test$x40)
    
    # factor 변환
    test$x43 <- factor(test$x43, levels = x43_level)
    test$x44 <- factor(test$x44, levels = x44_level)
    
    return(test)
}


#glm
train_data <- train[,2:35]
glm_fit<-glm(delta ~ x1 + x7 + x12 + x18 + x20 + x22 + x23 + 
                 x24 + x26 + x29 + x33 + x40 + x41 + x42 + x44, family = binomial(link = "logit"),
             data = train_data)

#gam
gam_fit<-gam(delta ~ x6 + s(x7, 8) + x8 + x12 + x14 + x18 + 
                 x19 + s(x20, 4) + s(x21, 4) + x22 + x24 + s(x25, 4) + x26 + 
                 s(x27, 4) + x29 + x33 + s(x39, 8) + s(x40, 8) + s(x41, 8) + 
                 x44, family = binomial, data = train_data)

#cox
train$delta <- as.numeric(as.character(train$delta))
#train$y <- as.numeric(as.character(train$y))
train.mean <- apply(train[,2:32], 2, mean)
train.c <-cbind(data.frame(scale(train[,2:32],scale=F)),train[,-(2:32)])

cox.model <- coxph(Surv(y, delta) ~  x1 + x7 + x12 + x18 + x20 + 
                       x22 + x23 + x24 + x25 + x26 + x29 + x33 + x40 + x41 + x42 + 
                       x44, data = train.c)

S <- basehaz(cox.model)
lm <- lm(log(hazard)~time, data = S)
alpha <- summary(lm)$coefficients[1]
beta <- summary(lm)$coefficients[2]
delta.t <- 365
Ht <- exp(alpha+beta*0)
Ht.delta <- exp(alpha+beta*(0+delta.t))
St<- exp(-Ht)
St.delta<-exp(-Ht.delta)

#lda
ld <- lda(delta~., data=train_data, cv=TRUE)

#knn
## train data -> min-max scaling
train_min <- apply(train_data[,1:31], 2, min)
train_max <- apply(train_data[,1:31], 2, max)
train_mm <- train_data
for(i in 1:31){
    train_mm[,i] <- (train_mm[,i]-train_min[i])/(train_max[i]-train_min[i])
}
## encoding
train_mm$delta <- ifelse(train_mm$delta==1, "yes", "no")
control <- trainControl(method="cv",
                        number=10,
                        classProbs = TRUE)
set.seed(2021)
kn <- train(delta~., data=train_mm, method="knn", trControl=control)
best_k <- kn$bestTune

#svm
svm_fit <- svm(delta~., data=train_data, gamma=0.001, cost=3, probability=TRUE)

#rf
rf_model <- randomForest(delta ~ ., data=train_data, mtry = floor(sqrt(34)), ntree = 500, importance = T)

#nn
nn_model <- nnet(delta ~. , data=train_data, size = 5, decay = 5e-04, na.action = na.omit)


p.function<-function(newdata){
    coln <- which(!(colnames(newdata) %in% c("x43","x44")))
    #print(coln)
    newdata.scale <- newdata
    newdata.scale[,coln] <- (newdata.scale[,coln]-train_min)/(train_max-train_min)
    newdata.c <- newdata
    newdata.c[,coln] <- (newdata.c[,coln]-train.mean)
    
    print(newdata.scale)
    
    #glm
    glm_pred <- predict(glm_fit, newdata = newdata , type = 'response')
    
    #gam
    gam_pred <- predict(gam_fit, newdata = newdata, type = 'response')
    
    #cox
    mu.cox <- predict(cox.model, newdata.c, type = 'risk')
    cox_pred <- 1-(St.delta/St)^mu.cox
    
    #lda
    lda_pred<- predict(ld,newdata=newdata)$posterior[,2]
    
    #knn
    set.seed(2021)
    kn_fit <- knn(train=train_mm[,-c(32:34)], cl=train_mm$delta, test=newdata.scale[,-c(32,33)], k=best_k, prob=TRUE)
    knn_pred <- 1-attr(kn_fit,"prob")
    
    #svm
    svm_pred <- predict(svm_fit, newdata, probability = TRUE)
    
    #rf
    rf_pred <- predict(rf_model, newdata)
    
    #nn
    nn_pred <- predict(nn_model, newdata)[,1]
    
    p <- list(GLM=glm_pred, GAM=gam_pred, CoxPHM=cox_pred, 
              LDA=lda_pred, KNN=knn_pred, SVM=svm_pred, 
              RandomForest=rf_pred, NeuralNetwork=nn_pred)
    return(p)
}


server <- function(input, output, session){
    out <- reactive({
        df<-data.frame(x1 = input$x1, x2 = input$x2,x3=input$x3,x4=input$x4,
                       x5=input$x5, x6=input$x6, x7=input$x7,
                       x8=input$x8, x9=input$x9, x10=input$x10, x11=input$x11,x12=input$x12,x13=input$x13,x14=input$x14,x15=input$x15,
                       x16=input$x16,x17=input$x17,x18=input$x18,x19=input$x19,x20=input$x20,x21=input$x21,x22=input$x22,x23=input$x23,
                       x24=input$x24,x25=input$x25,x26=input$x26,x27=input$x27,x28=input$x28,x29=input$x29,x30=input$x30,x31=input$x31,
                       x32=input$x32,x33=input$x33,x34=input$x34,x35=input$x35,x36=input$x36,x37=input$x37,x38=input$x38,x39=input$x39,
                       x40=input$x40,x41=input$x41,x42=input$x42,x43=input$x43,x44=input$x44)
        print(df)
        df <- make.test(df)
        print(df)
        res <- p.function(df)
        score <- 1.2*input$x18 + 1.4*input$x9 + 3.3*input$x30 + 0.6*input$x25 + 1 * input$x39
        score <- score/100
        return(list(data=df, p=res,z=score))
    }
    )
    
    output$Plot.p <- renderPlot({
        out<-out()
        p <-round(out$p[[input$method]],3)
        df1 <- data.frame( group = c("p" , "1-p"), value = c(p, 1-p) )
        bp<- ggplot(df1, aes(x=2 , y=value, fill=group))+ geom_bar(width = 1, stat = "identity") 
        pie <- bp +  scale_fill_manual(values = c("lightsteelblue2", "lightcoral"))+ coord_polar("y", start=0) +
            theme_void()+xlim(0.1,2.5)  + theme(legend.position = "none")+ ggtitle(paste0(p*100,"%")) +theme(plot.title = element_text(size = 30, face = "bold",hjust = 0.5))
        print(pie)
    }, height = 200)
    output$p <- renderValueBox({
        valueBox({out <- out()
        round(out$p[[input$method]],3)},
        paste0(input$name,"'s Bankruptcy Probability"),color = "purple",icon = icon("percentage")
        )})
    output$z.score <- renderValueBox({
        valueBox({out <- out()
        round(out$z,3)},
        paste0(input$name,"'s Altman Z score"),color = "purple",icon = icon("building")
        )})

    output$Plot.p2 <- renderPlot({
      out<-out()
      z <-round(out$z,3)
      df1 <- data.frame( group = c("z" , "1-z"), value = c(min(z,10), max(10-z,0)) )
      title <- ifelse(z < 1.81, "Distress Zone", ifelse(z < 2.99, "Grey Zone", "Safe Zone"))
      bp<- ggplot(df1, aes(x=2 , y=value, fill=group))+ geom_bar(width = 1, stat = "identity") 
      pie <- bp +  scale_fill_manual(values = c("lightsteelblue2", "lightcoral"))+ coord_polar("y", start=0) +
        theme_void()+xlim(0.1,2.5)  + theme(legend.position = "none")+ ggtitle(title) +theme(plot.title = element_text(size = 30, face = "bold",hjust = 0.5))
      print(pie)
    }, height = 200 )
    
}


shinyApp(ui=ui, server = server)
library(shiny)
library(flexdashboard)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(ggplot2)
library(kableExtra)
library(reshape)
library(ggpubr)
library(gridExtra)
library(MASS)
library(survival)
library(survminer)
library(ggfortify)

header <- dashboardHeader(title = "Heart disease attack predicion")


sidebar <- dashboardSidebar(
    
    
    sidebarMenu(id="type",
                
                fileInput('file', label=h4('Upload Data File'),
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                textInput("name", h3("Name"), value = "Kim Ewha"),
                sliderInput("age", h3("Age in years"), value=30,min=0,max=100 ),
                radioButtons("sex", h3("Gender"), choices=list("male"=1,
                                                               'female'=0)),
                
                numericInput("height",h3("Height(cm)"),value=160),
                numericInput("weight",h3("Weight(kg)"),value=50),
                numericInput("month",h3("Month"),value=6)
                
                
    ))





body <- dashboardBody(
    
    # fluidRow(valueBoxOutput("p.cox",width=6),
    #          valueBoxOutput("p.alt",width=6)),
    
    fluidRow(box(column (flexdashboard::gaugeOutput("p.chart") ,width = 6),
                 column(flexdashboard::gaugeOutput("p.chart2"),width = 6),
                 title="Heartdisease Attack Probability",solidHeader = T ,status = "primary", height = 250 ,width = 12 )
    ),
    
    fluidRow(
        box(
            sliderInput("sbp", h3("Systolic blood pressure"), value=100, min = 0, max = 300),
            sliderInput("dbp", h3("Diastolic blood pressure"), value=80,min=0,max=300),
            sliderInput("scl", h3("Serum cholesterol"), value=200,min=0,max=600),height = 500, width = 7
        ) ,
        valueBoxOutput("bmi",width=5),valueBoxOutput("health",width=5),
        valueBoxOutput("text",width=5),valueBoxOutput('chol',width=5)
        
    ),
    
    
    # fluidRow(valueBoxOutput("bmi",width=3),
    #          valueBoxOutput("health",width=3)),
    # 
    # fluidRow(valueBoxOutput("text",width=3),
    #          valueBoxOutput('chol',width=3))
    # 
    
)


ui <- dashboardPage(header, sidebar, body , skin = "black")

df <- read_excel("Framingham.xls",sheet = 1 )

data.setting<-function(df){
    
    df <- data.frame(df)
    df <- na.omit(df)
    
    df$log_scl <- log(df$scl)
    df$log_sbp <- log(df$sbp)
    df$log_bmi <- log(df$bmi)
    df$log_dbp <- log(df$dbp) 
    
    df$sex <- as.factor(df$sex)
    df <- df %>%dplyr::select(-sbp ,- scl , - bmi , - dbp)
    out <- c(3360, 956, 1225, 1906, 3329, 3435, 612) 
    df <- df[-out,]
    df$age <- cut(df$age, breaks= c(0, 40, 50,60, 100) ,right = FALSE , 
                  labels = c( "30ages","40ages","50ages","60ages"))
    
    return(df)
}


df <-data.setting(df)

alt<-survreg(formula = Surv(followup, chdfate) ~ month + sex + log_scl + 
                 log_sbp + log_bmi + log_dbp + age + log_scl:log_sbp + 
                 log_scl:log_dbp + log_scl:age + log_sbp:log_dbp + log_sbp:age + 
                 log_bmi:log_dbp + log_dbp:age, data = df, dist = "weibull")


sigma.alt <- alt$scale


cox<-coxph(Surv(followup, chdfate) ~ log_scl + log_dbp + log_scl + age + log_sbp + 
               log_bmi + sex + log_sbp:log_dbp + log_sbp:log_scl + log_sbp:age + log_dbp:log_scl + log_dbp:age + 
               log_dbp:log_bmi + log_scl:age, data = df)


S <- basehaz(cox)
lm <- lm(log(hazard)~log(time), data = S)
alpha <- summary(lm)$coefficients[1]
beta <- summary(lm)$coefficients[2]

p.function<-function(newdata){
    mu.alt <- predict(alt,type = "lp",newdata=newdata)
    u.alt <- (log(3650)-mu.alt)/sigma.alt
    alt.p<-(1-exp(-exp(u.alt)))
    
    mu.cox <- predict(cox,type = 'risk',newdata=newdata)
    cox.p<-1-exp(-exp((alpha+beta*(log(3650))))*mu.cox)
    p<-list(ALT=alt.p,CoxPHM=cox.p)
    return(p)
}




server <- function(input, output, session){
    out <- reactive({
        df<-data.frame(sbp = input$sbp, dbp = input$dbp,age=input$age,scl=input$scl,
                       bmi=input$weight/((input$height)*0.01)^2, sex=input$sex, month=input$month)
        df2 <- data.setting(df)
        print(df)
        res <- p.function(df2)
        bmi<-df$bmi
        scl<-df$scl
        dbp<-df$dbp
        sbp<-df$sbp
        return(list(data=df, p.cox=res$CoxPHM, p.alt = res$ALT ,bmi=bmi,scl=scl,dbp=dbp,sbp=sbp))
    }
    )
    
    output$p.chart <- renderGauge({
        out <- out()
        rate <- round(out$p.alt,3)*100
        gauge(as.numeric(rate) ,min = 0, max = 100, symbol = '%',  label= "ALT",
              gaugeSectors(
                  success = c(0,30), warning = c(30,60), danger = c(60,100)
              )) 
    })
    
    output$p.chart2 <-renderGauge({
        gauge(as.numeric(round(out()$p.cox,3)*100) ,min = 0, max = 100, symbol = '%', label= "CoxPHM",
              gaugeSectors(
                  success = c(0,30), warning = c(30,60), danger = c(60,100)
              )) 
    })
    
    
    output$bmi <- renderValueBox({
        valueBox({out <- out()
        round(out$bmi,1)},
        paste0(input$name,"'s BMI"),color = "blue")
    })
    
    output$health <- renderValueBox({
        out<-out()
        if ((out$bmi)<=18.5){
            mycolor="blue"
            myicon = icon('exclamation')
            
        }
        else if (18.5<(out$bmi)&(out$bmi)<25){
            mycolor="green"
            myicon = icon('smile-wink')
        }
        else if (25<=(out$bmi)&(out$bmi)<30){
            mycolor="orange"
            myicon = icon("exclamation-triangle")
        }
        else{
            mycolor="red"
            myicon = icon("bomb")
        }
        valueBox({out <- out()
        if ((out$bmi)<=18.5){
            print("UNDERWEIGHT!!")
            
            
        }
        else if (18.5<(out$bmi)&(out$bmi)<25){
            print("Healthy")
            
        }
        else if (25<=(out$bmi)&(out$bmi)<30){
            print("Overweight")
            
        }
        else{
            print("OBESE!!")
        }
        
        },h4("BMI Condition"),color = mycolor,icon = myicon)
    })
    output$text<-renderValueBox({
        out<-out()
        if (out$sbp >= 180 &out$dbp >=120){
            mycolor="red"
            myicon = icon("bomb")
        }
        else if (out$sbp>=130&out$sbp<180 | out$dbp<120&out$dbp > 80){
            mycolor="orange"
            myicon = icon('exclamation-triangle')
        }
        else{
            mycolor="green"
            myicon = icon('smile-wink')
        }
        
        valueBox({out<-out()
        if (out$sbp >= 180 &out$dbp >=120){
            print("DANGER!!")
        }
        else if (out$sbp>=130&out$sbp<180 | out$dbp<120&out$dbp > 80){
            print("High")
        }
        else {
            print("Healthy")
        }
        
        
        },h4("Blood Pressure Conditoin"),color=mycolor,icon = myicon)
    })
    output$chol<-renderValueBox({
        out<-out()
        if (out$scl >= 240){
            mycolor="red"
            myicon = icon("bomb")
        }
        else if (out$scl>=200&out$scl<240){
            mycolor="orange"
            myicon = icon('exclamation-triangle')
        }
        else{
            mycolor="green"
            myicon = icon('smile-wink')
        }
        
        valueBox({out<-out()
        if (out$scl >= 240){
            print("DANGER!!")
        }
        else if (out$scl>=200&out$scl<240){
            print("Caution")
        }
        else {
            print("Healthy")
        }
        
        
        },h4("Cholesterol Conditoin"),color=mycolor,icon = myicon )
    })
    
    
    
    
    
}






shinyApp(ui=ui, server = server)
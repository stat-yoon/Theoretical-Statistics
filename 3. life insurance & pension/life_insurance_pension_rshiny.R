library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
options(scipen=999)

header <- dashboardHeader(title = "Insuarance Premium")

sidebar <- dashboardSidebar(
    
    sidebarMenu(id="type",
                menuItem("Life insuarance", tabName = "life", icon = icon("heartbeat")),
                menuItem("Annuity insuarance", tabName = "annuity", icon = icon("money-check")))
    
)


body <- dashboardBody(
    
    fluidRow(
        valueBoxOutput("b", width = 6),
        valueBoxOutput("px", width = 6)),
    
    fluidRow(
        box(title = "Input",
            radioButtons("sex", label = h3("Sex"),
                         choices = list("Male"="Male","Female"="Female"),selected = 'Female'),
            sliderInput("x", h3("Age"), min = 0, max = 120, value=30, step=1),
            numericInput("i", h3("Annual Interst Rate"), value=0.05),
            numericInput("n", h3("Payment Expiration"), value=35),
            numericInput("m", h3("Pay Expiration"), value=35),
            numericInput("b", h3("Benefit"), value=10000), width = 6, solidHeader = T, status = "primary"),
        
        box(title = "life-table", tableOutput('df'), width = 6, status = "danger", solidHeader = T)
))

ui <- dashboardPage(header, sidebar, body, skin="purple")


data<-read.csv("./2010.csv",
               col.names = c("x","ex.m","ex.f","qx.m","qx.f","lx.m","lx.f", "Lx.m","Lx.f","dx.m","dx.f"))
pv.life<-function(data,x,i,sex,n,m,b){
    
    r<-log(1+i)
    if(sex=="Male"){
        lx<-data$lx.m
    } else{
        lx<-data$lx.f
    }
    a <- c()
    for (j in 1:m){
        a[j]<-(lx[x+1+j]/lx[x+1])*exp(-r*j)
        a.sum<-sum(a)
    }
    Ax.bar<-1-r*((1/2)+a.sum+(lx[x+1+m]/lx[x+1])*exp(-r*(m+1))/(1-exp(-r)))
    
    c <- c()
    for (j in 1:n-1){
        c[j]<-(lx[x+1+j]/lx[x+1])*exp(-r*j)
        c.sum<-sum(c)
    }
    ax<-(1/2)+c.sum+(1/2)*(lx[x+1+n]/lx[x+1])*exp(-r*n)
    value<-(Ax.bar/ax)
    
    px<-b*value/12
    
    return(px)
}

pv.annual<-function(data,x,i,sex,n,m,b){
    r<-log(1+i)
    a<-c()
    c<-c()
    if(sex=="Male"){
        lx<-data$lx.m
    } else{
        lx<-data$lx.f
    }
    for (j in (m+1):100){
        a[j]<-(lx[x+1+j]/lx[x+1])*exp(-r*j)
        a.sum<-sum(a,na.rm = T)
        
    }
    Ax.bar<-(1/2)*(lx[x+1+m]/lx[x+1])*(exp(-r*m))+a.sum
    
    for (j in 1:n-1){
        c[j]<-((lx[x+1+j]/lx[x+1])*exp(-r*j))
        c.sum<-sum(c)
    }
    ax<-1/2+c.sum+(1/2)*(lx[x+1+n]/lx[x+1])*exp(-r*n)
    value<-(Ax.bar/ax)
    px<-b*value/12
    
    return(px)
}


life.table <- function(data, sex){
    if(sex=="Male"){
        df <- data %>% dplyr::select(x, contains("m"))
    }
    else{
        df <- data %>% dplyr::select(x, contains("f"))
    }
    colnames(df) <- c("age", "e", "q", "l", "L", "d")
    return(df)
}

## server.R ##

server <- function(input, output, session){
    output$b <- renderValueBox({
        valueBox(format(input$b*10000,big.mark = ","), h4("Benefit"), icon = icon("won-sign"), color = "blue")
    })
    output$px <- renderValueBox({
        valueBox({
            if(req(input$type)=="life"){
                format(round(pv.life(data, input$x, input$i, input$sex, input$n,input$m,input$b),2)*10000,big.mark = ",")
            }
            else{
                format(round(pv.annual(data, input$x, input$i, input$sex, input$n,input$m,input$b),2)*10000,big.mark = ",")
            }
        }, h4("Premium"), icon = icon("won-sign"), color = "red")       
    })
    output$df <- renderTable(rbind(head(life.table(data, input$sex)), 
                                   data.frame(age=c("..."), e=c("..."), q=c("..."), l=c("..."), L=c("..."), d=c("...")), 
                                   tail(life.table(data, input$sex))))
}

shinyApp(ui=ui, server = server)

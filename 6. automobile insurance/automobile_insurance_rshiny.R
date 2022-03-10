library(shiny)
library(shinydashboard)
library(tidyverse)
options("scipen" = 100)
library(readxl)
library(gridExtra)


header <- dashboardHeader(title = "Car Insurance Premium")

sidebar <- dashboardSidebar(
  
  sidebarMenu(id="type",
              menuItem("Car Insurance", tabName = "acc", icon = icon("car")),
              
              fileInput('file', label=h6('Upload Data File'),
                        accept=c('text/csv', 
                                 'text/comma-separated-values,text/plain', 
                                 '.csv')),
              
              
              tags$hr(),
              
              sliderInput("a", h3("Deductible"), value=0,min = 0,max = 1000),
              sliderInput("b", h3("Limit"), value=0, min = 0, max = 2000000),
              paste("--------------","Enter 0 if no limit","--------------",sep = " "),
              
              numericInput("Bonus",h4("Bonus :No accident period(year)"),value = 1),
              
              radioButtons("Kilometres", h3("Kilometres"), 
                           choices = list("less than 1000"= 1,
                                          "from 1000 to 15000" = 2,
                                          "15000 to 20000"=3,
                                          "20000 to 25000" = 4,
                                          "more than 25000" =5), 
                           
                           selected = 1)
  )
)



body <- dashboardBody(
  
  fluidRow(valueBoxOutput("p_kr"),valueBoxOutput("p_usd"),valueBoxOutput("p_won")),
  
  fluidRow(
    box(radioButtons("Zone", h3("Zone"), 
                     choices = list("Seoul"= 1,
                                    "Gyeonggi-do, Incheon" = 2,
                                    "Ulsan, Busan, Daegu"=3,
                                    "Gyeongsang-do" = 4,
                                    "Gwangju, Sejong, Daejeon" =5,
                                    "Chungcheong-do" = 6,
                                    "Jeju" = 7
                     ), 
                     selected = 1), width = 6, height=300, status = "warning", solidHeader = T),
    
    box(radioButtons("Make", h3("Type of Cars"), 
                     choices = list("Benz - Sedan"= 1,
                                    "Granger - Sedan" = 2,
                                    "KIA - K7"=3,
                                    "Hyndai - Genesis" = 4,
                                    "Hyndai - Sonata" =5,
                                    "KIA - K9" = 6,
                                    "Chevrolet - Malibu" = 7,
                                    "Benz - Passenger Car" =8,
                                    "Audi - Passenger Car" = 9
                                    
                     ), 
                     selected = 1), width = 6, height=300, status = "warning", solidHeader = T),
    
    
  ),
  
  fluidRow(
    box(title = 'Coefficient Effect Plot', status = 'success',width = 12, plotOutput("Plot"), solidHeader = T)
  )
)




ui <- dashboardPage(header, sidebar, body)


## function
data <- read_excel("sweden.xls",sheet = 1)

f_gamma <- function(y, glm,new, d=TRUE){
  
  mu <- predict(glm,newdata = new,type= "response")
  shape <- 1/summary(glm)$dispersion
  scale <- mu/shape
  
  pdf <- dgamma(y, shape=shape, scale=scale)
  cdf <- pgamma(y, shape=shape, scale=scale)
  
  if(d==TRUE){
    y*pdf
  }
  else{
    cdf
  }
}


premium <- function(f,A,B,new){
  
  data %>% mutate(
    Zone = factor(Zone),
    Make = factor(Make),
    Kilometres = factor(Kilometres),
    Bonus=factor(Bonus) ) -> data 
  
  df_fit1 <- filter(data, Insured >= 1)
  df_fit2 <- filter(data, Claims >= 1)
  
  glm_poiss <-glm(Claims/Insured ~ Kilometres+Bonus+Make+Zone ,weights= Insured, family=poisson(link = "log"), data=df_fit1)
  glm_gamma <- glm(Payment/Claims~ Kilometres+Bonus+Make+Zone, weights = Claims, family=Gamma(link='log'), data=df_fit2)
  
  
  #EN
  EN <- predict(glm_poiss,newdata = new,type= "response")
  #EY
  B <- ifelse(B==0, Inf, B)
  part1 <- integrate(f, A, (A+B), glm=glm_gamma,new=new, d=TRUE)
  part2 <- ifelse(B==Inf , 0, B*(1-f((A+B), glm_gamma,new=new, d=FALSE)))
  part3 <- -A*(f((A+B), glm_gamma,new=new, d=FALSE) - f(A, glm_gamma,new=new, d=FALSE))
  EY <- part1$value+part2+part3
  #Premium
  price <-EN*EY
  return(list(coef1=glm_poiss$coef, coef2=glm_gamma$coef, price=price))
}


server <- function(input, output, session){
  
  new <- reactive({
    df<-data.frame(Kilometres = as.factor(input$Kilometres), Bonus = as.factor(input$Bonus+1),
                   Make = as.factor(input$Make),  Zone = as.factor(input$Zone))
    
    res <- premium(f_gamma, input$a, input$b, df)
    return(res)
  }
  )
  
  out<-reactive({
    p  <- new()$price
    result <- list(p_kr = round(p), p_usd = round(p*0.22), p_won=round(p*0.22*484))
    return(result)
  })
  
  output$p_kr <- renderValueBox({
    valueBox({out <- out()
    out$p_kr},
    "Premium(krona)", icon = icon("money-bill-wave"),
    color = "red"
    )})
  
  output$p_usd <- renderValueBox({
    valueBox({out <- out()
    out$p_usd},
    "Premium(dollar)", icon = icon("dollar-sign"),
    color = "red"
    )})
  
  output$p_won <- renderValueBox({
    valueBox({out <- out()
    out$p_won},
    "Premium(won)", icon = icon("won-sign"),
    color = "yellow"
    )})
  
  output$Plot <- renderPlot({
    df <- new()
    coef1 <- data.frame(coef = df$coef1[-1]) 
    coef1["var"] <- gsub("\\d", "", rownames(coef1))
    coef1["level"] <- gsub("\\D", "", rownames(coef1))
    rownames(coef1) <- 1:nrow(coef1)
    coef.zero <- data.frame(coef=rep(0,4), var=unique(coef1$var), level=rep(1,4))
    coef1 <- rbind(coef1, coef.zero)
    
    g1 <- ggplot(coef1) + geom_point(aes(x=level, y=coef, col=var), size=2) + geom_line(aes(x=level, y=coef, col=var, group=var), size=1) + facet_wrap(~var, scales = "free") + labs(title = "Accident No.") + theme(legend.position = "none")
    
    coef2 <- data.frame(coef = df$coef2[-1]) 
    coef2["var"] <- gsub("\\d", "", rownames(coef2))
    coef2["level"] <- gsub("\\D", "", rownames(coef2))
    rownames(coef2) <- 1:nrow(coef2)
    coef2 <- rbind(coef2, coef.zero)
    
    g2 <- ggplot(coef2) + geom_point(aes(x=level, y=coef, col=var), size=2) + geom_line(aes(x=level, y=coef, col=var, group=var), size=1) + facet_wrap(~var, scales = "free") + labs(title = "Accident compensation") + theme(legend.position = "none")
    
    grid.arrange(g1,g2, nrow=1)
    
  })
}




shinyApp(ui=ui, server = server)

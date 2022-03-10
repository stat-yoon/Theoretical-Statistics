library(shiny)
library(shinydashboard)
library(tidyverse)

header <- dashboardHeader(title = "Asian Option Price")


sidebar <- dashboardSidebar(
    # load data
    fileInput('file', label=h5('Upload Data File'),
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    
    dateInput('date', label=h5('Choose starting date'), value = "2021-10-1")
    
)


body <- dashboardBody(
    
    fluidRow(valueBoxOutput("price")),
    
    fluidRow(
        box(title = "Input", radioButtons("option", h3("Option"), 
                         choices = list("Asian Call"= 'call', "Asian Put" = 'put'), 
                         selected = "call"),
            numericInput("r", h3("Riskless interest rate"),value=0.05), 
            numericInput("sigma", h3("Volatility"), value=0.5),
            numericInput("k", h3("Excercise Price"), value=60000),
            sliderInput("t", h3("Year"), min=0, max=5, value=1),
            sliderInput("t2", h3("Month"), min=0, max=11, value=0, step=1),  solidHeader = T, status = "warning"),
        
        fluidRow(
            box(title = 'Time-Series plot', status = 'primary',width = 6, plotOutput("Plot"), solidHeader = T)
        ))
)



ui <- dashboardPage(header, sidebar, body)

## function
mc <- function(s0, K, t, r, sigma){
    M <- 100000
    delta.t <- 1/250
    large.t <- round(250*t)
    ci <- c()
    pi <- c()
    z <- matrix(rnorm(M*large.t), ncol=M)
    
    st <- matrix(c(rep(s0, M), rep(NA, M*large.t)), ncol=M, byrow=T)
    
    for(j in 1:large.t){
        st[j+1,] <- st[j,]*exp((r-sigma^2/2)*delta.t + sigma*sqrt(delta.t)*z[j,]) 
    }
    
    st.bar <- colMeans(st)
    c.list <- exp(-r*t)*sapply(st.bar-K, max, 0)
    p.list <- exp(-r*t)*sapply(K-st.bar, max, 0)
    
    ct <- mean(c.list)
    pt <- mean(p.list)
    
    return(list(call=ct, put=pt))
}

## server

server <- function(input, output, session){
    data <- reactive({
        infile <- input$file
        
        if(is.null(infile)){
            return(NULL)
        }
        df <- read.csv(infile$datapath, col.names = c('date', 'St'))
        df$date <- as.Date(df$date)
        df <- df %>% arrange(date)
        return(df)
    })
    
    output$Plot <- renderPlot({
        data <- data()
        if(!is.null(data)){
            data <- data[(nrow(data)-250):nrow(data),]
            gg_plot <- ggplot(data)+geom_line(aes(x=date, y=St), lwd=1) + ggtitle(paste(min(data$date),"~",max(data$date)))
            print(gg_plot)
        }
    })
    
    observeEvent(input$option, {
        output$price <- renderValueBox({
            valueBox({
                data <- data()
                
                validate(need(data, "please upload"))
                s0 <- data[data$date==input$date,]$St
                
                if(input$option=="call"){
                  format(round(mc(s0, input$k, input$t/12, input$r, input$sigma)$call),big.mark = ",")
                }
                else{
                  format(round(mc(s0, input$k, input$t/12, input$r, input$sigma)$put), big.mark = ",")
                } 
                
            }, "Price", icon = icon("won-sign"),
            color = "red")       
        })
    })
    
}





shinyApp(ui=ui, server = server)

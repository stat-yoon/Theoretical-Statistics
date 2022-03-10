

library(shiny)
library(shinydashboard)
library(ggplot2)

header <- dashboardHeader(title = "Insurance Premium")

sidebar <- dashboardSidebar(
    
    sidebarMenu(id="type",
                menuItem("Accident Insurance", tabName = "acc", icon = icon("car")),
                menuItem("Fire Insurance", tabName = "fire", icon = icon("fire-alt")))
    
)


body <- dashboardBody(
    
    fluidRow(valueBoxOutput("p"),
             valueBoxOutput("par1"),
             valueBoxOutput("par2")),
    
    fluidRow(
        box(radioButtons("model", h3("Claim Size Distribution"), 
                         choices = list("Pareto"= 'pareto', "Frechet" = 'frechet',"log-Normal"='lognormal'), 
                         selected = "pareto"), width = 5, height=260, status = "warning"),
        box(numericInput("a", h3("Deductible"), value=0),
            numericInput("b", h3("Limit"), value=10000),
            paste0("Enter negative number for limit : Inf"), background = "black"
                         ),width = 5 ),
    
    fluidRow(
        box(title = "Data", status = "primary", tableOutput('df'), width = 5)
    )
)


ui <- dashboardPage(header, sidebar, body)

## function
data1 <- read.csv("./accident_insurance.csv")
data2 <- read.csv("./fire_insurance.csv", col.names = c("x","count"))


n_df <- function(data){
    n <- sum(data$count)
    df <- data[1:nrow(data)-1,]
    return(list(n=n,df=df))
}

#pr function
log_normal <- function(pr){
    return(qnorm(pr))
}

frenchet <- function(pr){
    return(-log(-log(pr)))
}

pareto <- function(pr){
    return(-log(1-pr))
}

iw.coef <- function(coef){
    mu <- coef[[1]]
    sigma <- coef[[2]]
    tau<- 1/sigma 
    c<- exp(mu/sigma)
    return(list(tau=tau,c=c))
}


#estimate function 
estimator <- function(data,dist){
    df<-n_df(data)$df
    x <- df$x
    count <- df$count
    n<- n_df(data)$n 
    pr <- cumsum(count)/(n+1)
    
    if(dist=="lognormal"){
        lognormal <- log_normal(pr)
        model<-lm(log(x)~lognormal)
        mu<-model$coef[[1]]
        sigma<-model$coef[[2]]
        result<- list(mu=mu,sigma=sigma)
        
    } 
    else if(dist=="pareto"){
        
        
        pareto <- pareto(pr)
        par_r <- c()
        ll <- seq(1,100, 1)
        for (i in ll){
            y1 = log(1+x/i)
            x1 = pareto
            par_r = c(par_r, summary(lm(y1 ~ x1 -1))$r.squared)
        }
        
        ## lambda
        lambda <- ll[which.max(par_r)]
        
        ## alpha
        y2 = log(1+x/lambda)
        x2 = pareto
        alpha <- 1/(lm(y2 ~x2 -1)$coef[[1]])
        
        pareto.y <- log(1+x/lambda)
        pareto.x <- -log(1-pr)/alpha
        
        model<-lm(pareto.y~pareto.x)
        result<-list(lambda=lambda,alpha=alpha)
        
    } else {
        frechet <- frenchet(pr)
        model<-lm(log(x)~frechet)
        tau <- iw.coef(model$coef)[[1]]
        c <- iw.coef(model$coef)[[2]]
        result <- list(tau=tau,c=c)
    }
    return(result) 
}


f_dist <- function(x, par1, par2, model, d=TRUE){
    
    if(model=="pareto"){
        pdf <- par2*(par1^par2)*(par1+x)^(-par2-1)
        cdf <- 1-(par1/(par1+x))^par2
    }
    else if(model=="frechet"){
        pdf <- (par2*par1*exp(-par2/x^par1))/x^(par1+1)
        cdf <- exp(-par2/(x^par1))
    }
    else{
        pdf <- dlnorm(x, par1, par2)
        cdf <- plnorm(x, par1, par2)
    }
    
    if(d==TRUE){
        x*pdf
    }
    else{
        cdf
    }
}




EY <- function(f, par1, par2, A, B, model){
    B <- ifelse(B<=0, Inf, B)
    part1 <- integrate(f, A, (A+B), par1=par1, par2=par2, model=model, d=TRUE)
    part2 <- ifelse(B==Inf , 0, B*(1-f((A+B), par1=par1, par2=par2,  model=model, d=FALSE)))
    part3 <- -A*(f((A+B), par1=par1, par2=par2,  model=model, d=FALSE) - f(A, par1=par1, par2=par2, model=model, d=FALSE))
    return(part1$value+part2+part3)
}


premium <- function(data,model,a,b, type){
    if(type==1){EN <- 0.031}
    else{EN <- 0.009}
    price <-EN*EY(f_dist, estimator(data,model)[[1]], estimator(data,model)[[2]], A=a, B=b, model=model)
    return(price)
}


## server

server <- function(input, output, session){
    
    output$p <- renderValueBox({
        valueBox({
            if(req(input$type)=="acc"){
                round(premium(data1, input$model, input$a, input$b, 1)*10000)
            }
            else{
                round(premium(data2, input$model, input$a, input$b, 2)*10000)
            }
        }, "Premium", icon = icon("won-sign"),
            color = "red"
        )})
    
    output$df <- renderTable({
            if(req(input$type) == "acc"){
                print(head(data1))
            }
            else{
                print(head(data2))
            }
        })
    output$par1 <- renderValueBox({
        valueBox({
            if(req(input$type)=="acc"){
                round(estimator(data1,input$model)[[1]],3)
            }
            else{
                round(estimator(data2,input$model)[[1]],3)
            }
        }, "Parameter1",icon = icon("signal"))
    })
    
    output$par2 <- renderValueBox({
        valueBox({
            if(req(input$type)=="acc"){
                round(estimator(data1,input$model)[[2]],3)
            }
            else{
                round(estimator(data2,input$model)[[2]],3)
            }
        }, "Parameter2",icon = icon("signal"))
    })
}
        
    
    


shinyApp(ui=ui, server = server)


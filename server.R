# server.R

library(quantmod)
library(compiler)

shinyServer(function(input, output) {

#{{{	      
lik <- function(theta, market, stock){


    ## R and Q transformed below (squared) -- need to sqrt in filter
    R <- theta[1]
    Q <- theta[2]
    F <- theta[3]
    mu <- theta[4]
    
    beta_tt <- rep(0, length(market))
    beta_tt_1 <- rep(0, length(market))

    eta <- rep(0, length(market))
    f <- rep(0, length(market))

    Ptt <- rep(0, length(market))

    Ptt_1 <- rep(0, length(market))
        
    beta_tt[1] <- input$init
    Ptt[1] <- (input$init.sd)^2

    for(i in 2:length(market)){
    ## Prediction
        beta_tt_1[i] <- mu + F*beta_tt[i-1]
        Ptt_1[i] <- F*Ptt[i-1]*F+Q^2
        eta[i] <- stock[i]-market[i]*beta_tt_1[i]
        f[i] <- market[i]*Ptt_1[i]*market[i]+R^2
    ## Updating
        beta_tt[i] <- beta_tt_1[i]+Ptt_1[i]*market[i]*(1/f[i])*eta[i]
        Ptt[i] <- Ptt_1[i]-Ptt_1[i]*market[i]*(1/f[i])*market[i]*Ptt_1[i]
    }

    logl <- -0.5*sum(log((((2*pi)^length(market))*abs(f))[-1]))-.5*sum(eta*eta*(1/f),na.rm=T)

    return(-logl)
}

lik <- cmpfun(lik)
#}}}
  
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo", 
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  market.p <- getSymbols("^GSPC", src = "yahoo", 
                       from = "1980-01-01",
                       to = Sys.Date(),
                       auto.assign = FALSE)
  
  output$plot <- renderPlot({   
    data.s <- dataInput()
    
# calculate returns ----
stock <- Delt(Ad(to.monthly(data.s)), type='log')[-1]
market <- Delt(Ad(to.monthly(market.p)), type='log')[-1]
rets <- merge.xts(stock, market, join = "inner")
names(rets) <- c("stock", "market")
stock <- rets[,1]
market <- rets[,2]

## Kalman Filter Starts here ----

theta.start <- c(0.01,0.01, 0.1, 0.1)
max.lik.optim <- optim(theta.start, lik, market=market, stock=stock, hessian=FALSE)

## Run though filter to get betas

R.hat <- max.lik.optim$par[1]
Q.hat <- max.lik.optim$par[2]
F.hat <- max.lik.optim$par[3]
mu.hat <- max.lik.optim$par[4]

beta_tt <- rep(0, length(market))
    beta_tt_1 <- rep(0, length(market))

    eta <- rep(0, length(market))
    f <- rep(0, length(market))

    Ptt <- rep(0, length(market))

    Ptt_1 <- rep(0, length(market))
        
    beta_tt[1] <- input$init
    Ptt[1] <- (input$init.sd)^2

    for(i in 2:length(market)){
    ## Prediction
        beta_tt_1[i] <- mu.hat + F.hat*beta_tt[i-1]
        Ptt_1[i] <- F.hat*Ptt[i-1]*F.hat+Q.hat^2
        eta[i] <- stock[i]-market[i]*beta_tt_1[i]
        f[i] <- market[i]*Ptt_1[i]*market[i]+R.hat^2
    ## Updating
        beta_tt[i] <- beta_tt_1[i]+Ptt_1[i]*market[i]*(1/f[i])*eta[i]
        Ptt[i] <- Ptt_1[i]-Ptt_1[i]*market[i]*(1/f[i])*market[i]*Ptt_1[i]
    }
    logl <- -0.5*sum(log((((2*pi)^length(market))*abs(f))[-1]))-.5*sum(eta*eta*(1/f),na.rm=T)

    # par(mfrow=c(2,1))
    # plot(ts(beta_tt, start =c(2000,2), frequency=12))
    # plot(ts(Ptt, start =c(2000,2), frequency=12))

### End Kalman Filtering Code ----
kalman_filtered_beta <- as.xts(beta_tt, order.by = index(rets)) 
      chartSeries(kalman_filtered_beta, theme = chartTheme("white"), 
                   type = "line", TA = NULL)
      legend("topleft", "test", lty = 1, lwd = 1, col = 1)
  })
})

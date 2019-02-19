rm(list = ls())

'
A time series study on Bitcoin

Content
  1. Data and Preprocessing
  2. GRAPHS
  3. Descriptive stats
  4. Analysis
    - regression
    - robust regression
    - log-linear regression
    - GAM
    - AR(1)
'

#
# 1. Data and Preprocessing
#
  # Load Packages - highlight and run these lines EACH time you launch R.
    library("psych")
    library("Hmisc")
    library("moments")
    library("lmtest")
    library("gam")
    library("readxl")
    library("robust")
  
  #import data from local dir
    datadump <- read_excel("C:/Users/Sen/Downloads/DS 633 (Statistics)/datadump.xlsx")
    #View(datadump)
  
  #convert to dataframe without na's
    mdf <- datadump[1:322,]
    mdf <- mdf[order(as.Date(mdf$Date, format = "%Y-%m-%d")),] #order by date
    
    mdf_clean <- subset(mdf,!is.null(mdf))
    mdf_clean <- as.data.frame(na.omit(mdf_clean))
    
    dim(mdf_clean)
    names(mdf_clean)
    
  #Custom functions used in the Analysis
    
    ftsplot2 <- function(x,y, counter){
      # displays time series plot with 2 lines
      # x is in solid line and y is in dotted line
      plot(x,type="l",lty=1, main = paste("Fig. ", counter,"Time Series Plot for actual and predicted")); lines(y,lty=2)}
    
    residAnalysis <- function(rdf, dependent){
      
      'Plots the hist of residuals, 
        actual vs predicted, 
        scatterplot of actual vs predicted 
        and %change if resid over time
      '
      par(mfcol=c(1,3))
      
      ftsplot2(dependent,rdf$p,counter) ## Time Series Plot for aactual and predicted
        counter <<- counter + 1
      
      scatter.smooth(rdf$p,dependent,  pch =21, bg = "green",main = paste("Fig. ",counter,"Scatterplot of actual vs predicted"), xlab = 'predicted BTC', ylab = 'Actual BTC') #scatterplot of actual vs predicted
        counter <<- counter + 1

      hist(rdf$r, main = paste("Fig. ",counter,"Hist. of residuals"), xlab = 'residuals',prob=1,density = 20, col = "dark green")
      lines(density(rdf$r))
        counter <<- counter + 1 #add to global variable counter
      '  
      rdf$pctResid <- rdf$r/dependent
      ts.plot(rdf$pctResid, main = paste("Fig. ",counter,"Percentage change in resid"));abline(h=0) #percentage change in resid
        counter <<- counter + 1
      '
      }
    

#
# 2. GRAPHS
#

  #Histograms
    counter = 1
    
    par(mfcol=c(2,2))
    for (i in 2:5){
      index = names(mdf_clean[i])
      hist( mdf_clean[,i],xlab = index, main= paste("Fig. ",counter, "Hist of ",index),prob=1,density = 20, col = "dark green") 
      lines(density(mdf_clean[,i]))
      counter = counter + 1
    }
  
  #Time-series plot
    par(mfcol=c(2,2))
    for (i in 2:5){
      index = names(mdf_clean[i])
      ts.plot(mdf_clean[,i],xlab = 'obs',ylab = index, main= paste("Fig. ",counter, "TS plot of ",index))
      counter = counter + 1
    }
  
  #Scatter-plot of all var vs BTC/$
    par(mfcol=c(1,3))
    for (i in 2:4){
      index = names(mdf_clean[i])
      scatter.smooth(mdf_clean[,i], mdf_clean$btc, xlab=index, ylab="BTC/USD",pch = 21, bg="green",main= paste("Fig. ",counter, "Scatter plot of btc and",index))
      counter = counter + 1
    }

    
#
# 3. Descriptive stats
#
  describe.by(mdf_clean[,2:5])
  
  #Correlarion
  round(cor(mdf_clean[,2:5]),3)

#
# 4. Analyisis
#

# 4.1. Simple Regression Model
  
  fit = lm(btc~sp500+gold+btctr, data=mdf_clean)
    summary(fit)
    dwtest(fit)
  
  # residual analysis
  rdf <- data.frame(mdf_clean, p= fit$fitted.values, r= fit$residuals)
  residAnalysis(rdf,rdf$btc)

'
# 4.2. Robust Regression Model
  
  fit = lmRob(btc~sp500+gold+btctr, data=mdf_clean)
  summary(fit)
  dwtest(fit)
  
  # residual analysis
  rdf <- data.frame(mdf_clean, p= fit$fitted.values, r= fit$residuals)
  residAnalysis(rdf,rdf$btc)
' 

# 4.4. log-linear regression

  mdf_clean$btclog <- log(mdf_clean$btc)
  ts.plot(mdf_clean$btclog)
  ts.plot(mdf_clean$btc)
  
  fit = lm(btclog~sp500+gold+btctr, data=mdf_clean)
    summary(fit)
    dwtest(fit)
    
  # residual analysis
  rdf <- data.frame(mdf_clean, p= fit$fitted.values, r= fit$residuals)
  
  #residAnalysis(rdf, rdf$btc)
  par(mfcol=c(1,3))
    ftsplot2(rdf$btc,exp(rdf$p),counter) ## Time Series Plot for aactual and predicted
    counter = counter + 1
    scatter.smooth(exp(rdf$p),rdf$btc, pch=21, bg = "green", main = paste("Fig. ",counter,"Scatterplot of actual vs predicted")) #scatterplot of actual vs predicted
    counter = counter + 1
    hist(rdf$r, main = paste("Fig. ",counter,"Hist. of residuals"),prob=1,density = 20, col = "dark green")
    lines(density(rdf$r))
    counter = counter + 1
    
# 4.3. GAM
  
  fit<-gam(btc~s(sp500,3)+s(gold,1.5)+s(btctr,1.1), data = mdf_clean)
  summary(fit)
  #plot GAM
  par(mfcol=c(2,2))
  plot.gam(fit)
  
  rdf <- data.frame(mdf_clean, p= fit$fitted.values, r= fit$residuals)
  cor(rdf$btc,rdf$p)^2 #Psudo r^2
  dwtest(fit)
  
  # residual analysis
  residAnalysis(rdf,rdf$btc)
  
# 4.5. AR(1) linear regression
  
  n <- length(mdf_clean$btc)
  mdf_clean$btc.lag1 = Lag(mdf_clean$btc,+1) #Hmic
  mdf_clean <- na.omit(mdf_clean)
  
  fit <- lm(btc~btc.lag1, data=mdf_clean)
    summary(fit)
    dwtest(fit)
  
  # residual analysis
  rdf <- data.frame(mdf_clean, p= fit$fitted.values, r= fit$residuals)
  residAnalysis(rdf,rdf$btc)
  
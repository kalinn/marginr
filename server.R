
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(rPython)
library(ROCR)
dir = system("pwd", intern=TRUE)
python.load(paste0(dir,"/fit_svm.py"))
source(paste0(dir,"/normalize_fns.R"))

shinyServer(function(input, output) {
   
    faithful$labels = rep(0, nrow(faithful))
    faithful$labels[which(faithful$eruptions > 3.2)] = 1
# plot(faithful$eruptions,faithful$waiting,col=faithful$labels+1)

    
    output$distPlot <- renderPlot({
    ## TRY PUTTING THE CLOUDS TOGETHER
    faithful.star<-faithful
    faithful.star[faithful$labels==1,]$eruptions<-faithful[faithful$labels==1,]$eruptions-input$xx/10
    faithful.star[faithful$labels==1,]$waiting<-faithful[faithful$labels==1,]$waiting-input$yy

    samp <- sample(1:nrow(faithful), input$n, replace=FALSE)
    # generate bins based on input$bins from ui.R
    y <- faithful$labels[samp]
    x <- as.matrix(faithful.star[samp,1:2])
    colnames(x) <- NULL
    cn.fit <- cn.norm(x, y, x) 
    cn.x <- as.matrix(cn.fit$sc.x)
    colnames(cn.x) <- NULL
    z.fit <- z.norm(x, x) 
    z.x <- as.matrix(z.fit$sc.x)
    colnames(z.x) <- NULL
    
    cvec <- 10^c(seq(-5,-1,length.out=200))
    cn.svm <- fit.svm(cn.x, y, cn.x, cvec)
    z.svm <- fit.svm(z.x, y, z.x, cvec)
    xseq = seq(-30, 30, length.out=10000)
    cn.line = -cn.svm$rho/cn.svm$w[2] - cn.svm$w[1]*xseq/cn.svm$w[2]
    z.line = -z.svm$rho/z.svm$w[2] - z.svm$w[1]*xseq/z.svm$w[2]
    
    # draw the histogram with the specified number of bins
    par(mfrow=c(2,2),mar=c(2,2,2,2))
    plot(z.x[,1], z.x[,2], col=2*y+2, xlab="eruptions", ylab="waiting", main="Z-normalization")
    lines(xseq, z.line)
    plot(cn.x[,1], cn.x[,2], col=2*y+2, xlab="eruptions", ylab="waiting", main="Control-normalization")
    lines(xseq, cn.line)

    cn.t<-as.matrix(faithful.star[,1:2])%*%cn.svm$w
    z.t<-as.matrix(faithful.star[,1:2])%*%z.svm$w
    cn.pred <- prediction(cn.t, faithful.star$labels)
    cn.perf <- performance(cn.pred, measure = "tpr", x.measure = "fpr") 
    z.pred <- prediction(z.t, faithful.star$labels)
    z.perf <- performance(z.pred, measure = "tpr", x.measure = "fpr") 
    plot(z.perf, col='red',lwd=3)
    text(0.5,0.5,round(unlist(performance(z.pred, measure = "auc")@y.values),2))
    plot(cn.perf, col='green',lwd=3)
    text(0.5,0.5,round(unlist(performance(cn.pred, measure = "auc")@y.values),2))
    
    print(paste("Cost parameter for z-normalization tuned to: ", z.svm$bestC$C))
    print(paste("Cost parameter for control-normalization tuned to: ", cn.svm$bestC$C))
    })

})

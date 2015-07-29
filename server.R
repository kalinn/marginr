
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
ts.n<-1E3
library(shiny)
library(rPython)
library(ROCR)
library(MASS)
dir = system("pwd", intern=TRUE)
python.load(paste0(dir,"/fit_svm.py"))
source(paste0(dir,"/normalize_fns.R"))

pROC = function(pred, fpr.stop){
  perf <- performance(pred,"tpr","fpr")
  for (iperf in seq_along(perf@x.values)){
    ind = which(perf@x.values[[iperf]] <= fpr.stop)
    perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
    perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]
  }
  return(perf)
}

shinyServer(function(input, output) {
      
  output$distPlot <- renderPlot({
      #    faithful$labels = rep(0, nrow(faithful))
      #    faithful$labels[which(faithful$eruptions > 3.2)] = 1
      #    plot(faithful$eruptions,faithful$waiting,col=faithful$labels+1)
      ## TRY PUTTING THE CLOUDS TOGETHER
      #    faithful.star<-faithful
      #    faithful.star[faithful$labels==1,]$eruptions<-faithful[faithful$labels==1,]$eruptions-input$xx/10
      #    faithful.star[faithful$labels==1,]$waiting<-faithful[faithful$labels==1,]$waiting-input$yy
      
      #    print(input$n0)
      #    print(input$n1)
      #    print(faithful$labels/sum(faithful$labels))
      #    sum(print(faithful$labels/sum(faithful$labels))>0)
      #    sum(print((1-faithful$labels)/sum(1-faithful$labels))>0)
      #    samp <- sample(1:nrow(faithful), input$n, replace=FALSE)
      #    samp0 <- sample(1:nrow(faithful), input$n0, prob=faithful$labels/sum(faithful$labels), replace=FALSE)
      #    samp1 <- sample(1:nrow(faithful), input$n1, prob=(1-faithful$labels)/sum(1-faithful$labels), replace=FALSE)
      #    samp<-c(samp0,samp1)
      #    y <- faithful$labels[samp]
      #    x <- as.matrix(faithful.star[samp,1:2])
    
      gen.seed = abs(floor(10000*rnorm(1)))
      set.seed(gen.seed)
      print(paste("Seed is: ", gen.seed))
      
      m1 = c(input$meanX1n1, input$meanX2n1, rep(0, input$p))
      m2 = c(input$meanX1n2, input$meanX2n2, rep(0, input$p))
      v1 = c(input$varX1n1, input$varX2n1)
      v2 = c(input$varX1n2, input$varX2n2)
      c1 = input$corn1/10*sqrt(v1[1])*sqrt(v1[2])
      c2 = input$corn2/10*sqrt(v2[1])*sqrt(v2[2])
      sig1 = matrix(rep(0, (input$p+2)^2), input$p+2, input$p+2)
      diag(sig1) = c(v1, rep(1, input$p))
      sig1[1,2] = c1
      sig1[2,1] = c1
      sig2 = matrix(rep(0, (input$p+2)^2), input$p+2, input$p+2)
      diag(sig2) = c(v2, rep(1, input$p))   
      sig2[1,2] = c2
      sig2[2,1] = c2
      n1 = input$n1
      n2 = input$n2
      cloud1 = mvrnorm(n1, m1, sig1)
      cloud2 = mvrnorm(n2, m2, sig2)

#      transfm<-function(x,ma) log(x-min(x)+0.01)+ma
#      cloud1[1,]<-transfm(cloud1[1,],m1)
#      cloud2[1,]<-transfm(cloud2[1,],m2)

      x = rbind(cloud1, cloud2)
      y = c(rep(0, n1), rep(1, n2))
      ts.cloud1 = mvrnorm(ts.n, m1, sig1)
      ts.cloud2 = mvrnorm(ts.n, m2, sig2)

#      ts.cloud1[1,]<-transfm(ts.cloud1[1,],m1)
#      ts.cloud2[1,]<-transfm(ts.cloud2[1,],m2)

      ts.x = rbind(ts.cloud1, ts.cloud2)
      ts.y = c(rep(0, ts.n), rep(1, ts.n))

      
      colnames(x) <- NULL
      colnames(ts.x) <- NULL
      cn.fit <- cn.norm(x, y, ts.x) 
      cn.x <- as.matrix(cn.fit$sc.x)
      cn.ts <- as.matrix(cn.fit$sc.ts)
      colnames(cn.x) <- NULL
      colnames(cn.ts) <- NULL
      z.fit <- z.norm(x, ts.x) 
      z.x <- as.matrix(z.fit$sc.x)
      z.ts <- as.matrix(z.fit$sc.ts)
      colnames(z.x) <- NULL
      colnames(z.ts) <- NULL
      
      
      cvec <- 10^c(seq(-5,2,length.out=10))
      cn.svm <- fit.svm(cn.x, y, cn.ts, cvec)
      z.svm <- fit.svm(z.x, y, z.ts, cvec)
      xseq = seq(-30, 30, length.out=10000)
      cn.line = -cn.svm$rho/cn.svm$w[2] - cn.svm$w[1]*xseq/cn.svm$w[2]
      z.line = -z.svm$rho/z.svm$w[2] - z.svm$w[1]*xseq/z.svm$w[2]
      
     # draw the histogram with the specified number of bins
      par(mfrow=c(4,2),mar=c(2,2,2,2))
      
      plot(z.x[,1], z.x[,2], col=2*y+2, xlab="X1", ylab="X2", main="Z-normalization", cex.main=2)
      lines(xseq, z.line)
      abline(a=-z.svm$rho/z.svm$w[2],b=- z.svm$w[1]/z.svm$w[2])
      plot(cn.x[,1], cn.x[,2], col=2*y+2, xlab="X1", ylab="X2", main="Control-normalization", cex.main=2)
      lines(xseq, cn.line)
      abline(a=-cn.svm$rho/cn.svm$w[2],b=- cn.svm$w[1]/cn.svm$w[2])
      
      plot(z.ts[,1], z.ts[,2], col=2*ts.y+2, xlab="X1", ylab="X2", main="Z-normalization", cex.main=2)
      lines(xseq, z.line)
      abline(a=-z.svm$rho/z.svm$w[2],b=- z.svm$w[1]/z.svm$w[2])
      plot(cn.ts[,1], cn.ts[,2], col=2*ts.y+2, xlab="X1", ylab="X2", main="Control-normalization", cex.main=2)
      lines(xseq, cn.line)
      abline(a=-cn.svm$rho/cn.svm$w[2],b=- cn.svm$w[1]/cn.svm$w[2])
      
      cn.t<-cn.ts%*%cn.svm$w
      z.t<-z.ts%*%z.svm$w
      cn.pred <- prediction(cn.t, ts.y)
      cn.perf <- performance(cn.pred, measure = "tpr", x.measure = "fpr") 
      z.pred <- prediction(z.t, ts.y)
      z.perf <- performance(z.pred, measure = "tpr", x.measure = "fpr") 
     
      plot(z.perf, col='red',lwd=3)
      text(0.5,0.75,round(unlist(performance(z.pred, measure = "auc")@y.values),2), cex=3)
      text(0.5,0.5,round(unlist(performance(z.pred, measure = "auc", fpr.stop=.25)@y.values),2), cex=3)
      text(0.5,0.25,round(unlist(performance(z.pred, measure = "auc", fpr.stop=.5)@y.values),2), cex=3)
      plot(cn.perf, col='green',lwd=3)
      text(0.5,0.75,round(unlist(performance(cn.pred, measure = "auc")@y.values),2), cex=3)
      text(0.5,0.5,round(unlist(performance(cn.pred, measure = "auc", fpr.stop=.25)@y.values),2), cex=3)
      text(0.5,0.25,round(unlist(performance(cn.pred, measure = "auc", fpr.stop=.5)@y.values),2), cex=3)
     
      plot(z.perf, col='red', lwd=3)
      lines(performance(cn.pred, measure = "tpr", x.measure = "fpr")@x.values[[1]],
            performance(cn.pred, measure = "tpr", x.measure = "fpr")@y.values[[1]], col='green', lwd=3)
      plot(0, 0)
      text(0, 0.5, print(paste("Seed = ", gen.seed)), cex=3)
     
      print(paste("Cost parameter for z-normalization tuned to: ", z.svm$bestC$C))
      print(paste("Cost parameter for control-normalization tuned to: ", cn.svm$bestC$C))
    })

})

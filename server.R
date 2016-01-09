
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
# ts.n<-1E3
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
		# sample size, CN group	
		n1 = input$n1
    	# MVN means
		m1 = rep(0, input$p + 2)
		m2 = c(input$meanX1n2, input$meanX2n2, 
			rep(0, input$p))
        # MVN variances
		v1 = c(input$varX1n1, input$varX2n1)
		v2 = c(input$varX1n2, input$varX2n2)
        # Covariances
		c1 = input$corn1*sqrt(v1[1])*sqrt(v1[2])
		c2 = input$corn2*sqrt(v2[1])*sqrt(v2[2])
        # Convert to covariance matrices
		sig1 = matrix(rep(0, (input$p+2)^2), 
			input$p+2, input$p+2)
		diag(sig1) = c(v1, rep(1, input$p))
		sig1[1,2] = c1
		sig1[2,1] = c1
		sig2 = matrix(rep(0, (input$p+2)^2), 
			input$p+2, input$p+2)
		diag(sig2) = c(v2, rep(1, input$p))   
		sig2[1,2] = c2
		sig2[2,1] = c2
      
#      gen.seed = abs(floor(10000*rnorm(1)))
#      set.seed(gen.seed)
		set.seed(1)
#      print(paste("Seed is: ", gen.seed))
		n2 = floor(n1/2)
		cloud1 = mvrnorm(n1, m1, sig1)
		cloud2A = mvrnorm(n2, m2, sig2)
		cloud2B = mvrnorm(n2, m2, sig2)
		cloud2C = mvrnorm(n2, m2, sig2)
		#		cloud2A = cbind(rlnorm(n2, input$meanX1n2, 1/input$varX1n2), 
#		                rnorm(n2, input$meanX2n2, input$varX2n2))
#		cloud2B = cbind(rlnorm(n2, input$meanX1n2, 1/input$varX1n2), 
#		                rnorm(n2, input$meanX2n2, input$varX2n2))
#		cloud2C = cbind(rlnorm(n2, input$meanX1n2, 1/input$varX1n2), 
#		                rnorm(n2, input$meanX2n2, input$varX2n2))
		
#		cloud2A = mvrnorm(n2, m2, sig2)
#		m2B = m2
#		m2B[1] = 2*sqrt(v2[1])*m2[1]
#		m2B[2] = 2*sqrt(v2[2])*m2[2]
#		cloud2B = mvrnorm(n2, m2B, sig2)
#		m2C = m2B
#		m2C[1] = 4*sqrt(v2[1])*m2[1]
#		m2C[2] = 4*sqrt(v2[2])*m2[2]
#		cloud2C = mvrnorm(n2, m2B, sig2)
##		cloud2 = rbind(cloud2A, cloud2B, cloud2C)
#		grps = rep(c(1:3), nrow(cloud2))[1:nrow(cloud2)]
#		samp = sample(grps, length(grps), replace=FALSE)
#		cloud2 = cbind(cloud2, samp)
#		cloud2A = cloud2[which(samp==1),-ncol(cloud2)]
#		cloud2B = cloud2[which(samp==2),-ncol(cloud2)]
#		cloud2C = cloud2[which(samp==3),-ncol(cloud2)]
		
		xA = rbind(cloud1, cloud2A)
		yA = c(rep(0, n1), rep(1, n2))
		xB = rbind(cloud1, cloud2A, cloud2B)
		yB = c(rep(0, n1), rep(1, 2*n2))
		xC = rbind(cloud1, cloud2A, cloud2B, cloud2C)
		yC = c(rep(0, n1), rep(1, 3*n2))
		set.seed(1)      
		cnA <- cn.norm(xA, yA, xA) 
		cnB <- cn.norm(xB, yB, xB) 
		cnC <- cn.norm(xC, yC, xC) 
		cnAx <- as.matrix(cnA$sc.x)
		cnBx <- as.matrix(cnB$sc.x)
		cnCx <- as.matrix(cnC$sc.x)
		colnames(cnAx) <- NULL
		colnames(cnBx) <- NULL
		colnames(cnCx) <- NULL
		zA <- z.norm(xA, xA) 
		zB <- z.norm(xB, xB) 
		zC <- z.norm(xC, xC) 
		zAx <- as.matrix(zA$sc.x)
		zBx <- as.matrix(zB$sc.x)
		zCx <- as.matrix(zC$sc.x)
		colnames(zAx) <- NULL
		colnames(zBx) <- NULL
		colnames(zCx) <- NULL
      
		cvec <- 10^c(seq(-4,2))
		cnAsvm <- fit.svm(cnAx, yA, cnAx, cvec)
		cnBsvm <- fit.svm(cnBx, yB, cnBx, cvec)
		cnCsvm <- fit.svm(cnCx, yC, cnCx, cvec)
		zAsvm <- fit.svm(zAx, yA, zAx, cvec)
		zBsvm <- fit.svm(zBx, yB, zBx, cvec)
		zCsvm <- fit.svm(zCx, yC, zCx, cvec)
		xseq = seq(-30, 30, length.out=10000)
		cnAline = -cnAsvm$rho/cnAsvm$w[2] - cnAsvm$w[1]*xseq/cnAsvm$w[2]
		cnBline = -cnBsvm$rho/cnBsvm$w[2] - cnBsvm$w[1]*xseq/cnBsvm$w[2]
		cnCline = -cnCsvm$rho/cnCsvm$w[2] - cnCsvm$w[1]*xseq/cnCsvm$w[2]
		zAline = -zAsvm$rho/zAsvm$w[2] - zAsvm$w[1]*xseq/zAsvm$w[2]
		zBline = -zBsvm$rho/zBsvm$w[2] - zBsvm$w[1]*xseq/zBsvm$w[2]
		zCline = -zCsvm$rho/zCsvm$w[2] - zCsvm$w[1]*xseq/zCsvm$w[2]
      
     # draw the histogram with the specified number of bins
      par(mfrow=c(5,2),mar=c(2,2,2,2))
      
      plot(zAx[,1], zAx[,2], col=-2*(yA-1)+2, xlab="X1", ylab="X2", 
           main="Z-normalization", cex.main=2, ylim=c(-6,6), xlim=c(-6,6))
      lines(xseq, zAline)
      abline(a=-zAsvm$rho/zAsvm$w[2], b=-zAsvm$w[1]/zAsvm$w[2])
      plot(cnAx[,1], cnAx[,2], col=-2*(yA-1)+2, xlab="X1", ylab="X2", 
           main="Control-normalization", cex.main=2, ylim=c(-6,6), xlim=c(-6,6))
      lines(xseq, cnAline)
      abline(a=-cnAsvm$rho/cnAsvm$w[2], b=-cnAsvm$w[1]/cnAsvm$w[2])
      
      plot(zBx[,1], zBx[,2], col=-2*(yB-1)+2, xlab="X1", ylab="X2", 
           main="Z-normalization", cex.main=2, ylim=c(-6,6), xlim=c(-6,6))
      lines(xseq, zBline)
      abline(a=-zBsvm$rho/zBsvm$w[2],b=-zBsvm$w[1]/zBsvm$w[2])
      plot(cnBx[,1], cnBx[,2], col=-2*(yB-1)+2, xlab="X1", ylab="X2", 
           main="Control-normalization", cex.main=2, ylim=c(-6,6), xlim=c(-6,6))
      lines(xseq, cnBline)
      abline(a=-cnBsvm$rho/cnBsvm$w[2], b=-cnBsvm$w[1]/cnBsvm$w[2])

      plot(zCx[,1], zCx[,2], col=-2*(yC-1)+2, xlab="X1", ylab="X2", 
           main="Z-normalization", cex.main=2, ylim=c(-6,6), xlim=c(-6,6))
      lines(xseq, zCline)
      abline(a=-zCsvm$rho/zCsvm$w[2],b=-zCsvm$w[1]/zCsvm$w[2])
      plot(cnCx[,1], cnCx[,2], col=-2*(yC-1)+2, xlab="X1", ylab="X2", 
           main="Control-normalization", cex.main=2, ylim=c(-6,6), xlim=c(-6,6))
      lines(xseq, cnCline)
      abline(a=-cnCsvm$rho/cnCsvm$w[2], b=-cnCsvm$w[1]/cnCsvm$w[2])
      
      # cn.t<-cn.ts%*%cn.svm$w
      # z.t<-z.ts%*%z.svm$w
      # cn.pred <- prediction(cn.t, ts.y)
      # cn.perf <- performance(cn.pred, measure = "tpr", x.measure = "fpr") 
      # z.pred <- prediction(z.t, ts.y)
      # z.perf <- performance(z.pred, measure = "tpr", x.measure = "fpr") 
     
#       plot(z.perf, col='red',lwd=3)
#       text(0.5,0.75,round(unlist(performance(z.pred, measure = "auc")@y.values),2), cex=3)
#       text(0.5,0.5,round(unlist(performance(z.pred, measure = "auc", fpr.stop=.25)@y.values),2), cex=3)
#       text(0.5,0.25,round(unlist(performance(z.pred, measure = "auc", fpr.stop=.5)@y.values),2), cex=3)
#       plot(cn.perf, col='green',lwd=3)
#       text(0.5,0.75,round(unlist(performance(cn.pred, measure = "auc")@y.values),2), cex=3)
#       text(0.5,0.5,round(unlist(performance(cn.pred, measure = "auc", fpr.stop=.25)@y.values),2), cex=3)
#       text(0.5,0.25,round(unlist(performance(cn.pred, measure = "auc", fpr.stop=.5)@y.values),2), cex=3)
     
#       plot(z.perf, col='red', lwd=3)
#       lines(performance(cn.pred, measure = "tpr", x.measure = "fpr")@x.values[[1]],
#             performance(cn.pred, measure = "tpr", x.measure = "fpr")@y.values[[1]], col='green', lwd=3)
#       plot(0, 0)
# #      text(0, 0.5, print(paste("Seed = ", gen.seed)), cex=3)
     
#       print(paste("Cost parameter for z-normalization tuned to: ", z.svm$bestC$C))
#       print(paste("Cost parameter for control-normalization tuned to: ", cn.svm$bestC$C))
    })

})

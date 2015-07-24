
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(rPython)
dir = system("pwd", intern=TRUE)
python.load(paste0(dir,"/fit_svm.py"))
source(paste0(dir,"/normalize_fns.R"))

shinyServer(function(input, output) {
   
    faithful$labels = rep(0, nrow(faithful))
    faithful$labels[which(faithful$eruptions > 3.2)] = 1
    
    output$distPlot <- renderPlot({
    
    samp <- sample(1:nrow(faithful), input$n, replace=FALSE)
    # generate bins based on input$bins from ui.R
    y <- faithful$labels[samp]
    x <- as.matrix(faithful[samp,1:2])
    colnames(x) <- NULL
    cn.fit <- cn.norm(x, y, x) 
    cn.x <- as.matrix(cn.fit$sc.x)
    colnames(cn.x) <- NULL
    z.fit <- z.norm(x, x) 
    z.x <- as.matrix(z.fit$sc.x)
    colnames(z.x) <- NULL
    
    cvec <- 10^c(-3:2)
    cn.svm <- fit.svm(cn.x, y, cn.x, cvec)
    z.svm <- fit.svm(z.x, y, z.x, cvec)
    xseq = seq(-30, 30, length.out=10000)
    cn.line = -cn.svm$rho/cn.svm$w[2] - cn.svm$w[1]*xseq/cn.svm$w[2]
    z.line = -z.svm$rho/z.svm$w[2] - z.svm$w[1]*xseq/z.svm$w[2]
    
    # draw the histogram with the specified number of bins
    par(mfrow=c(1,2))
    plot(z.x[,1], z.x[,2], col=2*y+2, xlab="eruptions", ylab="waiting", main="Z-normalization")
    lines(xseq, z.line)
    plot(cn.x[,1], cn.x[,2], col=2*y+2, xlab="eruptions", ylab="waiting", main="Control-normalization")
    lines(xseq, cn.line)

    print(paste("Cost parameter for z-normalization tuned to: ", z.svm$bestC$C))
    print(paste("Cost parameter for control-normalization tuned to: ", cn.svm$bestC$C))
    })

})

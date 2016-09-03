# marginr

A shiny web application to investigate the influence of feature standardization on the estimated optimal hyperplane of a linear support vector machine.  For more details, see [this paper](http://www.sciencedirect.com/science/article/pii/S1053811916001531).

Depends on the following R packages: shiny, rPython, ROCR, 

### To run this app:

1. Download the zip from https://github.com/kalinn/marginr and unzip in a convenient location.
2. Open R and set your working directory to wherever the marginr-master folder lives.
3. Run the following code in R:

>	library(shiny)	
>	runApp("marginr-master”)

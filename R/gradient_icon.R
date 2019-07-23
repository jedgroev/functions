#' gradient_icon
#'
#' Generates a gradient scale

#' @param col spatialpointsdataframe with the parameters 
#' @param labels minimum and maximum value
#' @param tdist distance of the labels from the gradient scale
#' @param bars number of lines in the gradient scale
#' @param title title of the gradient scale 
#' @param dist distance from x and y top corner
#' @param tcol color of labels and title

#' @keywords gradient, plot

#' @examples 
#' export(gradient_icon(col=colorRampPalette(c('grey90','grey20'))(100), labels = c(500,1000), tdist=0.05,bars=10,title=NULL),res2=30,ratio=0.8,'gradient_icon',type='png') 

gradient_icon <- function(col=colorRampPalette(c('grey90','grey20'))(100), labels = c(0,1), tdist=0.01,bars=10,title=NULL, dist=c(0.15,0.15), tcol='white'){
par(bg=NA, mar=c(0,0,1,0),oma=c(0,0,1,0), cex=2.8)
x <- 0:1
y <- 0:1
plot(x,y,type='n',axes=FALSE,ann=FALSE) 
gradient(x,y, col, len=c(0.7,0.8), labels=labels, bars=bars, title=title, tdist=tdist,dist=dist,tcol=tcol) 
}
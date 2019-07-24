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
#' @param filename the name of the legend_icon when saving as a png, if NULL the png is not exported
#' @param ratio height-width ratio 
#' @param res2 resolution of the png legend

#' @keywords gradient, plot
#' @export

#' @examples 
#' gradient_icon(col=colorRampPalette(c('grey90','grey20'))(100), labels = c(500,1000), tdist=0.2,bars=10,title=NULL,filename='gradient_icon',res2=30,ratio=0.5) 

gradient_icon <- function(col=colorRampPalette(c('grey90','grey20'))(100), labels = c(0,1), tdist=0.2,bars=10,title=NULL,dist=c(0.40,0.20),tcol='white',filename=NULL,ratio=0.5,res2=30){

ff <- function(){
	par(bg=NA, mar=c(0,0,1,0),oma=c(0,0,1,0), cex=2.8)
	x <- 0:1
	y <- 0:1
	plot(x,y,type='n',axes=FALSE,ann=FALSE) 
	gradient(x,y, col, len=c(0.7,0.8), labels=labels, bars=bars, title=title, tdist=tdist,dist=dist,tcol=tcol) 
	}
ff()

if(is.null(filename) == FALSE){export(func=ff(),filename,ratio=ratio,res2=res2,type='png')}


}
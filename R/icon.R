#' icon 
#'
#' generate an icon

#' @param pch symbol to use
#' @param col color border
#' @param bg color background
#' @param filename the name of the icon when saving as a png, if NULL the png is not exported
#' @param ratio the width/height ratio of the exported icon  

#' @keywords export, icon, png
#' @export

#' @examples 
#' # generate and export an icon
#' icon(pch=21, col='black',bg='white',filename='icon')

icon <- function(pch=24,col='black',bg='white',filename=NULL,ratio=0.5){

	ff <- function(){
		par(bg=NA)
		plot(1,1,axes=FALSE,ann=FALSE, cex =40, col=col,bg=bg,pch=pch)
		}
	ff()
if(is.null(filename) == FALSE){export(func=ff(),filename,type='png',ratio=ratio)}
}
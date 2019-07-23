#' icon 
#'
#' generate an icon

#' @param pch symbol to use
#' @param col color border
#' @param bg color background

#' @keywords export, icon, png
#' @export

#' @examples 
#' # generate and export an icon
#' export(func=icon(pch=21, col='black',bg='white'),'icon',type='png') 

icon <- function(pch=24,col='black',bg='white'){
par(bg=NA)
plot(1,1,axes=FALSE,ann=FALSE, cex =40, col=col,bg=bg,pch=pch)
} 
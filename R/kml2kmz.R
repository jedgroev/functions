 #' kml2kmz 
#'
#' Generates a kmz from a klm

#' @param files_to_zip a vector with the files that need to be zipped into the kmz file 
#' @param filename output file name of the kmz

#' @keywords kml,kmz

#' @examples 
#' prepare a dataframe 
#' obj <- data.frame(altitude=1:100,colour=heat.colors(100),heading=1:100,variable=runif(100),longitude=seq(15.202020, 16.202020,length.out=100), latitude=seq(45.459069,46.543523,length.out=100))
#' coordinates(obj) <- c('longitude','latitude')
#' proj4string(obj)<- CRS("+init=epsg:4326")

#' Example using png locally stored
#' # first generate an icon
#' icon <- function(pch=21){
#' par(bg=NA)
#' plot(1,1,axes=FALSE,ann=FALSE, cex =30, col='black',bg='white',pch=pch)
#' } 
#' export(func=icon(),'icon',type='png')
#' # use function with the generated icon
#' kml2(obj,altitude='altitude',colour='colour',heading='heading',size='variable',icon="icon.png",filename='track_kml.kml')
#' kml2kmz(files_to_zip=c('icon.png','track_kml.kml'),os='mac',filename='track_kmz')

 # set your working directory to where your image is 
 kml2kmz <- function(files_to_zip = c('icon.jpg','track_kml.kml'), filename='track_kmz'){
 zip(zipfile = filename, files = files_to_zip)
 file.rename(paste0(filename,'.zip'), paste0(filename,'.kmz'))
 }
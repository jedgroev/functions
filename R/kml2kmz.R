 #' kml2kmz 
#'
#' Generates a kmz from a klm

#' @param files_to_zip a vector with the files that need to be zipped into the kmz file 
#' @param filename output file name of the kmz

#' @keywords kml,kmz
#' @export

#' @examples 
#' ## Example using png a locally stored png
#'
#' # Prepare a dataframe 
#' obj <- data.frame(altitude=1:100,colour=heat.colors(100),heading=1:100,variable=runif(100),longitude=seq(15.202020, 16.202020,length.out=100), latitude=seq(45.459069,46.543523,length.out=100))
#' coordinates(obj) <- c('longitude','latitude')
#' proj4string(obj)<- CRS("+init=epsg:4326")
#'
#' # First generate and save icon
#' icon(pch=24, col='black',bg='white',filename='icon')
#' 
#' # Generate a legend  
#' gradient_icon(col=colors_legend$colour, 
#'               labels = round(c(min(colors_legend$variable_colour),max(colors_legend$variable_colour)),2),
#'               bars=5,
#'               title='title',
#'               tdist=0.2,
#'               tcol='white',
#'               ratio=0.5,
#'               filename='gradient_icon')
#'
#' # Use function with the generated icon and legend
#' kml2(obj,altitude='altitude',colour='colour',heading='heading',size='variable_size',icon="icon.png",filename='test.kml',legend_icon='gradient_icon.png')
#' kml2kmz(files_to_zip=c('icon.png','test.kml','gradient_icon.png'),filename='track_kmz')
#' 
#' #### If you want to change the icon you can store a png in the same folder 
#' #### as where the kml is generated and specify the icon's name (e.g., arrow.png) 
#' #### in the kml function
#' kml2(obj,altitude='altitude',colour='colour',heading='heading',size='variable',icon="arrow.png",filename='track_kml.kml', legend_icon='gradient_icon.png')
#' Refer to the icon's name or directory so that the function knows where to find the png
#' This won't work unless you have a png named 'arrow.png' in the same folder
#' kml2kmz(files_to_zip=c('arrow.png','track_kml.kml','gradient_icon.png'),filename='track_kmz') 

 # set your working directory to where your image is 
 kml2kmz <- function(files_to_zip = c('icon.jpg','track_kml.kml'), filename='track_kmz'){
 zip(zipfile = filename, files = files_to_zip)
 file.rename(paste0(filename,'.zip'), paste0(filename,'.kmz'))
 }
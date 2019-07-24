#' kml2
#'
#' Generates a kml including the heading

#' @param obj spatialpointsdataframe with the parameters 
#' @param altitude altitude column in the data frame of obj
#' @param colour column that represents the colours in the data frame of obj
#' @param heading column that represents the heading in the data frame of obj
#' @param size variable that represents the size in the data frame of obj
#' @param size_scale rescaling min and max value
#' @param icon image weblink readible by plotKLM or an image you want to use as icon 
#' @param filename output file name of the kml
#' @param legend_icon png that represents the legend 

#' @keywords kml, heading
#' @export

#' @examples 
#' # Prepare a dataframe 
#' obj <- data.frame(altitude=1:100,colour=heat.colors(100),variable_colour=1:100,heading=seq(1,365,length.out=100),variable_size=runif(100),longitude=seq(15.202020, 16.202020,length.out=100), latitude=seq(45.459069,46.543523,length.out=100),stringsAsFactors = FALSE)
#' coordinates(obj) <- c('longitude','latitude')
#' proj4string(obj)<- CRS("+init=epsg:4326")

#' # Example using png plucked from the internet 
#' kml2(obj,altitude='altitude',colour='colour',heading='heading',size='variable_size',icon="http://maps.google.com/mapfiles/kml/shapes/airports.png",filename='test.kml')

#' # Example using png locally stored
#' # first generate an icon
#' icon(pch=21, col='black',bg='white',filename='icon')
#' # Use function with the generated icon
#' kml2(obj,altitude='altitude',colour='colour',heading='heading',size='variable_size',icon="icon.png",filename='test.kml')
#' # Example that also includes a legend
#' # generate a gradient
#' colors_legend <- unique(obj@data[order(obj$variable_colour),c('colour','variable_colour')])
#' gradient_icon(col=colors_legend$colour, 
#'               labels = round(c(min(colors_legend$variable_colour),max(colors_legend$variable_colour)),2),
#'               bars=5,
#'               title='title',
#'               tdist=0.2,
#'               tcol='white',
#'               res2=30,
#'               ratio=0.5,
#'               filename='gradient_icon')
#' kml2(obj,altitude='altitude',colour='colour',heading='heading',size='variable_size',icon="icon.png",filename='test.kml',legend_icon='gradient_icon.png')
#' kml2kmz(c('icon.png','test.kml','gradient_icon.png'),'test_kmz')


kml2 <- function(obj,altitude,colour,heading,size,size_scale=c(0.5,2),icon,filename='text.kml', legend_icon=NULL){
  require(sp)
  require(plotKML)
  require(scales)
  
  obj$rownumber <- 1:nrow(obj)
  require("XML")
  pnt.kml <- newXMLNode('kml')
  h2 <- newXMLNode("Document", parent = pnt.kml)
  h3 <- newXMLNode("name", "flight", parent= h2)
  h4 <- newXMLNode("Folder", parent=pnt.kml[["Document"]])
  txtc <- paste0('<Placemark><name/><styleUrl>#pnt',obj$rownumber[2:nrow(obj)],
                 '</styleUrl><Point><extrude>1</extrude><altitudeMode>relativeToGround</altitudeMode><coordinates>'
                 ,coordinates(obj)[2:nrow(obj),1],',',coordinates(obj)[2:nrow(obj),2],',',round(obj@data[2:nrow(obj),altitude]),'</coordinates></Point></Placemark>')
  style <- paste0('<Style id="', 'pnt',obj$rownumber[2:nrow(obj)],'"><LabelStyle><scale>',0.5,'</scale></LabelStyle><IconStyle><color>',col2kml(obj@data[2:nrow(obj),colour]),'</color><heading>',obj@data[2:nrow(obj),heading],'</heading>
                <scale>',rescale(obj@data[2:nrow(obj),size],size_scale),'</scale>
                <Icon><href>',icon,'</href></Icon></IconStyle><BalloonStyle><text>$[description]</text></BalloonStyle></Style>')

  txtc_start <- paste0('<Placemark><name/><styleUrl>#pnt',1,
                 '</styleUrl><Point><extrude>1</extrude><altitudeMode>relativeToGround</altitudeMode><coordinates>'
                 ,coordinates(obj)[1,1],',',coordinates(obj)[1,2],',',round(obj@data[1,altitude]),'</coordinates></Point></Placemark>')
  style_start <- paste0('<Style id="', 'pnt',1,'"><LabelStyle><scale>',0.5,'</scale></LabelStyle><IconStyle><color>',col2kml(obj@data[1,colour]),'</color>
                <scale>',rescale(obj@data[1,size],size_scale),'</scale>
                <Icon><href>','http://maps.google.com/mapfiles/kml/pal2/icon18.png','</href></Icon></IconStyle><BalloonStyle><text>StarTrack</text></BalloonStyle></Style>')
  if(is.null(legend_icon) == FALSE){
  legend <- paste0('<ScreenOverlay><name>Legend: Gradient</name><Icon><href>',legend_icon,'</href></Icon>
    <overlayXY x="0" y="1" xunits="fraction" yunits="fraction"/>
    <screenXY x="0" y="1" xunits="fraction" yunits="fraction"/>
  <rotationXY x="0" y="0" xunits="fraction" yunits="fraction"/>
  <size x="0" y="0" xunits="fraction" yunits="fraction"/></ScreenOverlay>')
  }

  parseXMLAndAdd(style_start, parent = h4)
  parseXMLAndAdd(txtc_start, parent = h4)
  parseXMLAndAdd(style, parent = h4)
  parseXMLAndAdd(txtc, parent = h4)
  if(is.null(legend_icon) == FALSE){
  parseXMLAndAdd(legend, parent = h4)
  }

  XML::saveXML(pnt.kml, filename)
 }

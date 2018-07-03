#'
#'
#' Raster plot of streamflow data. This produces a plot showing the flow data in colours 
#' Raster layout lets the use see the data in a different context than in 
#' a hydrograph.
#' 
#' @description {
#' Produces a raster plot: years against day of year, showing magnitude of flow data
#'}
#' @param dframe - a datafile from water survey read by read.wsc()


#' @author Paul Whitfield <paul.h.whitfield@gmail.com>


#' Produces a colour plot of observations with colour symbol high flows in warm colours
#'
#' @export 
#' 
#' 
#' @examples
#' data(W05AA008)
#' qplot <-raster_qplot(W05AA008)
#' 



raster_qplot <- function( dframe, rastercolours=c("lightblue","cyan", "blue", "slateblue", "orange", "red")) {
  
  
  ##### Fixed labels and text strings
  DOY <- "Day of Year"
  ylabelq=expression(paste("Discharge m" ^{3}, "/sec"))
  
  qcols=grDevices::colorRampPalette(rastercolours)
  
  station <- as.character(dframe$ID[1])
  sname<- get_wscstation(station)
  title=sname$Station_lname
  
  date <-as.Date(dframe$Date,"%Y/%m/%d")
  
  Year <- as.numeric(format(date,"%Y"))
  doy <- as.numeric(timeDate::dayOfYear(timeDate::as.timeDate(date)))
  
  mYear <-max(Year, na.rm=TRUE)
  nYear <-min(Year, na.rm=TRUE)-1
  Years <-mYear-nYear
  
  qdata <- array(dim=c(366, Years))

  for (k in 1:length(dframe[,1])) {
  
    qdata[doy[k],(Year[k]-nYear)] <- dframe$Flow[k]
   
  }
  
 
  
  qmax <-max(qdata, na.rm=TRUE)
  qmin <-min(qdata, na.rm=TRUE)
  
  
  ################################   raster map of daily flows
  qdata <-as.matrix(qdata)

  ########################################################### start  plotting section
 graphics::par(oma=c(0,0,3,0))
 graphics::layout(matrix(c(1,1,1,1,2,1,1,1,1,3), 2, 5, byrow = TRUE))
 graphics::par(mar=c(4,4,1,1))

  #################################################################  panel one

  doys <-c(1:366)
  lyears <- c((nYear+1):mYear)

    graphics::image(1:366,1:length(lyears), qdata, axes=FALSE, col=qcols(9),
                  zlim=c(qmin,qmax),  xlab="", ylab="")
                  
    sdoy <- sub_set_Years(doys,10)
    graphics::axis(1, at=sdoy$position,labels=sdoy$label, cex=1.2)
     
    if(length(lyears)>=70) nn<-10 else nn<-5
    sYears <- sub_set_Years(lyears,nn)
     
     graphics::axis(2, at=sYears$position,labels=sYears$label, cex.axis=1.2, las=1)
     graphics::mtext(DOY,side=1, line =2.2, cex=0.9)
     
     graphics::box()
     

  #################################################################  panel two
  graphics::frame()
  graphics::par(mar=c(2,2,2,2))
  

  ######### scale bar and legend
  

  fields::image.plot(zlim= c(qmin,qmax), col=qcols(9), legend.only=TRUE,
                     legend.width=4, legend.mar=1, 
                     legend.shrink=1.0,
                     bigplot=c(0.1,0.2,0.1,0.2),
                     legend.args=list(text=ylabelq, side=2,line=0.5, cex=0.90))
  
  #################################################################  panel three (element #ten)

  graphics::frame()
  graphics::frame()
  graphics::frame()
  graphics::frame()
  graphics::frame()

  
  

  ##############################################################  Add title
  tscale=1.7
  if(nchar(title)>=45) tscale=1.5
  if(nchar(title)>=50) tscale=1.2
  graphics::mtext(title, side=3, line=0, cex=tscale,outer=TRUE)
  

  ############################################################### end plotting section
  
  return()
}
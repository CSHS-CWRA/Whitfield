#' Raster plot and simple trends of WSC observations
#' 
#' Creates a raster plot plus trend plots for day of year, and over time
#' which may be binned by a number of days
#'
#' Plot contains four panels based upon binned data. 
#' 1. the annual maximum,minimum,and median with a trend test #' 
#' for each period: red arrows indicate decreases, blue arrows indicate increases.
#' 2. scale bar for the colours used in the raster plot, 
#' 3. the rasterplot with a colour for each 
#' period and each year where data exist, and 
#' 4. a time series plot of the minimum, median, and maximum annual bin values.
#' If there is no trend the points are black, decreasing is red, 
#' and increasing in blue. 
#' p<=0.05. 
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' 
#' @param Year - array of length n containing the Year as numeric
#' @param doy - array of length n containing the numeric day of year
#' @param Flow - array of length n containing the flow data
#' @param step - a number indicating the degree of smoothing eg. 1, 5, 11.
#' @param title - uses format of 05BB001 - Bow River at Banff AB* from get_WSCstation
#' @param missing - If FALSE years with missing data are excluded. If TRUE partial years
#' are included.
#' @param colours Default is c("lightblue","cyan", "blue", "slateblue", "darkblue", "red"). 
#' Can be changed to another series as desired.
#
#' @return a list containing:
#' 	\code{stationID}  Station ID eg 05BB001
#' 	\code{missing} How missing values were used FALSE=used ,TRUE=removed
#'	\code{step} # of days in a bin
#'	\code{periods} # of periods in a year
#'	\code{period} period numbers i.e. 1:365/step
#'	\code{bins} values for each period in each year
#'	\code{med_period} median for each period
#'	\code{max_period} maximum for each period
#'	\code{min_period} minimum for each period
#'	\code{tau_period} Kendall's Tau for each period
#'	\code{prob_period} pr0bability of Tau for each period
#'	\code{year} years spanning the data
#'	\code{median_year} median bin for each year
#'	\code{max_year} maximum bin for each year
#'	\code{min_year} minimum bin for each year
#'	\code{tau_median_year} value of tau and probability for median_year
#'	\code{tau_maximum_year} value of tau and probability for max_year
#'	\code{tau_minimum_year} value of tau and probability for min_year
#' @export 
#' 
#'  
#' @examples
#' 
#'  data(W05AA008)
#'  dd <- W05AA008
#'  mdoy <- doys(dd$Date)
#'  mplot <- raster_trend(mdoy$year, mdoy$doy, dd$Flow, step=5, station="05AA008")
#' 
#



## requires function slice


raster_trend <- function(Year, doy, Flow, step=5,  station, missing=FALSE,
                         colours=c("lightblue","cyan", "blue", "slateblue", "darkblue", "red")) {
  
  sname<- get_wscstation(station)
  title=sname$Station_lname
  
  l_disch  <- expression(paste("m" ^{3}, "/sec"))
  l_disch2 <- expression(paste("\nm" ^{3}, "/sec"))
  DOY   <- paste("Period of Year (",step," day)", sep="")
  
  if(step>=31){
    print("step has been reset to the maximum allowed [30] ")
    step<- 30
  }
  
  days<-365
  periods <- days/step
  periods <- round(periods,digits=0)
  period <- c(1:periods)
## Some records have stretches of missing years so the data needs to be reconfigured to individual years which have no record.
  
  mYear <- max(Year, na.rm=TRUE)
  nYear <- min(Year, na.rm=TRUE)-1
  nYears <- mYear-nYear    ## total number of years
  Years <- c((nYear+1):mYear)  ## all years in range
  aYears <- unique(Year)   ## actual years in range
  
  mslice <- slice(doy,step)		###  create a factor for n day periods
  myear <- as.factor(Year)
  fac   <- list(myear,mslice)
  
  q_sliced <-tapply(Flow, fac, stats::median)  # get median value for each bin.


  qsliced <-array(dim=c(periods,nYears))
  
  for (k in 1:length(aYears)){
    for (kk in 1:periods){
    qsliced[kk,(aYears[k]-nYear)] <- q_sliced[k,kk]
  }
  }
  

  
  colnames(qsliced) <- Years
  rownames(qsliced) <- period
  
  qmin <- min(Flow, na.rm=TRUE)
  qmax <- max(Flow, na.rm=TRUE)
  
  med_n <- array(NA, length(period))
  max_n <- array(NA, length(period))
  min_n <- array(NA, length(period))
  
  tau  <- array(NA,length(period))
  prob <- array(NA, length(period))
  code <- array(2,length(period))
  arrow <- array(1,length(period))
  
  for( i in 1:length(period)) {  ### loop over getting values for periods of year
    
    med_n[i] <- stats::median(qsliced[i,], na.rm=TRUE)
    max_n[i] <- max(qsliced[i,], na.rm=TRUE)
    min_n[i] <- min(qsliced[i,], na.rm=TRUE)
    
    max_n[is.infinite(max_n)] <- NA 
    min_n[is.infinite(min_n)] <- NA 
    
    
    t1 <- NA
    t1 <- Kendall::MannKendall(qsliced[i,])
    tau[i] <- t1$tau
    prob[i] <- t1$sl
    
    # set flags for plotting   
    if(abs(prob[i])== 1.00) code[i]=1
    if(prob[i]<=0.05) code[i]=3
    
    if(prob[i]<=0.05) arrow[i]=2
    if(prob[i]<=0.05 && tau[i]<=0.) arrow[i]=3
    
  }
  

  
  ymed_n <- array(NA, length(Years))
  ymax_n <- array(NA, length(Years))
  ymin_n <- array(NA, length(Years))
  
  for( i in 1:length(Years)) { 			### loop over getting values for each year
    
    ymed_n[i] <- stats::median(qsliced[,i], na.rm=missing)
    ymax_n[i] <- max(qsliced[ ,i], na.rm=missing)
    ymin_n[i] <- min(qsliced[ ,i], na.rm=missing)
    
  }
  ############################# replace -Inf with NA
  ymax_n[is.infinite(ymax_n)] <- NA 
  ymin_n[is.infinite(ymin_n)] <- NA 
  
  tcol<- c("red","black","blue")
  tmy   <- Kendall::MannKendall(ymed_n)
  tminy <- Kendall::MannKendall(ymin_n)
  tmaxy <- Kendall::MannKendall(ymax_n)
  
  t1 <- ifelse(as.numeric(tmy[2])  >0.05,2,ifelse(tmy[1]  >=0,3,1))
  t2 <- ifelse(as.numeric(tminy[2])>0.05,2,ifelse(tminy[1]>=0,3,1))
  t3 <- ifelse(as.numeric(tmaxy[2])>0.05,2,ifelse(tmaxy[1]>=0,3,1))
  

 #####################################################  three panel output
  
  graphics::par(oma=c(0,0,3,0))
  qcols=grDevices::colorRampPalette(colours)
  
  nf <- graphics::layout(matrix(c(2,4,1,3),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)
  
  
 #####################################################  panel one raster image
  graphics::par(mar=c(4,4,0,0))
  
     graphics::image(1:periods,1:length(Years), qsliced, axes=FALSE, col=qcols(9),
                  zlim=c(qmin,qmax),  xlab="", ylab="")  
     
     sstep <-round(periods/5)
     speriod <- sub_set_Years(period,sstep)
     graphics::axis(1, at=speriod$position,labels=speriod$label, cex=1.2)
     
     nn<-1
     if(length(Years)>=70) nn <- 10 
     if(length(Years)>=40) nn <-  5
     if(length(Years)>=20) nn <-  2     
     sYears <- sub_set_Years(Years,nn)
    
     graphics::axis(2, at=sYears$position,labels=sYears$label, cex.axis=1.2, las=1)
     graphics::mtext(DOY,side=1, line =2.2, cex=0.9)  
     graphics::box()

 #####################################################  panel two doy summary of trends
  mch   <- c("",1,19)
  mch_n <- c("",173,175)
  mcolour <- c("white","blue","red")
  
  ylimits<- c(min(qsliced,na.rm=TRUE),max(qsliced, na.rm=TRUE))
  graphics::par(mar=c(1,4,0,0))
  graphics::plot(period,med_n, ylab=l_disch, col="black", ylim=ylimits, xaxt="n", xaxs="i", las=1,pch=as.numeric(mch[code]))
  graphics::points(period,max_n, type="l", col="gray35")
  graphics::points(period,min_n, type="l", col="gray35")
  graphics::par(font=5)
  graphics::points(period,med_n, type="p", col=mcolour[arrow], pch=as.numeric(mch_n[arrow]), cex=1.2)
  graphics::par(font=1)


 #####################################################  panel three time series
  options(scipen = 999)
  
  xy<- c(1:length(Years))
  ylimits<- c(min(qsliced,na.rm=TRUE),max(qsliced, na.rm=TRUE))
  if(ylimits[1]==0) (ylimits[1]<-0.001)
  graphics::par(mar=c(4,1,0,0))
  graphics::plot(ymed_n,xy,  col=tcol[t1],xlim=ylimits, xlab=l_disch, yaxt="n", yaxt="n",yaxs="i", log="x", ylab="")
  graphics::points(ymax_n, xy, col=tcol[t3], pch=19, cex=0.7)
  graphics::points(ymin_n, xy, col=tcol[t2], pch=19, cex=0.7)
  
  ########################################################  Add title
  tscale=1.6
  if(nchar(title)>=45) tscale=1.4
  if(nchar(title)>=50) tscale=1.2
  graphics::mtext(title, side=3, line=1, cex=tscale,outer=TRUE)
  
  
  ########################################################  Add scalebar
  graphics::frame()
  
  graphics::par(mar=c(0,0,0,4))
  
  zr=c(qmin,qmax)
  
  fields::image.plot( zlim= zr, col=qcols(9),legend.only=TRUE,
                      legend.width=4.5, legend.shrink=0.8,
                      bigplot=c(0.1,0.2,0.1,0.2),
                      legend.args=list(text=l_disch, side=2,line=0.5, cex=0.9))
  
  sID <- substr(title,1,7)
  
  
  line1 <-list(sID, missing, step, periods,qsliced,period,med_n,max_n,min_n,tau,prob, Years, 
               ymed_n,ymax_n,ymin_n, tmy, tmaxy,tminy)
  names(line1) <-c("sID","na.rm=", "step", "periods", "bins","period", "med_period","max_period","min_period", 	"tau_period", "prob_period",
                   "year", "median_year", "max_year", "min_year","tau_median_year", "tau_maximum_year", "tau_minimum_year")
  return(line1)
  
}


#### PH Whitfield 2017-11-06

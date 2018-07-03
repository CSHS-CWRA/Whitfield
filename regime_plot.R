#' regime_plot
#'
#' Produces a regime plot similar to that in the reference. It shows the flow quantiles for each 
#' day of the year and the maximum and minimum. Parameters can be set to change colours and fix a y scale 
#' to allow plots of same scale to be produced.
#'
#'
#' @param {flow} { array of flows}
#' @param {year} { array of years}
#' @param {doy}  { array of days of year or wateryear }
#' @param {title} { text to be used as a title }
#' @param {q} { quantiles; default is q= c( 0.1, 0.9)}
#' @param {wyear} {set wyear=10 for October water year, =1 for calendar year}
#' @param {colour} {if FALSE resulting plot is grayscale}
#' @param {mx} {set the maximum y value; if =1 then series max is used to set ylimit[2].
#' This allows one to produce a series of plots with the same scale.}
#' 
#' @author Paul Whitfield
#' @export
#' 
#' @references MacCulloch, G. and P. H. Whitfield (2012). Towards a Stream Classification System 
#' for the Canadian Prairie Provinces. Canadian Water Resources Journal 37: 311-332.
#' 
#' @examples
#' data(W05AA008)
#' mdoy <-doys(W05AA008$Date)
#' test <-regime_plot(W05AA008$Flow,mdoy$year,mdoy$doy, title="05AA008", colour=FALSE, wyear=0)
#'

regime_plot <-function(flow, year, doy, title,  wyear=1, colour=TRUE, mx=1) {
  
  
  ############################################################################# labels
  dmf = expression(paste("Mean Daily Discharge m(" ^{3}, "/sec)"))
  
  doys=366
  doy1<-c(1:doys)
  years <-unique(year)
  nyears <-max(years)-min(years)+1
  min_year<-min(years)-1
  ############################################################################# arrays
  q <-array(NA, dim=c(nyears,doys))
  
  
  colr <-c("gray70", "gray50", "gray30", "black", "gray10")
  if(colour==TRUE) colr <-c("gray", "cyan", "deepskyblue2", "red", "darkblue")
  ############################################################################ create table of year of daily discharge
  for (k in 1:length(year)){
    q[(year[k]-min_year),doy[k]] <-flow[k]
  }
  
  
  qquantiles <- c(0.95,0.9,0.75,0.5,0.25,0.1,0.05)
  qquantiles <-rev(qquantiles)
  
  regime <-array(NA,dim=c(9,doys))
  
  for (jj in 1:doys) {
    regime[1,jj] <- min(q[,jj], na.rm=TRUE)
    regime[9,jj] <- max(q[,jj], na.rm=TRUE)
    for( j in 2:8){
      regime[j,jj] <-quantile(q[,jj],probs=qquantiles[j-1], na.rm=TRUE)
    }
  }
  
  ############################  need to replace Inf and -Inf with NA  Infs come from all days being NA
  regime[is.infinite(regime)] <-NA
  
  ###########################  create polgons for 0.95,0.05   0.90,0.1    0.75,0.25 
  ylims <- c(0,mx)
  if(mx==1) ylims <- c(0,max(flow, na.rm=TRUE))
  
  mdays <-c(doy1,rev(doy1))
  poly1 <- c(regime[2,],rev(regime[8,]))
  poly2 <- c(regime[3,],rev(regime[7,]))
  poly3 <- c(regime[4,],rev(regime[6,]))
  
  ######################################################################### plot start
  par(mar=c(3,5,3,1))
  plot   (doy1, regime[9,],type = "p", xlab = "", xaxt='n',col=colr[4],cex=0.5, ylab = dmf, ylim=ylims, xlim=c(1,366), main=title)
  axis_doy(wyear)
  polygon(mdays, poly1, col = colr[1], border = colr[1])
  polygon(mdays, poly2, col = colr[2], border = colr[2])
  polygon(mdays, poly3, col = colr[3], border = colr[3])
  points(doy1, regime[1,], type="p", col=colr[4], cex=0.5)
  points(doy1, regime[5,], type="l", col=colr[5], lwd=3)
  ltext1 <-c("min/max", "05-95%", "10-90%", "25-75%", "Median")
  lcol1 <-c(colr[4],colr[1],colr[2], colr[3],colr[5])
  legend ("topleft", legend=ltext1, col=lcol1, lty=1, lwd=3, bty='n')
  ######################################################################### plot end
  
}




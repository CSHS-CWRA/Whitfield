#' Flow Duration Curve based upon Observations
#' 
#' A flow duration curve is a plot of flow magnitude against exceedance probability.
#' The plot may contain the Gustard Curves or they can be omitted. The default is for 
#' curves to be plotted against probability, but an option is to plot against the normalized
#' exceedance probability. In that case, the x axis represents a normal distribution.
#' 
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#'
#' @param flow - array of length n containing the flow data
#' @param title : uses format of 05BB001 - Bow River at Banff AB* from get_wscstation
#' @param normal : FALSE is default, if normal=TRUE then exceedance probability is normalized
#' @param gust : True is default,  adds the curves from Gustard et al 1992; if FALSE they are omitted
#'
#' @return dataframe containing exceedance_probabilty and flow
#'
#' @references 
#' Gustard, A., A. Bullock, and J.M. Dixon. 1992. Low flow estimation in the United Kingdom. 
#' Institute of Hydrology, 292. Wallingford: Institute of Hydrology.
#' 
#' Vogel, R.M., and N.M. Fennessy. 1994. Flow-duration curves. I: New Interpretation and 
#' confidence intervals. Journal of Water Resources Planning and Management ASCE 120:485-504.
#' 
#' Vogel, R.M., and N.M. Fennessy. 1995. Flow duraction curves II: A review of applications 
#' in water resources planning. Water Resources Bulletin 31:1030-9.
#'
#'
#' @export 
#' @examples
#' data(W05AA008)
#' flow <-W05AA008$Flow
#' test <-fdcurve(flow, title="Station", normal=FALSE, gust=TRUE) #with Gustard 1992 curves
#' test <-fdcurve(flow, title="Station", normal=TRUE, gust=FALSE) #with normalized exceedance probability
#

fdcurve <- function(flow, title="", normal=FALSE, gust=TRUE) {
  
  ## load the values for the Gustard curves for %mean flow for Type Curves Gustard et al 1992.
  g <-array(NA,dim=c(19,7))		
  g[1,] <-c( 904.17, 534.08,  22.69,  4.42,  2.13,  1.26,  0.51)
  g[2,] <-c( 838.77, 511.37,  25.10,  5.27,  2.62,  1.58,  0.67)
  g[3,] <-c( 776.04, 480.48,  27.86,  6.33,  3.25,  2.00,  0.88)
  g[4,] <-c( 719.91, 452.42,  30.82,  7.54,  3.99,  2.51,  1.16)
  g[5,] <-c( 667.48, 425.82,  34.11,  9.00,  4.92,  3.16,  1.53)
  g[6,] <-c( 618.22, 400.44,  37.81, 10.77,  6.07,  3.98,  2.02)
  g[7,] <-c( 572.53, 376.64,  41.82, 12.86,  7.47,  5.01,  2.65)
  g[8,] <-c( 520.00, 350.65,  45.10, 15.20,  9.16,  6.30,  3.46)
  g[9,] <-c( 472.29, 326.46,  48.64, 17.98, 11.22,  7.94,  4.52)
  g[10,] <-c( 428.96, 303.93,  52.46, 21.25, 13.75, 10.00,  5.89)
  g[11,] <-c( 389.60, 282.96,  56.57, 25.13, 16.86, 12.57,  7.69)
  g[12,] <-c( 353.86, 263.44,  61.01, 29.71, 20.66, 15.83, 10.03)
  g[13,] <-c( 321.39, 245.26,  65.79, 35.12, 25.32, 19.93, 13.08)
  g[14,] <-c( 291.65, 228.19,  71.00, 41.58, 31.09, 25.13, 17.11)
  g[15,] <-c( 264.89, 212.45,  76.57, 49.16, 38.10, 31.64, 22.32)
  g[16,] <-c( 240.09, 197.49,  82.60, 58.08, 46.67, 39.81, 29.13)
  g[17,] <-c( 206.89, 176.99,  89.91, 67.82, 56.95, 50.13, 39.00)
  g[18,] <-c( 178.28, 158.62,  97.86, 79.21, 69.50, 63.12, 52.22)
  g[19,] <-c( 153.69, 142.20, 106.49, 92.46, 84.77, 79.43, 69.85)
  
  p <-c(.02,.05,.50,.80,.90,.95,.99)
  rank <-rank(flow,ties.method="max")
  rank <- max(rank)-rank
  exceedtime <-1*(rank/(length(flow)+1))
  
  q <-sort(100*flow/mean(flow), decreasing=FALSE)
  exceed <-sort(exceedtime, decreasing=TRUE)
  yl <-"Percent of mean discharge"
  xl <-"Exceedance probability (%)"
  xla <-"Normalized Exceedance probability (%)"
  ylims<-c(1,max(q))
  
  graphics::par(mfrow=c(1,1))
  graphics::par(mar=c(4,4,2,1))
  
  if(normal==TRUE) {
    exceed.z <-stats::qnorm(exceed)
    p.z <-stats::qnorm(p)
    
    xlims <-c(-3,3)
    graphics::plot(exceed.z, q, type='l', lwd=2, col='blue', log="y", xaxt="n",ylim=ylims,xlim=xlims,
         xlab=xla, ylab=yl, las=1, main=title)
    # Draw the normal-probability axis
    probs <- c( 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 
                0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99, 0.995, 0.998, 0.999)
    z.vals <- stats::qnorm(probs)
    graphics::axis(side=1, at=z.vals , labels=probs, line = 0, tck = -0.025, xlab=xla)
    
    graphics::abline(h=0,lty=3)
    graphics::abline(h=100, lty=2, col='red')
    graphics::abline(v=stats::qnorm(0.5), lty=2, col='red')
    
    if(gust==TRUE){
      for(k in 1:19) {
        graphics::points(p.z,g[k,],type='l', col='gray')
        graphics::text(p.z[7],g[k,7],k, col='gray', pos=4, cex=0.7)
      }
      graphics::points(exceed.z, q, type='l', lwd=2, col='blue')
      graphics::text(-1,1.25,"Flow Duration Curve with Gustard's Type Curves",col='gray', cex=0.9, pos=1)
      graphics::text(stats::qnorm(p[7]),g[19,7],"permeable", col='gray', pos=3, cex=0.7)
      graphics::text(stats::qnorm(p[7]),g[1,7],"impermeable", col='gray', pos=1, cex=0.7)
    }
    
  }
  
  
  else{
    xlims <-c(0,1)
    graphics::plot(exceed, q, type='l', lwd=2, col='blue', log="y",ylim=ylims,xlim=xlims,xlab=xl,
         ylab=yl, las=1, main=title)
    
    graphics::abline(v=0,lty=3)
    graphics::abline(h=100, lty=2, col='red')
    graphics::abline(v=0.5, lty=2, col='red')
    
    if(gust==TRUE){
      for(k in 1:19) {
        graphics::points(p,g[k,],type='l', col='gray')
        graphics::text(p[7],g[k,7],k, col='gray', pos=4, cex=0.7)
      }
      graphics::points(exceed, q, type='l', lwd=2, col='blue')
      graphics::text(.1,1.25,"Flow Duration Curve with Gustard's Type Curves",col='gray', cex=0.9, pos=4)
      graphics::text(p[7],g[19,7],"permeable", col='gray', pos=3, cex=0.7)
      graphics::text(p[7],g[1,7],"impermeable", col='gray', pos=1, cex=0.7)
    }
  }
  result <-data.frame(exceed,q)
  names(result) <- c("exceedance_prob", "flow")
return (result)
}

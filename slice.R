#' Converts doy or dwy into a factor that is used to bin data
#' based upon a chosen step size.
#' 
#' Whenever the number of bins does not divide in 365 evenly a message is printed 
#' showing the number of bins created and the number of days added to the last bin
#'
#' @param doy  - an array of the day of calendar year for the dataset
#' @param step - width of bin in days
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @return 
#' {An array of bin numbers that is used as a factor for each day in the dataset
#' prints a message indicating the handling of partial bins 
#' }
#' 
#' imply put, slice is used to convert doy into a factor which is a number of bins per year. 
#' A year can be converted into any number of bins; slice does it based upon a number of days. 
#' So when you send it am array of doy it slices that into bins of the desired width.  For example,
#' if the step is 5. They 365/5 gives 73 bins and becasue of leap years there might be one extra day 
#' added every four years to the final bin.
#' 
#' To illustrate: doy:
#'   1  2  3  4  5  6  7  8  9 10 11  12
#' Bin:
#'   1  1  1  1  1  2  2  2  2  2  3   3
#' 
#' 
#' @export 
#'
#' @seealso binned_MannWhitney, raster_trend
#' 
#' @examples
#' doy <-c(1:365)
#' dice <- slice(doy,30)
#' plot(doy,dice) # first 30 days are 1, 31-60 are 2 etc

slice <- function(doy, step)  #  use day of year 1-365 and slice into periods based upon step 5 11 or ....
{
  
  limit <- floor(366/step)
  period <- floor((doy+step-1)/step)
  
  extra <- 366 -limit*step
  
  for(k in 1:length(doy)) {
    if(period[k]>limit) period[k] <-limit
  }
  
  llevels <-as.character(c(1:limit))
  period <-factor(period, levels=llevels)
  
  print(paste("Bins =",limit," The number of extra points in last bin is up to ",extra, " per year"))
  return (period)
}
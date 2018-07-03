#' Days of Year and Water Year Function
#'
#' @param {Date} {an array as.Date}
#' @param {mon} the month starting the water year, default is 10 (October).
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @return a dataframe with different format dates
#' @return \code{Date}  in Date format
#' @return \code{year}  numeric calendar year
#' @return \code{month} number calendar month
#' @return \code{doy}   numeric day of year
#' @return \code{wyear} numeric day of water year starting on 1 October
#' @return \code{dwy}   numeric day of water year
#' 
#'  Converts an array of dates into a dataframe with date,doy, dowy, Year
#'  Calculates day of year and day of water year since 10.01 of present water year.
#'  uses base::isodate
#'  
#'  
#' @export 
#' 
#' @examples
#' dd <- seq.Date(as.Date("2010-01-01"),as.Date("2018-01-01"),by=1)
#' output <- doys(dd)
#' head(output)
#' 

doys <-function (Date, mon=10) # Date needs to be as.Date
{
  
  if(mon==2) print("Currently restricted to water year starting March to October")
  
  dm <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31 ,30, 31)
  dm <- dm[mon-1]
  
  month <- as.numeric( format(Date, "%m"))
  year  <- as.numeric( format(Date, "%Y"))
  day   <- as.numeric( format(Date, "%d"))
  
  
  doy <-array(NA,dim=length(Date))
  for (k in 1:length(Date)) {
    doy[k] <- as.numeric(ISOdate(year[k],month[k],day[k]) - ISOdate(year[k]-1,12,31))
  }
  
  year1 <- year-1
  
  dwy <-array(NA,dim=length(Date))
  wyear <- array(NA,dim=length(Date))
  
  for (k in 1:length(Date)) {
    if (month[k] >=mon) 	
    {
      dwy[k] <- as.numeric(ISOdate(year[k],month[k],day[k]) - ISOdate(year[k],mon-1,dm))
      wyear[k] <-year[k]+1
    }
    else {
      dwy[k] <- as.numeric(ISOdate(year[k],month[k],day[k]) - ISOdate(year1[k],mon-1,dm))
      wyear[k] <-year[k]
    }
  }
  dowy <- data.frame(Date, year, month, doy, wyear, dwy)
  
  return (dowy)
}

#' A helper for wsc data using read.csv returns a dataframe of the data
#' with the last three lines removed.  
#' Those contain the EC disclaimer.

#' read_wsc
#'
#' @param filename Datafile retrieved from ECDataExplorer
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#'
#' @return a dataframe with the last three rows removed and the Date as Date
#' @export
#'
#' @examples
#' mdata <- read_wsc("05AA008_Daily_Flow_ts.csv")
#' 

  read_wsc <- function(filename) {
  mdata <- utils::read.csv(filename)
  mdata$Date <-as.Date(mdata$Date, format="%Y/%m/%d")
  cut <- length(mdata[,1])-3
  mdata <- mdata[1:cut,]
  return(mdata)
}

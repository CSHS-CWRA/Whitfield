#' cut_block
#' 
#' Allows the user to select a time period from a longer record. Could be used to
#' get the same period of time from several station for comparison.
#'
#'
#' @param dataframe streamflow dataframe with a Date column
#' @param st_date starting date format is "%Y/%m/%d"
#' @param end_date ending date format is "%Y/%m/%d"
#' 
#'
#' @export
#' @author Paul Whitfield
#' @examples 
#' subset <- cut_block(W05AA005,2000/01/01, 2010/12/31)

cut_block <- function ( dataframe, st_date, end_date) {
  
  st_date  <- as.Date(st_date, format="%Y/%m/%d")
  if(!st_date >= min(dataframe$Date)){
    print(paste("Starting Date",st_date, "is before records are available"))
    return()
  }
  
  if(!end_date <= max(dataframe$Date)){
    print(paste("Ending Date",st_date, "is after records are available"))
    return()
  }
  
  end_date <- as.Date(end_date, format="%Y/%m/%d")
  
  result1 <- dataframe[dataframe$Date>=st_date,]
  
  
  
  result <- result1[result1$Date<=end_date,]
  print(paste("between",st_date,"and", end_date, length(result[,1]), 
              "records were selected"))
  
  return(result)
}
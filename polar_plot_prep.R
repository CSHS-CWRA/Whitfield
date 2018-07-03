#' polar_plot_prep
#' 
#' Used to create a data structure to be passed to polar_plot.
#' Could be used to move data from a different type of analysis different to the binned_mann_whitney
#' 
#' This makes passing output from a different method to the plotting function.
#' 
#' The two series need to be of the same length and that length related to the step size.  For examples 73 periods links to 5 
#' day periods.
#' 
#' @param station typically a station number
#' @param plot_title text to appears on polar plot usually this will be a station name
#' @param step the number of days binned
#' @param x a time series of length n for a single seasonal cycle
#' @param x1 a time series of length n for a single seasonal cycle
#' @param stat a time series of length n for statistical test value for each bin
#' @param prob a time series of length n of probability of test value
#' @param test_s an array of length n with values of -1, 0, 1 for significance, -1 negative, 1 positive, 0 not significant
#' @param variable default is "discharge"
#' @param bin_method default is "unstated"
#' @param test_method default is "unstated"
#' @param lline1 names of first period, default is "Period 1"
#' @param lline2 names of second period, default is "Period 2"
#'
#' @references 
#' Whitfield, P.H. and A.J. Cannon. 2000. Polar plotting of seasonal hydrologic 
#' and climatic data. Northwest Science 74: 76-80.
#' 
#' Whitfield, P.H., Cannon, A.J., 2000. Recent variations in climate and hydrology 
#' in Canada. Canadian Water Resources Journal 25: 19-65. 

#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#'
#' @export
#'
#' @example
#' 
#' 

polar_plot_prep <- function (station, plot_title, step, x0, x1, stat, prob, test_s, variable="discharge", 
                             bin_method="unstated",  test_method="unstated",
                             lline1="Period 1", lline2="Period 2", pvalue=0.05)
{
  fail=FALSE
  
  if(length(x0)!=length(x1)) return(paste( "Arrays of x unequal length",length(x0),length(x1)))
  if(length(x0)!=length(stat))  return(paste( "Arrays of x0 and stat unequal length",length(x0),length(stat)))
  if(length(x0)!=length(prob))  return(paste( "Arrays of x0 and prob unequal length",length(x0),length(prob)))
  if(length(x0)!=length(test_s))  return(paste( "Arrays of x0 and test_s unequal length",length(x0),length(test_s)))
  
  period <-c(1:length(x0))
  
  
  series <-data.frame(period,x0,x1,stat,prob,test_s)
  names(series) <- c("period","period1","period2","stat","prob","code")
  
  
  result <-list(station,plot_title,variable, step, lline1,lline2, pvalue, fail, bin_method, test_method,series)
  
  names(result) <-c("StationID","Station_lname", "variable", "bin_width","range1","range2",
                    "p_used","fail", "bin_method", "test_method","series")
  return(result)
  
  
}



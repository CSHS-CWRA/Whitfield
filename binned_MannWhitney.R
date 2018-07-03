#' Compares two time periods of data using Mann-Whitney.
#' 
#' It bins data based upon a bin size, extracting data for two time periods 
#' and tests for change between two such periods

#' result can be passed to polar_plot for visualization
#'
#' @author Paul Whitfield <paul.h.whitfield@gmail.com>
#' @references
#' Whitfield, P.H., Cannon, A.J., 2000. Recent variations in climate and 
#' hydrology in Canada. Canadian Water Resources Journal 25: 19-65. 

#' 
#' @param mdata - hydrometric data file
#' @param step - a number indicating the degree of smoothing eg. 1, 5, 11.
#' @param range1 - first and last year of first period c(first,last)
#' @param range2 - first and last year of second period c(first,last)
#' @param ptest - significance level default is p=0.05
#' @param fail - if eries contain mising values fail is set to TRUE
#
#' @return a list containing:
#' 	\code{period} period numbers i.e. 1:365/step
#'	\code{period1} median values for each bin in period 1
#'	\code{period2} median values for each bin in period 2
#'	\code{mwu} Mann Whitney U for each bin between the two periods
#'	\code{prob} probability of U for each period
#'	\code{code} significance codes for each bin

#' @export 
#' 
#'  
#' @examples
#' data(HYDAT_list)
#' data(W05AA008)
#' dd <-W05AA008
#' mdoys <- doys(dd$Date)
#' range1 <- c(1960,1969)
#' range2 <- c(1990,1999)
#' mplot <- binned_MannWhitney(W05AA008, step=5, range1, range2, ptest=0.05)
#' #fails due to missing data in both periods
#'
#' range1 <- c(1970,1979)
#' range2 <- c(1990,1999)
#' mplot <- binned_MannWhitney(dd, mdoys$doy, step=5, range1, range2, ptest=0.05)
#' 


binned_MannWhitney <- function(mdata, step, range1, range2, ptest=0.05, variable="discharge") {
  
  fail=FALSE
  mdoy <-doys(mdata$Date)
  doy <- mdoy$doy
  years <-mdoy$year
  
  flow <- mdata$Flow
  sID <-as.character(mdata[1,1])
  
  binmethod="median"
  testmethod="Mann-Whitney U"
  
  days <- 365
  periods <- days/step
  periods <- round(periods,digits=0)
  period <- c(1:periods)
  ## Some records have stretches of missing years so the data needs to be reconfigured to individual years which have no record.
  
  mYear <- max(years, na.rm=TRUE)
  nYear <- min(years, na.rm=TRUE)-1
  nYears <- mYear-nYear    ## total number of years
  Years <- c((nYear+1):mYear)  ## all years in range
  aYears <- unique(years)   ## actual years in range
  
  
  mslice <- slice(doy,step)		###  create a factor for n day periods
  myear <- as.factor(years)
  fac   <- list(myear,mslice)
  qsliced <-array(dim=c(nYears,periods))
  
  q_sliced <-tapply(flow, fac, stats::median)  # get median value for each bin.
  
  #qliced contains median for periods and for only year where data existed. Need to reform so missing years are included
  
  
  
  for (k in 1:length(aYears)){
    qsliced[(aYears[k]-nYear),] <- q_sliced[k,]
  }
  
  colnames(qsliced) <- period
  rownames(qsliced) <- Years 
  
  # set up arrays for results
  period1 <- array(NA, length(period))
  period2 <- array(NA, length(period))
  mwu <- array(NA, length(period))
  prob <- array(NA, length(period))
  code <- array(0, length(period))
  
  rg1 <- c(range1-nYear)
  rg2 <- c(range2-nYear)
  
  for( i in 1:length(period)) {  ### loop over getting values for periods of year
    
    s1 <- qsliced[rg1[1]:rg1[2],i]
    s2 <- qsliced[rg2[1]:rg2[2],i]
    sout <-  stats::wilcox.test(s1,s2, exact=FALSE)
    
    period1[i] <-median(s1)
    period2[i] <-median(s2)
    mwu[i] <- sout[[1]]
    prob[i] <-sout[[3]]
    if(prob[i] <= ptest) code[i] <- (mean(s1)-mean(s2))/(abs(mean(s1)-mean(s2)))
  }
  
  if(length(period1[!is.na(period1)])!=length(period1))
  {print("Range_1 contains missing values")
    fail=TRUE}
  if(length(period2[!is.na(period2)])!=length(period2)) 
  {print("Range_2 contains missing values")
    fail=TRUE}
  
  series <- data.frame(period,period1,period2,mwu,prob,code)
  
  
  sname <- get_wscstation(sID, stn=HYDAT_list)
  
  result <-list(sID,sname[21],variable, step, range1,range2,ptest, fail, binmethod, testmethod,series)
  
  names(result) <-c("StationID","Station_lname", "variable", "bin_width","range1","range2",
                    "p_used","fail","bin_method", "test_method","series")
  
  return(result)
}


#### PH Whitfield 2018-01-08

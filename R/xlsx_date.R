year_ <- function(dt=Sys.Date()){
   if("Date" %in% class(dt)) dt <- format(dt, "%Y")
   as.numeric(substr(dt,0,4))
}

#' number to date for vector/column from excel sheet with mixed value types
#'
#' readxl::read_excel() on a column with mixed formated cells will not easily parse dates
#' consider read_excel(..., col_types = c("date","guess",...))
#' unless column is mixed valued (e.g. "42068", "1/31/2010,1/2/2011", ...)
#' NB. excel's date origin is 1899-12-30
#' @param x (vector of) would be dates to transform back
#' @export
#' @examples 
#' print(xlsx_date(c(41977,"42068"))) # "2014-12-04" "2015-03-05"
#' xlsx_date("10977") # error out of range (1933-01-14)
xlsx_date <- function(x, msg=NULL, minyear=2000, maxyear=year_()+2) {
   if (length(x) == 0L) return() # otherwise would error at bottom
   if (is.null(msg)) msg <- substitute(x)
   orig <- x # used for error message

   # 20220525 - add num->char but not using
   #            not in original function. might be problematic for some columns?
   #            if was read in as all numeric, would probably already be correct dates?
   # if (is.numeric(x)) x<-as.character(x)

   if (is.character(x)) {
      xn <- as.numeric(x)
      x <- as.Date(xn, origin="1899-12-30")

      # if number is really high, unix epoch instead of xlsx date
      # ifelse converts to numeric? so use this:
      e_i <- !is.na(xn) & xn>10^9
      if (any(e_i))
       x[e_i] <- as.Date(as.POSIXct(xn[e_i], origin="1970-01-01"))
   }

   # provide some warning/error messaging for unexpected dates
   # we're likely to hit here if input isn't actually excel dates
   out_of_range <- !is.na(x) & !dplyr::between(year_(x), minyear, maxyear)
   if (any(out_of_range))
     stop(msg, ": xlsx_date() parse failure (",
          paste(collapse=",", sep="=",
                head(orig[out_of_range], n=3),
                head(x[out_of_range],    n=3)),
          " ...): outside of range ", minyear, "-", maxyear)

   # return as character. ifelse doesn't do well mixing with Date
   format(x, "%Y-%m-%d")
}

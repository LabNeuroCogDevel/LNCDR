#
# Author:  Will Foran
#
#' match dates in a data frame by one-to-many merge and rank. Useful to match behavioral to brain over multiple timepoints.
#' @param d1 -- main dataframe (has the "true" date. one part of one-to-many)
#' @param d2 -- supplimental dataframe with dates to match (many part of one-to-many)
#' @param id -- column in both d1 and d2 (lunaid)
#' @param datecol1 -- date column one, this will be the id column
#' @param datecal2 -- date column two. this will be the many in one-to-many merge. defaults to same name as datacol1
#' @param all.x -- weather or not to keep NAs (b/c match is over maxdatediff or does not existed)
#' @param suffix.y -- what to call the d2 columns that match d1 names
#' @param maxdatediff -- maximum time between matches (default 180 days)
#' @param maxrank -- highest index value of datediff ranked. set to 1 to ingore ties (deafult is 1.999)
#' @export
#' @examples
#'   dummy1 <- data.frame(id=c(1,2,3,3),d=c(20120302,20120302,20120302,20150302),j=1)
#'   dummy2 <- data.frame(id=c(1,2,3,3,3),d=c(20120202,20120402,20120902,20130902,20150102),j2=2)
#'   dummy1$d <- lubridate::ymd(dummy1$d)
#'   dummy2$d <- lubridate::ymd(dummy2$d)
#'   date_match(dummy1,dummy2,'id','d')

date_match <-function(d1, d2, idcol, datecol1, datecol2=datecol1, all.x=F,
                      suffix.y=".y", maxdatediff=180, maxrank=2-1e-3){
   require(dplyr)

   # for error messages
   df_names <- c(substitute(d1), substitute(d2))
   date_col_names <- c(datecol1, datecol2)

   # change name if datecolumns are the same
   # changing now saves us from having .x and .y
   if (datecol1==datecol2) {
      datecol2 <- paste0(datecol1, suffix.y)
      names(d2)[ which(names(d2)==datecol1) ] <- datecol2
   }

   n_uniq_iddate <- length(unique(paste(d1[[idcol]], d1[[datecol1]])))
   n_d1 <- nrow(d1)
   if (n_uniq_iddate < n_d1)
      stop(sprintf("datecol1 '%s' repeats within id (%d unique id+date pairs in %d total rows)\n",
                   datecol1, n_uniq_iddate, n_d1)
           "cannot find a match for every row in first dataframe if id+date not all unique.\n",
           "To resolve, consider merging after date_match on subset of unique id+date:\n",
           "  d1 %>% select(idcol, datecol1) %>%\n",
           "    unique %>% date_merge(d2, 'idcol', 'datecol1') %>% inner_join(d1)")

   # check data time.
   # we dont _need_ columns to be Date type, but it'll probably be an issue
   ctypes <- c(class(d1[[datecol1]]), class(d2[[datecol2]]))
   ctype_matches_date <- grepl("Date", ctypes)
   if (!all(ctype_matches_date)) {
      # very verbose warning
      not_a_date <- date_col_names[!ctype_matches_date]
      df_col_not_date <- c('d1','d2')[!ctype_matches_date]
      instead_is <- ctypes[!ctype_matches_date]
      wrong_types <- paste(not_a_date, instead_is, sep=": ", collapse=", ")
      fixme <- paste0(df_names, '$', date_col_names, ' <- lubridate::ymd(', df_names, '$', date_col_names, ')')
      warning("column(s) not of type 'Date': ", paste(collapse=", ", sep="$", df_col_not_date, not_a_date),
                '. Instead have: ', wrong_types,
                ".\noutput 'maxdatediff' column could be weird.  consider \n",
                "\t", paste(collapse='; ', fixme[!ctype_matches_date]))
   }

   # merge all the d2 onto each potential d1 match
   onetomany <-
      merge(d1, d2, by=idcol, all.x=all.x, suffixes=c("", suffix.y)) %>%
      group_by(.data[[idcol]], .data[[datecol1]]) %>%
      mutate(datediff = .data[[datecol1]] - .data[[datecol2]])  %>%
      mutate(r = rank(abs(datediff)))

   # todo: report ties with warning??

   # NA out one that are too high
   row_na <- which(abs(onetomany$datediff) > maxdatediff)
   col_na <- which(! names(onetomany) %in% names(d1) &
               ! names(onetomany) %in% c(idcol, datecol2, "r", "datediff")) %>%
             unique
   for (r in row_na) onetomany [r, col_na] <- NA

   # remove where datediff is too big
   if (!all.x && length(row_na)>0L) onetomany <- onetomany[-row_na, ]

   returnval <- onetomany %>%
      filter(r<=maxrank) %>%
      select(-r) %>%
      ungroup

   # if we are going to keep datediff around,
   # make sure we know where it came from
   names(returnval)[names(returnval) == "datediff"] <-
      paste0("datediff", suffix.y)


   # check repeated date2 (best match is not unique)
   d2check <- returnval[, c(idcol, datecol2)]
   d2reps   <- duplicated(d2check)
   if (any(d2reps)) {
      dispreps <- merge( returnval[, c(idcol, datecol1, datecol2) ],
                         d2check[d2reps, ],
                         by=c(idcol, datecol2) )
      dfstr <- capture.output( dispreps %>% print.data.frame(row.names=F) ) %>%
         paste(collapse="\n")
      warning("best match is not unique: ",
              "repeated second date for subject+first date\n",
              "Consider switching the order of input arguments.\n",
              " (trying to match all rows of first (d1) to any in second (d2))\n",
              dfstr, "\n")
   }

   return(returnval)
}

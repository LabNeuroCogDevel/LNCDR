#
# Author: Will Foran
# 

#' @import dplyr


# USAGE:
#  source('/Volumes/Zeus/DB_SQL/db_query.R')
#  v   <- db_query('select * from visit limit 2')
#  meg <- db_query(readr::read_file('/Volumes/Zeus/DB_SQL/queries/allMEG2016.sql'))
#
#######
# N.B. care is NOT taken to clean up memory/sockets 
#  ** `db_query` is not intended to be used to run many queries! **
# i.e. proper use of dbDisconnect()
# ( db_query.m shares this limitation )
# see `db_example.R` for correct connection managemnet 
# *OR*
#  add 2 steps to your query calls  like:
#    conn <- lncd_pgconn()
#    p <- db_query('select * from person', conn)
#    v <- db_query('select * from visits', conn)
#    dbDisconnect(conn)

#require(dplyr)
#require(RPostgreSQL)
#require(jsonlite)
#require(tidyr) 




# read password file (~/.pgpass if not specified)
# return info list with names host ... pass
pgpassread <- function(passfile="~/.pgpass") {
 if(!file.exists(passfile)) stop(sprintf('cannot read pass file %s',passfile))
 # TODO: there is no check to make sure passfile matches spec
 info <- 
   # read file in as one string
   # readr::read_file(passfile) %>%
   paste0(collapse='\n',scan('~/.pgpass','character')) %>%
   # assume firstline
   gsub(pattern='\\n.*',replacement='') %>%
   # split from host:port:db:user:pass
   strsplit(':') %>% 
   # flatten list from list of lists to single list
   unlist %>% as.list %>%
   # rename
   `names<-`(c('host','port','db','user','pass'))
}

#' lncd_pgconn -- create connection to lncd postgres database
#' @param  info is a named list with db, host, user, and pass. will be created from ~/.pgpass is left empty
#' @export
#' @examples
#' conn <- lncd_pgconn()
#' # lapply(dbListConnections(RPostgreSQL::PostgreSQL()),dbDisconnect)
lncd_pgconn <- function(info=pgpassread()) {
  conn<-DBI::dbConnect(
         #dbDriver(RPostgreSQL::PostgreSQL),
         RPostgreSQL::PostgreSQL(),
         dbname   = info$db,
         host     = info$host,
         user     = info$user,
         password = info$pass)
  # TODO:
  #  reuse old connections
  #  dbListConnections(RPostgreSQL::PostgreSQL())
  return(conn)
}

#' db_qeury -- query database 
#' @param  query is the sql string to run 
#' @param  conn is the optional connection. will be created from ~/.pgpass is left empty
#' @export
#' @examples 
#'  v   <- db_query('select * from visit limit 2')
#'  meg <- db_query(readr::read_file('/Volumes/Zeus/DB_SQL/queries/allMEG2016.sql'))
db_query <-function(query, conn=lncd_pgconn())  d<-DBI::dbGetQuery(conn, query)

### unnest jsonb
# some table have jsonb objects
#  e.g. `visit_task` has column `measures` that is (unvalidated) jsonb like:
#   {"wbdT": 44.0, "wmrT": 41.0, "Notes": null, "wbdrw": 38.0, ...}
# this is essentally a nested table -- and R has tools for this
#
#json2df.idv <- function(x)  jsonlite::fromJSON(x) %>% unlist %>% t %>% data.frame
# updated 2017-08-25. unlist causes value to be null, so remove unlist
json2df.idv <- function(x)   jsonlite::fromJSON(x) %>% t %>% data.frame  

json2df  <- function(x) lapply(x,FUN=json2df.idv)
unnestWithNull <- function(x) unlist(sapply(x,function(y) ifelse(is.null(y),NA,y)))

#' unnest json(b) object
#' @param  d the dataframe contianing a column to unnest
#' @param  column the column (string, in quotes) containing the json string to be unnested. defaults to 'measures'
#' @export
#' @examples 
#'  v  <- db_query('select * from visit_tasks where task like 'Sen%' limit 2') %>% unnestjson
unnestjson <- function(d,column='measures')  {
   bad <- unname(sapply(d[,column], function(x) {is.null(x) | is.na(x) }))
   valid <-  d[!bad,] 
   if(nrow(valid) < 1L) stop('no non-nil values in ',column,'!')
   newcols <- Reduce(rbind,json2df(valid[,column])) %>% 
              mutate_all(unnestWithNull)
   valid <- cbind(valid, newcols)
   commonnames <- setdiff(names(d),column)
   merge(valid, d[bad,], by=commonnames, all=T)
}

#unnestjson <- function(d,column='measures') { 
#    d[,column] <- json2df(d[,column])
#    tidyr::unnest(d)
#}


#' task_query - return all rows for a given task from psql database
#' @param  task -- like "DrugUse", "Demographics", etc
#' @param  columns -- sql list of columns (default "measures")
#' @export
#' @examples 
#'  rist  <- task_query("RIST", "measures->'rist_ristindex' as ristindex")
#'  demo  <- task_query("Demographics")
task_query <- function(task, columns="measures"){
 d <-
  sprintf("
  select id,
    to_char(vtimestamp,'YYYYMMDD') as ymd,
    vtype, vscore, age,
    %s
  from  visit
  natural join  visit_task
  natural join enroll
  where
    task like '%s' and
    enroll.etype like 'LunaID'
    and measures is not null
  ", columns, task) %>%
  db_query %>%
  mutate(ymd=lubridate::ymd(ymd))

  # get all columns from measures
  if (columns=="measures")
     d <- unnestjson(d) %>% select(-measures.x, -measures.y)

  return(d)
}

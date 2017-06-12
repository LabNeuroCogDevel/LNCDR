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
#    conn <- pgconn()
#    p <- db_query('select * from person', conn)
#    v <- db_query('select * from visits', conn)
#    dbDisconnect(conn)

#require(dplyr)
#require(RPostgreSQL)
#require(jsonlite)
#require(tidyr) 
#require(readr)




# read password file (~/.pgpass if not specified)
# return info list with names host ... pass
pgpassread <- function(passfile="~/.pgpass") {
 if(!file.exists(passfile)) stop(sprintf('cannot read pass file %s',passfile))
 # TODO: there is no check to make sure passfile matches spec
 info <- 
   # read file in as one string
   readr::read_file(passfile) %>%
   # assume firstline
   gsub(pattern='\\n.*',replacement='') %>%
   # split from host:port:db:user:pass
   strsplit(':') %>% 
   # flatten list from list of lists to single list
   unlist %>% as.list %>%
   # rename
   `names<-`(c('host','port','db','user','pass'))
}

pgconn <- function(info=pgpassread()) {
  if(!file.exists(passfile)) stop(sprintf('cannot read pass file %s',passfile))
  conn<-DBI::dbConnect(
         #dbDriver(RPostgreSQL::PostgreSQL), 
         RPostgreSQL::PostgreSQL(), 
         dbname   = info$db,
         host     = info$host, 
         user     = info$user,
         password = info$pass)
}

#' db_qeury -- query database 
#' @param  query is the sql string to run 
#' @param  conn is the optional connection. will be created from ~/.pgpass is left empty
#' @export
#' @examples 
#'  v   <- db_query('select * from visit limit 2')
#'  meg <- db_query(readr::read_file('/Volumes/Zeus/DB_SQL/queries/allMEG2016.sql'))
db_query <-function(query,conn=pgconn())  d<-dbGetQuery(conn, query) 

### unnest jsonb
# some table have jsonb objects
#  e.g. `visit_task` has column `measures` that is (unvalidated) jsonb like:
#   {"wbdT": 44.0, "wmrT": 41.0, "Notes": null, "wbdrw": 38.0, ...}
# this is essentally a nested table -- and R has tools for this
#
json2df.idv <- function(x)  jsonlite::fromJSON(x) %>% unlist %>% t %>% data.frame
json2df  <- function(x) lapply(x,FUN=json2df.idv)
#' unnest json(b) object
#' @param  d the dataframe contianing a column to unnest
#' @param  column the column (string, in quotes) containing the json string to be unnested. defaults to 'measures'
#' @export
#' @examples 
#'  v  <- db_query('select * from visit_tasks where task like 'Sen%' limit 2') %>% unnestjson
unnestjson <- function(d,column='measures')  d %>% mutate(!!column = json2df(!!column) %>% tidyr::unnset

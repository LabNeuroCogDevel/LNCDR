qualtrics_fetch <- function(sid) {
  # work around error by explictly running infer_data_types
  # and returning original dataframe if that fails
  # Error: Problem with mutate() input Q2.
  # is.character(x) is not TRUE
  # Input Q2 is readr::parse_factor(...)
  tryCatch({
    d <- qualtRics::fetch_survey(sid, force=T, convert=F)
    d <- tryCatch(qualtRics:::infer_data_types(d, sid),
                  error=function(e){
                     cat("WARNING: error w/qualtrics:::infer_data_types(...,'",sid,"')\n")
                     return(d)})},
   # if all of this fails, return NULL
   error=function(e) {warning("failed to fetch ",sid, "b/c", e); NULL})
}



#' @name qualtrics_auth
#' @title authenticate with qualtrics using api token in config file
#' @description
#' default config expected in $HOME/.qualtrics.ini
#  looks like
#'   [api]
#'   api_token= 67dfRZ2ibBRPxbLmbxxxxxxxxxxxxxxxxxxxxxxx
#'   root_url = https://pitt.co1.qualtrics.com
#'
#' @param config - where to read qualtrics settings
#' @importFrom ini read.ini
#' @import qualtRics
#' @export
qualtrics_auth <- function(config="~/.qualtrics.ini") {
  if(!file.exists(config))
     stop(paste0("cannot find qualtrics config file: ", config,
                 "\nsee: help(qualtrics_surveys) for ini description"))
  ini <- ini::read.ini(config)
  # qualtRics doesn't want the protocol in base_url
  root_url <- gsub('^https?://', '', ini$api$root_url)

  # qualtRics::qualtrics_api_credentials(api_key=ini$api$api_token, base_url=root_url)
  # readRenviron("~/.Renviron")
  Sys.setenv(QUALTRICS_API_KEY = ini$api$api_token, QUALTRICS_BASE_URL = root_url)
}


#' @name qualtrics_surveys
#' @title pull surveys matching a pattern into a list of dataframes
#' @description
#'  read all surveys from qualtrics accessible to api key provided in config
#'  see help(qualtircs_auth) for more on config format
#'  consider combining returned list with data.table::rbindlist or dplyr::bind_rows
#' @param name_pattern - grep pattern to match against survey names (e.g. 'Covid')
#' @param active - include only active surveys (default = T)
#' @param config - where to read qualtrics settings
#' @param cautious_infer - tryCatch qualtRics:::infer_data_types. lose metadata but read in when there are errors. NULL for no covert
#' @import qualtRics
#' @export
qualtrics_surveys <- function(name_pattern=".*", active=T, config="~/.qualtrics.ini", cautious_infer=FALSE) {
  qualtrics_auth(config)
  root_url <- Sys.getenv("QUALTRICS_BASE_URL")
  survey_list <- qualtRics::all_surveys()
  keep <- grepl(name_pattern, survey_list$name, ignore.case=T, perl=T) 
  if(active) keep <- keep & survey_list$isActive == T

  # do we need to manually convert? will lose info on empty surveys
  fetch_func <-
   if(is.null(cautious_infer))  function(id) qualtRics::fetch_survey(id, force=TRUE, convert=FALSE) 
   else if(cautious_infer)      function(id) qualtrics_fetch(id)
   else                         function(id) qualtRics::fetch_survey(id, force=T) 

  svys <- lapply(survey_list$id[keep], fetch_func)
  names(svys) <- survey_list$name[keep]
  return(svys)
}

#' @name qualtrics_labels
#' @title extract label from each column of a dataframe
#' @param d - dataframe like qualtRics::fetch_survey output (e.g. qualtircs_surveys()[[1]])
#' @export
qualtrics_labels <- function(d) {
   sapply(d, attr, 'label')
}

#' @name qualtrics_battery_names
#' @title separate survey names into 
#' @description
#'  expect names like SRVY_X.Q1 ... SRVY_X.Q10a, SRVY_Y.Q1 ... SRVY_Y.Q20
#'  this is set in design of qualtrics survey (Q title not seen by participants)
#'  included here for reference
#' @param srvy - dataframe like qualtRics::fetch_survey output (e.g. qualtircs_surveys()[[1]])
#' @export
qualtrics_battery_names <- function(srvy)
   names(srvy) %>%
      # just survey name, not question numbers for each column
      gsub('\\.Q.*','',.) %>%
      unique %>%
      # just the Q# part (not the survey name) as a vector within a list
      # names of list elements will be survey name
      sapply(function(l) grep(l, names(srvy), value=T) %>% gsub(paste0(l, '.Q'), '', .))

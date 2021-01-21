qualtrics_fetch <- function(sid, root_url) {
  # work around error by explictly running infer_data_types
  # and returning original dataframe if that fails
  # Error: Problem with mutate() input Q2.
  # is.character(x) is not TRUE
  # Input Q2 is readr::parse_factor(...)
  tryCatch({
    # config has a bug somehwere that is duplicating https:// at the start of the url
    root_url <- gsub('^https://https://', 'https://', root_url)
    d <- qualtRics::fetch_survey(sid, root_url=root_url, force=T, convert=F)
    d <- tryCatch(qualtRics:::infer_data_types(d, sid),
                  error=function(e){
                     cat("WARNING: error w/qualtrics:::infer_data_types(...,'",sid,"')\n")
                     return(d)})},
   # if all of this fails, return NULL
   error=function(e) NULL)
}


#' @name qualtrics_surveys
#' @title pull surveys matching a pattern into a list of dataframes
#' @descrption
#' qualtircs.ini looks like
#'   [api]
#'   api_token= 67dfRZ2ibBRPxbLmbxxxxxxxxxxxxxxxxxxxxxxx
#'   root_url = https://pitt.co1.qualtrics.com
#'
#' @param name_pattern - grep pattern to match against survey names (e.g. 'Covid')
#' @param active - include only active surveys (default = T)
#' @param config - where to read qualtrics settings
#' @importFrom ini read.ini
#' @import qualtRics
#' @export
qualtrics_surveys <- function(name_pattern, active=T, config="qualtrics.ini") {
  ini <- ini::read.ini("qualtircs.ini")
  qualtRics::qualtrics_api_credentials(api_key=ini$api$api_token, base_url=ini$api$root_url)
  # readRenviron("~/.Renviron")
  survey_list <- qualtRics::all_surveys()
  keep <- grepl(name_pattern, survey_list$name, ignore.case=T, perl=T) 
  if(active) keep <- keep & survey_list$isActive == T
  svys <- lapply(survey_list$id[keep], qualtrics_fetch, root_url=ini$api$root_url)
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

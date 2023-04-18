#' Build OS (windows or unix) agnostic mount path to hera data share
#' @param path location on hera. use '/' as path separater
#' @param ...  additional path components. will concat with file.path
#' @export
#' @examples 
#' hera("/Volumes/Hera/scratch/")
#' hera("H:/scratch/")
#' hera("scratch/")
hera <- function(path, ...){
   # okay to specify full path on either windows or osx/linux
   # but we need to remove it
   path <- gsub("^(H:[/\\]|/Volumes/Hera/)", "", path)
   prefix <- ifelse(grepl("windows",.Platform$OS.type), "H:/", "/Volumes/Hera") 
   if(! dir.exists(prefix)) stop("Hera is not mounted on '%s'!", prefix)
   return(file.path(prefix, path, ...))
}

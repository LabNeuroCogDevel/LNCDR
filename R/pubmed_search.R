#
# Author: Will Foran
# Motivator: Brenden Tervo-Clemmens
#
# 

#' Pubmed search into a dataframe
#'
#' @description From pubmed query to dataframe
#' @param  query  -- a pubmed search query  
#' @param  prefix -- xml file prefix: what to search/call temporary output
#' @return dataframe with columns title, authors, year, journal, abstract, doi
#' @import dplyr
#' @export
#' @examples 
#'   sub_use <- pubmed_search('"substance use" risk nueroimaging', 'tmp/fmri_substance')
pubmed_search <- function(query, prefix){
  if (!requireNamespace("easyPubMed", quietly = TRUE)) {
     warning("pubmed_search requires 'easyPubMed'")
     return(NULL)
  }
  xmls <- prefix_xml(prefix)
  pdir <- dirname(prefix)
  if (!dir.exists(pdir)) dir.create(pdir, recursive=T)
  if (length(xmls) == 0L){
      cat("saving query results into ", prefix, "*.txt ...\n")
      easyPubMed::batch_pubmed_download(query,
                            format = "xml",
                            batch_size = 400,
                            dest_file_prefix = prefix)
      xmls <- prefix_xml(prefix)
  }else{
      warning(sprintf("reusing previous batch of %d, use a different prefix('%s') or rm files if undesired",
                      length(xmls),
                      prefix))
  }

  if (length(xmls) == 0L)  warning("No results pulled from ", prefix)

  dplyr::bind_rows(lapply(xmls, xml2df))
}

### supporting functions for extracting fields from xml
#   particular attention for returning NA if tree is missing a field
xqry <- function(tx, q){
  if (!requireNamespace("XML", quietly = TRUE)) {
     warning("LNCDR::xqry requires the 'XML' package")
     return(NULL)
  }
  unlist(XML::xpathApply(tx, q, xmlValue))
}
l_xqry <- function(lx, q) {
    l <- lapply(lx, function(x) xqry(xmlParse(x), q))
    l[sapply(l, is.null)] <- NA
    as.character(unlist(l))
}

## authors are treated differently:
#  need to collapse a list into one string
parseAuthor <- function(x) {
   l <- XML::xpathApply(xmlParse(x), "//AuthorList", saveXML)
   if ( length(l)==0L ) return(NA)
   unlist(lapply(l, idvAuthor))
}
idvAuthor<-function(s){
    xp<-xmlParse(s)
    if ( is.null(xp) ) return("")
    paste(xqry(xp, "//LastName"), xqry(xp, "//ForeName"), collapse="; ", sep=", ")
}

## how to take a pubmed xml output and turn it into a dataframe
qxml2df <- function(xml_in){
 require(XML)
 xl <- XML::xpathApply(xml_in, "//PubmedArticle", saveXML)
 l <- list(
   journal     = l_xqry(xl, "//Journal/Title"),
   title       = l_xqry(xl, "//ArticleTitle"),
   year        = l_xqry(xl, "//PubDate/Year"),
   abstract    = l_xqry(xl, "//Abstract"),
   doi         = l_xqry(xl, "//ArticleId[@IdType='doi']"),
   authors     = sapply(xl, parseAuthor)
  )

 # check lengths of output
 lens <- sapply(l, length)
 rl <- rle(lens); md <- rl$value[which.max(rl$lengths)]
 bad <- names(lens[lens !=  md])
 if (length(bad)!=0L) {
    print(lens)
    stop("missing xml for ", bad)
 }

 # return dataframe
 as.data.frame(l)
}

# read in a pubmed xml file. make into dataframe 
xml2df<-function(f) {
   cat("reading ", f, "\n")
   qxml2df(xmlParse(f))
}

# find all xml files that look like prefix
prefix_xml <- function(p) Sys.glob(sprintf("%s*.txt", p))

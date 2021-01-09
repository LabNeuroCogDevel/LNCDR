#' roicormat_wide - extract upper tri as single vector. name with rois
#' @param input_file_or_mat - dataframe or path to e.g. adj_pearson.txt
#' @param roi_names - optional names for rows and columns (symmetric)
#' @export
roicormat_wide <- function(input_file_or_mat, roi_names=NULL){
   if(is.character(input_file_or_mat))
     input_file_or_mat <- read.table(input_file_or_mat)

   m <- as.matrix(input_file_or_mat)
   # set row and column to be the same names
   if(!is.null(roi_names))
      rownames(m) <- colnames(m) <- roi_names  
   # remove repeat values and diagonal
   m[!upper.tri(m)] <- NA
   # from matrix to long (row per value)
   # Var1     Var2    value
   # rowname  colname matrix_value
   d <- reshape2::melt(as.matrix(m))
   # remove things not in the upper triangle
   d <- d[!is.na(d$value),]
   # a vector of each pair with names
   wide <- data.frame(t(d$value))
   names(wide) <- paste(d$Var1, d$Var2, sep="_")

   return(wide)
}

# given a list of adj_pearson.txt files
# extract roi names (numeric values) from corresponding diagnostic file
# or given roi_names, repeat them for each file
names_from_diagnostic <- function(adj_files, roi_names=NULL) {
   n_files <- length(adj_files)
   # if we were given roi_names. we dont have to work that hard
   # just repeat them for each file
   if(!is.null(roi_names)){
      names_per_file <- lapply(1:n_files, function(i) roi_names)
      return(names_per_file)
   }

   diagnostic_files <- gsub('adj_pearson.txt$', 'ROIdiagnostics.txt', adj_files)
   n_diagnostic <- sum(sapply(diagnostic_files, file.exists))
   if(n_diagnostic != n_files)
      stop("found ", n_diagnostic, " diagnostic files ",
           "and ", n_files, " adj_files.",
           "provide roi_names or make missing files")
   names_per_file <- lapply(diagnostic_files, function(f) read.csv(f)[,1])

   return(names_per_file)
}

#' parseROItempcor - read adj_pearson.txt and diagnostic_files.txt into wide
#' @param adj_glob file path glob to adj_pearson.txt fles
#' @param roi_names names to use instead of searcing for diagnostic_files.txt and using first column
#' @param idfunc default ld8from. function to pull id from filename
#' @examples
#' adjfilesglob<-"/Volumes/Hera/Projects/BTC_Diss/rs_20200321/striatalseeddata/*_20161*/frontostrialROItempcor20210106.rac1.adj_pearson.txt"
#' rac1 <- parseROItempcor(adjfilesglob)
#' @export
parseROItempcor <- function(adj_glob, roi_names=NULL, idfunc=LNCDR::ld8from){
   adj_files <- Sys.glob(adj_glob)
   # diagnostic files have roi value in them as first row
   names_per_file <- names_from_diagnostic(adj_files, roi_names)

   # use roicormat_wide to read in each files
   # add id if we have a function to do that
   read_adj <- roicormat_wide
   if(!is.null(idfunc))
       read_adj <- function(f, ...) {
        wide <- roicormat_wide(f, ...)
        wide$id <- idfunc(f)
        return(wide)
       }

   subjvects <- mapply(read_adj, adj_files, names_per_file, SIMPLIFY=F)
   dplyr::bind_rows(subjvects)
}


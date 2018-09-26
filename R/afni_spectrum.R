#
# Author: Will Foran
# 



#' NIFTI range using afni's 3dBrickStat
#' @param nifti file
#' @export
#' @examples 
#'  nii_range('myfile.nii.gz')
#'  nii_range('myfile.nii.gz[0]')
nii_range <- function(nii) {
  nii_no_subbrick <- gsub("\\[.*\\]$", "", nii)
  if (!file.exists(nii_no_subbrick)) stop("file %s DNE", nii_no_subbrick)
  cmd <- '3dBrickStat -non-zero -min -max "%s"'
  minmaxstr <- system(sprintf(cmd, nii), intern= T)
  inrange <- as.numeric(strsplit(minmaxstr, "\\s+")[[1]])
  return(inrange)
}

#' wrapper for optionally creating a pdf of plot_colorspectrum(afni.spectrum(...)))
#' @param volumemax - maximum value for spectrum
#' @param pdf name
#' @param threshold - where to cut off data
#' @param ispos - only postiive values?
#' @param ... other options for plot_colorspectrum (lab="val", side=2, ax=T)
#' @export
#' @examples 
#'  # minmax <- nii_range('myfile.nii.gz[0]')
#'  minmax <- c(5,10)
#'  afni_save_spectrum(minmax[1],threshold=minmax[0],ispos=T,lab="F",ax=F)
#'  # thresholded w/neg postive and better axis labels
#'  afni_save_spectrum(10,threshold=5,ispos=F,lab="F",ax=F) 
#'  axis(side=2,at=c(-10,-5,5,10),labels=c("-10","-5","5","10"),las=2)
#'  # with image saved from afni color bar
#'  afni_save_spectrum(10,img="saved_spectrum.png") 
# given range save a pdf with colorscale
afni_save_spectrum<-function(volumemax, savename=NA,
                             threshold=NA, ispos=F, img=NULL, ...){
   # spectrum ranges -max,max unless we have only positive (then 0 to max)
   posmax <- abs(volumemax)
   plotrange <- c(-posmax, posmax)
   if (ispos) plotrange <- c(0, posmax)

   # if we are applying a threshold, dont show colors below the thres
   # -- only applicable when ispos
   colorvals <- LNCDR::afni.spectrum(plotrange, img)
   if (!is.na(threshold) && ispos) {
      colorvals<-colorvals[colorvals$invals >= threshold, ]
      colorvals$invals[1] <- threshold # fix e.g thres is 5, but scale shows 4.9
      threshold <- NA # plot_colorspectrum doesn't need to know
   }

   # plot
   p <- plot_colorspectrum(colorvals, thres=threshold, ...)
   if (!is.na(savename)) {
      pdf(savename, height=10, width=3)
      print(p)
      dev.off()
   }
   return(p)
}


#' AFNI color spectrum -> R object
#' @param coloreddata 1) the single autoRange or manual range value provided to the AFNI "Define Overlay" gui.  OR 2) the vector of data the spectrum has been applied to. This can be just the range in lieu of the actual data. Use this option if "Pos?" is checked like colordata=c(0,4.44). N.B.  colordata=4.44 is the same as coloreddata=c(-4.44,4.44).
#' @param img the jpeg exported afni spectrum. Right click 'OLay' -> export ppm. Use this when using discrete or customized color scales.
#' @export
#' @examples 
#'  # range value set to 5 in afni
#'  colorvals <- afni.spectrum(5)
#'  # same as
#'  colorvals <- afni.spectrum(-5:5)
#'  # same as
#'  colorvals <- afni.spectrum(c(-5,5))
#'  # if Pos? is checked 
#'  colorvals <- afni.spectrum(c(0,5)) 

# read in a vector (colreddata) and the image that colors it (img)
# output a list with $invals and $clrs.hex
# the color of any invals is clrs.hex
afni.spectrum <- function(coloreddata, img=NULL) {
   # if we are only given one value, assume we want from negative to positive
   if(length(coloreddata)==1L){
     coloreddata < -c(-1, 1)*coloreddata
   }
   # 512 (colors in spectrum) x 64 (width, all same vlue) x 3 (r,g,b)
   if (!is.null(img)){

      require(jpeg)         # for readJPEG
      require(colorspace)   # for hex and RGB

      spct <- readJPEG(img)

      # THIS IS BAD: hex(RGB()) is broken?
      # convert to hex, use 32 width pixel to avoid tick marks
      #clrs.img <- spct[,32,]
      #clrs.hex <- rev(hex(RGB(clrs.img)))

      # second attempt
      # take the mean of all the good voxels (no scale bar)
      clrs.img <- apply(spct[,6:27,],c(1,3),mean)

      # rev so hotest (highest val) color is on top (@ idx 512)
      #crls.hex <- paste0("#",apply(matrix(sprintf("%02X",round(clrs.img*255)),nrow=512),
      #                             1,paste0,collapse=""))
      clrs.hexmat <- matrix( sprintf("%02X",round(clrs.img*255)), nrow=512)
      clrs.hexcat <- apply(clrs.hexmat,1,paste0,collapse="") 
      clrs.hex    <- paste0("#",clrs.hexcat)
      clrs.hex    <- rev(clrs.hex)  # hot on top
  } else {
      clrs.hex <- afni.default_color_spectrum()
  }

   # range
   minmax<-range(coloreddata)
   invals <- seq(minmax[1],minmax[2],length.out=length(clrs.hex))

   # retrun "colorval"
   colorval <- within( data.frame(invals=invals,clrs.hex=clrs.hex), clrs.hex <- as.character(clrs.hex))
   return(colorval)
}

#' the hex color value of any point on the spectrum read from afni.spectrum
#' @param colorval the list output of afni.spectrum
#' @param val the value for which a color (hex value) position on the spectrum is desired
#' @export
#' @examples 
#'  colorval <- afni.spectrum(-5:5)
#'  afni.color_at_value(colorval,6) # out of range, so highest value 
#'  afni.color_at_value(colorval,0) # middle value is 
afni.color_at_value <- function(colorval,val){
 idx <- which.min(abs(val-colorval$invals))[1]
 colorval$clrs.hex[idx]
}



### TEST/VIEW
plot.singlecolor <- function(i,colorspec,interval=1,init=1,side=2) {
 color <-  colorspec[i]
 boxstart <- init + (i-1)*interval
 boxstop  <- boxstart + interval
 # xl,yb,xr,yt
 if(side==2) {
   rect(0,boxstop,1,boxstart,
        col=color,border=color)
 } else{
   rect(boxstop,0,boxstart,1,
        col=color,border=color)
 }
}

## PLOT the spectrum
#' plot a color spectrum read in by afni.spectrum
#' @param colorval the list output of afni.spectrum
#' @param lab label for the relevant axis, defaults to useless 'val'
#' @param side which direction to draw the graph 1 horz, 2 vertical. matches par's meaning of side
#' @param ax T/F weither or not this function should draw an axis. Default T;label for max,middle, and min
#' @export
#' @examples 
#'  # Simple
#'  colorval <- afni.spectrum(5)
#'  plot_colorspectrum(colorval,'F') 
#' 
#'  # Threshold, provide on axis
#'  # limit to a threshold
#'  plot_colorspectrum(colorval,'F',thres=2.68) 
#'  # only  positive
#'  cv <- afni.spectrum(c(0,5))
#'  plot_colorspectrum(cv,'F') 
#'  # start at threshold
#'  cv <- cv[cv$invals>2.68,]
#'  plot_colorspectrum(cv,'F') 
#'  # make text 150% of the normal size for the labels
#'  par(cex.lab=1.5)
#'  # plot
#'  plot_colorspectrum(cv,'F',ax=F) 
#'  # add our own axis
#'  axis(side=2,at=c(3,4,5),labels=c("3","","5"),las=2)
plot_colorspectrum <-function(colorval, lab="val", side=2, ax=T, thres=NA) {

 rr<-range(colorval$invals)
 if (side==1) {
   xlim<-rr;     ylim<-c(0, 1); xlab<-lab; ylab<-""
 } else {
   xlim<-c(0, 1); ylim<-rr;     xlab<-"";  ylab<-lab
 }
 # open an empty plot with no x axis ticks
 # set lables
 plot(x=NULL, y=NULL,
   xlim=xlim, ylim=ylim, ylab=ylab, xlab=xlab,
   # no xaxis,no yaxis,no box
   xaxt="n", yaxt="n", bty="n")
 
 ## rr will be our axis/labels, add zero if zero is the middle
 #if(sum(rr)==0) rr <- sort(c(0,rr))   # maybe 0 is just in the range: if(findInterval(0,rr)==1)

 # give our axis a middle value
 rr <- round(sort(c(mean(rr), rr)), 2)

 # set our own axis if ax is passed in as true
 par(xaxt="s", yaxt="s") # turn the axis back on
 if (ax) axis(side=side, at=rr, labels=as.character(rr), las=side)

 # vertical plot
 interval <- mean(diff(colorval$invals))
 for (i in 1:length(colorval$clrs.hex)) {
    # skip values outside of threshold
    if (!is.na(thres) && abs(colorval$invals[i]) < thres) next
    # plot this color
    plot.singlecolor(i,
                     colorval$clrs.hex,
                     side=side,
                     init=min(colorval$invals),
                     interval=interval)
 }
}




##### default color spectrum
afni.default_color_spectrum <- function(){
rev(c("#A10006", "#D8142C", "#F50625", "#FF001E", "#FB031D", "#FC051A", 
"#FD0012", "#FF0111", "#FE0211", "#FC030B", "#FE0906", "#FF0F03", 
"#FF1303", "#FF1701", "#FF1A02", "#FE1D01", "#FF2001", "#FE2201", 
"#FF2502", "#FF2802", "#FF2A01", "#FE2C00", "#FE3000", "#FF3201", 
"#FF3401", "#FF3602", "#FF3801", "#FF3B02", "#FF3C01", "#FF4002", 
"#FF4201", "#FF4400", "#FE4600", "#FF4900", "#FF4C02", "#FF4E02", 
"#FE4E01", "#FE5102", "#FE5301", "#FF5602", "#FE5801", "#FE5A00", 
"#FE5C00", "#FF5F01", "#FF6100", "#FF6201", "#FF6400", "#FF6501", 
"#FE6801", "#FE6B02", "#FF6C01", "#FF6E01", "#FF6F01", "#FF7101", 
"#FE7400", "#FE7701", "#FE7902", "#FE7A02", "#FF7B02", "#FF7C02", 
"#FF7F02", "#FF8202", "#FF8502", "#FF8601", "#FE8800", "#FF8901", 
"#FF8B00", "#FF8C01", "#FF8E01", "#FF9002", "#FF9202", "#FF9402", 
"#FF9601", "#FE9801", "#FE9A00", "#FF9C01", "#FF9F02", "#FF9F02", 
"#FFA101", "#FFA302", "#FEA400", "#FEA600", "#FFAA01", "#FEAC01", 
"#FEAC00", "#FEAE00", "#FEB100", "#FFB200", "#FEB300", "#FFB502", 
"#FFB701", "#FFB802", "#FFBA01", "#FFBC01", "#FFBE00", "#FFC001", 
"#FFC101", "#FFC302", "#FFC502", "#FFC602", "#FEC802", "#FECA02", 
"#FFCC01", "#FECE01", "#FED001", "#FFD102", "#FFD302", "#FFD402", 
"#FFD601", "#FFD701", "#FFD902", "#FFDB02", "#FFDD01", "#FFDE01", 
"#FEE001", "#FEE202", "#FEE401", "#FEE502", "#FEE801", "#FFE902", 
"#FFEB01", "#FFEC02", "#FFEE01", "#FFF001", "#FEF200", "#FFF301", 
"#FFF402", "#FFF602", "#FFF700", "#FFFA01", "#FEFA01", "#FEFC02", 
"#FEFE02", "#FEFF03", "#FCFF02", "#FCFF02", "#F9FF01", "#F8FF01", 
"#F6FE01", "#F4FF01", "#F2FE00", "#F2FF00", "#EFFE01", "#EEFE01", 
"#ECFE02", "#EBFE02", "#E9FE01", "#E8FF01", "#E5FF01", "#E4FF01", 
"#E1FF02", "#E1FF02", "#DFFE02", "#DFFE02", "#DCFE01", "#DAFF01", 
"#D8FE01", "#D7FF02", "#D4FE01", "#D3FF01", "#D0FF01", "#D0FF01", 
"#CDFF00", "#CCFF00", "#CAFE01", "#C9FF01", "#C6FF00", "#C6FF00", 
"#C3FF01", "#C2FF01", "#C0FE01", "#BFFF01", "#BCFE00", "#BCFF00", 
"#B9FF00", "#B8FF00", "#B6FF00", "#B4FF00", "#B2FF01", "#B2FF01", 
"#AFFF00", "#AEFF00", "#ACFE01", "#A9FF00", "#A8FF02", "#A4FF01", 
"#A3FF02", "#A2FF01", "#A0FF01", "#9FFE00", "#9EFF02", "#9BFF01", 
"#9AFF02", "#97FF02", "#94FF01", "#93FF01", "#93FF03", "#90FE02", 
"#8EFF01", "#8CFF01", "#89FF01", "#88FE01", "#86FF01", "#85FE02", 
"#83FE03", "#80FE02", "#80FF02", "#7DFF01", "#7CFF03", "#7AFF02", 
"#77FE01", "#76FF01", "#73FE01", "#71FF01", "#6EFF00", "#6DFF00", 
"#6CFF01", "#6AFE00", "#68FE02", "#65FF01", "#63FE02", "#62FF02", 
"#5FFF01", "#5EFF01", "#5BFF02", "#58FE01", "#56FE00", "#54FF00", 
"#51FF00", "#50FF00", "#4DFF01", "#4BFF00", "#49FE01", "#47FF01", 
"#45FE02", "#42FF02", "#40FF02", "#3CFF01", "#3BFE00", "#3AFF00", 
"#38FE01", "#36FF02", "#32FF02", "#2FFF01", "#2CFF00", "#2AFF00", 
"#29FF01", "#26FF01", "#24FE02", "#20FE02", "#1CFE01", "#1AFF01", 
"#18FF01", "#15FE01", "#12FE02", "#0DFE04", "#06FE07", "#03FF0C", 
"#02FF10", "#02FE13", "#01FF18", "#00FF1B", "#00FF1D", "#01FF21", 
"#01FF23", "#01FF24", "#02FF27", "#02FF2A", "#01FF2D", "#01FE30", 
"#00FF33", "#00FF34", "#00FF36", "#01FF39", "#01FF3B", "#01FF3D", 
"#01FF3F", "#02FF43", "#01FF44", "#01FF46", "#01FF48", "#02FF4B", 
"#01FF4C", "#01FF4F", "#01FF51", "#02FF54", "#02FF56", "#02FF57", 
"#00FF59", "#01FF5D", "#01FF5E", "#01FF61", "#01FE63", "#01FE66", 
"#01FF68", "#01FF69", "#01FF6B", "#01FE6C", "#00FF6F", "#00FF71", 
"#00FF73", "#00FE76", "#01FF78", "#01FF79", "#02FE7C", "#01FE7D", 
"#00FF7F", "#01FF81", "#02FF81", "#02FF83", "#01FF86", "#01FE88", 
"#01FF89", "#01FF8A", "#02FF8D", "#02FE90", "#02FF92", "#02FF94", 
"#00FF95", "#00FF97", "#00FF99", "#01FF9B", "#01FF9C", "#01FF9E", 
"#01FEA0", "#01FEA2", "#01FFA4", "#01FFA5", "#01FFA7", "#01FFA8", 
"#01FEAA", "#02FFAD", "#00FFAE", "#00FFB0", "#00FFB2", "#01FFB5", 
"#01FFB6", "#01FFB8", "#01FEBA", "#01FEBC", "#02FFBE", "#02FFBF", 
"#01FFC1", "#01FFC1", "#01FFC3", "#02FFC5", "#01FFC7", "#01FFC8", 
"#01FFCA", "#01FFCC", "#01FECE", "#02FFD0", "#01FFD2", "#01FFD3", 
"#00FFD5", "#00FFD6", "#01FFD9", "#01FFDA", "#02FFDC", "#01FFDD", 
"#01FEDF", "#02FFE1", "#01FFE2", "#01FFE3", "#02FEE6", "#02FEE8", 
"#02FFEA", "#01FFEA", "#00FFEC", "#00FFED", "#00FFEF", "#00FFF0", 
"#03FFF3", "#03FEF4", "#01FFF6", "#01FFF7", "#00FFF9", "#01FFFB", 
"#01FFFC", "#01FFFE", "#01FDFE", "#00FCFF", "#01FAFF", "#02FAFF", 
"#01F6FF", "#02F6FF", "#02F4FF", "#02F2FF", "#02F0FF", "#02EFFF", 
"#01ECFF", "#01ECFF", "#02EAFF", "#01E9FF", "#01E7FF", "#00E6FE", 
"#01E3FF", "#01E2FE", "#01E0FF", "#00DFFF", "#01DDFF", "#01DBFF", 
"#01DAFE", "#01D8FD", "#02D6FE", "#01D4FE", "#01D2FE", "#01D1FF", 
"#02CFFE", "#01CEFE", "#01CCFE", "#01CBFE", "#01C9FE", "#01C7FE", 
"#02C5FF", "#02C3FE", "#01C1FF", "#00C0FE", "#01BEFE", "#00BCFE", 
"#01BAFE", "#02B9FF", "#02B6FF", "#01B5FF", "#01B2FE", "#01B2FE", 
"#02B1FF", "#01AEFE", "#00ACFE", "#02ABFF", "#02A8FF", "#01A6FF", 
"#01A4FF", "#01A4FF", "#01A1FF", "#00A0FE", "#009EFF", "#009CFF", 
"#019AFF", "#0099FF", "#0097FF", "#0096FF", "#0093FF", "#0091FE", 
"#008FFF", "#008EFE", "#018BFF", "#0189FF", "#0188FE", "#0086FE", 
"#0184FF", "#0182FF", "#0280FF", "#017EFE", "#007AFE", "#017AFE", 
"#0178FE", "#0177FF", "#0074FF", "#0172FF", "#0270FE", "#026EFE", 
"#026CFF", "#016BFF", "#0068FF", "#0066FE", "#0064FF", "#0062FF", 
"#0160FF", "#025EFF", "#015BFF", "#015AFF", "#0158FF", "#0056FE", 
"#0054FE", "#0050FD", "#004EFE", "#024EFE", "#004AFE", "#0049FF", 
"#0046FE", "#0044FE", "#0042FE", "#0140FF", "#013DFF", "#023BFF", 
"#0138FF", "#0136FE", "#0134FF", "#0133FE", "#0030FE", "#012DFE", 
"#012AFE", "#0228FF", "#0224FF", "#0122FE", "#0220FE", "#011CFE", 
"#021AFE", "#0018FF", "#0015FF", "#0110FF", "#0608FE", "#0A02FD", 
"#0F00FF", "#1201FF", "#1501FF", "#1802FF", "#1D00FE", "#2001FF", 
"#2102FE", "#2102FE"))

}

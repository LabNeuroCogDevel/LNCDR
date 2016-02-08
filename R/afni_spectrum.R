#
# Author: Will Foran
# 

require(jpeg) # for readJPEG
require(colorspace) # for hex and RGB


#' AFNI color spectrum -> R object
#' @param coloreddata vector of data the spectrum has been applied to (can be just a range)
#' @param img the jpeg exported afni spectrum. Right click 'OLay' -> export ppm
#' @export
#' @examples 
#' 
#' 

# read in a vector (colreddata) and the image that colors it (img)
# output a list with $invals and $clrs.hex
# the color of any invals is clrs.hex
afni.spectrum <- function(coloreddata,img=NULL) {
   # 512 (colors in spectrum) x 64 (width, all same vlue) x 3 (r,g,b)
   if(!is.null(img)){
      spct <- readJPEG(img)

      # convert to hex, use 32 width pixel to avoid tick marks
      clrs.img <- spct[,32,]
      # rev so hotest (highest val) color is on top (@ idx 512)
      clrs.hex <- rev(hex(RGB(clrs.img)))
      # same as
      # rev(hex(RGB(clrs.img[,1],clrs.img[,2],clrs.img[,3])))
  } else {
      clrs.hex <- afni.default_color_spectrum()
  }

   # range
   minmax<-range(coloreddata)
   invals <- seq(minmax[1],minmax[2],length.out=length(clrs.hex))

   # retrun "colorval"
   return(list(
     invals=invals,clrs.hex=clrs.hex
   ))
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
plot.singlecolor <- function(i,colorspec,width=1,height=1,ybs=1) {
 color <-  colorspec[i]
 yb <- ybs + (i-1)*height
 # xl,yb,xr,yt
 rect(0,yb,width,yb+height,
     col=color,border=color)
}

## PLOT the spectrum
#' plot a color spectrum read in by afni.spectrum
#' @param colorval the list output of afni.spectrum
#' @export
#' @examples 
#'  colorval <- afni.spectrum(-5:5)
#'  plot.colorspectrum() 
plot.colorspectrum <-function(colorval) {

 # open an empty plot with no x axis ticks
 # set lables
 plot(x=NULL,y=NULL,
   xlim=c(0,1),
   ylim=range(colorval$invals),
   ylab="val",
   xlab="",
   xaxt='n',bty='n')
 
 
 height <- mean(diff(colorval$inval))
 for (i in 1:512) { plot.singlecolor(i,colorval$clrs.hex,ybs=min(colorval$inval),height=height) }
}




##### default color spectrum
afni.default_color_spectrum <- function(){

c("#D0002A", "#ED4F73", "#FB2A6A", "#FF0060", "#FD1C5F", "#FE265A", 
"#FE004B", "#FF0D49", "#FF1649", "#FE1C3B", "#FF352A", "#FF451C", 
"#FF4D1C", "#FF550D", "#FF5A16", "#FF5F0D", "#FF630D", "#FF6600", 
"#FF6A0D", "#FF6E0D", "#FF7100", "#FE7500", "#FF7800", "#FF7A0D", 
"#FF7D0D", "#FF7F16", "#FF810D", "#FF840D", "#FF8600", "#FF890D", 
"#FF8B00", "#FF8D00", "#FF8F00", "#FF9200", "#FF9416", "#FF9616", 
"#FF9700", "#FF9916", "#FF9B0D", "#FF9D0D", "#FF9F00", "#FFA100", 
"#FEA300", "#FFA40D", "#FFA600", "#FFA70D", "#FFA800", "#FFA90D", 
"#FFAB0D", "#FFAD0D", "#FFAF00", "#FFAF0D", "#FFB100", "#FFB200", 
"#FEB400", "#FFB60D", "#FFB616", "#FFB71C", "#FFB916", "#FFBA0D", 
"#FFBB00", "#FFBD0D", "#FFC00D", "#FFC00D", "#FFC100", "#FFC20D", 
"#FFC300", "#FFC40D", "#FFC50D", "#FFC616", "#FFC816", "#FFC90D", 
"#FFCA00", "#FFCB0D", "#FFCD00", "#FFCD0D", "#FFCF16", "#FFCF16", 
"#FFD00D", "#FFD20D", "#FFD200", "#FFD400", "#FFD50D", "#FFD600", 
"#FED700", "#FFD700", "#FFD900", "#FFDA00", "#FFDA00", "#FFDB16", 
"#FFDC0D", "#FFDD0D", "#FFDE00", "#FFDF0D", "#FFE000", "#FFE10D", 
"#FFE200", "#FFE30D", "#FFE40D", "#FFE416", "#FFE50D", "#FFE616", 
"#FFE70D", "#FFE800", "#FFE900", "#FFEA0D", "#FFEB00", "#FFEB0D", 
"#FFEC00", "#FFED0D", "#FFEE16", "#FFEE0D", "#FFEF00", "#FFF00D", 
"#FFF10D", "#FFF216", "#FEF30D", "#FFF316", "#FFF50D", "#FFF516", 
"#FFF60D", "#FFF60D", "#FFF700", "#FFF80D", "#FFF900", "#FFFA0D", 
"#FFFA16", "#FFFB0D", "#FFFB00", "#FFFD0D", "#FFFD0D", "#FEFE16", 
"#FEFF16", "#FFFF1C", "#FEFF16", "#FEFF16", "#FCFF0D", "#FCFF0D", 
"#FBFF0D", "#FAFF0D", "#F9FF00", "#F9FF00", "#F8FF0D", "#F7FF0D", 
"#F6FF16", "#F6FF16", "#F5FF0D", "#F5FF0D", "#F3FF00", "#F3FF00", 
"#F1FF0D", "#F1FF0D", "#F0FF16", "#F0FF16", "#EEFF0D", "#EEFF0D", 
"#EDFF0D", "#ECFF0D", "#EBFF00", "#EAFF00", "#E9FF0D", "#E9FF0D", 
"#E8FF00", "#E7FF00", "#E6FF0D", "#E5FF0D", "#E4FF00", "#E4FF00", 
"#E3FF0D", "#E2FF0D", "#E1FF0D", "#E0FF0D", "#DFFF00", "#DFFF00", 
"#DDFF00", "#DDFF00", "#DCFF00", "#DBFF00", "#DAFF0D", "#DAFF0D", 
"#D8FF00", "#D7FF00", "#D6FF0D", "#D5FF00", "#D4FF16", "#D2FF0D", 
"#D1FF16", "#D1FF0D", "#D0FF0D", "#CFFF00", "#CEFF16", "#CDFF0D", 
"#CCFF1C", "#CAFF16", "#C8FF0D", "#C7FF0D", "#C7FF1C", "#C5FF16", 
"#C4FF0D", "#C4FF0D", "#C2FF0D", "#C0FF00", "#C0FF16", "#BEFF0D", 
"#BDFF1C", "#BCFF16", "#BBFF16", "#B9FF0D", "#B9FF1C", "#B8FF16", 
"#B6FF0D", "#B5FF0D", "#B3FF0D", "#B1FF0D", "#AFFF00", "#AFFF00", 
"#AEFF0D", "#ACFF00", "#ABFF16", "#A9FF0D", "#A7FF16", "#A7FF16", 
"#A4FF0D", "#A3FF0D", "#A1FF16", "#9FFF0D", "#9CFF00", "#9BFF00", 
"#99FF00", "#97FF00", "#94FF0D", "#94FF00", "#92FF0D", "#90FF0D", 
"#8EFF16", "#8BFF16", "#89FF16", "#85FF0D", "#83FF00", "#81FF00", 
"#7FFF0D", "#7DFF0D", "#7AFF16", "#78FF0D", "#75FF00", "#71FF00", 
"#6EFF0D", "#6CFF00", "#68FF0D", "#62FF0D", "#5FFF16", "#5AFF16", 
"#56FF0D", "#4FFF00", "#4BFF0D", "#3DFF1C", "#2AFF2E", "#1CFF3D", 
"#16FF47", "#0DFF4D", "#00FF56", "#00FF5C", "#00FF5F", "#0DFF63", 
"#0DFF66", "#0DFF68", "#16FF6C", "#16FF71", "#0DFF75", "#0DFF77", 
"#00FF7A", "#00FF7C", "#00FF7E", "#0DFF81", "#0DFF83", "#0DFF86", 
"#0DFF88", "#16FF8B", "#0DFF8D", "#0DFF8E", "#0DFF90", "#16FF93", 
"#0DFF94", "#0DFF97", "#0DFF99", "#16FF9B", "#16FF9D", "#16FF9E", 
"#00FF9F", "#0DFFA2", "#0DFFA3", "#0DFFA6", "#0DFFA7", "#0DFFA9", 
"#0DFFAB", "#0DFFAC", "#0DFFAD", "#0DFFAD", "#00FFAF", "#00FFB2", 
"#00FFB3", "#00FFB4", "#0DFFB6", "#0DFFB7", "#0DFFB9", "#0DFFB9", 
"#00FFBA", "#0DFFBC", "#0DFFBD", "#0DFFBE", "#0DFFBF", "#0DFFC0", 
"#0DFFC2", "#0DFFC2", "#0DFFC3", "#0DFFC5", "#16FFC7", "#16FFC8", 
"#00FFC9", "#00FFCA", "#00FFCB", "#0DFFCC", "#0DFFCD", "#0DFFCE", 
"#0DFFCF", "#0DFFD0", "#0DFFD2", "#0DFFD2", "#0DFFD4", "#0DFFD4", 
"#0DFFD5", "#16FFD6", "#00FFD7", "#00FFD8", "#00FFD9", "#0DFFDB", 
"#0DFFDC", "#0DFFDC", "#0DFFDD", "#0DFFDE", "#16FFE0", "#16FFE0", 
"#0DFFE2", "#0DFFE2", "#0DFFE3", "#16FFE3", "#0DFFE4", "#0DFFE5", 
"#0DFFE6", "#0DFFE7", "#0DFFE8", "#16FFE9", "#00FFEA", "#00FFEB", 
"#00FFEC", "#00FFEC", "#0DFFED", "#0DFFEE", "#0DFFEE", "#0DFFEF", 
"#0DFFF0", "#16FFF1", "#0DFFF2", "#0DFFF2", "#0DFFF3", "#0DFFF4", 
"#16FFF6", "#16FFF6", "#00FFF6", "#00FFF7", "#00FFF8", "#00FFF8", 
"#1CFFF9", "#1CFFFA", "#0DFFFB", "#0DFFFB", "#00FFFC", "#0DFFFD", 
"#0DFFFE", "#0DFFFE", "#0DFEFF", "#00FEFF", "#0DFDFF", "#0DFDFF", 
"#00FBFF", "#16FBFF", "#16FAFF", "#0DFAFF", "#0DF9FF", "#16F8FF", 
"#0DF6FF", "#0DF6FF", "#16F6FF", "#0DF5FF", "#0DF4FF", "#00F4FF", 
"#00F3FF", "#0DF2FF", "#0DF1FF", "#00F0FF", "#0DEFFF", "#00EFFF", 
"#00EEFF", "#00EDFE", "#0DECFF", "#0DECFE", "#16EBFF", "#0DEAFF", 
"#0DE9FF", "#00E8FF", "#00E7FF", "#00E7FF", "#00E6FF", "#00E5FF", 
"#0DE4FF", "#0DE3FF", "#0DE2FF", "#00E1FF", "#0DE0FF", "#00DFFE", 
"#00DEFF", "#0DDDFF", "#0DDCFF", "#0DDBFF", "#0DDAFF", "#0DDAFF", 
"#16D9FF", "#00D8FF", "#00D6FF", "#0DD6FF", "#0DD5FF", "#0DD3FF", 
"#0DD2FF", "#0DD2FF", "#0DD0FF", "#00D0FF", "#00CEFF", "#00CDFF", 
"#0DCCFF", "#00CBFF", "#00CAFF", "#00CAFF", "#00C8FF", "#00C7FF", 
"#00C5FF", "#00C5FF", "#00C4FF", "#0DC2FF", "#0DC1FF", "#00C0FE", 
"#00BFFF", "#0DBEFF", "#0DBDFF", "#0DBBFF", "#00B8FF", "#00B8FF", 
"#00B6FF", "#0DB6FF", "#00B4FF", "#0DB2FF", "#0DB1FF", "#0DAFFF", 
"#16AEFF", "#0DADFF", "#00ABFF", "#00AAFF", "#00A9FF", "#00A7FF", 
"#00A6FF", "#0DA3FF", "#00A1FF", "#0DA0FF", "#0D9FFF", "#009EFF", 
"#009BFE", "#0099FE", "#0097FF", "#0D96FF", "#0094FF", "#0092FF", 
"#008FFF", "#008EFE", "#008CFF", "#0D89FF", "#0D86FF", "#0D84FF", 
"#0081FF", "#0D7FFF", "#0D7DFF", "#007CFF", "#0078FF", "#0D75FF", 
"#0D71FF", "#0D6EFF", "#0D6AFF", "#0D66FE", "#1663FF", "#0D5FFE", 
"#165CFF", "#0056FF", "#0051FF", "#0D47FF", "#2A32FE", "#3516FE", 
"#420DFF", "#4B0DFF", "#510DFF", "#5516FF", "#5D0DFF", "#630DFF", 
"#6516FF", "#6516FF")
}

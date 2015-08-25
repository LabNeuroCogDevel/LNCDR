#
# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran
# 
require(ggplot2) 
require(grid) #unit

#' Re-theme plot a la Luna
#' This function stylizes a ggplot2 plot to match the aesthetic of Dr. Luna
#'  * Removes grid lines and borders
#'  * Increases font size 
#'  * Increases gap between x and y axis titles (by adding "\n" to the labels)
#' @param p plot to retheme
#' @param ajust vjust and hjust values for axis text Defauts to .05
#' @export
#' @examples 
#' p <- ggplot(iris,aes(x=Sepal.Length,y=Petal.Width))+geom_point()
#' lunaize(p) # or with dplyr: p %>% lunaize

lunaize <- function(p,ajust=.05){
  p$labels$y <- paste0(p$labels$y,"\n")
  p$labels$x <- paste0("\n",p$labels$x)
  p+ theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=20),
        axis.text.y = element_text(hjust = ajust,color='black'),
        axis.text.x = element_text(vjust = ajust,color='black')
        #plot.margin=unit(c(1,1,1.75,1.75),"cm"), # Weird spacing
        #panel.background = element_rect(fill='white'), # theme_bw does this
  )
}

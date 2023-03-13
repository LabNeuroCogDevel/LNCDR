# Author: Finn Calabro
# Packager: Will Foran

#' age_animate_weight
#'
#' @description duplicate dataframe for each element in frames_at
#' @param d          dataframe with age
#' @param frames_at   vector of each "age" value to use as a frame
#' @param weight_sigma   how fast points disappear (higher is faster)
#' @export
#' @return dataframe with length(frames_at) times more rows: and columns 'frameage' age@frame and 'w' weight for each point
#' @examples
#' d <- data.frame(age=seq(10,30, length.out=4))
#' movdata <- age_animate_weight(d, c(10,20,30), 2)
age_animate_weight <- function(d, frames_at, weight_sigma) {
  # duplicate the input dataframe for each age step to plot
  #   nrow(movdata) == nrows(d)*rep_steps
  # and make what we will plot relative to each age
  merge(d, data.frame(frameage = frames_at)) %>%
    mutate(w = exp(-(age - frameage)^2/(2*(weight_sigma^2)))) %>% 
    group_by(frameage) %>%
    mutate(w = w/max(w),
           w = pmax(w, .2*max(w))) %>%
    ungroup()
}

#' age_animate
#'
#' @description visualize age interaction with gganimate
#' @param d            dataframe with age
#' @param weight_sigma   how fast points disappear (higher is faster)
#' @param rep_steps    how many steps to plot
#' @param test_age    show only a single frame at 'test_age' instead of animating
#' @param ...    additional ggplot aesthetics. probably want x= and y=
#' #@importFrom viridis scale_color_viridis
#' #@importFrom gganimate ease_aes transition_time
#' @export
#' @examples
#' d <- data.frame(age=seq(10,30, length.out=8),
#'                 x=runif(8,0,10),
#'                 y=runif(8,0,10))
#' p  <- age_animate(d, rep_steps=16, x=x, y=y)
#' p2 <- age_animate(d, test_age=10) + aes(x=x,y=y) + cowplot::theme_cowplot() + theme(legend.position='none')

age_animate <- function(d, weight_sigma=2, rep_steps=100, test_age=NULL, ...) {
  require(viridis)
  require(ggplot2)
  require(gganimate)
  # need age to exist and be a numeric value
  with(d, qassertm(age='n'))

  # duplicated all rows for each step as an animation frame.
  # weight at each frame is relative to current "frameage"
  frames_at_age <- seq(min(d$age), max(d$age), length.out=rep_steps)
  movdata <- age_animate_weight(d, frames_at_age, weight_sigma)

  # if we just wnat to test out the plot aesthetics
  if(!is.null(test_age)){
   return(movdata %>%
          filter(frameage == test_age) %>%
          ggplot() +
          aes(color = age, ...) +
          geom_smooth(method='lm', aes(weight=w, color=frameage)) +
          geom_point(aes(alpha = w, size=w)) +
          theme(legend.position='none') +
          viridis::scale_color_viridis(option='H')
        )
  }
  
  p <- ggplot(movdata) +
     aes(color = age, ...) +
     geom_smooth(method='lm', aes(weight=w, color=frameage)) +
     geom_point(aes(alpha = w, size=w)) +
     transition_time(frameage) +
     ease_aes('linear') + 
     # maybe consider leaving these up to the user
     theme(legend.position='none') +
     scale_color_viridis(option='H')

  # to save, see
  #  gif <- animate(p, width = 480, height = 480)
  #  anim_save("interaction.gif", animation = gif)
} 

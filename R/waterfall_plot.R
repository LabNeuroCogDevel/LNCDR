#' waterfall plot data
#'
#' @description create age_id ranked id for data grouped by start age
#' @param d   dataframe with columns id (repeated) and  age
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#'  d <- data.frame(age=c(10,20,25, 13,14), id=c(100,100,100, 200,200))
#'  d_grouped <- waterfall_group(d)
waterfall_group <- function(d) {
   if (!all(c("id", "age") %in% names(d))){
      stop("dataframe must have columns 'id' and 'age'")
   }
   age_ranked <-
      d %>%
      select(id, age) %>%
      group_by(id) %>%
      summarise(minage=min(age)) %>%
      ungroup() %>%
      mutate(age_id = rank(minage, ties.method="first") ) %>%
      inner_join(d, by="id")
}

#' waterfall plot
#'
#' @description subj x age plot: line-per-subject dot-per-age ordered by age at first visit
#' @param d   dataframe with columns id (repeated) and  age
#' @param ...  additional aesthetics
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#'  d <- data.frame(age=c(10,20,25,13,14,NA), id=c(100,100,100, 200,200,100))
#'  p <- waterfall_plot(d)
#'  print(p)
waterfall_plot <- function(d, ...) {
   d <- d[!is.na(d$age),]

   age_ranked <- waterfall_group(d)

   p <-
      ggplot(age_ranked) +
      aes(x=age, y=age_id, group=age_id, ...) +
      geom_line() +
      geom_point()

   p <- lunaize(p) +
      ylab("") +
      xlab("Age") +
      theme(
         axis.title.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y  = element_blank()
      )

   return(p)
}

#' Create my own theme for my papers
#'
my_theme_paper <- function() {
  th <- theme(
    axis.text = element_text(size = 10, family = "serif"),
    axis.title = element_text(size = 15, family = "serif"),
    plot.caption = element_text(size = 10, family = "serif"),
    legend.text = element_text(size = 12, family = "serif"),
    legend.title = element_text(size = 12, family = "serif"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey93")
  )
  return(th)
}

#' Create my own theme for my slides
#'
my_theme_slides <- function() {
  th <- theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 15),
    plot.caption = element_text(size = 10),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey93")
  )
  return(th)
}


# see how to create standard palettes
# for temperature or scores blue-white-red

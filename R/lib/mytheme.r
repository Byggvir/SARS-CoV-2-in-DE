# Title: My personal theme
# Version: 0.1.0
# Author: Thomas Arend
# Maintainer: Thomas Arend <thomas@arend-rhb.de>
#   Description: Define a personal theme to be used in all projects.
#   My theme for the uniform appearance of the diagrams
# License: GNU General Public License 3
# Encoding: UTF-8
#
# 
# 

theme_ta <- function (
  base_family = "Helvetica",
  base_size = 11.5,
  plot_title_family = base_family,
  plot_title_size = 18,
  plot_title_face = "bold",
  plot_title_margin = 10,
  subtitle_family = base_family,
  subtitle_size = 12,
  subtitle_face = "plain",
  subtitle_margin = 15,
  strip_text_family = base_family,
  strip_text_size = 12,
  strip_text_face = "plain",
  caption_family = base_family,
  caption_size = 9,
  caption_face = "italic",
  caption_margin = 10,
  axis_text_size = base_size,
  axis_title_family = subtitle_family,
  axis_title_size = 9,
  axis_title_face = "plain",
  axis_title_just = "rt",
  plot_margin = margin(30, 30, 30, 30),
  grid_col = "#cccccc",
  grid = TRUE,
  axis_col = "#cccccc",
  axis = FALSE,
  ticks = FALSE) {
  
  theme_ipsum(   base_size = base_size
               , base_family = base_family ) %+replace%
  theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255),
        face = "bold",
        hjust = 0,
        size = rel(3),
        
      )
      
      , legend.position = 'bottom'
      , axis.title = element_text(
        size = rel(0.75))
      , axis.text = element_text(
        size = rel(0.75))
      , axis.title.y = element_text ( angle = 90 )
      , axis.title.y.right = element_text ( angle = 90 )
      
     , strip.text.x = element_text (
        size = rel(0.5)
        , color = "black"
        , face = "bold.italic"
      ),
      
      complete = TRUE
    )
  
}

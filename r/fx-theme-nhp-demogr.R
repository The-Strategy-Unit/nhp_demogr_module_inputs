
# README
# for use with project 1045
# @param base_size = base font size 
# @param base_family = base font family 




# theme for project 1045 ----
theme_nhp_demogr <- function(base_size = 10, base_family = "Roboto") {
  
  half_line <- base_size / 2
  
  theme(
    
    # elements in this first block aren't used directly, but are inherited by others
    line = element_line(colour = "#2c2825", linewidth = .5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "#ffffff", colour = "#2c2825", linewidth = .5, linetype = 1),
    text = element_text(
      family =       base_family
      , face =       "plain"
      , colour =     "#686f73"
      , size =       base_size
      , lineheight = .9
      , hjust =      .5
      , vjust =      .5
      , angle =      0
      , margin =     margin()
      , debug =      FALSE)
    
    # axes
    , axis.line =          element_line(linewidth = .2, color = "#d3d3d3")
    , axis.line.x =        element_line(linewidth = .2, color = "#d3d3d3")
    , axis.line.y =        element_blank()
    , axis.text =          element_text(size = base_size, colour = "#686f73", hjust = 0)
    , axis.text.x =        element_text(margin = margin(t = .8 * half_line / 2), vjust = .5, hjust = .5)
    , axis.text.x.top =    element_text(margin = margin(b = .8 * half_line / 2), vjust = 0, hjust = .5)
    , axis.text.y =        element_text(margin = margin(r = .8 * half_line / 2), hjust = 1)
    , axis.text.y.right =  element_text(margin = margin(l = .8 * half_line / 2), hjust = 0)
    , axis.ticks =         element_line(linewidth = .5, colour = "#686f73")
    , axis.ticks.length =  unit(2, "mm")
    , axis.ticks.x =       element_line(linewidth = .4, color = "#d3d3d3")  
    , axis.ticks.y =       element_blank()
    , axis.title =         element_text()
    , axis.title.x =       element_text(margin = margin(t = half_line), vjust = 1)
    , axis.title.x.top =   element_text(margin = margin(b = half_line), vjust = 0)
    , axis.title.y =       element_text(angle = 90, margin = margin(r = half_line), vjust = 1)
    , axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line), vjust = 0)
    
    # legend
    , legend.background =     element_rect(fill = NA, colour = NA)
    , legend.spacing =        unit(4, "mm")
    , legend.spacing.x =      NULL
    , legend.spacing.y =      NULL
    , legend.margin =         margin(2, 2, 2, 2, "mm")
    , legend.key =            element_rect(fill = "#f8f8f7", colour = NA)
    , legend.key.size =       unit(1, "lines")
    , legend.key.height =     NULL
    , legend.key.width =      NULL
    , legend.text =           element_text(size = base_size)
    , legend.text.align =     NULL
    , legend.title =          element_text(hjust = 0)
    , legend.title.align =    NULL
    , legend.position =       "right"
    , legend.direction =      NULL
    , legend.justification =  "centre"
    , legend.box =            NULL
    , legend.box.margin =     margin(0, 0, 0, 0, "mm")
    , legend.box.background = element_blank()
    , legend.box.spacing =    unit(4, "mm")
    
    # panel
    , panel.background =   element_rect(fill = "#f8f8f7", colour = NA)
    , panel.border =       element_blank()
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_line(linewidth = .2, color = "#d3d3d3")
    , panel.grid.minor =   element_blank()
    , panel.spacing =      unit(2, "mm")
    , panel.spacing.x =    NULL
    , panel.spacing.y =    NULL
    , panel.ontop    =     FALSE
    
    # strip
    , strip.background =      element_rect(fill = "#e7e7e7", color = NA)
    , strip.text =            element_text(colour = "#2c2825", size = base_size, hjust = 0, vjust = .5, margin = margin(t = half_line * .5, b = half_line * .5, l = half_line * .5))
    , strip.placement =       "inside"
    , strip.placement.x =     NULL
    , strip.placement.y =     NULL
    , strip.switch.pad.grid = unit(2, "mm")
    , strip.switch.pad.wrap = unit(2, "mm")
    
    # plot
    # , plot.background = element_blank()
    , plot.background = element_rect(fill = "#f8f8f7", colour = NA)
    , plot.title =      element_text(family = "Fira Sans Medium", size = base_size * 1.3, hjust = 0, vjust = 1, margin = margin(b = half_line * 2), lineheight = 1.2)
    , plot.title.position = "plot"
    , plot.subtitle =   element_text(colour = "#686f73", size = base_size, hjust = 0, vjust = 1, margin = margin(b = half_line * 2))
    , plot.caption =    element_text(size = base_size, hjust = 0, vjust = 1, margin = margin(t = half_line * 2), colour = "#686f73")
    , plot.caption.position = "plot"
    , plot.margin =     margin(half_line, half_line, half_line, half_line)
    , complete = TRUE
  )
}

# Warning message:
# New theme missing the following elements: axis.ticks.length.x, axis.ticks.length.x.top, axis.ticks.length.x.bottom, axis.ticks.length.y, axis.ticks.length.y.left, axis.ticks.length.y.right, panel.grid, plot.tag, plot.tag.position, strip.text.x, strip.text.y 


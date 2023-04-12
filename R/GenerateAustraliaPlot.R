library(tidyverse)
library(sf)

# Read in shapefile
## Sourced from: https://www.abs.gov.au/statistics/standards/
## australian-statistical-geography-standard-asgs-edition-3/
## jul2021-jun2026/access-and-downloads/digital-boundary-files
AusBoundariesRaw <- sf::read_sf("C:/Users/kille/Downloads/AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")

# Cull it down, way too big!
AusBoundarySimplified <- AusBoundariesRaw %>%
  sf::st_simplify(dTolerance = 40000) %>%
  dplyr::filter(AUS_CODE21 == "AUS")

# Remove small islands (Tasmania - you're safe!)
AusBoundarySimplified2 <- AusBoundarySimplified %>% 
  sf::st_cast("POLYGON") %>%
  dplyr::mutate(Area = sf::st_area(.)) %>%
  dplyr::arrange(dplyr::desc(Area)) %>%
  dplyr::slice(1:2)

# Plot Aus
Chart <- ggplot(AusBoundarySimplified2) +
  geom_sf(fill = "#feb9a9", lwd = 1.5, colour = "#fe9179") +
  ggimage::geom_image(image = "C:/Users/kille/Downloads/location-dot-solid.svg",
                      x = 150.2096535, y = -33.214044, colour = "#005B88", size = 0.07) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.background = element_blank())

ggplot2::ggsave(Chart, filename = "img/AusMapSydney.png", bg = "transparent",
                width = 698*3, height = 626*3,
                units = "px")

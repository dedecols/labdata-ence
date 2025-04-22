library(janitor)
library(dplyr)
library(tidyr)
library(giscoR)
library(ggplot2)
library(sf)
library(ggiraph)
library(purrr)
library(htmltools)
library(htmlwidgets)

savedir <- "C:/Users/a_les/Documents/ENCE/2025.1/LABDATA/"

germany_districts <- giscoR::gisco_get_nuts(
  country = 'Germany',
  nuts_level = 3,
  year = "2021",
  epsg = 3035
  ) %>% 
  tidyr::as_tibble() %>% 
  janitor::clean_names()

class(germany_districts)

germany_states <- giscoR::gisco_get_nuts(
  country = 'Germany',
  nuts_level = 1,
  year = "2021",
  epsg = 3035
) %>% 
  tidyr::as_tibble() %>% 
  janitor::clean_names()


# gg_plt <- germany_districts %>% 
#   ggplot2::ggplot(ggplot2::aes(geometry = geometry)) +
#   ggplot2::geom_sf(
#     data = germany_states,
#     ggplot2::aes(fill = nuts_name),
#     color = 'black',
#     linewidth = 0.5
#   ) +
#   ggiraph::geom_sf_interactive(
#     fill = NA,
#     ggplot2::aes(
#       data_id = nuts_id,
#       tooltip = nuts_name
#     ),
#     color = 'black',
#     linewidth = 0.1
#   ) +
#   ggplot2::theme_void() +
#   ggplot2::theme(legend.position = 'none')
# 
# ggiraph::girafe(ggobj = gg_plt)

state_nmbrs <- map_dbl(
  germany_districts$geometry,
  \(x) {
    purrr::map_lgl(
      germany_states$geometry,
      \(y) {
        st_within(
          x,
          y
        ) %>% as.logical()
      }
    ) %>% which()}
  )

germany_districts_w_states <- germany_districts %>% 
  dplyr::mutate(
    state = germany_states$nuts_name[state_nmbrs]
  )


make_nice_label <- function(nuts_name, state){
  nuts_name_label <- htmltools::span(
    nuts_name,
    style = htmltools::css(
      fontweight = 600,
      font_family = 'Source Sans Pro',
      font_size = '32px'
    )
  )
  state_label <- htmltools::span(
    state,
    style = htmltools::css(
      font_family = 'Source Sans Pro',
      fonte_size = '20px'
    )
  )
  glue::glue('{nuts_name_label}<br>{state_label}')
}


gg_plt <- germany_districts_w_states %>% 
  dplyr::mutate(
    nice_label = map2_chr(
      nuts_name,
      state,
      make_nice_label
    )
  ) %>% 
  ggplot2::ggplot(ggplot2::aes(geometry = geometry)) +
  ggplot2::geom_sf(
    data = germany_states,
    ggplot2::aes(fill = nuts_name),
    color = 'black',
    linewidth = 0.5
  ) +
  ggiraph::geom_sf_interactive(
    fill = NA,
    ggplot2::aes(
      data_id = nuts_id,
      tooltip = nice_label
    ),
    color = 'black',
    linewidth = 0.1
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none') +
  ggplot2::scale_fill_manual(
    values = c(
      "#A8DADC", "#F1FAEE", "#E4D5B7", "#F4A261",
      "#2A9D8F", "#264653", "#E9C46A", "#F2B93D",
      "#D9E9B8", "#F3A8A0", "#B0C7C3", "#A7D8C1",
      "#FFE156", "#6C7A6D", "#6A8D92", "#B5EAEA"
    )
  ) +
  ggplot2::coord_sf(expand = FALSE)

# mapa_alemanha <- ggiraph::girafe(ggobj = gg_plt)

mapa_alemanha <- ggiraph::girafe(
  ggobj = gg_plt,
  width_svg = 10,  # Largura em polegadas
  height_svg = 8,  # Altura em polegadas
  options = list(ggiraph::opts_sizing(rescale = FALSE)
  )
)

htmlwidgets::saveWidget(mapa_alemanha, paste0(savedir,'Mapa_Alemanha.html'))













# ggiraph::girafe(
#   ggobj = gg_plt,
#   options = list(
#     opts_hover(
#       css = ggiraph::girafe_css(
#         css = '',
#         area = 'stroke: black; fill; black;'
#       )
#     )
#   ))


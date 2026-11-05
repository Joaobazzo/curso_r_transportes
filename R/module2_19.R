library(sf)
library(dplyr)
library(ggplot2)

arquivo <- "dados/brutos/muni_pr_sc.gpkg"
if (!file.exists(arquivo)) {
  muni <- dplyr::bind_rows(
    geobr::read_municipality(code_muni = "PR", year = 2022, showProgress = FALSE),
    geobr::read_municipality(code_muni = "SC", year = 2022, showProgress = FALSE)
  )
  sf::st_write(muni, arquivo, quiet = TRUE)
}
muni <- sf::st_read(arquivo, quiet = TRUE)

portos <- data.frame(
  nome = c("Paranaguá", "Antonina", "São Francisco do Sul", "Itajaí", "Imbituba"),
  tku_bi = c(21.4, 1.2, 8.7, 6.3, 3.1),
  lon = c(-48.5119, -48.7119, -48.6383, -48.6486, -48.6706),
  lat = c(-25.5016, -25.4283, -26.2400, -26.9077, -28.2300)
) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)


# 19.1 ggplot2 -----

ggplot(muni) + geom_sf()


muni |>
  dplyr::mutate(area_km2 = sf::st_area(geom)) |>
  dplyr::mutate(area_km2 = units::set_units(area_km2, 'km^2')) |>
  dplyr::mutate(area_km2 = as.numeric(area_km2)) |>
  ggplot() +
  geom_sf(aes(fill = area_km2), colour = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "mako", direction = -1
                       , transform = "log10",
                       name = "Área (km²)"
                       ,labels = scales::label_number(big.mark = ".")) +
  theme_void(base_size = 10)

help(theme)


ggplot() +
  geom_sf(data = muni, fill = "grey96", colour = "grey80", linewidth = 0.2) +
  geom_sf(data = portos, aes(size = tku_bi), colour = "#1a5b8a", alpha = 0.75) +
  ggrepel::geom_text_repel(
    data = portos,
    aes(label = nome, geometry = geometry),
    stat = "sf_coordinates", size = 2.8, min.segment.length = 0
  ) +
  scale_size_continuous(range = c(2, 8), name = "TKU (bi t·km)") +
  coord_sf(xlim = c(-49.5, -47.8), ylim = c(-28.8, -25.0)) +
  labs(caption = "Valores ilustrativos."
       ,x = 'longitude', y= 'latitude'
       ,title = 'Valores de TKU por portos') +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom")



litoral <- muni |>
  dplyr::filter(name_muni %in% c("Paranaguá", "Antonina", "Morretes", "Guaratuba",
                                 "Matinhos", "Pontal do Paraná", "Guaraqueçaba"))

ggplot(litoral) +
  geom_sf(aes(fill = name_muni), colour = "white", linewidth = 0.3) +
  geom_sf(data = portos |> dplyr::filter(nome %in% c("Paranaguá", "Antonina")),
          colour = "black", size = 2) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.25) +
  ggspatial::annotation_north_arrow(
    location = "tr", style = ggspatial::north_arrow_fancy_orienteering(),
    height = unit(1, "cm"), width = unit(1, "cm")
  ) +
  scale_fill_brewer(palette = "RdYlBu", name = NULL) +
  labs(title = "Litoral do Paraná",
       caption = "Fonte: IBGE, malha municipal 2022 (geobr).") +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_line(colour = "grey92", linewidth = 0.2))






muni |>
  dplyr::mutate(area_km2 = sf::st_area(geom)) |>
  dplyr::mutate(area_km2 = units::set_units(area_km2, 'km^2')) |>
  dplyr::mutate(area_km2 = as.numeric(area_km2)) |>
  ggplot() +
  geom_sf(aes(fill = area_km2), colour = NA) +
  facet_wrap(~abbrev_state,ncol = 1) +
  scale_fill_viridis_c(option = "mako", direction = +1,
                       transform = "log10", name = "km²") +
  theme_void(base_size = 9)

# 19.5.1 leaflet: mapas interativos de publicação

leaflet::leaflet(portos) |>
  leaflet::addProviderTiles("Esri.WorldGrayCanvas") |>
  leaflet::addCircleMarkers(
    radius = ~sqrt(tku_bi) * 2,
    color = "#1a5b8a", fillOpacity = 0.6, stroke = FALSE,
    popup = ~paste0("<b>", nome, "</b><br>TKU: ", tku_bi, " bi t·km")
  ) |>
  leaflet::addPolygons(data = sf::st_transform(litoral, 4326),
                       fillColor = "#e07b39", fillOpacity = 0.2,
                       color = "white", weight = 1, label = ~name_muni) |>
  leaflet::addScaleBar(position = "bottomleft")



leaflet::leaflet(portos) |>
  leaflet::addProviderTiles("Esri.WorldImagery") |>
  leaflet::addCircleMarkers(
    radius = ~sqrt(tku_bi) * 2,
    color = "#1a5b8a", fillOpacity = 0.6, stroke = FALSE,
    popup = ~paste0("<b>", nome, "</b><br>TKU: ", tku_bi, " bi t·km")
  ) |>
  leaflet::addPolygons(data = sf::st_transform(litoral, 4326),
                       fillColor = "#e07b39", fillOpacity = 0.2,
                       color = "white", weight = 1, label = ~name_muni) |>
  leaflet::addScaleBar(position = "bottomleft")


# 19.5.3 mapgl: mapas vetoriais modernos

library(mapgl)

mapgl::maplibre(bounds = portos) |>
  mapgl::add_circle_layer(
    id = "portos",
    source = portos,
    circle_color = "#1a5b8a",
    circle_radius = 8,
    circle_opacity = 0.7,
    tooltip = "nome"
  )
mapgl::

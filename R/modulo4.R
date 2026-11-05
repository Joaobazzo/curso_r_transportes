library(sf)
library(dplyr)
library(ggplot2)

# mapa-base padrão dos mapview(): OpenStreetMap
mapview::mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas", "Esri.WorldImagery"))

arquivo <- "dados/brutos/portos_osm.gpkg"

if (!file.exists(arquivo)) {
  res <- osmdata::opq(bbox = c(-48.75, -25.65, -48.35, -25.35), timeout = 120) |>
    osmdata::add_osm_feature(key = "seamark:type", value = "harbour") |>
    osmdata::osmdata_sf()
  
  res$osm_points |>
    filter(!is.na(`seamark:type`)) |>     # descarta os nós que só desenham outras feições
    select(any_of(c("osm_id", "name", "seamark:harbour:category"))) |>
    st_write(arquivo, quiet = TRUE)
}

portos_osm <- st_read(arquivo, quiet = TRUE)
nrow(portos_osm)
#> [1] 12
#> 

mapview::mapview(portos_osm, col.regions = "#1a5b8a", layer.name = "Portos e marinas (OSM)")


arquivo_pontes <- "dados/brutos/pontes_br116.gpkg"

if (!file.exists(arquivo_pontes)) {
  res <- osmdata::opq(bbox = c(-49.40, -25.75, -48.40, -25.15)
                      , timeout = 4*180) |>
    osmdata::add_osm_feature(key = "bridge", value = "yes") |>
    osmdata::osmdata_sf()
  
  pontes <- res$osm_lines |>
    filter(!is.na(highway)) |>
    select(any_of(c("osm_id", "name", "highway", "maxweight", "maxheight", "layer")))
  
  st_write(pontes, arquivo_pontes, quiet = TRUE)
}

pontes <- st_read(arquivo_pontes, quiet = TRUE)
head(pontes,2)

View(pontes)

c(pontes = nrow(pontes),
  com_restricao_peso = sum(!is.na(pontes$maxweight)))
#>             pontes com_restricao_peso 
#>               1404                  8



mapview::mapview(pontes, zcol = "highway", 
                 lwd = 3, layer.name = "Tipo de via")



library(osmextract)

# install.packages("osmextract")
# 1. descobrir qual extrato cobre a área
oe_match("Ceará")
oe_match("Paraná")

# 2. baixar só o arquivo .pbf (é o que o r5r consome, @sec-m4-gtfs)
options(timeout = 5000)
oe_download(
  file_url = oe_match("Ceará")$url,
  download_directory = "dados/brutos/osm"
)

# 3. ou já ler como sf, filtrando na origem com SQL
rede <- oe_get(
  place = "Ceará",
  layer = "lines",
  query = "SELECT *
           FROM lines
           WHERE highway IN ('motorway','trunk','primary','secondary','tertiary')",
  download_directory = "dados/brutos/osm"
)


# rede sao paulo - santos -----


arquivo_rede <- "dados/brutos/rede_sao_paulo_santos.gpkg"

if (!file.exists(arquivo_rede)) {
  # servidor espelho do Overpass, menos congestionado que o principal
  osmdata::set_overpass_url("https://overpass.kumi.systems/api/interpreter")
  
  res <- osmdata::opq(bbox = c(-46.80, -24.02, -46.25, -23.45)
                      , timeout = 5*300) |>
    osmdata::add_osm_feature(key = "highway", value = c(
      "motorway", "motorway_link", "trunk", "trunk_link"
      , "primary", "primary_link",
      "secondary", "secondary_link")) |>
    osmdata::osmdata_sf()
  
  res$osm_lines |>
    select(any_of(c("osm_id", "name", "ref", "highway"
                    , "maxspeed", "oneway", "lanes"))) |>
    st_write(arquivo_rede, quiet = TRUE)
}

rede <- st_read(arquivo_rede, quiet = TRUE) |>
  rename(geometry = geom)     # o GeoPackage chama a geometria de "geom"

oe_match("Ceará")


# 

library(osmextract)

# 1. descobrir qual extrato cobre a área
oe_match("Ceará")
oe_match("Paraná")

# 2. baixar só o arquivo .pbf (é o que o r5r consome, @sec-m4-gtfs)
oe_download(
  file_url = oe_match("Ceará")$url,
  download_directory = "dados/brutos/osm"
)

# 3. ou já ler como sf, filtrando na origem com SQL
rede <- oe_get(
  place = "Ceará",
  layer = "lines",
  query = "SELECT osm_id, highway, name, maxspeed, oneway
           FROM lines
           WHERE highway IN ('motorway','trunk','primary','secondary','tertiary')",
  download_directory = "dados/brutos/osm"
)
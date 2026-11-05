library(sf)
library(dplyr)
library(ggplot2)
library(dodgr)

# mapa-base padrão dos mapview(): OpenStreetMap
mapview::mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas",
                                     "Esri.WorldImagery"))


arquivo_rede <- "dados/brutos/rede_sao_paulo_santos.gpkg"

if (!file.exists(arquivo_rede)) {
  # download lento (vários minutos): o arquivo normalmente já vem no pacote de dados
  osmdata::set_overpass_url("https://overpass.kumi.systems/api/interpreter")
  
  res <- osmdata::opq(bbox = c(-46.80, -24.02, -46.25, -23.45), timeout = 300) |>
    osmdata::add_osm_feature(key = "highway", value = c(
      "motorway", "motorway_link", "trunk", "trunk_link", "primary", "primary_link",
      "secondary", "secondary_link")) |>
    osmdata::osmdata_sf()
  
  res$osm_lines |>
    dplyr::select(dplyr::any_of(c("osm_id", "name", "ref", "highway", "maxspeed",
                                  "oneway", "lanes"))) |>
    sf::st_write(arquivo_rede, quiet = TRUE)
}

rede <- sf::st_read(arquivo_rede, quiet = TRUE) |>
  dplyr::rename(geometry = geom)



grafo <- dodgr::weight_streetnet(rede, wt_profile = "motorcar", type_col = "highway")
grafo1 <- dodgr::weight_streetnet(rede[!(rede$oneway %in% c('alternating')),]
                                  , wt_profile = "motorcar", type_col = "highway")

nrow(grafo)
nrow(grafo1)
unique(rede$oneway)
table(rede$oneway,useNA = "always")

head(rede,4)
c(trechos_sf = nrow(rede),
  arestas    = nrow(grafo),
  vertices   = nrow(dodgr::dodgr_vertices(grafo)))
#> trechos_sf    arestas   vertices 
#>      35299     253236     199773
#>      
#>      

table(grafo$component)


100 * nrow(grafo[grafo$component != 1, ])/ nrow(grafo)
# cada aresta tem as coordenadas das duas pontas: basta ligá-las
soltos <- grafo[grafo$component != 1, ]

soltos_sf <- sf::st_sf(
  componente = factor(soltos$component),
  geometry = sf::st_sfc(lapply(seq_len(nrow(soltos)), \(i) sf::st_linestring(rbind(
    c(soltos$from_lon[i], soltos$from_lat[i]),
    c(soltos$to_lon[i],   soltos$to_lat[i])
  ))), crs = 4326)
)

mapview::mapview(soltos_sf, zcol = "componente", lwd = 4, layer.name = "Componente")





head(grafo,2)

class(grafo)
nrow(rede)    # trechos de via, como linhas sf

dodgr::weighting_profiles$weighting_profiles

dodgr::weighting_profiles$weighting_profiles |>
  dplyr::filter(name == "bicycle")

dodgr::weighting_profiles$weighting_profiles |>
  dplyr::filter(name == "motorcar",
                way %in% c("motorway", "trunk", "primary", "secondary", "tertiary",
                           "residential")) |>
  dplyr::select(way, value, max_speed)

dodgr::weighting_profiles$weighting_profiles |> head()

# exportar o perfil padrão, editar as velocidades e usar o arquivo editado
dodgr::write_dodgr_wt_profile("dados/perfil_dodgr.json")

# ... editar o JSON ...
grafo <- dodgr::weight_streetnet(rede, wt_profile = "motorcar", type_col = "highway",
                                 wt_profile_file = "dados/perfil_dodgr.json")


grafo <- grafo[grafo$component == 1, ]


pontos <- data.frame(id = c("São Paulo", "Santos"),
                     x  = c(-46.6339, -46.3336),     # longitude
                     y  = c(-23.5503, -23.9345))     # latitude

verts <- dodgr::dodgr_vertices(grafo)
head(verts)
idx   <- dodgr::match_points_to_verts(verts, pontos[, c("x", "y")])   # vértice mais próximo

head(idx)
# os pontos e os vértices escolhidos, como sf
pontos_sf   <- sf::st_as_sf(pontos, coords = c("x", "y"), crs = 4326)
vertices_sf <- sf::st_as_sf(verts[idx, ], coords = c("x", "y"), crs = 4326)


library(mapview)
mapview(pontos_sf,col.regions = "blue") + 
mapview(vertices_sf,col.regions = "red") 

# distância de cada ponto ao seu vértice (by_element: 1º com 1º, 2º com 2º)
dist_ligacao <- sf::st_distance(pontos_sf, vertices_sf, by_element = TRUE)

ligacao <- data.frame(
  ponto   = pontos$id,
  vertice = verts$id[idx],
  dist_m  = round(as.numeric(dist_ligacao))
)
ligacao

dist_m <- dodgr::dodgr_dists(grafo, from = pontos, to = pontos)
tempo_s <- dodgr::dodgr_times(grafo, from = pontos, to = pontos)

round(dist_m / 1000, 1)     # km
#>           São Paulo Santos
#> São Paulo         0   70.7
#> Santos           68    0.0
round(tempo_s / 60)         # minutos
#>           São Paulo Santos
#> São Paulo         0     59
#> Santos           52      0


caminho <- dodgr::dodgr_paths(grafo
                              , from = pontos[1, ]
                              , to = pontos[2, ])

caminho
caminho[[1]]
length(caminho[[1]][[1]])     # quantos vértices o caminho atravessa
#> [1] 1675


head(verts,2)

rota <- verts[match(caminho[[1]][[1]], verts$id), c("x", "y")] |>
  as.matrix() |>
  sf::st_linestring() |>
  sf::st_sfc(crs = 4326)

mapview::mapview(rota, color = "#b3402a", lwd = 5, layer.name = "Rota") +
  mapview::mapview(pontos_sf, col.regions = "#1a5b8a", layer.name = "Origem e destino")


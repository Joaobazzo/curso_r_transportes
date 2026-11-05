
# cabecalho ----
library(sf)
library(dplyr)
library(ggplot2)

# mapa-base padrão dos mapview(): OpenStreetMap
mapview::mapviewOptions(basemaps = c("OpenStreetMap", "CartoDB.Positron", "Esri.WorldImagery"))

arquivo <- "dados/brutos/muni_br.gpkg"

if (!file.exists(arquivo)) {
  br <- geobr::read_municipality(code_muni = "all", year = 2022,
                                 simplified = TRUE, showProgress = FALSE)
  sf::st_write(br, arquivo, quiet = TRUE)
}
br <- sf::st_read(arquivo, quiet = TRUE)

nrow(br)
#> [1] 5570
head(br, 3) |> sf::st_drop_geometry()


# censo
setores <- censobr::read_tracts(year = 2022, dataset = "Basico",
                                as_data_frame = FALSE, showProgress = FALSE)

help(read_tracts)
head(setores)
pop_setor <- setores |>
  dplyr::filter(name_state == "Paraná") |>   # V0001 = população residente
  dplyr::collect()

dim(pop_setor)
head(pop_setor)

sidrar::search_sidra("população")
sidrar::info_sidra(4709)



# osm
arquivo_br116 <- "dados/brutos/br116_serra.gpkg"

if (!file.exists(arquivo_br116)) {
  consulta <- osmdata::opq(bbox = c(-49.40, -25.75, -48.40, -25.15), timeout = 120) |>
    osmdata::add_osm_feature(key = "ref", value = "BR-116") |>
    osmdata::osmdata_sf()
  
  br116 <- consulta$osm_lines |>
    dplyr::select(dplyr::any_of(c("osm_id", "name", "ref", "highway", "lanes", "maxspeed"))) |>
    sf::st_transform(4674)
  
  sf::st_write(br116, arquivo_br116, quiet = TRUE)
}

br116 <- sf::st_read(arquivo_br116, quiet = TRUE)
head(br116,3)
nrow(br116)
#> [1] 2429
sf::st_geometry_type(br116, by_geometry = FALSE) |> unique()
#> [1] LINESTRING
#> 18 Levels: GEOMETRY POINT LINESTRING POLYGON MULTIPOINT ... TRIANGLE


mapview::mapview(br116)


arquivo <- "dados/brutos/muni_pr_sc.gpkg"
muni <- sf::st_read(arquivo, quiet = TRUE)

# library(ggplot2)

ggplot() +
  geom_sf(data = muni, fill = "grey97", colour = "grey85", linewidth = 0.2) +
  geom_sf(data = br116, colour = "#e07b39", linewidth = 0.7) +
  coord_sf(xlim = c(-49.45, -48.35), ylim = c(-25.80, -25.10)) +
  labs(caption = "Fonte: OpenStreetMap.") +
  theme_void(base_size = 10)




# osmextract: a malha viária de um estado inteiro, a partir do .pbf
options(timeout = 3600)   # 1 hora

osmextract::oe_match("Ceará", provider = "openstreetmap_fr")   # confere qual arquivo seria baixado

rede <- osmextract::oe_get(
  place = "Ceará",
  provider = "openstreetmap_fr",
  layer = "lines",
  query = "SELECT osm_id, highway, name FROM lines WHERE highway IS NOT NULL",
  download_directory = "dados/brutos/osm"
)

# sf
# ponto
porto <- sf::st_point(c(-48.5, -25.5))
class(porto)
#> [1] "XY"    "POINT" "sfg"

# a partir de uma tabela de coordenadas: o caso mais comum no dia a dia.
# em vez de digitar as coordenadas na mão, lemos de um arquivo real, com
# a localização de instalações portuárias do cadastro da ANTAQ
portos_tab <- readr::read_csv("dados/portos_antaq.csv", show_col_types = FALSE)
portos_tab


portos <- sf::st_as_sf(portos_tab, coords = c("lon", "lat"), crs = 4326)
portos

head(portos,2)

mapview::mapviewOptions(basemaps = c("OpenStreetMap", "CartoDB.Positron", "Esri.WorldImagery"))
mapview::mapview(portos)






# analise recortes

library(sf)
library(dplyr)
library(ggplot2)

# mapa-base padrão dos mapview(): OpenStreetMap
mapview::mapviewOptions(basemaps = c("OpenStreetMap", "CartoDB.Positron", "Esri.WorldImagery"))

crs_trabalho <- 31982   # SIRGAS 2000 / UTM 22S (PR/SC)

arquivo <- "dados/brutos/muni_pr_sc.gpkg"

if (!file.exists(arquivo)) {
  # uma UF de cada vez, para deixar claro o que cada chamada devolve
  muni_pr <- geobr::read_municipality(code_muni = "PR", year = 2022, showProgress = FALSE)
  muni_sc <- geobr::read_municipality(code_muni = "SC", year = 2022, showProgress = FALSE)
  
  # empilha as duas: mesmas colunas, uma UF embaixo da outra
  muni <- dplyr::bind_rows(muni_pr, muni_sc)
  
  sf::st_write(muni, arquivo, quiet = TRUE)
}

muni <- sf::st_read(arquivo, quiet = TRUE) |> sf::st_transform(crs_trabalho)

# padroniza o nome da coluna de geometria: cada fonte usa um nome diferente
# ("geom", "geometry", "SHAPE"), e isso quebra código escrito para outra base
sf::st_geometry(muni) <- "geom"

dim(muni)
#> [1] 694   9
names(muni)
#> [1] "code_muni"    "name_muni"    "code_state"   "abbrev_state" "name_state"  
#> [6] "code_region"  "name_region"  "year"         "geom"





mapview(arquivo <- "dados/brutos/muni_pr_sc.gpkg"

if (!file.exists(arquivo)) {
  # uma UF de cada vez, para deixar claro o que cada chamada devolve
  muni_pr <- geobr::read_municipality(code_muni = "PR", year = 2022, showProgress = FALSE)
  muni_sc <- geobr::read_municipality(code_muni = "SC", year = 2022, showProgress = FALSE)

  # empilha as duas: mesmas colunas, uma UF embaixo da outra
  muni <- dplyr::bind_rows(muni_pr, muni_sc)

  sf::st_write(muni, arquivo, quiet = TRUE)
}




# 
# Item 2.17 ----
# 

library(sf)
library(dplyr)
library(ggplot2)

# mapa-base padrão dos mapview(): OpenStreetMap
mapview::mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas", "Esri.WorldImagery"))

crs_trabalho <- 31982   # SIRGAS 2000 / UTM 22S (PR/SC)

arquivo <- "dados/brutos/muni_pr_sc.gpkg"

if (!file.exists(arquivo)) {
  # uma UF de cada vez, para deixar claro o que cada chamada devolve
  muni_pr <- geobr::read_municipality(code_muni = "PR", year = 2022, showProgress = FALSE)
  muni_sc <- geobr::read_municipality(code_muni = "SC", year = 2022, showProgress = FALSE)
  
  # empilha as duas: mesmas colunas, uma UF embaixo da outra
  muni <- dplyr::bind_rows(muni_pr, muni_sc)
  
  sf::st_write(muni, arquivo, quiet = TRUE)
}

muni <- sf::st_read(arquivo, quiet = TRUE) |> sf::st_transform(crs_trabalho)

# padroniza o nome da coluna de geometria: cada fonte usa um nome diferente
# ("geom", "geometry", "SHAPE"), e isso quebra código escrito para outra base
sf::st_geometry(muni) <- "geom"

dim(muni)
#> [1] 694   9
names(muni)
#> [1] "code_muni"    "name_muni"    "code_state"   "abbrev_state" "name_state"  
#> [6] "code_region"  "name_region"  "year"         "geom"

mapview::mapview(muni) # visualiza todo o dado

mapview::mapview(muni$geom) # visualiza a geometria


portos <- data.frame(
  nome = c("Paranaguá", "Antonina", "São Francisco do Sul", "Itajaí", "Imbituba"),
  lon  = c(-48.5119, -48.7119, -48.6383, -48.6486, -48.6706),
  lat  = c(-25.5016, -25.4283, -26.2400, -26.9077, -28.2300)
) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  sf::st_transform(crs_trabalho)

library(mapview)
mapview::mapview(portos)

## estimativa area ----

# perímetro: converte o polígono em linha, mede o comprimento, passa para km.
# em etapas separadas para deixar claro o que cada função devolve
muni_linhas    <- sf::st_cast(muni$geom, "MULTILINESTRING")  # polígono -> contorno
perimetro_m    <- sf::st_length(muni_linhas)                 # comprimento, em metros
perimetro_km   <- units::set_units(perimetro_m, km)          # metros -> km


head(perimetro_km,2)
muni_area <- muni |>
  dplyr::mutate(
    area_km2 = as.numeric(units::set_units(sf::st_area(geom), km^2)),
    perim_km = as.numeric(perimetro_km)
  )

head(muni_area,2)


muni_area |>
  sf::st_drop_geometry() |>
  dplyr::slice_max(area_km2, n = 5) |>
  dplyr::select(name_muni, abbrev_state, area_km2, perim_km) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 1))) |>
  knitr::kable()

centroides <- sf::st_centroid(muni)          # centro geométrico

ponto_interno <- sf::st_point_on_surface(muni)  # garantidamente dentro do polígono

library(mapview)
mapview::mapview(muni) + mapview(centroides,col.regions = "red") +
  mapview(ponto_interno,col.regions = "green")

# 17.2 Buffer: a área de influência ----

buffer_portos <- sf::st_buffer(portos, dist = 15000)   # 15 km, em metros (CRS projetado)

as.numeric(units::set_units(sf::st_area(buffer_portos), km^2)) |> round(1)
#> [1] 706.5 706.5 706.5 706.5 706.5

  mapview::mapview(buffer_portos) + mapview::mapview(portos)


  
buffer_uniao <- sf::st_union(buffer_portos)   # os 5 buffers como uma única geometria
  

mapview(buffer_uniao)
municipios_intersecta <- muni[sf::st_intersects(muni, buffer_uniao, sparse = FALSE)[, 1], ]
municipios_toca       <- muni[sf::st_touches(muni, buffer_uniao, sparse = FALSE)[, 1], ]
  
c(intersecta = nrow(municipios_intersecta)
  , toca_so_pela_borda = nrow(municipios_toca))
  #>         intersecta toca_so_pela_borda 
  #>                 21                  0
mapview(muni)



ggplot() +
  geom_sf(data = muni, fill = "grey97", colour = "grey85", linewidth = 0.2) +
  geom_sf(data = municipios_intersecta, fill = "#cfe0ee"
          , colour = "#1a5b8a", linewidth = 0.3) +
  geom_sf(data = buffer_portos, fill = "#1a5b8a", alpha = 0.3, colour = "#1a5b8a") +
  geom_sf(data = portos, size = 1.8, colour = "#0d3b5c") +
  geom_sf_text(data = portos, aes(label = nome), size = 2.6, nudge_x = 25000
               , nudge_y = 25000) +
  theme_void(base_size = 10)


mapview::mapview(municipios_intersecta, col.regions = "#1a5b8a", alpha.regions = 0.3,
                 layer.name = "Municípios afetados") +
  mapview::mapview(buffer_portos, col.regions = "#e07b39", layer.name = "Área de influência") +
  mapview::mapview(portos, layer.name = "Portos")


# quais municípios contêm um porto? em etapas: a matriz de comparação,
# depois "algum TRUE na linha", depois o filtro
matriz_intersecta <- sf::st_intersects(muni, portos, sparse = FALSE)  # matriz municípios x portos
tem_porto         <- apply(matriz_intersecta, 1, any)                 # TRUE se a linha tem algum TRUE
com_porto         <- muni[tem_porto, ]

com_porto |> sf::st_drop_geometry() |> dplyr::select(name_muni, abbrev_state)


# municípios vizinhos de Paranaguá (compartilham fronteira). em etapas:
pgua         <- muni |> dplyr::filter(name_muni == "Cascavel")
matriz_toca  <- sf::st_is_within_distance(muni, pgua, dist=15000,sparse = FALSE)  # matriz municípios x 1 (Paranaguá)
eh_vizinho   <- matriz_toca[, 1]                              # única coluna, vira vetor lógico
vizinhos     <- muni[eh_vizinho, ]

vizinhos |> sf::st_drop_geometry() |> dplyr::pull(name_muni)


mapview(vizinhos,col.regions = "blue") + mapview(pgua,col.regions = "#e07b39")
#> character(0)

# 17.4 juncao espacial -----

portos
head(muni,2)
portos_com_muni <- sf::st_join(portos, 
                               muni |> 
                                 dplyr::select(name_muni, abbrev_state, code_muni))

head(portos_com_muni)
portos_com_muni |>
  sf::st_drop_geometry() |>
  knitr::kable()

# 17.5 interseccao -------

# INTERSEÇÃO: recorta uma camada pela outra, mantendo os atributos das duas
recorte <- sf::st_intersection(muni, buffer_portos |> sf::st_union())
head(recorte)
nrow(muni)
#> [1] 694
nrow(recorte)     # só os municípios tocados, e só a parte dentro do buffer
#> [1] 21

# recorta a VISUALIZAÇÃO para a vizinhança do resultado, com uma margem de 20 km:
# sem isso, o mapa mostra o litoral inteiro de PR/SC, quase todo vazio
zoom <- sf::st_bbox(sf::st_buffer(sf::st_union(buffer_portos), 10000))

ggplot() +
  geom_sf(data = muni, fill = "grey97", colour = "grey85", linewidth = 0.2) +
  geom_sf(data = recorte, aes(fill = abbrev_state), colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = c(PR = "#1a5b8a", SC = "#e07b39"), name = "UF") +
  coord_sf(xlim = zoom[c("xmin", "xmax")], ylim = zoom[c("ymin", "ymax")]
           , expand = FALSE) +
  theme_void(base_size = 10)


# matriz de distâncias porto × porto, em km
d <- sf::st_distance(portos) |>
  units::set_units(km) |>
  as.numeric() |>
  matrix(nrow = nrow(portos), dimnames = list(portos$nome, portos$nome))

head(d)



# para cada município, a distância ao porto mais próximo
muni_d <- muni |>
  dplyr::mutate(
    dist_porto_km = round(
      apply(sf::st_distance(sf::st_point_on_surface(geom), portos), 1, min) / 1000, 1),
    porto_proximo = portos$nome[sf::st_nearest_feature(sf::st_point_on_surface(geom), portos)]
  )


head(muni_d)
muni_d |>
  sf::st_drop_geometry() |>
  dplyr::slice_max(dist_porto_km, n = 5) |>
  dplyr::select(name_muni, abbrev_state, porto_proximo, dist_porto_km) |>
  knitr::kable()



muni_simples <- sf::st_simplify(muni, dTolerance = 200, preserveTopology = TRUE)

pgua_simples <- sf::st_simplify(pgua, dTolerance = 200, preserveTopology = TRUE)


mapview::mapview(pgua, color = "#1a5b8a", col.regions = NA, lwd = 3,                  layer.name = "Original") +  
  mapview::mapview(pgua_simples, color = "#e07b39", col.regions = NA, lwd = 2,                    layer.name = "Simplificado (200 m)")


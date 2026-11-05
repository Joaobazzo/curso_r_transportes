rodovias_sugeridas <- data.frame(
  br     = c("BR-116", "BR-101", "BR-153", "BR-262", "BR-381"),
  trecho = c("Serra do Mar, Curitiba-litoral",
             "Litoral catarinense, Florianópolis-Joinville",
             "Transbrasiliana, trecho em Goiás",
             "Serra do Espinhaço, Vitória-Belo Horizonte",
             "Fernão Dias, São Paulo-Belo Horizonte"),
  uf     = c("PR/SC", "SC", "GO", "ES/MG", "SP/MG"),
  motivo = c("relevo acentuado, corredor até o Porto de Paranaguá (o exemplo deste capítulo)",
             "eixo costeiro, ocupação urbana praticamente contínua",
             "rodovia longa e de baixa densidade, contraste com um trecho urbano",
             "corredor de minério, relevo montanhoso",
             "um dos corredores rodoviários mais movimentados do país")
)

knitr::kable(rodovias_sugeridas)



library(sf)
library(mapview)
library(dplyr)
library(ggplot2)

# mapa-base padrão dos mapview(): OpenStreetMap
mapview::mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas", "Esri.WorldImagery"))

crs_trabalho <- 31982    # SIRGAS 2000 / UTM 22S, em metros, para medir e fazer buffer

# 20.1 passo 1: eixo viario -----



arquivo_br116 <- "dados/brutos/br116_serra.gpkg"

if (!file.exists(arquivo_br116)) {
  consulta <- osmdata::opq(bbox = c(-49.40, -25.75, -48.40, -25.15)
                           , timeout = 120) |>
    osmdata::add_osm_feature(key = "ref", value = "BR-116") |>
    osmdata::osmdata_sf()
  # mapview(consulta$osm_polygons$geometry)+
  # mapview(consulta$osm_lines$geometry)
  
  recorte <- consulta$osm_lines |>
    dplyr::select(dplyr::any_of(c("osm_id", "name", "ref", "highway", "lanes", "maxspeed"))) |>
    sf::st_transform(4674)
  
  #mapview(recorte$geometry)
  
  sf::st_write(recorte,
    arquivo_br116, quiet = TRUE
  )
}

br116 <- sf::st_read(arquivo_br116, quiet = TRUE) |>
  sf::st_transform(crs_trabalho)

c(trechos = nrow(br116),
  extensao_km = round(as.numeric(sum(sf::st_length(br116))) / 1000, 1))
#>     trechos extensao_km 
#>      2429.0      1656.6
#>      


eixo <- sf::st_union(br116)

eixo
c(trechos = nrow(eixo),
  extensao_km = round(as.numeric(sum(sf::st_length(eixo))) / 1000, 1))


# 20.2 Passo 2: A faixa de influência
faixas <- c(2000, 5000, 10000)

influencia <- lapply(faixas, \(d) {
  sf::st_buffer(eixo, dist = d) |>
    sf::st_as_sf() |>
    dplyr::mutate(faixa_m = d)
})

influencia <- influencia |>
  dplyr::bind_rows() |>
  dplyr::rename(geom = x) |>
  dplyr::arrange(dplyr::desc(faixa_m))     # do maior para o menor, para desenhar em camadas

head(influencia,4)

mapview(eixo) + mapview(influencia[influencia$faixa == 2000,])
# área de cada faixa, numa tabela à parte
area_km2 <- units::set_units(sf::st_area(influencia), km^2)

tabela_faixas <- sf::st_drop_geometry(influencia)
tabela_faixas$area_km2 <- round(as.numeric(area_km2), 1)

knitr::kable(tabela_faixas)

# 20.3 Passo 3: População pela grade estatística do IBGE
arquivo_grade <- "dados/brutos/grade_pr.gpkg"

if (!file.exists(arquivo_grade)) {
  grade_pr <- geobr::read_statistical_grid(year = 2022
                                           , code_muni = "PR"
                                           , showProgress = FALSE)
  sf::st_write(grade_pr, arquivo_grade, quiet = TRUE)
}

grade <- sf::st_read(arquivo_grade, quiet = TRUE) |> 
  sf::st_transform(crs_trabalho)

dim(grade)
#> [1] 497935     15
names(grade)
#>  [1] "code_muni"    "name_muni"    "id_unico"     "nome_1km"     "nome_5km"    
#>  [6] "nome_10km"    "nome_50km"    "nome_100km"   "nome_500km"   "pop_total"   
#> [11] "total_dom"    "code_state"   "abbrev_state" "quadrante"    "geom"
head(grade,2)


# recorte espacial pela maior faixa, feito antes de qualquer conta:
# reduz de centenas de milhares de células para o que interessa
maior_faixa <- influencia |> dplyr::filter(faixa_m == max(faixa_m))

grade_recorte <- grade[sf::st_intersects(grade, maior_faixa, sparse = FALSE)[, 1], ]
nrow(grade_recorte)
#> [1] 35682



ponto_eixo <- sf::st_sfc(sf::st_point(c(-49.2066, -25.3895)), crs = 4326) |>
  sf::st_transform(crs_trabalho)

parcela_grade <- grade_recorte[
  sf::st_is_within_distance(grade_recorte, ponto_eixo, dist = 3000, sparse = FALSE)[, 1], ]

head(parcela_grade,2)
mapview::mapview(parcela_grade, zcol = "pop_total", layer.name = "População por célula") +
  mapview::mapview(br116, color = "#b3402a", lwd = 3, layer.name = "BR-116")

# 20.4 Passo 4: População por faixa

pop_por_faixa <- lapply(sort(faixas), \(d) {
  f <- influencia |> dplyr::filter(faixa_m == d)
  dentro <- grade_recorte[sf::st_intersects(grade_recorte, f, sparse = FALSE)[, 1], ]
  data.frame(
    faixa_m   = d,
    celulas   = nrow(dentro),
    populacao = sum(dentro$pop, na.rm = TRUE)
  )
}) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    area_km2 = as.numeric(units::set_units(
      sf::st_area(influencia$geom[match(faixa_m, influencia$faixa_m)]), km^2)),
    densidade = populacao / area_km2
  )

pop_por_faixa |>
  dplyr::transmute(
    `Faixa (km)` = faixa_m / 1000,
    `Área (km²)` = round(area_km2, 1),
    `População`  = scales::comma(populacao, big.mark = "."),
    `Hab./km²`   = round(densidade, 1)
  ) |>
  knitr::kable(align = "rrrr")


# 20.5 Passo 5: Distribuição por município
arquivo <- "dados/brutos/muni_pr_sc.gpkg"
if (!file.exists(arquivo)) {
  muni <- dplyr::bind_rows(
    geobr::read_municipality(code_muni = "PR", year = 2022, showProgress = FALSE),
    geobr::read_municipality(code_muni = "SC", year = 2022, showProgress = FALSE)
  )
  sf::st_write(muni, arquivo, quiet = TRUE)
}
muni <- sf::st_read(arquivo, quiet = TRUE) |> sf::st_transform(crs_trabalho)

faixa5 <- influencia |> dplyr::filter(faixa_m == 5000)

pop_muni <- grade_recorte[sf::st_intersects(grade_recorte, faixa5, sparse = FALSE)[, 1], ] |>
  # a grade já vem com code_muni/name_muni do IBGE; descartamos para evitar
  # colisão de nomes com o join abaixo, que é a fonte que queremos usar
  dplyr::select(pop_total) |>
  # o join usa o CENTRO de cada célula: com st_intersects, células sobre a divisa
  # seriam contadas em dois municípios e a população total dobraria
  sf::st_point_on_surface() |>
  sf::st_join(muni |> dplyr::select(code_muni, name_muni, abbrev_state)) |>
  sf::st_drop_geometry() |>
  dplyr::summarise(populacao = sum(pop_total, na.rm = TRUE),
                   celulas = dplyr::n(),
                   .by = c(code_muni, name_muni, abbrev_state)) |>
  dplyr::filter(!is.na(code_muni)) |>
  dplyr::arrange(dplyr::desc(populacao))

pop_muni |>
  dplyr::mutate(part = populacao / sum(populacao)) |>
  dplyr::transmute(Município = name_muni, UF = abbrev_state,
                   `População na faixa de 5 km` = scales::comma(populacao, big.mark = "."),
                   `Part. (%)` = scales::percent(part, accuracy = 0.1)) |>
  knitr::kable(align = "llrr")


# 20.6 Passo 6: O mapa -----
recorte_vis <- sf::st_bbox(influencia |> 
                             dplyr::filter(faixa_m == 10000))
#st_crs()
recorte_vis <- muni |> dplyr::filter(name_muni == "Curitiba")|>
  sf::st_transform(crs_trabalho) |> st_bbox()
ggplot() +
  geom_sf(data = muni, fill = "grey98", colour = "grey80"
          , linewidth = 0.2) +
  geom_sf(data = grade_recorte |> dplyr::filter(pop_total > 0),
          aes(fill = pop_total), colour = NA,alpha = 0.3) +
  geom_sf(data = influencia, fill = NA, colour = "#1a5b8a",
          linewidth = 0.4, linetype = "dashed") +
  geom_sf(data = br116, colour = "#b3402a", linewidth = 0.8) +
  scale_fill_viridis_c(option = "rocket", direction = -1, transform = "log10",
                       name = "População\npor célula") +
  coord_sf(xlim = recorte_vis[c("xmin", "xmax")]
           ,ylim = recorte_vis[c("ymin", "ymax")]
           , expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25) +
  labs(
    title = "BR-116 / Serra do Mar: faixas de influência de 2, 5 e 10 km",
    caption = paste("Fontes: eixo viário OpenStreetMap; população IBGE, grade estatística.",
                    "Elaboração própria.")
  ) +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11))


# plot do site

zoom_curitiba <- muni |>
  dplyr::filter(name_muni == "Curitiba") |>
  sf::st_buffer(8000) |>
  sf::st_bbox()

ggplot() +
  geom_sf(data = muni, fill = "grey98", colour = "grey80", linewidth = 0.2) +
  geom_sf(data = grade_recorte |> dplyr::filter(pop_total > 0),
          aes(fill = pop_total), colour = NA) +
  geom_sf(data = influencia, fill = NA, colour = "#1a5b8a",
          linewidth = 0.4, linetype = "dashed") +
  # contorno branco por baixo + linha grossa por cima: a rodovia se destaca da grade
  geom_sf(data = br116, colour = "white", linewidth = 3) +
  geom_sf(data = br116, colour = "#1a5b8a", linewidth = 1.6) +
  scale_fill_viridis_c(option = "rocket", direction = -1, transform = "log10",
                       name = "População\npor célula") +
  coord_sf(xlim = zoom_curitiba[c("xmin", "xmax")],
           ylim = zoom_curitiba[c("ymin", "ymax")], expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25) +
  labs(title = "Detalhe: início do corredor, região de Curitiba") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11))

library(dplyr)

con <- duckdb::dbConnect(duckdb::duckdb())
links <- DBI::dbGetQuery(con, "
  SELECT * FROM 'dados/processados/modelo4etapas/links_rmc_2018_completo.parquet'
")
duckdb::dbDisconnect(con, shutdown = TRUE)

unique(links$typeno)

completa <- links |>
  summarise(veic_km = sum(veic_km, na.rm = TRUE),
            atraso  = sum(veic_h_carreg - veic_h_livre, na.rm = TRUE))
completa

estrutural <- links |> filter(typeno < 50) |>
  summarise(veic_km = sum(veic_km, na.rm = TRUE),
            atraso  = sum(veic_h_carreg - veic_h_livre, na.rm = TRUE))

estrutural

rodovias <- links |> filter(typeno %/% 10 == 1) |>
  summarise(veic_km = sum(veic_km, na.rm = TRUE),
            atraso  = sum(veic_h_carreg - veic_h_livre, na.rm = TRUE))
rodovias


#' Indicadores de desempenho de uma rede alocada
#'
#' @param links data.frame com as colunas corrigidas do recorte do VISUM
#' @param cenario rótulo do cenário, para identificar a linha no resultado
#' @return data.frame de uma linha
indicadores_rede <- function(links, cenario = "sem nome") {
  obrigatorias <- c("veic_km", "veic_h_livre", "veic_h_carreg", "vc")
  faltando <- setdiff(obrigatorias, names(links))
  if (length(faltando) > 0) {
    stop("Faltam colunas: ", paste(faltando, collapse = ", "))
  }
  
  links |>
    summarise(
      links         = n(),
      veic_km       = sum(veic_km, na.rm = TRUE),
      atraso_veic_h = sum(veic_h_carreg - veic_h_livre, na.rm = TRUE),
      vel_media_kmh = sum(veic_km, na.rm = TRUE) / sum(veic_h_carreg, na.rm = TRUE),
      saturados     = sum(vc >= 1, na.rm = TRUE)
    ) |>
    mutate(cenario = cenario, .before = 1)
}


indicadores_rede(links, "Rede completa")
r <- indicadores_rede(links)
r

cenarios <- list(
  "Rede completa"   = \(x) x,
  "Rede estrutural" = \(x) filter(x, typeno < 50),
  "Só rodovias"     = \(x) filter(x, typeno %/% 10 == 1),
  "Só conectores"   = \(x) filter(x, typeno %/% 10 == 7)
)

resultado <- purrr::imap(cenarios, 
                         \(filtro, nome) indicadores_rede(filtro(links), nome)) |>
  purrr::list_rbind()

round(resultado$saturados / resultado$links  , 2) 


resultado |>
  mutate(across(where(is.numeric), \(v) round(v, 1))) |>
  knitr::kable(format.args = list(big.mark = "."))


# for 


# for: você cuida do recipiente
saida <- vector("list", length(cenarios))
for (i in seq_along(cenarios)) {
  saida[[i]] <- indicadores_rede(cenarios[[i]](links), names(cenarios)[i])
}
saida
# lapply: base R, sem pacote
lapply(names(cenarios), \(n) indicadores_rede(cenarios[[n]](links), n))

# purrr: família com o tipo de saída no nome
purrr::map(cenarios, \(f) nrow(f(links)))       # lista
class(purrr::map(cenarios, \(f) nrow(f(links)))     )
purrr::map_int(cenarios, \(f) nrow(f(links)))   # vetor de inteiros, ou erro

# declividade

library(sf)
library(terra)
library(dplyr)
library(ggplot2)

crs_trabalho <- 31982

if (!file.exists("dados/brutos/dem_serra.tif")) {
  area <- sf::st_bbox(c(xmin = -49.40, ymin = -25.75, xmax = -48.40, ymax = -25.15),
                      crs = sf::st_crs(4326)) |> sf::st_as_sfc() |> sf::st_as_sf()
  terra::writeRaster(
    terra::rast(elevatr::get_elev_raster(locations = area, z = 11, clip = "bbox")),
    "dados/brutos/dem_serra.tif", overwrite = TRUE)
}

dem <- terra::rast("dados/brutos/dem_serra.tif") |> 
  terra::project("EPSG:31982")
names(dem) <- "altitude"

declividade <- tan(terra::terrain(dem, v = "slope", unit = "radians")) * 100
names(declividade) <- "declividade"

terra::res(dem)

c(resolucao_m = round(terra::res(dem)[1], 1),
  alt_min = round(terra::global(dem, "min", na.rm = TRUE)[[1]]),
  alt_max = round(terra::global(dem, "max", na.rm = TRUE)[[1]]))



#> resolucao_m     alt_min     alt_max 
#>        68.7      -120.0      1830.0
#>        

if (!file.exists("dados/brutos/br116_serra.gpkg")) {
  consulta <- osmdata::opq(bbox = c(-49.40, -25.75, -48.40, -25.15), timeout = 120) |>
    osmdata::add_osm_feature(key = "ref", value = "BR-116") |>
    osmdata::osmdata_sf()
  sf::st_write(consulta$osm_lines |>
                 dplyr::select(dplyr::any_of(c("osm_id", "name", "ref", "highway"))) |>
                 sf::st_transform(4674),
               "dados/brutos/br116_serra.gpkg", quiet = TRUE)
}

br116 <- sf::st_read("dados/brutos/br116_serra.gpkg", quiet = TRUE) |>
  sf::st_transform(crs_trabalho)

library(mapview)

mapview::mapview(br116$geom) + mapview::mapview(area_recorte)
# o Overpass devolve a geometria inteira de cada via, não recortada pelo bbox
# da consulta. Sem este filtro, o trecho mais longo pode vir de bem longe da Serra
area_recorte <- sf::st_bbox(c(xmin = -49.40, ymin = -25.75, xmax = -48.40, ymax = -25.15),
                            crs = sf::st_crs(4326)) |> 
  sf::st_as_sfc() |> 
  sf::st_transform(crs_trabalho)

br116 <- br116[sf::st_intersects(br116, area_recorte, sparse = FALSE)[, 1], ]

mapview::mapview(br116$geom) + mapview::mapview(area_recorte)

dim(br116)
# alternativa 1: o trecho contínuo mais longo da BR-116 no recorte
alt1 <- br116 |>
  dplyr::mutate(comp = as.numeric(sf::st_length(geom))) |>
  dplyr::slice_max(comp, n = 1) |>
  sf::st_geometry()
alt1
mapview::mapview(alt1) + mapview::mapview(area_recorte) 

View(br116)
# alternativa 2: traçado hipotético entre os mesmos extremos, por outro caminho
extremos <- sf::st_cast(alt1, "POINT")
p_ini <- extremos[1]
p_fim <- extremos[length(extremos)]

desvio <- sf::st_point(colMeans(rbind(sf::st_coordinates(p_ini), sf::st_coordinates(p_fim))) +
                         c(4000, -6000))

alt2 <- sf::st_sfc(
  sf::st_linestring(rbind(sf::st_coordinates(p_ini), sf::st_coordinates(desvio),
                          sf::st_coordinates(p_fim))),
  crs = crs_trabalho
)

tracados <- sf::st_sf(
  alternativa = c("Alt. 1: traçado existente", "Alt. 2: traçado hipotético"),
  geometry = c(alt1, alt2)
) |>
  dplyr::mutate(extensao_km = round(as.numeric(sf::st_length(geometry)) / 1000, 1))

mapview::mapview(tracados["alternativa"])

sf::st_drop_geometry(tracados)

# passo 3

segmentar <- function(linha, passo_m = 200) {
  comp <- as.numeric(sf::st_length(linha))
  n <- max(2, floor(comp / passo_m))
  
  pontos <- sf::st_line_sample(linha, n = n) |> sf::st_cast("POINT")
  coords <- sf::st_coordinates(pontos)
  
  segs <- lapply(seq_len(nrow(coords) - 1), \(i) {
    sf::st_linestring(coords[i:(i + 1), ])
  }) |>
    sf::st_sfc(crs = sf::st_crs(linha))
  
  sf::st_sf(seg_id = seq_along(segs), geometry = segs) |>
    dplyr::mutate(
      comp_m = as.numeric(sf::st_length(geometry)),
      dist_acum_km = cumsum(comp_m) / 1000
    )
}

segmentos <- dplyr::bind_rows(
  segmentar(alt1) |> dplyr::mutate(alternativa = tracados$alternativa[1]),
  segmentar(alt2) |> dplyr::mutate(alternativa = tracados$alternativa[2])
)

head(segmentos)

mapview::mapview(segmentos["seg_id"])
dplyr::count(sf::st_drop_geometry(segmentos), alternativa)


# 24.4 Passo 4: Cruzar com o terreno ------

segmentos <- segmentos |>
  dplyr::mutate(
    # 1. declividade média do terreno sob o segmento
    decliv_terreno = terra::extract(declividade, terra::vect(segmentos),
                                    fun = mean, na.rm = TRUE, ID = FALSE)[[1]],
    # 2. rampa do próprio traçado
    alt_ini = terra::extract(dem, terra::vect(sf::st_line_sample(geometry, sample = 0) |>
                                                sf::st_cast("POINT")), ID = FALSE)[[1]],
    alt_fim = terra::extract(dem, terra::vect(sf::st_line_sample(geometry, sample = 1) |>
                                                sf::st_cast("POINT")), ID = FALSE)[[1]],
    rampa_pct = 100 * (alt_fim - alt_ini) / comp_m
  )

head(segmentos,3)
summary(segmentos$decliv_terreno)

# passo 5

limite_desejavel <- 4    # %
limite_maximo    <- 6    # %

segmentos <- segmentos |>
  dplyr::mutate(
    classe = dplyr::case_when(
      abs(rampa_pct) <= limite_desejavel ~ "até 4% (desejável)",
      abs(rampa_pct) <= limite_maximo    ~ "4% a 6% (admissível)",
      TRUE                               ~ "acima de 6% (crítico)"
    ),
    classe = factor(classe, levels = c("até 4% (desejável)", "4% a 6% (admissível)",
                                       "acima de 6% (crítico)"))
  )

head(segmentos,3)

resumo <- segmentos |>
  sf::st_drop_geometry() |>
  dplyr::summarise(km = sum(comp_m) / 1000, .by = c(alternativa, classe)) |>
  dplyr::mutate(part = km / sum(km), .by = alternativa)

resumo |>
  dplyr::mutate(km = round(km, 1), part = scales::percent(part, accuracy = 0.1)) |>
  dplyr::arrange(alternativa, classe) |>
  knitr::kable(align = "llrr")

# 24.6 Passo 6: O perfil e o mapa
perfil <- segmentos |>
  sf::st_drop_geometry() |>
  dplyr::mutate(dist_km = dist_acum_km)

head(perfil,2)

g1 <- ggplot(perfil, aes(dist_km, alt_ini, colour = alternativa)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("#1a5b8a", "#b3402a"), name = NULL) +
  labs(x = NULL, y = "Altitude (m)") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top")

g2 <- ggplot(perfil, aes(dist_km, rampa_pct, colour = alternativa)) +
  geom_hline(yintercept = c(-6, -4, 4, 6), linetype = "dotted", colour = "grey60") +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  scale_colour_manual(values = c("#1a5b8a", "#b3402a"), guide = "none") +
  labs(x = "Distância ao longo do traçado (km)", y = "Rampa (%)") +
  theme_minimal(base_size = 9)

patchwork::wrap_plots(g1, g2, ncol = 1, heights = c(1, 1))


recorte <- sf::st_bbox(sf::st_buffer(
  sf::st_union(tracados), 4000))

decliv_recorte <- terra::crop(declividade,
                              terra::ext(recorte[c("xmin", "xmax", "ymin", "ymax")]))

g3 <- ggplot() +
  tidyterra::geom_spatraster(data = decliv_recorte) +
  geom_sf(data = segmentos, aes(colour = classe), linewidth = 0.9) +
  scale_fill_gradientn(colours = c("#f7f7f7", "#cfd8dc", "#8fa3ad", "#4d5c63"),
                       na.value = NA, name = "Declividade\ndo terreno (%)") +
  scale_colour_manual(values = c("até 4% (desejável)" = "#1a9850",
                                 "4% a 6% (admissível)" = "#e0a03f",
                                 "acima de 6% (crítico)" = "#b3402a"),
                      name = "Rampa do traçado") +
  coord_sf(xlim = recorte[c("xmin", "xmax")]
           , ylim = recorte[c("ymin", "ymax")],
           expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25) +
  labs(caption = paste("Fontes: altimetria AWS Terrain Tiles (via elevatr);",
                       "eixo viário OpenStreetMap. Elaboração própria.")) +
  theme_void(base_size = 9) +
  theme(legend.position = "right")


patchwork::wrap_plots(g3, g1, ncol = 1, heights = c(3, 0.5))

# passo 7


if (!file.exists("dados/brutos/rodovias_serra_pr.gpkg")) {
  consulta <- osmdata::opq(bbox = c(-49.10, -25.62, -48.70, -25.30), timeout = 240) |>
    osmdata::add_osm_feature(key = "ref", value = c("BR-277", "PR-410")) |>
    osmdata::osmdata_sf()
  sf::st_write(consulta$osm_lines |>
                 dplyr::select(dplyr::any_of(c("osm_id", "name", "ref", "highway"))) |>
                 sf::st_transform(4674),
               "dados/brutos/rodovias_serra_pr.gpkg", quiet = TRUE)
}

zona_serra <- sf::st_bbox(c(xmin = -49.10, ymin = -25.62
                            , xmax = -48.70, ymax = -25.30),
                          crs = sf::st_crs(4326)) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs_trabalho)

mapview(zona_serra)

rodovias_serra <- sf::st_read("dados/brutos/rodovias_serra_pr.gpkg", quiet = TRUE) |>
  dplyr::rename(geometry = geom) |>          # o GeoPackage chama a geometria de "geom"
  dplyr::filter(ref %in% c("BR-277", "PR-410")) |>
  sf::st_transform(crs_trabalho) |>
  sf::st_intersection(zona_serra) |>
  dplyr::group_by(ref) |>
  dplyr::summarise() |>                  # junta os trechos de cada rodovia
  sf::st_line_merge() |>
  sf::st_cast("LINESTRING")              # uma linha por pista

mapview(zona_serra) + mapview(rodovias_serra)

rodovias_serra |>
  dplyr::mutate(km = round(as.numeric(sf::st_length(geometry)) / 1000, 1)) |>
  sf::st_drop_geometry()


# a pista mais longa de cada rodovia
rodovias_serra <- rodovias_serra |>
  dplyr::mutate(comp = as.numeric(sf::st_length(geometry))) |>
  dplyr::slice_max(comp, n = 1, by = ref)

rodovias_serra

decliv_serra <- terra::crop(declividade, terra::vect(zona_serra))

faixas_decliv <- terra::classify(decliv_serra, rcl = matrix(c(
  -Inf, 30, 1,
  30, 47, 2,
  47, Inf, 3
), ncol = 3, byrow = TRUE))

plot(faixas_decliv, type = "classes",
     levels = c("até 30%", "30% a 47%", "acima de 47%"),
     col = c("#f0f0f0", "#e0a03f", "#b3402a"),
     main = "Declividade do terreno na serra")
plot(terra::vect(rodovias_serra), add = TRUE, lwd = 2.5, col = "#1a5b8a")

segmentos_serra <- lapply(seq_len(nrow(rodovias_serra)), \(i) {
  segmentar(sf::st_geometry(rodovias_serra)[i], passo_m = 200) |>
    dplyr::mutate(ref = rodovias_serra$ref[i])
}) |>
  dplyr::bind_rows()

# faixa de 150 m de cada lado do segmento, sem as pontas arredondadas
faixa_150 <- sf::st_buffer(segmentos_serra, 150, endCapStyle = "FLAT")

# a média de um raster 0/1 é a fração de células que valem 1
ingreme <- declividade > 30

segmentos_serra <- segmentos_serra |>
  dplyr::mutate(
    pct_ingreme  = 100 * terra::extract(ingreme, terra::vect(faixa_150),
                                        fun = mean, na.rm = TRUE, ID = FALSE)[[1]],
    decliv_media = terra::extract(declividade, terra::vect(faixa_150),
                                  fun = mean, na.rm = TRUE, ID = FALSE)[[1]],
    exposicao = cut(pct_ingreme, breaks = c(-Inf, 10, 50, Inf),
                    labels = c("baixa (< 10%)", "média (10% a 50%)", "alta (> 50%)"))
  )

segmentos_serra |>
  sf::st_drop_geometry() |>
  dplyr::select(ref, seg_id, dist_acum_km, pct_ingreme, decliv_media, exposicao) |>
  dplyr::slice_max(pct_ingreme, n = 6, with_ties = FALSE)

segmentos_serra |>
  sf::st_drop_geometry() |>
  dplyr::summarise(km = sum(comp_m) / 1000, .by = c(ref, exposicao)) |>
  dplyr::mutate(part = km / sum(km), .by = ref) |>
  dplyr::arrange(ref, exposicao) |>
  dplyr::mutate(km = round(km, 1), part = scales::percent(part, accuracy = 0.1)) |>
  knitr::kable(col.names = c("Rodovia", "Exposição a encosta íngreme", "km", "Part."),
               align = "llrr")


ggplot(sf::st_drop_geometry(segmentos_serra), aes(dist_acum_km, pct_ingreme)) +
  geom_col(aes(fill = exposicao), width = 0.2) +
  facet_wrap(~ref, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = c("#cfd8dc", "#e0a03f", "#b3402a"), name = NULL) +
  labs(x = "Distância ao longo da rodovia, na zona da serra (km)",
       y = "% da faixa com encosta > 30%") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top")









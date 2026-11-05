library(terra)
library(sf)
library(dplyr)
library(ggplot2)

# mapa-base padrão dos mapview(): OpenStreetMap
mapview::mapviewOptions(basemaps = c("OpenStreetMap"
                                     , "Esri.WorldGrayCanvas"
                                     , "Esri.WorldImagery"))


municipio_dem <- "Morretes"
uf_dem        <- "PR"

arquivo_dem <- "dados/brutos/dem_morretes.tif"

if (!file.exists(arquivo_dem)) {
  area_muni <- geobr::read_municipality(code_muni = uf_dem
                                        , year = 2022
                                        , showProgress = FALSE) |>
    dplyr::filter(name_muni == municipio_dem)
  
  # margem de 5 km em volta do município, para o MDE ter contexto
  # e não cortar exatamente na fronteira administrativa
  area <- area_muni |> sf::st_buffer(dist = 5000) |> sf::st_transform(4326)
  
  dem_raw <- elevatr::get_elev_raster(locations = area, z = 10, clip = "bbox")
  terra::writeRaster(terra::rast(dem_raw), arquivo_dem, overwrite = TRUE)
}

arquivo_dem
dem <- terra::rast(arquivo_dem)
dem
#> class       : SpatRaster
#> size        : 843, 642, 1  (nrow, ncol, nlyr)
#> resolution  : 0.000654097, 0.000654097  (x, y)
#> extent      : -49.03887, -48.61894, -25.78395, -25.23255  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)
#> source      : dem_morretes.tif
#> name        : fileb2883588ae1
#> min value   :             -58
#> max value   :            1838
#> 
#> 

terra::plot(dem, main = "Altitude (m)")


arquivo <- "dados/brutos/muni_pr_sc.gpkg"
if (!file.exists(arquivo)) {
  muni <- dplyr::bind_rows(
    geobr::read_municipality(code_muni = "PR", year = 2022, showProgress = FALSE),
    geobr::read_municipality(code_muni = "SC", year = 2022, showProgress = FALSE)
  )
  sf::st_write(muni, arquivo, quiet = TRUE)
}
muni <- sf::st_read(arquivo, quiet = TRUE)

# limites da visualização a partir da extensão do próprio raster, não digitados na mão:
# assim o mapa se ajusta sozinho se você trocar municipio_dem por outro lugar
zoom_dem <- sf::st_bbox(dem)

ggplot() +
  tidyterra::geom_spatraster(data = dem) +
  geom_sf(data = muni, fill = NA, colour = "white", linewidth = 0.75) +
  coord_sf(xlim = zoom_dem[c("xmin", "xmax")], ylim = zoom_dem[c("ymin", "ymax")], expand = FALSE) +
  scale_fill_viridis_c(option = "turbo", na.value = NA, name = "Altitude (m)") +
  theme_void(base_size = 10)


morretes <- muni |> dplyr::filter(name_muni == "Morretes") |>
  sf::st_transform(terra::crs(dem))

dem_crop <- terra::crop(dem, terra::vect(morretes))
dem_mask <- terra::mask(dem_crop, terra::vect(morretes))

par(mfrow = c(1, 2), mar = c(2, 2, 2, 4))
terra::plot(dem_crop, main = "crop")
terra::plot(dem_mask, main = "crop + mask")
par(mfrow = c(1, 1))


# reprojetar: note o method - "bilinear" para contínuo, "near" para categórico
terra::res(dem)

dem_utm <- terra::project(dem, "EPSG:31982", method = "bilinear")
terra::res(dem_utm)     # agora em metros
#> [1] 70.08786 70.08786


# agregar: reduz a resolução (útil para acelerar análises exploratórias)
dem_500 <- aggregate(dem_utm, fact = 10, fun = "mean")
c(original_m = terra::res(dem_utm)[1], agregado_m = terra::res(dem_500)[1])
#> original_m agregado_m 
#>   70.08786  350.43928

par(mfrow = c(1, 2), mar = c(2, 2, 2, 4))
terra::plot(dem_utm, main = "Original")
terra::plot(dem_500, main = "Aggregate (fact 5)")
par(mfrow = c(1, 1))


r <- raster::raster(nrow=3, ncol=3)
values(r) <- 1:ncell(r)
s <- raster::raster(nrow=10, ncol=10)
s <- raster::resample(r, s, method='bilinear')
par(mfrow=c(1,2))
plot(r)
plot(s)


# Disagragacao

dem_2x <- terra::disagg(dem_utm, fact = 2) 

# numero de celulas
ncell(dem_utm)
ncell(dem_2x)
par(mfrow = c(1, 2), mar = c(2, 2, 2, 4))
terra::plot(dem_utm, main = "Original")
terra::plot(dem_2x, main = "Dissaggregate (fact 2)")
par(mfrow = c(1, 1))

# operações aritméticas e lógicas são vetorizadas, célula a célula
acima_800 <- dem_utm > 800
terra::freq(acima_800)

# reclassificar em faixas de altitude
faixas <- matrix(c(
  -Inf,  50, 1,    # planície litorânea
  50, 300, 2,    # baixa encosta
  300, 800, 3,    # encosta
  800, Inf, 4     # planalto
), ncol = 3, byrow = TRUE)

dem_classes <- terra::classify(dem_utm, faixas)
dem_classes

terra::plot(dem_classes, main = "Original")


terra::freq(dem_classes) |> 
  dplyr::mutate(pct = round(100 * count / sum(count), 1))

# aplica função por célula entre camadas
help(app)
dem_app <- terra::app(dem_utm, fun = mean)       

# if-else raster
dem_ifel <- terra::ifel(dem_utm > 800, 1, 0)    

# janela móvel (suavização, detecção de borda)
dem_focal <- terra::focal(dem_utm, w = c(1,3,1), fun = "mean")    

par(mfrow = c(1, 4), mar = c(2, 2, 2, 4))
terra::plot(dem_utm, main = "Original")
terra::plot(dem_app, main = "função por célula entre camadas")
terra::plot(dem_ifel, main = "if-else raster")
terra::plot(dem_focal, main = "janela móvel")
par(mfrow = c(1, 1))

par(mfrow = c(1, 2), mar = c(2, 2, 2, 4))
terra::plot(dem_utm, main = "Original")
terra::plot(dem_focal, main = "janela móvel")
par(mfrow = c(1, 1))

# 18.4 Declividade------

terreno <- terra::terrain(dem_utm, v = c("slope", "aspect"), unit = "degrees")
terreno



par(mfrow = c(1, 2), mar = c(2, 2, 2, 4))
terra::plot(dem_utm, main = "Original")
terra::plot(terreno$slope, main = "Declividade (graus)")
par(mfrow = c(1, 1))


terra::global(terreno$slope, 
              c("min","mean", "max"), na.rm = TRUE) |>
  round(1)

# 18.7 Extrair valores por geometria


# por ponto: a altitude da sede de cada município, vinda do geobr
# (em vez de digitar longitude/latitude na mão)
arquivo_sedes <- "dados/brutos/sedes_2022.gpkg"
if (!file.exists(arquivo_sedes)) {
  sf::st_write(geobr::read_municipal_seat(year = 2022, showProgress = FALSE),
               arquivo_sedes, quiet = TRUE)
}

sedes <- sf::st_read(arquivo_sedes, quiet = TRUE) |>
  dplyr::filter(name_muni %in% c("Curitiba", "Morretes", "Paranaguá")) |>
  sf::st_transform(terra::crs(dem_utm))

terra::extract(dem_utm, terra::vect(sedes), ID = FALSE) |>
  dplyr::bind_cols(local = sedes$name_muni) |>
  dplyr::rename(altitude_m = 1) |>
  dplyr::mutate(altitude_m = round(altitude_m)) |>
  knitr::kable()

terra::vect(sedes)

# por polígono: estatística zonal, altitude média e máxima por município
muni_serra <- muni |>
  dplyr::filter(name_muni %in% c("Morretes", "Antonina", "Paranaguá", "Piraquara", "Curitiba")) |>
  sf::st_transform(terra::crs(dem_utm))
muni_serra

zonal_alt <- terra::extract(dem_utm, terra::vect(muni_serra),
                            fun = mean, na.rm = TRUE, ID = FALSE)
zonal_alt

muni_serra |>
  sf::st_drop_geometry() |>
  dplyr::transmute(municipio = name_muni,
                   altitude_media_m = round(zonal_alt[[1]])) |>
  dplyr::arrange(dplyr::desc(altitude_media_m)) |>
  knitr::kable()


terra::rasterize(v, r, "id")
help(rasterize)

# extracao de poligonos

poligonos <- terra::as.polygons(dem_classes) |> sf::st_as_sf()
par(mfrow = c(1, 2), mar = c(2, 2, 2, 4))
terra::plot(dem_classes, main = "Original")
terra::plot(poligonos, main = "Poligonos")
par(mfrow = c(1, 1))


# contornos

contornos <- terra::as.contour(dem_utm, 
                               levels = seq(0, 1600, by = 200))
contornos <- sf::st_as_sf(contornos)

terra::plot(contornos, main = "Original")

mapview::mapview(contornos, zcol = "level"
                 , layer.name = "Altitude (m)",
                 map.types = c("Esri.WorldImagery"
                               , "OpenStreetMap"))

as.data.frame(dem_utm, xy = TRUE)



library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Matrix)

# tipos

od_longo <- data.frame(
  origem    = c("AM", "AM", "PA", "SP", "PR"),
  destino   = c("SP", "PR", "MA", "AM", "AM"),
  toneladas = c(1200, 450, 980, 310, 120)
)

head(od_longo)

od_largo <- od_longo |>
  tidyr::pivot_wider(names_from = destino
                     , values_from = toneladas
                     , values_fill = 0)

head(od_largo)
od_matriz <- as.matrix(od_largo[, -1])
rownames(od_matriz) <- od_largo$origem

od_matriz
#>      SP  PR  MA  AM
#> AM 1200 450   0   0
#> PA    0   0 980   0
#> SP    0   0   0 310
#> PR    0   0   0 120


# margens: emissão e atração
data.frame(
  zona    = rownames(od_matriz),
  emitido = rowSums(od_matriz)
) |>
  dplyr::full_join(
    data.frame(zona = colnames(od_matriz),
               atraido = colSums(od_matriz)),
    by = "zona"
  ) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) tidyr::replace_na(x, 0)),
                saldo = atraido - emitido)





# matriz esparsa

n <- 5000    # zonas

# matriz densa: guarda n² valores
memoria_densa <- n^2 * 8 / 1024^3       # em GB (double = 8 bytes)
round(memoria_densa, 2)
#> [1] 0.19

set.seed(1)
n_pares <- 50000       # só 0,2% dos pares têm fluxo

esparsa <- Matrix::sparseMatrix(
  i = sample(n, n_pares, replace = TRUE),
  j = sample(n, n_pares, replace = TRUE),
  x = round(rlnorm(n_pares, 4, 1), 1),
  dims = c(n, n)
)
esparsa[1:4,1:4]
format(object.size(esparsa), units = "MB")
#> [1] "0.6 Mb"
dim(esparsa)
#> [1] 5000 5000

soma_linhas <- Matrix::rowSums(esparsa)
soma_colunas <- Matrix::colSums(esparsa)

c(total = sum(esparsa),
  pares_com_fluxo = length(esparsa@x),
  densidade = round(length(esparsa@x) / (n * n), 5))
#>           total pares_com_fluxo       densidade 
#>     4505842.800       49947.000           0.002

# voltar ao formato longo, só com os pares que têm fluxo
longo <- summary(esparsa) |>
  as.data.frame() |>
  dplyr::rename(origem = i, destino = j, fluxo = x)


longo[36:50,]
head(longo, 4)


# matriz espacial

zonas <- data.frame(
  zona = c("AM", "PA", "MA", "PR", "SP"),
  lon  = c(-60.02, -48.50, -44.30, -49.27, -46.63),
  lat  = c( -3.10,  -1.46,  -2.53, -25.43, -23.55)
) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4674)

library(mapview)
mapview(zonas)
# função genérica: tabela OD + pontos das zonas -> linhas de desejo
od_para_linhas <- function(od, pontos, col_origem = "origem",
                           col_destino = "destino", col_id = "zona") {
  xy <- sf::st_coordinates(pontos)
  idx <- setNames(seq_len(nrow(pontos)), pontos[[col_id]])
  
  io <- idx[od[[col_origem]]]
  id <- idx[od[[col_destino]]]
  
  geom <- Map(\(a, b) sf::st_linestring(rbind(xy[a, ], xy[b, ])), io, id) |>
    sf::st_sfc(crs = sf::st_crs(pontos))
  
  sf::st_sf(od, geometry = geom)
}

linhas <- od_para_linhas(od_longo, zonas)
linhas

arquivo <- "dados/brutos/uf_br.gpkg"
if (!file.exists(arquivo)) {
  ufs <- geobr::read_state(year = 2020, showProgress = FALSE)
  sf::st_write(ufs, arquivo, quiet = TRUE)
}
ufs <- sf::st_read(arquivo, quiet = TRUE)

ggplot() +
  geom_sf(data = ufs, fill = "grey97", colour = "grey85", linewidth = 0.2) +
  geom_sf(data = linhas |> dplyr::arrange(toneladas),
          aes(linewidth = toneladas, alpha = toneladas), colour = "#1a5b8a") +
  geom_sf(data = zonas, size = 2, colour = "#0d3b5c") +
  scale_linewidth_continuous(range = c(0.3, 3), name = "Toneladas") +
  scale_alpha_continuous(range = c(0.35, 0.9), guide = "none") +
  theme_void(base_size = 10)

regiao <- c(AM = "Norte", PA = "Norte", MA = "Nordeste", PR = "Sul", SP = "Sudeste")

od_longo |>
  dplyr::mutate(reg_o = regiao[origem], reg_d = regiao[destino]) |>
  dplyr::summarise(toneladas = sum(toneladas), .by = c(reg_o, reg_d)) |>
  tidyr::pivot_wider(names_from = reg_d, values_from = toneladas
                     , values_fill = 0)

od_longo |>
  dplyr::mutate(tipo = dplyr::if_else(origem == destino, "interno", "externo")) |>
  dplyr::summarise(toneladas = sum(toneladas), pares = dplyr::n(), .by = tipo)



# distância entre as zonas (em km), pela geometria
d_km <- sf::st_distance(zonas) |> units::set_units(km) |> as.numeric() |>
  matrix(nrow = nrow(zonas), dimnames = list(zonas$zona, zonas$zona))

od_ind <- od_longo |>
  dplyr::mutate(
    dist_km = d_km[cbind(origem, destino)],
    tku     = toneladas * dist_km
  )

od_ind |>
  dplyr::arrange(dplyr::desc(tku)) |>
  dplyr::mutate(dplyr::across(c(dist_km, tku), \(x) round(x, 1))) |>
  knitr::kable()


library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)

crs_br <- 5880   # SIRGAS 2000 / Policônica do Brasil, para o mapa nacional


colunas <- c("ano", "perfil", "sh2",
             "pais_origem", "uf_origem", "cidade_origem", "instalacao_origem",
             "pais_destino", "uf_destino", "cidade_destino", "instalacao_destino",
             "tku")

cabotagem <- readxl::read_excel("dados/cabotagem_tku_2025.xlsx", na = c("", "-"))
head(cabotagem)
names(cabotagem)
stopifnot(ncol(cabotagem) == length(colunas))
names(cabotagem) <- colunas

cabotagem <- cabotagem |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), str_squish)) |>
  dplyr::filter(tku > 0) |>
  dplyr::mutate(
    uf_origem  = stringr::str_to_upper(uf_origem),
    uf_destino = stringr::str_to_upper(uf_destino),
    offshore   = uf_origem == "ZEE/PLAT. CONTINENTAL",
    tku_bi     = tku / 1e9
  )

od <- cabotagem |>
  dplyr::summarise(tku = sum(tku),
                   .by = c(offshore, uf_origem, cidade_origem,
                                           uf_destino, cidade_destino))

c(pares = nrow(od), tku_bi = round(sum(od$tku) / 1e9, 1))
#>  pares tku_bi 
#>  428.0  286.9


arquivo <- "dados/brutos/muni_br.gpkg"
if (!file.exists(arquivo)) {
  br <- geobr::read_municipality(code_muni = "all", year = 2022,
                                 simplified = TRUE, showProgress = FALSE)
  sf::st_write(br, arquivo, quiet = TRUE)
}
muni <- sf::st_read(arquivo, quiet = TRUE)

chave <- function(nome, uf) {
  paste(
    nome |> stringr::str_to_lower() |>
      stringi::stri_trans_general("Latin-ASCII") |> stringr::str_squish(),
    stringr::str_to_upper(uf), sep = "|"
  )
}

centroides <- muni |>
  sf::st_point_on_surface() |>
  dplyr::mutate(k = chave(name_muni, abbrev_state)) |>
  dplyr::select(k, code_muni, name_muni, abbrev_state)

head(muni,2)

head(centroides,2)

nrow(centroides)
#> [1] 5570

# diagnóstico ANTES do join: que municípios da ANTAQ não existem na malha do IBGE?
pontas <- dplyr::bind_rows(
  od |> 
    dplyr::filter(!offshore) |> 
    dplyr::transmute(k = chave(cidade_origem, uf_origem)),
  od |>
    dplyr::transmute(k = chave(cidade_destino, uf_destino))
) |>
  dplyr::distinct()

nao_encontrados <- pontas |> 
  dplyr::anti_join(sf::st_drop_geometry(centroides), by = "k")
nao_encontrados

ponto_zee <- sf::st_sfc(sf::st_point(c(-40.20, -22.40)), crs = 4674)

library(mapview)
mapview(ponto_zee)

origens <- od |>
  dplyr::mutate(k = dplyr::if_else(offshore, "zee"
                                   , chave(cidade_origem, uf_origem)))

coords_zee <- sf::st_sf(
  k = "zee", code_muni = NA_real_,
  name_muni = "ZEE / Plataforma Continental",
  abbrev_state = NA_character_,
  geom = ponto_zee
)

pontos <- dplyr::bind_rows(centroides |> dplyr::rename(geom = geom)
                           , coords_zee)

mapview(pontos)


# montar linhas de desejo

xy <- sf::st_coordinates(pontos)
idx <- setNames(seq_len(nrow(pontos)), pontos$k)

od_geo <- origens |>
  dplyr::mutate(
    k_o = k,
    k_d = chave(cidade_destino, uf_destino),
    i_o = idx[k_o],
    i_d = idx[k_d]
  ) |>
  dplyr::filter(!is.na(i_o), !is.na(i_d))

head(od_geo,2)

c(pares_totais = nrow(origens),
  pares_com_geometria = nrow(od_geo),
  tku_preservado = round(sum(od_geo$tku) / sum(origens$tku), 3))
#>        pares_totais pares_com_geometria      tku_preservado 
#>             428.000             419.000               0.997

linhas <- sf::st_sf(
  od_geo,
  geometry = Map(\(a, b) sf::st_linestring(rbind(xy[a, ], xy[b, ])),
                 od_geo$i_o, od_geo$i_d) |> sf::st_sfc(crs = 4674)
) |>
  dplyr::mutate(tku_bi = tku / 1e9)

head(linhas,2)



# mapas de fluxos 
arquivo_uf <- "dados/brutos/uf_br.gpkg"

if (!file.exists(arquivo_uf)) {
  ufs <- geobr::read_state(year = 2020, showProgress = FALSE)
  sf::st_write(ufs, arquivo_uf, quiet = TRUE)
}
ufs <- sf::st_read(arquivo_uf, quiet = TRUE) |> sf::st_transform(crs_br)


top_linhas <- linhas |>
  dplyr::slice_max(tku_bi, n = 80) |>
  dplyr::arrange(tku_bi) |>
  sf::st_transform(crs_br)

pontos_usados <- pontos |>
  dplyr::filter(k %in% c(top_linhas$k_o, top_linhas$k_d)) |>
  sf::st_transform(crs_br)

ggplot() +
  geom_sf(data = ufs, fill = "grey97", colour = "grey86", linewidth = 0.2) +
  geom_sf(data = top_linhas,
          aes(linewidth = tku_bi, colour = offshore, alpha = tku_bi)) +
  geom_sf(data = pontos_usados, size = 0.9, colour = "grey25") +
  scale_linewidth_continuous(range = c(0.2, 3.2), name = "TKU (bi t·km)",
                             transform = "sqrt") +
  scale_alpha_continuous(range = c(0.4, 0.9), guide = "none") +
  scale_colour_manual(values = c(`FALSE` = "#1a5b8a", `TRUE` = "#b3402a"),
                      labels = c(`FALSE` = "porto a porto", `TRUE` = "offshore (ZEE)"),
                      name = NULL) +
  labs(
    title = "Cabotagem: principais fluxos em TKU, 2025",
    caption = paste("Fonte: ANTAQ, Estatístico Aquaviário (TB6.1).\n",
                    "Linhas retas entre centroides municipais; \norigem offshore representada",
                    "por ponto único na Bacia de Campos.")
  ) +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.position = "bottom")


# Tonelagem

linhas <- linhas |>
  dplyr::mutate(
    dist_reta_km = as.numeric(units::set_units(sf::st_length(geometry), km)),
    # a navegação acompanha a costa: a rota real é maior que a linha reta.
    # 1,25 é um fator ilustrativo; em estudo oficial, use distâncias náuticas.
    dist_rota_km = dist_reta_km * 1.25,
    toneladas_est = tku / dist_rota_km
  )

linhas |>
  dplyr::filter(tku > 0, dist_rota_km >0) |>
  sf::st_drop_geometry() |>
  dplyr::summarise(
    tku_bi = sum(tku) / 1e9,
    toneladas_mi = sum(toneladas_est, na.rm = TRUE) / 1e6,
    distancia_media_km = sum(tku, na.rm = TRUE) / sum(toneladas_est, na.rm = TRUE)
  ) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), \(x) round(x, 1))) |>
  knitr::kable()


linhas |>
  sf::st_drop_geometry() |>
  dplyr::slice_max(tku, n = 10) |>
  dplyr::transmute(
    origem  = dplyr::if_else(offshore, "ZEE/Plataforma", paste0(cidade_origem, " (", uf_origem, ")")),
    destino = paste0(cidade_destino, " (", uf_destino, ")"),
    `TKU (bi)` = round(tku / 1e9, 2),
    `Dist. (km)` = round(dist_rota_km),
    `Toneladas (mi)` = round(toneladas_est / 1e6, 2)
  ) |>
  knitr::kable(align = "llrrr")


corredores <- linhas |>
  sf::st_drop_geometry() |>
  dplyr::mutate(corredor = dplyr::case_when(
    offshore ~ paste("Offshore →", uf_destino),
    TRUE     ~ paste(uf_origem, "→", uf_destino)
  )) |>
  dplyr::summarise(tku_bi = sum(tku) / 1e9,
                   toneladas_mi = sum(toneladas_est, na.rm = TRUE) / 1e6,
                   ligacoes = dplyr::n(),
                   .by = corredor) |>
  dplyr::arrange(dplyr::desc(tku_bi))

corredores |>
  dplyr::slice_head(n = 12) |>
  dplyr::mutate(part = tku_bi / sum(corredores$tku_bi),
                dplyr::across(c(tku_bi, toneladas_mi), \(x) round(x, 2)),
                part = scales::percent(part, accuracy = 0.1)) |>
  knitr::kable(align = "lrrrr")

# exportacao
dir.create("resultados", showWarnings = FALSE)

sf::st_write(sf::st_transform(linhas, 4674),
             "resultados/cabotagem_fluxos.gpkg",
             layer = "fluxos_od", delete_dsn = TRUE)

writexl::write_xlsx(
  list("Corredores" = corredores,
       "Pares OD"   = sf::st_drop_geometry(linhas)),
  "resultados/cabotagem_od_2025.xlsx"
)

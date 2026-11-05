

arq_mun <- "dados/brutos/comexstat/tab_UF_MUN.csv"
arq_exp <- "dados/brutos/comexstat/EXP_2024_MUN.csv"
ingenuo <- readr::read_delim(arq_exp, delim = ";", show_col_types = FALSE,
                             locale = readr::locale(encoding = "latin1"))
dim(ingenuo)

ingenuo |> dplyr::select(CO_ANO, CO_MES, SH4, CO_PAIS, SG_UF_MUN, CO_MUN) |>
  head(3)

c(
  cafe_original = "0901",
  cafe_lido     = as.character(unique(ingenuo$SH4[ingenuo$SH4 == 0901])[1])
)
#> cafe_original     cafe_lido 
#>        "0901"            NA
#>        

con <- duckdb::dbConnect(duckdb::duckdb())

exp_mun <- DBI::dbGetQuery(con, sprintf("
  SELECT CO_ANO, CO_MES, SH4, CO_PAIS, SG_UF_MUN, CO_MUN,
         CAST(KG_LIQUIDO AS DOUBLE) AS kg,
         CAST(VL_FOB     AS DOUBLE) AS fob_usd
  FROM read_csv('%s', delim=';', header=true, all_varchar=true, encoding='latin-1')
", normalizePath(arq_exp, winslash = "/")))

head(exp_mun,2) |> str()

mun_tab <- DBI::dbGetQuery(con, sprintf("
  SELECT * FROM read_csv('%s', delim=';', header=true, all_varchar=true, encoding='latin-1')
", normalizePath(arq_mun, winslash = "/")))

head(mun_tab,2) |> str()

# ancora

ancora <- exp_mun |>
  dplyr::summarise(
    linhas      = dplyr::n(),
    municipios  = dplyr::n_distinct(CO_MUN),
    fob_bilhoes = sum(fob_usd) / 1e9
  )
ancora |> knitr::kable(digits = 1)


duckdb::dbDisconnect(con, shutdown = TRUE)

dim(exp_mun)
#> [1] 1144828       8
#> 


# passo 4 - o join que functiona -----

arquivo <- "dados/brutos/muni_br.gpkg"
if (!file.exists(arquivo)) {
  br <- geobr::read_municipality(code_muni = "all", year = 2022,
                                 simplified = TRUE, showProgress = FALSE)
  sf::st_write(br, arquivo, quiet = TRUE)
}
muni <- sf::st_read(arquivo, quiet = TRUE) |>
  dplyr::mutate(code_muni = as.character(code_muni))

head(muni,2)

por_municipio <- exp_mun |>
  dplyr::summarise(fob_usd = sum(fob_usd), .by = CO_MUN)

head(por_municipio,2)

ingenuo_join <- por_municipio |>
  dplyr::left_join(sf::st_drop_geometry(muni), by = c("CO_MUN" = "code_muni"))

head(ingenuo_join,2)
dim(ingenuo_join[is.na(ingenuo_join$name_muni),])
dim(ingenuo_join[!is.na(ingenuo_join$name_muni),])


nao_casaram <- ingenuo_join |>
  dplyr::filter(is.na(name_muni)) |>
  dplyr::left_join(dplyr::select(mun_tab, CO_MUN_GEO, NO_MUN, SG_UF),
                   by = c("CO_MUN" = "CO_MUN_GEO"))

head(nao_casaram)


nao_casaram |>
  dplyr::summarise(municipios = dplyr::n(),
                   fob_bilhoes = sum(fob_usd) / 1e9,
                   .by = SG_UF) |>
  dplyr::arrange(dplyr::desc(fob_bilhoes)) |>
  knitr::kable(digits = 1)

nao_casaram |>
  dplyr::slice_max(fob_usd, n = 5) |>
  dplyr::transmute(CO_MUN, NO_MUN, SG_UF, fob_bilhoes = round(fob_usd / 1e9, 2)) |>
  knitr::kable()

prefixo_comex_para_ibge <- c("34" = "35", "52" = "50", "53" = "52", "54" = "53")

co_mun_comex_para_ibge <- function(co_mun) {
  co_mun  <- as.character(co_mun)
  prefixo <- substr(co_mun, 1, 2)
  sufixo  <- substr(co_mun, 3, 7)
  novo    <- prefixo_comex_para_ibge[prefixo]
  prefixo <- ifelse(is.na(novo), prefixo, novo)
  out <- paste0(prefixo, sufixo)
  out[substr(co_mun, 1, 2) %in% c("93", "99")] <- NA_character_
  out
}

stopifnot(
  co_mun_comex_para_ibge("3106705") == "3106705",  # Betim/MG, não muda
  co_mun_comex_para_ibge("3425904") == "3525904"   # Jundiaí/SP, muda
)
co_mun_comex_para_ibge("3106705")
co_mun_comex_para_ibge("3425904") 
co_mun_comex_para_ibge("3450308") 



head(por_municipio,2)
corrigido <- por_municipio |>
  dplyr::mutate(code_muni = co_mun_comex_para_ibge(CO_MUN)) |>
  dplyr::left_join(sf::st_drop_geometry(muni), by = "code_muni")

head(ingenuo_join)
head(corrigido)

c(
  linhas_antes  = nrow(por_municipio),
  linhas_depois = nrow(corrigido),
  fob_antes_bi  = round(sum(por_municipio$fob_usd) / 1e9, 3),
  fob_depois_bi = round(sum(corrigido$fob_usd) / 1e9, 3)
)
#>  linhas_antes linhas_depois  fob_antes_bi fob_depois_bi 
#>      2500.000      2500.000       337.046       337.046
#>      
#>      

head(muni)

mapa <- muni |>
  dplyr::left_join(
    corrigido |> dplyr::select(code_muni, fob_usd) |> dplyr::filter(!is.na(code_muni)),
    by = "code_muni"
  ) |>
  dplyr::mutate(fob_bi = fob_usd / 1e9)


mapa


ggplot2::ggplot(mapa) +
  ggplot2::geom_sf(ggplot2::aes(fill = fob_bi), colour = NA) +
  ggplot2::scale_fill_viridis_c(trans = "log10", na.value = "grey92",
                                labels = scales::label_number(accuracy = 0.01)) +
  ggplot2::labs(fill = "US$ bilhões (log)",
                title = "Exportações por município, 2024",
                subtitle = "Domicílio da empresa exportadora, não origem da produção") +
  ggplot2::theme_void(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")


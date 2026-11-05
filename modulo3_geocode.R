library(dplyr)
library(stringr)
library(sf)

# mapa-base padrão dos mapview(): OpenStreetMap
mapview::mapviewOptions(basemaps = c("OpenStreetMap",
                                     "Esri.WorldGrayCanvas", "Esri.WorldImagery"))

enderecos <- data.frame(
  id         = 1:4,
  logradouro = c("Praça Cívica", "Avenida Goiás", "Avenida Afonso Pena", "Avenida Paulista"),
  numero     = c(1, 400, 1212, 1578),
  cep        = c("74003-010", "74005-010", "30130-003", "01310-200"),
  bairro     = c("Setor Central", "Setor Central", "Centro", "Bela Vista"),
  municipio  = c("Goiânia", "Goiânia", "Belo Horizonte", "São Paulo"),
  estado     = c("GO", "GO", "MG", "SP")
)

campos <- geocodebr::definir_campos(
  estado     = "estado",
  municipio  = "municipio",
  logradouro = "logradouro",
  numero     = "numero",
  cep        = "cep",
  localidade = "bairro"       # no geocodebr, "localidade" é o bairro
)

resultado <- geocodebr::geocode(
  enderecos       = enderecos,
  campos_endereco = campos,
  verboso         = FALSE
)
#> duckdb is storing downloaded extensions and secrets under ~/.duckdb:
#> ℹ C:\Users\joaob/.duckdb
#> This persists across sessions and is shared with the DuckDB CLI and other clients.
#> ℹ Run duckdb(shared_home = FALSE) to use a temporary directory instead.
#> ℹ See ?duckdb_storage for details and alternatives.


sujos <- c(
  "R. XV de Novembro, 300 - Centro",
  "AV. BRASIL 1200 APTO 51",
  "Rod. BR-116, km 25, s/n",
  "Rua Sete de Setembro nº 45, fundos"
)

data.frame(original = sujos
           , padronizado = enderecobr::padronizar_logradouros(sujos)) |>
  knitr::kable()


head(resultado,2)
resultado |>
  dplyr::select(id, logradouro, numero, municipio, lat, lon, precisao, desvio_metros) |>
  knitr::kable()


enderecobr::padronizar_ceps(c("74003010", "1310200"))
#> [1] "74003-010" "01310-200"
enderecobr::padronizar_numeros(c("0045", "s/n", "SN"))
#> [1] "45"  "S/N" "S/N"
enderecobr::padronizar_estados(c("go", "Sao Paulo", "35"), formato = "sigla")
#> [1] "GO" "SP" "SP"
enderecobr::padronizar_municipios(c("goiania", "SAO PAULO", "3550308"))
#> [1] "GOIANIA"   "SAO PAULO" "SAO PAULO"
enderecobr::padronizar_bairros(c("setor central", "JD. AMERICA"))
#> [1] "SETOR CENTRAL"  "JARDIM AMERICA"
#> 
#> 


head(resultado,2)


separar_numero <- function(x) {
  # logradouro, depois vírgula/espaço (e "NO" opcional), depois número ou S/N, depois o resto
  partes <- stringr::str_match(x, "^(.*?)[, ]+(?:NO )?(\\d{1,6}|S/N)\\b[ ,-]*(.*)$")
  data.frame(
    logradouro  = dplyr::coalesce(partes[, 2], x),
    numero      = partes[, 3],
    complemento = dplyr::na_if(partes[, 4], "")
  )
}

sujos
sujos |>
  enderecobr::padronizar_logradouros() |>
  separar_numero() |>
  knitr::kable()

pontos <- resultado |>
  dplyr::filter(!is.na(lon), !is.na(lat)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4674)

# 1. quantos não foram geocodificados?
c(total = nrow(resultado), geocodificados = nrow(pontos),
  taxa = round(nrow(pontos) / nrow(resultado), 3))
#>          total geocodificados           taxa 
#>              4              4              1


mapview::mapview(pontos)

resultado

# 2. caíram no município declarado?
arquivo <- "dados/brutos/muni_br.gpkg"
if (!file.exists(arquivo)) {
  br <- geobr::read_municipality(code_muni = "all", year = 2022,
                                 simplified = TRUE, showProgress = FALSE)
  sf::st_write(br, arquivo, quiet = TRUE)
}
muni <- sf::st_read(arquivo, quiet = TRUE)

conferencia <- pontos |>
  sf::st_join(muni |> dplyr::select(code_muni, name_muni)) |>
  dplyr::mutate(bate = stringr::str_to_upper(name_muni) == stringr::str_to_upper(municipio))

dplyr::count(sf::st_drop_geometry(conferencia), bate)


# 25.4 Geocode reverso

coords <- data.frame(id = 1:2,
                     lon = c(-49.2733, -46.6333),
                     lat = c(-25.4284, -23.5505)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4674)

library(mapview)
mapview(coords)

geocodebr::geocode_reverso(coords, verboso = FALSE) |>
  sf::st_drop_geometry() |>
  knitr::kable()


# 26 OSM

arquivo <- "dados/brutos/portos_osm.gpkg"

if (!file.exists(arquivo)) {
  res <- osmdata::opq(bbox = c(-48.75, -25.65, -48.35, -25.35), timeout = 240) |>
    osmdata::add_osm_feature(key = "seamark:type", value = "harbour") |>
    osmdata::osmdata_sf()
  
  res$osm_points |>
    filter(!is.na(`seamark:type`)) |>     # descarta os nós que só desenham outras feições
    select(any_of(c("osm_id", "name", "seamark:harbour:category"))) |>
    st_write(arquivo, quiet = TRUE)
}

portos_osm <- st_read(arquivo, quiet = TRUE)
nrow(portos_osm)
mapview(portos_osm)
#> [1] 12


arquivo <- "dados/brutos/od_sintetica.csv"

if (!file.exists(arquivo)) {
  set.seed(42)
  n <- 2e6
  od <- data.frame(
    origem     = sample(1:2000, n, replace = TRUE),
    destino    = sample(1:2000, n, replace = TRUE),
    modo       = sample(c("rodoviario", "ferroviario", "cabotagem"), n,
                        replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    distancia_km = round(runif(n, 5, 2500), 1),
    toneladas    = round(rlnorm(n, meanlog = 3, sdlog = 1.2), 2)
  )
  data.table::fwrite(od, arquivo)
}

dim(od)
round(file.size(arquivo) / 1024^2, 1)   # tamanho em MB
#> [1] 63
#> 

head(od)

# duckDB
# 1. DuckDB: a consulta roda no banco, só o resultado (3 linhas) volta para o R
arquivo
unique(od$modo)

tempo_duckdb <- system.time({
  con <- duckdb::dbConnect(duckdb::duckdb())
  resultado_duckdb <- DBI::dbGetQuery(con, sprintf("
    SELECT modo,
           COUNT(*)           AS pares,
           SUM(distancia_km * toneladas)   AS tku
    FROM read_csv_auto('%s')
    GROUP BY modo
    ORDER BY tku DESC
  ", arquivo))
  duckdb::dbDisconnect(con, shutdown = TRUE)
})

# 2) Data.table()

# 2. data.table: rápido, mas carrega a base inteira na memória
tempo_dt <- system.time({
  dt <- data.table::fread(arquivo)
  resultado_dt <- dt[, .(pares = .N,
                         tku = sum(distancia_km * toneladas)), by = modo]
})
tempo_dt

tempo_duckdb
resultado_duckdb

# 3. readr + dplyr: o mais legível, o mais lento e o que mais consome memória
tempo_dplyr <- system.time({
  resultado_dplyr <- readr::read_csv(arquivo, show_col_types = FALSE, progress = FALSE) |>
    dplyr::summarise(pares = dplyr::n(), tku = sum(distancia_km * toneladas), .by = modo)
})

data.frame(
  abordagem = c("DuckDB (SQL, sem carregar)", "data.table::fread", "readr + dplyr"),
  segundos  = round(c(tempo_duckdb[["elapsed"]],
                      tempo_dt[["elapsed"]],
                      tempo_dplyr[["elapsed"]]), 2)
) |>
  dplyr::arrange(segundos) |>
  knitr::kable(caption = "Tempo para agregar 2 milhões de linhas")


library(duckplyr)

od_lazy <- duckplyr::read_csv_duckdb(arquivo)     # nada é lido ainda

resultado <- od_lazy |>
  dplyr::filter(modo == "rodoviario", distancia_km > 500) |>
  dplyr::summarise(tku = sum(distancia_km * toneladas), .by = origem) |>
  dplyr::arrange(dplyr::desc(tku)) |>
  dplyr::collect()                              # aqui a consulta é executada
resultado


# duckdplyr quando a base de dados ja foi criada

con <- duckdb::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, sprintf("CREATE VIEW od AS SELECT * FROM read_csv_auto('%s')", arquivo))
#> [1] 0

dplyr::tbl(con, "od") |>
  dplyr::filter(modo == "cabotagem") |>
  dplyr::group_by(origem) |>
  dplyr::summarise(tku = sum(distancia_km * toneladas), pares = dplyr::n()) |>
  dplyr::arrange(dplyr::desc(tku)) |>
  head(5) |>
  dplyr::collect()


dplyr::tbl(con, "od") |>
  dplyr::filter(modo == "cabotagem") |>
  dplyr::group_by(origem) |>
  dplyr::summarise(tku = sum(distancia_km * toneladas)) |>
  dplyr::arrange(dplyr::desc(tku)) |>
  head(5)  |>
  dplyr::collect()
  dplyr::show_query()

DBI::dbGetQuery(con,
                "
SELECT origem, SUM(distancia_km * toneladas) AS tku
FROM od
WHERE (modo = 'cabotagem')
GROUP BY origem
ORDER BY tku DESC
LIMIT 5
                ")


arquivo_parquet <- "dados/brutos/od_sintetica.parquet"

con <- duckdb::dbConnect(duckdb::duckdb())
if (!file.exists(arquivo_parquet)) {
  DBI::dbExecute(con, sprintf(
    "COPY (SELECT * FROM read_csv_auto('%s')) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
    arquivo, arquivo_parquet))
}

tamanhos <- c(csv_mb = file.size(arquivo), parquet_mb = file.size(arquivo_parquet)) / 1024^2
round(tamanhos, 1)
#>     csv_mb parquet_mb 
#>       63.0       16.4


tku <- 1250
uf <- "PR"
tku
uf

class(tku)
class(uf)

ufs <- c("AM", "PR","SP", "BA")
tkus <- c(4213,8333,1600,1560)

class(tkus)
class(ufs)
length(ufs)
ufs[3]
tkus[3]

tkus > 2000
ufs[tkus > 2000]

x <- c(1.34,2.1,3.6,"PR")
class(x)

x <- c(1.34,2.1,3.6,NA)
x
class(x)

mean(x)
help(mean)

mean(x,na.rm = TRUE)

od <- matrix(
  c(  0, 120, 340,
      95,   0, 210,
      310, 180,   0),
  nrow = 3, byrow = TRUE,
  dimnames = list(origem = c("AM", "PR", "SP"),
                  destino = c("AM", "PR", "SP"))
)

od
class(od)

dim(od)
ncol(od)
nrow(od)
od

od["SP","PR"]

# total emitido por origem
rowSums(od)

# total emitido por destino
colSums(od)

# Dados de lista
cenario <- list(
  nome     = "Cabotagem 2025",
  ano      = 2025,
  ufs      = c("AM", "PR", "SP"),
  matriz   = od
)

cenario$ufs
cenario$matriz
cenario$matriz["AM",]

# 4.6 data.frame
# install.packages("readxl")

bruto <- readxl::read_excel("dados/cabotagem_tku_2025.xlsx")

#View(bruto)
fluxos <- data.frame(
  origem  = bruto[["UF de Origem"]],
  destino = bruto[["UF de Destino"]],
  tku     = bruto[["TKU\n(t x km)"]]
)

# um recorte real: uma linha de cada uma destas UFs de origem, mais um
# frete pequeno, para as três faixas de TKU aparecerem no exemplo
alvo <- c("AM", "PR", "SP", "BA", "SC", "RS")
pequeno <- which(fluxos$tku >= 5e5 & fluxos$tku < 1e6)[1]

fluxos <- fluxos[c(match(alvo, fluxos$origem), pequeno), ]
rownames(fluxos) <- NULL

# sumario
summary(fluxos)
nrow(fluxos); ncol(fluxos)
sapply(fluxos, class)


sapply(fluxos[, "tku", drop = FALSE], mean)      # retorna vetor
#>        tku 
#> 1351560732
lapply(c(1e6, 1e9), function(x) sum(fluxos$tku > x))  # retorna lista
#> [[1]]
#> [1] 6
#> 
#> [[2]]
#> [1] 3
vapply(fluxos[, "tku", drop = FALSE], mean, numeric(1))  # com tipo garantido
#>        tku 
#> 1351560732
#> 

fluxos$tku |> 
  mean() |>
  sqrt() 

mean(fluxos$tku)



# datas e texto

# install.packages('lubridate')
# install.packages('stringr')

library(lubridate)
library(stringr)


d <- lubridate::ymd("2026-09-28")

format(d, "%d/%m/%Y")          # "28/09/2026"
#> [1] "28/09/2026"
format(d, "%d de %B de %Y")    # por extenso, no locale do sistema
#> [1] "28 de September de 2026"

gsub(pattern = " Do ",replacement = "do", "Dorama")

# 6  Manipulação de dados

# https://joaobazzo.github.io/curso-r-transportes/m1-04-manipulacao.html
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

bruto <- readxl::read_excel("dados/cabotagem_tku_2025.xlsx")
bruto <- as.data.frame(bruto)
dim(bruto)
names(bruto)
bruto |> janitor::clean_names() |> names()
colunas <- c("ano", "perfil", "sh2",
             "pais_origem", "uf_origem", "cidade_origem", "instalacao_origem",
             "pais_destino", "uf_destino", "cidade_destino", "instalacao_destino",
             "tku")

base <- bruto
names(base) <- colunas
head(base,1)


dplyr::glimpse(base)


base |> dplyr::count(tem_fluxo = tku > 0)
# tem_fluxo   n
# 1     FALSE 132
# 2      TRUE 600

# base <- bruto |>
#   dplyr::rename(
#     ano           = Ano,
#     perfil        = `Perfil da Carga`,
#     sh2           = `Grupo de Mercadoria (SH2)`,
#     uf_origem     = `UF de Origem`,
#     cidade_origem = `Cidade de Origem`,
#     tku           = `TKU\n(t x km)`
#   )
head(base,2)


# escrever

readxl::read_excel("arquivo.xlsx", sheet = "Dados")        # aba pelo nome

readxl::read_excel("arquivo.xlsx", skip = 3)               # pula linhas de cabeçalho decorativo
readxl::read_excel("arquivo.xlsx", n_max = 1000)           # só as primeiras linhas (para inspeção)
readxl::read_excel("arquivo.xlsx", col_types = "text")     # lê tudo como texto (evita coerção)
readxl::read_excel("arquivo.xlsx", na = c("", "-", "n/d")) # o que deve virar NA


# 

csv <- readr::read_csv("dados/cabotagem_tku_2025.csv", show_col_types = FALSE)
dim(csv)
#> [1] 732  12
names(csv) |> head(4)
#> [1] "Ano"                       "Perfil da Carga"          
#> [3] "Grupo de Mercadoria (SH2)" "País de Origem"

base |>
  dplyr::select(uf_origem, uf_destino, tku) |>
  head(4)


base |>
  dplyr::filter(uf_destino == "PR") |>
  head(3)

base |>
  dplyr::select(uf_origem, uf_destino, tku) |>
  dplyr::filter(uf_destino == "PR") |>
  head(3)

base |>
  dplyr::filter(uf_origem == "AM", tku > 1e9) |>          # a vírgula é "e"
  dplyr::select(cidade_origem, uf_destino, tku)

base |>
  dplyr::filter(uf_destino %in% c("PR", "SC", "RS")) |>   # Região Sul
  nrow()

base |>
  dplyr::filter(stringr::str_detect(perfil, "Granel")) |>
  dplyr::count(perfil)


base |>
  dplyr::filter(uf_origem != uf_destino) |>   
  nrow()

# 6.2.3 mutate(): criar e alterar colunas

base <- base |>
  dplyr::mutate(
    tku_bi   = tku / 1e9,
    ligacao  = stringr::str_c(uf_origem, " > ", uf_destino),
    interestadual = uf_origem != uf_destino,
    faixa = dplyr::case_when(
      tku >= 1e9 ~ "alto",
      tku >= 1e6 ~ "médio",
      .default   = "baixo"
    )
  )

head(base,3)

# 6.2.4 arrange(): ordenar

base |>
  dplyr::arrange(dplyr::desc(tku)) |>
  dplyr::select(cidade_origem, cidade_destino, ligacao, tku_bi) |>
  head(5)

# 6.2.5 summarise() e agrupamento

base |>
  dplyr::summarise(
    ligacoes = dplyr::n(),
    tku_total = sum(tku, na.rm = TRUE),
    tku_medio = mean(tku, na.rm = TRUE),
    tku_max   = max(tku, na.rm = TRUE)
  )


por_uf_destino <- base |>
  dplyr::summarise(
    ligacoes  = dplyr::n(),
    tku_bi    = sum(tku, na.rm = TRUE) / 1e9,
    .by = uf_destino
  ) |>
  dplyr::arrange(dplyr::desc(tku_bi))

head(por_uf_destino,3)
dim(por_uf_destino)
por_uf_destino |> head(10)

# contribuicao percentual 

base |>
  dplyr::summarise(tku = sum(tku, na.rm = TRUE), .by = c(perfil, uf_destino)) |>
  dplyr::mutate(part = tku / sum(tku), .by = perfil) |>
  dplyr::filter(part > 0.10) |>
  dplyr::arrange(perfil, dplyr::desc(part))

# 6.2.6 count(), distinct(), slice_max()

base |> dplyr::count(perfil, sort = TRUE)
base |> dplyr::filter(tku > 0) |> dplyr::count(perfil, sort = TRUE)


base |> dplyr::distinct(uf_origem) |> nrow()

base |> dplyr::slice_max(tku, n = 3)

# 6.3 Reformatar: long ↔︎ wide

od_longo <- base |>
  dplyr::filter(uf_origem != uf_destino) |>
  dplyr::summarise(tku_bi = sum(tku, na.rm = TRUE) / 1e9, .by = c(uf_origem, uf_destino))

nrow(od_longo)
head(od_longo, 4)


od_largo <- od_longo |>
  tidyr::pivot_wider(
    names_from  = uf_destino,
    values_from = tku_bi,
    values_fill = 0
  ) |>
  dplyr::arrange(uf_origem)

m <- as.matrix(od_largo[, -1])
rownames(m) <- od_largo$uf_origem
round(m[1:5, 1:5], 2)
m['AM','SP']

# 6.3.2 Wide to long: voltar para a forma analítica


head(od_largo,3)
od_largo |>
  tidyr::pivot_longer(
    cols = -uf_origem,
    names_to  = "uf_destino",
    values_to = "tku_bi"
  ) |>
  dplyr::filter(tku_bi > 0) |>
  dplyr::arrange(dplyr::desc(tku_bi)) |>
  head(5)


pop <- data.frame(
  municipio = c("Paranaguá", "Santos"),
  pop_2010  = c(140469, 419400),
  pop_2022  = c(156174, 418375)
)
pop

pop |>
  tidyr::pivot_longer(
    cols = tidyr::starts_with("pop_"),
    names_to = "ano",
    names_prefix = "pop_",
    names_transform = list(ano = as.integer),
    values_to = "populacao"
  )

# 6.4 Exportar resultados

dir.create("resultados", showWarnings = FALSE)


# CSV padrão internacional (ponto decimal)
# install.packages("readr")
readr::write_csv(por_uf_destino, "resultados/tku_por_uf_destino.csv")
readr::write_csv2(por_uf_destino, "resultados/tku_por_uf_destino_excel.csv")

writexl::write_xlsx(
  list(
    "TKU por UF" = por_uf_destino,
    "Matriz OD"  = od_largo
  ),
  "resultados/cabotagem_2025.xlsx"
)

valores <- c("1250,75", "n/d", "-", "890,10")
valores
tku_corrigido <- valores |>
  dplyr::na_if("n/d") |>
  dplyr::na_if("-") |>
  stringr::str_replace(",", ".") |>
  as.numeric()|>
  mean(na.rm = TRUE)

# 7.5 Padronização de texto

head(base,2)
base <- base |>
  dplyr::mutate(
    dplyr::across(dplyr::where(is.character), str_squish),
    uf_origem  = stringr::str_to_upper(uf_origem),
    uf_destino = stringr::str_to_upper(uf_destino),
    chave_destino = cidade_destino |>
      stringr::str_to_lower() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      stringr::str_squish()
  )
head(base,2)
base |> dplyr::distinct(cidade_destino, chave_destino) |> head(4)




caso <- base |>
  dplyr::count(instalacao_origem, instalacao_destino, sh2) |>
  dplyr::filter(n > 1) |>
  dplyr::slice(1)
head(caso)


# 1.8 Os tipos de junção

fluxos <- data.frame(
uf_destino = c("PR", "SP", "BA", "AM"),
tku_bi     = c(4.8, 12.3, 3.1, 0.4)
)

pib <- data.frame(
  uf   = c("PR", "SP", "BA", "CE"),
  pib_bi = c(613, 2720, 400, 220)
)

fluxos |> dplyr::left_join(pib, by = c("uf_destino" = "uf"))





# 8.4

regioes <- data.frame(
  uf = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA",
         "PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO"),
  regiao = c("Norte","Nordeste","Norte","Norte","Nordeste","Nordeste","Centro-Oeste",
             "Sudeste","Centro-Oeste","Nordeste","Centro-Oeste","Centro-Oeste","Sudeste",
             "Norte","Nordeste","Sul","Nordeste","Nordeste","Sudeste","Nordeste","Sul",
             "Norte","Norte","Sul","Sudeste","Nordeste","Norte")
)

regioes <- dplyr::bind_rows(
  regioes,
  data.frame(uf = c("ZEE/PLAT. CONTINENTAL", "INDETERMINADO"),
             regiao = c("Offshore (ZEE)", "Indeterminado"))
)

# 1. o que não casa, nas duas direções
cabotagem <- readxl::read_excel("dados/cabotagem_tku_2025.xlsx", na = c("", "-"))
names(cabotagem) <- janitor::make_clean_names(names(cabotagem))

head(cabotagem,2)
cabotagem |>
  dplyr::anti_join(regioes, by = c("uf_de_origem" = "uf")) |>
  dplyr::count(uf_de_origem, wt = tku_t_x_km , name = "tku_t_x_km")




cabotagem_reg <- cabotagem |>
  dplyr::left_join(regioes, by = c("uf_de_origem" = "uf"), relationship = "many-to-one") |>
  dplyr::rename(regiao_origem = regiao) |>
  dplyr::left_join(regioes, by = c("uf_de_destino" = "uf"), relationship = "many-to-one") |>
  dplyr::rename(regiao_destino = regiao)
cabotagem_reg

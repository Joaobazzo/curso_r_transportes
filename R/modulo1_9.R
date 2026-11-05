library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(readxl)

colunas <- c("ano", "perfil", "sh2",
             "pais_origem", "uf_origem", "cidade_origem", "instalacao_origem",
             "pais_destino", "uf_destino", "cidade_destino", "instalacao_destino",
             "tku")

cabotagem <- readxl::read_excel("dados/cabotagem_tku_2025.xlsx", na = c("", "-"))
stopifnot(ncol(cabotagem) == length(colunas))
names(cabotagem) <- colunas

cabotagem <- cabotagem |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), str_squish)) |>
  dplyr::filter(tku > 0) |>
  dplyr::mutate(uf_origem  = stringr::str_to_upper(uf_origem),
                uf_destino = stringr::str_to_upper(uf_destino),
                tku_bi     = tku / 1e9)

por_uf <- cabotagem |>
  dplyr::summarise(tku_bi = sum(tku_bi), .by = uf_destino) |>
  dplyr::arrange(dplyr::desc(tku_bi))

ggplot(por_uf, aes(x = tku_bi, y = reorder(uf_destino, tku_bi))) +
  geom_col()

ggplot(por_uf, aes(x = tku_bi, y = reorder(uf_destino, tku_bi))) +
  geom_col(fill = "#1a5b8a") +
  geom_text(aes(label = scales::number(tku_bi
                                       , accuracy = 0.1
                                       , decimal.mark = ",")),
            hjust = -0.15, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Cabotagem: TKU por UF de destino",
    subtitle = "Bilhões de toneladas-quilômetro úteis, 2025",
    x = "TKU (bi t·km)", y = NULL,
    caption = "Fonte: ANTAQ, Estatístico Aquaviário."
  ) +
  theme_minimal(base_size = 11)

# 9.24
ggplot(cabotagem, aes(x = tku)) +
  geom_histogram(bins = 30, fill = "#1a5b8a", colour = "white") +
  scale_x_log10(labels = scales::label_log()) +
  labs(x = "TKU por ligação (t·km, escala log)", y = "Nº de ligações") +
  theme_minimal(base_size = 11)

g1 <- ggplot(cabotagem, aes(x = tku)) +
  geom_density(fill = "#1a5b8a", alpha = 0.3) +
  scale_x_log10() +
  labs(x = "TKU (log)", y = "Densidade") +
  theme_minimal(base_size = 10)
g1
g2 <- ggplot(cabotagem, aes(x = tku)) +
  stat_ecdf(geom = "step", linewidth = 0.8, colour = "#1a5b8a") +
  scale_x_log10() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "TKU (log)", y = "Ligações acumuladas") +
  theme_minimal(base_size = 10)
g2
patchwork::wrap_plots(g1, g2)


serie <- data.frame(
  ano = 2015:2025,
  tku_bi = c(24.1, 23.4, 25.8, 27.2, 28.9, 27.1, 30.4, 32.2, 33.8, 35.1, 36.4)
)

ggplot(serie, aes(x = ano, y = tku_bi)) +
  geom_line(linewidth = 0.9, colour = "#1a5b8a") +
  geom_point(size = 2, colour = "#1a5b8a") +
  scale_x_continuous(breaks = seq(2015, 2025, 2)) +
  labs(x = NULL, y = "TKU (bi t·km)",title = "Evolucao TKU") +
  theme_minimal(base_size = 11) 


cabotagem |>
  dplyr::summarise(tku_bi = sum(tku_bi), .by = perfil) |>
  dplyr::mutate(perfil = forcats::fct_reorder(perfil, tku_bi)) |>
  ggplot(aes(x = tku_bi, y = perfil)) +
  geom_col(fill = "#1a5b8a") +
  labs(x = "TKU (bi t·km)", y = NULL) +
  theme_minimal(base_size = 11)

cabotagem |>
  dplyr::summarise(tku_bi = sum(tku_bi), .by = c(perfil, uf_destino)) |>
  dplyr::slice_max(tku_bi, n = 5, by = perfil) |>
  ggplot(aes(x = tku_bi, y = reorder(uf_destino, tku_bi))) +
  geom_col(fill = "#1a5b8a") +
  facet_wrap(~perfil, scales = "free") +
  labs(x = "TKU (bi t·km)", y = NULL) +
  theme_minimal(base_size = 10)

maior <- cabotagem |> dplyr::slice_max(tku, n = 1)

ggplot(resumo_uf, aes(x = ligacoes, y = tku_bi)) +
  geom_point(colour = "grey60", size = 2.5) +
  geom_point(data = resumo_uf |> dplyr::filter(uf_destino == maior$uf_destino),
             colour = "#e07b39", size = 4) +
  annotate("text", x = 30, y = max(resumo_uf$tku_bi) * 0.95,
           label = stringr::str_glue("Maior ligação: {maior$cidade_origem} > {maior$cidade_destino}"),
           hjust = 0, size = 3.2) +
  geom_hline(yintercept = mean(resumo_uf$tku_bi), linetype = "dashed", colour = "grey50") +
  labs(x = "Ligações OD", y = "TKU (bi t·km)") +
  theme_minimal(base_size = 11)

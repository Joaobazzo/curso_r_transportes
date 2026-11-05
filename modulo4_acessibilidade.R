library(sf)
library(dplyr)
library(ggplot2)
library(accessibility)


# introducao

arquivo <- "dados/brutos/aop_fortaleza.rds"

if (!file.exists(arquivo)) {
  aop <- aopdata::read_access(
    city = "Fortaleza",
    mode = "public_transport",
    year = 2019,
    peak = TRUE,
    geometry = TRUE,
    showProgress = FALSE
  )
  saveRDS(aop, arquivo)
}

aop <- readRDS(arquivo)

head(aop,2)

help(read_access)


mapa_hex <- function(dados, coluna, titulo, paleta) {
  ggplot(dados) +
    geom_sf(aes(fill = .data[[coluna]]), colour = NA) +
    scale_fill_viridis_c(option = paleta, direction = -1, transform = "sqrt",
                         breaks = scales::breaks_pretty(n = 3),
                         labels = scales::label_number(big.mark = "."), name = NULL) +
    labs(title = titulo) +
    theme_void(base_size = 9) +
    theme(legend.position = "bottom", legend.key.width = unit(1.6, "cm"))
}

patchwork::wrap_plots(
  mapa_hex(aop, "P001", "Onde as pessoas moram", "mako"),
  mapa_hex(aop, "T001", "Onde estão os empregos", "rocket"),
  nrow = 1
)

centros_hex <- sf::st_point_on_surface(sf::st_transform(aop, 31984))   # UTM 24S

# dois pontos de partida: o hexágono com mais empregos (o centro) e,
# entre os 10% mais
# populosos, o mais distante dele (um bairro populoso da periferia)
i_centro <- which.max(aop$T001)
dist_ao_centro <- as.numeric(sf::st_distance(centros_hex,
                                             centros_hex[i_centro, ]))
populosos <- which(aop$P001 >= quantile(aop$P001[aop$P001 > 0], 0.9))
i_periferia <- populosos[which.max(dist_ao_centro[populosos])]

tempo_a_partir_de <- function(i, velocidade_kmh = 18) {
  dist_km <- as.numeric(sf::st_distance(centros_hex, centros_hex[i, ])) / 1000
  dist_km / velocidade_kmh * 60
}

aop_centro <- aop |> dplyr::mutate(origem = "Centro"
                                  , tempo_min = tempo_a_partir_de(i_centro))
aop_periferia <- aop |> dplyr::mutate(origem = "Periferia"
                               , tempo_min = tempo_a_partir_de(i_periferia))
origens <- dplyr::bind_rows(aop_centro,aop_periferia)

empregos_30 <- origens |>
  sf::st_drop_geometry() |>
  dplyr::summarise(empregos = sum(T001[tempo_min <= 30]), .by = origem)

empregos_30


rotulos <- setNames(
  paste0(empregos_30$origem, ": ",
         scales::label_number(big.mark = ".")(empregos_30$empregos),
         " empregos em até 30 min"),
  empregos_30$origem
)

pontos_partida <- dplyr::bind_rows(
  aop[i_centro, ] |> dplyr::mutate(origem = "Centro"),
  aop[i_periferia, ] |> dplyr::mutate(origem = "Periferia")
) |>
  sf::st_point_on_surface()

mapview::mapview(pontos_partida)

ggplot(origens) +
  geom_sf(aes(fill = cut(tempo_min, c(0, 15, 30, 45, 60, Inf),
                         labels = c("até 15", "15 a 30", "30 a 45", "45 a 60", "mais de 60"),
                         include.lowest = TRUE)),
          colour = NA) +
  geom_sf(data = pontos_partida, shape = 21, size = 3
          , fill = "white", stroke = 1.2) +
  facet_wrap(~origem, labeller = labeller(origem = rotulos)) +
  scale_fill_manual(values = c("#1a5b8a", "#5b9bc8",
                               "#e8e8e8", "#f2f2f2", "#fafafa"),
                    name = "minutos") +
  theme_void(base_size = 9) +
  theme(legend.position = "bottom")


t <- 0:90
data.frame(
  minutos = rep(t, 3),
  peso = c(exp(-0.05 * t),
           as.numeric(t <= 30),
           pmax(0, 1 - t / 60)),
  funcao = rep(c("exponencial (0,05)", "cumulativa (30 min)", "linear (60 min)"),
               each = length(t))
) |>
  ggplot(aes(minutos, peso, colour = funcao)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("#1a5b8a", "#b3402a", "#e0a03f"), name = NULL) +
  labs(x = "Tempo de viagem (min)", y = "Peso da oportunidade") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top")

cols_cma <- grep("^CMATT", names(aop), value = TRUE)
cols_cma
#> [1] "CMATT15"  "CMATT30"  "CMATT60"  "CMATT90"  "CMATT120"

# coluna de estrato de renda: o pacote traz quintil e decil (R002 e R003).
# Confira no dicionário qual é qual antes de rotular o gráfico:
# aopdata::aopdata_dictionary(lang = "pt")
col_renda <- dplyr::intersect(c("R002", "R003"), names(aop))[1]
col_renda
#> [1] "R002"

col_60 <- dplyr::intersect(c("CMATT60"), names(aop))[1]

if (!is.na(col_60)) {
  aop_plot <- aop |>
    dplyr::filter(!is.na(.data[[col_60]]), P001 > 0) |>
    dplyr::mutate(acesso = .data[[col_60]])
  
  ggplot(aop_plot) +
    geom_sf(aes(fill = acesso), colour = NA) +
    scale_fill_viridis_c(option = "C", direction = -1,
                         labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                         name = "Empregos\nem 60 min") +
    labs(caption = "Fonte: Ipea, Projeto Acesso a Oportunidades (aopdata), 2019.") +
    theme_void(base_size = 9)
}

set.seed(2026)

hex <- aop |>
  dplyr::filter(P001 > 0) |>
  dplyr::select(id_hex, P001, T001, dplyr::all_of(col_renda)) |>
  dplyr::slice_sample(n = 250)          # amostra, para o exemplo rodar rápido

centros <- sf::st_point_on_surface(sf::st_transform(hex, 31984))   # UTM 24S (Fortaleza)

d <- sf::st_distance(centros) |> units::set_units(km) |> as.numeric()
d <- matrix(d, nrow = nrow(hex))

velocidade_kmh <- 18    # velocidade porta-a-porta típica de transporte público urbano

matriz <- expand.grid(from_id = hex$id_hex, to_id = hex$id_hex,
                      stringsAsFactors = FALSE) |>
  dplyr::mutate(travel_time = round(as.vector(d) / velocidade_kmh * 60, 1)) |>
  dplyr::filter(travel_time <= 90)

head(matriz,5)
uso_solo <- hex |>
  sf::st_drop_geometry() |>
  dplyr::rename(id = id_hex, populacao = P001, empregos = T001)

head(uso_solo, 3)

# 1. oportunidades cumulativas em 30, 60 e 90 minutos
cma <- accessibility::cumulative_cutoff(
  travel_matrix = matriz,
  land_use_data = uso_solo,
  opportunity   = "empregos",
  travel_cost   = "travel_time",
  cutoff        = c(30, 60, 90)
)

head(cma, 4)


# 2. tempo até a oportunidade mais próxima
tmi <- accessibility::cost_to_closest(
  travel_matrix = matriz,
  land_use_data = uso_solo,
  opportunity   = "empregos",
  travel_cost   = "travel_time",
  n             = 10000
)

head(tmi)
summary(tmi[[ncol(tmi)]])   # a última coluna traz o tempo até a oportunidade mais próxima


# 3. medida gravitacional, com decaimento exponencial
grav <- accessibility::gravity(
  travel_matrix = matriz,
  land_use_data = uso_solo,
  opportunity   = "empregos",
  travel_cost   = "travel_time",
  decay_function = accessibility::decay_exponential(decay_value = 0.05)
)

head(grav, 4)


# cenário ilustrativo: a linha nova atende os hexágonos da metade oeste da amostra
x <- sf::st_coordinates(centros)[, "X"]
oeste <- hex$id_hex[x < stats::median(x)]

matriz_atual   <- matriz
matriz_cenario <- matriz |>
  dplyr::mutate(travel_time = 
                  dplyr::if_else(from_id %in% oeste | to_id %in% oeste,
                                             travel_time * 0.60, travel_time))

acesso_atual  <- accessibility::cumulative_cutoff(matriz_atual,  uso_solo, "empregos", "travel_time", 30)
acesso_novo   <- accessibility::cumulative_cutoff(matriz_cenario, uso_solo, "empregos", "travel_time", 30)

head(acesso_atual,2)
head(acesso_novo,2)

comparacao <- acesso_atual |>
  dplyr::rename(acesso_atual = empregos) |>
  dplyr::left_join(dplyr::rename(acesso_novo, acesso_novo = empregos), by = "id") |>
  dplyr::mutate(
    variacao_abs = acesso_novo - acesso_atual,
    # sem acesso no cenário atual, a variação percentual não existe
    variacao_pct = dplyr::if_else(acesso_atual > 0, 100 * (acesso_novo / acesso_atual - 1), NA_real_)
  ) |>
  dplyr::left_join(uso_solo, by = "id")

head(comparacao,2)
# o número que interessa ao gestor: quantas pessoas ganharam acesso, e quem são
comparacao |>
  dplyr::summarise(
    pessoas_beneficiadas = sum(populacao[variacao_abs > 0]),
    ganho_medio_pct = weighted.mean(variacao_pct, populacao, na.rm = TRUE),
    .by = dplyr::all_of(col_renda)
  )



# fim 
# ver site do curso

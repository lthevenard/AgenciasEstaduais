library(tidyverse)
library(lubridate)


# Data --------------------------------------------------------------------

na_to_zero <- function(ref) {
  for (i in seq_along(ref)) {
    col <- names(ref)[[i]]
    ref[[col]] <- ifelse(ref[[col]] == -1, 0, ref[[col]])
  }
  return(ref)
}


df <- read_csv2("input/df_2023-04-12.csv") %>% 
  rename(id = "...1") %>% 
  filter(!duplicated(Nome)) %>%
  janitor::clean_names()

names(df)

df_dimensions <- df %>%
  na_to_zero() %>%
  mutate(
    ## Autonomia e poderes regulatórios
    previsoes_expressas = (autonomia_administrativa + autonomia_financeira + autonomia_patrimonial + autonomia_tecnica) / 4,
    pge = (3 - assessoria_vinculada_pge - assessores_procuradores - algum_procurador) / 3,
    autonomia_soma = previsoes_expressas + pge + taxa_de_regulacao + competencia_normativa + (1 - poder_concedente) + poder_sancionatorio + poder_tac,
    autonomia_perc = autonomia_soma / 7,
    ## Governança e boas práticas
    ouvidoria = (ouvidoria + ouvidor_mandato_fixo) / 2,
    governanca_soma = ouvidoria + programa_de_compliance + participacao_social + analise_de_impacto + agenda_regulatoria,
    governanca_perc = governanca_soma / 5,
    ## Requisitos/impedimentos dos dirigentes
    requisitos_impedimentos_soma = algum_impedimento_previo_setor + alguma_vedacao_setor_publico + vedacao_filiacao_partidaria + experiencia_previa + quarentena,
    requisitos_impedimentos_perc = requisitos_impedimentos_soma  / 5,
    ## Processo de nomeação dos dirigentes
    processo_nomeacao_soma = nomeacao_exclusivamente_pelo_governador + aprovacao_assembleia + sabatina_assembleia + eleicao_interna_presidente + (1 - indicacao_livre_mandatos_provisorios),
    processo_nomeacao_perc = processo_nomeacao_soma / 5,
    ## Garantias funcionais dos dirigentes
    garantias_funcionais_soma = mandato_definido + mandato_nao_coincidente_diretores + mandato_nao_coincidente_governador + preve_casos_exoneracao + preve_procedimento_exoneracao,
    garantias_funcionais_perc = garantias_funcionais_soma / 5
)

df_dimensions_select <- df_dimensions %>%
  select(uf, nome, autonomia_soma, autonomia_perc, governanca_soma, governanca_perc,
         requisitos_impedimentos_soma, requisitos_impedimentos_perc, processo_nomeacao_soma,
         processo_nomeacao_perc, garantias_funcionais_soma, garantias_funcionais_perc)


df_dimensions_select_longer <- df_dimensions_select %>%
  select(nome, autonomia_perc, governanca_perc, requisitos_impedimentos_perc, processo_nomeacao_perc, garantias_funcionais_perc) %>%
  pivot_longer(cols = c("autonomia_perc", "governanca_perc", "requisitos_impedimentos_perc", "processo_nomeacao_perc", "garantias_funcionais_perc")) %>%
  mutate(name = case_when(
    name == "autonomia_perc" ~ "Autonomia e poderes regulatórios",
    name == "governanca_perc" ~ "Governança e boas práticas",
    name == "requisitos_impedimentos_perc" ~ "Requisitos/impedimentos dos dirigentes",
    name == "processo_nomeacao_perc" ~ "Processo de nomeação dos dirigentes",
    name == "garantias_funcionais_perc" ~ "Garantias funcionais dos dirigentes"
  ))

# df_subdimensions <- df_dimensions %>%
#   select(nome, processo_nomeacao, garantias_funcionais, requisitos_impedimentos) %>%
#   pivot_longer(cols = c("processo_nomeacao", "garantias_funcionais", "requisitos_impedimentos")) %>%
#   mutate(name = case_when(
#     name == "processo_nomeacao" ~ "Processo de Nomeação",
#     name == "garantias_funcionais" ~ "Garantias Funcionais",
#     name == "requisitos_impedimentos" ~ "Requisitos e Impedimentos"
#   ))

# Theme -------------------------------------------------------------------

palette_1 <- c(
  "#EA5455", "#002B5B", "#FFB100", "#539165", "#8D7B68"
)

palette_2 <- c(
  "#A4907C", "#C8B6A6", "#F1DEC9"
)

set_theme_fgv_digital <- function () {
  theme_set({
    theme(
      panel.background = element_rect(
        fill = "grey97", 
        color = "transparent"
      ),
      panel.border = element_rect(
        color = "grey90", 
        fill = NA, 
        size = 1
      ),
      axis.ticks = element_line(
        color = "grey90"
      ),
      legend.key = element_rect(
        fill = "grey97", 
        color = "grey90"
      ),
      text = element_text(
        color = "#081734"
      ),
      axis.text = element_text(
        color = "#081734"),
      plot.title = element_text(
        face = "bold", 
        hjust = 0.5, 
        vjust = 1,
        color = "#081734"
      )
    )
  })
}


save_plot <- function(filename, path = "./plots/2023-05-15", height = 6, width = 10, dpi = 300, ...) {
  ggsave(filename=filename, height=height, width=width, path=path, dpi=dpi, ...)
}


# Analysis ----------------------------------------------------------------

theme_set({theme_light()})
subtitle = "FGV Direito-Rio: Regime jurídico das agências reguladoras estaduais (2023)"

## Agregado

df_dimensions_select_longer %>%
  ggplot(aes(x = fct_reorder(nome, value, .fun = sum), y = value, fill = name)) +
  geom_col() +
  labs(
    title = "Indicador agregado das agências estatuais nas 5 dimensões de análise",
    subtitle = subtitle, x = "", y = "Indicador", fill = "Dimensão de análise"
  ) +
  scale_fill_manual(values = palette_1) +
  coord_flip()

save_plot("1_indicadores_agregados.png")

df_dimensions_select_longer %>%
  ggplot(aes(x = fct_reorder(name, value), y = value, fill = name)) +
  geom_boxplot() +
  labs(x = "", y = "Valor dos indicadores", subtitle = subtitle, fill = "Dimensão de análise",
       title = "Dispersão dos indicadores de cada dimensão de análise") +
  scale_fill_manual(values = palette_1) +
  coord_flip()

save_plot("2_comparacao_dimensoes.png")

## Autonomia e poderes regulatórios

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, autonomia_perc), y = autonomia_perc)) +
  geom_col(fill=palette_1[1]) +
  labs(title = "Comparação entre as agências: indicador de autonomia e poderes regulatórios",
       subtitle = subtitle, x = "", y = "Indicador de autonomia e poderes regulatórios") +
  coord_flip()
  
save_plot("3_autonomia.png")

df_dimensions_select %>%
  ggplot(aes(x = autonomia_perc)) +
  geom_histogram(fill=palette_1[1]) +
  labs(x = "Indicador de autonomia e poderes regulatórios", y = "Número de casos",
       title = "Dispersão do indicador de autonomia e poderes regulatórios", subtitle=subtitle) +
  scale_y_continuous(breaks = 0:6, limits = c(0, 6.5)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1))

save_plot("4_autonomia_dispersao.png")

## Garantias funcionais dos dirigentes

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, garantias_funcionais_perc), y = garantias_funcionais_perc)) +
  geom_col(fill=palette_1[2]) +
  labs(title = "Comparação entre as agências: indicador de garantias funcionais dos dirigentes",
       subtitle = subtitle, x = "", y = "Indicador de garantias funcionais dos dirigentes") +
  coord_flip()

save_plot("5_garantias.png")

df_dimensions_select %>%
  ggplot(aes(x = garantias_funcionais_perc)) +
  geom_histogram(fill=palette_1[2]) +
  labs(x = "Indicador de garantias funcionais dos dirigentes", y = "Número de casos",
       title = "Dispersão do indicador de garantias funcionais dos dirigentes", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(0, 14, 2), limits = c(0, 15)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05))

save_plot("6_garantias_dispersao.png")

## Governança e boas práticas

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, governanca_perc), y = governanca_perc)) +
  geom_col(fill=palette_1[3]) +
  labs(title = "Comparação entre as agências: indicador de governança e boas práticas",
       subtitle = subtitle, x = "", y = "Indicador de governança e boas práticas") +
  coord_flip()

save_plot("7_governanca.png")

df_dimensions_select %>%
  ggplot(aes(x = governanca_perc)) +
  geom_histogram(fill=palette_1[3]) +
  labs(x = "Indicador de governança e boas práticas", y = "Número de casos",
       title = "Dispersão do indicador de governança e boas práticas", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

save_plot("8_governanca_dispersao.png")

## Processo de nomeação dos dirigentes

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, processo_nomeacao_perc), y = processo_nomeacao_perc)) +
  geom_col(fill=palette_1[4]) +
  labs(title = "Comparação entre as agências: indicador de processo de nomeação dos dirigentes",
       subtitle = subtitle, x = "", y = "Indicador de processo de nomeação dos dirigentes") +
  coord_flip()

save_plot("9_nomeacao.png")

df_dimensions_select %>%
  ggplot(aes(x = processo_nomeacao_perc)) +
  geom_histogram(fill=palette_1[4]) +
  labs(x = "Indicador de processo de nomeação dos dirigentes", y = "Número de casos",
       title = "Dispersão do indicador de processo de nomeação dos dirigentes", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

save_plot("10_nomeacao_dispersao.png")

## Requisitos/impedimentos dos dirigentes

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, requisitos_impedimentos_perc), y = requisitos_impedimentos_perc)) +
  geom_col(fill=palette_1[5]) +
  labs(title = "Comparação entre as agências: indicador de requisitos/impedimentos dos dirigentes",
       subtitle = subtitle, x = "", y = "Indicador de requisitos/impedimentos dos dirigentes") +
  coord_flip()

save_plot("11_requisitos.png")

df_dimensions_select %>%
  ggplot(aes(x = requisitos_impedimentos_perc)) +
  geom_histogram(fill=palette_1[5]) +
  labs(x = "Indicador de requisitos/impedimentos dos dirigentes", y = "Número de casos",
       title = "Dispersão do indicador de requisitos/impedimentos dos dirigentes", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1))

save_plot("12_requisitos_dispersao.png")


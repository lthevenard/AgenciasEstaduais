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
    ouvidoria = (ouvidoria + ouvidor_mandato_fixo) / 2,
    pge = (3 - assessoria_vinculada_pge - assessores_procuradores - algum_procurador) / 3,
    integridade_soma = ouvidoria + pge + programa_de_compliance + assessoria_juridica,
    integridade_perc = integridade_soma / 4,
    requisitos_impedimentos = (algum_impedimento_previo_setor + alguma_vedacao_setor_publico + vedacao_filiacao_partidaria + experiencia_previa + quarentena) / 5,
    processo_nomeacao = (nomeacao_exclusivamente_pelo_governador + aprovacao_assembleia + sabatina_assembleia + eleicao_interna_presidente - indicacao_livre_mandatos_provisorios) / 4,
    garantias_funcionais = (mandato_definido + mandato_nao_coincidente_diretores + mandato_nao_coincidente_governador + preve_casos_exoneracao + preve_procedimento_exoneracao) / 5,
    dirigentes_soma = requisitos_impedimentos + processo_nomeacao + garantias_funcionais,
    dirigentes_perc = dirigentes_soma / 3,
    previsoes_expressas = (autonomia_administrativa + autonomia_financeira + autonomia_patrimonial + autonomia_tecnica) / 4,
    autonomia_institucional_soma = previsoes_expressas + taxa_de_regulacao + competencia_normativa + poder_concedente,
    autonomia_institucional_perc = autonomia_institucional_soma / 4,
    fiscalizacao_julgamento_soma = poder_tac + poder_sancionatorio + multa,
    fiscalizacao_julgamento_perc = fiscalizacao_julgamento_soma / 3,
    boas_praticas_soma = agenda_regulatoria + analise_de_impacto + participacao_social,
    boas_praticas_perc = boas_praticas_soma / 3
  )

df_dimensions_select <- df_dimensions %>%
  select(uf, nome, boas_praticas_soma, boas_praticas_perc, fiscalizacao_julgamento_soma,
         fiscalizacao_julgamento_perc, autonomia_institucional_soma, autonomia_institucional_perc,
         dirigentes_soma, dirigentes_perc, integridade_soma, integridade_perc)


df_dimensions_select_longer <- df_dimensions_select %>%
  select(nome, boas_praticas_perc, fiscalizacao_julgamento_perc, autonomia_institucional_perc, dirigentes_perc, integridade_perc) %>%
  pivot_longer(cols = c("boas_praticas_perc", "fiscalizacao_julgamento_perc", "autonomia_institucional_perc", "dirigentes_perc", "integridade_perc")) %>%
  mutate(name = case_when(
    name == "boas_praticas_perc" ~ "Boas práticas regulatórias",
    name == "fiscalizacao_julgamento_perc" ~ "Fiscalização e julgamento",
    name == "autonomia_institucional_perc" ~ "Autonomia institucional",
    name == "dirigentes_perc" ~ "Seleção dos dirigentes",
    name == "integridade_perc" ~ "Integridade e compliance"
  ))

df_subdimensions <- df_dimensions %>%
  select(nome, processo_nomeacao, garantias_funcionais, requisitos_impedimentos) %>%
  pivot_longer(cols = c("processo_nomeacao", "garantias_funcionais", "requisitos_impedimentos")) %>%
  mutate(name = case_when(
    name == "processo_nomeacao" ~ "Processo de Nomeação",
    name == "garantias_funcionais" ~ "Garantias Funcionais",
    name == "requisitos_impedimentos" ~ "Requisitos e Impedimentos"
  ))

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


save_plot <- function(filename, path = "./plots/2023-05-03", height = 6, width = 10, dpi = 300, ...) {
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

## Autonomia institucional

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, autonomia_institucional_perc), y = autonomia_institucional_perc)) +
  geom_col(fill=palette_1[1]) +
  labs(title = "Comparação entre as agências estaduais: indicador de autonomia institucional",
       subtitle = subtitle, x = "", y = "Indicador de Autonomia Institucional") +
  coord_flip()
  
save_plot("3_autonomia_institucional_agencias.png")

df_dimensions_select %>%
  ggplot(aes(x = autonomia_institucional_perc)) +
  geom_histogram(fill=palette_1[1]) +
  labs(x = "Indicador de Autonomia Institucional", y = "Número de casos",
       title = "Dispersão do indicador de autonomia institucional", subtitle=subtitle) +
  scale_y_continuous(breaks = 0:10) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

save_plot("4_autonomia_institucional_dispersao.png")

## Boas práticas

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, boas_praticas_perc), y = boas_praticas_perc)) +
  geom_col(fill=palette_1[2]) +
  labs(title = "Comparação entre as agências estaduais: indicador de boas práticas",
       subtitle = subtitle, x = "", y = "Indicador de Boas Práticas Regulatórias") +
  coord_flip()

save_plot("5_boas_praticas_agencias.png")

df_dimensions_select %>%
  ggplot(aes(x = boas_praticas_perc)) +
  geom_histogram(fill=palette_1[2]) +
  labs(x = "Indicador de Boas Práticas Regulatórias", y = "Número de casos",
       title = "Dispersão do indicador de boas práticas", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

save_plot("6_autonomia_institucional_dispersao.png")

## Fiscalização e julgamento

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, fiscalizacao_julgamento_perc), y = fiscalizacao_julgamento_perc)) +
  geom_col(fill=palette_1[3]) +
  labs(title = "Comparação entre as agências estaduais: indicador de fiscalização e julgamento",
       subtitle = subtitle, x = "", y = "Indicador de Fiscalização e Julgamento") +
  coord_flip()

save_plot("7_fiscalizacao_julgamento_agencias.png")

df_dimensions_select %>%
  ggplot(aes(x = fiscalizacao_julgamento_perc)) +
  geom_histogram(fill=palette_1[3]) +
  labs(x = "Indicador de Fiscalização e Julgamento", y = "Número de casos",
       title = "Dispersão do indicador de fiscalização e julgamento", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

save_plot("8_fiscalizacao_julgamento_dispersao.png")

## Integridade e compliance

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, integridade_perc), y = integridade_perc)) +
  geom_col(fill=palette_1[4]) +
  labs(title = "Comparação entre as agências estaduais: indicador de integridade e compliance",
       subtitle = subtitle, x = "", y = "Indicador de Integridade e Compliance") +
  coord_flip()

save_plot("9_integridade_agencias.png")

df_dimensions_select %>%
  ggplot(aes(x = fiscalizacao_julgamento_perc)) +
  geom_histogram(fill=palette_1[4]) +
  labs(x = "Indicador de Integridade e Compliance", y = "Número de casos",
       title = "Dispersão do indicador de integridade e compliance", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

save_plot("10_integridade_dispersao.png")

## Dirigentes

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, dirigentes_perc), y = dirigentes_perc)) +
  geom_col(fill=palette_1[5]) +
  labs(title = "Comparação entre as agências estaduais: indicador de seleção dos dirigentes",
       subtitle = subtitle, x = "", y = "Indicador de Seleção dos Dirigentes") +
  coord_flip()

save_plot("11_dirigentes_agencias.png")

df_dimensions_select %>%
  ggplot(aes(x = dirigentes_perc)) +
  geom_histogram(fill=palette_1[5]) +
  labs(x = "Indicador de Seleção dos Dirigentes", y = "Número de casos",
       title = "Dispersão do indicador de seleção dos dirigentes", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

save_plot("12_dirigentes_dispersao.png")

df_subdimensions %>%
  ggplot(aes(x = fct_reorder(nome, value, .fun = sum), y = value, fill = name)) +
  geom_col() +
  labs(
    title = "Decomposição do indicador de seleção dos dirigentes em subdimensões",
    subtitle = subtitle, x = "", y = "Indicador", fill = "Subdimensão"
  ) +
  scale_fill_manual(values = palette_2) +
  coord_flip()

save_plot("13_dirigentes_subs_comparacao.png")

df_subdimensions %>%
  ggplot(aes(x = fct_reorder(name, value), y = value, fill = name)) +
  geom_boxplot() +
  labs(x = "", y = "Valor dos indicadores", subtitle = subtitle, fill = "Subdimensão",
       title = "Dispersão dos indicadores de cada subdimensão referente aos dirigentes") +
  scale_fill_manual(values = palette_2) +
  coord_flip()

save_plot("14_dirigentes_subs_dispersao.png")
  

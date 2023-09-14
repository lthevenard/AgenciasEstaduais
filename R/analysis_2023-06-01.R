library(tidyverse)
library(lubridate)
library(rvest)


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
    processo_nomeacao_soma = aprovacao_assembleia + sabatina_assembleia + eleicao_interna_presidente + (1 - indicacao_livre_mandatos_provisorios),
    processo_nomeacao_perc = processo_nomeacao_soma / 4,
    ## Garantias funcionais dos dirigentes
    garantias_funcionais_soma = mandato_definido + mandato_nao_coincidente_diretores + mandato_nao_coincidente_governador + preve_casos_exoneracao + preve_procedimento_exoneracao,
    garantias_funcionais_perc = garantias_funcionais_soma / 5,
    ## Dirigentes
    dirigentes_soma = requisitos_impedimentos_perc + processo_nomeacao_perc + garantias_funcionais_perc,
    dirigentes_perc = dirigentes_soma / 3
)

df_dimensions_select <- df_dimensions %>%
  select(uf, nome, autonomia_perc, governanca_perc, dirigentes_perc,
         processo_nomeacao_perc, garantias_funcionais_perc, requisitos_impedimentos_perc)


df_dimensions_select_longer <- df_dimensions_select %>%
  select(uf, nome, autonomia_perc, governanca_perc, dirigentes_perc) %>%
  pivot_longer(cols = c("autonomia_perc", "governanca_perc", "dirigentes_perc")) %>%
  mutate(name = case_when(
    name == "autonomia_perc" ~ "Autonomia e poderes regulatórios",
    name == "governanca_perc" ~ "Governança e boas práticas",
    name == "dirigentes_perc" ~ "Independência e expertise dos dirigentes"
  ))


df_subdimensions <- df_dimensions %>%
  select(nome, processo_nomeacao_perc, garantias_funcionais_perc, requisitos_impedimentos_perc) %>%
  pivot_longer(cols = c("processo_nomeacao_perc", "garantias_funcionais_perc", "requisitos_impedimentos_perc")) %>%
  mutate(name = case_when(
    name == "processo_nomeacao_perc" ~ "Processo de Nomeação",
    name == "garantias_funcionais_perc" ~ "Garantias Funcionais",
    name == "requisitos_impedimentos_perc" ~ "Requisitos e Impedimentos"
  ))

# Theme -------------------------------------------------------------------

palette_1 <- c(
  "#A7C7E7", "#C1E1C1", "#8D7B68"
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


save_plot <- function(filename, path = "./plots/2023-06-01", height = 6, width = 10, dpi = 300, ...) {
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
  mutate(autonomia_cat = round(autonomia_perc, digits=1)) %>%
  group_by(autonomia_cat) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = autonomia_cat, y = total)) +
  geom_col(fill=palette_1[1], width=0.095) +
  labs(x = "Indicador de autonomia e poderes regulatórios", y = "Número de casos",
       title = "Dispersão do indicador de autonomia e poderes regulatórios", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(1, 12, 2), limits = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.1))

save_plot("4_autonomia_dispersao.png")

## Governança e boas práticas

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, governanca_perc), y = governanca_perc)) +
  geom_col(fill=palette_1[2]) +
  labs(title = "Comparação entre as agências: indicador de governança e boas práticas",
       subtitle = subtitle, x = "", y = "Indicador de governança e boas práticas") +
  coord_flip()

save_plot("5_governanca.png")

df_dimensions_select %>%
  mutate(governanca_cat = round(governanca_perc, digits=1)) %>%
  group_by(governanca_cat) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = governanca_cat, y = total)) +
  geom_col(fill=palette_1[2], width=0.095) +
  labs(x = "Indicador de governança e boas práticas", y = "Número de casos",
       title = "Dispersão do indicador de governança e boas práticas", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(0, 14, 2)) +
  scale_x_continuous(breaks = seq(0, 1.1, 0.1))

save_plot("6_governanca_dispersao.png")

## Dirigentes

df_dimensions_select %>%
  ggplot(aes(x = fct_reorder(nome, dirigentes_perc), y = dirigentes_perc)) +
  geom_col(fill=palette_1[3]) +
  labs(title = "Comparação entre as agências: indicador de independência e expertise dos dirigentes",
       subtitle = subtitle, x = "", y = "Indicador de independência e expertise dos dirigentes") +
  coord_flip()

save_plot("7_dirigentes.png")

df_dimensions_select %>%
  mutate(dirigentes_cat = round(dirigentes_perc, digits=1)) %>%
  group_by(dirigentes_cat) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = dirigentes_cat, y = total)) +
  geom_col(fill=palette_1[3], width=0.095) +
  labs(x = "Indicador de independência e expertise dos dirigentes", y = "Número de casos",
       title = "Dispersão do indicador de independência e expertise dos dirigentes", subtitle=subtitle) +
  scale_y_continuous(breaks = seq(0, 7, 2), limits = c(0, 7.5)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05))

save_plot("8_dirigentes_dispersao.png")

## Sub-dimensões

df_subdimensions %>%
  ggplot(aes(x = fct_reorder(nome, value, .fun = sum), y = value, fill = name)) +
  geom_col() +
  labs(
    title = "Decomposição do indicador de autonomia e expertise dos dirigentes em subdimensões",
    subtitle = subtitle, x = "", y = "Indicador", fill = "Subdimensão"
  ) +
  scale_fill_manual(values = palette_2) +
  coord_flip()

save_plot("9_dirigentes_subs_agregadas.png")

df_subdimensions %>%
  ggplot(aes(x = fct_reorder(name, value), y = value, fill = name)) +
  geom_boxplot() +
  labs(x = "", y = "Valor dos indicadores", subtitle = subtitle, fill = "Subdimensão",
       title = "Dispersão das subdimensões de independência e expertise dos dirigentes") +
  scale_fill_manual(values = palette_2) +
  coord_flip()

save_plot("10_comparacao_subs.png")

### REGIÕES



regioes <- read_html("https://www.estadosecapitaisdobrasil.com/") %>%
  html_table() 

regioes <- regioes[[1]] %>%
  select(Sigla, Região) %>%
  rename(uf = "Sigla", regiao = "Região")

df_dimensions_select_longer %>%
  group_by(uf, nome) %>%
  summarise(agregado = sum(value)) %>%
  left_join(regioes, by="uf") %>%
  ungroup() %>%
  group_by(regiao) %>%
  summarise(mean = mean(agregado),
            median = median(agregado),
            min = min(agregado),
            max = max(agregado))


df_dimensions_select_longer %>%
  group_by(uf, nome) %>%
  summarise(agregado = sum(value)) %>%
  left_join(regioes, by="uf") %>%
  ungroup() %>%
  group_by(regiao) %>%
  count()


df_dimensions_select_longer %>%
  group_by(uf, nome) %>%
  summarise(agregado = sum(value)) %>%
  left_join(regioes, by="uf") %>%
  filter(regiao == "Sul")


df_dimensions_select_longer %>%
  group_by(name) %>%
  summarise(mean = mean(value),
            median = median(value),
            min = min(value),
            max = max(value))

df_dimensions_select_longer %>%
  filter(nome == "ARESC")

df_dimensions_select_longer %>%
  filter(name == "Independência e expertise dos dirigentes") %>%
  arrange(value)
  
  
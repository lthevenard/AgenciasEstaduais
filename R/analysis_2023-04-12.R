library(tidyverse)
library(lubridate)
library(corrplot)

# Data --------------------------------------------------------------------

df <- read_csv2("input/df_2023-04-12.csv") %>% 
  rename(id = "...1") %>% 
  filter(!duplicated(Nome))

df %>% glimpse()

cols <- names(df)

cols_numeric <- cols[7:49][str_detect(cols[7:49], "^Tempo")]
cols_boolean <- cols[7:49][!str_detect(cols[7:49], "^Tempo")]

for (i in seq_along(df)) {
  if (i > 6) {
    col <- names(df)[[i]]
    if (col %in% cols_numeric) {
      df[[col]] <- ifelse(df[[col]] == 0, -1, df[[col]])
    }
    df[[col]] <- ifelse(df[[col]] == -1, NA, df[[col]])
  }
}


# Functions ---------------------------------------------------------------

plot_boolean <- function(measurement, col, clean_col, dir = "./plots/categorias_booleanas") {
  ref <- tibble(
    x = measurement
  )
  p <- ggplot(ref, aes(x = x)) +
    geom_bar(fill = "#406cc3", width = 0.7) +
    labs(title = col, x = "Preenchimento", y = "Número de Preenchimentos") +
    theme_bw()
  
  ggsave(
    filename = paste0(clean_col, ".png"),
    plot = p,
    path = dir,
    height = 6,
    width = 6
  )
}

plot_numeric <- function(measurement, col, clean_col, dir = "./plots/categorias_numericas") {
  ref <- tibble(
    x = measurement
  ) %>%
    filter(x > 0)
  
  p <- ggplot(ref, aes(x = x)) +
    geom_density(fill = "#406cc3", color = "#12419e", alpha = 0.9) +
    labs(title = col, x = "Tempo", y = "Densidade de Preenchimentos") +
    theme_bw()
  
  ggsave(
    filename = paste0(clean_col, ".png"),
    plot = p,
    path = dir,
    height = 6,
    width = 6
  )
}

getmode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Plots for every column --------------------------------------------------

clean_cols <- df %>% janitor::clean_names() %>% names()

for (i in seq_along(df)) {
  if (i > 6) {
    col <- names(df)[[i]]
    if (any(df[[col]] > 1, na.rm=TRUE)) {
      plot_numeric(df[[col]], col, clean_cols[[i]])
    } else {
      plot_boolean(df[[col]], col, clean_cols[[i]])
    }
  }
}


# Boolean categories ------------------------------------------------------

df_boolean <- df %>% 
  select(all_of(cols_boolean)) %>%
  pivot_longer(cols = all_of(cols_boolean), names_to = "categoria", values_to = "classificacao")

df_boolean %>%
  filter(!is.na(classificacao)) %>%
  group_by(classificacao) %>%
  summarise(n = n()) %>%
  mutate(Perc = round((n / sum(n))*100, digits = 2) %>% paste0("%")) %>%
  ggplot(aes(x = as_factor(classificacao), y = n, label = Perc)) +
  geom_col(fill = "#406cc3", color = "#12419e", alpha = 0.9, width = 0.7) +
  geom_label(fill = "#12419e", color = "white", nudge_y = -30) +
  labs(title = "Respostas booleanas (todas as categorias)",
       x = "Classificação", y = "Número total de casos") +
  theme_light()

ggsave(
  filename = "_1_resultados_booleanos_gerais.png",
  path = "./plots",
  height = 6,
  width = 5
)

boolean_heterogeneity <- df_boolean %>%
  filter(!is.na(classificacao)) %>%
  count(categoria, classificacao) %>%
  mutate(classificacao = ifelse(classificacao == 1, "Classe1", "Classe0")) %>%
  pivot_wider(names_from = classificacao, values_from = n, values_fill = 0) %>%
  mutate(MaiorClasse = ifelse(Classe0 > Classe1, Classe0, Classe1),
         Perc = MaiorClasse / (Classe0 + Classe1),
         PercLabel = round(Perc*100, digits=2) %>% paste0("%"))

boolean_heterogeneity %>%
  ggplot(aes(x = Perc)) +
  geom_density(fill = "#406cc3", color = "#12419e", alpha = 0.8) +
  labs(title = "Homogeneidade das categorias booleanas",
       subtitle = "",
       x = "Porcentagem da classificação mais frequente (0 ou 1)",
       y = "Densidade de Casos\n(Kernel-Density)") +
  scale_x_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.1)) +
  theme_light()


ggsave(
  filename = "_2_homogeneidade_dos_resultados_distribuicao.png",
  path = "./plots",
  height = 5,
  width = 6.5
)

boolean_heterogeneity %>%
  mutate(case_prevalence = ifelse(Classe0 > Classe1, "Prevalência de 0", "Prevalência de 1")) %>%
  mutate(categoria = fct_reorder(categoria, Perc)) %>%
  ggplot(aes(y = Perc, x = categoria, color = case_prevalence)) +
  geom_segment(aes(x = categoria, xend = categoria, y = 0.5, yend = Perc)) +
  geom_point() +
  labs(title = "Categorias booleanas da pesquisa, por nível\nde homegeneidade e categoria prevalente",
       y = "Porcentagem da classificação mais frequente",
       x = "", color = "Categoria Prevalente: ") +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.1)) +
  scale_color_manual(values = c("red", "purple")) +
  coord_flip() +
  theme_light() +
  theme(legend.position = "bottom")


ggsave(
  filename = "_3_homogeneidade_dos_resultados_frequencias.png",
  path = "./plots",
  height = 7.2,
  width = 7.2
)

boolean_heterogeneity %>%
  arrange(desc(Perc)) %>%
  write_csv2("output/categorias_booleanas.csv")


# Correlations ------------------------------------------------------------

interesting_boolean_cases <- boolean_heterogeneity %>%
  filter(Perc < 0.75 & (Classe0 + Classe1 == 29)) %>%
  .$categoria

boolean_values <- df %>% select(all_of(interesting_boolean_cases))

boolean_names <- tibble(
  simple_values = paste0("c", 1:14),
  categories = names(boolean_values),
  legends = paste(paste0("c", 1:14), names(boolean_values), sep = " - ")
)

names(boolean_values) <- boolean_names$simple_values

cor_booleans <- cor(boolean_values)

corrplot(cor_booleans, method = "shade", type = "lower")


# Agencies ----------------------------------------------------------------

df_agencies_boolean <- df %>% 
  select(all_of(c("Nome", cols_boolean))) %>%
  pivot_longer(cols = all_of(cols_boolean), names_to = "categoria", values_to = "classificacao") %>%
  group_by(Nome) %>%
  summarise(mean_v = mean(classificacao, na.rm = TRUE))

df_agencies_boolean %>%
  ggplot(aes(x = fct_reorder(Nome, mean_v), y = mean_v, label = round(mean_v, digits = 2))) +
  geom_col(fill = "#406cc3", color = "#12419e", alpha = 0.9, width = 0.7) +
  geom_label(fill = "#12419e", color = "white", nudge_y = 0.03, size = 3) +
  labs(title = "Valor booleano médio por agências",
       y = "Valor booleano médio (categorias preenchidas)",
       x = "") +
  coord_flip() +
  theme_light()

ggsave(
  filename = "_5_booleano_por_agencia.png",
  path = "./plots",
  height = 7.2,
  width = 7.2
)


# Numeric columns ---------------------------------------------------------

df_numeric <- df %>%
  select(all_of(cols_numeric))

df_numeric %>%
  pivot_longer(cols = cols_numeric, names_to = "Categoria", values_to = "Preenchimento") %>%
  group_by(Categoria) %>%
  summarise(
    Moda = getmode(Preenchimento),
    Media = mean(Preenchimento, na.rm = TRUE),
    Mediana = median(Preenchimento, na.rm = TRUE)
)



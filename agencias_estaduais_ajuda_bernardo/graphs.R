library(tidyverse)
library(lubridate)
library(readxl)

# Dados ----

df_despesa_long <- read_xlsx("despesas_agencias_2.xlsx") %>%
  janitor::clean_names()

ipca <- read_xlsx("IPCA.xlsx") %>%
  janitor::clean_names()

df_ref <- read_csv2("df_final.csv")

df_ref_ufs <- df_ref %>%
  select(uf, nome) %>%
  arrange(uf)

df_despesa_long <- df_despesa_long %>%
  mutate(unidade_orcamentaria = ifelse(unidade_orcamentaria == "ARSAM", "ARSEPAM", unidade_orcamentaria),
         dimensao = ifelse(dimensao == "Despesa Orçada", "despesa autorizada", str_to_lower(dimensao)),
         valor = as.numeric(valor),
         exercicio = paste0(exercicio, "-01-01") %>% ymd())

df_despesa <- df_despesa_long %>%
  pivot_wider(names_from = "dimensao", values_from = "valor") %>%
  janitor::clean_names()
  

unique_ufs <- unique(df_ref$uf)
unique_ags <- unique(df_ref$nome)

# Funções ----

save_plot <- function(filename, path = "./plots", height = 6, width = 10, dpi = 300, ...) {
  ggsave(filename=filename, height=height, width=width, path=path, dpi=dpi, ...)
}

dollar_to_reais <- function(unit) {
  unit %>%
    str_replace_all(",", ".") %>%
    str_replace_all("\\$", "R$ ")
}

scope_from_vec <- function(vec) {
  max(vec, na.rm = TRUE) - min(vec, na.rm = TRUE)
}

# Gráficos ----

theme_set({{theme_bw()}})

color_scheme <- list(
  fgv_primary = "#112f68",
  fgv_secondary = "#3f8ac7",
  red = "#FF4646",
  orange = "#FFB732",
  green = "#A5D732",
  green2 = "#507e4e",
  green3 = "#014a01"
)

# 1. Comparar autorizada vs. empenhada no estado e na agência ----

## 1a Facet wrap com evolução da agência e da UF

for (i in seq_along(unique_ags)) {
  ag <- unique_ags[[i]]
  uf <- df_ref_ufs %>%
    filter(nome == ag) %>%
    .$uf %>%
    .[[1]]
  
  if (ag == "AGEMS") {
    ags <- c("AGEPAN", ag)
  } else if (ag == "MOB") {
    ags <- c("ARSEP", "ARSEMA", ag)
  } else {
    ags <- ag
  }
  
  cases <- c(ags, uf)
  levs <- c(paste(ags, collapse = ", "), uf)

  major_type = "Autorizada"
  
  if (uf == "SP") {
    df_graph <- df_despesa_long %>%
      filter(exercicio >= dmy("01/01/2007")) %>%
      mutate(dimensao = str_to_title(dimensao)) %>%
      filter(unidade_orcamentaria %in% cases) %>%
      mutate(unidade_orcamentaria = ifelse(unidade_orcamentaria %in% ags,
                                           levs[1],
                                           unidade_orcamentaria)) %>%
      mutate(unidade_orcamentaria = factor(unidade_orcamentaria, ordered = TRUE, levels = levs)) %>%
      filter(!is.na(valor))
  } else {
    df_graph <- df_despesa_long %>%
      mutate(dimensao = str_to_title(dimensao)) %>%
      filter(unidade_orcamentaria %in% cases) %>%
      mutate(unidade_orcamentaria = ifelse(unidade_orcamentaria %in% ags,
                                           levs[1],
                                           unidade_orcamentaria)) %>%
      mutate(unidade_orcamentaria = factor(unidade_orcamentaria, ordered = TRUE, levels = levs)) %>%
      filter(!is.na(valor))
  }
  try({{
    p1 <- df_graph %>%
      ggplot(aes(x = exercicio, y = valor, group = dimensao, color = dimensao)) +
      geom_point() +
      geom_line() +
      labs(
        title = paste0("Despesa ", major_type, " e Empenhada (", ag, " vs. ", uf, ")"),
        subtitle = "Dados orçamentários das agências reguladoras estaduais",
        x = "Exercício",
        y = "Despesa (R$)",
        color = "Dimensão Orçamentária"
      ) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
      scale_color_manual(values = c(color_scheme$fgv_primary, color_scheme$green)) +
      facet_wrap(~unidade_orcamentaria, nrow = 2, scales = "free_y")
    
    plot_name <- paste0("1a_comp_autorizada_empenhada_", uf, "_", ag, ".png")
    save_plot(plot_name, plot = p1)
  }})
} # 1a (comp) sem labels

for (i in seq_along(unique_ags)) {
  ag <- unique_ags[[i]]
  uf <- df_ref_ufs %>%
    filter(nome == ag) %>%
    .$uf %>%
    .[[1]]
  
  if (uf == "AM") {
    major_type = "Orçada"
    df_graph <- df_despesa_long %>%
      mutate(dimensao = dimensao %>%
               str_to_title() %>%
               str_replace("Autorizada", "Orçada")) %>%
      filter(unidade_orcamentaria %in% c(ag, uf)) %>%
      mutate(unidade_orcamentaria = factor(unidade_orcamentaria, ordered = TRUE, levels = c(uf, ag))) %>%
      filter(!is.na(valor)) %>%
      mutate(l = paste0("R$", round(valor / 1000000, 1), "Mi"))
  } else {
    major_type = "Autorizada"
    df_graph <- df_despesa_long %>%
      mutate(dimensao = str_to_title(dimensao)) %>%
      filter(unidade_orcamentaria %in% c(ag, uf)) %>%
      mutate(unidade_orcamentaria = factor(unidade_orcamentaria, ordered = TRUE, levels = c(uf, ag))) %>%
      filter(!is.na(valor)) %>%
      mutate(l = paste0("R$", round(valor / 1000000, 1), "Mi"))
  }
  
  scope_uf <- df_despesa_long %>%
    filter(!is.na(valor)) %>%
    filter(unidade_orcamentaria == uf) %>%
    .$valor %>%
    scope_from_vec()
  
  scope_ag <- df_despesa_long %>%
    filter(!is.na(valor)) %>%
    filter(unidade_orcamentaria == ag) %>%
    .$valor %>%
    scope_from_vec()
  
  try({{
    p1 <- df_graph %>%
      mutate(nudge = ifelse(unidade_orcamentaria == ag,
                            ifelse(dimensao == "Despesa Empenhada", valor-(0.1 * scope_ag), valor+(0.1 * scope_ag)),
                            ifelse(dimensao == "Despesa Empenhada", valor-(0.1 * scope_uf), valor+(0.1 * scope_uf)))) %>%
      ggplot(aes(x = exercicio, y = valor, group = dimensao, color = dimensao, label = l)) +
      geom_text(aes(y = nudge), size = 2, show.legend = FALSE) +
      geom_point() +
      geom_line() +
      labs(
        title = paste0("Despesa ", major_type, " e Empenhada (", ag, " vs. ", uf, ")"),
        subtitle = "Dados orçamentários das agências reguladoras estaduais",
        x = "Exercício",
        y = "Despesa (R$)",
        color = "Dimensão Orçamentária"
      ) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
      scale_color_manual(values = c(color_scheme$fgv_primary, color_scheme$green)) +
      facet_wrap(~unidade_orcamentaria, nrow = 2, scales = "free_y")
    
    plot_name <- paste0("1a_label_comp_autorizada_empenhada_", uf, "_", ag, ".png")
    save_plot(plot_name, plot = p1)
  }})
} # 1a (comp) com labels

## 1b Evolução dos percentuais - agência vs. UF

for (i in seq_along(unique_ags)) {
  ag <- unique_ags[[i]]
  uf <- df_ref_ufs %>%
    filter(nome == ag) %>%
    .$uf %>%
    .[[1]]
  
  if (uf == "AM") {
    major_type = c("Orçada", "orçada")
  } else {
    major_type = c("Autorizada", "autorizada")
  }
  
  p1 <- df_despesa %>%
    filter(unidade_orcamentaria %in% c(ag, uf)) %>%
    mutate(perc_empenhada = despesa_empenhada / despesa_autorizada,
           unidade_orcamentaria = factor(unidade_orcamentaria, ordered = TRUE, levels = c(uf, ag)))  %>%
    filter(!is.na(perc_empenhada)) %>%
    ggplot(aes(x = exercicio, y = perc_empenhada, group = unidade_orcamentaria, color = unidade_orcamentaria)) +
    geom_point() +
    geom_line() +
    labs(title = paste0("Percentual da despesa ", major_type[2], " que é empenhado (", ag, " vs. ", uf, ")"),
         subtitle = "Dados orçamentários das agências reguladoras estaduais",
         x = "Exercício", y = paste0("Despesa Empenhada / Despesa ", major_type[1]),
         color = "Unidade Orçamentária") +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_color_manual(values = c(color_scheme$fgv_primary, color_scheme$fgv_secondary))
    
  plot_name <- paste0("1b_perc_autorizada_empenhada_", uf, "_", ag, ".png")
  save_plot(plot_name, plot = p1)

} # 1b (perc) sem labels

# 2. Evolução da despesa empenhada da agência (com e sem inflação) ----

df_despesa_ipca <- df_despesa %>%
  mutate(ano = str_extract(exercicio, "\\d{4}") %>% as.numeric()) %>%
  left_join(ipca, by = "ano")
  
for (i in seq_along(unique_ags)) {
  ag <- unique_ags[[i]]
  graph_df <- df_despesa_ipca %>%
    filter(unidade_orcamentaria == ag) %>%
    filter(!is.na(despesa_empenhada))
  
  years <- sort(unique(graph_df$ano))
  
  index_table <- vector('list', length(years))
  
  for (i in seq_along(years)) {
    year <- years[[i]]
    if (i == 1) {
      index_table[[1]] <- tibble(
        ano = year,
        multiplier = 1
      )
    } else {
      perc <- ipca %>%
        filter(ano == year) %>%
        .$ipca
      
      index_table[[i]] <- tibble(
        ano = year,
        multiplier = index_table[[(i-1)]]$multiplier * (1 + perc)
      )
    }
  }
  
  try({{
    p2a <- graph_df %>%
      left_join(bind_rows(index_table), by = "ano") %>%
      filter(!is.na(despesa_empenhada)) %>%
      ggplot(aes(x = exercicio, y = despesa_empenhada, group = "")) +
      geom_point(color = color_scheme$orange) +
      geom_line(color = color_scheme$orange) +
      labs(title = paste0("Evolução da despesa empenhada da ", ag, " (valores nominais)"),
           subtitle = "Dados orçamentários das agências reguladoras estaduais",
           x = "Exercício", y = "Despesa Empenhada (R$)") +
      scale_y_continuous(labels = scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ","))
    
    plot_name_2a <- paste0("2a_despesa_empenhada_", ag, "_nominal.png")
    save_plot(plot_name_2a, plot = p2a)
  }})

  try({{
    p2b <- graph_df %>%
      left_join(bind_rows(index_table), by = "ano") %>%
      mutate(empenhada_corrigida = despesa_empenhada / multiplier) %>%
      filter(!is.na(empenhada_corrigida)) %>%
      ggplot(aes(x = exercicio, y = empenhada_corrigida, group = "")) +
      geom_point(color = color_scheme$fgv_secondary) +
      geom_line(color = color_scheme$fgv_secondary) +
      labs(title = paste0("Evolução da despesa empenhada da ", ag, ", corrigida pelo IPCA"),
           subtitle = "Dados orçamentários das agências reguladoras estaduais",
           x = "Exercício", y = "Despesa Empenhada corrigida pelo IPCA (R$)") +
      scale_y_continuous(labels = scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ","))
    
    plot_name_2b <- paste0("2b_despesa_empenhada_", ag, "_corrigida.png")
    save_plot(plot_name_2b, plot = p2b)
  }})
  
  try({{
    p2c <- graph_df %>%
      left_join(bind_rows(index_table), by = "ano") %>%
      mutate(empenhada_corrigida = despesa_empenhada / multiplier) %>%
      filter(!is.na(empenhada_corrigida)) %>%
      pivot_longer(cols = c("despesa_empenhada", "empenhada_corrigida")) %>%
      mutate(dimensao = ifelse(name == "despesa_empenhada", "Valores Nominais", "Correção pelo IPCA"),
             dimensao = factor(dimensao, ordered = T, levels = c("Valores Nominais", "Correção pelo IPCA"))) %>%
      ggplot(aes(x = exercicio, y = value, group = dimensao, color = dimensao)) +
      geom_point() +
      geom_line() +
      labs(title = paste0("Evolução da despesa empenhada da ", ag, " (valores nominais e corrigidos)"),
           subtitle = "Dados orçamentários das agências reguladoras estaduais",
           x = "Exercício", y = "Despesa Empenhada (R$)", color = "Correção Monetária") +
      scale_color_manual(values = c(color_scheme$orange, color_scheme$fgv_secondary)) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ","))
    
    plot_name_2c <- paste0("2c_despesa_empenhada_", ag, "_comp.png")
    save_plot(plot_name_2c, plot = p2c)
  }})
}

# 3. Comparar a despesa empenhada em uma base comum (100), em cada estado ----

for (i in seq_along(unique_ufs)) {
  
  this_uf <- unique_ufs[[i]]
  
  uf_ref <- df_despesa %>%
    filter(!is.na(unidade_orcamentaria)) %>%
    filter(unidade_orcamentaria == this_uf) %>%
    filter(!is.na(despesa_empenhada)) %>%
    select(!despesa_autorizada) %>%
    arrange(exercicio)
  
  if (nrow(uf_ref) < 2) { next() }
  
  # print(this_uf)
  
  uf_base <- vector('list', nrow(uf_ref))
  
  for (i in seq_along(uf_base)) {
    # print(paste("i =", i))
    if (i == 1) {
      uf_base[[1]] <- tibble(
        exercicio = uf_ref$exercicio[[1]],
        unidade_orcamentaria = this_uf,
        despesa_empenhada = uf_ref$despesa_empenhada[[1]],
        base = 100
      )
    } else {
      previous_base <- uf_base[[(i-1)]]$base
      perc_var <- (uf_ref$despesa_empenhada[[i]] - uf_ref$despesa_empenhada[[(i-1)]]) / uf_ref$despesa_empenhada[[(i-1)]]
      
      uf_base[[i]] <- tibble(
        exercicio = uf_ref$exercicio[[i]],
        unidade_orcamentaria = this_uf,
        despesa_empenhada = uf_ref$despesa_empenhada[[i]],
        base = previous_base * (1 + perc_var)
      )
    }
  }
  uf_base <- bind_rows(uf_base)
  
  ags <- df_ref %>%
    filter(uf == this_uf) %>%
    .$nome
  
  # print(paste(c("ags: ", ags), collapse = " "))
  
  ags_base <- vector('list', length(ags))
  
  for (j in seq_along(ags_base)) {
    
    ag <- ags[[j]]
    
    # print(paste("ag =", ag))
    # print(paste("j =", j))
    
    ag_ref <- df_despesa %>%
      filter(!is.na(unidade_orcamentaria)) %>%
      filter(unidade_orcamentaria == ag) %>%
      filter(!is.na(despesa_empenhada)) %>%
      select(!despesa_autorizada) %>%
      arrange(exercicio)
    
    if (nrow(ag_ref) < 2) { next() }
    
    ag_base <- vector('list', nrow(ag_ref))
    
    for (u in seq_along(ag_base)) {
      
      # print(paste("u =", u))
      
      if (u == 1) {
        ag_base[[1]] <- tibble(
          exercicio = ag_ref$exercicio[[1]],
          unidade_orcamentaria = ag,
          despesa_empenhada = ag_ref$despesa_empenhada[[1]],
          base = 100
        )
      } else {
        previous_base <- ag_base[[(u-1)]]$base
        perc_var <- (ag_ref$despesa_empenhada[[u]] - ag_ref$despesa_empenhada[[(u-1)]]) / ag_ref$despesa_empenhada[[(u-1)]]
        
        ag_base[[u]] <- tibble(
          exercicio = ag_ref$exercicio[[u]],
          unidade_orcamentaria = ag,
          despesa_empenhada = ag_ref$despesa_empenhada[[u]],
          base = previous_base * (1 + perc_var)
        )
      }
    }
    ags_base[[j]] <- bind_rows(ag_base)
  }
  ags_base <- bind_rows(ags_base)
  df_graph <- bind_rows(uf_base, ags_base)
  
  colors <- c(color_scheme$fgv_primary, color_scheme$green, color_scheme$green2, color_scheme$green3)
  
  comps <- paste(c(this_uf, ags), collapse = ", ")
  
  p3 <- df_graph %>%
    mutate(unidade_orcamentaria = factor(unidade_orcamentaria, ordered = TRUE, levels = c(this_uf, ags))) %>%
    ggplot(aes(x = exercicio, y = base, group = unidade_orcamentaria, color = unidade_orcamentaria)) +
    geom_point(alpha = 0.8) +
    geom_line() +
    scale_color_manual(values = colors) +
    labs(title = paste0("Comparativo da evolução da despesa empenhada (base 100, calculada para " , comps, ")"),
         subtitle = "Dados orçamentários das agências reguladoras estaduais",
         x = "Exercício",
         y = "Despesa, calculada em base 100, a partir do primeiro ano",
         color = "Unidade Orçamentária")
  
  plot_name <- paste0("3_base_100_empenhada_", this_uf, ".png")
  save_plot(plot_name, plot = p3)
}

# 4. Medir percentual da despesa empenhada no estado que corresponde à despesa empenhada pela agência

for (i in seq_along(unique_ags)) {
  ag <- unique_ags[[i]]
  uf <- df_ref_ufs %>%
    filter(nome == ag) %>%
    .$uf %>%
    .[[1]]
  
  ag_ref <- df_despesa %>%
    filter(!is.na(despesa_empenhada)) %>%
    filter(unidade_orcamentaria == ag) %>%
    select(!despesa_autorizada) %>%
    arrange(exercicio)
  
  ag_uf_empenhada <- vector('list', nrow(ag_ref))
  
  for (j in seq_along(ag_uf_empenhada)) {
    
    this_exercicio <- ag_ref$exercicio[[j]]
    
    uf_empenhada <- df_despesa %>%
      filter(exercicio == this_exercicio) %>%
      filter(unidade_orcamentaria == uf) %>%
      .$despesa_empenhada
    
    ag_uf_empenhada[[j]] <- tibble(
      exercicio = this_exercicio,
      uf_ref = uf_empenhada
    )
  }
  
  try({{
    df_graph <- ag_ref %>%
      left_join(bind_rows(ag_uf_empenhada), by = "exercicio") %>%
      mutate(perc_empenhada = despesa_empenhada / uf_empenhada) %>%
      filter(!is.na(perc_empenhada))
    
    p4 <- df_graph %>%
      ggplot(aes(x = exercicio, y = perc_empenhada)) +
      geom_point(color = color_scheme$fgv_secondary) +
      geom_line(color = color_scheme$fgv_secondary) +
      labs(title = paste0("Percentual da despesa empenhada no estado que corresponde à agência (", uf, " vs. ", ag, ")"),
           subtitle = "Dados orçamentários das agências reguladoras estaduais",
           x = "Exercício", y = "Percentual da despesa empenhada (%)") +
      scale_y_continuous(labels = scales::label_percent())
    
    plot_name <- paste0("4_perc_empenhada_uf_", ag, ".png")
    save_plot(plot_name, plot = p4)
  }})
}

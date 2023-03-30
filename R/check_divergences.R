library(tidyverse)

# DATA ----

df <- read_csv2("input/preenchimento_limpo.csv")

cols <- names(df)
cols_clean <- df %>% janitor::clean_names() %>% names()
agencies <- df$Nome %>% unique()

# PLOTS ----

plot_boolean <- function(measurement, col, clean_col, dir = "./plots") {
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

plot_numeric <- function(measurement, col, clean_col, dir = "./plots") {
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

for (i in seq_along(cols)) {
  col <- cols[[i]]
  measurement <- df[[col]]
  if (i > 3 && !str_detect(col, "^Tempo")) {
    plot_boolean(measurement, col, cols_clean[[i]])
  } else if (i > 3) {
    plot_numeric(measurement, col, cols_clean[[i]])
  }
}

# DIVERGENCES ----

report_divergence <- function(report, col, researchers, measurements) {
  report <- paste0(report, "### ", col, "\n\n")
  for (i in seq_along(researchers)) {
    report <- paste0(
      report, "- ", researchers[[i]], " : ", measurements[[i]], "\n"
    )
  }
  report <- paste0(report, "\n---\n\n")
}

divergences <- vector("list", length(agencies))
report <- "# Relatório de Divergências\n\n---\n\n"

for (i in seq_along(agencies)) {
  agency <- agencies[[i]]
  report <- paste0(report, "## ", agency, "\n\n---\n\n")
  ref <- df %>% filter(Nome == agency)
  researchers <- ref[[2]] %>% unique() %>% paste(collapse = ", ")
  agency_divergences <- tibble(
    agencia = agency,
    pesquisadores = researchers
  )
  for (j in seq_along(cols)) {
    if (j > 3) {
      col <- cols[[j]]
      col_measurements <- ref[[col]]
      if (length(unique(col_measurements)) == 1) {
        has_divergence <- tibble(x = 0)
        names(has_divergence)[[1]] <- col
        agency_divergences <- bind_cols(agency_divergences, has_divergence)
      } else {
        has_divergence <- tibble(x = 1)
        names(has_divergence)[[1]] <- col
        agency_divergences <- bind_cols(agency_divergences, has_divergence)
        report <- report_divergence(report, col, ref[[2]], col_measurements)
      }
    }
  }
  divergences[[i]] <- agency_divergences
}

divergences <- bind_rows(divergences)

write_csv2(divergences, "output/divergencias.csv")
write_file(report, "output/divergencias.md")

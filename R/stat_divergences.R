library(tidyverse)

divergences <- read_csv2("output/divergencias.csv")

divergences_cols <- vector("list", length(divergences))

for (i in seq_along(divergences)) {
  if (i > 2) {
    col <- names(divergences)[[i]]
    divergences_cols[[i]] <- tibble(
      tipo = "variável de análise",
      grupo = col,
      perc_div = sum(divergences[[i]]) / nrow(divergences)
    )
  }
}

divergences_cols <- bind_rows(divergences_cols)

divergences_cols %>%
  arrange(desc(perc_div)) %>% 
  View()

divergences_agencies <- vector("list", nrow(divergences))

for (i in seq_len(nrow(divergences))) {
  agency <- divergences$agencia[[i]]
  measurements <- divergences[i, 3:ncol(divergences)] %>%
    as.numeric()
  divergences_agencies[[i]] <- tibble(
    tipo = "agência",
    grupo = agency,
    perc_div = sum(measurements) / length(measurements)
  )
}

divergences_agencies <- bind_rows(divergences_agencies)

divergence_stats <- bind_rows(divergences_cols, divergences_agencies)

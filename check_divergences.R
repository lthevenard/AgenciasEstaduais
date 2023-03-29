library(tidyverse)
library(googlesheets4)

googlesheets4::gs4_auth()

ss <- googlesheets4::as_sheets_id(
  "https://docs.google.com/spreadsheets/d/1vO-oTi9QgUt0CxgVn2Eho5wcbgn71u-kwbXu_ihXE3w/edit#gid=1801310720"
)

df <- googlesheets4::read_sheet(ss, sheet = "Preenchimento")

# cols <- names(df)
# compare <- vector(length = length(cols))
# 
# for (i in seq_along(compare)) {
#   compare[[i]] <- readline(prompt=paste(cols[i], "- ")) %>%
#     as.numeric() %>%
#     as.logical()
# }
# 
# cols_to_compare <- tibble(col = cols, must_compare = compare)
# saveRDS(cols_to_compare, "formal_cols_to_compare.RDS")

compare <- readRDS("formal_cols_to_compare.RDS") %>% 
  .$must_compare

for (i in 1:nrow(df)) {
  for (j in 1:ncol(df)) {
    if (is.null(df[i, j])) {
      df[i, j] <- NA
    }
  }
}

agencias <- df$Nome %>% unique()

divergencias <- list()

for (i in seq_along(cols)) {
  if (compare[[i]]) {
    coluna <- cols[[i]]
    divergencias[[coluna]] <- list()
    
    ref <- tibble(
      pesquisador = df$`Pesquisador Responsável`,
      agencia = df$Nome,
      preenchimento = df[[cols[[i]]]],
      categoria = coluna
    )
    
    for (j in seq_along(agencias)) {
      
      ref_agencia <- ref %>% 
        filter(agencia == agencias[[j]])
      
      num_preenchimentos <- ref_agencia$preenchimento %>% 
        unique() %>% 
        length()
      
      if (num_preenchimentos > 1) {
        agencia <- agencias[[j]]
        divergencias[[coluna]][[agencia]] <- ref_agencia
      }
    }
  }
}

num_divergencias <- vector('list', length(cols))

for (i in seq_along(cols)) {
  if (compare[[i]]) {
    coluna <- cols[[i]]
    divs <- length(divergencias[[coluna]])
    num_divergencias[[i]] <- tibble(
      categoria = coluna,
      num_divergencias = divs
    )
  }
}

bind_rows(num_divergencias) %>% View()




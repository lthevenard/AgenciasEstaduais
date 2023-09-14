library(tidyverse)
library(readxl)

dfs_por_agencia <- list.files("input/dirigentes/", full.names = T) %>%
  map(read_xlsx)



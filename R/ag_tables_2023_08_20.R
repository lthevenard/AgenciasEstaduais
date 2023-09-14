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

df_dimensions <- df %>%
  na_to_zero() %>%
  mutate(
    ## Autonomia e poderes regulatórios
    previsoes_expressas = (autonomia_administrativa + autonomia_financeira + autonomia_patrimonial + autonomia_tecnica) / 4,
    pge = (3 - assessoria_vinculada_pge - assessores_procuradores - algum_procurador) / 3,
    autonomia_soma = previsoes_expressas + pge + taxa_de_regulacao + competencia_normativa + (1 - poder_concedente) + poder_sancionatorio + poder_tac,
    autonomia_perc = autonomia_soma / 7,
    ## Governança e boas práticas
    ouvidoria_c = (ouvidoria + ouvidor_mandato_fixo) / 2,
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

df_dimensions <- df_dimensions %>%
  mutate(nome_completo = case_when(
    nome == "ADASA" ~ "Agência Reguladora de Águas, Energia e Saneamento Básico do Distrito Federal",
    nome == "AGEAC" ~ "Agência Reguladora dos Serviços Públicos do Estado do Acre",
    nome == "AGEMS" ~ "Agência Estadual de Regulação de Serviços Públicos de Mato Grosso do Sul",
    nome == "AGENERSA" ~ "Agência Reguladora de Energia e Saneamento Básico do Estado do Rio de Janeiro",
    nome == "AGEPAR" ~ "Agência Reguladora de Serviços Públicos Delegados do Paraná",
    nome == "AGERBA" ~ "Agência Estadual de Regulação de Serviços Públicos de Energia, Transportes e Comunicações da Bahia",
    nome == "AGERGS" ~ "Agência Estadual de Regulação dos Serviços Públicos Delegados do Rio Grande do Sul",
    nome == "AGERO" ~ "Agência de Regulação dos Serviços Públicos de Rondônia",
    nome == "AGERSA" ~ "Agência Reguladora de Saneamento Básico do Estado da Bahia",
    nome == "AGETRANSP" ~ "Agência Reguladora de Serviços Públicos Concedidos de Transportes Aquaviários, Ferroviários, Metroviários e de Rodovias do Estado do Rio de Janeiro",
    nome == "AGR" ~ "Agência Goiana de Regulação, Controle e Fiscalização de Serviços Públicos",
    nome == "AGRESE" ~ "Agência Reguladora de Serviços Públicos do Estado de Sergipe",
    nome == "AGRESPI" ~ "Agência Reguladora do Serviços Públicos Delegados do Estado do Piauí",
    nome == "ARCE" ~ "Agência Reguladora de Serviços Públicos Delegados do Estado do Ceará",
    nome == "ARCON" ~ "Agência de Regulação e Controle de Serviços Públicos do Estado do Pará",
    nome == "ARESC" ~ "Agência de Regulação dos Serviços Públicos de Santa Catarina",
    nome == "ARPB" ~ "Agência de Regulação do Estado da Paraíba",
    nome == "ARPE" ~ "Agência de Regulação dos Serviços Públicos Delegados do Estado de Pernambuco",
    nome == "ARSAE" ~ "Agência Reguladora de Serviços de Abastecimento de Água e de Esgotamento Sanitário do Estado de Minas Gerais",
    nome == "ARSAL" ~ "Agência Reguladora de Serviços Públicos do Estado de Alagoas",
    nome == "ARSAP" ~ "Agência Reguladora de Serviços Públicos Delegados do Estado do Amapá",
    nome == "ARSEP" ~ "Agência Reguladora de Serviços Públicos do Rio Grande do Norte",
    nome == "ARSEPAM" ~ "Agência Reguladora de Serviços Públicos Delegados e Contratados do Estado do Amazonas",
    nome == "ARSESP" ~ "Agência Reguladora de Serviços Públicos do Estado de São Paulo",
    nome == "ARSP" ~ "Agência de Regulação de Serviços Públicos do Espírito Santo",
    nome == "ARTESP" ~ "Agência Reguladora de Serviços Públicos Delegados de Transporte do Estado de São Paulo",
    nome == "ATR" ~ "Agência Tocantinense de Regulação Controle e Fiscalização de Serviços Públicos",
    nome == "MOB" ~ "Agência Estadual de Mobilidade Urbana e Serviços Públicos",
    nome == "AGER" ~ "Agência Estadual de Regulação dos Serviços Públicos Delegados do Estado do Mato Grosso"
  )) %>%
  mutate(
    nome_sigla = paste(nome, "-", nome_completo)
  )

inv <- function(x) {
  ifelse(x == 1, "0", "1")
}

neg <- function(x) {
  ifelse(x == 1, "-1", "0")
}

ac <- function(x) {
  as.character(x)
}

nomes_ag <- unique(df_dimensions$nome)
tabelas <- vector("list", length(nomes_ag))
size_table <- 42

for (i in seq_along(tabelas)) {
  df_ag <- df_dimensions %>%
    filter(nome == nomes_ag[[i]])
  
  ag_table <- tibble(
    col0 = rep("", size_table),
    col1 = rep("", size_table),
    col2 = rep("", size_table),
    col3 = rep("", size_table),
    col4 = rep("", size_table),
    col5 = rep("", size_table),
    col6 = rep("", size_table),
    col7 = rep("", size_table)
  )
  
  ag_table$col1[1] <- df_ag$nome_sigla[1]
  
  ag_table$col2[2] <- "Autonomia e poderes regulatórios"
  ag_table$col7[2] <- scales::percent_format(accuracy = 0.1)(df_ag$autonomia_perc[1])
  
  ag_table$col3[3] <- "Previsões Expressas de Autonomia"
  ag_table$col7[3] <- ac(round(df_ag$previsoes_expressas[1], digits = 2))
  ag_table$col4[4] <- "Autonomia Administrativa"
  ag_table$col7[4] <- ac(df_ag$autonomia_administrativa[1])
  ag_table$col4[5] <- "Autonomia Financeira"
  ag_table$col7[5] <- ac(df_ag$autonomia_financeira[1])
  ag_table$col4[6] <- "Autonomia Patrimonial"
  ag_table$col7[6] <- ac(df_ag$autonomia_patrimonial[1])
  ag_table$col4[7] <- "Autonomia Técnica"
  ag_table$col7[7] <- ac(df_ag$autonomia_tecnica[1])
  
  ag_table$col3[8] <- "Previsão de Taxa de Regulação"
  ag_table$col7[8] <- ac(df_ag$taxa_de_regulacao[1])
  
  ag_table$col3[9] <- "Previsão de Competência Normativa"
  ag_table$col7[9] <- ac(df_ag$competencia_normativa[1])
  
  ag_table$col3[10] <- "Previsão de Poder Sancionatório"
  ag_table$col7[10] <- ac(df_ag$poder_sancionatorio[1])
  
  ag_table$col3[11] <- "Previsão de poder para celebrar TAC"
  ag_table$col7[11] <- ac(df_ag$poder_tac[1])
  
  ag_table$col3[12] <- "Grau de Vinculação à PGE"
  ag_table$col7[12] <- ac(round(df_ag$pge[1], digits = 2))
  ag_table$col4[13] <- "Acessoria vinculada à PGE"
  ag_table$col7[13] <- neg(df_ag$assessoria_vinculada_pge[1])
  ag_table$col4[14] <- "Acessores são procuradores"
  ag_table$col7[14] <- neg(df_ag$assessores_procuradores[1])
  ag_table$col4[15] <- "Algum procurador alocado"
  ag_table$col7[15] <- neg(df_ag$algum_procurador[1])
  
  
  ag_table$col3[16] <- "Não é Poder Concedente"
  ag_table$col7[16] <- inv(df_ag$poder_concedente[1])
  
  ag_table$col2[17] <- "Governança e boas práticas"
  ag_table$col7[17] <- scales::percent_format(accuracy = 0.1)(df_ag$governanca_perc[1])
  
  ag_table$col3[18] <- "Ouvidoria"
  ag_table$col7[18] <- ac(round(df_ag$ouvidoria_c[1], digits = 2))
  ag_table$col4[19] <- "Possui ouvidoria"
  ag_table$col7[19] <- ac(df_ag$ouvidoria[1])
  ag_table$col4[20] <- "Ouvidor tem mandato fixo"
  ag_table$col7[20] <- ac(df_ag$ouvidor_mandato_fixo[1])
  
  ag_table$col3[21] <- "Previsão de Programa de Integridade (Compliance)"
  ag_table$col7[21] <- ac(df_ag$programa_de_compliance[1])
  
  ag_table$col3[22] <- "Previsão de Participação Social"
  ag_table$col7[22] <- ac(df_ag$participacao_social[1])
  
  ag_table$col3[23] <- "Previsão de Agenda Regulatória"
  ag_table$col7[23] <- ac(df_ag$agenda_regulatoria[1])
  
  ag_table$col3[24] <- "Previsão de AIR"
  ag_table$col7[24] <- ac(df_ag$analise_de_impacto[1])
  
  ag_table$col2[25] <- "Independência e expertise dos dirigentes"
  ag_table$col7[25] <- scales::percent_format(accuracy = 0.1)(df_ag$dirigentes_perc[1])
  
  ag_table$col3[26] <- "Requisitos/Impedimentos dos Dirigentes"
  ag_table$col7[26] <- ac(round(df_ag$requisitos_impedimentos_perc[1], digits = 2))
  ag_table$col4[27] <- "Impedimento por atuação no setor"
  ag_table$col7[27] <- ac(df_ag$algum_impedimento_previo_setor[1])
  ag_table$col4[28] <- "Impedimento por ocupar cargo político"
  ag_table$col7[28] <- ac(df_ag$alguma_vedacao_setor_publico[1])
  ag_table$col4[29] <- "Vedação de filiação partidária"
  ag_table$col7[29] <- ac(df_ag$vedacao_filiacao_partidaria[1])
  ag_table$col4[30] <- "Exigência de experiência prévia"
  ag_table$col7[30] <- ac(df_ag$experiencia_previa[1])
  ag_table$col4[31] <- "Previsão de quarentena após mandato"
  ag_table$col7[31] <- ac(df_ag$quarentena[1])
  
  ag_table$col3[32] <- "Processo de Nomeação dos Dirigentes"
  ag_table$col7[32] <- ac(round(df_ag$processo_nomeacao_perc[1], digits = 2))
  ag_table$col4[33] <- "Sabatina prévia da Assembleia Legislativa"
  ag_table$col7[33] <- ac(df_ag$sabatina_assembleia[1])
  ag_table$col4[34] <- "Aprovação prévia da Assembleia Legislativa"
  ag_table$col7[34] <- ac(df_ag$aprovacao_assembleia[1])
  ag_table$col4[35] <- "Eleição interna do Diretor-Presidente"
  ag_table$col7[35] <- ac(df_ag$eleicao_interna_presidente[1])
  ag_table$col4[36] <- "Livre indicação para mandatos provisórios"
  ag_table$col7[36] <- neg(df_ag$indicacao_livre_mandatos_provisorios[1])
  
  ag_table$col3[37] <- "Garantias Funcionais dos Dirigentes"
  ag_table$col7[37] <- ac(round(df_ag$garantias_funcionais_perc[1], digits = 2))
  ag_table$col4[38] <- "Mandato por prazo determinado"
  ag_table$col7[38] <- ac(df_ag$mandato_definido[1])
  ag_table$col4[39] <- "Mandatos não-coincidentes entre diretores"
  ag_table$col7[39] <- ac(df_ag$mandato_nao_coincidente_diretores[1])
  ag_table$col4[40] <- "Mandatos não-coincidentes com o Governador"
  ag_table$col7[40] <- ac(df_ag$mandato_nao_coincidente_governador[1])
  ag_table$col4[41] <- "Exoneração sob hipóteses pré-determinadas"
  ag_table$col7[41] <- ac(df_ag$preve_casos_exoneracao[1])
  ag_table$col4[42] <- "Procedimento expresso de exoneração"
  ag_table$col7[42] <- ac(df_ag$preve_procedimento_exoneracao[1])
  
  tabelas[[i]] <- ag_table
}

for (i in seq_along(tabelas)) {
  write_csv2(
    tabelas[[i]],
    paste0("output/tabelas_agencias/", nomes_ag[[i]],".csv")
  )
}

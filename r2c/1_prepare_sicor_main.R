################################################################################
# r2c/1_prepare_sicor_main.R
# -----------------------------------------------------------------------------
# Empilha os arquivos anuais brutos do SICOR (raw/sicor/*.gz) em uma unica
# base consolidada. Padroniza tipos, normaliza nomes de colunas (clean_names)
# e adiciona uma coluna `ano_base` com o ano de origem de cada linha.
#
# Tempo medio de execucao: ~16 min (depende do volume anual)
#
# Inputs
# ------
# - raw/sicor/SICOR_<ANO>.gz   (um arquivo por ano: 2018..2026)
#
# Outputs
# -------
# - clean/sicor_main_2018_2026.Rds
#
# Observacoes
# -----------
# * `CD_CONTRATO_STN` aparece como character em alguns anos e numerico em outros.
#   Forcamos integer64 para compatibilidade. NAs sao mantidos.
# * Nomes de colunas sao padronizados para snake_case via janitor::clean_names();
#   `X.Ref_Bacen` vira `x_ref_bacen` e renomeamos para `ref_bacen`.
################################################################################

# SETUP ------------------------------------------------------------------------
rm(list=ls())
gc()
strt.time <- Sys.time()

# source("_functions/GroundhogLibraries.R")

caminho <- file.path("C:/Users", Sys.getenv("USERNAME"),
                     "Documents", "baseMCR/dados/raw/sicor")


library(tidyverse)
library(stringr)
library(lubridate)
library(bit64)
library(janitor)

# pkgs <- c('tidyverse', 'lubridate', 'stringr', 'janitor', 'bit64')
# 
# groundhogLibraries(pkgs, date = "2024-02-08", tolerate.R.version='4.3.3')

# Remove Scientific Notation
options(scipen = 999)


#  Loading and basic cleaning each year main file, joining together ------------
# Read each year file
years <- 2018:2026
list_bases <- list.files(caminho, full.names = TRUE,
                         pattern = ".gz")



# Name and class check of variables over database years
var_check <- tibble::tibble(colname = character())

for (i in seq_along(list_bases)) {
  
  df <- read.delim(list_bases[[i]],
                   sep = ";",
                   header = TRUE,
                   encoding = "UTF-8")
  
  # Criar tibble com nome e classe de cada coluna
  x <- df %>%
    imap_dfr(~ tibble(colname = .y, classes = class(.x) %>% str_c(collapse = ", ")))
  
  # Juntar com var_check
  var_check <- full_join(var_check, x, by = 'colname')
  
  # Renomear a última coluna para o ano correspondente
  d <- min(years) - 1 + i
  colnames(var_check)[ncol(var_check)] <- as.character(d)
  
  rm(x, d)
}

# Create database year variable
list_df <- list()

ano_inicial  <- min(years)

# Loop para ler e adicionar coluna ANO_BASE
for (i in seq_along(list_bases)) {
  
  df <- read.delim(list_bases[[i]],
                   sep = ";",
                   header = TRUE,
                   encoding = "UTF-8")
  
  # Adiciona a coluna ANO_BASE
  df <- df %>%
    mutate(ANO_BASE = years[i])
  
  # Substitui na lista
  list_df[[i]] <- df
}

#check if class variables are constant
aux <- lapply(list_df[[1]], class) 
for (i in 2:length(list_df)) {
 
aux2 <-  (lapply(list_df[[i]], class) )

aux <- bind_rows(aux,aux2)
  }

# Variable "CD_CONTRATO_STN" is character in 2021-2023
# Changing to integer64 for compatibility
for (i in 1:length(list_df)) {
  list_df[[i]] <- list_df[[i]] %>%
    mutate(x = as.integer64(CD_CONTRATO_STN)) %>%
    mutate(x = na_if(x, is.na(CD_CONTRATO_STN) == T)) %>%
    select(-CD_CONTRATO_STN) %>%
    rename(CD_CONTRATO_STN = x)
}



# Joining all years together
df <- do.call(bind_rows, list_df)

df

# Clean names
df <- df %>%
  clean_names() %>%
  rename(
    ref_bacen = x_ref_bacen
  ) %>%
  relocate(ref_bacen, nu_ordem, ano_base)

unique(df$ano_base)

# Save full database
output <- file.path("C:/Users", Sys.getenv("USERNAME"),"Documents",
                    "baseMCR/dados/clean")

if(!dir.exists(output)){
  dir.create(output)
}

saveRDS(df, file.path(output, 
                      paste0(
                        "sicor_main_",
                        min(years),
                        '_',
                        max(years),
                        ".rds")
                      )
        )

# # Clean memory
# rm(list=ls())
# gc()

Sys.time() - strt.time

#### ---------------------------------------------------------------------- ####
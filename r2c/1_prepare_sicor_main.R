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
# Versao paralela: cada ano e' lido uma unica vez (a versao antiga lia cada
# .gz duas vezes -- uma para inspecionar classes e outra para a base final)
# e os arquivos sao processados em paralelo via future.apply.
rm(list=ls())
gc()
strt.time <- Sys.time()

caminho <- file.path("C:/Users", Sys.getenv("USERNAME"),
                     "Documents", "baseMCR/dados/raw/sicor")


library(tidyverse)
library(stringr)
library(lubridate)
library(bit64)
library(janitor)
library(future)
library(future.apply)

# Remove Scientific Notation
options(scipen = 999)

# Plano paralelo: usa N-2 workers; ajuste se a maquina tiver RAM limitada.
n_workers_par <- max(1L, parallel::detectCores() - 2L)
plan(multisession, workers = n_workers_par)
options(future.globals.maxSize = 4 * 1024^3)

cat("[PAR] workers configurados:", n_workers_par, "\n")


#  Loading and basic cleaning each year main file, joining together ------------
years <- 2018:2026
list_bases <- list.files(caminho, full.names = TRUE,
                         pattern = ".gz")

# Le cada arquivo ano UMA UNICA VEZ em paralelo e devolve:
# - df_year: data.frame com a coluna ANO_BASE ja adicionada
# - var_check_year: tibble (colname, classes) usada na auditoria
read_year <- function(i) {
  df <- read.delim(list_bases[[i]],
                   sep = ";",
                   header = TRUE,
                   encoding = "UTF-8")

  vc <- tibble::tibble(
    colname = names(df),
    classes = vapply(df, function(x) paste(class(x), collapse = ", "),
                     FUN.VALUE = character(1))
  )

  df$ANO_BASE <- years[i]

  list(df = df, var_check = vc)
}

read_results <- future_lapply(seq_along(list_bases), read_year,
                              future.seed = TRUE)

# Consolida o auditoria de classes (1 coluna por ano)
var_check <- tibble::tibble(colname = character())
for (i in seq_along(read_results)) {
  vc <- read_results[[i]]$var_check
  colnames(vc)[2] <- as.character(years[i])
  var_check <- dplyr::full_join(var_check, vc, by = "colname")
}

# Lista de dfs anuais
list_df <- lapply(read_results, `[[`, "df")
rm(read_results)
gc()

# Variable "CD_CONTRATO_STN" is character in 2021-2023
# Changing to integer64 for compatibility
list_df <- lapply(list_df, function(x) {
  x %>%
    mutate(tmp_stn = as.integer64(CD_CONTRATO_STN)) %>%
    mutate(tmp_stn = na_if(tmp_stn, is.na(CD_CONTRATO_STN))) %>%
    select(-CD_CONTRATO_STN) %>%
    rename(CD_CONTRATO_STN = tmp_stn)
})

# Joining all years together
df <- do.call(bind_rows, list_df)
rm(list_df)
gc()

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

# encerra workers paralelos
plan(sequential)

Sys.time() - strt.time

#### ---------------------------------------------------------------------- ####
################################################################################
# r2c/2_merge_comp_basic.R
# -----------------------------------------------------------------------------
# Junta a base SICOR consolidada com o complemento basico (que traz municipio
# e outros campos) e adiciona a serie do IPCA (Ipeadata) para deflacionar os
# valores nominais a precos de dez/2025.
#
# Cria duas variaveis fundamentais:
#   * ano_safra  -- comeca em julho do ano N e termina em junho de N+1
#   * vl_parc_credito_real  -- valor parcela em R$ dez/2025
#
# Inputs
# ------
# - clean/sicor_main_2018_2026.Rds
# - raw/sicor/complementos/SICOR_COMPLEMENTO_OPERACAO_BASICA.gz
# - raw/ipeadata[<data>].csv  (IPCA mensal)
#
# Output
# ------
# - clean/sicor_main_2018_2026_basic_complement.Rds
#
# Fonte: https://www.bcb.gov.br/estabilidadefinanceira/creditorural?modalAberto=tabelas_sicor
################################################################################

# SETUP ------------------------------------------------------------------------
rm(list=ls())
gc()

strt.time <- Sys.time()

library(tidyverse)
library(janitor)
library(data.table)

root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "baseMCR/dados")

# Remove Scientific Notation
options(scipen = 999)

# Sources
#https://www.bcb.gov.br/estabilidadefinanceira/creditorural?modalAberto=tabelas_sicor


#    Load SICOR data files -----------------------------------------------------

### Load full database
df <- readRDS(file.path(root,"clean", "sicor_main_2018_2026.Rds"))

input <- file.path(root,"raw/sicor",
                   "complementos")

### Load Basic complementary file (contains Municipality)
basic <-  read.delim(file.path(input, "SICOR_COMPLEMENTO_OPERACAO_BASICA.gz"),
                     sep = ";",
                     header = TRUE,
                     encoding = "UTF-8")  

### Load ipca ####
ipca <- fread(file.path(root,"raw", "ipeadata[30-03-2026-09-16].csv"))

# colnames(basic) <- str_to_lower(colnames(basic))

basic <- basic %>%
  clean_names() %>%
  rename(ref_bacen = x_ref_bacen)

# This file contains information on the municipality of the operation only for 
# the universe of subsidized credit operations.
gc()

# Identifying municipalities with the basic complementary file -----------------

# Check number of contracts in the main file
df %>%
  distinct(ref_bacen, nu_ordem) %>%
  nrow() == length(df$ref_bacen)# No duplicates 

# Check number of contracts in the complementary file
basic %>%
  distinct(ref_bacen, nu_ordem) %>%
  nrow() == length(basic$ref_bacen) # No duplicates 

### Merge full database and basic complementary file
basic <- basic %>% mutate(is_basic = 1)

### ipca data ####
colnames(ipca) <- c("data","value","nops")
ipca <- ipca %>% select(-nops)

dez2025 <- as.numeric(str_replace(ipca[data == 2025.12]$value,",","\\."))

ipca <- ipca %>% 
  mutate(
    ano = round(data,0),
    mes = round((data - ano)*100,0),
    value = as.numeric(str_replace(value,",","\\.")),
    value = dez2025/value
  ) %>% 
  filter(ano > 2017)

head(ipca)


### create data and year variable ####
df <- df %>% 
mutate(
  dt_emissao = as.Date(dt_emissao, format = "%d/%m/%Y"), 
  
  # Pegando as datas para criar a variável ano_safra,
  # que será o ano da nossa análise.
  ano = year(dt_emissao), 
  mes = month(dt_emissao),
  
  # Começa no primeiro dia de julho e se extende até junho do ano 
  #seguinte:
  ano_safra = ifelse(
    mes >= 7,
    paste0(ano, "/", (ano+1)), 
    paste0((ano-1), "/", ano) 
  )
) 

df <- df %>% 
  left_join(
    ipca, by = c("ano","mes")
  ) %>% 
  mutate(
    vl_parc_credito_real = vl_parc_credito*value
  )


df %>% 
  group_by(ano_safra) %>% 
  dplyr::summarise(
    credit_bi = sum(vl_parc_credito)/10^9,
    credit_bi_real = sum(vl_parc_credito_real)/10^9
  )

df <- df %>%
  left_join(basic, by = c("ref_bacen", "nu_ordem")) %>%
  mutate(is_basic = replace_na(is_basic, 0))

# Check for observations in the basic complementary file that are not in the
# operations database
# teste <- left_join(basic, df, by = c("ref_bacen", "nu_ordem")) %>%
#   filter(is.na(ano_base) == T)
# 
# nrow(teste)/nrow(basic) # 0 observations


### Save merged data
saveRDS(df, file.path(root,"clean", "sicor_main_2018_2026_basic_complement.Rds"))

Sys.time() - strt.time

### Clean memory
rm(list=ls())
gc()



#### ---------------------------------------------------------------------- ####
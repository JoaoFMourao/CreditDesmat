# =============================================================================
# sicor_per_car.R
# -----------------------------------------------------------------------------
# Junta SICOR (operacoes de credito rural) com a base de imoveis CAR (CAR ↔
# operacao via SICOR_PROPRIEDADES) e com o resultado do cruzamento PRODES x CAR
# (changedImpact.rds gerado em r2c/3_cross_prodes_car.R).
#
# Inputs
# ------
# - clean/sicor_main_2018_2026_basic_complement.Rds  (output do passo 2)
# - raw/sicor/complementos/SICOR_PROPRIEDADES.gz     (link contrato <-> CAR)
# - output/INPEs_exercise/changedImpact.rds          (CAR + desmat + biome)
# - raw/sicor/complementos/Fontes MCR 6-1-2 e 6-7-7.csv (linhas monitoradas)
#
# Outputs
# -------
# - built/asvCar_credit.Rds   (1 linha por CAR; flag tomou_credito + desmat)
# - built/credit_asv.Rds      (1 linha por contrato SICOR + dimensoes p/ analise)
# - built/properties_asv.Rds  (1 linha por (contrato, CAR); link bruto)
# - built/asvCar_credit.csv / credit_asv.csv / properties_asv.csv (samples csv)
#
# Variaveis novas geradas para o output municipal (item 4) e Shiny (item 5)
# -------------------------------------------------------------------------
# - status_car  -> categoriza cada contrato:
#                   "Sigiloso"           : informacao do CAR e' sigilosa
#                   "CAR nao informado"  : SICOR sem cod_imovel
#                   "CAR fora da base"   : cod_imovel informado mas ausente
#                                          do shapefile lista_mcr (nao foi
#                                          possivel cruzar com PRODES)
#                   "CAR na base"        : ao menos um cod_imovel cruzado
# - faixa_mf    -> faixa de modulos fiscais (apenas quando "CAR na base"):
#                   "<4 MF" / "4-15 MF" / ">=15 MF"
#                  Calculada a partir do MAIOR m_fiscal entre os CARs do
#                  contrato (regra de classificacao mais conservadora).
# =============================================================================

# SETUP ------------------------------------------------------------------------
rm(list = ls())
gc()

strt.time <- Sys.time()

library(tidyverse)
library(janitor)
library(data.table)
library(lubridate)

root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "baseMCR/dados")

options(scipen = 999)


# LOAD DATA --------------------------------------------------------------------

## Base SICOR completa ja com municipio + IPCA (passo 2)
df <- readRDS(file.path(root, "clean", "sicor_main_2018_2026_basic_complement.Rds"))

## Slim das colunas relevantes para o restante da analise.
## Mantemos:
##   - chaves (ref_bacen, nu_ordem)
##   - tempo (ano_base, ano, mes, ano_safra)
##   - valores (vl_parc_credito, vl_parc_credito_real)
##   - dimensoes do contrato pedidas para os outputs do item 4:
##       cd_fonte_recurso, cd_programa, cd_subprograma, cd_modalidade,
##       cd_tipo_pessoa
##   - localizacao (cd_municipio_ibge_cc) -- vem do complemento basico
##   - flag is_basic (esta no complemento basico?)
## Colunas selecionadas (nomes verificados na base):
## - cd_modalidade e cd_tipo_pessoa NAO existem no arquivo principal do SICOR;
##   modalidade/custeio-investimento pode ser derivada de cd_empreendimento
##   ou cd_categ_emitente; tipo de pessoa (F/J) vem de tabela de beneficiarios.
## - municipio: nome correto e' cd_ibge_municipio (nao cd_municipio_ibge_cc)
df <- df %>%
  select(
    ref_bacen, nu_ordem, ano_base,
    cd_fonte_recurso, cd_programa, cd_subprograma,
    cd_empreendimento, cd_categ_emitente,
    cd_ibge_municipio,
    vl_parc_credito, vl_parc_credito_real,
    ano, mes, ano_safra,
    is_basic
  )

## Tabela de propriedades (link contrato <-> CAR) -----------------------------
input <- file.path(root, "raw/sicor", "complementos")

properties <- read.delim(
  file.path(input, "SICOR_PROPRIEDADES.gz"),
  sep = ";",
  header = TRUE,
  encoding = "UTF-8"
)

## Saida do cruzamento PRODES x CAR (passo 3)
asvCar <- readRDS(
  file.path(root, "output", "INPEs_exercise", "changedImpact.rds")
)

## Linhas/fontes monitoradas (universo de credito controlado/direcionado)
fonteMonitor <- fread(
  file.path(input, "Fontes MCR 6-1-2 e 6-7-7.csv"),
  encoding = "Latin-1"
)


# DATA HANDLING ----------------------------------------------------------------

## Properties: padroniza nomes e marca os contratos que tomaram credito --------
properties <- properties %>%
  clean_names() %>%
  rename(
    ref_bacen  = x_ref_bacen,
    cod_imovel = cd_car
  ) %>%
  mutate(tookCredit = "Tomou Credito")

## asvCar: limpa cod_imovel (a base CAR usa hifens, properties nao).
## Todo CAR em asvCar (= lista_mcr) precisa apresentar ASV; portanto a propria
## marca `apresenteASV == "Apresente ASV"` ja identifica "CAR na base" no
## cruzamento com SICOR (nao mantemos um in_base_car redundante).
asvCar <- asvCar %>%
  mutate(
    cod_imovel    = str_remove_all(cod_imovel, "-"),
    apresenteASV  = "Apresente ASV"
  ) %>%
  select(cod_imovel, soma_desmat, uf, biome, criterio_new,
         apresenteASV, area_total_ha, m_fiscal) %>%
  mutate(quinzeMf = ifelse(m_fiscal >= 15, 1, 0)) %>%
  distinct(cod_imovel, .keep_all = TRUE)

## Fontes monitoradas: padroniza chave de join
fonteMonitor <- fonteMonitor %>%
  clean_names() %>%
  rename(cd_fonte_recurso = number_codigo) %>%
  mutate(monitored = 1)


# MERGE ------------------------------------------------------------------------

## (contrato, CAR) <-> dados do CAR (desmat, bioma, mf, status)
properties <- properties %>%
  left_join(asvCar, by = "cod_imovel")

gc()

## Marca contratos cuja fonte de recurso e' monitorada (controlado/direcionado)
df <- df %>%
  left_join(fonteMonitor, by = "cd_fonte_recurso") %>%
  mutate(monitored = ifelse(is.na(monitored), 0, 1))


# AGGREGATE: 1 linha por contrato (com flags do CAR) ---------------------------
# Para cada (ref_bacen, nu_ordem) contamos:
#   n_car_total    : numero de linhas em properties (qualquer cod_imovel)
#   n_car_id       : numero de CARs informados (cod_imovel != "-1")
#   n_car_in_base  : numero de CARs que cruzaram com lista_mcr (== asv_flag)
#   max_m_fiscal   : maior modulo fiscal entre os CARs do contrato
#   salvoBorda     : 1 se TODOS os CARs do contrato foram salvos pela borda

df_asv <- properties %>%
  filter(ref_bacen %in% df$ref_bacen) %>%
  select(ref_bacen, nu_ordem, apresenteASV, cod_imovel, criterio_new,
         quinzeMf, m_fiscal) %>%
  mutate(
    # asv_flag == 1 sse o CAR esta em asvCar; ele tambem indica "CAR na base"
    asv_flag    = ifelse(!is.na(apresenteASV) & apresenteASV == "Apresente ASV", 1, 0),
    haCAR       = ifelse(cod_imovel == "-1", 0, 1),
    salvoBorda  = ifelse(!is.na(criterio_new) & criterio_new == "Salvo pela borda", 1, 0)
  ) %>%
  group_by(ref_bacen, nu_ordem) %>%
  summarise(
    n_car_total    = n(),
    n_car_id       = sum(haCAR),
    n_car_in_base  = sum(asv_flag),
    quinzeMf       = sum(quinzeMf, na.rm = TRUE),
    max_m_fiscal   = suppressWarnings(max(m_fiscal, na.rm = TRUE)),
    salvoBorda     = ifelse(n_car_id == sum(salvoBorda), 1, 0),
    .groups = "drop"
  ) %>%
  mutate(max_m_fiscal = ifelse(is.infinite(max_m_fiscal), NA_real_, max_m_fiscal))

gc()


# JOIN no df principal e construcao das variaveis status_car / faixa_mf -------
df <- df %>%
  filter(
    ano_safra %in% c("2020/2021", "2021/2022", "2022/2023", "2023/2024",
                     "2024/2025")
  ) %>%
  left_join(df_asv, by = c("ref_bacen", "nu_ordem")) %>%
  mutate(
    # status_car: 4 categorias mutuamente exclusivas
    status_car = case_when(
      is.na(n_car_total)   ~ "Sigiloso",
      n_car_id == 0        ~ "CAR nao informado",
      n_car_in_base == 0   ~ "CAR fora da base",
      n_car_in_base > 0    ~ "CAR na base"
    ),

    # mantem o rotulo original "apresenteASV" para retrocompat com desciptive.R.
    # Para "CAR na base" todos os CARs em asvCar precisam apresentar ASV --
    # nao existe ramo "Sem desmatamento" porque n_car_in_base > 0 implica que
    # pelo menos um CAR esta em lista_mcr e portanto precisa de ASV.
    apresenteASV = case_when(
      status_car == "Sigiloso"            ~ "Dado sigiloso",
      status_car == "CAR nao informado"   ~ "Sem CAR associado",
      status_car == "CAR fora da base"    ~ "CAR fora da base",
      status_car == "CAR na base"         ~ "Apresente ASV"
    ),

    # faixa_mf: somente quando o CAR esta na base (caso contrario nao temos MF)
    faixa_mf = case_when(
      status_car != "CAR na base" ~ NA_character_,
      max_m_fiscal <  4           ~ "<4 MF",
      max_m_fiscal <  15          ~ "4-15 MF",
      max_m_fiscal >= 15          ~ ">=15 MF",
      TRUE                        ~ NA_character_
    )
  )


# Resumos rapidos para sanity check -------------------------------------------
summary_status_car <- df %>%
  group_by(ano_safra, status_car) %>%
  summarise(
    n_ops      = n(),
    credito_bi = sum(vl_parc_credito_real, na.rm = TRUE) / 1e9,
    .groups = "drop"
  )

print(summary_status_car)


# Marca CAR a CAR quem tomou credito (para a base asvCar)
unique.prop <- properties %>%
  select(cod_imovel, tookCredit) %>%
  distinct()

asvCar <- asvCar %>%
  left_join(unique.prop, by = "cod_imovel") %>%
  mutate(
    tookCredit = ifelse(is.na(tookCredit), "Nao afetado", tookCredit)
  )


# SAVE FILES -------------------------------------------------------------------
dir.create(file.path(root, "built"), showWarnings = FALSE)

saveRDS(asvCar,     file.path(root, "built", "asvCar_credit.Rds"))
saveRDS(df,         file.path(root, "built", "credit_asv.Rds"))
saveRDS(properties, file.path(root, "built", "properties_asv.Rds"))

# Samples em csv para inspecao
write.csv(asvCar,
          file.path(root, "built", "asvCar_credit.csv"),
          row.names = FALSE)

write.csv(df %>% arrange(-vl_parc_credito_real) %>% head(1000),
          file.path(root, "built", "credit_asv_sample.csv"),
          row.names = FALSE)

write.csv(properties %>% head(1000),
          file.path(root, "built", "properties_asv_sample.csv"),
          row.names = FALSE)

cat("[OK] tempo total:", format(Sys.time() - strt.time), "\n")

# =============================================================================
# 4_municipal_long_output.R
# -----------------------------------------------------------------------------
# Gera o output agregado em formato LONG (uma linha por combinacao das
# dimensoes), com chaves de agrupamento pedidas no item 4 da revisao do
# projeto:
#
#   * cd_municipio_ibge_cc  (municipio do SICOR)
#   * biome                 (bioma DOMINANTE do contrato, quando ha CAR na base)
#   * ano_safra
#   * status_car            (Sigiloso / CAR nao informado /
#                            CAR fora da base / CAR na base)
#   * cd_tipo_pessoa        (F: fisica, J: juridica)
#   * cd_fonte_recurso
#   * cd_programa
#   * cd_subprograma
#   * cd_modalidade
#   * faixa_mf              (<4 MF / 4-15 MF / >=15 MF; somente CAR na base)
#   * programa_fonte        (cd_programa se informado, senao cd_fonte_recurso)
#                           --> reflete a regra "o emprestimo segue a regra da
#                               fonte se nao tem programa"
#
# Metricas
# --------
#   * n_ops                  (numero de operacoes)
#   * vl_parc_credito        (R$ nominais)
#   * vl_parc_credito_real   (R$ dez/2025, deflacionado pelo IPCA)
#   * desmat_ha_proxy        (soma de soma_desmat dos CARs ligados ao contrato;
#                             COM SOBREPOSICAO -- o tratamento sem sobreposicao
#                             e' o item 6, ainda pendente)
#
# Inputs
# ------
# - built/credit_asv.Rds       (1 linha por contrato, com status_car/faixa_mf)
# - built/properties_asv.Rds   (link contrato <-> CAR + biome + area)
#
# Outputs
# -------
# - output/long/credit_long_municipal.csv
# - output/long/credit_long_municipal.xlsx
# =============================================================================

# SETUP ------------------------------------------------------------------------
rm(list = ls())
gc()

strt.time <- Sys.time()

library(tidyverse)
library(data.table)
library(openxlsx)

root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "baseMCR/dados")

options(scipen = 999)

output_dir <- file.path(root, "output", "long")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


# LOAD DATA --------------------------------------------------------------------
df         <- readRDS(file.path(root, "built", "credit_asv.Rds"))
properties <- readRDS(file.path(root, "built", "properties_asv.Rds"))


# BIOMA DOMINANTE POR CONTRATO -------------------------------------------------
# Quando o contrato tem CAR na base e os CARs estao em mais de um bioma,
# escolhemos o bioma "dominante" como aquele com a maior `area_total_ha`
# entre os CARs do contrato. Esse criterio reflete a relevancia espacial
# do CAR no contrato e e' replicavel a partir do shapefile.

bioma_dominante <- properties %>%
  filter(!is.na(biome), !is.na(area_total_ha)) %>%
  group_by(ref_bacen, nu_ordem) %>%
  arrange(desc(area_total_ha), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(ref_bacen, nu_ordem, biome_dominante = biome,
         n_biomas_contrato = area_total_ha)  # placeholder

# n_biomas_contrato como contagem real
n_biomas <- properties %>%
  filter(!is.na(biome)) %>%
  group_by(ref_bacen, nu_ordem) %>%
  summarise(
    n_biomas_contrato = n_distinct(biome),
    .groups = "drop"
  )

bioma_dominante <- bioma_dominante %>%
  select(-n_biomas_contrato) %>%
  left_join(n_biomas, by = c("ref_bacen", "nu_ordem"))


# DESMAT TOTAL POR CONTRATO (com sobreposicao) --------------------------------
# Soma simples do `soma_desmat` dos CARs do contrato. Se o CAR aparece em
# mais de um contrato, o mesmo desmat e' contado em todos. O tratamento
# sem sobreposicao e' o item 6, deixado para depois.
desmat_contrato <- properties %>%
  group_by(ref_bacen, nu_ordem) %>%
  summarise(
    desmat_ha_proxy = sum(soma_desmat, na.rm = TRUE),
    .groups = "drop"
  )


# JOIN no df principal e construcao do "programa_fonte" -----------------------
df <- df %>%
  left_join(bioma_dominante,  by = c("ref_bacen", "nu_ordem")) %>%
  left_join(desmat_contrato,  by = c("ref_bacen", "nu_ordem")) %>%
  mutate(
    # programa_fonte: usa cd_programa se informado, senao cd_fonte_recurso.
    # Padroniza como string para nao confundir com valores numericos.
    programa_fonte = ifelse(
      is.na(cd_programa) | cd_programa == "" | cd_programa == "0",
      paste0("FR_", cd_fonte_recurso),
      paste0("PR_", cd_programa)
    )
  )


# AGREGACAO LONG --------------------------------------------------------------
group_keys <- c(
  "ano_safra", "cd_municipio_ibge_cc", "biome_dominante",
  "status_car", "faixa_mf",
  "cd_tipo_pessoa",
  "cd_fonte_recurso", "cd_programa", "cd_subprograma", "cd_modalidade",
  "programa_fonte"
)

long_municipal <- df %>%
  group_by(across(all_of(group_keys))) %>%
  summarise(
    n_ops                  = n(),
    vl_parc_credito        = sum(vl_parc_credito,      na.rm = TRUE),
    vl_parc_credito_real   = sum(vl_parc_credito_real, na.rm = TRUE),
    desmat_ha_proxy        = sum(desmat_ha_proxy,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ano_safra, cd_municipio_ibge_cc)


# EXPORT ----------------------------------------------------------------------
fwrite(
  long_municipal,
  file.path(output_dir, "credit_long_municipal.csv")
)

wb <- createWorkbook()
addWorksheet(wb, "long_municipal")
writeData(wb, "long_municipal", long_municipal)
saveWorkbook(
  wb,
  file.path(output_dir, "credit_long_municipal.xlsx"),
  overwrite = TRUE
)

cat("[OK] linhas no long:", nrow(long_municipal), "\n")
cat("[OK] tempo total:",   format(Sys.time() - strt.time), "\n")

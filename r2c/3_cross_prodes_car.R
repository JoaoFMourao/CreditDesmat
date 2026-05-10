# =============================================================================
# 3_cross_prodes_car.R
# -----------------------------------------------------------------------------
# Antigo: INPEs_exersice.R
#
# Objetivo
# --------
# Cruzar a base de imoveis do CAR (lista_mcr_biomas_VF.gdb) com os poligonos
# do PRODES (prodes_mcr.gdb) e:
#   1. Aplicar a regra da "borda interna de 60m": desconsidera o desmatamento
#      que cai nos primeiros 60m a partir do limite do imovel.
#   2. Reclassificar cada CAR como "Apresente ASV" ou "Salvo pela borda" usando
#      o criterio de hectares por bioma (Amazonia 6.25 ha, Caatinga/Pampa/Mata
#      Atlantica 2 ha, Cerrado/Pantanal 5 ha).
#   3. Gerar o RDS principal usado pelo restante do pipeline.
#   4. Gerar um output de AUDITORIA que permite conferir se o desmatamento por
#      CAR calculado aqui bate com a coluna `soma_desmat` que ja vem na base
#      original (item 2 da revisao do projeto).
#
# Inputs
# ------
# - raw/lista_mcr_biomas_VF.gdb   (atualizado em mai/2026 via SharePoint MMA)
# - raw/prodes_mcr.gdb            (sem alteracao)
#
# Outputs
# -------
# - output/INPEs_exercise/changedImpact.rds                   (principal)
# - output/INPEs_exercise/audit_desmat_per_car.csv            (auditoria)
# - output/INPEs_exercise/audit_desmat_per_car_resumo.csv     (resumo)
#
# Observacoes
# -----------
# * O cruzamento espacial usa CRS 102033 (Albers Equal Area America do Sul),
#   compatibilizando areas em ha/km2.
# * st_use_s2(FALSE) e necessario porque o sf 1.x quebra em st_intersection
#   com geometrias muito grandes quando s2 esta ligado.
# * O calculo do desmat por CAR aqui (`desmat_calc_ha`) intersecta CAR x PRODES
#   completo, sem a borda. Isso e' a re-implementacao que confere se o
#   `soma_desmat` que vem na base ja esta certo.
# =============================================================================

# SET-UP -----------------------------------------------------------------------
rm(list = ls())
gc()

real.strt.time <- Sys.time()

library(sf)
library(dplyr)
library(geobr)
library(stringr)
library(ggplot2)
library(purrr)
library(readr)

# s2 desligado: evita falhas em st_intersection com geometrias grandes
sf::sf_use_s2(FALSE)

## paths --------------------------------------------------------------------
input  <- file.path("C:/Users", Sys.getenv("USERNAME"),
                    "Documents", "baseMCR", "dados", "raw")
output <- file.path("C:/Users", Sys.getenv("USERNAME"),
                    "Documents", "baseMCR", "dados", "output", "INPEs_exercise")

if (!dir.exists(output)) {
  dir.create(output, recursive = TRUE)
}

gdb_1 <- file.path(input, "lista_mcr_biomas_VF.gdb")  # CAR
gdb_2 <- file.path(input, "prodes_mcr.gdb")           # PRODES (desmatamento)


# LOAD DATA --------------------------------------------------------------------

## explora layers (debug)
layers_gdb_1 <- st_layers(gdb_1)
layers_gdb_2 <- st_layers(gdb_2)

print(layers_gdb_1)
print(layers_gdb_2$name)

## CAR: empilha todos os layers de bioma e reprojeta
lista_mcr <- lapply(layers_gdb_1$name, function(x) {
  st_read(dsn = gdb_1, layer = x, quiet = FALSE)
}) %>%
  bind_rows() %>%
  st_transform(102033) %>%
  st_make_valid()

## PRODES: empilha todos os layers (anos) e reprojeta
prodes_list <- lapply(layers_gdb_2$name, function(x) {
  st_read(dsn = gdb_2, layer = x, quiet = FALSE)
})

prodes_all <- bind_rows(prodes_list) %>%
  st_transform(102033) %>%
  st_make_valid()


# DATA HANDLING ----------------------------------------------------------------

## remove CARs duplicados na base CAR (mesmo cod_imovel em mais de um layer)
lista_mcr <- lista_mcr %>%
  distinct(cod_imovel, .keep_all = TRUE)

## seleciona variaveis e filtra status valido
# - status_imo == "SU"        => imovel suspenso, desconsidera
# - condicao "Cancelado..."   => CAR cancelado por decisao administrativa
impacted.properties_org <- lista_mcr %>%
  select(cod_imovel, soma_desmat, dentro_criterio, status_imo,
         criterio_aplicado, tipo_imove, uf, municipio,
         cod_munici, condicao, area_total_ha, m_fiscal) %>%
  filter(
    status_imo != "SU",
    condicao   != "Cancelado por decisão administrativa"
  ) %>%
  st_make_valid()


# AUDITORIA: desmat por CAR via re-intersecao CAR x PRODES (item 2) ------------
# Aqui calculamos por conta propria o desmatamento total dentro de cada CAR
# fazendo st_intersection(CAR, PRODES) e somando area por cod_imovel.
# Esse numero deve ser proximo do `soma_desmat` que ja vem na base.
# Diferencas grandes indicam:
#   - mudanca na malha PRODES desde o calculo original
#   - geometrias invalidas
#   - filtros distintos no calculo original do MMA

audit.strt <- Sys.time()

audit_calc <- impacted.properties_org %>%
  st_intersection(prodes_all %>% select(Shape)) %>%
  mutate(area_ha = as.numeric(st_area(Shape)) / 10000) %>%
  as_tibble() %>%
  group_by(cod_imovel) %>%
  summarise(
    desmat_calc_ha = sum(area_ha, na.rm = TRUE),
    .groups = "drop"
  )

cat("[AUDIT] tempo de re-intersecao:",
    format(Sys.time() - audit.strt), "\n")

audit_df <- impacted.properties_org %>%
  as_tibble() %>%
  select(cod_imovel, uf, municipio, biome = criterio_aplicado,
         soma_desmat_original = soma_desmat) %>%
  left_join(audit_calc, by = "cod_imovel") %>%
  mutate(
    desmat_calc_ha = ifelse(is.na(desmat_calc_ha), 0, desmat_calc_ha),
    diff_ha        = desmat_calc_ha - soma_desmat_original,
    diff_pct       = ifelse(
      soma_desmat_original > 0,
      100 * diff_ha / soma_desmat_original,
      NA_real_
    )
  )

# CSV completo (1 linha por CAR)
write_csv(
  audit_df,
  file.path(output, "audit_desmat_per_car.csv")
)

# Resumo agregado para inspecao rapida
audit_resumo <- audit_df %>%
  summarise(
    n_cars               = n(),
    soma_original_ha     = sum(soma_desmat_original, na.rm = TRUE),
    soma_calc_ha         = sum(desmat_calc_ha, na.rm = TRUE),
    diff_total_ha        = sum(diff_ha, na.rm = TRUE),
    pct_cars_com_diferenca = mean(abs(diff_ha) > 0.01, na.rm = TRUE) * 100,
    pct_diff_total       = 100 * sum(diff_ha, na.rm = TRUE) /
                                 sum(soma_desmat_original, na.rm = TRUE)
  )

write_csv(
  audit_resumo,
  file.path(output, "audit_desmat_per_car_resumo.csv")
)

print(audit_resumo)


# REGRA DA BORDA INTERNA DE 60M ------------------------------------------------
# A nova regra do MCR exclui o desmatamento que ocorre nos primeiros 60m a
# partir do limite externo do imovel. Implementamos isso via:
#   borda = imovel - buffer(imovel, -60)
# e cruzamos a borda com PRODES para descontar essa parcela do total.

borda_60m <- impacted.properties_org %>%
  mutate(
    Shape = map2(Shape, st_buffer(Shape, -60), st_difference) %>%
      st_sfc(crs = st_crs(impacted.properties_org))
  )

## cruza PRODES com a faixa de 60m (borda)
str.date <- Sys.time()

bordaProdes <- borda_60m %>%
  st_intersection(prodes_all) %>%
  mutate(
    area_ha_naBorda = as.numeric(st_area(Shape)) / 10000
  ) %>%
  as_tibble() %>%
  group_by(cod_imovel) %>%
  summarise(
    desmatBorda_ha = sum(area_ha_naBorda, na.rm = TRUE),
    .groups = "drop"
  )

cat("[BORDA] tempo de cruzamento:",
    format(Sys.time() - str.date), "\n")


# RECLASSIFICACAO POR CRITERIO DO BIOMA ----------------------------------------
# Threshold em hectares por bioma (regra do MCR):
#   AMAZONIA           => 6.25 ha
#   CAATINGA / PAMPA / MATA ATLANTICA => 2 ha
#   CERRADO / PANTANAL => 5 ha

changedImpact <- impacted.properties_org %>%
  as_tibble() %>%
  select(-Shape) %>%
  left_join(bordaProdes, by = "cod_imovel") %>%
  mutate(
    desmatBorda_ha = ifelse(is.na(desmatBorda_ha), 0, desmatBorda_ha),
    desmatNew      = soma_desmat - desmatBorda_ha
  ) %>%
  mutate(
    biome = criterio_aplicado,

    criterio_aplicado = case_when(
      criterio_aplicado == "AMAZÔNIA" ~ 6.25,
      str_detect(criterio_aplicado, "CAATINGA|PAMPA|MATA ATLÂNTICA") ~ 2,
      str_detect(criterio_aplicado, "CERRADO|PANTANAL") ~ 5
    ),

    criterio_new = ifelse(
      desmatNew < criterio_aplicado,
      "Salvo pela borda",
      "Apresente ASV"
    )
  )


# SAVE -------------------------------------------------------------------------
saveRDS(
  changedImpact,
  file.path(output, "changedImpact.rds")
)

cat("[OK] tempo total:", format(Sys.time() - real.strt.time), "\n")

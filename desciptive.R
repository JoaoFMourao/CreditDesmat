################################################################################
# desciptive.R
# -----------------------------------------------------------------------------
# Analise descritiva final do projeto. Le os RDS construidos no passo
# `built/sicor_per_car.R` (asvCar_credit, credit_asv) e gera tabelas Excel,
# CSVs e graficos PNG cobrindo as analises 1 a 9:
#   1) tamanho da base CAR x CAR Brasil
#   2) borda 60m: "Apresente ASV" vs "Salvo pela borda"
#   3) credito monitorado por ano-safra
#   4) dentro do monitorado, situacao do CAR
#   5) quanto dos CARs da nossa base tomam credito
#   6) ticket medio por grupo (e por modulo fiscal)
#   7) credito que NAO sera afetado pela nova norma
#   8) operacoes afetadas pela nova norma (universo observavel)
#   9) detalhamentos por bioma/UF
#
# Outputs sao escritos em <root>/output/analysis_asv_credit/.
################################################################################

# SETUP ------------------------------------------------------------------------
# limpa ambiente e memoria
rm(list=ls())
gc()

# marca tempo inicial
strt.time <- Sys.time()

# carrega bibliotecas
library(tidyverse)
library(janitor)
library(data.table)
library(lubridate)
library(openxlsx)
library(flextable)
library(scales)

# define caminho raiz
root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "baseMCR/dados")

output_dir <- file.path(root,"output","analysis_asv_credit")

plot_dir <- file.path(output_dir, "plots")
table_dir <- file.path(output_dir, "tables")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)

# evita notacao cientifica
options(scipen = 999)

# define pastas de input e output
input_dir  <- file.path(root, "built")
output_dir <- file.path(root, "output", "analysis_asv_credit")

# cria pasta de output se necessario
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# LOAD DATA --------------------------------------------------------------------

# carrega bases completas em RDS
asvCar     <- readRDS(file.path(input_dir, "asvCar_credit.Rds"))
credit_asv <- readRDS(file.path(input_dir, "credit_asv.Rds"))
df <- readRDS(file.path(root,"built", "credit_asv.Rds"))

# carrega base do CAR Brasil (csv sem area)
car_brasil <- fread(file.path(root,"raw","dados_car_brasil.csv"),
                    encoding = "UTF-8")

# CLEAN + UF -------------------------------------------------------------------
# objetivo: padronizar nomes, criar UF e imputar area manualmente

car_brasil <- car_brasil %>%
  rename(
    estado = Estado,
    n_car = `Nº Registros CAR`
  ) %>%
  mutate(
    # cria UF a partir do nome do estado
    uf = case_when(
      estado == "Acre" ~ "AC",
      estado == "Alagoas" ~ "AL",
      estado == "Amazonas" ~ "AM",
      estado == "Bahia" ~ "BA",
      estado == "Distrito Federal" ~ "DF",
      estado == "Goias" ~ "GO",
      estado == "Mato Grosso" ~ "MT",
      estado == "Mato Grosso do Sul" ~ "MS",
      estado == "Minas Gerais" ~ "MG",
      estado == "Pernambuco" ~ "PE",
      estado == "Rio de Janeiro" ~ "RJ",
      estado == "Rio Grande do Norte" ~ "RN",
      estado == "Rio Grande do Sul" ~ "RS",
      estado == "Roraima" ~ "RR",
      estado == "Santa Catarina" ~ "SC",
      estado == "Sergipe" ~ "SE",
      estado == "Tocantins" ~ "TO",
      estado == "Amapá" ~ "AP",
      estado == "Ceará" ~ "CE",
      estado == "Espírito Santo" ~ "ES",
      estado == "Maranhão" ~ "MA",
      estado == "Pará" ~ "PA",
      n_car == 197645 ~ "PB", # tratamento especifico para PB
      estado == "Paraná" ~ "PR",
      estado == "Piauí" ~ "PI",
      estado == "Rondônia" ~ "RO",
      estado == "São Paulo" ~ "SP"
    ),
    
    # imputacao manual da area em Mha
    area_mha = case_when(
      uf == "AC" ~ 7.3,
      uf == "AL" ~ 2.3,
      uf == "AM" ~ 20.2,
      uf == "BA" ~ 34.8,
      uf == "DF" ~ 0.7,
      uf == "GO" ~ 32.0,
      uf == "MT" ~ 76.9,
      uf == "MS" ~ 34.1,
      uf == "MG" ~ 53.8,
      uf == "PE" ~ 6.8,
      uf == "RJ" ~ 2.5,
      uf == "RN" ~ 3.7,
      uf == "RS" ~ 23.8,
      uf == "RR" ~ 2.9,
      uf == "SC" ~ 8.0,
      uf == "SE" ~ 1.7,
      uf == "TO" ~ 19.7,
      uf == "AP" ~ 2.3,
      uf == "CE" ~ 10.6,
      uf == "ES" ~ 3.8,
      uf == "MA" ~ 27.8,
      uf == "PA" ~ 39.6,
      uf == "PB" ~ 4.2,
      uf == "PR" ~ 18.2,
      uf == "PI" ~ 20.5,
      uf == "RO" ~ 9.3,
      uf == "SP" ~ 21.0
    ),
    
    # converte de Mha para ha
    area_ha = area_mha * 1e6
  ) %>%
  select(uf, n_car, area_ha)

# CHECK
car_brasil

# 1) Objetivo: medir o tamanho da base em relacao ao CAR Brasil ####
## data prep ####

# tabela agregada Brasil
tab_brasil <- tibble(
  n_properties_base = n_distinct(asvCar$cod_imovel),
  area_base_mha = sum(asvCar$area_total_ha, na.rm = TRUE) / 1e6,
  n_properties_brasil = sum(car_brasil$n_car, na.rm = TRUE),
  area_brasil_mha = sum(car_brasil$area_ha, na.rm = TRUE) / 1e6,
  desmat_base_km2 = sum(asvCar$soma_desmat, na.rm = TRUE) / 100
)

# tabela por UF
tab_uf <- asvCar %>%
  group_by(uf) %>%
  summarise(
    n_properties_base = n_distinct(cod_imovel),
    area_base_mha = sum(area_total_ha, na.rm = TRUE) / 1e6,
    desmat_base_km2 = sum(soma_desmat, na.rm = TRUE) / 100,
    .groups = "drop"
  ) %>%
  left_join(car_brasil, by = "uf") %>%
  mutate(
    area_brasil_mha = area_ha / 1e6
  ) %>%
  select(
    uf,
    n_properties_base,
    area_base_mha,
    n_car,
    area_brasil_mha,
    desmat_base_km2
  )

# arredonda numeros para facilitar leitura
tab_brasil <- tab_brasil %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

tab_uf <- tab_uf %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

## export data #### ----------------------------------------------------------------

# versao amigavel para Excel (nomes bonitos)
tab_brasil_export <- tab_brasil %>%
  rename(
    "Numero de imoveis na base" = n_properties_base,
    "Area da base (Mha)" = area_base_mha,
    "Numero de imoveis no CAR Brasil" = n_properties_brasil,
    "Area total do CAR Brasil (Mha)" = area_brasil_mha,
    "Desmatamento na base (km2)" = desmat_base_km2
  )

tab_uf_export <- tab_uf %>%
  rename(
    "UF" = uf,
    "Numero de imoveis na base" = n_properties_base,
    "Area da base (Mha)" = area_base_mha,
    "Numero de imoveis no CAR Estado" = n_car,
    "Area total do CAR Estado (Mha)" = area_brasil_mha,
    "Desmatamento na base (km2)" = desmat_base_km2
  )

# salva arquivos Excel usando openxlsx
wb <- createWorkbook()

addWorksheet(wb, "Brasil")
writeData(wb, "Brasil", tab_brasil_export)

addWorksheet(wb, "UF")
writeData(wb, "UF", tab_uf_export)

saveWorkbook(
  wb,
  file.path(output_dir, "tamanho_base.xlsx"),
  overwrite = TRUE
)

# salva tambem em CSV
fwrite(tab_brasil,
       file.path(output_dir, "tamanho_base_brasil.csv"))

fwrite(tab_uf,
       file.path(output_dir, "tamanho_base_uf.csv"))

# 2) BORDA: dentro da base, separar "Apresente ASV" de "Salvo pela borda" ####

# aqui assumo que a classificacao final de borda esta em criterio_new
# e que ela distingue "Apresente ASV" de "Salvo pela borda"
# se o nome exato dos rotulos estiver um pouco diferente, ajuste so os strings

tab_borda_brasil <- asvCar %>%
  group_by(criterio_new) %>%
  summarise(
    n_properties = n_distinct(cod_imovel),
    area_mha = sum(area_total_ha, na.rm = TRUE) / 1e6,
    desmat_km2 = sum(soma_desmat, na.rm = TRUE) / 100,
    .groups = "drop"
  )

tab_borda_brasil_total <- tab_borda_brasil %>%
  summarise(
    criterio_new = "Total",
    n_properties = sum(n_properties, na.rm = TRUE),
    area_mha = sum(area_mha, na.rm = TRUE),
    desmat_km2 = sum(desmat_km2, na.rm = TRUE)
  )

tab_borda_brasil <- bind_rows(tab_borda_brasil, tab_borda_brasil_total) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

tab_borda_uf <- asvCar %>%
  mutate(
    criterio_new = case_when(
      criterio_new %in% c("Apresente ASV", "apresente asv") ~ "Apresente ASV",
      TRUE ~ "Salvo pela borda"
    )
  ) %>%
  group_by(uf, criterio_new) %>%
  summarise(
    n_properties = n_distinct(cod_imovel),
    area_mha = sum(area_total_ha, na.rm = TRUE) / 1e6,
    desmat_km2 = sum(soma_desmat, na.rm = TRUE) / 100,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

tab_borda_brasil_export <- tab_borda_brasil %>%
  rename(
    "Criterio" = criterio_new,
    "Numero de imoveis" = n_properties,
    "Area (Mha)" = area_mha,
    "Desmatamento (km2)" = desmat_km2
  )

tab_borda_uf_export <- tab_borda_uf %>%
  rename(
    "UF" = uf,
    "Criterio" = criterio_new,
    "Numero de imoveis" = n_properties,
    "Area (Mha)" = area_mha,
    "Desmatamento (km2)" = desmat_km2
  )

# tabela bonita para ppt
ft_borda_brasil <- flextable(tab_borda_brasil_export) %>%
  theme_vanilla() %>%
  autofit()

save_as_image(
  ft_borda_brasil,
  path = file.path(table_dir, "tab_borda_brasil.png")
)

# excel
wb2 <- createWorkbook()
addWorksheet(wb2, "Brasil")
writeData(wb2, "Brasil", tab_borda_brasil_export)
addWorksheet(wb2, "UF")
writeData(wb2, "UF", tab_borda_uf_export)
saveWorkbook(
  wb2,
  file.path(output_dir, "analise_2_borda.xlsx"),
  overwrite = TRUE
)

fwrite(tab_borda_brasil, file.path(output_dir, "analise_2_borda_brasil.csv"))
fwrite(tab_borda_uf, file.path(output_dir, "analise_2_borda_uf.csv"))

# 3) CREDITO MONITORADO POR ANO SAFRA ####
## data prep ####
tab_credit_monitor <- credit_asv %>%
  mutate(
    monitoramento = case_when(
      monitored == 1 ~ "Credito Controlado ou Direcionado",
      TRUE ~ "Resto do Credito"
    )
  ) %>%
  group_by(ano_safra, monitoramento) %>%
  summarise(
    credito_bi = sum(vl_parc_credito_real, na.rm = TRUE) / 1e9,
    opsM = n()/10^3,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

tab_credit_monitor_export <- tab_credit_monitor %>%
  rename(
    "Ano safra" = ano_safra,
    "Grupo" = monitoramento,
    "Credito (R$ bi, reais corrigidos)" = credito_bi,
    "Mil Operações" = opsM
  )

## credit ####
g_credit_monitor <- ggplot(
  tab_credit_monitor,
  aes(x = ano_safra, y = credito_bi, color = monitoramento, group = monitoramento)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  
  geom_label(
    aes(label = paste0("R$ ", round(credito_bi), "B")),
    size = 3,
    vjust = -0.5,
    linewidth = 0,
    show.legend = FALSE
  ) +
  
  labs(
    x = "Ano safra",
    y = "Credito (R$ bi)",
    color = NULL,
    title = "Credito Controlado ou Direcionado e Resto do Credito"
  ) +
  
  coord_cartesian(ylim = c(0, 410)) +
  
  scale_color_manual(
    values = c(
      "Credito Controlado ou Direcionado" = "#1B3A4B",
      "Resto do Credito" = "#8B2E2E"
    )
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    axis.title = element_text(size = 13),
    axis.text  = element_text(size = 11),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    
    plot.margin = margin(10, 10, 10, 10)
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 2,
        shape = NA,
        fill = NA
      )
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_credit_monitorado.png"),
  plot = g_credit_monitor,
  width = 9,
  height = 5
)

## Operacoes ####
g_ops_monitor <- ggplot(
  tab_credit_monitor,
  aes(x = ano_safra, y = opsM, color = monitoramento, group = monitoramento)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  
  geom_label(
    aes(label = paste0(round(opsM), " M")),
    size = 3,
    vjust = -0.5,
    linewidth = 0,
    show.legend = FALSE
  ) +
  
  labs(
    x = "Ano safra",
    y = "Mil Operações",
    color = NULL,
    title = "Credito Controlado ou Direcionado e Resto do Credito"
  ) +
  
  coord_cartesian(ylim = c(0, 2400)) +
  
  scale_color_manual(
    values = c(
      "Credito Controlado ou Direcionado" = "#1B3A4B",
      "Resto do Credito" = "#8B2E2E"
    )
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    axis.title = element_text(size = 13),
    axis.text  = element_text(size = 11),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    
    plot.margin = margin(10, 10, 10, 10)
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 2,
        shape = NA,
        fill = NA
      )
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_ops_monitorado.png"),
  plot = g_ops_monitor,
  width = 9,
  height = 5
)
## Save Workbook ####
wb3 <- createWorkbook()
addWorksheet(wb3, "Credito monitorado")
writeData(wb3, "Credito monitorado", tab_credit_monitor_export)
saveWorkbook(
  wb3,
  file.path(output_dir, "analise_3_credito_monitorado.xlsx"),
  overwrite = TRUE
)

fwrite(tab_credit_monitor, file.path(output_dir, "analise_3_credito_monitorado.csv"))

# 4) DENTRO DO MONITORADO, SEPARAR O QUE TEM CAR VISIVEL NA BASE PUBLICA ####

## Data Prep ####
tab_credit_basic <- credit_asv %>%
  filter(monitored == 1) %>%
  mutate(
    grupo = case_when(
      apresenteASV == "Dado sigiloso" ~ "Dado Sigiloso",
      apresenteASV == "Sem desmatamento" ~ "Não precisaria apresentar ASV",
      apresenteASV == "Apresente ASV" ~ "precisaria apresentar ASV",
      apresenteASV == "Sem CAR associado" ~ "CAR não informado"
    )
  ) %>%
  group_by(ano_safra, grupo) %>%
  summarise(
    credito_bi = sum(vl_parc_credito_real, na.rm = TRUE) / 1e9,
    opsM = n()/10^3,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
  mutate(
    grupo = factor(
      grupo,
      levels = c(
        "Dado Sigiloso",
        "CAR nao informado",
        "CAR sem exigencia de ASV",
        "Apresente ASV"
      )
    ),
    vjust_pos = case_when(
      grupo == "Apresente ASV" ~ -1.2,
      grupo == "CAR nao informado" ~ 1.5,
      TRUE ~ -0.6
    )
  )

cores_mma <- c(
  "Dado Sigiloso" = "#264653",
  "CAR nao informado" = "#2A9D8F",
  "CAR sem exigencia de ASV" = "#6A994E",
  "Apresente ASV" = "#9B2226"
)

## Credito ####
g_credit_basic <- ggplot(
  tab_credit_basic,
  aes(x = ano_safra, y = credito_bi, color = grupo, group = grupo)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  
  geom_text(
    aes(
      label = paste0("R$ ", round(credito_bi), "B"),
      vjust = vjust_pos
    ),
    size = 3,
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = cores_mma) +
  
  labs(
    x = "Ano safra",
    y = "Credito (R$ bi)",
    color = NULL,
    title = "Credito Controlado ou Direcionado por situacao do CAR"
  ) +
  
  coord_cartesian(
    ylim = c(0, max(tab_credit_basic$credito_bi) * 1.15)
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    
    axis.title = element_text(size = 13),
    axis.text  = element_text(size = 11),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 2,
        shape = NA
      )
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_credit_ApresenteASV.png"),
  plot = g_credit_basic,
  width = 9,
  height = 5
)

### Operacoes ####
g_ops_basic <- ggplot(
  tab_credit_basic,
  aes(x = ano_safra, y = opsM, color = grupo, group = grupo)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  
  geom_text(
    aes(
      label = paste0(round(opsM), " M"),
      vjust = vjust_pos
    ),
    size = 3,
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = cores_mma) +
  
  labs(
    x = "Ano safra",
    y = "Mil Operações",
    color = NULL,
    title = "Credito Controlado ou Direcionado por situacao do CAR"
  ) +
  
  coord_cartesian(
    ylim = c(0, max(tab_credit_basic$opsM) * 1.15)
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    
    axis.title = element_text(size = 13),
    axis.text  = element_text(size = 11),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 2,
        shape = NA
      )
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_ops_ApresenteASV.png"),
  plot = g_ops_basic,
  width = 9,
  height = 5
)

### Operacoes - versao simplificada sem dado sigiloso ####

tab_ops_basic_clean <- tab_credit_basic %>%
  filter(grupo != "Dado Sigiloso") %>%
  mutate(
    grupo_clean = case_when(
      grupo == "CAR nao informado" ~ "CAR Não Informado",
      grupo == "CAR sem exigencia de ASV" ~ "Não precisaria apresentar ASV",
      grupo == "Apresente ASV" ~ "Precisaria apresentar ASV"
    ),
    grupo_clean = factor(
      grupo_clean,
      levels = c(
        "CAR Não Informado",
        "Não precisaria apresentar ASV",
        "Precisaria apresentar ASV"
      )
    ),
    vjust_pos = case_when(
      grupo_clean == "Precisaria apresentar ASV" ~ -1.2,
      grupo_clean == "CAR Não Informado" ~ 1.5,
      TRUE ~ -0.6
    )
  )

cores_mma_clean <- c(
  "CAR Não Informado" = "#2A9D8F",
  "Não precisaria apresentar ASV" = "#6A994E",
  "Precisaria apresentar ASV" = "#9B2226"
)

g_ops_ApresenteASV_clean <- ggplot(
  tab_ops_basic_clean,
  aes(
    x = ano_safra,
    y = opsM,
    color = grupo_clean,
    group = grupo_clean
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  
  geom_text(
    aes(
      label = paste0(round(opsM), " M"),
      vjust = vjust_pos
    ),
    size = 3,
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = cores_mma_clean) +
  
  labs(
    x = "Ano safra",
    y = "Mil Operações",
    color = NULL,
    title = "Operações por situação frente à exigência de ASV"
  ) +
  
  coord_cartesian(
    ylim = c(0, max(tab_ops_basic_clean$opsM) * 1.15)
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(size = 13),
    axis.text  = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 2,
        shape = NA
      )
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_ops_ApresenteASV_clean.png"),
  plot = g_ops_ApresenteASV_clean,
  width = 9,
  height = 5
)

### Save Workbook ####
wb4 <- createWorkbook()
addWorksheet(wb4, "Credito")
writeData(wb4, "Credito", tab_credit_basic)
saveWorkbook(
  wb4,
  file.path(output_dir, "analise_4_credito_monitorado.xlsx"),
  overwrite = TRUE
)

# 5) QUANTO DOS CARs DA NOSSA BASE TOMAM CREDITO ####

## Brasil ####
tab_credit_flag_brasil <- asvCar %>%
  mutate(
    tookCredit = case_when(
      tookCredit == "Tomou Credito" ~ "Tomou credito",
      TRUE ~ "Nao tomou credito"
    )
  ) %>%
  group_by(tookCredit) %>%
  summarise(
    n_properties_base = n_distinct(cod_imovel),
    area_base_mha = sum(area_total_ha, na.rm = TRUE) / 1e6,
    desmat_base_km2 = sum(soma_desmat, na.rm = TRUE) / 100,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)),
         label_text = paste0(
           scales::label_number(big.mark = ".", decimal.mark = ",")(n_properties_base),
           " Cadastros",
           "\n",
           round(area_base_mha, 1), " Mha em área"
         )
  )

## UF ####
tab_credit_flag_uf <- asvCar %>%
  mutate(
    tookCredit = case_when(
      tookCredit == "Tomou Credito" ~ "Tomou credito",
      TRUE ~ "Nao tomou credito"
    )
  ) %>%
  group_by(uf, tookCredit) %>%
  summarise(
    n_properties_base = n_distinct(cod_imovel),
    area_base_mha = sum(area_total_ha, na.rm = TRUE) / 1e6,
    desmat_base_km2 = sum(soma_desmat, na.rm = TRUE) / 100,
    .groups = "drop"
  ) %>%
  left_join(car_brasil, by = "uf") %>%
  mutate(
    area_brasil_mha = area_ha / 1e6
  ) %>%
  select(
    uf,
    tookCredit,
    n_properties_base,
    area_base_mha,
    n_car,
    area_brasil_mha,
    desmat_base_km2
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

## EXPORT ####
tab_credit_flag_brasil_export <- tab_credit_flag_brasil %>%
  rename(
    "Grupo" = tookCredit,
    "Numero de imoveis na base" = n_properties_base,
    "Area da base (Mha)" = area_base_mha,
    "Desmatamento na base (km2)" = desmat_base_km2
  )

tab_credit_flag_uf_export <- tab_credit_flag_uf %>%
  rename(
    "UF" = uf,
    "Grupo" = tookCredit,
    "Numero de imoveis na base" = n_properties_base,
    "Area da base (Mha)" = area_base_mha,
    "Numero de imoveis no CAR Brasil" = n_car,
    "Area total do CAR Brasil (Mha)" = area_brasil_mha,
    "Desmatamento na base (km2)" = desmat_base_km2
  )

## CORES ####
cores_credito <- c(
  "Tomou credito" = "#1B3A4B",
  "Nao tomou credito" = "#9B2226"
)

## GRAFICO ####
g_credit_flag <- ggplot(
  tab_credit_flag_brasil,
  aes(x = tookCredit, y = desmat_base_km2, fill = tookCredit)
) +
  geom_col(width = 0.6) +
  
  geom_text(
    aes(label = label_text),
    color = "white",
    size = 3.5,
    vjust = 1.5
  ) +
  
  geom_text(
    aes(label = paste0(round(desmat_base_km2), " km²")),
    vjust = -0.6,
    size = 3
  ) +
  
  scale_fill_manual(values = cores_credito) +
  
  labs(
    x = "",
    y = "Desmatamento (km2)",
    fill = NULL,
    title = "Desmatamento por acesso ao credito"
  ) +
  
  coord_cartesian(
    ylim = c(0, max(tab_credit_flag_brasil$desmat_base_km2) * 1.15)
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(size = 13),
    axis.text  = element_text(size = 11),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  filename = file.path(plot_dir, "g_base_tookCredit.png"),
  plot = g_credit_flag,
  width = 8,
  height = 5
)

## SAVE ####
wb5 <- createWorkbook()

addWorksheet(wb5, "Brasil")
writeData(wb5, "Brasil", tab_credit_flag_brasil_export)

addWorksheet(wb5, "UF")
writeData(wb5, "UF", tab_credit_flag_uf_export)

saveWorkbook(
  wb5,
  file.path(output_dir, "analise_5_base_tookCredit.xlsx"),
  overwrite = TRUE
)

# 6) VALOR MEDIO DO CONTRATO POR GRUPO ####

## Data prep ####
tab_ticket_medio <- credit_asv %>%
  mutate(
    grupo = case_when(
      monitored == 0 ~ "Resto do Credito",
      monitored == 1 & apresenteASV == "Dado sigiloso" ~ "Dado Sigiloso",
      monitored == 1 & apresenteASV == "Sem CAR associado" ~ "CAR nao informado",
      monitored == 1 & apresenteASV == "Sem desmatamento" ~ "CAR sem exigencia de ASV",
      monitored == 1 & apresenteASV == "Apresente ASV" ~ "Apresente ASV"
    )
  ) %>%
  group_by(ano_safra, grupo) %>%
  summarise(
    credito_bi = sum(vl_parc_credito_real, na.rm = TRUE) / 1e9,
    n_operacoes = n(),
    ticket_medio_mil = sum(vl_parc_credito_real, na.rm = TRUE) / n() / 10^3,
    .groups = "drop"
  ) %>%
  mutate(
    grupo = factor(
      grupo,
      levels = c(
        "Apresente ASV",
        "CAR sem exigencia de ASV",
        "CAR nao informado",
        "Dado Sigiloso",
        "Resto do Credito"
      )
    ),
    across(c(credito_bi, ticket_medio_mil), ~ round(.x, 2))
  )

tab_ticket_medio_total <- credit_asv %>%
  mutate(
    grupo = case_when(
      monitored == 0 ~ "Resto do Credito",
      monitored == 1 & apresenteASV == "Dado sigiloso" ~ "Dado Sigiloso",
      monitored == 1 & apresenteASV == "Sem CAR associado" ~ "CAR nao informado",
      monitored == 1 & apresenteASV == "Sem desmatamento" ~ "CAR sem exigencia de ASV",
      monitored == 1 & apresenteASV == "Apresente ASV" ~ "Apresente ASV"
    )
  ) %>%
  group_by(grupo) %>%
  summarise(
    credito_bi = sum(vl_parc_credito_real, na.rm = TRUE) / 1e9,
    n_operacoes = n(),
    ticket_medio_mil = sum(vl_parc_credito_real, na.rm = TRUE) / n() / 10^3,
    .groups = "drop"
  ) %>%
  mutate(
    grupo = factor(
      grupo,
      levels = c(
        "Apresente ASV",
        "CAR sem exigencia de ASV",
        "CAR nao informado",
        "Dado Sigiloso",
        "Resto do Credito"
      )
    ),
    across(c(credito_bi, ticket_medio_mil), ~ round(.x, 2))
  )

## Export ####
tab_ticket_medio_export <- tab_ticket_medio %>%
  rename(
    "Ano safra" = ano_safra,
    "Grupo" = grupo,
    "Credito (R$ bi)" = credito_bi,
    "Numero de operacoes" = n_operacoes,
    "Valor medio do contrato (R$ mil)" = ticket_medio_mil
  )

tab_ticket_medio_total_export <- tab_ticket_medio_total %>%
  rename(
    "Grupo" = grupo,
    "Credito (R$ bi)" = credito_bi,
    "Numero de operacoes" = n_operacoes,
    "Valor medio do contrato (R$ mil)" = ticket_medio_mil
  )

## Cores ####
cores_ticket <- c(
  "Apresente ASV" = "#9B2226",
  "CAR sem exigencia de ASV" = "#6A994E",
  "CAR nao informado" = "#2A9D8F",
  "Dado Sigiloso" = "#264653",
  "Resto do Credito" = "#6D597A"
)

## Grafico ####
g_ticket_medio <- ggplot(
  tab_ticket_medio,
  aes(x = ano_safra, y = ticket_medio_mil, color = grupo, group = grupo)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(label = paste0("R$ ", round(ticket_medio_mil), " mil")),
    size = 2.8,
    vjust = -0.6,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cores_ticket) +
  labs(
    x = "Ano safra",
    y = "Valor medio do contrato (R$ mil)",
    color = NULL,
    title = "Valor medio do contrato por grupo"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_ticket_medio$ticket_medio_mil) * 1.15)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 2,
        shape = NA
      )
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_ticket_medio_grupos.png"),
  plot = g_ticket_medio,
  width = 10,
  height = 5.5
)

## Tabela bonita para slide ####
ft_ticket_medio <- flextable(tab_ticket_medio_total_export) %>%
  theme_vanilla() %>%
  autofit()

save_as_image(
  ft_ticket_medio,
  path = file.path(table_dir, "tab_ticket_medio_grupos.png")
)

## Save workbook ####
wb6 <- createWorkbook()

addWorksheet(wb6, "Ano_safra")
writeData(wb6, "Ano_safra", tab_ticket_medio_export)

addWorksheet(wb6, "Total")
writeData(wb6, "Total", tab_ticket_medio_total_export)

saveWorkbook(
  wb6,
  file.path(output_dir, "analise_6_ticket_medio.xlsx"),
  overwrite = TRUE
)

fwrite(
  tab_ticket_medio,
  file.path(output_dir, "analise_6_ticket_medio.csv")
)

fwrite(
  tab_ticket_medio_total,
  file.path(output_dir, "analise_6_ticket_medio_total.csv")
)


### 6.2 Ticket medio: apenas CAR observavel ####


tab_ticket_medio_obs <- credit_asv %>%
  filter(
    monitored == 1,
    apresenteASV %in% c(
      "Apresente ASV",
      "Sem desmatamento"
    )
  ) %>%
  mutate(
    grupo = case_when(
      apresenteASV == "Apresente ASV" ~ "Precisaria apresentar ASV",
      apresenteASV == "Sem desmatamento" ~ "Não precisaria apresentar ASV"
    )
  ) %>%
  group_by(ano_safra, grupo) %>%
  summarise(
    n_operacoes = n(),
    ticket_medio_mil =
      sum(vl_parc_credito_real, na.rm=TRUE)/n()/1e3,
    .groups="drop"
  ) %>%
  mutate(
    ticket_medio_mil = round(ticket_medio_mil,1),
    vjust_pos = case_when(
      grupo=="Não precisaria apresentar ASV" ~ 1.4,
      TRUE ~ -0.6
    )
  )

cores_obs <- c(
  "Precisaria apresentar ASV" = "#9B2226",
  "Não precisaria apresentar ASV" = "#6A994E"
)

tab_ticket_medio_obs_export <- tab_ticket_medio_obs %>%
  rename(
    "Ano safra"=ano_safra,
    "Grupo"=grupo,
    "Numero de operacoes"=n_operacoes,
    "Valor medio do contrato (R$ mil)"=ticket_medio_mil
  )



g_ticket_medio_obs <- ggplot(
  tab_ticket_medio_obs,
  aes(
    x=ano_safra,
    y=ticket_medio_mil,
    color=grupo,
    group=grupo
  )
)+
  geom_line(linewidth=1.2)+
  geom_point(size=2)+
  
  geom_text(
    aes(
      label=paste0(
        "R$ ",
        round(ticket_medio_mil),
        " mil"
      ),
      vjust=vjust_pos
    ),
    size=3,
    show.legend=FALSE
  )+
  
  scale_color_manual(
    values=cores_obs
  )+
  
  labs(
    x="Ano safra",
    y="Valor medio do contrato (R$ mil)",
    color=NULL,
    title="Valor medio do contrato entre grupos observaveis"
  )+
  
  coord_cartesian(
    ylim=c(
      0,
      max(tab_ticket_medio_obs$ticket_medio_mil)*1.15
    )
  )+
  
  theme_minimal()+
  theme(
    plot.title=element_text(
      hjust=0.5,
      face="bold",
      size=16
    ),
    axis.title=element_text(size=13),
    axis.text=element_text(size=11),
    
    legend.position="bottom",
    legend.title=element_blank(),
    legend.key=element_blank(),
    legend.text=element_text(size=11),
    
    plot.margin=margin(
      20,20,20,20
    )
  )+
  guides(
    color=guide_legend(
      override.aes=list(
        linewidth=2,
        shape=NA
      )
    )
  )


ggsave(
  filename=file.path(
    plot_dir,
    "g_6_2_ticket_medio_observavel.png"
  ),
  plot=g_ticket_medio_obs,
  width=9,
  height=5
)


wb62 <- createWorkbook()

addWorksheet(wb62,"Ticket_medio_obs")
writeData(
  wb62,
  "Ticket_medio_obs",
  tab_ticket_medio_obs_export
)

saveWorkbook(
  wb62,
  file.path(
    output_dir,
    "analise_6_2_ticket_medio_observavel.xlsx"
  ),
  overwrite=TRUE
)

fwrite(
  tab_ticket_medio_obs,
  file.path(
    output_dir,
    "analise_6_2_ticket_medio_observavel.csv"
  )
)
# 6.3) VALOR MEDIO DO CONTRATO POR MODULO FISCAL ####

## Data prep ####

tab_ticket_mf <- credit_asv %>%
  filter(
    monitored == 1,
    apresenteASV %in% c("Apresente ASV", "Sem desmatamento")
  ) %>%
  mutate(
    grupo = case_when(
      apresenteASV == "Sem desmatamento" ~ "CAR sem exigencia de ASV",
      apresenteASV == "Apresente ASV" & quinzeMf > 0 ~ "Apresente ASV - Mais de 15 MF",
      apresenteASV == "Apresente ASV" & quinzeMf == 0 ~ "Apresente ASV - Menos de 15 MF"
    )
  ) %>%
  filter(!is.na(grupo)) %>%
  group_by(ano_safra, grupo) %>%
  summarise(
    n_operacoes = n(),
    credito_bi = sum(vl_parc_credito_real, na.rm = TRUE) / 1e9,
    ticket_medio_mil = sum(vl_parc_credito_real, na.rm = TRUE) / n() / 1e3,
    .groups = "drop"
  ) %>%
  mutate(
    grupo = factor(
      grupo,
      levels = c(
        "CAR sem exigencia de ASV",
        "Apresente ASV - Menos de 15 MF",
        "Apresente ASV - Mais de 15 MF"
      )
    ),
    ticket_medio_mil = round(ticket_medio_mil, 1),
    credito_bi = round(credito_bi, 2),
    vjust_pos = case_when(
      grupo == "CAR sem exigencia de ASV" ~ 1.4,
      grupo == "Apresente ASV - Menos de 15 MF" ~ -0.6,
      grupo == "Apresente ASV - Mais de 15 MF" ~ -1.4
    )
  )

## Cores ####

cores_mf <- c(
  "CAR sem exigencia de ASV" = "#6A994E",
  "Apresente ASV - Menos de 15 MF" = "#C1121F",
  "Apresente ASV - Mais de 15 MF" = "#780000"
)

## Grafico ####

g_6_3_ticket_mf <- ggplot(
  tab_ticket_mf,
  aes(
    x = ano_safra,
    y = ticket_medio_mil,
    color = grupo,
    group = grupo
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(
      label = paste0("R$ ", round(ticket_medio_mil), " mil"),
      vjust = vjust_pos
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cores_mf) +
  labs(
    x = "Ano safra",
    y = "Valor medio do contrato (R$ mil)",
    color = NULL,
    title = "Valor medio do contrato por grupo e modulo fiscal"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_ticket_mf$ticket_medio_mil, na.rm = TRUE) * 1.20)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 2, shape = NA)
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_6_3_ticket_medio_modulo_fiscal.png"),
  plot = g_6_3_ticket_mf,
  width = 10,
  height = 5.5
)

## Save workbook ####

tab_ticket_mf_export <- tab_ticket_mf %>%
  rename(
    "Ano safra" = ano_safra,
    "Grupo" = grupo,
    "Numero de operacoes" = n_operacoes,
    "Credito (R$ bi)" = credito_bi,
    "Valor medio do contrato (R$ mil)" = ticket_medio_mil
  )

wb63 <- createWorkbook()

addWorksheet(wb63, "Ticket_medio_MF")
writeData(wb63, "Ticket_medio_MF", tab_ticket_mf_export)

saveWorkbook(
  wb63,
  file.path(output_dir, "analise_6_3_ticket_medio_modulo_fiscal.xlsx"),
  overwrite = TRUE
)

fwrite(
  tab_ticket_mf,
  file.path(output_dir, "analise_6_3_ticket_medio_modulo_fiscal.csv")
)# 7) CREDITO QUE NAO SERA AFETADO PELA NOVA NORMA ####

## Data prep ####

tab_nao_afetado <- credit_asv %>%
  mutate(
    grupo = case_when(
      monitored == 0 ~ "Resto do Credito",
      monitored == 1 & apresenteASV == "Sem desmatamento" ~ "CAR sem exigencia de ASV",
      monitored == 1 & apresenteASV == "Sem CAR associado" ~ "CAR nao informado",
      monitored == 1 & apresenteASV == "Dado sigiloso" ~ "Dado Sigiloso",
      monitored == 1 & apresenteASV == "Apresente ASV" ~ "Apresente ASV"
    ),
    nao_afetado = case_when(
      grupo %in% c(
        "Resto do Credito",
        "CAR sem exigencia de ASV",
        "CAR nao informado"
      ) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(ano_safra) %>%
  summarise(
    credito_total = sum(vl_parc_credito_real, na.rm = TRUE),
    credito_nao_afetado = sum(vl_parc_credito_real * nao_afetado, na.rm = TRUE),
    ops_total = n(),
    ops_nao_afetado = sum(nao_afetado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_credito = 100 * credito_nao_afetado / credito_total,
    pct_operacoes = 100 * ops_nao_afetado / ops_total
  ) %>%
  select(ano_safra, pct_credito, pct_operacoes) %>%
  pivot_longer(
    cols = c(pct_credito, pct_operacoes),
    names_to = "serie",
    values_to = "percentual"
  ) %>%
  mutate(
    serie = case_when(
      serie == "pct_credito" ~ "Volume do credito",
      serie == "pct_operacoes" ~ "Operacoes"
    ),
    percentual = round(percentual, 2)
  )

## Export ####

tab_nao_afetado_export <- tab_nao_afetado %>%
  rename(
    "Ano safra" = ano_safra,
    "Indicador" = serie,
    "Percentual" = percentual
  )

## Cores ####

cores_pct <- c(
  "Volume do credito" = "#1B3A4B",
  "Operacoes" = "#8B2E2E"
)

## Grafico ####

g_nao_afetado <- ggplot(
  tab_nao_afetado,
  aes(x = ano_safra, y = percentual, color = serie, group = serie)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(label = paste0(round(percentual), "%")),
    size = 3,
    vjust = -0.6,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cores_pct) +
  labs(
    x = "Ano safra",
    y = "Percentual do credito total (%)",
    color = NULL,
    title = "Credito que nao sera afetado pela nova norma"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_nao_afetado$percentual, na.rm = TRUE) * 1.15)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 2, shape = NA)
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_7_credito_nao_afetado.png"),
  plot = g_nao_afetado,
  width = 9,
  height = 5
)

## Save workbook ####

wb7 <- createWorkbook()

addWorksheet(wb7, "Nao_afetado")
writeData(wb7, "Nao_afetado", tab_nao_afetado_export)

saveWorkbook(
  wb7,
  file.path(output_dir, "analise_7_credito_nao_afetado.xlsx"),
  overwrite = TRUE
)

fwrite(
  tab_nao_afetado,
  file.path(output_dir, "analise_7_credito_nao_afetado.csv")
)


# 8) OPERACOES AFETADAS PELA NOVA NORMA ####

## 8.1 Dentro do universo observavel ####

tab_observavel_asv <- credit_asv %>%
  mutate(
    grupo = case_when(
      monitored == 1 &
        apresenteASV == "Sem desmatamento" ~
        "CAR sem exigencia de ASV",
      
      monitored == 1 &
        apresenteASV == "Sem CAR associado" ~
        "CAR nao informado",
      
      monitored == 1 &
        apresenteASV == "Apresente ASV" ~
        "Apresente ASV",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(grupo)) %>%
  group_by(ano_safra) %>%
  summarise(
    credito_total = sum(
      vl_parc_credito_real,
      na.rm=TRUE
    ),
    credito_asv = sum(
      vl_parc_credito_real[
        grupo=="Apresente ASV"
      ],
      na.rm=TRUE
    ),
    
    ops_total=n(),
    ops_asv=sum(
      grupo=="Apresente ASV"
    ),
    
    .groups="drop"
  ) %>%
  mutate(
    pct_credito=
      round(
        100*credito_asv/credito_total,
        1
      ),
    
    pct_operacoes=
      round(
        100*ops_asv/ops_total,
        1
      )
  ) %>%
  select(
    ano_safra,
    pct_credito,
    pct_operacoes
  ) %>%
  pivot_longer(
    cols=c(
      pct_credito,
      pct_operacoes
    ),
    names_to="serie",
    values_to="percentual"
  ) %>%
  mutate(
    serie=case_when(
      serie=="pct_credito"~
        "Volume do credito",
      TRUE~
        "Operacoes"
    )
  )


g_observavel_asv <- ggplot(
  tab_observavel_asv,
  aes(
    x=ano_safra,
    y=percentual,
    color=serie,
    group=serie
  )
)+
  geom_line(linewidth=1.2)+
  geom_point(size=2)+
  geom_text(
    aes(
      label=percentual
    ),
    vjust=-0.6,
    size=3,
    show.legend=FALSE
  )+
  scale_color_manual(
    values=cores_pct
  )+
  labs(
    x="Ano safra",
    y="% do universo observavel",
    color=NULL,
    title="8.1 Percentual do universo observavel que exigiria ASV"
  )+
  theme_minimal()+
  theme(
    plot.title=element_text(
      hjust=.5,
      face="bold",
      size=16
    ),
    axis.title=element_text(size=13),
    axis.text=element_text(size=11),
    legend.position="bottom",
    legend.key=element_blank()
  )

ggsave(
  file.path(
    plot_dir,
    "g_8_1_credito_observavel_asv.png"
  ),
  g_observavel_asv,
  width=9,
  height=5
)



## 8.2 Dentro do universo total do credito ####

tab_total_asv <- credit_asv %>%
  group_by(ano_safra) %>%
  summarise(
    
    credito_total=
      sum(
        vl_parc_credito_real,
        na.rm=TRUE
      ),
    
    credito_asv=
      sum(
        vl_parc_credito_real[
          monitored==1 &
            apresenteASV=="Apresente ASV"
        ],
        na.rm=TRUE
      ),
    
    ops_total=n(),
    
    ops_asv=sum(
      monitored==1 &
        apresenteASV=="Apresente ASV"
    ),
    
    .groups="drop"
  ) %>%
  mutate(
    pct_credito=
      round(
        100*credito_asv/credito_total,
        1
      ),
    
    pct_operacoes=
      round(
        100*ops_asv/ops_total,
        1
      )
  ) %>%
  select(
    ano_safra,
    pct_credito,
    pct_operacoes
  ) %>%
  pivot_longer(
    cols=c(
      pct_credito,
      pct_operacoes
    ),
    names_to="serie",
    values_to="percentual"
  ) %>%
  mutate(
    serie=case_when(
      serie=="pct_credito"~
        "Volume do credito",
      TRUE~
        "Operacoes"
    )
  )


g_total_asv <- ggplot(
  tab_total_asv,
  aes(
    x=ano_safra,
    y=percentual,
    color=serie,
    group=serie
  )
)+
  geom_line(linewidth=1.2)+
  geom_point(size=2)+
  geom_text(
    aes(label=percentual),
    vjust=-0.6,
    size=3,
    show.legend=FALSE
  )+
  scale_color_manual(
    values=cores_pct
  )+
  labs(
    x="Ano safra",
    y="% do credito rural total",
    color=NULL,
    title="8.2 Percentual do credito rural total que exigiria ASV"
  )+
  theme_minimal()+
  theme(
    plot.title=element_text(
      hjust=.5,
      face="bold",
      size=16
    ),
    axis.title=element_text(size=13),
    axis.text=element_text(size=11),
    legend.position="bottom",
    legend.key=element_blank()
  )

ggsave(
  file.path(
    plot_dir,
    "g_8_2_credito_total_asv.png"
  ),
  g_total_asv,
  width=9,
  height=5
)



## Export ####

wb8 <- createWorkbook()

addWorksheet(wb8,"8_1_observavel")
writeData(
  wb8,
  "8_1_observavel",
  tab_observavel_asv
)

addWorksheet(wb8,"8_2_total")
writeData(
  wb8,
  "8_2_total",
  tab_total_asv
)

saveWorkbook(
  wb8,
  file.path(
    output_dir,
    "analise_8_duas_metricas_asv.xlsx"
  ),
  overwrite=TRUE
)
# 9) PERFIL DO PRODUTOR OBSERVAVEL ####

## Data prep ####

ticket_2024 <- credit_asv %>%
  filter(
    ano_safra == "2024/2025",
    monitored == 1,
    apresenteASV %in% c("Apresente ASV", "Sem desmatamento")
  ) %>%
  mutate(
    grupo = case_when(
      apresenteASV == "Apresente ASV" ~ "Apresente ASV",
      apresenteASV == "Sem desmatamento" ~ "CAR sem exigencia de ASV"
    ),
    grupo = factor(
      grupo,
      levels = c("CAR sem exigencia de ASV", "Apresente ASV")
    ),
    vl_credito_mil = vl_parc_credito_real / 10^3
  )

## Cores ####

cores_9 <- c(
  "Apresente ASV" = "#9B2226",
  "CAR sem exigencia de ASV" = "#6A994E"
)

## 9.1 Boxplot ####

g_9_boxplot <- ggplot(
  ticket_2024,
  aes(x = grupo, y = vl_credito_mil, fill = grupo)
) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.55
  ) +
  scale_fill_manual(values = cores_9) +
  labs(
    x = "",
    y = "Valor do contrato (R$ mil)",
    fill = NULL,
    title = "Distribuicao do valor dos contratos"
  ) +
  coord_cartesian(
    ylim = quantile(ticket_2024$vl_credito_mil, c(0, 0.95), na.rm = TRUE)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  filename = file.path(plot_dir, "g_9_1_boxplot_ticket.png"),
  plot = g_9_boxplot,
  width = 8,
  height = 5
)

## 9.2 Densidade ####

g_9_density <- ggplot(
  ticket_2024,
  aes(x = vl_credito_mil, color = grupo, fill = grupo)
) +
  geom_density(
    alpha = 0.20,
    linewidth = 1.1
  ) +
  scale_color_manual(values = cores_9) +
  scale_fill_manual(values = cores_9) +
  labs(
    x = "Valor do contrato (R$ mil)",
    y = "Densidade",
    color = NULL,
    fill = NULL,
    title = "Densidade do valor dos contratos"
  ) +
  coord_cartesian(
    xlim = quantile(ticket_2024$vl_credito_mil, c(0, 0.95), na.rm = TRUE)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  filename = file.path(plot_dir, "g_9_2_density_ticket.png"),
  plot = g_9_density,
  width = 9,
  height = 5
)

## 9.3 Percentis ####

tab_percentis <- ticket_2024 %>%
  group_by(grupo) %>%
  summarise(
    p10 = quantile(vl_credito_mil, 0.10, na.rm = TRUE),
    p20 = quantile(vl_credito_mil, 0.20, na.rm = TRUE),
    p30 = quantile(vl_credito_mil, 0.30, na.rm = TRUE),
    p40 = quantile(vl_credito_mil, 0.40, na.rm = TRUE),
    p50 = quantile(vl_credito_mil, 0.50, na.rm = TRUE),
    p60 = quantile(vl_credito_mil, 0.60, na.rm = TRUE),
    p70 = quantile(vl_credito_mil, 0.70, na.rm = TRUE),
    p80 = quantile(vl_credito_mil, 0.80, na.rm = TRUE),
    p90 = quantile(vl_credito_mil, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("p"),
    names_to = "percentil",
    values_to = "valor_mil"
  ) %>%
  mutate(
    percentil_num = as.numeric(str_remove(percentil, "p")),
    valor_mil = round(valor_mil, 2),
    vjust_pos = case_when(
      grupo == "CAR sem exigencia de ASV" ~ 1.4,
      TRUE ~ -0.6
    )
  )

g_9_percentis <- ggplot(
  tab_percentis,
  aes(x = percentil_num, y = valor_mil, color = grupo, group = grupo)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(
      label = round(valor_mil),
      vjust = vjust_pos
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cores_9) +
  scale_x_continuous(
    breaks = seq(10, 90, by = 10)
  ) +
  labs(
    x = "Percentil",
    y = "Valor do contrato (R$ mil)",
    color = NULL,
    title = "Percentis do valor dos contratos"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_percentis$valor_mil, na.rm = TRUE) * 1.15)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 2, shape = NA)
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_9_3_percentis_ticket.png"),
  plot = g_9_percentis,
  width = 9,
  height = 5
)

## 9.3b Percentis por ASV e modulo fiscal ####

tab_percentis_mf <- ticket_2024 %>%
  mutate(
    grupo_mf = case_when(
      grupo == "CAR sem exigencia de ASV" ~ "CAR sem exigencia de ASV",
      grupo == "Apresente ASV" & quinzeMf > 0 ~ "Apresente ASV - 15 ou mais MF",
      grupo == "Apresente ASV" & quinzeMf == 0 ~ "Apresente ASV - Menos de 15 MF"
    )
  ) %>%
  filter(!is.na(grupo_mf)) %>%
  group_by(grupo_mf) %>%
  summarise(
    p10 = quantile(vl_credito_mil, 0.10, na.rm = TRUE),
    p20 = quantile(vl_credito_mil, 0.20, na.rm = TRUE),
    p30 = quantile(vl_credito_mil, 0.30, na.rm = TRUE),
    p40 = quantile(vl_credito_mil, 0.40, na.rm = TRUE),
    p50 = quantile(vl_credito_mil, 0.50, na.rm = TRUE),
    p60 = quantile(vl_credito_mil, 0.60, na.rm = TRUE),
    p70 = quantile(vl_credito_mil, 0.70, na.rm = TRUE),
    p80 = quantile(vl_credito_mil, 0.80, na.rm = TRUE),
    p90 = quantile(vl_credito_mil, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("p"),
    names_to = "percentil",
    values_to = "valor_mil"
  ) %>%
  mutate(
    percentil_num = as.numeric(str_remove(percentil, "p")),
    valor_mil = round(valor_mil, 2),
    grupo_mf = factor(
      grupo_mf,
      levels = c(
        "CAR sem exigencia de ASV",
        "Apresente ASV - Menos de 15 MF",
        "Apresente ASV - 15 ou mais MF"
      )
    ),
    vjust_pos = case_when(
      grupo_mf == "CAR sem exigencia de ASV" ~ 1.4,
      grupo_mf == "Apresente ASV - Menos de 15 MF" ~ -0.6,
      grupo_mf == "Apresente ASV - 15 ou mais MF" ~ -1.4
    )
  )

cores_93b <- c(
  "CAR sem exigencia de ASV" = "#6A994E",
  "Apresente ASV - Menos de 15 MF" = "#C1121F",
  "Apresente ASV - 15 ou mais MF" = "#780000"
)

g_9_3b_percentis_mf <- ggplot(
  tab_percentis_mf,
  aes(
    x = percentil_num,
    y = valor_mil,
    color = grupo_mf,
    group = grupo_mf
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(
      label = round(valor_mil),
      vjust = vjust_pos
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cores_93b) +
  scale_x_continuous(
    breaks = seq(10, 90, by = 10)
  ) +
  labs(
    x = "Percentil",
    y = "Valor do contrato (R$ mil)",
    color = NULL,
    title = "Percentis do valor dos contratos por modulo fiscal"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_percentis_mf$valor_mil, na.rm = TRUE) * 1.20)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 2, shape = NA)
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_9_3b_percentis_ticket_modulo_fiscal.png"),
  plot = g_9_3b_percentis_mf,
  width = 10,
  height = 5.5
)

fwrite(
  tab_percentis_mf,
  file.path(output_dir, "analise_9_3b_percentis_ticket_modulo_fiscal.csv")
)
## 9.4 Pontos da distribuicao ####

tab_dist_points <- ticket_2024 %>%
  group_by(grupo) %>%
  summarise(
    `Percentil 25` = quantile(vl_credito_mil, 0.25, na.rm = TRUE),
    Media = mean(vl_credito_mil, na.rm = TRUE),
    Mediana = median(vl_credito_mil, na.rm = TRUE),
    `Percentil 75` = quantile(vl_credito_mil, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(`Percentil 25`, Media, Mediana, `Percentil 75`),
    names_to = "estatistica",
    values_to = "valor_mil"
  ) %>%
  mutate(
    valor_mil = round(valor_mil, 2),
    estatistica = factor(
      estatistica,
      levels = c("Percentil 25", "Mediana", "Media", "Percentil 75")
    )
  )


pd_94 <- position_dodge(width = 0.75)

cores_94 <- c(
  "Percentil 25" = "#264653",
  "Mediana" = "#2A9D8F",
  "Media" = "#E9C46A",
  "Percentil 75" = "#9B2226"
)

g_9_dist_points <- ggplot(
  tab_dist_points,
  aes(x = grupo, y = valor_mil, fill = estatistica)
) +
  geom_col(
    position = pd_94,
    width = 0.65
  ) +
  geom_text(
    aes(label = round(valor_mil)),
    position = pd_94,
    vjust = -0.35,
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = cores_94) +
  labs(
    x = "",
    y = "Valor do contrato (R$ mil)",
    fill = NULL,
    title = "Pontos da distribuicao do valor dos contratos"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_dist_points$valor_mil, na.rm = TRUE) * 1.25)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  filename = file.path(plot_dir, "g_9_4_pontos_distribuicao_ticket.png"),
  plot = g_9_dist_points,
  width = 9,
  height = 5
)



## Export ####

tab_ticket_2024_resumo <- ticket_2024 %>%
  group_by(grupo) %>%
  summarise(
    n_operacoes = n(),
    credito_bi = sum(vl_parc_credito_real, na.rm = TRUE) / 1e9,
    media_mil = mean(vl_credito_mil, na.rm = TRUE),
    mediana_mil = median(vl_credito_mil, na.rm = TRUE),
    p25_mil = quantile(vl_credito_mil, 0.25, na.rm = TRUE),
    p75_mil = quantile(vl_credito_mil, 0.75, na.rm = TRUE),
    p90_mil = quantile(vl_credito_mil, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

wb9 <- createWorkbook()

addWorksheet(wb9, "Resumo")
writeData(wb9, "Resumo", tab_ticket_2024_resumo)

addWorksheet(wb9, "Percentis")
writeData(wb9, "Percentis", tab_percentis)

addWorksheet(wb9, "Pontos")
writeData(wb9, "Pontos", tab_dist_points)

saveWorkbook(
  wb9,
  file.path(output_dir, "analise_9_perfil_produtor_observavel.xlsx"),
  overwrite = TRUE
)

fwrite(
  tab_ticket_2024_resumo,
  file.path(output_dir, "analise_9_perfil_produtor_observavel_resumo.csv")
)

fwrite(
  tab_percentis,
  file.path(output_dir, "analise_9_perfil_produtor_observavel_percentis.csv")
)

fwrite(
  tab_dist_points,
  file.path(output_dir, "analise_9_perfil_produtor_observavel_pontos.csv")
)
## 9.4b Pontos da distribuicao por ASV e modulo fiscal ####

tab_dist_points_mf <- ticket_2024 %>%
  mutate(
    grupo_mf = case_when(
      grupo == "CAR sem exigencia de ASV" ~ "CAR sem exigencia de ASV",
      grupo == "Apresente ASV" & quinzeMf > 0 ~ "Apresente ASV - 15 ou mais MF",
      grupo == "Apresente ASV" & quinzeMf == 0 ~ "Apresente ASV - Menos de 15 MF"
    )
  ) %>%
  filter(!is.na(grupo_mf)) %>%
  group_by(grupo_mf) %>%
  summarise(
    `Percentil 25` = quantile(vl_credito_mil, 0.25, na.rm = TRUE),
    Media = mean(vl_credito_mil, na.rm = TRUE),
    Mediana = median(vl_credito_mil, na.rm = TRUE),
    `Percentil 75` = quantile(vl_credito_mil, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(`Percentil 25`, Mediana, Media, `Percentil 75`),
    names_to = "estatistica",
    values_to = "valor_mil"
  ) %>%
  mutate(
    valor_mil = round(valor_mil, 1),
    grupo_mf = factor(
      grupo_mf,
      levels = c(
        "CAR sem exigencia de ASV",
        "Apresente ASV - Menos de 15 MF",
        "Apresente ASV - 15 ou mais MF"
      )
    ),
    estatistica = factor(
      estatistica,
      levels = c("Percentil 25", "Mediana", "Media", "Percentil 75")
    )
  )

pd_94b <- position_dodge(width = 0.75)

cores_94b <- c(
  "Percentil 25" = "#264653",
  "Mediana" = "#2A9D8F",
  "Media" = "#E9C46A",
  "Percentil 75" = "#9B2226"
)

g_9_4b_dist_points_mf <- ggplot(
  tab_dist_points_mf,
  aes(x = grupo_mf, y = valor_mil, fill = estatistica)
) +
  geom_col(
    position = pd_94b,
    width = 0.65
  ) +
  geom_text(
    aes(label = round(valor_mil)),
    position = pd_94b,
    vjust = -0.35,
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = cores_94b) +
  labs(
    x = "",
    y = "Valor do contrato (R$ mil)",
    fill = NULL,
    title = "Pontos da distribuicao do valor dos contratos por modulo fiscal"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_dist_points_mf$valor_mil, na.rm = TRUE) * 1.25)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(size = 10, angle = 15, hjust = 1),
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  filename = file.path(plot_dir, "g_9_4b_pontos_distribuicao_ticket_modulo_fiscal.png"),
  plot = g_9_4b_dist_points_mf,
  width = 10,
  height = 5.5
)

fwrite(
  tab_dist_points_mf,
  file.path(output_dir, "analise_9_4b_pontos_distribuicao_ticket_modulo_fiscal.csv")
)
# 10) SENSIBILIDADE POR MODULO FISCAL ####

## Data prep ####

base_10 <- credit_asv %>%
  mutate(
    grupo_obs = case_when(
      monitored == 1 & apresenteASV == "Apresente ASV" ~ "Apresente ASV",
      monitored == 1 & apresenteASV == "Sem desmatamento" ~ "CAR sem exigencia de ASV",
      monitored == 1 & apresenteASV == "Sem CAR associado" ~ "CAR nao informado",
      TRUE ~ NA_character_
    ),
    grupo_15mf = case_when(
      monitored == 1 & apresenteASV == "Apresente ASV" & quinzeMf > 0 ~ "Apresente ASV - 15 ou mais MF",
      monitored == 1 & apresenteASV == "Apresente ASV" & quinzeMf == 0 ~ "Apresente ASV - Menos de 15 MF",
      TRUE ~ NA_character_
    )
  )

cores_10 <- c(
  "Apresente ASV - 15 ou mais MF" = "#780000",
  "Apresente ASV - Menos de 15 MF" = "#C1121F"
)

# 10.1) VALOR DO CREDITO EM APRESENTE ASV POR MODULO FISCAL ####

## Data prep ####

tab_10_1 <- base_10 %>%
  filter(!is.na(grupo_15mf)) %>%
  group_by(ano_safra, grupo_15mf) %>%
  summarise(
    credito_bi = sum(vl_parc_credito_real, na.rm = TRUE) / 1e9,
    .groups = "drop"
  ) %>%
  mutate(
    credito_bi = round(credito_bi, 2),
    vjust_pos = case_when(
      grupo_15mf == "Apresente ASV - 15 ou mais MF" ~ -0.6,
      TRUE ~ 1.4
    )
  )

## Grafico ####

g_10_1_credito_asv_mf <- ggplot(
  tab_10_1,
  aes(
    x = ano_safra,
    y = credito_bi,
    color = grupo_15mf,
    group = grupo_15mf
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(
      label = paste0("R$ ", round(credito_bi), "B"),
      vjust = vjust_pos
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cores_10) +
  labs(
    x = "Ano safra",
    y = "Credito (R$ bi)",
    color = NULL,
    title = "Credito em Apresente ASV por modulo fiscal"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_10_1$credito_bi, na.rm = TRUE) * 1.20)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 2, shape = NA)
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_10_1_credito_asv_modulo_fiscal.png"),
  plot = g_10_1_credito_asv_mf,
  width = 9,
  height = 5
)

# 10.2) OPERACOES EM APRESENTE ASV POR MODULO FISCAL ####

## Data prep ####

tab_10_2 <- base_10 %>%
  filter(!is.na(grupo_15mf)) %>%
  group_by(ano_safra, grupo_15mf) %>%
  summarise(
    opsM = n() / 1e3,
    .groups = "drop"
  ) %>%
  mutate(
    opsM = round(opsM, 2),
    vjust_pos = case_when(
      grupo_15mf == "Apresente ASV - 15 ou mais MF" ~ -0.6,
      TRUE ~ 1.4
    )
  )

## Grafico ####

g_10_2_ops_asv_mf <- ggplot(
  tab_10_2,
  aes(
    x = ano_safra,
    y = opsM,
    color = grupo_15mf,
    group = grupo_15mf
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(
      label = paste0(round(opsM), " M"),
      vjust = vjust_pos
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cores_10) +
  labs(
    x = "Ano safra",
    y = "Mil operacoes",
    color = NULL,
    title = "Operacoes em Apresente ASV por modulo fiscal"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_10_2$opsM, na.rm = TRUE) * 1.20)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 2, shape = NA)
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_10_2_ops_asv_modulo_fiscal.png"),
  plot = g_10_2_ops_asv_mf,
  width = 9,
  height = 5
)

# 10.3) PARTICIPACAO NO UNIVERSO OBSERVAVEL ####

## Data prep ####

tab_10_3 <- base_10 %>%
  filter(!is.na(grupo_obs)) %>%
  group_by(ano_safra) %>%
  summarise(
    credito_obs = sum(vl_parc_credito_real, na.rm = TRUE),
    ops_obs = n(),
    
    credito_asv_15mais = sum(
      vl_parc_credito_real[grupo_15mf == "Apresente ASV - 15 ou mais MF"],
      na.rm = TRUE
    ),
    credito_asv_menos15 = sum(
      vl_parc_credito_real[grupo_15mf == "Apresente ASV - Menos de 15 MF"],
      na.rm = TRUE
    ),
    
    ops_asv_15mais = sum(grupo_15mf == "Apresente ASV - 15 ou mais MF", na.rm = TRUE),
    ops_asv_menos15 = sum(grupo_15mf == "Apresente ASV - Menos de 15 MF", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    ano_safra,
    `Valor - 15 ou mais MF` = 100 * credito_asv_15mais / credito_obs,
    `Valor - Menos de 15 MF` = 100 * credito_asv_menos15 / credito_obs,
    `Operacoes - 15 ou mais MF` = 100 * ops_asv_15mais / ops_obs,
    `Operacoes - Menos de 15 MF` = 100 * ops_asv_menos15 / ops_obs
  ) %>%
  pivot_longer(
    cols = -ano_safra,
    names_to = "serie",
    values_to = "percentual"
  ) %>%
  mutate(
    percentual = round(percentual, 1),
    tipo = case_when(
      str_detect(serie, "Valor") ~ "Valor",
      TRUE ~ "Operacoes"
    ),
    grupo_15mf = case_when(
      str_detect(serie, "15 ou mais") ~ "Apresente ASV - 15 ou mais MF",
      TRUE ~ "Apresente ASV - Menos de 15 MF"
    ),
    vjust_pos = case_when(
      tipo == "Valor" &
        grupo_15mf == "Apresente ASV - Menos de 15 MF" ~ -0.6,
      tipo == "Valor" &
        grupo_15mf == "Apresente ASV - 15 ou mais MF" ~ -0.6,
      tipo == "Operacoes" &
        grupo_15mf == "Apresente ASV - 15 ou mais MF" ~ 1.8,
      tipo == "Operacoes" &
        grupo_15mf == "Apresente ASV - Menos de 15 MF" ~ -1.2,
      TRUE ~ -0.6
    )
  )
## Grafico ####

g_10_3_share_obs <- ggplot(
  tab_10_3,
  aes(
    x = ano_safra,
    y = percentual,
    color = grupo_15mf,
    linetype = tipo,
    group = interaction(grupo_15mf, tipo)
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(
      label = paste0(percentual, "%"),
      vjust = vjust_pos
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cores_10) +
  labs(
    x = "Ano safra",
    y = "% do universo observavel",
    color = NULL,
    linetype = NULL,
    title = "Participacao no universo observavel por modulo fiscal"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_10_3$percentual, na.rm = TRUE) * 1.20)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 2, shape = NA)
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_10_3_share_observavel_asv_modulo_fiscal.png"),
  plot = g_10_3_share_obs,
  width = 10,
  height = 5.5
)

# 10.4) PARTICIPACAO NO CREDITO RURAL TOTAL ####

## Data prep ####

tab_10_4 <- base_10 %>%
  group_by(ano_safra) %>%
  summarise(
    credito_total = sum(vl_parc_credito_real, na.rm = TRUE),
    ops_total = n(),
    
    credito_asv_15mais = sum(
      vl_parc_credito_real[grupo_15mf == "Apresente ASV - 15 ou mais MF"],
      na.rm = TRUE
    ),
    credito_asv_menos15 = sum(
      vl_parc_credito_real[grupo_15mf == "Apresente ASV - Menos de 15 MF"],
      na.rm = TRUE
    ),
    
    ops_asv_15mais = sum(grupo_15mf == "Apresente ASV - 15 ou mais MF", na.rm = TRUE),
    ops_asv_menos15 = sum(grupo_15mf == "Apresente ASV - Menos de 15 MF", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    ano_safra,
    `Valor - 15 ou mais MF` = 100 * credito_asv_15mais / credito_total,
    `Valor - Menos de 15 MF` = 100 * credito_asv_menos15 / credito_total,
    `Operacoes - 15 ou mais MF` = 100 * ops_asv_15mais / ops_total,
    `Operacoes - Menos de 15 MF` = 100 * ops_asv_menos15 / ops_total
  ) %>%
  pivot_longer(
    cols = -ano_safra,
    names_to = "serie",
    values_to = "percentual"
  ) %>%
  mutate(
    percentual = round(percentual, 1),
    tipo = case_when(
      str_detect(serie, "Valor") ~ "Valor",
      TRUE ~ "Operacoes"
    ),
    grupo_15mf = case_when(
      str_detect(serie, "15 ou mais") ~ "Apresente ASV - 15 ou mais MF",
      TRUE ~ "Apresente ASV - Menos de 15 MF"
    ),
    vjust_pos = case_when(
      tipo == "Valor" &
        grupo_15mf == "Apresente ASV - Menos de 15 MF" ~ -0.6,
      tipo == "Valor" &
        grupo_15mf == "Apresente ASV - 15 ou mais MF" ~ -0.6,
      tipo == "Operacoes" &
        grupo_15mf == "Apresente ASV - 15 ou mais MF" ~ 1.8,
      tipo == "Operacoes" &
        grupo_15mf == "Apresente ASV - Menos de 15 MF" ~ -1.2,
      TRUE ~ -0.6
    )
  )
## Grafico ####

g_10_4_share_total <- ggplot(
  tab_10_4,
  aes(
    x = ano_safra,
    y = percentual,
    color = grupo_15mf,
    linetype = tipo,
    group = interaction(grupo_15mf, tipo)
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(
      label = paste0(percentual, "%"),
      vjust = vjust_pos
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cores_10) +
  labs(
    x = "Ano safra",
    y = "% do credito rural total",
    color = NULL,
    linetype = NULL,
    title = "Participacao no credito rural total por modulo fiscal"
  ) +
  coord_cartesian(
    ylim = c(0, max(tab_10_4$percentual, na.rm = TRUE) * 1.20)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 2, shape = NA)
    )
  )

ggsave(
  filename = file.path(plot_dir, "g_10_4_share_total_asv_modulo_fiscal.png"),
  plot = g_10_4_share_total,
  width = 10,
  height = 5.5
)

# SAVE 10 ####

wb10 <- createWorkbook()

addWorksheet(wb10, "10_1_credito")
writeData(wb10, "10_1_credito", tab_10_1)

addWorksheet(wb10, "10_2_operacoes")
writeData(wb10, "10_2_operacoes", tab_10_2)

addWorksheet(wb10, "10_3_share_observavel")
writeData(wb10, "10_3_share_observavel", tab_10_3)

addWorksheet(wb10, "10_4_share_total")
writeData(wb10, "10_4_share_total", tab_10_4)

saveWorkbook(
  wb10,
  file.path(output_dir, "analise_10_sensibilidade_modulo_fiscal.xlsx"),
  overwrite = TRUE
)

fwrite(tab_10_1, file.path(output_dir, "analise_10_1_credito_asv_modulo_fiscal.csv"))
fwrite(tab_10_2, file.path(output_dir, "analise_10_2_ops_asv_modulo_fiscal.csv"))
fwrite(tab_10_3, file.path(output_dir, "analise_10_3_share_observavel_asv_modulo_fiscal.csv"))
fwrite(tab_10_4, file.path(output_dir, "analise_10_4_share_total_asv_modulo_fiscal.csv"))
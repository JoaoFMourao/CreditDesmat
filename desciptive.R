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
      monitored == 1 ~ "Monitorado",
      TRUE ~ "Nao monitorado"
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
    show.legend = FALSE   # 🔥 remove influencia na legenda
  ) +
  
  labs(
    x = "Ano safra",
    y = "Credito (R$ bi)",
    color = NULL,
    title = "Credito monitorado e nao monitorado"
  ) +
  
  coord_cartesian(ylim = c(0, 410)) +
  
  # 🎨 cores mais elegantes
  scale_color_manual(
    values = c(
      "Monitorado" = "#1B3A4B",       # azul escuro elegante
      "Nao monitorado" = "#8B2E2E"    # vermelho vinho
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
        shape = NA,     # remove ponto
        fill = NA       # remove quadrado branco
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
    show.legend = FALSE   # 🔥 remove influencia na legenda
  ) +
  
  labs(
    x = "Ano safra",
    y = "Mil Operações",
    color = NULL,
    title = "Credito monitorado e nao monitorado"
  ) +
  
  coord_cartesian(ylim = c(0, 2400)) +
  
  # 🎨 cores mais elegantes
  scale_color_manual(
    values = c(
      "Monitorado" = "#1B3A4B",       # azul escuro elegante
      "Nao monitorado" = "#8B2E2E"    # vermelho vinho
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
        shape = NA,     # remove ponto
        fill = NA       # remove quadrado branco
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
      apresenteASV == "Sem desmatamento" ~ "CAR sem desmatamento",
      apresenteASV == "Apresente ASV" ~ "Apresente ASV",
      apresenteASV == "Sem CAR associado" ~ "CAR nao informado"
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
    vjust_pos = case_when(
      grupo == "Apresente ASV" ~ -1.2,
      grupo == "CAR nao informado" ~ 1.5,
      TRUE ~ -0.6
    )
  )

cores_mma <- c(
  "Dado Sigiloso" = "#264653",        # azul petróleo
  "CAR nao informado" = "#2A9D8F",    # verde escuro
  "CAR sem desmatamento" = "#E9C46A", # amarelo queimado
  "Apresente ASV" = "#9B2226"         # vermelho escuro
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
    title = "Credito monitorado por situacao do CAR"
  ) +
  
  coord_cartesian(
    ylim = c(0, max(tab_credit_basic$credito_bi) * 1.15)  # mais espaço
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
    
    # 🔥 mais margem pra respirar
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
    title = "Credito monitorado por situacao do CAR"
  ) +
  
  coord_cartesian(
    ylim = c(0, max(tab_credit_basic$opsM) * 1.15)  # mais espaço
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
    
    # 🔥 mais margem pra respirar
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
  
  # label dentro da barra
  geom_text(
    aes(label = label_text),
    color = "white",
    size = 3.5,
    vjust = 1.5   # joga pra dentro da barra
  ) +
  
  # label do topo (desmatamento)
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

# cria os cinco grupos de interesse na base de operacoes
tab_ticket_medio <- credit_asv %>%
  mutate(
    grupo = case_when(
      monitored == 0 ~ "Nao monitorado",
      monitored == 1 & apresenteASV == "Dado sigiloso" ~ "Dado sigiloso",
      monitored == 1 & apresenteASV == "Sem CAR associado" ~ "CAR nao informado",
      monitored == 1 & apresenteASV == "Sem desmatamento" ~ "CAR sem desmatamento",
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
    across(c(credito_bi, ticket_medio_mil), ~ round(.x, 2))
  )

# tabela resumo acumulada no periodo
tab_ticket_medio_total <- credit_asv %>%
  mutate(
    grupo = case_when(
      monitored == 0 ~ "Nao monitorado",
      monitored == 1 & apresenteASV == "Dado sigiloso" ~ "Dado sigiloso",
      monitored == 1 & apresenteASV == "Sem CAR associado" ~ "CAR nao informado",
      monitored == 1 & apresenteASV == "Sem desmatamento" ~ "CAR sem desmatamento",
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
  "CAR sem desmatamento" = "#E9C46A",
  "CAR nao informado" = "#2A9D8F",
  "Dado sigiloso" = "#264653",
  "Nao monitorado" = "#6D597A"
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




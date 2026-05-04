# HEADER ####
# Credito mensal deflacionado: participacao percentual por mes ####

# SETUP ####
rm(list = ls())
gc()

strt.time <- Sys.time()

library(tidyverse)
library(openxlsx)
library(scales)

root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "baseMCR/dados")

input_dir <- file.path(root, "clean")
output_dir <- file.path(root, "output", "monthly_credit_share")
plot_dir <- file.path(output_dir, "plots")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# remove scientific notation
options(scipen = 999)

# LOAD DATA ####

# puxa diretamente o output ja construido no pipeline
df <- readRDS(file.path(input_dir, "sicor_main_2018_2026_basic_complement.Rds"))

# ANALYSIS ####

## Data prep ####

# filtra anos calendario de 2023 a 2025
# agrega o credito real por ano e mes
# calcula a participacao percentual de cada mes no total do ano
tab_month_credit <- df %>%
  filter(ano %in% 2023:2025) %>%
  group_by(ano, mes) %>%
  dplyr::summarise(
    credito_real = sum(vl_parc_credito_real, na.rm = TRUE),
    operacoes = n(),
    .groups = "drop"
  ) %>%
  group_by(ano) %>%
  mutate(
    credito_bi = credito_real / 10^9,
    share_credito = 100 * credito_real / sum(credito_real, na.rm = TRUE),
    share_operacoes = 100 * operacoes / sum(operacoes, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    mes_nome = factor(month.abb[mes], levels = month.abb),
    mes_nome = factor(
      c("Jan","Fev","Mar","Abr","Mai","Jun",
        "Jul","Ago","Set","Out","Nov","Dez")[mes],
      levels = c(
        "Jan","Fev","Mar","Abr","Mai","Jun",
        "Jul","Ago","Set","Out","Nov","Dez"
      )
    )
  ) %>%
  mutate(
    across(c(credito_bi, share_credito, share_operacoes), ~ round(.x, 2))
  )

## Export data ####

# tabela longa
tab_month_credit_export <- tab_month_credit %>%
  rename(
    "Ano" = ano,
    "Mes_num" = mes,
    "Mes" = mes_nome,
    "Credito_real_R$" = credito_real,
    "Credito_real_R$_bi" = credito_bi,
    "Operacoes" = operacoes,
    "Participacao_credito_pct" = share_credito,
    "Participacao_operacoes_pct" = share_operacoes
  )

# tabela wide so para a participacao do credito
tab_month_credit_wide <- tab_month_credit %>%
  select(ano, mes_nome, share_credito) %>%
  pivot_wider(
    names_from = mes_nome,
    values_from = share_credito
  ) %>%
  rename(
    "Ano" = ano
  )

# salva excel
wb <- createWorkbook()

addWorksheet(wb, "Long")
writeData(wb, "Long", tab_month_credit_export)

addWorksheet(wb, "Wide")
writeData(wb, "Wide", tab_month_credit_wide)

saveWorkbook(
  wb,
  file.path(output_dir, "credito_mensal_participacao_2023_2025.xlsx"),
  overwrite = TRUE
)

# salva csv
write.csv(
  tab_month_credit,
  file.path(output_dir, "credito_mensal_participacao_2023_2025.csv"),
  row.names = FALSE
)

write.csv(
  tab_month_credit_wide,
  file.path(output_dir, "credito_mensal_participacao_2023_2025_wide.csv"),
  row.names = FALSE
)

## Graph ####

cores_anos <- c(
  "2023" = "#264653",
  "2024" = "#2A9D8F",
  "2025" = "#9B2226"
)

## Graph ####

cores_anos <- c(
  "2023" = "#264653",
  "2024" = "#2A9D8F",
  "2025" = "#9B2226"
)

pd <- position_dodge(width = .75)

g_month_share <- ggplot(
  tab_month_credit,
  aes(
    x = mes_nome,
    y = share_credito,
    fill = factor(ano)
  )
) +
  geom_col(
    position = pd,
    width = .65
  ) +
  
  geom_text(
    aes(
      label = paste0(round(share_credito))
    ),
    position = pd,
    vjust = -0.35,
    size = 3
  ) +
  
  scale_fill_manual(values = cores_anos) +
  
  labs(
    x = "Mes",
    y = "Participacao do credito (%)",
    fill = NULL
  ) +
  
  coord_cartesian(
    ylim = c(
      0,
      max(tab_month_credit$share_credito) * 1.20
    )
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = .5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    
    legend.position = "bottom",
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    
    plot.margin = margin(20,20,20,20)
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(color = NA)
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "g_credito_mensal_participacao.png"
  ),
  plot = g_month_share,
  width = 10,
  height = 5.5
)

Sys.time() - strt.time
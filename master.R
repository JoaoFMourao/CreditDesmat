# MASTER SCRIPT ---------------------------------------------------------------
# Projeto: Analise MCR / Credito Rural / CAR
# Autor:   Joao Mourao
# Objetivo: rodar o pipeline completo do projeto

# ---------------------------------------------------------------------------
# ORDEM DE EXECUCAO
# ---------------------------------------------------------------------------
# 1) preparar base SICOR              (raw -> consolidada)
# 2) merge com complemento basico     (+ municipio, IPCA, ano_safra)
# 3) cruzar PRODES x CAR              (regra da borda 60m + auditoria)
# 4) merge SICOR x CAR/ASV            (1 linha por contrato; status_car)
# 5) output municipal long            (item 4 da revisao do projeto)
# 6) analise descritiva final         (tabelas + graficos)
# 7) (opcional) Shiny dashboard       (shiny/app.R)
# 8) (TODO)  desmat sem sobreposicao  (item 6 da revisao -- pendente)

# ---------------------------------------------------------------------------
# CAMINHO BASE
# ---------------------------------------------------------------------------
root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "GitHub", "CreditDesmat")

# ============================================================================
# 1) PREPARAR BASE SICOR
# ============================================================================
# INPUT:  raw/sicor/SICOR_<ANO>.gz
# OUTPUT: clean/sicor_main_2018_2026.Rds
source(file.path(root, "r2c", "1_prepare_sicor_main.R"))

# ============================================================================
# 2) MERGE COM COMPLEMENTO BASICO + IPCA
# ============================================================================
# INPUT:  clean/sicor_main_2018_2026.Rds
#         raw/sicor/complementos/SICOR_COMPLEMENTO_OPERACAO_BASICA.gz
#         raw/ipeadata[<data>].csv
# OUTPUT: clean/sicor_main_2018_2026_basic_complement.Rds
root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "GitHub", "CreditDesmat")
file.edit(file.path(root, "r2c", "2_merge_comp_basic.R"))
source(file.path(root, "r2c", "2_merge_comp_basic.R"))

# ============================================================================
# 3) CRUZAMENTO PRODES x CAR (antigo INPEs_exersice.R) #2.107201 hours 
# ============================================================================
# INPUT:  raw/lista_mcr_biomas_VF.gdb   (atualizado mai/2026 - SharePoint MMA)
#         raw/prodes_mcr.gdb
# OUTPUT: output/INPEs_exercise/changedImpact.rds
#         output/INPEs_exercise/audit_desmat_per_car.csv         <- item 2
#         output/INPEs_exercise/audit_desmat_per_car_resumo.csv  <- item 2
root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "GitHub", "CreditDesmat")

file.edit(file.path(root, "r2c", "3_cross_prodes_car.R"))
source(file.path(root, "r2c", "3_cross_prodes_car.R"))

# ============================================================================
# 4) MERGE SICOR x CAR/ASV/MONITORAMENTO  tempo total: 3.293324 hours 
# ============================================================================
# INPUT:  clean/sicor_main_2018_2026_basic_complement.Rds
#         raw/sicor/complementos/SICOR_PROPRIEDADES.gz
#         output/INPEs_exercise/changedImpact.rds
#         raw/sicor/complementos/Fontes MCR 6-1-2 e 6-7-7.csv
# OUTPUT: built/asvCar_credit.Rds
#         built/credit_asv.Rds          (com status_car + faixa_mf)
#         built/properties_asv.Rds
root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "GitHub", "CreditDesmat")
file.edit(file.path(root, "built", "sicor_per_car.R"))

source(file.path(root, "built", "sicor_per_car.R"))

# ============================================================================
# 5) OUTPUT MUNICIPAL LONG (item 4 da revisao) tempo total: 6.980845 hours 
# ============================================================================
# INPUT:  built/credit_asv.Rds
#         built/properties_asv.Rds
# OUTPUT: output/long/credit_long_municipal.csv
#         output/long/credit_long_municipal.xlsx
# root <- file.path("C:/Users", Sys.getenv("USERNAME"),
#                   "Documents", "GitHub", "CreditDesmat")
# source(file.path(root, "r2c", "4_municipal_long_output.R"))

# ============================================================================
# 6) ANALISE DESCRITIVA FINAL
# ============================================================================
# INPUT:  built/asvCar_credit.Rds
#         built/credit_asv.Rds
#         raw/dados_car_brasil.csv
# OUTPUT: tabelas Excel (secao 1 a 9) + graficos PNG + CSVs auxiliares
root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "GitHub", "CreditDesmat")
source(file.path(root, "desciptive.R"))

# ============================================================================
# 7) (OPCIONAL) DASHBOARD SHINY
# ============================================================================

# Atencao: os scripts anteriores usam rm(list=ls()) e podem ter sobrescrito a
# variavel `root`. Por isso redefinimos aqui antes de chamar runApp.
code_root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                       "Documents", "GitHub", "CreditDesmat")
# Descomente a linha abaixo para abrir o dashboard:
# shiny::runApp(file.path(code_root, "shiny"))

# ============================================================================
# FIM
# ============================================================================
cat("Pipeline finalizado com sucesso\n")

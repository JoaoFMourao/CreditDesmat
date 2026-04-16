# MASTER SCRIPT ---------------------------------------------------------------
# Projeto: Analise MCR / Credito Rural / CAR
# Autor: Joao Mourao
# Objetivo: rodar pipeline completo do projeto

# ---------------------------------------------------------------------------
# ORDEM DE EXECUCAO
# ---------------------------------------------------------------------------
# 1) preparar base SICOR (dados brutos → base consolidada)
# 2) merge com complementos (municipio + inflacao)
# 3) merge com CAR e ASV
# 4) exercicio da borda (INPE)
# 5) analise descritiva final

# ---------------------------------------------------------------------------
# CAMINHO BASE
# ---------------------------------------------------------------------------
root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "baseMCR/code")

# ============================================================================
# 1) PREPARAR BASE SICOR
# ============================================================================

# INPUT:
# - raw/sicor/*.gz (arquivos anuais do SICOR)
#
# OUTPUT:
# - clean/sicor_main_2018_2026.Rds

source(file.path(root,"r2c","1_prepare_sicor_main.R"))

# ============================================================================
# 2) MERGE COM COMPLEMENTO BASICO + IPCA
# ============================================================================

# INPUT:
# - clean/sicor_main_2018_2026.Rds
# - raw/sicor/complementos/SICOR_COMPLEMENTO_OPERACAO_BASICA.gz
# - raw/ipeadata.csv (inflacao)
#
# OUTPUT:
# - clean/sicor_main_2018_2026_basic_complement.Rds

source(file.path(root,"r2c" ,"2_merge_comp_basic.R"))

# ============================================================================
# 3) EXERCICIO DE BORDA (INPE)
# ============================================================================

# INPUT:
# - raw/lista_mcr_biomas_VF.gdb
# - raw/prodes_mcr.gdb
#
# OUTPUT:
# - output/INPEs_exercise/changedImpact.rds

source(file.path(root, "INPEs_exersice.R"))


# ============================================================================
# 4) MERGE COM CAR + ASV + MONITORAMENTO
# ============================================================================

# INPUT:
# - clean/sicor_main_2018_2026_basic_complement.Rds
# - raw/sicor/complementos/SICOR_PROPRIEDADES.gz
# - output/INPEs_exercise/changedImpact.rds (ASV + CAR)
# - raw/Fontes MCR.csv (linhas monitoradas)
#
# OUTPUT:
# - built/asvCar_credit.Rds
# - built/credit_asv.Rds
# - built/properties_asv.Rds

source(file.path(root,"built" ,"sicor_per_car.R"))


# ============================================================================
# 5) ANALISE DESCRITIVA FINAL
# ============================================================================

# INPUT:
# - built/asvCar_credit.Rds
# - built/credit_asv.Rds
# - raw/dados_car_brasil.csv
#
# OUTPUT:
# - tabelas Excel (secao 1 a 5)
# - graficos
# - CSVs auxiliares

source(file.path(root, "desciptive.R"))

# ============================================================================
# FIM
# ============================================================================

cat("Pipeline finalizado com sucesso\n")
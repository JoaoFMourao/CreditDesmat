# SETUP ------------------------------------------------------------------------
rm(list=ls())
gc()

strt.time <- Sys.time()

library(tidyverse)
library(janitor)
library(data.table)
library(lubridate)

root <- file.path("C:/Users", Sys.getenv("USERNAME"),
                  "Documents", "baseMCR/dados")

# Remove Scientific Notation
options(scipen = 999)

# Sources
#https://www.bcb.gov.br/estabilidadefinanceira/creditorural?modalAberto=tabelas_sicor


#    Load SICOR data files -----------------------------------------------------

## Load full database ####
df <- readRDS(file.path(root,"clean", "sicor_main_2018_2026_basic_complement.Rds"))

df <- df %>% 
  select(
    ref_bacen, nu_ordem, ano_base, cd_fonte_recurso,vl_parc_credito, ano,mes,
    ano_safra,vl_parc_credito_real, is_basic
  )
  
aux <- df %>% slice_sample(n = 10.000)

## Load property data ####

input <- file.path(root,"raw/sicor",
                   "complementos")

properties <-  read.delim(file.path(input, "SICOR_PROPRIEDADES.gz"),
                     sep = ";",
                     header = TRUE,
                     encoding = "UTF-8")  

## Load asv-car data ####

asvCar <- readRDS(file.path(root,"output","INPEs_exercise",
                            "changedImpact.rds"))

## Load linhas Monitoradas ####

fonteMonitor <- fread(
  file.path(
    input,"Fontes MCR 6-1-2 e 6-7-7.csv"
    ),  encoding = "Latin-1"
  )


# DATA HANDLING ####
## prep properties ####
properties <- properties %>% 
  clean_names()%>%
  rename(ref_bacen = x_ref_bacen,
         cod_imovel = cd_car) %>% 
  mutate(
    tookCredit = "Tomou Credito"
  ) #%>% 
  # filter(cod_imovel != -1)

## check for duplicates ####
# Check number of contracts in the main file
# properties %>%
#   distinct(ref_bacen, nu_ordem,cod_imovel) %>%
#   # filter(cod_imovel != -1) %>%
#   nrow() == length(properties$ref_bacen)# DUPLICATES

#check for duplicates in asvCar file
# asvCar %>% 
#   distinct(cod_imovel) %>% 
#   nrow() == length(asvCar$cod_imovel)

## remove "-" form codigo car ####
asvCar <- asvCar %>% 
  mutate(
    cod_imovel = str_remove_all(cod_imovel,"-"),
    apresenteASV = "Apresente ASV"
  ) %>%
  # filter(
  #   condicao != "Cancelado por decisão administrativa",
  #   tipo_imove == "IRU"
  # ) %>%
  select(cod_imovel,soma_desmat,uf,biome,criterio_new,apresenteASV,
         area_total_ha, m_fiscal) %>% 
  mutate(quinzeMf = ifelse(m_fiscal >= 15, 1,0)) %>% 
  distinct(cod_imovel, .keep_all = TRUE)


## clean fonte monitor ####
fonteMonitor <- fonteMonitor %>% 
  clean_names() %>% 
  rename(
    cd_fonte_recurso = number_codigo
  ) %>% 
  mutate(
    monitored = 1
  )


# MERGE ####
## merge properties with car that need to present asv ####
properties <- properties %>% 
  left_join(asvCar, by = "cod_imovel") 

gc()

### get monitored data ####
df <- df %>% 
  left_join(
    fonteMonitor,
    by = "cd_fonte_recurso"
  ) %>% 
  mutate(
    monitored = ifelse(
      is.na(monitored),
      0,
      1
    )
  ) 

# tmp <- df %>% group_by(monitored,ano_safra) %>% 
#   dplyr::summarise(
#     vl_parc_credito = sum(vl_parc_credito)/10^9,
#     vl_parc_credito_real = sum(vl_parc_credito_real)/10^9,
#     ops = n()
#   )
# 
# tmp
# 
# tmp2 <- df %>% group_by(ano_safra) %>% 
#   dplyr::summarise(
#     vl_parc_credito = sum(vl_parc_credito)/10^9,
#     vl_parc_credito_real = sum(vl_parc_credito_real)/10^9,
#     ops = n()
#   )
# 
# tmp2
# 
# tmp3 <- df %>% group_by(ano_safra, mes,ano) %>% 
#   dplyr::summarise(
#     vl_parc_credito = sum(vl_parc_credito)/10^9,
#     vl_parc_credito_real = sum(vl_parc_credito_real)/10^9,
#     ops = n()
#   )
# 
# tmp3 %>% filter(ano_safra == "2024/2025")
# 
# tmp4 <- df %>% filter(ano_safra == "2024/2025") %>% 
#   group_by(ano_safra, cd_fonte_recurso) %>% 
#   dplyr::summarise(
#     vl_parc_credito = sum(vl_parc_credito)/10^9,
#     vl_parc_credito_real = sum(vl_parc_credito_real)/10^9,
#     ops = n()
#   ) %>% 
#   left_join(
#     fonteMonitor %>%  select(-monitored)
#   ) %>% arrange(descricao)

### get operation level data on car that needs to present asv ####
aux <- properties %>% 
  filter(cod_imovel == "-1") %>%  head(10000) %>%
  bind_rows(
    properties %>% filter(cod_imovel != "-1") %>% head(10000)
  ) 


df_asv <- properties %>% 
  #   filter(cod_imovel == "-1") %>%  tail(10000) %>%
  # bind_rows(
  #   properties %>% filter(cod_imovel != "-1") %>% tail(10000)
  # )  %>% 
  filter(ref_bacen %in% df$ref_bacen) %>%
  select(ref_bacen,nu_ordem,apresenteASV,cod_imovel, criterio_new,
         quinzeMf) %>% 
  mutate(
    apresenteASV =     ifelse(
      apresenteASV == "Apresente ASV",
      1,
      0
    ),
    haCAR = ifelse(
      cod_imovel == "-1",
      0,
      1
    ),
    
    salvoBorda = ifelse(
      criterio_new == "Salvo pela borda",
      1,
      0
    )
  ) %>% 
  group_by(ref_bacen, nu_ordem) %>% 
  dplyr::summarise(
    apresenteASV = sum(apresenteASV, na.rm = T),
    n_car = sum(haCAR),
    quinzeMf = sum(quinzeMf, na.rm = T),
    salvoBorda = ifelse(
      n_car == sum(salvoBorda),
      1,
      0
    )
  ) 

gc()

# Monitored data ####
gc()
df <- df %>%
  filter(
    ano_safra %in%
      c("2020/2021","2021/2022","2022/2023","2023/2024",
        "2024/2025")
  ) %>%
  left_join(
    df_asv,
    by = join_by(ref_bacen, nu_ordem)
  )   %>% 
  mutate(
    apresenteASV = case_when(
      n_car == 0 ~                     "Sem CAR associado",
      n_car != 0 & apresenteASV > 0 ~  "Apresente ASV",
      n_car != 0 & apresenteASV == 0 ~ "Sem desmatamento",
      is.na(apresenteASV)            ~ "Dado sigiloso"
    )
  )


 
unique.prop <-  properties %>% 
  select(cod_imovel,tookCredit) %>% 
  distinct()

## merge asvCAR with properties ####

asvCar <- asvCar %>% 
  left_join(unique.prop, by = "cod_imovel") %>% 
  mutate(
    tookCredit = ifelse(
      is.na(tookCredit),
      "Nao afetado",
      tookCredit
    ) 
  )

summaryAsvCar <- asvCar %>% 
  group_by(tookCredit) %>% 
  dplyr::summarise(
    desmat_km = round(sum(soma_desmat)/100),
    properties = n(),
    area_ha = sum(area_total_ha)
  ) 

summaryDf <- df %>% 
  filter(ano_safra == "2024/2025") %>% 
  group_by(monitored, apresenteASV) %>%
    dplyr::summarise(
      credit_bi = sum(vl_parc_credito)/10^9
    )  %>% 
  pivot_wider(
    names_from = apresenteASV,
    values_from = credit_bi)
    

summaryDf_ops <- df %>% 
  filter(ano_safra == "2024/2025") %>% 
  group_by(monitored, apresenteASV) %>%
  dplyr::summarise(
    ops = n()
  ) %>% 
  pivot_wider(
    names_from = apresenteASV,
    values_from = ops)

  
# SAVE FILES ####
  
  dir.create(file.path(root,"built"))
  
  saveRDS(asvCar, file.path(root,"built", "asvCar_credit.Rds"))
  
  saveRDS(df, file.path(root,"built", "credit_asv.Rds"))
  
  saveRDS(properties, file.path(root,"built", "properties_asv.Rds"))  
  
  #
  write.csv(asvCar,
            file.path(root,"built", "asvCar_credit.csv"),
            row.names = FALSE)
  
  write.csv(df %>% arrange(-vl_parc_credito_real) %>%  head(1000),
            file.path(root,"built", "credit_asv.csv"),
            row.names = FALSE)
  
  write.csv(properties  %>%  head(1000),
            file.path(root,"built", "properties_asv.csv"),
            row.names = FALSE)
  
  
  Sys.time() - strt.time

# %>% 
#   ungroup() %>% 
#   mutate(
#     desmat_shr = round((desmat_km/sum(desmat_km))*100,0),
#     properties_shr = round((properties/sum(properties))*100,0)
#   ) %>% 
#   select(tookCredit,properties,properties_shr,desmat_km,desmat_shr)

asvCar %>% 
  group_by(tookCredit) %>% 
  dplyr::summarise(
    desmat_km = sum(soma_desmat)/100,
    properties = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    properties_shr = round(100*properties/length(unique.prop$tookCredit),2)
  )


asvCar %>% 
  group_by(tookCredit,biome) %>% 
  dplyr::summarise(
    desmat_km = sum(soma_desmat)/100,
    properties = n()
  ) %>% 
  group_by(tookCredit) %>% 
  mutate(
    desmat_shr = round((desmat_km/sum(desmat_km))*100,0),
    properties_shr = round((properties/sum(properties))*100,0)
  ) %>% 
  select(tookCredit,biome,properties,properties_shr,desmat_km,desmat_shr) %>% 
  arrange(biome) #%>% 
  # filter(tookCredit == "Tomou Credito") %>% 
  # arrange(-p)


# asvCar %>% 
#   group_by(tookCredit, tipo_imove) %>% 
#   dplyr::summarise(
#     desmat_km = sum(soma_desmat)/100,
#     properties = n()
#   ) %>% 
#   ungroup() %>% 
#   mutate(
#     desmat_shr = round((desmat_km/sum(desmat_km))*100,0),
#     properties_shr = round((properties/sum(properties))*100,0)
#   ) %>% 
#   select(tookCredit,tipo_imove,properties,properties_shr,desmat_km,desmat_shr) %>% 
#   arrange(tipo_imove)


# SAVE DATA ####




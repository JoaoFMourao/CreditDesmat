# HEADER ####
# Script para explorar bases .gdb, inspecionar conteúdo
# e testar exclusão da borda interna de 60m no cálculo do desmatamento
# para o município de Altamira (PA)

#Time difference of 9.896843 mins

# SET-UP ####
rm(list = ls())
gc()

real.strt.time <-Sys.time()

library(sf)
library(dplyr)
library(geobr)
library(stringr)
library(ggplot2)
library(purrr)

sf::sf_use_s2(FALSE)

## config relative paths ####
input  <- file.path("baseMCR", "dados", "raw")
output <- file.path("baseMCR", "dados", "output", "INPEs_exercise")

if (!dir.exists(output)) {
  dir.create(output, recursive = TRUE)
}

## paths to gdb ####
gdb_1 <- file.path(input, "lista_mcr_biomas_VF.gdb")
gdb_2 <- file.path(input, "prodes_mcr.gdb")


# LOAD DATA ####
## Explore Layers ####
layers_gdb_1 <- st_layers(gdb_1)
layers_gdb_2 <- st_layers(gdb_2)

print(layers_gdb_1)
print(layers_gdb_2$name)


## Actually read layers and convert crs MCR
lista_mcr <- lapply(layers_gdb_1$name, function(x) {
  st_read(dsn = gdb_1, layer = x, quiet = FALSE)
}) %>% 
  bind_rows() %>% 
  st_transform(102033) %>% 
  st_make_valid()

## carregar PRODES e empilhar ####
prodes_list <- lapply(layers_gdb_2$name, function(x) {
  st_read(dsn = gdb_2, layer = x, quiet = FALSE)
})

prodes_all <- bind_rows(prodes_list) %>% 
  st_transform(102033) %>% 
  st_make_valid()


# DATA HANDLING ####

## Clean duplicates ####
lista_mcr <- lista_mcr %>% 
  distinct(cod_imovel, .keep_all = TRUE)

## select variables e crop para Altamira ####

impacted.properties_org <- lista_mcr %>% 
  select(cod_imovel, soma_desmat, dentro_criterio,status_imo,
         criterio_aplicado, tipo_imove,uf,municipio,
         cod_munici,condicao,area_total_ha,m_fiscal) %>% 
  filter(
    status_imo != "SU",
    condicao != "Cancelado por decisão administrativa"
    ) %>% 
  st_make_valid()  
  

# lista_mcr %>% 
#   as_tibble() %>% 
#   select(-Shape) %>% 
#   group_by(uf) %>% 
#   summarise(area = sum(area)/10^6,
#             area_total_ha = sum(area_total_ha)/10^6,)

head(impacted.properties_org)

## alternative prodes-car ####
# prodes_alt <- impacted.properties_org %>% 
#   st_intersection(
#     prodes_all %>% select(Shape)
#   ) %>% 
#   mutate(
#     prodes_alt = as.numeric(st_area(Shape))/10000
#   ) %>% 
#   as_tibble() %>% 
#   group_by(cod_imovel) %>% 
#   summarise(prodes_alt = sum(prodes_alt))
# 
# prodesAlt_tot <- sum(prodes_alt$prodes_alt)
# prodesOrg_tot <- sum(impacted.properties_org$soma_desmat)



# TESTE DA BORDA INTERNA DE 60 METROS ####


borda_60m <- impacted.properties_org %>%
  mutate(
    Shape = map2(Shape, st_buffer(Shape, -60), st_difference) %>%
      st_sfc(crs = st_crs(impacted.properties_org))
  ) 

## cross prodes with borda ####

str.date <- Sys.time()

bordaProdes <- borda_60m %>% 
  st_intersection(prodes_all) %>% 
  mutate(
    area_ha_naBorda = as.numeric(st_area(Shape))/10000
  ) %>% 
  as_tibble() %>% 
  group_by(cod_imovel) %>% 
  summarise(
    desmatBorda_ha = sum(area_ha_naBorda)
  )

Sys.time() - str.date

join.strt <- Sys.time()

changedImpact <- impacted.properties_org %>%
  as_tibble() %>% 
  select(-Shape) %>% 
  left_join(bordaProdes) %>% 
  mutate(
    desmatBorda_ha = ifelse(is.na(desmatBorda_ha),0,desmatBorda_ha),
    desmatNew = soma_desmat - desmatBorda_ha
  )  %>% 
  mutate(
    biome = criterio_aplicado,
    
    criterio_aplicado = case_when(
      criterio_aplicado == "AMAZÔNIA" ~ 6.25,
      str_detect(criterio_aplicado,"CAATINGA|PAMPA|MATA ATLÂNTICA") ~ 2,
      str_detect(criterio_aplicado,"CERRADO|PANTANAL") ~ 5
    ),
    
    criterio_new = ifelse(
      desmatNew < criterio_aplicado,
      "Salvo pela borda",
      "Apresente ASV"
    )
  )

saveRDS(
  changedImpact,
  file.path(output, "changedImpact.rds")
)


Sys.time() - real.strt.time


# table(changedImpact$dentro_criterio,changedImpact$criterio_new)
# table(changedImpact$biome,changedImpact$criterio_new)
# # 
# asvCar %>%
#   ungroup() %>% 
#   # group_by(criterio_new) %>%
#   dplyr::summarise(
#     # desmat_ha = sum(soma_desmat),
#     desmat_km = sum(soma_desmat)/100,
#     properties = n(),
#     area_total_Mha = sum(area_total_ha)/10^6
#   )
# # 
# changedImpact %>% 
#   group_by(tipo_imove) %>% 
#   dplyr::summarise(
#     desmat_ha = sum(soma_desmat),
#     desmat_km = sum(soma_desmat)/100,
#     properties = n()
#   ) %>% 
#   arrange(tipo_imove)
# 
# changedImpact %>% 
#   group_by(criterio_new,biome) %>% 
#   dplyr::summarise(
#     desmat_ha = sum(soma_desmat),
#     desmat_km = sum(soma_desmat)/100,
#     properties = n()
#   ) %>% 
#   arrange(biome)
# 
# 
# 
# 
# car <- lista_mcr %>% filter(
#   cod_imovel == "BA-2911105-2F1DBE03818B42B9AEA07E5A4587549E"
# )
# 
# borda <- borda_60m %>% filter(
#   cod_imovel == "BA-2911105-2F1DBE03818B42B9AEA07E5A4587549E")
# 
# desmatBorda <- bordaProdes %>% filter(
#   cod_imovel == "BA-2911105-2F1DBE03818B42B9AEA07E5A4587549E")
# 
# prodesOriginal <- prodes_all %>% 
#   st_intersection(car %>% select(Shape))%>% 
#   mutate(
#     prodes_car_ha = as.numeric(st_area(Shape))/10000
#   ) 
# 
# prodesOriginal_union <- prodesOriginal %>% 
#   st_union() 
# 
# 
# sum(prodesOriginal$prodes_car_ha)
# sum(st_area(prodesOriginal_union))/10^4
# 
# 
# sum(prodesOriginal$prodes_car_ha)
# 
# ggplot()+
#   # geom_sf(data = car, fill = NA, color = "black") +
#   # geom_sf(data = borda, fill = NA, color = "blue") +
#   # geom_sf(data = desmatBorda, fill = NA, color = "red") +
#   geom_sf(data = prodesOriginal,fill = NA, color = "orange")+
#   # geom_sf(data = prodesOriginal_union, fill = NA, color = "pink") +
#   theme_light()
# 
# 
# for (j in 1:length(prodesOriginal$fid_1)) {
# 
# for (i in 1:length(prodesOriginal$fid_1)) {
#   
#   if(st_intersects(prodesOriginal[j,],prodesOriginal[i,], sparse = F) &
#      i != j){
#   
#   print(paste(j,i))
# 
#   }
#   
#   
# }
#   print(paste(st_area(prodesOriginal[j, ])/10^5,j))
# }
# 
# st_union(prodesOriginal) %>% st_area()/10^5
# 
# 
# ggplot()+
#   geom_sf(data = prodesOriginal[2,], fill = NA, color = "red")+
#   geom_sf(data = prodesOriginal[5,], fill = NA, color = "blue")+
#   theme_light()

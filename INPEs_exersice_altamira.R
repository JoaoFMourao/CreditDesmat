# HEADER ####
# Script para explorar bases .gdb, inspecionar conteúdo
# e testar exclusão da borda interna de 60m no cálculo do desmatamento
# para o município de Altamira (PA)

# SET-UP ####
rm(list = ls())
gc()

real.strt.time <-Sys.time()

library(lwgeom)

library(sf)
library(dplyr)
library(geobr)
library(stringr)
library(ggplot2)
library(purrr)

sf::sf_use_s2(TRUE)

## config relative paths ####
input  <- file.path("baseMCR", "dados", "raw")
output <- file.path("baseMCR", "dados", "output", "INPEs_exercise","Altamira")

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
print(layers_gdb_2)

## Define relevant layers ####

prodes_layers <- c(
  "amaz_d2019",
  "amaz_d2020",
  "amaz_d2021",
  "amaz_d2022",
  "amaz_d2023",
  "amaz_d2024",
  "amaz_d2025"
)

lista_layer <- "lista_mcr_amazonia_VF"

## Actually read layers and convert crs MCR
lista_mcr <- st_read(dsn = gdb_1,
                     layer = lista_layer,
                     quiet = FALSE) %>% 
  
  # st_transform(5880) %>% 
  st_transform(102033) %>% 
  
  st_make_valid()

## carregar PRODES e empilhar
prodes_list <- lapply(prodes_layers, function(x) {
  st_read(dsn = gdb_2, layer = x, quiet = FALSE)
})

names(prodes_list) <- prodes_layers

prodes_all <- bind_rows(prodes_list) %>% 
  # st_transform(5880) %>% 
  st_transform(102033) %>% 
  st_make_valid()


## Load Altamira ####

altamira <- st_read(dsn = "geral/BR_Municipios_2024", quiet = TRUE) %>% 
  filter(CD_MUN == "1500602") %>% 
  select(geometry) %>% 
  # st_transform(5880) %>% 
  st_transform(102033) %>% 
  st_make_valid()

# DATA HANDLING ####

## Clean duplicates ####
aux <- table(lista_mcr$cod_imovel, useNA = "ifany")
aux <- sort(aux, decreasing = TRUE)

duplicated_cars <- names(aux[aux > 1])

lista_mcr <- lista_mcr %>% 
  filter(!(cod_imovel %in% duplicated_cars))

## select variables e crop para Altamira ####

impacted.properties_org <- lista_mcr %>% 
  select(cod_imovel, soma_desmat, dentro_criterio,
         criterio_aplicado) %>% 
  filter(dentro_criterio == "não") %>% 
  st_make_valid() %>% 
  st_intersection(altamira) %>% 
  st_make_valid() %>% 
  mutate(
    area_prop = as.numeric(st_area(Shape))
  )

sum(impacted.properties_org$area_prop)/1000000

head(impacted.properties_org)

## Crop Prodes ####
prodes_altamira <- prodes_all %>% 
  # st_union() %>% 
  # st_make_valid() %>%
  st_intersection(altamira) %>% 
  st_make_valid() %>% 
  mutate(
    area_prodes = as.numeric(st_area(Shape))/10000
  )

## alternative prodes-car ####
prodes_alt <- impacted.properties_org %>% 
  st_intersection(
  prodes_altamira %>% select(Shape)
) %>% 
  mutate(
    prodes_alt = as.numeric(st_area(Shape))/10000
  ) %>% 
  as_tibble() %>% 
  group_by(cod_imovel) %>% 
  summarise(prodes_alt = sum(prodes_alt))

sum(prodes_alt$prodes_alt)
sum(impacted.properties_org$soma_desmat)

# TESTE DA BORDA INTERNA DE 60 METROS ####


borda_60m <- impacted.properties_org %>%
  mutate(
    Shape = map2(Shape, st_buffer(Shape, -60), st_difference) %>%
      st_sfc(crs = st_crs(impacted.properties_org)),
    
    area_ha_borda = as.numeric(st_area(Shape))/10.000
  ) 



## cross prodes with borda ####

str.date <- Sys.time()
bordaProdes <- borda_60m %>% 
  st_intersection(prodes_altamira) 

bordaProdes_tbl <- bordaProdes %>% 
  mutate(
    desmat_naBorda = as.numeric(st_area(Shape))/10000
  ) %>% 
  as_tibble() %>% 
  group_by(cod_imovel) %>% 
  summarise(
    desmatBorda_ha = sum(desmat_naBorda)
  )

sum(impacted.properties_org$area_prop)/1000000
sum(borda_60m$area_ha_borda)/1000000
sum(prodes_altamira$area_prodes)/1000000
sum(bordaProdes$desmatBorda_ha)/1000000
sum(bordaProdes$desmat_naBorda)/1000000




Sys.time() - str.date

Sys.time() - real.strt.time

changedImpact <- impacted.properties_org %>%
  as_tibble() %>% 
  select(-Shape) %>% 
  left_join(bordaProdes_tbl) %>% 
  mutate(
    desmatBorda_ha = ifelse(is.na(desmatBorda_ha),0,desmatBorda_ha),
    desmatNew = soma_desmat - desmatBorda_ha
  ) %>% 
  mutate(
    biome = criterio_aplicado,
    
    criterio_aplicado = case_when(
      criterio_aplicado == "AMAZÔNIA" ~ 6.25,
      criterio_aplicado == "CAATINGA" ~ 2,
      criterio_aplicado == "CERRADO" ~ 5,
      criterio_aplicado == "MATA ATLÂNTICA" ~ 2,
      criterio_aplicado == "PAMPA" ~ 2,
      criterio_aplicado == "PANTANAL" ~ 5 
    ),
    
    criterio_new = ifelse(
      desmatNew < criterio_aplicado,
      "Salvo pela borda",
      "Apresente ASV"
    ),
    
    desmatChange = soma_desmat - desmatNew
  ) 

summary(changedImpact$soma_desmat)
summary(changedImpact$desmatBorda_ha)


table(changedImpact$dentro_criterio,changedImpact$criterio_new)

changedImpact %>% 
  group_by(criterio_new) %>% 
  dplyr::summarise(
    desmat_ha = sum(soma_desmat),
    desmat_km = sum(soma_desmat)/100,
    properties = n()
  )

summary(changedImpact)

## Save as RDS as well, to preserve classes and make re-loading faster in R
saveRDS(
  changedImpact,
  file.path(output, "changedImpact.rds")
)

car <- lista_mcr %>% filter(
  cod_imovel == "PA-1500602-37856FE50B9C47F5840EC5CF41E1F82A"
)

borda <- borda_60m %>% filter(
  cod_imovel == "PA-1500602-37856FE50B9C47F5840EC5CF41E1F82A")

desmatBorda <- bordaProdes %>% filter(
  cod_imovel == "PA-1500602-37856FE50B9C47F5840EC5CF41E1F82A")

prodesOriginal <- prodes_altamira %>% 
  st_intersection(car) %>% 
  mutate(
    prodes_car_ha = as.numeric(st_area(Shape))/10000
  )

sum(prodesOriginal$prodes_car_ha)

ggplot()+
  geom_sf(data = car, fill = NA, color = "black") +
  geom_sf(data = borda, fill = NA, color = "blue") +
  geom_sf(data = desmatBorda, fill = NA, color = "red") +
  geom_sf(data = prodesOriginal,fill = NA, color = "orange")+
  theme_light()




table(changedImpact$soma_desmat < 6.25)  
  table(changedImpact$desmatNew < 6.25)

  
  
  

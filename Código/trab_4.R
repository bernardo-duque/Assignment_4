
setwd("C://Users//duque//Desktop//PUC//Mestrado//Verão 1//Estatística//Data Science//Trab 4")

library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(sf)
library(geobr)
library(terra)
library(spData)

# Importando o arquivo 

raster_filepath <- "C://Users//duque//Desktop//PUC//Mestrado//Verão 1//Estatística//Data Science//Trab 4//brasil_coverage_2020.tif"
mapa <- rast(raster_filepath )

# Importando o mapa do estado do Rio pelo geobr

rio <- read_municipality(code_muni = "RJ",year = 2020) %>%
  mutate(ID = 1:n())

# Separando o RJ do resto do mapa e extraindo os valores

cr_rio <- crop(mapa, rio)
rio_rast <- mask(cr_rio, vect(rio))
valor <- extract(rio_rast, vect(rio))

#save(valor, file = "valor_rio.RData")
#load("valor_rio.RData")

# Calculando a cobertura florestal e a área total

cob_flore <- valor %>%
  filter(brasil_coverage_2020 %in% c(1,3,4,5,49)) %>%
  group_by(ID) %>%
  summarise(floresta = n())

cob_total <- valor %>%
  group_by(ID) %>%
  summarise(total = n())

# Juntando os dois cômputos

cobertura <- cob_flore %>%
  left_join(cob_total) %>%
  mutate(for_share = round((floresta/total)*100,2))

# Juntando o resultado com as localizacoes dos municipios para o plot

rio <- rio %>%
  select(ID,geom) %>%
  left_join(cobertura)

# Plotando e salvando o mapa que responde a pergunta "Qual a proporção florestal por município no Rio de Janeiro?"

grDevices::png(filename = "media_municipios.png",width = 800,height = 600)

rio %>%
  ggplot() +
  geom_sf(aes(fill=for_share), alpha = 0.9, col="white") +
  scale_fill_gradient(name = "Proporção (%)", labels = scales::comma,low = "lightgreen",high = "darkgreen",na.value = "grey") + 
  labs(title = "Cobertura Florestal por Município do Rio de Janeiro") +
  theme(plot.title = element_text(face = "bold"))

dev.off()



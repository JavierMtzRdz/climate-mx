##################################################################
##        Proyecto: Web scraping de informción del SNIA          ##
##################################################################
##
## Descripción:    Extracción de información del SNIA
##                 
##
## Autor:          Javier Mtz.  
##
## Fecha creac.:   2021-10-08
##
## Email:          
##
## ---------------------------
## Notas:          
## ---------------------------

# Setup ----
## Paquetes a utilizar ----
pacman::p_load(tidyverse, janitor, writexl, 
               readxl, scales, jsonlite)

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)

library(jsonlite)

fechas_ext <- seq(as.Date("1991-01-01"), Sys.Date(),
                  "1 month")
fechas <- sort(c(fechas_ext, 
            update(fechas_ext, day = 15)))

for (i in fechas) {
  date <- as_date(i)
  print(date)
  mydata <- fromJSON(paste0("https://sinav30.conagua.gob.mx:8080/PresasPG/presas/reporte/", date)) %>% 
    tibble()
  
  write_csv(mydata,
            paste0("01_data-raw/reports-presas/reporte_pesas_",
                   date,
            ".csv"))
  
}

# Aglutinar data ----
files <- list.files("01_data-raw/reports-presas",
                    full.names = T)

presas_data <- map_df(files,
                      ~read_csv(.x) %>% 
                        mutate(alturacortina = as.numeric(alturacortina),
                               elevcorona = as.numeric(elevcorona),
                               inicioop = as.numeric(inicioop)))

presas_data %>% 
  write_csv("04_data-prcssd/data-presas.csv")




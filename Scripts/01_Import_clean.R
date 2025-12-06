#------------------------
#1 Construcción del Dataset
#------------------------
#install.packages("writexl")
packs <- c("tidyverse","janitor","skimr","naniar","DataExplorer","ggplot2","openxlsx")
new_packs <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new_packs)) install.packages(new_packs)
lapply(packs, library, character.only = TRUE)

library(openxlsx)
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(naniar)

setwd("C:/Users/scars/Desktop/Ciencia de Datos/TP_Final_Ciencia_de_datos_2C2025")

#1.1 Introducimos WGI
wgi <- read_excel("Data/Raw/wgidataset.xlsx")

#1.2 Clean_WGI
YEARS <- c(2000, 2010, 2020)
wgi_clean <- wgi |>
  filter(year %in% YEARS,
         indicator %in% c("ge","rq","cc","rl")) |>
  select(
    iso3c = code,
    country = countryname,
    year,
    indicator,
    estimate
  ) |>
  pivot_wider(
    names_from = indicator,
    values_from = estimate
  ) |>
  rename(
    gov_effectiveness   = ge,
    regulatory_quality  = rq,
    control_corruption  = cc,
    rule_of_law         = rl
  )

#1.3 Import PIB per cápita PPP
pib_ppp <- read_excel(
  "Data/Raw/API_NY.GDP.PCAP.PP.KD_DS2_en_excel_v2_115172_PIB_pc_PPP.xlsx",
  skip = 2
)

#1.4 Clean PIB per cápita PPP  
pib_ppp_clean <- pib_ppp %>%
  # Eliminar filas sin código de país
  filter(!is.na(`Country Code`)) %>%
  
  # Pasar de ancho → largo
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "gdp_percap_ppp"
  ) %>%
  
  # Convertir año a número
  mutate(year = as.numeric(year)) %>%
  
  # Quedarnos sólo con los 3 años del TP
  filter(year %in% YEARS) %>%
  
  # Seleccionar columnas útiles
  select(
    iso3c = `Country Code`,
    year,
    gdp_percap_ppp
  )

head(pib_ppp)

#1.5 Import DFI
fdi <- read_excel("Data/Raw/API_BX.KLT.DINV.WD.GD.ZS_DS2_en_excel_v2_129612_Inver_Extranjera_Directa.xlsx", skip=2)

head(fdi)

#1.6 Clean FDI
fdi_clean <- fdi |> 
  # 1) borrar filas sin Country Code (metadata)
  filter(!is.na(`Country Code`)) |> 
  
  # 2) pasar de ancho a largo todas las columnas que son AÑOS
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "fdi_pct_gdp"
  ) |> 
  
  # 3) convertir year a número
  mutate(year = as.numeric(year)) |> 
  
  # 4) filtrar años relevantes
  filter(year %in% c(2000, 2010, 2020)) |> 
  
  # 5) renombrar código de país
  rename(iso3c = `Country Code`) |> 
  
  # 6) quedarnos solo con columnas útiles
  select(iso3c, year, fdi_pct_gdp)

head(fdi_clean)


#1.7 Merge Final
df_final <- wgi_clean |> 
  left_join(pib_ppp_clean, by = c("iso3c","year")) |>
  left_join(fdi_clean, by = c("iso3c","year"))

head(df_final)

# --------------------------------------------
#2 Limpieza
# --------------------------------------------

#2.1 Convertir columnas WGI a numéricas
wgi_vars <- df_final %>% 
  select(matches("corruption|effectiveness|law|quality|stability|accountability")) %>% 
  names()

df_final <- df_final %>%
  mutate(across(all_of(wgi_vars), ~na_if(.x, ".."))) %>% 
  mutate(across(all_of(wgi_vars), ~as.numeric(.x)))

str(df_final[, c("iso3c", "country")])

#2.2 Datos Faltantes (NA)

#Porcentaje de NAs por columna
na_percent <- sapply(df_final, function(x) mean(is.na(x)) * 100)

#Lo paso a data.frame ordenado
na_table <- data.frame(
  variable = names(na_percent),
  pct_na = round(na_percent, 2)
) %>% arrange(desc(pct_na))

na_table

#Gráficos

gg_miss_var(df_final)
vis_miss(df_final)
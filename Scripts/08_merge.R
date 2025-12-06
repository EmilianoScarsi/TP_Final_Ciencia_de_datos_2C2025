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

write_xlsx(df_final, "Data/Processed/df_final.xlsx")

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

write_xlsx(na_table, "Data/Processed/na_table.xlsx")

#Gráficos

gg_miss_var(df_final)
ggsave("output/figures/gg_mis_var.jpg")

vis_miss(df_final)
ggsave("output/figures/missing_heatmap.jpg")

#------------------------------
#3 EDA
#------------------------------

out_dir <- "Output/Tables"

#dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

moda <- function(x) {
  x <- na.omit(x)
  if(length(x)==0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

is_date_like <- function(x) {
  if(inherits(x, "Date") || inherits(x, "POSIXt")) return(TRUE)
  s <- na.omit(as.character(x))
  if(length(s) == 0) return(FALSE)
  s <- head(s, 50)
  parsed <- suppressWarnings(lubridate::parse_date_time(s, orders = c("Ymd","dmy","mdy","ymd HMS","ymd HM")))
  sum(!is.na(parsed)) / length(s) > 0.6
}

#3.1 Estructura general
structure_txt <- file.path(out_dir, "01_structure.txt")
sink(structure_txt)
cat("=== ESTRUCTURA (str) ===\n\n")
str(df_final)
cat("\n\n=== DIMENSIONES ===\n")
cat("Filas:", nrow(df_final), " Columnas:", ncol(df_final), "\n\n")
cat("=== NOMBRE DE COLUMNAS ===\n")
print(names(df_final))
sink()

# Función auxiliar para detectar fechas
is_date_like <- function(x) {
  inherits(x, c("Date", "POSIXct", "POSIXlt"))
}

sapply(df_final, function(x) class(x))

# 3.2 Tipos de datos, n_unicos, NAs, fechas
datos_info <- tibble(
  variable = names(df_final),
  class = sapply(df_final, function(x) paste(class(x), collapse = ", ")),
  n_unique = sapply(df_final, function(x) length(unique(x))),
  n_na = sapply(df_final, function(x) sum(is.na(x))),
  pct_na = round(100 * sapply(df_final, function(x) mean(is.na(x))), 2),
  is_numeric = sapply(df_final, is.numeric),
  is_character = sapply(df_final, is.character),
  is_factor = sapply(df_final, is.factor),
  is_date_like = sapply(df_final, is_date_like)
)

write.xlsx(datos_info, file.path(out_dir, "02_datos_info.xlsx"), overwrite = TRUE)

#3.3 Primeras observaciones
write.xlsx(head(df_final, 30), file.path(out_dir, "03_head_30.xlsx"), rowNames = FALSE)
rows_with_na <- df_final[rowSums(is.na(df_final)) > 0, ]
write.xlsx(head(rows_with_na, 50), file.path(out_dir, "04_rows_with_na_sample.xlsx"), rowNames = FALSE)

#---------------------------
#4 ESTADÍSTICAS DESCRIPTIVAS
#---------------------------

#4.1 Medidas de Tendencia y dispersión

moda <- function(x) {
  x <- na.omit(x)            # ignorar NAs
  ux <- unique(x)            # valores únicos
  ux[which.max(tabulate(match(x, ux)))]  # el que más se repite
}

cols_to_num <- c("gdp_percap_ppp", "rule_of_law", "control_corruption", "gov_effectiveness", "fdi_pct_gdp", "regulatory_quality")
df_final[cols_to_num] <- lapply(df_final[cols_to_num], as.numeric)

numeric_vars <- df_final %>% select(where(is.numeric))

desc_stats <- numeric_vars %>% 
  summarise(across(
    everything(),
    list(
      media = ~mean(.x, na.rm = TRUE),
      mediana = ~median(.x, na.rm = TRUE),
      moda = ~moda(.x),
      sd = ~sd(.x, na.rm = TRUE),
      IQR = ~IQR(.x, na.rm = TRUE),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    )
  ))

desc_stats

#4.2 Visualizaciones Complementarias

# Histograma
ggplot(df_final, aes_string("gdp_percap_ppp")) +
  geom_histogram(bins = 30)

# Histograma
p_hist <- ggplot(df_final, aes(gdp_percap_ppp)) +
  geom_histogram(bins = 30)

# Guardar
ggsave(
  filename = "output/figures/histograma_gdp.jpg",
  plot = p_hist,
  width = 8,
  height = 6,
  dpi = 300
)

#----------------------------------------
#5 ANÁLISIS DE OUTLIERS Y DATOS FALTANTES
#----------------------------------------

#5.1 DETECCIÓN DE OUTLIERS

numeric_vars <- df_final %>% select(where(is.numeric))

detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  tibble(
    Q1 = Q1,
    Q3 = Q3,
    IQR = IQR,
    lower = lower,
    upper = upper,
    n_outliers = sum(x < lower | x > upper, na.rm = TRUE),
    pct_outliers = round(100 * mean(x < lower | x > upper, na.rm = TRUE), 2)
  )
}

outliers_table <- numeric_vars %>%
  summarise(across(everything(), detect_outliers, .names = "{.col}")) %>%
  pivot_longer(everything()) %>%
  unnest(cols = value)

write.xlsx(outliers_table, file.path(out_dir, "06_outliers_table.xlsx"), overwrite = TRUE)


#5.2 DATOS FALTANTES

na_summary <- df_final %>%
  summarise(across(everything(), ~sum(is.na(.)), .names = "na_{.col}")) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  mutate(
    variable = gsub("na_", "", variable),
    pct_na = round(100 * n_na / nrow(df_final), 2)
  ) %>%
  arrange(desc(pct_na))

write.xlsx(na_summary, file.path(out_dir, "07_na_summary.xlsx"), overwrite = TRUE)

# Heatmap (DataExplorer)
create_report <- FALSE # para evitar reportes gigantes

plot_missing <- DataExplorer::plot_missing(df_final)
ggsave(file.path(out_dir, "missing_heatmap.png"), plot_missing, width = 8, height = 6)

#----------------------------------------
#6 EVALUACIÓN DEL IMPACTO DE LA LIMPIEZA
#----------------------------------------

#6.1 Crear datasets comparables

#6 Versión original (sin limpiar) y versión limpia
df_original <- df_final  

#Eliminar NA
df_clean <- df_original %>% drop_na()

#Función para eliminar outliers por IQR
remove_outliers <- function(df) {
  df_out <- df
  numeric_cols <- names(df_out)[sapply(df_out, is.numeric)]
  
  for (v in numeric_cols) {
    x <- df_out[[v]]
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    
    df_out <- df_out %>% 
      filter(.data[[v]] >= lower & .data[[v]] <= upper)
  }
  return(df_out)
}

df_clean <- remove_outliers(df_clean)

write.xlsx(df_clean, file.path("Data/Processed", "df_clean_no_na_no_outliers.xlsx"), overwrite = TRUE)


#6.2 Recalcular estadísticas descriptivas para cada dataset

vars_numeric <- df_original %>% select(where(is.numeric)) %>% names()

calc_stats <- function(df, vars) {
  df %>% summarise(across(
    all_of(vars),
    list(
      media = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      mediana = ~median(.x, na.rm = TRUE),
      IQR = ~IQR(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))
}

stats_original <- calc_stats(df_original, vars_numeric)
stats_clean <- calc_stats(df_clean, vars_numeric)

#6.3 Comparación entre ambas versiones

comparacion <- bind_rows(
  stats_original %>% mutate(version = "original"),
  stats_clean %>% mutate(version = "limpio")
) %>%
  pivot_longer(
    cols = !version,
    names_to = c("variable", "stat"),
    names_sep = "_(?=[^_]+$)",   # separa solo en el último "_"
    values_to = "valor",
    names_transform = list(stat = as.character)
  ) %>%
  pivot_wider(
    names_from = version,
    values_from = valor,
    values_fn = mean   # asegura valores únicos
  ) %>%
  mutate(
    cambio_pct = round(100 * (limpio - original) / original, 2)
  )

write.xlsx(comparacion, file.path(out_dir, "08_paracion_clean_vs_noclean.xlsx"), overwrite = TRUE)


#-------------------------------------
#7 ANÁLISIS DE ESTADÍSTICA INFERENCIAL
#-------------------------------------

#7.1 Test de Hipótesis

df_inf <- df_original %>%
  select(
    gdp_percap_ppp, gov_effectiveness, regulatory_quality,
    control_corruption, rule_of_law
  ) %>%
  drop_na()

vars_corr <- df_inf %>% 
  select(
    gdp_percap_ppp, gov_effectiveness, regulatory_quality, 
    control_corruption, rule_of_law
  )

cor_test_results <- lapply(vars_corr[-1], function(x) {
  cor.test(vars_corr$gdp_percap_ppp, x)
})

cor_test_results

#7.2 Regresión 

modelo <- lm(
  gdp_percap_ppp ~ gov_effectiveness + regulatory_quality +
    control_corruption + rule_of_law,
  data = df_inf
)

summary(modelo)
#install.packages("broom")

library(broom)
library(openxlsx)

tabla_reg <- tidy(modelo)

write.xlsx(tabla_reg, file.path(out_dir, "09_abla_reg.xlsx"), overwrite = TRUE)

#7.3 Prueba Multicolinealidad

#install.packages("car")
library(car)
vif(modelo)

#--------------------------------
#8 VISUALIZACIONES Y STORYTELLING
#--------------------------------

#8.1 Gráfico 1
ggplot(df_inf, aes(x = gov_effectiveness, y = gdp_percap_ppp)) +
  geom_point(alpha = 0.6, color = "orange") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, color = "purple") +
  labs(
    title = "Mayor efectividad del gobierno se asocia con mayor ingreso per cápita",
    subtitle = "Relación entre PIB per cápita (PPP) y Efectividad Gubernamental",
    x = "Efectividad del Gobierno",
    y = "PIB per cápita PPP (USD constantes)",
    caption = "Fuente: Banco Mundial (WGI & World Development Indicators)"
  ) +
  theme_minimal(base_size = 13)

grafico1 <- ggplot(df_inf, aes(x = gov_effectiveness, y = gdp_percap_ppp)) +
  geom_point(alpha = 0.6, color = "orange") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, color = "purple") +
  labs(
    title = "Mayor efectividad del gobierno se asocia con mayor ingreso per cápita",
    subtitle = "Relación entre PIB per cápita (PPP) y Efectividad Gubernamental",
    x = "Efectividad del Gobierno",
    y = "PIB per cápita PPP (USD constantes)",
    caption = "Fuente: Banco Mundial (WGI & World Development Indicators)"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path("output", "figures", "Relacion_PIB_Gob_effect.png"),
  plot = grafico1,
  width = 8,
  height = 6,
  dpi = 300
)

#Los países con instituciones más efectivas tienden a presentar niveles mucho 
#más altos de PIB per cápita. La relación es claramente positiva y estadísticamente 
#significativa, consistente con el coeficiente estimado en la regresión.


#8.2 Grafico 2
ggplot(df_final, aes(x = factor(year), y = gdp_percap_ppp, fill = factor(year))) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.6) +
  scale_fill_manual(values = c("#1F618D", "#2980B9", "#85C1E9")) +
  labs(
    title = "Distribución del PIB Per Cápita PPP en el Mundo (2000–2020)",
    subtitle = "Comparación de la variabilidad entre países por año",
    x = "Año",
    y = "PBI Per Cápita PPP",
    fill = "Año",
    caption = "Fuente: Banco Mundial (Worldwide Governance Indicators)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

grafico2 <- ggplot(df_final, aes(x = factor(year), y = gdp_percap_ppp, fill = factor(year))) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.6) +
  scale_fill_manual(values = c("#1F618D", "#2980B9", "#85C1E9")) +
  labs(
    title = "Distribución del PIB Per Cápita PPP en el Mundo (2000–2020)",
    subtitle = "Comparación de la variabilidad entre países por año",
    x = "Año",
    y = "PBI Per Cápita PPP",
    fill = "Año",
    caption = "Fuente: Banco Mundial (Worldwide Governance Indicators)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave(
  filename = file.path("output", "figures", "Variabilidad_PIB_entre_paises_por_año.png"),
  plot = grafico2,
  width = 8,
  height = 6,
  dpi = 300
)

library(reshape2)

#8.3 Gráfico Adicional 
df_top <- df_original %>% 
  filter(year %in% c(2000, 2020)) %>%
  select(country, year, gdp_percap_ppp) %>%
  drop_na()

top2000 <- df_top %>%
  filter(year == 2000) %>%
  slice_max(order_by = gdp_percap_ppp, n = 10) %>%
  mutate(rank2000 = row_number())

top2020 <- df_top %>%
  filter(year == 2020) %>%
  slice_max(order_by = gdp_percap_ppp, n = 10) %>%
  mutate(rank2020 = row_number())

ranking_change <- full_join(top2000, top2020, by = "country", suffix = c("_2000","_2020"))

ranking_change <- ranking_change %>%
  mutate(
    status = case_when(
      !is.na(rank2000) & !is.na(rank2020) ~ "Mantiene Top 10",
      is.na(rank2000) & !is.na(rank2020) ~ "Nuevo en Top 10",
      !is.na(rank2000) & is.na(rank2020) ~ "Sale del Top 10",
      TRUE ~ "Otro"
    )
  )

df_plot <- ranking_change %>%
  pivot_longer(
    cols = c(gdp_percap_ppp_2000, gdp_percap_ppp_2020),
    names_to = "year",
    values_to = "pib"
  ) %>%
  mutate(year = ifelse(year == "gdp_percap_ppp_2000", 2000, 2020))

colores <- c(
  "Mantiene Top 10" = "#0077FF",  
  "Nuevo en Top 10" = "#556B2F",  
  "Sale del Top 10" = "grey60"    
)


ggplot(df_plot, aes(x = pib, y = reorder(country, pib))) +
  geom_segment(aes(x = 0, xend = pib, yend = country, color = status),
               linewidth = 1.1, alpha = 0.7) +
  geom_point(aes(color = status), size = 4) +
  facet_wrap(~year, ncol = 2, scales = "free_y") +
  scale_color_manual(values = colores) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Top 10 PIB per cápita PPP — Comparación 2000 vs 2020",
    subtitle = "Gráfico tipo 'chupetín' con cambios en la composición del Top 10",
    x = "PIB per cápita (PPP, USD)",
    y = "",
    color = "Categoría"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold")
  )

grafico3 <- ggplot(df_plot, aes(x = pib, y = reorder(country, pib))) +
  geom_segment(aes(x = 0, xend = pib, yend = country, color = status),
               linewidth = 1.1, alpha = 0.7) +
  geom_point(aes(color = status), size = 4) +
  facet_wrap(~year, ncol = 2, scales = "free_y") +
  scale_color_manual(values = colores) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Top 10 PIB per cápita PPP — Comparación 2000 vs 2020",
    subtitle = "Gráfico tipo 'chupetín' con cambios en la composición del Top 10",
    x = "PIB per cápita (PPP, USD)",
    y = "",
    color = "Categoría"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold")
  )

ggsave(
  filename = file.path("output", "figures", "Top_10_2000_vs_2020.png"),
  plot = grafico3,
  width = 8,
  height = 6,
  dpi = 300
)

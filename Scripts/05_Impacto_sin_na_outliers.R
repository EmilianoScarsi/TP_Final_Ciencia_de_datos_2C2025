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

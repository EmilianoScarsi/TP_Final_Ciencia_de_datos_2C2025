
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
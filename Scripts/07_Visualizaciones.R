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

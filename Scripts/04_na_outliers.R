
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

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

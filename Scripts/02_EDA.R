
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

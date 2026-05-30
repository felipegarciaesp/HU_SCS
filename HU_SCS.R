# =====================================================================
# Codigo para determinar Hidrograma Unitario del SCS / Felipe Garcia
# =====================================================================

## Limpiar ambiente
#-------------------
rm(list=ls())
graphics.off()
cat("\014")

## Directorio de trabajo
#-------------------
setwd("C:/Codigos/HU_SCS")





# =====================================================================
# DEFINICION DE FUNCIONES
# =====================================================================

Pp_ef_ac <- function(Pp_tot_ac, CN) {
  # Pp_ef_ac: precipitación efectiva acumulada [mm]
  # Pp_tot_ac: precipitación total acumulada [mm]
  # CN: curva número (0 < CN <= 100)
  if (CN <= 0 || CN > 100 || Pp_tot_ac < 0) return(0.0)
  
  S <- 25400 / CN - 254
  Ia <- 0.2 * S
  
  if (Pp_tot_ac >= Ia) {
    return((Pp_tot_ac - Ia)^2 / (Pp_tot_ac - Ia + S))
  }
  return(0.0)
}

read_file_data <- function(file_path, sheet) {
  df <- openxlsx::read.xlsx(file_path, sheet = sheet, colNames = TRUE)
  rownames(df) <- as.character(df[,1])
  df <- df[,-1, drop = FALSE] # drop = FALSE evita que R convierta automáticamente el dataframe en un vector cuando el resultado tiene una sola columna.
                              # es decir, df continuara siendo un dataframe incluso si tiene 1 columna.
  df
}

create_empty_df <- function(index, columns) {
  df <- data.frame(matrix(NA, nrow = length(index), ncol = length(columns)))
  rownames(df) <- unlist(index)
  colnames(df) <- unlist(columns)
  df
}

# =====================================================================
# 
# =====================================================================

USCS <- read_file_data(file.path(getwd(),"Inputs.xlsx"), "DT_USCS")
PP_Max <- read_file_data(file.path(getwd(),"Inputs.xlsx"), "Pp Max")
CD <- read_file_data(file.path(getwd(),"Inputs.xlsx"), "CD")

# =====================================================================
# Se determinan precipitaciones de diseño (Pp_Max * CD)

cuencas <- colnames(PP_Max)  # nombres de las cuencas

Pp_dur <- lapply(cuencas, function(cuenca) {
  pp  <- PP_Max[[cuenca]]           # vector de Pp max 24h por T
  cd  <- CD[[cuenca]]               # vector de coeficientes de duración
  
  # Producto exterior: cada T * cada duración
  df <- outer(pp, cd)
  rownames(df) <- rownames(PP_Max)  # periodos de retorno
  colnames(df) <- rownames(CD)      # duraciones de tormenta
  as.data.frame(df)
})

names(Pp_dur) <- cuencas #Se asigna nombre de cuencas respectivas al df Pp_dur
# =====================================================================



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
  df <- df[,-1, drop = FALSE]
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




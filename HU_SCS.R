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

read_file_data <- function(file_path, sheet, use_rownames = TRUE) {
  df <- openxlsx::read.xlsx(file_path, sheet = sheet, colNames = TRUE)
  if (use_rownames) {
    rownames(df) <- as.character(df[,1])
    df <- df[,-1, drop = FALSE]
  }
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

Ratios_HU <- read_file_data(file.path(getwd(),"Inputs.xlsx"), "Ratios", use_rownames = FALSE)
colnames(Ratios_HU) <- c("t/Tp", "q/qp", "Qa/Q")

Param_HUS <- read_file_data(file.path(getwd(),"Inputs.xlsx"), "HUS")
colnames(Param_HUS) <- c("dt propuesto", "dt", "Tp", "Tb", "qp")

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


# =====================================================================
# Confeccion de Hidrograma Unitario
# =====================================================================

# Se crea lista de tiempos por cuenca (o hasta Tp/dt +1)

t <- lapply(cuencas, function(cuenca) {
  Tb <- Param_HUS[cuenca, "Tb"]
  dt <- Param_HUS[cuenca, "dt"]
  n <- as.integer(Tb / dt) + 1
  seq(0, n * dt, by = dt)
})

Ratios_HU <- lapply(cuencas, function(cuenca) {
  Tp <- Param_HUS[cuenca, "Tp"]
  df <- Ratios_HU
  # Insertar nueva columna "t" en la segunda posición
  df <- data.frame(
    df[, 1, drop = FALSE],
    t = df[["t/Tp"]] * Tp,
    df[, 2:ncol(df), drop = FALSE]
  )
  df
})

names(Ratios_HU) <- cuencas

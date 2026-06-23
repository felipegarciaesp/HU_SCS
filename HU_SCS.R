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
    df <- df[,-1, drop = FALSE] # drop = FALSE evita que R convierta automáticamente el dataframe en un vector cuando el resultado tiene una sola columna.
                                # es decir, df continuara siendo un dataframe incluso si tiene 1 columna.
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
colnames(Param_HUS) <- c("Area", "dt propuesto", "dt", "Tp", "Tb", "qp")


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

# Se crea dataframe con las coordenadas del Hidrograma Unitario:
Coord_HUS <- lapply(cuencas, function(cuenca) {
  Tp    <- Param_HUS[cuenca, "Tp"]
  qp    <- Param_HUS[cuenca, "qp"]
  A     <- Param_HUS[cuenca, "Area"]
  
  # Se define qp_ como (qp / A) * 1000, en unidades de [L / (s*mm*km2)]
  qp_   <- (qp / A) * 1000 
  
  data.frame(
    time = Ratios_HU[["t/Tp"]] * Tp,
    q    = Ratios_HU[["q/qp"]] * qp_
  )
})

names(Coord_HUS) <- cuencas

# Se crea dataframe con el Hidrograma Unitario de cada cuenca:
HUS <- lapply(cuencas, function(cuenca) {
  Tb    <- Param_HUS[cuenca, "Tb"]
  dt    <- Param_HUS[cuenca, "dt"]
  n     <- as.integer(Tb / dt) + 1
  time  <- seq(0, n * dt, by = dt)
  
  # Interpolación lineal de q desde Coord_HUS
  q <- approx(
    x    = Coord_HUS[[cuenca]][["time"]],   # tiempos de referencia
    y    = Coord_HUS[[cuenca]][["q"]],      # caudales de referencia
    xout = time,                            # tiempos donde interpolar
    rule = 2                                # fuera del rango, usa el valor extremo
  )$y
  
  data.frame(time = time, q = q)
  
  # approx() realiza interpolación lineal entre los puntos de Coord_HUS
  # rule = 2 → si algún valor de time cae fuera del rango de Coord_HUS, usa el valor del extremo más cercano en lugar de NA
  # $y → extrae solo los valores interpolados del resultado de approx()
  
})

names(HUS) <- cuencas

# Me faltaría hacer la correccion por el volumen para que salga unitario.
# Al retomar ve que los numeros obtenidos de q te calcen con lo indicado en la planilla Ausenco para C_01


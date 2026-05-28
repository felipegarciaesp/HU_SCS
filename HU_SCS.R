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

tp <- function(tc, dt_adopt) {
  # tc: tiempo de concentración (horas)
  # dt_adopt: paso temporal adoptado (horas)
  0.6 * tc + dt_adopt / 2
}

qp <- function(Area, tp) {
  # Area: área de la cuenca (km2)
  # tp: tiempo al peak (horas)
  ifelse(tp == 0, 0.0, 0.208 * Area / tp)
}

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
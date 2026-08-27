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
  # S: deficit potencial maximo de escorrentia [mm]
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

read_tormentas <- function(file_path, sheet) {
  # Leer hoja sin encabezados ni omitir filas/columnas vacías
  raw <- openxlsx::read.xlsx(
    file_path, sheet = sheet,
    colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE
  )
  
  ncols <- ncol(raw)
  tormentas <- list()
  col <- 1
  
  while (col <= ncols) {
    
    # --- Columna separadora (vacía): saltar ---
    if (all(is.na(raw[, col]))) {
      col <- col + 1
      next
    }
    
    # --- Validar que exista la segunda columna del par ---
    if (col + 1 > ncols) {
      stop(sprintf(
        "Error en columna %d: se esperaba una segunda columna de datos pero no existe.\n",
        col
      ))
    }
    
    # --- Fila 1: título ---
    titulo_label <- as.character(raw[1, col])
    titulo_nombre <- as.character(raw[1, col + 1])
    
    if (is.na(titulo_label) || !grepl("Tormenta", titulo_label, ignore.case = TRUE)) {
      stop(sprintf(
        "Error en columna %d: se esperaba 'Tormenta N°X' en la fila 1 pero se encontró: '%s'.\n",
        col, titulo_label
      ))
    }
    if (is.na(titulo_nombre) || titulo_nombre == "NA") {
      stop(sprintf(
        "Error en columna %d: falta el nombre de la tormenta en la celda derecha de la fila 1.\n",
        col
      ))
    }
    
    storm_name <- titulo_nombre
    
    # --- Filas 3 en adelante: datos ---
    data_raw <- raw[3:nrow(raw), c(col, col + 1)]
    valid    <- !is.na(data_raw[, 1]) & !is.na(data_raw[, 2])
    data_raw <- data_raw[valid, ]
    
    if (nrow(data_raw) == 0) {
      stop(sprintf("Error en tormenta '%s': no se encontraron datos.\n", storm_name))
    }
    
    # Valores en formato Percentage de Excel → vienen como decimales (0–1) → × 100
    tiempo <- as.numeric(data_raw[, 1]) * 100
    pp     <- as.numeric(data_raw[, 2]) * 100
    
    df <- data.frame(`tiempo (%)` = tiempo, `Pp (%)` = pp, check.names = FALSE)
    
    # --- Validaciones de rango ---
    tol <- 1e-6
    if (abs(df[1, "tiempo (%)"])              > tol) stop(sprintf("Error en tormenta '%s': el tiempo no comienza en 0%%.\n",          storm_name))
    if (abs(df[nrow(df), "tiempo (%)"] - 100) > tol) stop(sprintf("Error en tormenta '%s': el tiempo no termina en 100%%.\n",         storm_name))
    if (abs(df[1, "Pp (%)"])                  > tol) stop(sprintf("Error en tormenta '%s': la precipitación no comienza en 0%%.\n",   storm_name))
    if (abs(df[nrow(df), "Pp (%)"] - 100)     > tol) stop(sprintf("Error en tormenta '%s': la precipitación no termina en 100%%.\n", storm_name))
    
    # --- Validar separador después del bloque (si quedan columnas) ---
    next_col <- col + 2
    if (next_col <= ncols && !all(is.na(raw[, next_col]))) {
      stop(sprintf(
        "Error: se esperaba una columna vacía separadora después de la tormenta '%s' (columna %d), pero tiene datos.\n",
        storm_name, next_col
      ))
    }
    
    tormentas[[storm_name]] <- df
    col <- col + 2
  }
  
  if (length(tormentas) == 0) {
    stop("Error: no se encontró ninguna tormenta en la pestaña 'Tormentas'. Verifique el formato.\n")
  }
  
  message(sprintf("Se cargaron %d tormenta(s): %s", length(tormentas), paste(names(tormentas), collapse = ", ")))
  tormentas
}

# =====================================================================
# 
# =====================================================================

Tormentas <- read_tormentas(file.path(getwd(), "Inputs.xlsx"), "Tormentas")
PP_Max <- read_file_data(file.path(getwd(),"Inputs.xlsx"), "Pp Max")
CD <- read_file_data(file.path(getwd(),"Inputs.xlsx"), "CD")

Ratios_HU <- read_file_data(file.path(getwd(),"Inputs.xlsx"), "Ratios", use_rownames = FALSE)
colnames(Ratios_HU) <- c("t/Tp", "q/qp", "Qa/Q")

Param_HUS <- read_file_data(file.path(getwd(),"Inputs.xlsx"), "HUS")
colnames(Param_HUS) <- c("Area", "CN", "dt propuesto", "dt", "Tp", "Tb", "qp")


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
# Definicion de variables globales
# =====================================================================
  # Se utiliza esta sección para definir algunas variables globales, que seran
  # utilizadas en distintas partes del código.


#dt    <- Param_HUS[cuenca, "dt"] # Paso de tiempo escogido

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

# La profundidad de escorrentia directa en el hidrograma unitario debe comprobarse
# igual a 1 mm (o pulgadas o centimetros, segun se trabaje)
# A continuacion, se calcula esta profundidad de escorrentia directa y se normalizan por
# este valor los resultados de qp_, para que de esta forma quedarnos con valores de 
# q_corr cuya profundidad de escorrentia directa sea 1 mm.

S <- sapply(cuencas, function(cuenca) {
  sum(HUS[[cuenca]][["q"]])
}) # Sumatoria de escorrentia directa en el hidrograma unitario, en [L / (s*mm*km2)]

D <- sapply(cuencas, function(cuenca){
  dt    <- Param_HUS[cuenca, "dt"]
  S[[cuenca]] * dt * 3600 / 1000000
})  # Altura de lamina de agua, en mm por mm de lluvia efectiva. Este valor se ocupa para normalizar
    # los valores de q en HUS para asegurarnos que el hidrograma unitario tenga una lámina de agua
    # de 1 mm por mm de lluvia efectiva.

# Normalizar HUS: dividir cada q por el D de su cuenca
HUS <- lapply(cuencas, function(cuenca) {
  df <- HUS[[cuenca]]            # hidrograma original
  d  <- D[[cuenca]]              # factor de normalización para la cuenca
  if (is.na(d) || d == 0) {      # proteger contra NA/0
    warning(sprintf("D faltante o cero para la cuenca '%s' — no se normaliza", cuenca))
    return(df)
  }
  df$q <- df$q / d
  df
})

names(HUS) <- cuencas

# =====================================================================
# Confeccion de Hietograma de Tormenta
# =====================================================================

duraciones <- rownames(CD)   # duraciones disponibles [h]
periodos   <- rownames(PP_Max)  # periodos de retorno disponibles
dur_h_vals <- as.numeric(duraciones)  # duraciones como número

# Hietograma[[cuenca]][[tormenta]][[periodo]][[duracion]]
# → data.frame con columnas: time [h], Pp_cum [mm], Pp_inc [mm]

Hietograma <- lapply(cuencas, function(cuenca) {
  
  lapply(names(Tormentas), function(nombre_tormenta) {
    tormenta <- Tormentas[[nombre_tormenta]]  # df con tiempo(%) y Pp(%)
    
    lapply(periodos, function(periodo) {
      
      lapply(seq_along(duraciones), function(i) {
        dur   <- duraciones[i]   # duración como string [h]
        dur_h <- dur_h_vals[i]   # duración como número [h]
        Pp    <- Pp_dur[[cuenca]][periodo, dur]  # Pp Máx de diseño [mm]
        
        if (is.na(Pp)) {
          stop(sprintf(
            "Error: Pp no encontrada para cuenca='%s', T='%s', duración='%s'.\n",
            cuenca, periodo, dur
          ))
        }
        
        # tiempo (%) → time [h] : multiplicar por la duración
        time_h   <- (tormenta[["tiempo (%)"]] / 100) * dur_h
        
        # Pp (%) → Pp_cum [mm] : multiplicar por la Pp Max de diseño
        Pp_cum   <- (tormenta[["Pp (%)"]]    / 100) * Pp
        
        # Precipitación incremental [mm]
        Pp_inc   <- c(0, diff(Pp_cum))
        
        data.frame(
          time   = time_h,
          Pp_cum = Pp_cum,
          Pp_inc = Pp_inc
        )
        
      }) |> setNames(duraciones)
      
    }) |> setNames(periodos)
    
  }) |> setNames(names(Tormentas))
  
}) |> setNames(cuencas)

# =====================================================================
# Re-interpolación de Hietogramas al dt del Hidrograma
# =====================================================================

# Hietograma_dt[[cuenca]][[tormenta]][[periodo]][[duracion]]
# → data.frame con columnas: time [h], Pp_cum [mm], Pp_inc [mm]
# → con paso temporal dt de cada cuenca

Hietograma_dt <- lapply(cuencas, function(cuenca) {
  dt <- Param_HUS[cuenca, "dt"]
  
  lapply(names(Tormentas), function(nombre_tormenta) {
    
    lapply(periodos, function(periodo) {
      
      lapply(seq_along(duraciones), function(i) {
        dur   <- duraciones[i]
        dur_h <- dur_h_vals[i]
        
        hiet     <- Hietograma[[cuenca]][[nombre_tormenta]][[periodo]][[dur]]
        time_old <- hiet[["time"]]
        Pp_cum_old <- hiet[["Pp_cum"]]
        
        # Nuevo vector de tiempo con paso dt
        time_new <- seq(0, dur_h, by = dt)
        
        # Interpolación lineal de Pp_cum al nuevo dt
        Pp_cum_new <- approx(
          x    = time_old,
          y    = Pp_cum_old,
          xout = time_new,
          rule = 2
        )$y
        
        # Recalcular Pp_inc con el nuevo dt
        Pp_inc_new <- c(0, diff(Pp_cum_new))
        
        data.frame(
          time   = time_new,
          Pp_cum = Pp_cum_new,
          Pp_inc = Pp_inc_new
        )
        
      }) |> setNames(duraciones)
      
    }) |> setNames(periodos)
    
  }) |> setNames(names(Tormentas))
  
}) |> setNames(cuencas)

# =====================================================================
# Hietogramas de Precipitación Efectiva
# =====================================================================

# Hietograma_ef[[cuenca]][[tormenta]][[periodo]][[duracion]]
# → data.frame con columnas: time [h], Pp_ef_cum [mm], Pp_ef_inc [mm]

Hietograma_ef <- lapply(cuencas, function(cuenca) {
  CN <- Param_HUS[cuenca, "CN"]
  
  lapply(names(Tormentas), function(nombre_tormenta) {
    
    lapply(periodos, function(periodo) {
      
      lapply(seq_along(duraciones), function(i) {
        dur  <- duraciones[i]
        
        hiet <- Hietograma_dt[[cuenca]][[nombre_tormenta]][[periodo]][[dur]]
        
        # Aplicar Pp_ef_ac sobre cada valor de Pp_cum
        Pp_ef_cum <- sapply(hiet[["Pp_cum"]], function(Pp_tot_ac) {
          Pp_ef_ac(Pp_tot_ac, CN)
        })
        
        # Precipitación efectiva incremental [mm]
        Pp_ef_inc <- c(0, diff(Pp_ef_cum))
        
        data.frame(
          time      = hiet[["time"]],
          Pp_ef_cum = Pp_ef_cum,
          Pp_ef_inc = Pp_ef_inc
        )
        
      }) |> setNames(duraciones)
      
    }) |> setNames(periodos)
    
  }) |> setNames(names(Tormentas))
  
}) |> setNames(cuencas)

# =====================================================================
# Hidrograma de Escorrentía Directa (HED) - Convolución
# =====================================================================

# HED[[cuenca]][[tormenta]][[periodo]][[duracion]]
# → data.frame con columnas: time [h], q [m3/s]

HED <- lapply(cuencas, function(cuenca) {
  dt <- Param_HUS[cuenca, "dt"]
  A  <- Param_HUS[cuenca, "Area"]
  
  lapply(names(Tormentas), function(nombre_tormenta) {
    
    lapply(periodos, function(periodo) {
      
      lapply(seq_along(duraciones), function(i) {
        dur <- duraciones[i]
        
        # Vector de precipitación efectiva incremental [mm]
        P <- Hietograma_ef[[cuenca]][[nombre_tormenta]][[periodo]][[dur]][["Pp_ef_inc"]]
        
        # Vector de caudales del hidrograma unitario [L/(s·mm·km2)]
        U <- HUS[[cuenca]][["q"]]
        
        # Convolución → resultado en [L/(s·km2)]
        q_conv <- convolve(P, rev(U), type = "open")
        
        # Conversión de unidades:
        # [L/(s·mm·km2)] * [mm] * [km2] / 1000 → [m3/s]
        q_m3s <- q_conv * A / 1000
        
        # Vector de tiempo para el HED
        n_pasos <- length(q_m3s)
        time    <- seq(0, (n_pasos - 1) * dt, by = dt)
        
        data.frame(
          time = time,
          q    = q_m3s
        )
        
      }) |> setNames(duraciones)
      
    }) |> setNames(periodos)
    
  }) |> setNames(names(Tormentas))
  
}) |> setNames(cuencas)



# SIGUIENTES PASOS:

# 1. SE SUPONE QUE LAS PROFUNDIDADES DE EXCESO DE LLUVIA Y LA ESCORRENTIA DIRECTA DEBEN
# SER IGUALES. HAZ LOS CAMBIOS DE UNIDADES RESPECTIVOS Y CORROBORA ESTO. AVERIGUA QUE HACER
# SI ESTO NO SE CUMPLE.
# 2.CORROBORA QUE LOS RESULTADOS TE DAN PARECIDOS O DENTRO DEL ORDEN QUE PLANILLA DE AUSENCO.
# ES ESPERABLE CIERTA DIFERENCIA, DADO QUE HAY UNA FORMULA QUE NO ESTÁ BIEN DEFINIDA EN EL 
# EXCEL Y ADEMAS DE QUE NO ESTÁS DE ACUERDO EN EL PASO TEMPORAL CON LA QUE CALCULAN EL VOLUMEN
# CON EL QUE NORMALIZAN EL HU (O ALGO ASI ERA). PARA HACER ESTO DEBERIAS HOMOLOGAR EN EL CODIGO
# LAS DISTRIBUIONES DE TORMENTA QUE ESTAN EN LA PLANILLA, ANALIZA SOLO DURACION DE TORMENTA DE
# 12 HORAS Y EVALUA 2 PERIODOS DE RETORNO COMO MAXIMO.
# 3. PARA COMPARAR RESULTADOS DEBES PEDIRLE AL CODIGO QUE TE ARROJE EL Q MAX Y EL VOLUMEN.
# 4. OTRA CORROBORACION IMPORTANTE QUE DEBES HACER ES CON HEC-HMS, REVISA QUE TE DA CON ESTO.
# 5. LO SIGUIENTE SERIA PEDIRLE A COPILOT QUE TE GENERE CARPETAS CON RESULTADOS Y GRAFICOS




# =====================================================================
# Resumen HED: Caudal Máximo y Volumen Total
# =====================================================================

# Resumen_HED[[cuenca]][[tormenta]][[periodo]][[duracion]]
# → data.frame con columnas: q_max [m3/s], volumen [m3]

Resumen_HED <- lapply(cuencas, function(cuenca) {
  dt <- Param_HUS[cuenca, "dt"]
  
  lapply(names(Tormentas), function(nombre_tormenta) {
    
    lapply(periodos, function(periodo) {
      
      lapply(seq_along(duraciones), function(i) {
        dur <- duraciones[i]
        
        hed   <- HED[[cuenca]][[nombre_tormenta]][[periodo]][[dur]]
        q_max <- max(hed[["q"]])
        
        # Volumen total: suma de caudales * dt [h] * 3600 [s/h] → [m3]
        vol   <- sum(hed[["q"]]) * dt * 3600
        
        data.frame(
          q_max_m3s  = q_max,
          volumen_m3 = vol
        )
        
      }) |> setNames(duraciones)
      
    }) |> setNames(periodos)
    
  }) |> setNames(names(Tormentas))
  
}) |> setNames(cuencas)

# --- Imprimir resumen en consola ---
cat("\n========== RESUMEN HED: Q_MAX y VOLUMEN ==========\n")
for (cuenca in cuencas) {
  cat(sprintf("\n>>> Cuenca: %s\n", cuenca))
  for (tormenta in names(Tormentas)) {
    cat(sprintf("  Tormenta: %s\n", tormenta))
    for (periodo in periodos) {
      cat(sprintf("    T = %s años\n", periodo))
      for (dur in duraciones) {
        res <- Resumen_HED[[cuenca]][[tormenta]][[periodo]][[dur]]
        cat(sprintf(
          "      Duración %s h → Q_max = %.3f m3/s | Volumen = %.1f m3\n",
          dur, res[["q_max_m3s"]], res[["volumen_m3"]]
        ))
      }
    }
  }
}




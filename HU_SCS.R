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
dir_inputs  <- file.path(getwd(), "Inputs")
dir_outputs <- file.path(getwd(), "Outputs")
file_inputs <- file.path(dir_inputs, "Inputs.xlsx")

if (!dir.exists(dir_outputs)) {
  dir.create(dir_outputs)
} else {
  # Limpia resultados previos para asegurar que la corrida genere solo resultados nuevos.
  old_outputs <- list.files(dir_outputs, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(old_outputs) > 0) unlink(old_outputs, recursive = TRUE, force = TRUE)
}



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

Tormentas <- read_tormentas(file_inputs, "Tormentas")
PP_Max <- read_file_data(file_inputs, "Pp Max")
CD <- read_file_data(file_inputs, "CD")

Ratios_HU <- read_file_data(file_inputs, "Ratios", use_rownames = FALSE)
colnames(Ratios_HU) <- c("t/Tp", "q/qp", "Qa/Q")

Param_HUS <- read_file_data(file_inputs, "HUS")
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

# =====================================================================
# Exportacion de resultados HED a Outputs (cuenca/tormenta)
# =====================================================================

safe_name <- function(x) {
  # Normaliza nombres para Windows: sin caracteres invalidos, sin espacios repetidos
  # y sin puntos/espacios al final.
  s <- as.character(x)
  s <- iconv(s, from = "", to = "ASCII//TRANSLIT", sub = "_")
  s <- gsub('[\\\\/:*?"<>|]', "_", s)
  s <- gsub('[[:cntrl:]]', "", s)
  s <- gsub('\\s+', "_", s)
  s <- gsub('[^[:alnum:]_.-]', "_", s)
  s <- gsub('_+', "_", s)
  s <- gsub('^[._ ]+|[._ ]+$', "", s)
  if (is.na(s) || s == "") s <- "sin_nombre"
  s
}

for (cuenca in cuencas) {
  dir_cuenca <- file.path(dir_outputs, safe_name(cuenca))
  if (!dir.exists(dir_cuenca)) dir.create(dir_cuenca, recursive = TRUE)

  dt <- Param_HUS[cuenca, "dt"]

  for (nombre_tormenta in names(Tormentas)) {
    dir_tormenta <- file.path(dir_cuenca, safe_name(nombre_tormenta))
    if (!dir.exists(dir_tormenta)) dir.create(dir_tormenta, recursive = TRUE)

    wb <- openxlsx::createWorkbook()
    resumen_rows <- list()
    datos_rows <- list()

    for (j in seq_along(periodos)) {
      periodo <- periodos[j]
      # Grafico por periodo con todas las duraciones superpuestas.
      labels_dur <- paste0(duraciones, "h")
      paleta_alto_contraste <- c(
        "#0072B2", "#D55E00", "#009E73", "#CC79A7",
        "#E69F00", "#56B4E9", "#F0E442", "#000000"
      )
      colores <- rep(paleta_alto_contraste, length.out = length(duraciones))
      tipos_linea <- rep(c(1, 2, 3, 4, 5, 6), length.out = length(duraciones))

      y_max <- max(sapply(duraciones, function(dur) {
        max(HED[[cuenca]][[nombre_tormenta]][[periodo]][[dur]][["q"]], na.rm = TRUE)
      }), na.rm = TRUE)

      x_max <- max(sapply(duraciones, function(dur) {
        max(HED[[cuenca]][[nombre_tormenta]][[periodo]][[dur]][["time"]], na.rm = TRUE)
      }), na.rm = TRUE)

      # Nombre muy corto y secuencial para evitar rutas invalidas/largas en Windows.
      png_file <- file.path(dir_tormenta, sprintf("HED_%02d.png", j))

      if (!dir.exists(dir_tormenta)) {
        stop(sprintf("No existe carpeta de salida para graficos: %s", dir_tormenta))
      }

      # Verificacion defensiva de ruta para evitar error "invalid 'filename'" en Windows.
      if (nchar(png_file) >= 240) {
        stop(sprintf("Ruta de grafico demasiado larga (%d caracteres): %s", nchar(png_file), png_file))
      }

      png_opened <- FALSE
      try({
        grDevices::png(filename = png_file, width = 1400, height = 900, res = 140, type = "cairo-png")
        png_opened <- TRUE
      }, silent = TRUE)
      if (!png_opened) {
        grDevices::png(filename = png_file, width = 1400, height = 900, res = 140)
      }
      first_dur <- duraciones[1]
      first_hed <- HED[[cuenca]][[nombre_tormenta]][[periodo]][[first_dur]]

      plot(
        first_hed[["time"]], first_hed[["q"]],
        type = "l", lwd = 2.2, col = colores[1], lty = tipos_linea[1],
        xlim = c(0, ifelse(is.finite(x_max) && x_max > 0, x_max, max(first_hed[["time"]], na.rm = TRUE))),
        ylim = c(0, ifelse(is.finite(y_max) && y_max > 0, y_max * 1.05, max(first_hed[["q"]], na.rm = TRUE))),
        xlab = "Tiempo [h]", ylab = "Caudal [m3/s]",
        main = paste0("HED - Cuenca: ", cuenca, " | Tormenta: ", nombre_tormenta, " | T=", periodo)
      )

      if (length(duraciones) > 1) {
        for (i in 2:length(duraciones)) {
          dur_i <- duraciones[i]
          hed_i <- HED[[cuenca]][[nombre_tormenta]][[periodo]][[dur_i]]
          lines(hed_i[["time"]], hed_i[["q"]], lwd = 2.2, col = colores[i], lty = tipos_linea[i])
        }
      }

      legend(
        "topleft",
        legend = labels_dur,
        col = colores,
        lwd = 2.2,
        lty = tipos_linea,
        title = "Duracion"
      )
      grid(col = "gray85")
      grDevices::dev.off()

      for (dur in duraciones) {
        hed <- HED[[cuenca]][[nombre_tormenta]][[periodo]][[dur]]

        q_max <- max(hed[["q"]], na.rm = TRUE)
        vol   <- sum(hed[["q"]], na.rm = TRUE) * dt * 3600

        resumen_rows[[length(resumen_rows) + 1]] <- data.frame(
          cuenca = cuenca,
          tormenta = nombre_tormenta,
          "T (años)" = as.character(periodo),
          "Duración (horas)" = as.numeric(dur),
          "Q máx (m3/s)" = q_max,
          "Volumen (m3)" = vol,
          stringsAsFactors = FALSE
        )

        datos_rows[[length(datos_rows) + 1]] <- data.frame(
          cuenca = cuenca,
          tormenta = nombre_tormenta,
          "T (años)" = as.character(periodo),
          "Duración (horas)" = as.numeric(dur),
          "Tiempo (horas)" = hed[["time"]],
          "Q m3/s" = hed[["q"]],
          stringsAsFactors = FALSE
        )
      }
    }

    df_resumen <- do.call(rbind, resumen_rows)
    df_datos <- do.call(rbind, datos_rows)

    # Encabezados finales (colnames<- evita que R los convierta a nombres sintacticos con puntos).
    colnames(df_resumen) <- c(
      "cuenca", "tormenta", "T_(years)", "Storm_Duration_(hrs)",
      "Qmax_(cms)", "Volumen_(m3)"
    )
    colnames(df_datos) <- c(
      "cuenca", "tormenta", "T_(years)", "Storm_Duration_(hrs)",
      "Time_(hr)", "Q_(cms)"
    )

    # Hoja resumen por periodo/duracion (q_max y volumen)
    openxlsx::addWorksheet(wb, "Resumen")
    openxlsx::writeData(wb, "Resumen", df_resumen)

    # Hoja con series completas de tiempo-caudal
    openxlsx::addWorksheet(wb, "HED_series")
    openxlsx::writeData(wb, "HED_series", df_datos)

    out_xlsx <- file.path(dir_tormenta, "HED_resultados.xlsx")
    openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  }
}

cat("\nExportacion completada en carpeta Outputs.\n")


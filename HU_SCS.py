import pandas as pd
import numpy as np


### Funciones para el cálculo del tiempo de concentración (tc) utilizando diferentes métodos ###
def tc_california(Lcauce, RangoElev):
    # TC California para cuencas montañosas, con área superior a 2 km2 (CORROBORAR)
    # Lcauce = Largo del cauce principal (km)
    # RangoElev = ElevMax-ElevMin de la cuenca (m)
    # Resultado tc se expresa en horas
    if Lcauce == 0 or RangoElev == 0:
        tc = 0.0
    else:
        tc = 0.95 * ((Lcauce**3.0) / (RangoElev)) ** 0.385
    return tc

def tc_USNavy(Lcauce, Vmed):
    # Conocida también como tc del Texas Highway Department o método de la velocidad media (CORROBORAR)
    # Lcauce = Largo del cauce principal (km)
    # Vmed = Velocidad media del cauce principal (m/s)
    # Resultado tc se expresa en horas
    if Lcauce == 0 or Vmed == 0:
        tc = 0.0
    else:
        tc = (1.0 / 3.6) * (Lcauce / Vmed)
    return tc

def tc_Giandotti(Area, Lcauce, ElevMed, ElevMin):
    # Tc Giandotti para cuencas con área entre 50 y 500 km2 (CORROBORAR)
    # Area = Área de la cuenca (km2)
    # Lcauce = Largo del cauce principal (km)
    # ElevMed = Elevación media de la cuenca (m)
    # ElevMin = Elevación mínima de la cuenca (m)
    # Resultado tc se expresa en horas
    if Area == 0 or Lcauce == 0 or ElevMed == 0 or ElevMin == 0:
        tc = 0.0
    else:
        Hm = ElevMed - ElevMin
        tc = (4.0 * (Area ** 0.5) + 1.5 * Lcauce) / (0.8 * (Hm ** 0.5))
    return tc

def tc_SCS(Lcauce, CN, S):
    # Tc Lag SCS para cuencas agrícolas o urbanas con área inferior a 8 km2 (CORROBORAR)
    # Lcauce = Largo del cauce principal (km)
    # CN = Valor de curva número, humedad antecedente II (CORROBORAR)
    # S = Pendiente media de la cuenca (m/m)
    # Resultado tc se expresa en horas
    if Lcauce == 0 or CN == 0 or S == 0:
        tc = 0.0
    else:
        term = (1000.0 / CN) - 9.0
        if term <= 0 or S <= 0:
            tc = 0.0
        else:
            tc = (3.42 * (Lcauce ** 0.8) * (term ** 0.7) / (S ** 0.5)) / 60.0
    return tc

def tc_NEsp(Lcauce, S):
    # Tc Normas Españolas
    # Lcauce = Largo del cauce principal (km)
    # S = Pendiente media de la cuenca (m/m)
    # Resultado tc se expresa en horas
    if Lcauce == 0 or S == 0:
        tc = 0.0
    else:
        tc = (18.0 * (Lcauce ** 0.76) / (S ** 0.19)) / 60.0
    return tc

### Funciones que calculan los parámetros de SCS ###

def tp(tc, dt_adopt):
    # tp corresponde al tiempo al peak, expresado en horas
    # tc corresponde al tiempo de concentración, en horas
    # dt_adopt corresponde al paso temporal "delta t" adoptado, en horas
    return 0.6 * tc + dt_adopt / 2.0

def qp(Area, tp):
    # qp corresponde al caudal al peak, expresado en m3/s/mm
    # tp corresponde al tiempo al peak, en horas
    # Area corresponde al area de la cuenca, en km2
    if tp == 0:
        return 0.0
    return 0.208 * Area / tp

### Funcion para calcular Precipitacion efectiva acumulada ###

def Pp_ef_ac(Pp_tot_ac, CN):
    # Pp_ef_ac corresponde a la precipitación efectiva acumulada [mm]
    # Pp_tot_ac corresponde a la precipitación total acumulada en el intervalo analizado [mm]
    # CN corresponde al valor de la curva número de la cuenca analizada
    # S corresponde al almacenamiento
    if CN <= 0 or CN > 100 or Pp_tot_ac < 0:
        return 0.0

    S = 25400.0 / CN - 254.0
    a = 0.2
    Ia = a * S

    if Pp_tot_ac >= Ia:
        return (Pp_tot_ac - Ia) ** 2 / (Pp_tot_ac - Ia + S)
    return 0.0


### Carga de datos desde Excel ###

def cargar_Data_HU_SCS(ruta_excel="Data_HU_SCS.xlsx", hoja=0):
    # Carga la hoja indicada del archivo Excel en un DataFrame usando un context manager
    with pd.ExcelFile(ruta_excel) as excel_file:
        df = pd.read_excel(excel_file, sheet_name=hoja)
    return df


df_Data_HU_SCS = cargar_Data_HU_SCS("Data_HU_SCS.xlsx")




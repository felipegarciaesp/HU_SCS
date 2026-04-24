import pandas as pd
import numpy as np

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

def tc_LagSCS(Lcauce, CN, S):
    # Tc Lag SCS para cuencas agrícolas o urbanas con área inferior a 8 km2 (CORROBORAR)
    # Lcauce = Largo del cauce principal (km)
    # CN = Valor de curva número, humedad antecedente II (CORROBORAR)
    # S = Pendiente media de la cuenca (m/m)
    
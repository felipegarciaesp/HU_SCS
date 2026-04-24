import pandas as pd
import numpy as np

def tc_california(Lcauce, RangoElev):
    # TC California para cuencas montañosas, con área superior a 2 km2
    # Lcauce = Largo del cauce principal (km)
    # RangoElev = ElevMax-ElevMin de la cuenca (m)
    # Resultado tc se expresa en horas
    if Lcauce == 0 or RangoElev == 0:
        tc = 0.0
    else:
        tc = 0.95 * ((Lcauce**3.0) / (RangoElev)) ** 0.385
    return tc

def tc_USNavy(Lcauce, Vmed):
    # Conocida también como tc del Texas Highway Department o método de la velocidad media
    # Lcauce = Largo del cauce principal (km)
    # Vmed = Velocidad media del cauce principal (m/s)
    # Resultado tc se expresa en horas
    if Lcauce == 0 or Vmed == 0:
        tc = 0.0
    else:
        tc = (1.0 / 3.6) * (Lcauce / Vmed)
    return tc
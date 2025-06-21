# ─────────────────────────────────────────────────────────────────────────────
# LIBRERÍAS Y DATOS
# ─────────────────────────────────────────────────────────────────────────────
library(tidyverse)
library(readxl)
library(data.table)
library(googlesheets4)
library(DatawRappr)

# Cargar archivos principales
datos_vivienda <- read_tsv("ECEPOVvivienda_2021.csv")
cod_prov <- read_excel("Cods_provincias.xlsx") %>%
  rename(IDQ_PV = 1, nomprov = 2, nom_diario = 3, ccaa = 4)
cod_munis <- read_excel("diccionario23.xlsx")

# ─────────────────────────────────────────────────────────────────────────────
# ANÁLISIS POR VIVIENDA
# ─────────────────────────────────────────────────────────────────────────────

# Régimen de tenencia por municipio
datos_tenencia <- datos_vivienda %>%
  mutate(REGVI = case_when(
    REGVI %in% c(2, 3) ~ "Comprada/Hipotecada",
    REGVI == 4 ~ "Alquilada",
    TRUE ~ "Otros"
  )) %>%
  group_by(IDQ_MUN) %>%
  mutate(tot_viviendas = round(sum(FACTOR))) %>%
  group_by(IDQ_MUN, REGVI, tot_viviendas) %>%
  summarise(n_viviendas = round(sum(FACTOR)), .groups = "drop") %>%
  mutate(porcentaje = n_viviendas / tot_viviendas * 100)

write_csv(datos_tenencia, "tenencia_por_muni.csv")

# Nacional: porcentaje sin ascensor en bloques
sin_ascensor_nacional <- datos_vivienda %>%
  filter(TIPOEDIF == 2) %>%
  group_by(ASCENSOR) %>%
  summarise(n_viviendas = round(sum(FACTOR)), .groups = "drop") %>%
  pivot_wider(names_from = ASCENSOR, values_from = n_viviendas) %>%
  rename(con_ascensor = `1`, sin_ascensor = `6`) %>%
  mutate(p_sin_ascensor = sin_ascensor / (con_ascensor + sin_ascensor) * 100)

# Tamaño de municipio y altura de edificio
total_por_tamano_muni <- datos_vivienda %>%
  mutate(
    TAM_MUNI = case_when(
      TAM_MUNI == 1 ~ "Hasta 50.000",
      TAM_MUNI == 2 ~ "50.001–100.000",
      TAM_MUNI == 3 ~ "100.001–500.000",
      TAM_MUNI == 4 ~ "Más de 500.000"
    ),
    altura = if_else(NPLANTASSOB >= 3, "3 o más plantas", "Menos de 3 plantas")
  ) %>%
  group_by(TAM_MUNI, ASCENSOR, TIPOEDIF, altura) %>%
  summarise(n_viviendas = round(sum(FACTOR)), .groups = "drop") %>%
  pivot_wider(names_from = ASCENSOR, values_from = n_viviendas) %>%
  rename(con_ascensor = `1`, sin_ascensor = `6`) %>%
  mutate(
    p_sin_ascensor = sin_ascensor / (con_ascensor + sin_ascensor) * 100
  ) %>%
  filter(TIPOEDIF == 2, altura == "3 o más plantas")

dw_data_to_chart(total_por_tamano_muni, "0LbEP")

# ─────────────────────────────────────────────────────────────────────────────
# ANÁLISIS POR PERSONAS
# ─────────────────────────────────────────────────────────────────────────────

# Cargar datos de hogar y adultos
datos_hogar <- read_tsv("ECEPOVhogar_2021.csv")
datos_adultos <- read_tsv("ECEPOVadultos_2021.csv")

# Combinar en conjunto único
personas_ascensor_edad <- datos_hogar %>%
  full_join(datos_vivienda, by = "IDEN") %>%
  full_join(datos_adultos, by = "IDEN")

# Personas por grupo de edad
personas_por_edad <- personas_ascensor_edad %>%
  mutate(
    grupo_edad = case_when(
      EDAD < 16 ~ "Menos de 16",
      EDAD < 20 ~ "16 a 19",
      EDAD < 25 ~ "20 a 24",
      EDAD < 35 ~ "25 a 34",
      EDAD < 45 ~ "35 a 44",
      EDAD < 55 ~ "45 a 54",
      EDAD < 60 ~ "55 a 59",
      TRUE      ~ "60 o más"
    ),
    altura = if_else(NPLANTASSOB >= 3, "3 o más plantas", "Menos de 3 plantas")
  ) %>%
  filter(TIPOEDIF == 2, altura == "3 o más plantas") %>%
  group_by(grupo_edad, ASCENSOR) %>%
  summarise(n_personas = sum(FACTOR), .groups = "drop") %>%
  pivot_wider(names_from = ASCENSOR, values_from = n_personas) %>%
  rename(con_ascensor = `1`, sin_ascensor = `6`) %>%
  mutate(
    p_sin_ascensor = sin_ascensor / (con_ascensor + sin_ascensor) * 100
  )

dw_data_to_chart(personas_por_edad, "Tzlom")

# Mayores de 60 años
mayores_sin_ascensor <- personas_ascensor_edad %>%
  filter(EDAD >= 60, NPLANTASSOB >= 3, TIPOEDIF == 2) %>%
  group_by(ASCENSOR) %>%
  summarise(n_personas = sum(FACTOR), .groups = "drop") %>%
  pivot_wider(names_from = ASCENSOR, values_from = n_personas) %>%
  rename(con_ascensor = `1`, sin_ascensor = `6`) %>%
  mutate(p_sin_ascensor = sin_ascensor / (con_ascensor + sin_ascensor) * 100)

# Tipo de hogar de mayores
hogar_mas60 <- personas_ascensor_edad %>%
  filter(EDAD >= 60, TIPOEDIF == 2, NPLANTASSOB >= 3) %>%
  group_by(SITUHOGAR, ASCENSOR) %>%
  summarise(n_personas = sum(FACTOR), .groups = "drop") %>%
  pivot_wider(names_from = ASCENSOR, values_from = n_personas) %>%
  rename(con_ascensor = `1`, sin_ascensor = `6`) %>%
  mutate(
    tipo_hogar = case_when(
      SITUHOGAR == 1 ~ "Vive solo/a",
      SITUHOGAR == 3 ~ "En pareja sin hijos",
      TRUE ~ "Otros"
    ),
    p_sin_ascensor = sin_ascensor / (con_ascensor + sin_ascensor) * 100
  ) %>%
  filter(tipo_hogar != "Otros")

# Año de residencia
residencia_mayores <- personas_ascensor_edad %>%
  filter(EDAD >= 60, TIPOEDIF == 2, NPLANTASSOB >= 2) %>%
  mutate(anio_residencia = case_when(
    ANORESIDVIV <= 1940 ~ "Antes de 1940",
    ANORESIDVIV <= 1950 ~ "1941-1950",
    ANORESIDVIV <= 1960 ~ "1951-1960",
    ANORESIDVIV <= 1970 ~ "1961-1970",
    ANORESIDVIV <= 1980 ~ "1971-1980",
    ANORESIDVIV <= 1990 ~ "1981-1990",
    ANORESIDVIV <= 2000 ~ "1991-2000",
    ANORESIDVIV <= 2010 ~ "2001-2010",
    TRUE ~ "Después de 2010"
  )) %>%
  group_by(anio_residencia, ASCENSOR) %>%
  summarise(n_personas = sum(FACTOR), .groups = "drop") %>%
  pivot_wider(names_from = ASCENSOR, values_from = n_personas) %>%
  rename(con_ascensor = `1`, sin_ascensor = `6`) %>%
  mutate(p_sin_ascensor = sin_ascensor / (con_ascensor + sin_ascensor) * 100)

# Ingresos de mayores
ingresos_mayores <- personas_ascensor_edad %>%
  filter(EDAD >= 60, TIPOEDIF == 2, NPLANTASSOB >= 2) %>%
  mutate(ingreso = case_when(
    INGREHOG == 1 ~ "Menos de 500 €",
    INGREHOG == 2 ~ "500 - 1.000 €",
    INGREHOG == 3 ~ "1.000 - 1.500 €",
    INGREHOG == 4 ~ "1.500 - 2.000 €",
    INGREHOG == 5 ~ "2.000 - 2.500 €",
    INGREHOG == 6 ~ "2.500 - 3.000 €",
    INGREHOG == 7 ~ "3.000 - 5.000 €",
    INGREHOG %in% c(8, 9) ~ "Más de 5.000 €"
  )) %>%
  group_by(ingreso, ASCENSOR) %>%
  summarise(n_personas = sum(FACTOR), .groups = "drop") %>%
  pivot_wider(names_from = ASCENSOR, values_from = n_personas) %>%
  rename(con_ascensor = `1`, sin_ascensor = `6`) %>%
  mutate(p_sin_ascensor = sin_ascensor / (con_ascensor + sin_ascensor) * 100)

# Nivel educativo
educacion <- personas_ascensor_edad %>%
  filter(TIPOEDIF == 2, NPLANTASSOB >= 3) %>%
  mutate(estudios = case_when(
    NIVEDUCAHOGAR == 1 ~ "Ninguno tiene estudios superiores",
    NIVEDUCAHOGAR == 2 ~ "Alguno tiene estudios superiores",
    NIVEDUCAHOGAR == 3 ~ "Todos con estudios superiores"
  )) %>%
  group_by(estudios, ASCENSOR) %>%
  summarise(n_personas = sum(FACTOR), .groups = "drop") %>%
  pivot_wider(names_from = ASCENSOR, values_from = n_personas) %>%
  rename(con_ascensor = `1`, sin_ascensor = `6`) %>%
  mutate(p_sin_ascensor = sin_ascensor / (con_ascensor + sin_ascensor) * 100)

# ─────────────────────────────────────────────────────────────────────────────
# EXPORTAR A GOOGLE SHEETS (añade tus enlaces reales)
# ─────────────────────────────────────────────────────────────────────────────
# write_sheet(hogar_mas60, "https://docs.google.com/...", sheet = "tipo_hogar")
# write_sheet(ingresos_mayores, "https://docs.google.com/...", sheet = "ingresos_60")
# write_sheet(residencia_mayores, "https://docs.google.com/...", sheet = "residencia_60")

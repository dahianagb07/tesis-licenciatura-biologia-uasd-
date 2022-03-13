# Crear cuadrícula para diseño de muestreo
library(dplyr)
library(sf)
parque <- st_read('data/limite-parque.gpkg') # Creada en QGIS, ver nota abajo
cuad <- st_read('data/cuadricula.gpkg')
plot(parque %>% st_geometry)
plot(cuad %>% st_geometry, add=T)
cuad2 <- st_as_sf(cuad)
cuad2 <- cuad2 %>%
  mutate(
    ENLACE=1:nrow(cuad2),
    AREASQM1=st_area(geom) %>% units::drop_units())
cuad3 <- st_intersection(cuad2, parque %>% st_union) %>%
  mutate(AREASQM2=st_area(geom) %>% units::drop_units(),
         AREASQM_PCT=AREASQM2/AREASQM1*100)
pct_eleg <- 40
cuad4 <- cuad2 %>%
  inner_join(
    cuad3 %>%
      filter(AREASQM_PCT >= pct_eleg) %>%
      st_drop_geometry() %>%
      select(ENLACE, AREASQM2, AREASQM_PCT))
cuad4$ENLACE <- 1:nrow(cuad4)
cuad4$ENLACE
cuad_final <- cuad4
names(cuad_final)[grepl('^geom$', names(cuad_final))] <- "geometry"
st_geometry(cuad_final) <- "geometry"
cuad_final
cuad_final <- cuad_final %>% rename(a0_square_meters = AREASQM1)
plot(parque %>% st_geometry)
plot(cuad_final %>% st_geometry, add=T)
# st_write(cuad_final, 'data/cuadricula-final.gpkg')

# Nota: creado mediante QGIS
# processing.run("native:creategrid", {'TYPE':4,'EXTENT':'402637.28125,403169.15625,2041819.0,2042297.125 [EPSG:32619]','HSPACING':75,'VSPACING':75,'HOVERLAY':5,'VOVERLAY':5,'CRS':QgsCoordinateReferenceSystem('EPSG:32619'),'OUTPUT':'ogr:dbname=\'/home/jr/Documentos/tesis-dahiana/tesis-licenciatura-biologia-uasd-/data/cuadricula.gpkg\' table=\"cuadricula\" (geom)'})
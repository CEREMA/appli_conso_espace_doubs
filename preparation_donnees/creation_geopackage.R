library(sf)
library(RPostgreSQL)
library(DBI)
library(rgdal)
library(RSQLite)
# library(rmapshaper)
# library(rgeos)
# library(lwgeom)

# lecture des tables CeremaBase
drv <- dbDriver("PostgreSQL")
conn1 <- dbConnect(drv, host = "172.20.53.34", port = "5444", user= "gt", password="territoires", dbname="ceremabase")
dcommunes <- st_read(conn1, query = "SELECT * FROM a_detc_20ce0148_artificialisation_ddt25.n_adm_exp_cog_commune_025_2020 ORDER BY insee_com")
dzonages <- st_read(conn1, query = "SELECT * FROM a_detc_20ce0148_artificialisation_ddt25.zonages_025 ORDER BY type_zone, nom_zone")
denv_date <- st_read(conn1, query = "SELECT id, code_insee, datation, geom FROM a_detc_20ce0148_artificialisation_ddt25.l_enveloppe_bati_025_historique")
denv_nondate <- st_read(conn1, query = "SELECT id, code_insee, geom FROM a_detc_20ce0148_artificialisation_ddt25.l_enveloppe_bati_025_2017")
dbati <- st_read(conn1, query = "SELECT * FROM a_detc_20ce0148_artificialisation_ddt25.batiment_025")
dindicateurs <- st_read(conn1, query = "SELECT * FROM a_detc_20ce0148_artificialisation_ddt25.indicateurs_com_025")

dbDisconnect(conn1)

# conversion en UTF8 de certaines colonnes
convertutf8 <- function(x) {
  return (iconv(x, "UTF-8"))
}
dcommunes$nom_com <-sapply(dcommunes$nom_com, convertutf8)
dzonages$nom_zone <-sapply(dzonages$nom_zone, convertutf8)

# ajout d'une colonne surface
st_crs(denv_date) = 2154
st_crs(denv_nondate) = 2154
st_crs(dcommunes) = 2154
st_crs(dbati) = 2154


denv_date$surface <- st_area(denv_date)
denv_nondate$surface <- st_area(denv_nondate)
dcommunes$surface <- st_area(dcommunes)



# simplification de geométries
# dcommunesSimple <- ms_simplify(dcommunes, keep = 0.1)
# 
# dtupSimple <- st_collection_extract(dtup, "POLYGON")
# dtupSimple %>% st_cast("MULTIPOLYGON")
# dtupSimple <- ms_simplify(dtupSimple, keep = 0.5, keep_shapes = TRUE) ## fait planter R !!


# reprojection

dcommunes <- st_transform(dcommunes, crs = 4326)
denv_date <- st_transform(denv_date, crs = 4326)
denv_nondate <- st_transform(denv_nondate, crs = 4326)
dbati <- st_transform(dbati, crs = 4326)

# creation du geopackage

setwd("\\\\isere/DETC/AFFAIRES/GT/DDT25_Appui_Obs_Conso_Espace2020/3_Travail/SIG/shiny_app")

st_write(dcommunes, dsn = "./data/bdd25.gpkg", layer = "communes", driver = "GPKG", layer_options = 'OVERWRITE=YES', update = TRUE)
st_write(denv_date, dsn = "./data/bdd25.gpkg", layer = "env_date", driver = "GPKG", layer_options = 'OVERWRITE=YES', update = TRUE)
st_write(denv_nondate, dsn = "./data/bdd25.gpkg", layer = "env_nondate", driver = "GPKG", layer_options = 'OVERWRITE=YES', update = TRUE)
st_write(dbati, dsn = "./data/bdd25.gpkg", layer = "bati", driver = "GPKG", layer_options = 'OVERWRITE=YES', update = TRUE)

# connexion au geopackage


conn2 <- dbConnect(RSQLite::SQLite(), dbname = "./data/bdd25.gpkg")

# ajout tables sans géométrie
# dbWriteTable(conn2, "data_insee", dinsee)
# dbWriteTable(conn2, "data_emploi", demploi)
dbWriteTable(conn2, "zonages", dzonages)
dbWriteTable(conn2, "indicateurs", dindicateurs, overwrite = TRUE)

# listing des tables
dbListTables(conn2)

# creation d'index
dbGetQuery(conn2, "CREATE INDEX 'insee_com_idx' ON communes ('insee_com')")
dbGetQuery(conn2, "CREATE INDEX 'code_insee_idx' ON env_date ('code_insee')")
dbGetQuery(conn2, "CREATE INDEX 'datation_idx' ON env_date ('datation')")
dbGetQuery(conn2, "CREATE INDEX 'code_insee_ndt_idx' ON env_nondate ('code_insee')")
dbGetQuery(conn2, "CREATE INDEX 'bati_inseecom_idx' ON bati ('insee_com')")
dbGetQuery(conn2, "CREATE INDEX 'indic_inseecom_idx' ON indicateurs ('insee_com')")

dbDisconnect(conn2)



# exécution d'une requête
# commune <- st_as_sf(dbGetQuery(conn2, "select * from communes_025_2018_4326 WHERE insee_com = '25001' "))
# tup <- st_as_sf(dbGetQuery(conn2, "select * from tup_025_2018_4326 WHERE idcom = '25462'"))

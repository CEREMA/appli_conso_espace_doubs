#
# APPLI DE VISUALISATION DES ENVELOPPES BATIES DU DOUBS
# 
#

library(sf)
library(DBI)
library(rgdal)
library(RSQLite)

library(plotly)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(data.table)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(lwgeom)
library(classInt)

# LECTURE DES TABLES DEPUIS LE GEOPACKAGE -----------------------------------

conn <- dbConnect(RSQLite::SQLite(), dbname = "./data/bdd25.gpkg")
dcommunes <- st_as_sf(dbGetQuery(conn, "SELECT * FROM communes"))
dzonages <- dbReadTable(conn, "zonages")
dindic <- dbReadTable(conn, "indicateurs")

st_crs(dcommunes) = 4326

# liste déroulante pour choix des communes

choixcom <- dcommunes$insee_com
names(choixcom) <- dcommunes$nom_com

# liste déroulante pour choix des zonages d'étude

choixzone <- dzonages$id_zone
names(choixzone) <- dzonages$nom_zone

# année de référence par défaut

annee_t0 <- 1990

# calcul d'indicateurs supplémentaires

dcom <- dcommunes %>% select(nom_com, insee_com, surface)

dindic <- dindic %>%
  dplyr::left_join(dcom) %>%
  mutate(sartif = senv17 + senvnd,
         partif = 10000 * sartif / surface,
         cos = sbati / (senv17 + senvnd),
         sartif_par_hab = 10000 * sartif / p17_pop,
         occpot17 = p17_pop + p17_emplt + p17_rsec,
         occpot12 = p12_pop + p12_emplt + p12_rsec,
         occpot07 = p07_pop + p07_emplt + p07_rsec,
         sartif_par_op = 10000 * sartif / occpot17,
         sartif_evo_men = menhab1217
  )
  dindic <- st_as_sf(dindic)
  
# restructuration des données temporelles
  
dpop <- as.data.table(dindic) %>% 
  select(insee_com, p17_pop, p12_pop, p07_pop, d99_pop, d90_pop, d82_pop, d75_pop, d68_pop) %>%
  melt(id = c("insee_com"), variable.name = "annee", value.name = "population") %>%
  mutate(annee = strtoi(substr(annee, 2, 3)) + 2000) %>%
  mutate(annee = ifelse(annee > 2050, annee - 100, annee))

drsec <- as.data.table(dindic) %>% 
  select(insee_com, p17_rsec, p12_rsec, p07_rsec, d99_rsec, d90_rsec, d82_rsec, d75_rsec, d68_rsec) %>%
  melt(id = c("insee_com"), variable.name = "annee", value.name = "rsec") %>%
  mutate(annee = strtoi(substr(annee, 2, 3)) + 2000) %>%
  mutate(annee = ifelse(annee > 2050, annee - 100, annee))

dsenv <- as.data.table(dindic) %>% 
  select(insee_com, senv17, senv12, senv07, senv99, senv90, senv82, senv75, senv68, senvnd) %>%
  mutate(senv17 = senv17 + senvnd) %>%
  mutate(senv12 = senv12 + senvnd) %>%
  mutate(senv07 = senv07 + senvnd) %>%
  mutate(senv99 = senv99 + senvnd) %>%
  mutate(senv90 = senv90 + senvnd) %>%
  mutate(senv82 = senv82 + senvnd) %>%
  mutate(senv75 = senv75 + senvnd) %>%
  mutate(senv68 = senv68 + senvnd) %>%
  melt(id = c("insee_com"), variable.name = "annee", value.name = "stot") %>%
  mutate(annee = strtoi(substr(annee, 5, 6)) + 2000) %>%
  mutate(annee = ifelse(annee > 2050, annee - 100, annee))

demplt <- as.data.table(dindic) %>% 
  select(insee_com, p17_emplt, p12_emplt, p07_emplt) %>%
  melt(id = c("insee_com"), variable.name = "annee", value.name = "emplt") %>%
  mutate(annee = strtoi(substr(annee, 2, 3)) + 2000)

docpot <- as.data.table(dindic) %>% 
  select(insee_com, occpot17, occpot12, occpot07) %>%
  melt(id = c("insee_com"), variable.name = "annee", value.name = "ocpot") %>%
  mutate(annee = strtoi(substr(annee, 7, 8)) + 2000)

dmen <- as.data.table(dindic) %>% 
  select(insee_com, men12, men17) %>%
  melt(id = c("insee_com"), variable.name = "annee", value.name = "men") %>%
  mutate(annee = strtoi(substr(annee, 4, 5)) + 2000)

dtempo <- dpop %>% 
  dplyr::left_join(drsec, by = NULL, copy = FALSE) %>%
  dplyr::left_join(dsenv, by = NULL, copy = FALSE) %>%
  dplyr::left_join(demplt, by = NULL, copy = FALSE) %>%
  dplyr::left_join(docpot, by = NULL, copy = FALSE) %>%
  dplyr::left_join(dmen, by = NULL, copy = FALSE)



# couleurs pour les graphiques

col1 <- "#F58220" #orange
col2 <- "#268966" # pistache
col3 <- "#33475b" # bleu nuit

# dataframe nul

sfnul <- st_sf(surface = 0, datation = 0, geom = dcommunes$geom[1])

# années de référence

anneesref <- c(1968, 1975, 1982, 1990, 1999, 2007, 2012, 2017)

# nom des indicateurs

ind1 <- "surface artificialisée par le bâti en 2017"
ind2 <- "évolution de la surface artificialisée par le bâti"
ind3 <- "part de la surface communale artificialisée par le bâti en 2017"
ind4 <- "évolution relative de la surface artificialisée par le bâti"
ind5 <- "coefficient d'emprise au sol du bâti en 2017"
ind6 <- "surface artificialisée par habitant en 2017"
ind7 <- "surface artificialisée par occupant potentiel en 2017"
# ind8 <- "surface artificialisée par nouvel occupant potentiel"
ind9 <- "nombre de nouveaux ménages par ha artificialisé pour l'habitat"

i1 <- gsub(" ", "_", ind1)
i2 <- gsub(" ", "_", ind2)
i3 <- gsub(" ", "_", ind3)
i4 <- gsub(" ", "_", ind4)
i5 <- gsub(" ", "_", ind5)
i6 <- gsub(" ", "_", ind6)
i7 <- gsub(" ", "_", ind7)
# i8 <- gsub(" ", "_", ind8)
i9 <- gsub(" ", "_", ind9)


# requetes pour tester le fonctionnement de l'appli
## com_date <- dbGetQuery(conn, "SELECT code_insee, datation, surface FROM env_date WHERE code_insee = '25001'")
## com_bati <- st_as_sf(dbGetQuery(conn, "SELECT * FROM bati WHERE insee_com = '25001'", crs = 4326))


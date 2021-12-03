# Création des cartes -------------------------------------

# Encodage du fichier : UTF-8


## Importation des packages -----
library(here) # pour chemin valable partout
library(tidyverse) 
library(magrittr)
library(readr)
library(stringr) # travail sur les chaines de caractères
library(sf) # cartes
library(tmap) # idem

## Création des bases de données --------------------

### Importation des bases de données ----
#### BDD groupes (scraping ~ 20 mars) ----
groupes <- read_csv2(here("Donnees","Groupes.csv")) %>% tibble

groupes %<>% # transformation pour faire correspondre aux shapefiles
  mutate(code_insee = case_when(
  str_detect(ville, "Ajaccio") | str_detect(ville, "pietrosella") | str_detect(ville,"Bonifacio") ~ "2A",
  str_detect(ville, "Bastia") ~ "2B",
  location_country %in% c("FR", "GF", "GP", "MQ", "RE", "YT") ~ str_replace_all(str_extract(ville, "\\d+.\\d+")," ","") %>%
    str_sub(-5,-1), # code departement qu'il faut pour Métropole et DOM-TOM
  TRUE ~ "99" # étranger
)) %>%
  mutate(
    code_insee = case_when( # code à deux chiffres pour la métropole, à un pour les DOM-TOM
      str_sub(code_insee, 1, 2) == "97" ~ str_sub(code_insee, 1, 3),
      str_sub(code_insee, 1, 2) != "97" ~ str_sub(code_insee, 1, 2)
    ),
    nb_membres =  str_extract(nb_membres, "\\d+") %>% as.numeric # extraction chiffres
  )

# Corrections de bugs
groupes$code_insee[groupes$id == "https://actionpopulaire.fr/groupes/aefdf992-57f8-408c-af51-e2058c28008f/accueil/"] <- "37" 
groupes$code_insee[groupes$ville == "04 Alpes de haute provence"] <- "04"


#### Fonds de carte ----
# Métropole
fra_met <- readRDS(here("Donnees","Carto","gadm36_FRA_2_sf.rds")) %>% 
  rename(code_insee = CC_2) %>% 
  st_transform(crs = "WGS84") %>%
  mutate(NAME_0 = "France Métropolitaine")

# Guadeloupe
glp <- readRDS(here("Donnees","Carto","gadm36_GLP_0_sf.rds")) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(NAME_1 = NAME_0,
         NAME_2 = NAME_0,
         code_insee = "971",
         NAME_0 = "DOM-TOM")

# Martinique
mtq <- readRDS(here("Donnees","Carto","gadm36_MTQ_0_sf.rds")) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(NAME_1 = NAME_0,
         NAME_2 = NAME_0,
         code_insee = "972",
         NAME_0 = "DOM-TOM")

# Guyane
guy <- readRDS(here("Donnees","Carto","gadm36_GUF_0_sf.rds")) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(NAME_0 = "DOM-TOM", # shapefile différent
         NAME_1 = "Guyane",
         NAME_2 = "Guyane",
         code_insee = "973")

# Réunion
reu <- readRDS(here("Donnees","Carto","gadm36_REU_0_sf.rds")) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(NAME_0 = "DOM-TOM", # idem
         NAME_1 = "La Réunion",
         NAME_2 = "La Réunion",
         code_insee = "974",
         )

# Mayotte
myt <- readRDS(here("Donnees","Carto","gadm36_MYT_0_sf.rds")) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(NAME_1 = NAME_0,
         NAME_2 = NAME_0,
         code_insee = "976",
         NAME_0 = "DOM-TOM")

# Fusion
fra <- fra_met %>% 
  bind_rows(glp,mtq,guy,reu,myt) %>% 
  select(NAME_0,NAME_1,NAME_2,code_insee)


#### Jointure groupes-fonds de cartes ----
groupes_fra <- fra %>% 
  left_join(groupes, by = "code_insee") %>% # code Insee pour agréger par département
  select(NAME_0,NAME_1,NAME_2,code_insee, nb_membres, is_active, certifie, geometry) %>% 
  group_by(NAME_0,NAME_1,NAME_2,code_insee) %>% 
  summarise(groupes = n(), # nombre de groupes par département 
            actifs = sum(is_active), # nombre actifs
            certifie = sum(certifie), # nombre certifiés
            membres = sum(nb_membres, na.rm = TRUE), # nombre de membres par département
            membres_actif = sum(nb_membres[is_active], na.rm = TRUE), # nombre de membres dans les groupes actifs
            membres_certif = sum(nb_membres[certifie], na.rm = TRUE)) # nombre de membres dans les groupes certifiés

#### Fond de carte des régions (métropole) ----
groupes_fra_reg <- st_read(here("Donnees","Carto","regions","regions-20180101.shp")) %>% 
  filter(!(nuts2 %in% c("FR91","FR92","FR93","FR94")), !is.na(nuts2)) # on ne garde que la métropole


## Carte avec données questionnaire ------------------
# Pour des raisons de confidentialité, il n'y a ici que les ID + départements
nb_quest_dept <- read_csv2(here("Donnees","quest_dept.csv")) %>% 
  mutate(code_insee = str_extract(dept, "\\d+")) %>% # extraction chiffres
  group_by(code_insee) %>% # regroupement par code_insee
  tally(name = "nb_questio") # total par departement

groupes_fra %<>% # fusion avec le shapefile
  merge(nb_quest_dept, by = "code_insee", all.x = T) %>% 
  mutate(nb_questio = ifelse(is.na(nb_questio), 0, nb_questio))# si pas de donnée alors 0

groupes_fra %<>%
  mutate(pct_questio = nb_questio / sum(groupes_fra$nb_questio), # poids du nombre de réponse par département
         pct_membres = membres / sum(groupes_fra$membres), # poids du nombre de membres théorique
         pct_membres_actif = membres_actif / sum(groupes_fra$membres_actif), # poids membres de groupes actifs
         pct_membres_certif = membres_certif / sum(groupes_fra$membres_certif)) %>% # poids membres de groupes certifiés
  mutate(diff_pct = (pct_questio - pct_membres) * 100, # différence de pourcentage entre membres théoriques et enquêtés
         diff_pct_actif = (pct_questio - pct_membres_actif) * 100, # idem groupes actifs
         diff_pct_certif = (pct_questio - pct_membres_certif) * 100) %>% # idem groupes certifiés
  mutate(carte = case_when(
    NAME_0 == "France Métropolitaine" ~ NAME_0,
    T                                 ~ NAME_1 # tri pour les ≠ fonds de carte
  ))

# Construction des cartes --------

## Figure 1.2.1 (en comptant tous les groupes) ----
met <- tm_shape(groupes_fra %>%  filter(NAME_0 == "France Métropolitaine")) + # fond de carte métropole
  tm_polygons(col = "diff_pct", # couleur en fonction de la différence entre les pourcentages
              legend.show = TRUE,
              style = "cont",
              title = "Sur ou sous représentation\n(points de %)",
              palette = '-RdYlGn', midpoint = 0, #centrer le gradient en diff = 0
              breaks = c(-6,-4,-2,0,2,4,6), # échelle fixée pour l'utiliser aussi avec les DOM-TOM
              border.col = "black", border.alpha = 0.8) +
  tm_layout(frame = FALSE,
            main.title = "Représentation des militants insoumis en France\nselon le département (en comptant tous les groupes)",
            legend.outside = TRUE,
            legend.outside.position	= "right",
            legend.stack = "vertical",
            fontfamily = "Times New Roman") +
  tm_shape(groupes_fra_reg) + # Ajout d'un calque avec les frontières de région
  tm_borders(col = "Black", lwd = 3) # plus épaisses que les autres 

dom <- tm_shape(groupes_fra %>% filter(NAME_0 == "DOM-TOM")) + # carte DOM-TOM (même principe qu'au-dessus)
  tm_polygons(col = "diff_pct", legend.show = FALSE, style = "cont",
              palette = '-RdYlGn', midpoint = 0,
              breaks = c(-6,-4,-2,0,2,4,6),
              border.col = "black", border.alpha = 0.8) +
  tm_facets("NAME_1", ncol = 1, scale.factor = 0.5) + # un encadré par DOM-TOM -> sinon carte du monde illisible, 1 colonne
  tm_layout(frame = FALSE,
            panel.label.size = 0.8,
            panel.label.bg.color = "white",
            fontfamily = "Times New Roman",
            asp = 2)

tmap_arrange(dom, met, widths = c(0.3,0.7), asp = NULL, outer.margins = NULL) # DOM-TOM à gauche, métropole à droite


## Figure 1.2.2 (en comptant les groupes actifs) ----
# même principe
met_actif <- tm_shape(groupes_fra %>%  filter(NAME_0 == "France Métropolitaine")) +
  tm_polygons(col = "diff_pct_actif", legend.show = TRUE, style = "cont",
              title = "Sur ou sous représentation\n(points de %)",
              palette = '-RdYlGn', midpoint = 0,
              breaks = c(-6,-4,-2,0,2,4,6),
              border.col = "black", border.alpha = 0.8) +
  tm_layout(frame = FALSE,
            main.title = "Représentation des militants insoumis en France\nselon le département (en ne comptant que les groupes actifs)",
            legend.outside = TRUE,
            legend.text.size = 1,
            legend.outside.position	= "right",
            legend.stack = "vertical",
            fontfamily = "Times New Roman")
dom_actif <- tm_shape(groupes_fra %>% filter(NAME_0 == "DOM-TOM")) +
  tm_polygons(col = "diff_pct_actif", legend.show = FALSE, style = "cont",
              palette = '-RdYlGn', midpoint = 0,
              breaks = c(-6,-4,-2,0,2,4,6),
              border.col = "black", border.alpha = 0.8) +
  tm_facets("NAME_1", ncol = 1, scale.factor = 0.5) +
  tm_layout(frame = FALSE,
            panel.label.size = 0.8,
            panel.label.bg.color = "white",
            fontfamily = "Times New Roman",
            asp = 2)
tmap_arrange(dom_actif,met_actif, widths = c(0.3,0.7), asp = NULL, outer.margins = NULL)

## Figure 1.2.2 (en comptant les groupes certifiés) ----
# même principe
met_certif <- tm_shape(groupes_fra %>%  filter(NAME_0 == "France Métropolitaine")) +
  tm_polygons(col = "diff_pct_certif", legend.show = TRUE, style = "cont",
              title = "Sur ou sous représentation\n(points de %)",
              palette = '-RdYlGn', midpoint = 0,
              breaks = c(-6,-4,-2,0,2,4,6),
              border.col = "black", border.alpha = 0.8) +
  tm_layout(frame = FALSE,
            main.title = "Représentation des militants insoumis en France\nselon le département (en ne comptant que les groupes certifiés)",
            legend.outside = TRUE,
            legend.text.size = 1,
            legend.outside.position	= "right",
            legend.stack = "vertical",
            fontfamily = "Times New Roman")
dom_certif <- tm_shape(groupes_fra %>% filter(NAME_0 == "DOM-TOM")) +
  tm_polygons(col = "diff_pct_certif", legend.show = FALSE, style = "cont",
              palette = '-RdYlGn', midpoint = 0,
              breaks = c(-6,-4,-2,0,2,4,6),
              border.col = "black", border.alpha = 0.8) +
  tm_facets("NAME_1", ncol = 1, scale.factor = 0.5) +
  tm_layout(frame = FALSE,
            panel.label.size = 0.8,
            panel.label.bg.color = "white",
            fontfamily = "Times New Roman",
            asp = 2)
tmap_arrange(dom_certif,met_certif, widths = c(0.3,0.7), asp = NULL, outer.margins = NULL)




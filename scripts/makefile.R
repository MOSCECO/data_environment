# Routine de préparation des données environnementales pour les occurrences
# d'espèces globales moissonnées depuis le GBIF.

source(here::here("scripts", "boot.R"))
# Chargement des occurrences
# Histoire du gebcoast à clarifier

# (1) Téléchargements ----
# des données environnementales depuis copernicus
source(here::here("scripts", "download_physics_so_copernicus.R"))
source(here::here("scripts", "download_physics_bottomT_copernicus.R"))
source(here::here("scripts", "download_waves_copernicus.R"))

# (2) Extraction ----
# des variables environnementales pour chaque occurrences
# sur toutes les grilles de données disponibles
# + Réaliser une climatologie (moyennes et "écarts-types") pour chaque variable
source(here::here("scripts", "home_pointer_bottomT.R"))
source(here::here("scripts", "home_pointer_so.R"))
# Particularité ici : il faut déterminer au préalable les profondeurs de chaque
# occurrences (grâce aux cartes du gebco) et extraire les données dans
# les quatre différentes couches de salinité disponibles.
source(here::here("scripts", "home_pointer_waves.R"))

# Pour les modèles de distribution d'espèces au niveau global, il faut tirer
# des pseudo-absences. Pour cela il faut une grille de données pour la zone d'
# étude, à savoir les amériques (et la Méditerrannée et l'Afrique pour deux
# espèces). Pour limiter les temps de calcul, une bande de 0 m à n m de
# profondeur sera extraite pour calculer ces climatologies, où n est la
# profondeur maximale observée pour les occurrences mondiales des espèces.

# Boucle pour produire des spatial raster de profondeurs rognés pour les
# climatologies globales /!\ Produit trop de données ! On abandonne la relation
# à l'espèce pour ces phases pour le moment et on réduit l'emprise des climatos
# au moment de la sélection des pseudo-absences.
source(here::here("scripts", "boot.R"))
source(here::here("scripts", "grille_bathymétrique_pour_occurrences.R"))
# source(here::here("scripts", "filtre_profondeurs_gebco_par_espece.R"))
# code pour rééchantillonner les grilles de profondeurs (totales et par paliers)
# qui servent ensuite au calcule de climatologies sur le cluster
source(here::here("scripts", "grille_bathymétrique_reechantillonnee_0.083x0.083.R"))

# Anciens codes de climatologies (pré livrable 02)
source(here::here("scripts", "climatologies_vagues.R"))
source(here::here("scripts", "climatologies_salinité.R"))
source(here::here("scripts", "climatologies_salinité_hybride.R"))
# aggrégation de toutes les climatologies dans un seul objet stars
source(here::here("scripts", "climatologies_finale.R"))

# Code fonctionnel pour les climatologies :
source(here::here("scripts", "climatologies_stars_bottomt_valeurs_cotieres.R"))
# Modification de ce code fonctionnel au 29/08/2023
source(here::here("scripts", "climatologies_copernicus_ameriques_cotier.R"))

# Nouveaux codes de climatologies pour le cluster :
source(here::here("scripts", "boot_cluster.R"))
# Codes avec tentative de parallélisation inconcluant sur le cluster
# problème d'export des fonctions d'un paquet dans l'environnement de la boucle
# parallélisée
source(here::here("scripts", "climatologies_bottomt_cluster.R")) # V1
source(here::here("scripts", "climatologies_globales_cluster.R"))
# Découpage des données pour réduire le chargement
source(here::here("scripts", "climatologies_globales_cluster_chunks.R"))
# Le même sans parallélisation :
source(here::here("scripts", "climatologies_globales_cluster_chunks_nopar.R"))
# Qui sert de base aux scripts finaux (cc : climatologies cluster)
# Au 14/09/2023 : problème d'aggrégation des fichiers
# au moment du CLIM_mosaic (voir dossier scripts/ERR) : c'est parce que les
# chemins de sauvegarde pour les climatologies quantiles étaient mal renseignés
# du coup j'écrasais chaque fichier à chaque fois... corrigé dans cc_bottomt
# seulement
source(here::here("scripts", "cc_sw1.R"))
source(here::here("scripts", "cc_ww.R"))
source(here::here("scripts", "cc_vhm0.R"))
source(here::here("scripts", "cc_so.R"))
source(here::here("scripts", "cc_bottomt.R"))
# Problème d'agrégation pendant les clusters, j'agrège donc manuellement
# les climatologies entre elles avec :
source(here::here("scripts", "cc_aggregation.R"))
# Agrégation des différente salinités en un seul objet stars
source(here::here("scripts", "climatologies_salinité_hybride.R"))
# sauvegarde pour utilisaiton par les modèles d'habitats (cgc)
source(here::here("scripts", "climatologies_globales_copernicus.R"))

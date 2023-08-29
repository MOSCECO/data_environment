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

# Tentative de climatologies avec stars qui ne semble pas concluant
# pour le moment :
source(here::here("scripts", "climatologies_vagues.R"))
source(here::here("scripts", "climatologies_salinité.R"))
source(here::here("scripts", "climatologies_salinité_hybride.R"))
# aggrégation de toutes les climatologies dans un seul objet stars
source(here::here("scripts", "climatologies_finale.R"))
source(here::here("scripts", "climatologies_stars_bottomt_valeurs_cotieres.R"))
source(here::here("scripts", "climatologies_stars_bottomt.R"))

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
source(here::here("scripts", "interaction_occurrences_profondeurs_stars.R"))
source(here::here("scripts", "filtre_profondeurs_gebco_par_espece.R"))

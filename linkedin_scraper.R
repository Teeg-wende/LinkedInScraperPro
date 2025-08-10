# Scrapping LinkedIn ------------------------------------------------------
# Email : twinoussa55@gmail.com
# Tel : +33 7 73 25 55 32
# Rennes Student in MAS
# -------------------------------------------------------------------------

# Chargement des librairies nécessaires pour les requêtes HTTP, parsing HTML, manipulation de données et suivi de progression
library(httr)      # Pour effectuer des requêtes HTTP
library(rvest)     # Pour parser le contenu HTML
library(dplyr)     # Pour manipuler les data frames
library(purrr)     # Pour la programmation fonctionnelle (map, etc.)
library(progress)  # Pour afficher une barre de progression dans la console

# URL de base pour accéder à l'API LinkedIn des offres d'emploi invitées
api <- "https://www.linkedin.com/jobs-guest/jobs/api/seeMoreJobPostings/search?"

# URL principale pour la recherche d'offres LinkedIn
main_url <- "https://www.linkedin.com/jobs/search"

# Fonction pour construire dynamiquement une URL avec des paramètres de requête encodés
# base_url : chaîne de base de l'URL
# ... : paramètres nom=valeur à inclure dans la requête
# multi_keys : vecteur optionnel de clés qui acceptent plusieurs valeurs
build_url <- function(base_url, ..., multi_keys = NULL) {
  params <- list(...)
  # Supprime les paramètres NULL
  params <- params[!sapply(params, is.null)]
  
  # Construction de chaque paramètre encodé
  query_parts <- unlist(lapply(names(params), function(key) {
    val <- params[[key]]
    val <- as.character(val)
    encoded_vals <- utils::URLencode(val, reserved = TRUE)
    if (!is.null(multi_keys) && key %in% multi_keys) {
      paste0(key, "=", encoded_vals)
    } else {
      paste0(key, "=", encoded_vals[1])
    }
  }))
  
  # Assemblage de la chaîne de requête
  query_string <- paste(query_parts, collapse = "&")
  final_url <- paste0(base_url, "?", query_string)
  return(final_url)
}

# Fonction pour générer une URL LinkedIn dynamique selon mots-clés, localisation, ID géographique et filtre temps
# mot_cle : mot-clé de recherche (ex: "data scientist")
# location : localisation géographique (ex: "France")
# geoId : identifiant géographique LinkedIn
# f_TPR : filtre temps (ex: "r604800" pour la dernière semaine)
url_linkedin <- function(
    mot_cle, location = "France", geoId = "105015875", f_TPR = "r604800") {
  base <- api
  params <- list(
    keywords = mot_cle,
    location = location,
    geoId = geoId,
    f_TPR = f_TPR
  )
  # Encodage manuel des paramètres
  query <- paste0(names(params), "=", lapply(params, URLencode), collapse = "&")
  paste0(base, query)
}

# Fonction pour générer l'URL principale d'une page de recherche LinkedIn
# Permet la navigation par page avec paramètres de pagination
url_page_principale_linkedin <- function(
    keywords = "", 
    location = "France", 
    geoId = "", 
    position = 1, 
    pageNum = 0
) {
  build_url(
    base_url = main_url,
    keywords = keywords,
    location = location,
    geoId = geoId,
    trk = "public_jobs_jobs-search-bar_search-submit",
    position = position,
    pageNum = pageNum
  )
}

# Fonction pour lire le contenu HTML d'une page LinkedIn en simulant un navigateur
# url : URL à lire
# Retourne un objet HTML parsé avec rvest
lire_page_linkedin <- function(url) {
  tx <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/115"
  ua <- user_agent(tx)          # Définition du user-agent pour éviter les blocages
  page <- read_html(GET(url, ua))  # Requête GET avec user-agent puis parsing HTML
  return(page)
}

# Fonction pour extraire le nombre total d'offres disponibles sur la page principale LinkedIn
# page : objet HTML parsé
# Retourne un entier correspondant au nombre d'offres
nombre_offres_page_linkedin <- function(page){
  element_html <- page |>
    html_element(".results-context-header__job-count") |>  # Sélecteur CSS spécifique LinkedIn
    html_text2()
  extraire_le_nombre <- gsub("[^0-9]", "",element_html)   # Extraction des chiffres uniquement
  nombre_offres <- as.integer(extraire_le_nombre)
  return(nombre_offres)
}

# Génère une URL paginée et récupère la page HTML correspondante
# url : URL de base
# numero_page : numéro de page (décalage start)
# Retourne la page HTML parsée
generer_url_linkedin_via_numero_page <- function(url, numero_page){
  url <- paste0(url, "&start=", numero_page)
  page <- lire_page_linkedin(url)
  return(page)
}

# Extraction des informations sommaires des offres présentes sur une page LinkedIn
# page : page HTML parsée
# Retourne un data.frame avec colonnes : titre, entreprise, lieu, date, logo, lien vers l'offre
collect_infos_offres_linkedin <- function(page){
  titres <- page |> 
    html_elements(".base-search-card__title") |> html_text2()
  entreprises <- page |> 
    html_elements(".base-search-card__subtitle") |> html_text2()
  lieux <- page |> 
    html_elements(".job-search-card__location") |> html_text2()
  dates <- page |> 
    html_elements("time") |> html_attr("datetime")
  logos <- page |> 
    html_elements("img") |> html_attr("data-delayed-url")
  liens <- page |> 
    html_elements(".base-card__full-link") |> html_attr("href")
  
  db <- data.frame(
    titre = titres, entreprises = entreprises, lieux = lieux,
    dates = dates, logos = logos, liens = liens,
    stringsAsFactors = FALSE
  )
  return(db)
}

# Extraction des détails d'une offre depuis sa page individuelle
# page : page HTML parsée de l'offre
# Retourne un data.frame avec description, niveau hiérarchique, type d'emploi, fonction et secteur
collect_details_offres_linkedin <- function(page){
  description <- page %>% 
    html_elements(".show-more-less-html__markup") %>% html_text2()
  critere_job <- page %>% 
    html_elements(".description__job-criteria-list li") %>% 
    html_element("span") |> html_text2()
  
  # On suppose que les critères sont dans l’ordre : hiérarchie, type, fonction, secteur
  db <- data.frame(
    description = description,
    niveau_hierarchique = critere_job[1],
    type_emploi = critere_job[2],
    fonction = critere_job[3],
    secteur = critere_job[4],
    stringsAsFactors = FALSE
  )
  return(db)
}

# Fonction principale de scraping par chunks (lots) de pages
# url : URL de recherche LinkedIn
# start_page : page de départ (0-indexée)
# end_page : page finale
# pause_base : durée minimum de pause entre chunks (en secondes)
# Retourne un data.frame complet des résultats
auto_chunk_scraper <- function(url, start_page, end_page, pause_base = 60) {
  
  total_pages <- end_page - start_page + 1
  
  # Choix adaptatif de la taille du chunk en fonction du nombre total de pages
  if(total_pages <= 300) chunk_size <- total_pages
  else if(total_pages <= 1000) chunk_size <- sample(80:150, 1)
  else chunk_size <- sample(50:100, 1)
  
  chunks <- split(start_page:end_page, ceiling(seq_along(start_page:end_page) / chunk_size))
  
  message(sprintf("Nombre total de pages : %d", total_pages))
  message(sprintf("Taille de chunk choisie : %d", chunk_size))
  message(sprintf("Nombre de chunks : %d", length(chunks)))
  
  all_data <- list()
  
  for(i in seq_along(chunks)) {
    pages <- chunks[[i]]
    message(sprintf("\n Traitement du chunk %d : pages %d à %d", i, min(pages), max(pages)))
    
    chunk_result <- tryCatch({
      pb <- progress_bar$new(total = length(pages))
      
      # Pour chaque page dans le chunk
      donnees <- lapply(pages, function(num_page){
        Sys.sleep(runif(1, 1.5, 3))     # Pause aléatoire pour simuler un comportement humain
        pb$tick()
        page_numerote <- generer_url_linkedin_via_numero_page(url, num_page)
        resultats <- collect_infos_offres_linkedin(page_numerote)
        if (!is.null(resultats) && is.data.frame(resultats) && nrow(resultats) > 0)
          return(resultats)
        else return(NULL)
      })
      
      # Nettoyage des résultats : suppression des NULL
      donnees_clean <- Filter(Negate(is.null), donnees)
      donnees_df1 <- bind_rows(donnees_clean)
      donnees_unique <- distinct(donnees_df1)
      
      # Progression pour récupérer les détails complets des offres uniques
      pb <- progress_bar$new(total = length(donnees_unique[["liens"]]))
      donnees_sup <- lapply(donnees_unique[["liens"]], function(lien) {
        Sys.sleep(runif(1, 1.5, 3))
        pb$tick()
        page <- lire_page_linkedin(lien)
        collect_details_offres_linkedin(page)
      })
      
      donnees_df2 <- bind_rows(donnees_sup)
      cbind(donnees_unique, donnees_df2)
    }, 
    error = function(e) message(sprintf("❌ Erreur : %s", e$message)))
    
    if (!is.null(chunk_result) && nrow(chunk_result) > 0) {
      all_data[[length(all_data) + 1]] <- chunk_result
    }
    
    # Pause entre chunks pour réduire la charge serveur
    if (i < length(chunks)) {
      pause <- pause_base + sample(5:20, 1)
      message(sprintf("⏸ Pause de %d secondes avant le prochain chunk...", pause))
      Sys.sleep(pause)
    }
  }
  
  final_data <- bind_rows(all_data)
  message("✅ Scraping terminé.")
  return(final_data)
}

# Fonction d'encapsulation complète : construit l'URL, estime le nombre de pages à scraper,
# lance le scraping et retourne les résultats
# keys : mots-clés de recherche
# loc : localisation géographique
# geoId : identifiant géographique LinkedIn
# f_TPR : filtre temps pour la fraîcheur des offres
# max_pages : optionnel, limite du nombre de pages à scraper

parse_linkedin_data <- function(keys, loc, geoId, f_TPR, max_pages = NULL) {
  url <- url_linkedin(keys, loc, geoId, f_TPR)
  
  # Récupération de la page principale pour estimer le nombre total d'offres
  url_pp <- url_page_principale_linkedin(keys, loc, geoId)
  page_pp <- lire_page_linkedin(url_pp)
  nb_offres <- nombre_offres_page_linkedin(page_pp)
  
  # Estimation du nombre d'offres par page pour calculer le nombre total de pages
  page <- lire_page_linkedin(url)
  nb_offres_par_page <- length(
    page |> html_elements(".base-search-card__title") |> html_text2()
  )
  
  nb_pages <- ceiling(nb_offres / nb_offres_par_page)
  
  # Application de la limite max_pages si spécifiée
  if (!is.null(max_pages)) nb_pages <- min(nb_pages, max_pages)
  
  message(sprintf("LinkedIn : %d offres sur %d pages à scraper", nb_offres, nb_pages))
  
  # Lancement du scraping par chunks
  data <- auto_chunk_scraper(url, start_page = 0, end_page = nb_pages)
  return(data)
}

# Exemple d'utilisation : scraping limité à 20 pages pour toutes les offres en France
resultats <- parse_linkedin_data("", "France", "105015875", "r604800", 20)
View(resultats)

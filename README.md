
# Scraping LinkedIn en R

## 📋 Description

Ce projet propose un ensemble de fonctions R pour automatiser le scraping des offres d'emploi sur LinkedIn.  
Il permet d’extraire de manière structurée des informations clés telles que titres de poste, entreprises, lieux, dates, descriptions et critères spécifiques.  

Conçu pour les étudiants et professionnels en Data Science, Statistiques et Intelligence Artificielle, ce script facilite la collecte de données métier à grande échelle, en intégrant des pauses intelligentes pour éviter les blocages liés aux requêtes répétées.

---

## ⚙️ Fonctionnalités principales

- Construction dynamique et personnalisable des URLs de recherche LinkedIn  
- Parsing précis des pages HTML avec `rvest` pour extraire les données essentielles  
- Gestion de la pagination avec découpage en chunks pour un scraping efficace  
- Récupération des détails complémentaires pour chaque offre  
- Assemblage des données en data frames prêts à l’analyse  

---

## 🚀 Installation

Ce projet nécessite R (version 4.0+) avec les packages suivants :

```r
install.packages(c("httr", "rvest", "dplyr", "purrr", "progress"))
```

---

## 🛠️ Utilisation rapide

Voici un exemple minimal pour lancer le scraping sur des offres en France :

```r
source("linkedin_scraper.R")

resultats <- parse_linkedin_data(
  keys = "data scientist", 
  loc = "France", 
  geoId = "105015875", 
  f_TPR = "r604800",  # filtre date : offres de la dernière semaine
  max_pages = 20      # nombre maximum de pages à scraper
)

View(resultats)
```

---

## 📧 Contact

Pour toute question ou collaboration :  
**Email :** twinoussa55@gmail.com  
**Téléphone :** +33 7 73 25 55 32  

---

## ⚠️ Avertissements

- Veillez à respecter les conditions d’utilisation de LinkedIn et à ne pas surcharger leurs serveurs.  
- Ce code est fourni à titre pédagogique et doit être utilisé de manière responsable.  

---

## 🧑‍🎓 À propos

Projet développé par un étudiant en Master 2 Mathématiques Appliquées – Statistiques & IA, à Rennes.

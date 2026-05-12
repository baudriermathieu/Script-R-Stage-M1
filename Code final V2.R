# Library à charger ----

library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(geosphere)
library(mgcv)
library(gratia)
library(dunn.test)
library(report)
library(FactoMineR)
library(ggtext)

# Jeu de données à charger ----
jeu_donnees_final <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Jeu de données final/Jeu_de_données_final_V2.xlsx")
jeu_donnees_final_sans_lambert <- jeu_donnees_final[jeu_donnees_final$zone %in% c("plaine_des_maures", "lac_redon", "callas"),]
jeu_donnees_plaine_des_maures <- jeu_donnees_final[jeu_donnees_final$zone == "plaine_des_maures",]
jeu_donnees_callas <- jeu_donnees_final[jeu_donnees_final$zone == "callas",]
jeu_donnees_lac_redon <- jeu_donnees_final[jeu_donnees_final$zone == "lac_redon",]
jeu_donnees_lambert <- jeu_donnees_final[jeu_donnees_final$zone == "lambert",]

jeu_donnees_final_sans_lambert$zone[jeu_donnees_final_sans_lambert$zone == "lac_redon"] <- "Lac Redon"
jeu_donnees_final_sans_lambert$zone[jeu_donnees_final_sans_lambert$zone == "callas"] <- "Callas"
jeu_donnees_final_sans_lambert$zone[jeu_donnees_final_sans_lambert$zone == "plaine_des_maures"] <- "Plaine des Maures"

unique(jeu_donnees_plaine_des_maures$annee)
# Conversion jour julien en date ----

dates <- as.Date(314 - 1, origin = "2023-01-01")
format(dates, "%d/%m")

#####################################################
#### Création d'un jeu de données unique complet ####
#####################################################
  # Importation de plusieurs jeu de données ----
nom_fichiers <- list.files(path = "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Disque dur brut/Toutes les bases de données - avec suppression des non-adéquat/Base correcte_tous_les_mois/8", pattern = ".xlsx", full.names = TRUE)
liste_base_données <- lapply(nom_fichiers, read_excel)
# jeu_donnees_final <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Jeu de données final/Jeu_de_données_final_1.xlsx")

  # Code compilation jeux de données ----

    ## Compilation brute ----
jeu_donnees <- data.frame()

for(i in 1:length(liste_base_données)){
  if("age" %in% names(liste_base_données[[i]]) == TRUE){
    liste_base_données[[i]] <- liste_base_données[[i]] %>% filter(age >= 12) # Retrait juvéniles
  }
  if("statut_tr" %in% names(liste_base_données[[i]]) == TRUE){
    liste_base_données[[i]] <- liste_base_données[[i]] %>% filter(statut_tr != "trans") # Retrait transloquées
  }
  liste_base_données[[i]]$date <- as.Date(liste_base_données[[i]]$date, format = "%d-%m-%Y")
  liste_base_données[[i]]<-liste_base_données[[i]][,c("date","annee","mois","site","n","e","p_gps","id","sexe","activite","couv","bm","scl","expo","substrat","Cond_cach","comm")] # Sélection des variables d'intérêt
  jeu_donnees <- rbind(jeu_donnees, data.frame(liste_base_données[[i]])) # Compilation
}

    ## Affinage du jeu de données compilé ----

      ### Retrait des points sans coordonnées ou identifiant ----

jeu_donnees[] <- lapply(jeu_donnees, function(x) {
  if (is.character(x)) {
    x[x == "NA"] <- NA
  }
  x
})
jeu_donnees[] <- lapply(jeu_donnees, function(x) {
  if (is.character(x)) {
    x[x == "ND"] <- NA
  }
  x
})
jeu_donnees$n <- as.numeric(jeu_donnees$n)
jeu_donnees$e <- as.numeric(jeu_donnees$e)

# Retrait des points sans coordonnées
jeu_donnees <- jeu_donnees[!is.na(jeu_donnees$n), ]
jeu_donnees <- jeu_donnees[!is.na(jeu_donnees$e), ]

# Retrait des points sans identifiants
jeu_donnees <- jeu_donnees[!is.na(jeu_donnees$p_gps), ]
jeu_donnees <- jeu_donnees[!is.na(jeu_donnees$id), ]

# jeu_donnees <- jeu_donnees[!is.na(jeu_donnees$sexe), ] Pour retrait des juvéniles

      ### Retrait des points GPS associé au mauvais individu ----

mauvais_point_gps <- c()
jeu_donnees$id <- as.character(jeu_donnees$id)
jeu_donnees$p_gps <- as.character(jeu_donnees$p_gps)

for(i in 1:nrow(jeu_donnees)){
  if(grepl(jeu_donnees[i, "id"], jeu_donnees[i, "p_gps"]) == FALSE){
    mauvais_point_gps <- c(mauvais_point_gps, i)
  }
}
jeu_donnees <- jeu_donnees[-mauvais_point_gps,]

      ### Retrait des doublons ----

jeu_donnees <- jeu_donnees %>% jeu_donnees %>% mutate(nb_na = rowSums(is.na(.))) %>% arrange(nb_na) %>% distinct(date, id, .keep_all = TRUE) %>% select(-nb_na)

  # Exportation des données ----

write_xlsx(jeu_donnees, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Jeu de données provisoire_tous_les_mois/Jeu_de_données_provisoire_8.xlsx")
write_xlsx(jeu_donnees, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Disque dur brut/Toutes les bases de données - avec suppression des non-adéquat/Base correcte_tous_les_mois/8/Jeu_de_données_provisoire_7.xlsx")

###########################################
#### Obtention du jeu de données final ####
###########################################
  # Importation du jeu de données final ----

jeu_donnees_final <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Jeu de données provisoire_tous_les_mois/Jeu_de_données_provisoire_8.xlsx")

  # Conversion du format des variables ----
jeu_donnees_final$date <- as.Date(jeu_donnees_final$date, format = "%d-%m-%Y")
jeu_donnees_final$annee <- as.numeric(jeu_donnees_final$annee)
jeu_donnees_final$mois <- as.numeric(jeu_donnees_final$mois)
jeu_donnees_final$site <- as.character(jeu_donnees_final$site)
jeu_donnees_final$n <- as.numeric(jeu_donnees_final$n)
jeu_donnees_final$e <- as.numeric(jeu_donnees_final$e)
jeu_donnees_final$p_gps <- as.character(jeu_donnees_final$p_gps)
jeu_donnees_final$id <- as.character(jeu_donnees_final$id)
jeu_donnees_final$sexe <- as.character(jeu_donnees_final$sexe)
jeu_donnees_final$bm <- as.numeric(jeu_donnees_final$bm)
jeu_donnees_final$scl <- as.numeric(jeu_donnees_final$scl)
jeu_donnees_final$expo <- as.character(jeu_donnees_final$expo)

  # Contrôle qualitatif et uniformisation de la composition du jeu de données final ----

    ## Sites ----
for(i in 1:length(jeu_donnees_final$site)){
  if(jeu_donnees_final$site[i] == "saint_daumas" | jeu_donnees_final$site[i] == "saint daumas" | jeu_donnees_final$site[i] == "Saint daumas" | jeu_donnees_final$site[i] == "Saint Daumas" | jeu_donnees_final$site[i] == "st Daumas" | jeu_donnees_final$site[i] == "saint Daumas"){
    jeu_donnees_final$site[i] <- "St Daumas"
  }
  if(jeu_donnees_final$site[i] == "neuf_riaux" | jeu_donnees_final$site[i] == "neuf Riaux" | jeu_donnees_final$site[i] == "neufs riaux" | jeu_donnees_final$site[i] == "neuf riaux" | jeu_donnees_final$site[i] == "Neuf riaux"){
    jeu_donnees_final$site[i] <- "Neuf Riaux"
  }
  if(jeu_donnees_final$site[i] == "redon" | jeu_donnees_final$site[i] == "Redon"){
    jeu_donnees_final$site[i] <- "Lac Redon"
  }
  if(jeu_donnees_final$site[i] == "Rascas/Les mayons" | jeu_donnees_final$site[i] == "Les Mayons"){
    jeu_donnees_final$site[i] <- "Les mayons"
  }
  if(jeu_donnees_final$site[i] == "rouvede"){
    jeu_donnees_final$site[i] <- "Rouvède"
  }
}
unique(jeu_donnees_final$site)
table(jeu_donnees_final$site)

# Détection indivdus présents sur plusieurs sites sur plusieurs sites

sites_ind <- data.frame(ind = character(), site = character())
ind_plusieurs_sites <- data.frame(ind = character(), site = character())
for(i in 1:length(jeu_donnees_final$id)){
  if(jeu_donnees_final$id[i] %in% sites_ind$ind == F){
    sites_ind <- rbind(sites_ind, data.frame(ind = jeu_donnees_final$id[i], site = jeu_donnees_final$site[i]))
  } else {
    if(jeu_donnees_final$site[i] %in% sites_ind$site[sites_ind$ind == jeu_donnees_final$id[i]] == F){
      sites_ind <- rbind(sites_ind, data.frame(ind = jeu_donnees_final$id[i], site = jeu_donnees_final$site[i]))
      ind_plusieurs_sites <- rbind(ind_plusieurs_sites, data.frame(ind = jeu_donnees_final$id[i], site = c(unique(sites_ind$site[sites_ind$ind == jeu_donnees_final$id[i]]))))
    } else {
      next
    }
  }
}
ind_plusieurs_sites

    ## Année ----
jeu_donnees_final$annee[jeu_donnees_final$annee == "20120"] <- "2012"
unique(jeu_donnees_final$annee)

# Retrait du seul pointage de 2025 à St Daumas
temp <- c()
for(i in 1:length(jeu_donnees_final$annee)){
  if(jeu_donnees_final$annee[i] == 2025 & jeu_donnees_final$site[i] == "St Daumas"){
      temp <- c(temp, i)
  }
}
jeu_donnees_final <- jeu_donnees_final[-temp,]

    ## Sexe ----

jeu_donnees_final$sexe[jeu_donnees_final$sexe %in% c("Male", "Mâle", "m")] <- "M" #Vectorisation beaucoup plus simple
jeu_donnees_final$sexe[jeu_donnees_final$sexe %in% c("Femelle", "f")] <- "F"
jeu_donnees_final$sexe[jeu_donnees_final$sexe == "I"] <- NA
unique(jeu_donnees_final$sexe)

# Détection des individus non-sexés

na_sexe <- data.frame(id = character(), site = character())
for(i in 1:length(jeu_donnees_final$id)){
  if(is.na(jeu_donnees_final$sexe[i])){
    if(jeu_donnees_final$id[i] %in% unique(na_sexe$id)){
      next
    } else {
      na_sexe <- rbind(na_sexe, data.frame(id= jeu_donnees_final$id[i], site= jeu_donnees_final$site[i]))
    }
  }
}
na_sexe

jeu_donnees_final$sexe[jeu_donnees_final$id %in% c("FREJ8","L792","L434","L623","L226","LA25","E92","G035")] <- "F"
jeu_donnees_final$sexe[jeu_donnees_final$id %in% c("F011","E230","E378","L794")] <- "M"

    ## Mois ----

unique(jeu_donnees_final$mois)

    ## Expo ----
jeu_donnees_final$expo[jeu_donnees_final$expo == "1ARS"] <- "1"
jeu_donnees_final$expo[jeu_donnees_final$expo %in% c("TRUE","Immobilité")] <- NA
unique(jeu_donnees_final$expo)

    ## Cond_cach ----
jeu_donnees_final$Cond_cach[jeu_donnees_final$Cond_cach %in% c("entérrée partiellement", "Enterree partiellement", "enterrée partiellement", "Enterrée partiellement", "enterré partiellement", "enterree_partiellement")] <- "enterree partiellement"
jeu_donnees_final$Cond_cach[jeu_donnees_final$Cond_cach == "Invisible"] <- "invisible"
jeu_donnees_final$Cond_cach[jeu_donnees_final$Cond_cach %in% c("entérrée", "enterrée", "Enterrée")] <- "enterree"
jeu_donnees_final$Cond_cach[jeu_donnees_final$Cond_cach %in% c("Nichee", "Nichée", "nichée", "nichées", "Niché")] <- "nichee"
jeu_donnees_final$Cond_cach[jeu_donnees_final$Cond_cach %in% c("Découvert", "Decouvert")] <- "decouverte"
jeu_donnees_final$Cond_cach[jeu_donnees_final$Cond_cach %in% c("cachée", "Caché", "caché")] <- "cachee"
jeu_donnees_final$Cond_cach[jeu_donnees_final$Cond_cach %in% c("Abrité", "abrité")] <- "abritee"
unique(jeu_donnees_final$Cond_cach)

    ## Doublons ID ----

jeu_donnees_final$id[jeu_donnees_final$id == "e359"] <- "E359"
jeu_donnees_final$id[jeu_donnees_final$id == "e143"] <- "E143"
jeu_donnees_final$id[jeu_donnees_final$id == "e222"] <- "E222"
jeu_donnees_final$id[jeu_donnees_final$id == "e226"] <- "E226"
jeu_donnees_final$id[jeu_donnees_final$id == "e227"] <- "E227"
jeu_donnees_final$id[jeu_donnees_final$id == "e228"] <- "E228"
jeu_donnees_final$id[jeu_donnees_final$id == "e229"] <- "E229"
jeu_donnees_final$id[jeu_donnees_final$id == "e230"] <- "E230"
jeu_donnees_final$id[jeu_donnees_final$id == "e232"] <- "E232"
jeu_donnees_final$id[jeu_donnees_final$id == "e234"] <- "E234"
jeu_donnees_final$id[jeu_donnees_final$id == "e243"] <- "E243"
jeu_donnees_final$id[jeu_donnees_final$id == "e247"] <- "E247"
jeu_donnees_final$id[jeu_donnees_final$id == "e248"] <- "E248"
jeu_donnees_final$id[jeu_donnees_final$id == "e249"] <- "E249"
jeu_donnees_final$id[jeu_donnees_final$id == "e250"] <- "E250"
jeu_donnees_final$id[jeu_donnees_final$id == "e259"] <- "E259"
jeu_donnees_final$id[jeu_donnees_final$id == "g507"] <- "G507"
jeu_donnees_final$id[jeu_donnees_final$id == "0512"] <- "512"   
jeu_donnees_final$id[jeu_donnees_final$id == "g548"] <- "G548"
jeu_donnees_final$id[jeu_donnees_final$id == "g861"] <- "G861" 
jeu_donnees_final$id[jeu_donnees_final$id == "g915"] <- "G915"
jeu_donnees_final$id[jeu_donnees_final$id == "g135"] <- "G135"
jeu_donnees_final$id[jeu_donnees_final$id == "g490"] <- "G490"
jeu_donnees_final$id[jeu_donnees_final$id == "g010"] <- "G010"
jeu_donnees_final$id[jeu_donnees_final$id == "g987"] <- "G987"
jeu_donnees_final$id[jeu_donnees_final$id == "g901"] <- "G901"
jeu_donnees_final$id[jeu_donnees_final$id == "g113"] <- "G113"
jeu_donnees_final$id[jeu_donnees_final$id == "g126"] <- "G126"
jeu_donnees_final$id[jeu_donnees_final$id == "g674"] <- "G674"
jeu_donnees_final$id[jeu_donnees_final$id == "g035"] <- "G035"
jeu_donnees_final$id[jeu_donnees_final$id == "g536"] <- "G536"
jeu_donnees_final$id[jeu_donnees_final$id == "g531"] <- "G531"
jeu_donnees_final$id[jeu_donnees_final$id == "g539"] <- "G539"
jeu_donnees_final$id[jeu_donnees_final$id == "g910"] <- "G910"
jeu_donnees_final$id[jeu_donnees_final$id == "g922"] <- "G922" 
jeu_donnees_final$id[jeu_donnees_final$id == "g930"] <- "G930"
jeu_donnees_final$id[jeu_donnees_final$id == "g940"] <- "G940"
jeu_donnees_final$id[jeu_donnees_final$id == "g949"] <- "G949"
jeu_donnees_final$id[jeu_donnees_final$id == "g040"] <- "G040"
jeu_donnees_final$id[jeu_donnees_final$id == "g106"] <- "G106"
jeu_donnees_final$id[jeu_donnees_final$id == "g121"] <- "G121"
jeu_donnees_final$id[jeu_donnees_final$id == "h209"] <- "H209"   
jeu_donnees_final$id[jeu_donnees_final$id == "h500"] <- "H500"
jeu_donnees_final$id[jeu_donnees_final$id == "j765"] <- "J765"
jeu_donnees_final$id[jeu_donnees_final$id == "l415"] <- "L415"
jeu_donnees_final$id[jeu_donnees_final$id == "l795"] <- "L795"     
jeu_donnees_final$id[jeu_donnees_final$id == "l797"] <- "L797"
jeu_donnees_final$id[jeu_donnees_final$id == "l621"] <- "L621"
jeu_donnees_final$id[jeu_donnees_final$id == "l301"] <- "L301"
jeu_donnees_final$id[jeu_donnees_final$id == "l378"] <- "L378"
jeu_donnees_final$id[jeu_donnees_final$id == "l436"] <- "L436"
jeu_donnees_final$id[jeu_donnees_final$id == "l615"] <- "L615"     
jeu_donnees_final$id[jeu_donnees_final$id == "l618"] <- "L618"
jeu_donnees_final$id[jeu_donnees_final$id == "l767"] <- "L767"
jeu_donnees_final$id[jeu_donnees_final$id == "m13"] <- "M13"   
jeu_donnees_final$id[jeu_donnees_final$id == "m071"] <- "M071"
jeu_donnees_final$id[jeu_donnees_final$id == "Eso16"] <- "Eso 16"

length(unique(jeu_donnees_final$id))

  # Création de nouvelles variables ----

    ## Variable periode ----

jeu_donnees_final$annee <- as.numeric(jeu_donnees_final$annee)
jeu_donnees_final$mois <- as.numeric(jeu_donnees_final$mois)

jeu_donnees_final$periode <- NA
jeu_donnees_final$periode[jeu_donnees_final$mois <= 6] <- paste0(jeu_donnees_final$annee[jeu_donnees_final$mois <= 6]-1,"-",jeu_donnees_final$annee[jeu_donnees_final$mois <= 6])
jeu_donnees_final$periode[jeu_donnees_final$mois >=7] <- paste0(jeu_donnees_final$annee[jeu_donnees_final$mois >= 7],"-",jeu_donnees_final$annee[jeu_donnees_final$mois >= 7]+1)

    ## Variable zone ----

jeu_donnees_final$zone <- NA
hors_zone <- c()
for (i in 1:length(jeu_donnees_final$site)){
  if(jeu_donnees_final$site[i] == "Aurèdes" | jeu_donnees_final$site[i] == "Escarcets" | jeu_donnees_final$site[i] == "St Daumas" | jeu_donnees_final$site[i] == "Neuf Riaux" | jeu_donnees_final$site[i] == "Les mayons" | jeu_donnees_final$site[i] == "Rascas" | jeu_donnees_final$site[i] == "Plaine-Est"){
    jeu_donnees_final$zone[i] <- "plaine_des_maures"
  } else if(jeu_donnees_final$site[i] == "Garidelle" | jeu_donnees_final$site[i] == "Callas" | jeu_donnees_final$site[i] == "SOMECA"){
    jeu_donnees_final$zone[i] <- "callas"
  } else if(jeu_donnees_final$site[i] == "Lac Redon" | jeu_donnees_final$site[i] == "Rouvède"){
    jeu_donnees_final$zone[i] <- "lac_redon"
  } else if(jeu_donnees_final$site[i] == "Lambert"){
    jeu_donnees_final$zone[i] <- "lambert"
  } else {
    hors_zone <- c(hors_zone, i)
  }
}
if(length(hors_zone) > 0){
  jeu_donnees_final <- jeu_donnees_final[-hors_zone,]
}

# Détection indivdus présents sur plusieurs zones

zones_ind <- data.frame(ind = character(), zone = character())
ind_plusieurs_zones <- data.frame(ind = character(), zone = character())
for(i in 1:length(jeu_donnees_final$id)){
  if(jeu_donnees_final$id[i] %in% zones_ind$ind == F){
    zones_ind <- rbind(zones_ind, data.frame(ind = jeu_donnees_final$id[i], zone = jeu_donnees_final$zone[i]))
  } else {
    if(jeu_donnees_final$zone[i] %in% zones_ind$zone[zones_ind$ind == jeu_donnees_final$id[i]] == F){
      zones_ind <- rbind(zones_ind, data.frame(ind = jeu_donnees_final$id[i], zone = jeu_donnees_final$zone[i]))
      ind_plusieurs_zones <- rbind(ind_plusieurs_zones, data.frame(ind = jeu_donnees_final$id[i], zone = c(unique(zones_ind$zone[zones_ind$ind == jeu_donnees_final$id[i]]))))
    } else {
      next
    }
  }
}
ind_plusieurs_zones
#Pas d'individus présents sur 2 zones

    ## Variable j_julien ----

jeu_donnees_final$j_julien <- as.numeric(format(jeu_donnees_final$date, "%j"))
jeu_donnees_final <- jeu_donnees_final[!is.na(jeu_donnees_final$j_julien),]

    ## Amélioration variable activite ----

#for(i in 1:length(jeu_donnees_final$activite)){
#if(!is.na(jeu_donnees_final$Cond_cach[i]) &&
#(jeu_donnees_final$Cond_cach[i] == "invisible" | 
#jeu_donnees_final$sexe[i] == "f")){
#}
#}

  # Exportation jeu de données final ----

write_xlsx(jeu_donnees_final, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Jeu de données final/Jeu_de_données_final_V2.xlsx")

jeu_donnees_final_avec_mois_en_moins <- jeu_donnees_final[jeu_donnees_final$mois %in% c(1,2,3,4,10,11,12),]
write_xlsx(jeu_donnees_final_avec_mois_en_moins, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Jeu de données final/Jeu_de_données_final_avec_mois_en_moins.xlsx")

#####################################
#### Résumé stats jeu de données ####
#####################################
  # Effectif ----

    ## Nombre d'individus pointés ----

      ### Sur le jeu de données global ----

length(unique(jeu_donnees_final$id))

      ### Sur chaque sites ----

tableau_ind_site <- data.frame(site = unique(jeu_donnees_final$site))
tableau_ind_site$nb_individus <- NA
for(s in unique(tableau_ind_site$site)){
  data_site <- jeu_donnees_final[jeu_donnees_final$site == s,]
  individus <- length(unique(data_site$id))
  tableau_ind_site$nb_individus[tableau_ind_site$site == s] <- individus
}
tableau_ind_site

    ## Individus présents sur plusieurs sites ----

sites_ind <- data.frame(ind = character(), site = character())
ind_plusieurs_sites <- data.frame(ind = character(), site = character())
for(i in 1:length(jeu_donnees_final$id)){
  if(jeu_donnees_final$id[i] %in% sites_ind$ind == F){
    sites_ind <- rbind(sites_ind, data.frame(ind = jeu_donnees_final$id[i], site = jeu_donnees_final$site[i]))
  } else {
    if(jeu_donnees_final$site[i] %in% sites_ind$site[sites_ind$ind == jeu_donnees_final$id[i]] == F){
      sites_ind <- rbind(sites_ind, data.frame(ind = jeu_donnees_final$id[i], site = jeu_donnees_final$site[i]))
      ind_plusieurs_sites <- rbind(ind_plusieurs_sites, data.frame(ind = jeu_donnees_final$id[i], site = c(unique(sites_ind$site[sites_ind$ind == jeu_donnees_final$id[i]]))))
    } else {
      next
    }
  }
}
ind_plusieurs_sites

#########################
#### Rpzt° graphique ####
#########################
  # Histogramme simple ----

    ## Nombre d'individus par site ----

nombre_ind_site <- jeu_donnees_final %>% group_by(site) %>% summarise(n_ind = n_distinct(id))
nombre_ind_site
ggplot(nombre_ind_site, aes(x = site, y = n_ind)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Site", y = "Nombre d'individus uniques")

    ## Pointages par sites ----
ggplot(jeu_donnees_final, aes(x = site)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Pointages par site", x = "Site", y = "Pointages")

    ## Pointages par zone ----
ggplot(jeu_donnees_final, aes(x = zone)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Pointages par zone", x = "Zones", y = "Pointages")

    ## Pointages par période ----
ggplot(jeu_donnees_final, aes(x = periode)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Pointages par période", x = "Période", y = "Pointages")

    ## Pointages par mois ----
ggplot(jeu_donnees_final, aes(x = mois)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Pointages par mois", x = "Mois", y = "Pointages") +
  scale_x_continuous(breaks = seq(min(jeu_donnees_final$mois), max(jeu_donnees_final$mois), 1))

  # Période d'échantillonnage ----

    ## Pour chaque site ----

      ### Tableau ----

table(jeu_donnees_final$site,jeu_donnees_final$periode)

      ### Graphique ----

        #### Calcul du nombre de pointages par site ----

jeu_donnees_final$annee <- as.numeric(jeu_donnees_final$annee)
nbre_pointage_site_annee <- jeu_donnees_final %>% group_by(site, annee) %>% summarise(n = n(), .groups = "drop")

        #### Possibilité de filtrer le nombre de pointages conservés ----
filtre_20_pointage_site_annee <- nbre_pointage_site_annee %>% filter(n >= 1)
filtre_20_pointage_site_annee$color <- NA

        #### Mise en place d'un code couleur pour illustrer le nombre de pointages par année ----

for (i in 1:length(filtre_20_pointage_site_annee$n)){
  if (filtre_20_pointage_site_annee[i,"n"] <= 1000){
    filtre_20_pointage_site_annee[i,"color"] <- "red"
  }
  if (filtre_20_pointage_site_annee[i,"n"] <= 500){
    filtre_20_pointage_site_annee[i,"color"] <- "darkorange"
  }
  if (filtre_20_pointage_site_annee[i,"n"] <= 100){
    filtre_20_pointage_site_annee[i,"color"] <- "gold"
  }
  if (filtre_20_pointage_site_annee[i,"n"] > 1000){
    filtre_20_pointage_site_annee[i,"color"] <- "black"
  }
}

        #### Calcul et classement des périodes de pointages ----

periode_pointage_site <- filtre_20_pointage_site_annee %>%group_by(site) %>%summarise(debut_pointage = min(annee),fin_pointage = max(annee))
periode_pointage_site <- periode_pointage_site %>% arrange(debut_pointage)

filtre_20_pointage_site_annee$site <- factor(filtre_20_pointage_site_annee$site, levels = periode_pointage_site$site)
periode_pointage_site$site <- factor(periode_pointage_site$site, levels = periode_pointage_site$site)

        #### Représentation graphique ----

ggplot() + 
  geom_segment(data = periode_pointage_site, 
               aes(x = debut_pointage, xend = fin_pointage, y = site, yend = site),
               linewidth = 2.5, 
               colour = "grey85") +
  
  geom_point(data = filtre_20_pointage_site_annee,
             aes(x = annee, y = site),
             size = 2,
             colour = filtre_20_pointage_site_annee$color) +
  
  scale_x_continuous(breaks = seq(min(filtre_20_pointage_site_annee$annee), max(filtre_20_pointage_site_annee$annee), 1)) +
  
  labs(title = "Année d'échantillonage de chaque site", x = "Années", y = "Sites") +
  
  theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank())

    ## Pour chaque zone ----

      ### Graphique ----

        #### Calcul du nombre de pointages par zone ----

nbre_pointage_zone_annee <- jeu_donnees_final_sans_lambert %>% group_by(zone, annee) %>% summarise(n = n(), .groups = "drop")

        #### Possibilité de filtrer le nombre de pointages conservés ----

filtre_20_pointage_zone_annee <- nbre_pointage_zone_annee %>% filter(n >= 20)

        #### Mise en place d'un code couleur pour illustrer le nombre de pointages par année ----

filtre_20_pointage_zone_annee$color <- NA
for (i in 1:length(filtre_20_pointage_zone_annee$n)){
  if (filtre_20_pointage_zone_annee[i,"n"] <= 1000){
    filtre_20_pointage_zone_annee[i,"color"] <- "red"
  }
  if (filtre_20_pointage_zone_annee[i,"n"] <= 500){
    filtre_20_pointage_zone_annee[i,"color"] <- "darkorange"
  }
  if (filtre_20_pointage_zone_annee[i,"n"] <= 100){
    filtre_20_pointage_zone_annee[i,"color"] <- "gold"
  }
  if (filtre_20_pointage_zone_annee[i,"n"] > 1000){
    filtre_20_pointage_zone_annee[i,"color"] <- "black"
  }
}

        #### Calcul et classement des périodes de pointages ----

periode_pointage_zone <- filtre_20_pointage_zone_annee %>%group_by(zone) %>%summarise(debut_pointage = min(annee),fin_pointage = max(annee))
periode_pointage_zone <- periode_pointage_zone %>% arrange(debut_pointage)

filtre_20_pointage_zone_annee$zone <- factor(filtre_20_pointage_zone_annee$zone, levels = periode_pointage_zone$zone)
periode_pointage_zone$zone <- factor(periode_pointage_zone$zone, levels = periode_pointage_zone$zone)

        #### Représentation graphique ----

ggplot() + 
  geom_segment(data = periode_pointage_zone, 
               aes(x = debut_pointage, xend = fin_pointage, y = zone, yend = zone),
               linewidth = 2.5, 
               colour = "grey85") +
  
  geom_point(data = filtre_20_pointage_zone_annee,
             aes(x = annee, y = zone),
             size = 2,
             colour = filtre_20_pointage_zone_annee$color) +
  
  scale_x_continuous(breaks = seq(min(filtre_20_pointage_zone_annee$annee), max(filtre_20_pointage_zone_annee$annee), 1)) +
  
  labs(title = "Année d'échantillonage de chaque zone", x = "Années", y = "Zone") +
  
  theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank())


      ### Graphique sobre ----

        #### Calcul du nombre de pointages par zone ----

nbre_pointage_zone_annee <- jeu_donnees_final_sans_lambert %>% group_by(zone, annee) %>% summarise(n = n(), .groups = "drop")

        #### Possibilité de filtrer le nombre de pointages conservés ----

filtre_20_pointage_zone_annee <- nbre_pointage_zone_annee %>% filter(n >= 20)

        #### Calcul et classement des périodes de pointages ----

periode_pointage_zone <- filtre_20_pointage_zone_annee %>%group_by(zone) %>%summarise(debut_pointage = min(annee),fin_pointage = max(annee))
periode_pointage_zone <- periode_pointage_zone %>% arrange(debut_pointage)

filtre_20_pointage_zone_annee$zone <- factor(filtre_20_pointage_zone_annee$zone, levels = periode_pointage_zone$zone)
periode_pointage_zone$zone <- factor(periode_pointage_zone$zone, levels = periode_pointage_zone$zone)

      #### Représentation graphique ----

labels_colores <- c(
  "Plaine des Maures" = "<span style='color:red'>Plaine des Maures</span>",
  "Lac Redon" = "<span style='color:blue'>Lac Redon</span>",
  "Callas" = "<span style='color:#418E4D'>Callas</span>"
)


ggplot() + 
  geom_segment(data = periode_pointage_zone, 
               aes(x = debut_pointage, xend = fin_pointage, y = zone, yend = zone),
               linewidth = 2.5, 
               colour = "grey85") +
  
  geom_point(data = filtre_20_pointage_zone_annee,
             aes(x = annee, y = zone),
             size = 2,
             colour = filtre_20_pointage_zone_annee$color) +
  
  scale_x_continuous(
    breaks = seq(min(filtre_20_pointage_zone_annee$annee),
                 max(filtre_20_pointage_zone_annee$annee), 1)
  ) +
  
  scale_y_discrete(labels = labels_colores) +
  
  labs(x = "Années", y = "Zone") +
  
  theme_classic() +
  theme(
    axis.text.y = ggtext::element_markdown(size = 10),
    axis.text.x = element_text(
      size = 10,
      angle = 55,   # inclinaison
      hjust = 1     # alignement pour lisibilité
    ),
    axis.title = element_text(size = 12),
    panel.grid = element_blank()
  )


##################################
#### Tableau descriptif zones ####
##################################
## Zones ----

tableau_pres_zones <- data.frame(zone = character(), nb_individus = character(), nb_pointages = character(), nb_ind_sexe = numeric(), nb_male = numeric(), nb_femelle = numeric(), sex_ratio = numeric())

for(z in unique(jeu_donnees_final_sans_lambert$zone)){
  data_zone <- jeu_donnees_final_sans_lambert[jeu_donnees_final_sans_lambert$zone == z,]
  nb_ind_tot_zone <- length(unique(data_zone$id))
  pointages <- length(data_zone$id)
  data_zone <- data_zone[!is.na(data_zone$sexe),]
  sexe_ind <- data.frame(ind = character(), sexe = character())
  for(i in 1:length(data_zone$id)){
    if(data_zone$id[i] %in% sexe_ind$ind == F){
      sexe_ind <- rbind(sexe_ind, data.frame(ind = data_zone$id[i], sexe = data_zone$sexe[i]))
    } else {
      if(data_zone$sexe[i] %in% sexe_ind$sexe[sexe_ind$ind == data_zone$id[i]] == F){
        sexe_ind$sexe[sexe_ind$ind == data_zone$id[i]] <- "Données contradictoires"
      } else {
        next
      }
    }
  }
  nb_male <- 0
  nb_femelle <- 0
  bug <- c()
  for(j in 1:length(sexe_ind$ind)){
    if(sexe_ind$sexe[j] == "M"){
      nb_male <- nb_male + 1
    } else if (sexe_ind$sexe[j] == "F"){
      nb_femelle <- nb_femelle + 1
    } else{
      bug <- c(bug,sexe_ind$ind[j])
    }
  }
  ratio <- nb_male/nb_femelle
  ind_sexe <- nb_male+nb_femelle
  ind_na <- nb_ind_tot_zone - ind_sexe
  tableau_pres_zones <- rbind(tableau_pres_zones,data.frame(zone = z, nb_individus = nb_ind_tot_zone, nb_pointages = pointages, nb_ind_sexe = ind_sexe, nb_male = nb_male, nb_femelle = nb_femelle, sex_ratio = round(ratio,2)))
}
tableau_pres_zones

write_xlsx(tableau_pres_zones, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Tableau descriptif/Tableau présentation zones.xlsx")

############################################
#### Chargement du jeu de données final ####
############################################
  # Chargement ----

jeu_donnees_final <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Jeu de données final/Jeu_de_données_final_V2.xlsx")
jeu_donnees_final_sans_lambert <- jeu_donnees_final[jeu_donnees_final$zone %in% c("plaine_des_maures", "lac_redon", "callas"),]
jeu_donnees_plaine_des_maures <- jeu_donnees_final[jeu_donnees_final$zone == "plaine_des_maures",]
jeu_donnees_callas <- jeu_donnees_final[jeu_donnees_final$zone == "callas",]
jeu_donnees_lac_redon <- jeu_donnees_final[jeu_donnees_final$zone == "lac_redon",]
jeu_donnees_lambert <- jeu_donnees_final[jeu_donnees_final$zone == "lambert",]

######################################################
#### Cartographie et tableau descriptif des sites ####
######################################################
  # Création base de données pour réalisation de la carto ----

    ## Variables nécessaires à la carto ----

      ### Coordonnées ----

library(sf)
donnees_pour_carto <- data.frame(site = unique(jeu_donnees_final$site))
donnees_pour_carto$e <- NA
donnees_pour_carto$n <- NA
donnees_pour_carto$nb_pointages <- NA

for(site in unique(donnees_pour_carto$site)){
  data_site <- jeu_donnees_final[jeu_donnees_final$site == site,]
  n_median <- median(data_site$n)
  e_median <- median(data_site$e)
  donnees_pour_carto$n[donnees_pour_carto$site == site] <- n_median
  donnees_pour_carto$e[donnees_pour_carto$site == site] <- e_median
}

      ### Nombre de pointages ----
for(site in unique(donnees_pour_carto$site)){
  data_site <- jeu_donnees_final[jeu_donnees_final$site == site,]
  pointages <- length(data_site$id)
  donnees_pour_carto$nb_pointages[donnees_pour_carto$site == site] <- pointages
}

    ## Export ----

write.csv(donnees_pour_carto, "C:/Users/mathi/Documents/Cours/Master/Stage/Carto/Base de données/Données_carto_R_V1.csv")

  # Création tableau rendu final en 1 tableau ----

    ## Import des données ----

      ### Altitude ----
alti_sites <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/Carto/Base de données/Altitude_sites.xlsx")
alti_sites <- alti_sites[,-1]
colnames(alti_sites)[5] <- "Altitude"

      ### Données climatiques ----
climat_sites <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/Carto/Base de données/Données_climatiques_sites.xlsx")
climat_sites <- climat_sites[,-1]
nom_variables_temp <- data.frame(nom = rep(NA, 21))
nom_variables_temp$nom[1] <- "Pluie10"
nom_variables_temp$nom[2] <- "Pluie11"
nom_variables_temp$nom[3] <- "Pluie12"
nom_variables_temp$nom[4] <- "Pluie1"
nom_variables_temp$nom[5] <- "Pluie2"
nom_variables_temp$nom[6] <- "Pluie3"
nom_variables_temp$nom[7] <- "Pluie4"
nom_variables_temp$nom[8] <- "Soleil10"
nom_variables_temp$nom[9] <- "Soleil11"
nom_variables_temp$nom[10] <- "Soleil12"
nom_variables_temp$nom[11] <- "Soleil1"
nom_variables_temp$nom[12] <- "Soleil2"
nom_variables_temp$nom[13] <- "Soleil3"
nom_variables_temp$nom[14] <- "Soleil4"
nom_variables_temp$nom[15] <- "Temp10"
nom_variables_temp$nom[16] <- "Temp11"
nom_variables_temp$nom[17] <- "Temp12"
nom_variables_temp$nom[18] <- "Temp1"
nom_variables_temp$nom[19] <- "Temp2"
nom_variables_temp$nom[20] <- "Temp3"
nom_variables_temp$nom[21] <- "Temp4"
for(i in 5:ncol(climat_sites)){
  colnames(climat_sites)[i] <- nom_variables_temp$nom[i-4]
}

    ## Création de chaque variable du tableau ----

tableau_pres_sites <- data.frame(site=unique(climat_sites$site))

      ### Période de pointages ----
tableau_pres_sites$annees_pointages <- NA
for(s in unique(tableau_pres_sites$site)){
  data_site <- jeu_donnees_final[jeu_donnees_final$site == s,]
  date_debut_pointage <- min(data_site$annee)
  date_fin_pointage <- max(data_site$annee)
  if(date_debut_pointage != date_fin_pointage){
    tableau_pres_sites$annees_pointages[tableau_pres_sites$site == s] <- paste0(date_debut_pointage, "-", date_fin_pointage)
  } else {
    tableau_pres_sites$annees_pointages[tableau_pres_sites$site == s] <- date_debut_pointage
  }
}

      ### Nombre d'individus pointés ----
tableau_pres_sites$nb_individus <- NA
for(s in unique(tableau_pres_sites$site)){
  data_site <- jeu_donnees_final[jeu_donnees_final$site == s,]
  individus <- length(unique(data_site$id))
  tableau_pres_sites$nb_individus[tableau_pres_sites$site == s] <- individus
}

      ### Nombre de pointages ----
tableau_pres_sites$nb_pointages <- NA
for(s in unique(tableau_pres_sites$site)){
  data_site <- jeu_donnees_final[jeu_donnees_final$site == s,]
  pointages <- length(data_site$id)
  tableau_pres_sites$nb_pointages[tableau_pres_sites$site == s] <- pointages
}

write_xlsx(tableau_pres_sites, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Tableau descriptif/Tableau pointages par sites.xlsx")

      ### Coordonnées GPS du site ----
tableau_pres_sites$latitude <- NA
for(s in unique(tableau_pres_sites$site)){
  n <- climat_sites$n[climat_sites$site == s]
  tableau_pres_sites$latitude[tableau_pres_sites$site == s] <- n
}
tableau_pres_sites$longitude <- NA
for(s in unique(tableau_pres_sites$site)){
  e <- climat_sites$e[climat_sites$site == s]
  tableau_pres_sites$longitude[tableau_pres_sites$site == s] <- e
}
      ### Climat sur le site ----

        #### Température ----
tableau_pres_sites$temperature_moyenne_periode_entree <- NA
for(s in unique(tableau_pres_sites$site)){
  temp_entree_site <- c(climat_sites$Temp10[climat_sites$site == s],climat_sites$Temp11[climat_sites$site == s],climat_sites$Temp12[climat_sites$site == s])
  moyenne_temp_entree_site <- round(mean(temp_entree_site),2)
  tableau_pres_sites$temperature_moyenne_periode_entree[tableau_pres_sites$site == s] <- moyenne_temp_entree_site
}
tableau_pres_sites$temperature_moyenne_periode_sortie <- NA
for(s in unique(tableau_pres_sites$site)){
  temp_sortie_site <- c(climat_sites$Temp2[climat_sites$site == s],climat_sites$Temp3[climat_sites$site == s],climat_sites$Temp4[climat_sites$site == s])
  moyenne_temp_sortie_site <- round(mean(temp_sortie_site),2)
  tableau_pres_sites$temperature_moyenne_periode_sortie[tableau_pres_sites$site == s] <- moyenne_temp_sortie_site
}

        #### Radiation solaire ----
tableau_pres_sites$radiation_solaire_moyenne_periode_entree <- NA
for(s in unique(tableau_pres_sites$site)){
  soleil_entree_site <- c(climat_sites$Soleil10[climat_sites$site == s],climat_sites$Soleil11[climat_sites$site == s],climat_sites$Soleil12[climat_sites$site == s])
  moyenne_soleil_entree_site <- round(mean(soleil_entree_site),2)
  tableau_pres_sites$radiation_solaire_moyenne_periode_entree[tableau_pres_sites$site == s] <- moyenne_soleil_entree_site
}
tableau_pres_sites$radiation_solaire_moyenne_periode_sortie <- NA
for(s in unique(tableau_pres_sites$site)){
  soleil_sortie_site <- c(climat_sites$Soleil2[climat_sites$site == s],climat_sites$Soleil3[climat_sites$site == s],climat_sites$Soleil4[climat_sites$site == s])
  moyenne_soleil_sortie_site <- round(mean(soleil_sortie_site),2)
  tableau_pres_sites$radiation_solaire_moyenne_periode_sortie[tableau_pres_sites$site == s] <- moyenne_soleil_sortie_site
}

        #### Pluviométrie ----
tableau_pres_sites$pluiviometrie_moyenne_periode_entree <- NA
for(s in unique(tableau_pres_sites$site)){
  pluie_entree_site <- c(climat_sites$Pluie10[climat_sites$site == s],climat_sites$Pluie11[climat_sites$site == s],climat_sites$Pluie12[climat_sites$site == s])
  moyenne_pluie_entree_site <- round(mean(pluie_entree_site),2)
  tableau_pres_sites$pluiviometrie_moyenne_periode_entree[tableau_pres_sites$site == s] <- moyenne_pluie_entree_site
}
tableau_pres_sites$pluiviometrie_moyenne_periode_sortie <- NA
for(s in unique(tableau_pres_sites$site)){
  pluie_sortie_site <- c(climat_sites$Pluie2[climat_sites$site == s],climat_sites$Pluie3[climat_sites$site == s],climat_sites$Pluie4[climat_sites$site == s])
  moyenne_pluie_sortie_site <- round(mean(pluie_sortie_site),2)
  tableau_pres_sites$pluiviometrie_moyenne_periode_sortie[tableau_pres_sites$site == s] <- moyenne_pluie_sortie_site
}

      ### Altitude du site ----
tableau_pres_sites$altitude <- NA
for(s in unique(tableau_pres_sites$site)){
  altitude_site <- alti_sites$Altitude[climat_sites$site == s]
  tableau_pres_sites$altitude[tableau_pres_sites$site == s] <- altitude_site
}

tableau_pres_sites

  # Création tableau rendu final en 2 tableaux ----

    ## Import des données ----

      ### Altitude ----

alti_sites <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/Carto/Base de données/Altitude_sites.xlsx")
alti_sites <- alti_sites[,-1]
colnames(alti_sites)[5] <- "Altitude"

      ### Données climatiques ----

climat_sites <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/Carto/Base de données/Données_climatiques_sites_complet.xlsx")
climat_sites <- climat_sites[,-1]
nom_variables_temp <- data.frame(nom = rep(NA, 35))
nom_variables_temp$nom[1] <- "Hygro1"  
nom_variables_temp$nom[2] <- "Hygro2"  
nom_variables_temp$nom[3] <- "Hygro3"  
nom_variables_temp$nom[4] <- "Hygro4"  
nom_variables_temp$nom[5] <- "Hygro10"  
nom_variables_temp$nom[6] <- "Hygro11"  
nom_variables_temp$nom[7] <- "Hygro12"  
nom_variables_temp$nom[8] <- "Pluie1"  
nom_variables_temp$nom[9] <- "Pluie2"  
nom_variables_temp$nom[10] <- "Pluie3"  
nom_variables_temp$nom[11] <- "Pluie4"  
nom_variables_temp$nom[12] <- "Pluie10"  
nom_variables_temp$nom[13] <- "Pluie11"  
nom_variables_temp$nom[14] <- "Pluie12"  
nom_variables_temp$nom[15] <- "Soleil1"  
nom_variables_temp$nom[16] <- "Soleil2"  
nom_variables_temp$nom[17] <- "Soleil3"  
nom_variables_temp$nom[18] <- "Soleil4"  
nom_variables_temp$nom[19] <- "Soleil10"  
nom_variables_temp$nom[20] <- "Soleil11"  
nom_variables_temp$nom[21] <- "Soleil12"  
nom_variables_temp$nom[22] <- "Temp1"
nom_variables_temp$nom[23] <- "Temp2"
nom_variables_temp$nom[24] <- "Temp3"
nom_variables_temp$nom[25] <- "Temp4"
nom_variables_temp$nom[26] <- "Temp10"
nom_variables_temp$nom[27] <- "Temp11"
nom_variables_temp$nom[28] <- "Temp12"
nom_variables_temp$nom[29] <- "Vent1"
nom_variables_temp$nom[30] <- "Vent2"
nom_variables_temp$nom[31] <- "Vent3"
nom_variables_temp$nom[32] <- "Vent4"
nom_variables_temp$nom[33] <- "Vent10"
nom_variables_temp$nom[34] <- "Vent11"
nom_variables_temp$nom[35] <- "Vent12"

for(i in 5:ncol(climat_sites)){
  colnames(climat_sites)[i] <- nom_variables_temp$nom[i-4]
}

    ## Création de chaque variable du tableau effectifs par sites ----

      ### Sites et années de pointages ----

tableau_pres_sites <- data.frame(site=character(), annee=numeric())

for(s in unique(climat_sites$site)){
  data_site <- jeu_donnees_final[jeu_donnees_final$site == s,]
  annee_pointage <- c(unique(data_site$annee))
  annee_pointage <- annee_pointage[order(annee_pointage)]
  for(annee in annee_pointage){
    tableau_pres_sites <- rbind(tableau_pres_sites, data.frame(site=s, annee=annee))
  }
}

      ### Nombre d'individus pointés ----

tableau_pres_sites$nb_individus <- NA
for(i in 1:length(tableau_pres_sites$site)){
  s <- tableau_pres_sites$site[i]
  data_site <- jeu_donnees_final[jeu_donnees_final$site == s,]
  annee <- tableau_pres_sites$annee[i]
  data_annee <- data_site[data_site$annee == annee,]
  individus <- length(unique(data_annee$id))
  tableau_pres_sites$nb_individus[i] <- individus
}

      ### Nombre de pointages ----

tableau_pres_sites$nb_pointages <- NA
for(i in 1:length(tableau_pres_sites$site)){
  s <- tableau_pres_sites$site[i]
  data_site <- jeu_donnees_final[jeu_donnees_final$site == s,]
  annee <- tableau_pres_sites$annee[i]
  data_annee <- data_site[data_site$annee == annee,]
  pointages <- length(data_annee$id)
  tableau_pres_sites$nb_pointages[i] <- pointages
}

    ## Création de chaque variable du tableau description des sites ----

      ### Sites ----

tableau_pres_sites_2 <- data.frame(site=unique(climat_sites$site))

      ### Coordonnées GPS ----

tableau_pres_sites_2$latitude <- NA
for(s in unique(tableau_pres_sites_2$site)){
  n <- climat_sites$n[climat_sites$site == s]
  tableau_pres_sites_2$latitude[tableau_pres_sites_2$site == s] <- n
}
tableau_pres_sites_2$longitude <- NA
for(s in unique(tableau_pres_sites_2$site)){
  e <- climat_sites$e[climat_sites$site == s]
  tableau_pres_sites_2$longitude[tableau_pres_sites_2$site == s] <- e
}
      ### Climat sur le site ----

        #### Température ----

tableau_pres_sites_2$temperature_moyenne_periode_entree <- NA
for(s in unique(tableau_pres_sites_2$site)){
  temp_entree_site <- c(climat_sites$Temp10[climat_sites$site == s],climat_sites$Temp11[climat_sites$site == s],climat_sites$Temp12[climat_sites$site == s])
  moyenne_temp_entree_site <- round(mean(temp_entree_site),2)
  tableau_pres_sites_2$temperature_moyenne_periode_entree[tableau_pres_sites_2$site == s] <- moyenne_temp_entree_site
}
tableau_pres_sites_2$temperature_moyenne_periode_sortie <- NA
for(s in unique(tableau_pres_sites_2$site)){
  temp_sortie_site <- c(climat_sites$Temp2[climat_sites$site == s],climat_sites$Temp3[climat_sites$site == s],climat_sites$Temp4[climat_sites$site == s])
  moyenne_temp_sortie_site <- round(mean(temp_sortie_site),2)
  tableau_pres_sites_2$temperature_moyenne_periode_sortie[tableau_pres_sites_2$site == s] <- moyenne_temp_sortie_site
}

        #### Radiation solaire ----

tableau_pres_sites_2$radiation_solaire_moyenne_periode_entree <- NA
for(s in unique(tableau_pres_sites_2$site)){
  soleil_entree_site <- c(climat_sites$Soleil10[climat_sites$site == s],climat_sites$Soleil11[climat_sites$site == s],climat_sites$Soleil12[climat_sites$site == s])
  moyenne_soleil_entree_site <- round(mean(soleil_entree_site),2)
  tableau_pres_sites_2$radiation_solaire_moyenne_periode_entree[tableau_pres_sites_2$site == s] <- moyenne_soleil_entree_site
}
tableau_pres_sites_2$radiation_solaire_moyenne_periode_sortie <- NA
for(s in unique(tableau_pres_sites_2$site)){
  soleil_sortie_site <- c(climat_sites$Soleil2[climat_sites$site == s],climat_sites$Soleil3[climat_sites$site == s],climat_sites$Soleil4[climat_sites$site == s])
  moyenne_soleil_sortie_site <- round(mean(soleil_sortie_site),2)
  tableau_pres_sites_2$radiation_solaire_moyenne_periode_sortie[tableau_pres_sites_2$site == s] <- moyenne_soleil_sortie_site
}

        #### Pluviométrie ----

tableau_pres_sites_2$pluiviometrie_moyenne_periode_entree <- NA
for(s in unique(tableau_pres_sites_2$site)){
  pluie_entree_site <- c(climat_sites$Pluie10[climat_sites$site == s],climat_sites$Pluie11[climat_sites$site == s],climat_sites$Pluie12[climat_sites$site == s])
  moyenne_pluie_entree_site <- round(mean(pluie_entree_site),2)
  tableau_pres_sites_2$pluiviometrie_moyenne_periode_entree[tableau_pres_sites_2$site == s] <- moyenne_pluie_entree_site
}
tableau_pres_sites_2$pluiviometrie_moyenne_periode_sortie <- NA
for(s in unique(tableau_pres_sites_2$site)){
  pluie_sortie_site <- c(climat_sites$Pluie2[climat_sites$site == s],climat_sites$Pluie3[climat_sites$site == s],climat_sites$Pluie4[climat_sites$site == s])
  moyenne_pluie_sortie_site <- round(mean(pluie_sortie_site),2)
  tableau_pres_sites_2$pluiviometrie_moyenne_periode_sortie[tableau_pres_sites_2$site == s] <- moyenne_pluie_sortie_site
}

        #### Hygrométrie ----

tableau_pres_sites_2$hygro_moyenne_periode_entree <- NA
for(s in unique(tableau_pres_sites_2$site)){
  hygro_entree_site <- c(climat_sites$Hygro10[climat_sites$site == s],climat_sites$Hygro11[climat_sites$site == s],climat_sites$Hygro12[climat_sites$site == s])
  moyenne_hygro_entree_site <- round(mean(hygro_entree_site),2)
  tableau_pres_sites_2$hygro_moyenne_periode_entree[tableau_pres_sites_2$site == s] <- moyenne_hygro_entree_site
}
tableau_pres_sites_2$hygro_moyenne_periode_sortie <- NA
for(s in unique(tableau_pres_sites_2$site)){
  hygro_sortie_site <- c(climat_sites$Hygro2[climat_sites$site == s],climat_sites$Hygro3[climat_sites$site == s],climat_sites$Hygro4[climat_sites$site == s])
  moyenne_hygro_sortie_site <- round(mean(hygro_sortie_site),2)
  tableau_pres_sites_2$hygro_moyenne_periode_sortie[tableau_pres_sites_2$site == s] <- moyenne_hygro_sortie_site
}

        #### Vent ----

tableau_pres_sites_2$vent_moyenne_periode_entree <- NA
for(s in unique(tableau_pres_sites_2$site)){
  vent_entree_site <- c(climat_sites$Vent10[climat_sites$site == s],climat_sites$Vent11[climat_sites$site == s],climat_sites$Vent12[climat_sites$site == s])
  moyenne_vent_entree_site <- round(mean(vent_entree_site),2)
  tableau_pres_sites_2$vent_moyenne_periode_entree[tableau_pres_sites_2$site == s] <- moyenne_vent_entree_site
}
tableau_pres_sites_2$vent_moyenne_periode_sortie <- NA
for(s in unique(tableau_pres_sites_2$site)){
  vent_sortie_site <- c(climat_sites$Vent2[climat_sites$site == s],climat_sites$Vent3[climat_sites$site == s],climat_sites$Vent4[climat_sites$site == s])
  moyenne_vent_sortie_site <- round(mean(vent_sortie_site),2)
  tableau_pres_sites_2$vent_moyenne_periode_sortie[tableau_pres_sites_2$site == s] <- moyenne_vent_sortie_site
}

      ### Altitude du site ----

tableau_pres_sites_2$altitude <- NA
for(s in unique(tableau_pres_sites_2$site)){
  altitude_site <- alti_sites$Altitude[climat_sites$site == s]
  tableau_pres_sites_2$altitude[tableau_pres_sites_2$site == s] <- altitude_site
}

write_xlsx(tableau_pres_sites_2, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Différenciation sites/Tableau données ACP.xlsx")

  # ACP ----

variable_acp <- c("site", "altitude",
                  "temperature_moyenne_periode_entree", "temperature_moyenne_periode_sortie", 
                  "radiation_solaire_moyenne_periode_entree", "radiation_solaire_moyenne_periode_sortie",
                  "pluiviometrie_moyenne_periode_entree", "pluiviometrie_moyenne_periode_sortie",
                  "hygro_moyenne_periode_entree", "hygro_moyenne_periode_sortie",
                  "vent_moyenne_periode_entree", "vent_moyenne_periode_sortie")    

data_acp <- tableau_pres_sites_2[,variable_acp]

library(FactoMineR)
rownames(data_acp) <- data_acp[,1]
data_acp <- data_acp[,-1]
data_acp <- data_acp[-13,]

colnames(data_acp)[colnames(data_acp) == "temperature_moyenne_periode_entree"] <- "T°_entree"
colnames(data_acp)[colnames(data_acp) == "temperature_moyenne_periode_sortie"] <- "T_sortie"
colnames(data_acp)[colnames(data_acp) == "radiation_solaire_moyenne_periode_entree"] <- "Radsol_entree"
colnames(data_acp)[colnames(data_acp) == "radiation_solaire_moyenne_periode_sortie"] <- "Radsol_sortie"
colnames(data_acp)[colnames(data_acp) == "pluiviometrie_moyenne_periode_entree"] <- "Pluie_entree"
colnames(data_acp)[colnames(data_acp) == "pluiviometrie_moyenne_periode_sortie"] <- "Pluie_sortie"
colnames(data_acp)[colnames(data_acp) == "hygro_moyenne_periode_entree"] <- "Hygro_entree"
colnames(data_acp)[colnames(data_acp) == "hygro_moyenne_periode_sortie"] <- "Hygro_sortie"
colnames(data_acp)[colnames(data_acp) == "vent_moyenne_periode_entree"] <- "Vent_entree"
colnames(data_acp)[colnames(data_acp) == "vent_moyenne_periode_sortie"] <- "Vent_sortie"
PCA(data_acp)

unique(jeu_donnees_lambert$mois)

##############
#### Sexe ####
##############
  # Sex-ratio ----

    ## Sites ----

tableau_sex_ratio <- data.frame(site = character(), male = numeric(), femelle = numeric(), sex_ratio = numeric(), nb_ind_sexe = numeric() ,nb_ind_na = numeric())

for(s in unique(jeu_donnees_final$site)){
  data_site <- jeu_donnees_final[jeu_donnees_final$site == s,]
  nb_ind_tot_site <- length(unique(data_site$id))
  data_site <- data_site[!is.na(data_site$sexe),]
  sexe_ind <- data.frame(ind = character(), sexe = character())
  for(i in 1:length(data_site$id)){
    if(data_site$id[i] %in% sexe_ind$ind == F){
      sexe_ind <- rbind(sexe_ind, data.frame(ind = data_site$id[i], sexe = data_site$sexe[i]))
    } else {
      if(data_site$sexe[i] %in% sexe_ind$sexe[sexe_ind$ind == data_site$id[i]] == F){
        sexe_ind$sexe[sexe_ind$ind == data_site$id[i]] <- "Données contradictoires"
      } else {
        next
      }
    }
  }
  nb_male <- 0
  nb_femelle <- 0
  bug <- c()
  for(j in 1:length(sexe_ind$ind)){
    if(sexe_ind$sexe[j] == "M"){
      nb_male <- nb_male + 1
    } else if (sexe_ind$sexe[j] == "F"){
      nb_femelle <- nb_femelle + 1
    } else{
      bug <- c(bug,sexe_ind$ind[j])
    }
  }
  ratio <- nb_male/nb_femelle
  ind_sexe <- nb_male+nb_femelle
  ind_na <- nb_ind_tot_site - ind_sexe
  tableau_sex_ratio <- rbind(tableau_sex_ratio,data.frame(site = s, male = nb_male, femelle = nb_femelle, sex_ratio = ratio, nb_ind_sexe = ind_sexe, nb_ind_na = ind_na))
}
tableau_sex_ratio

    ## Zones ----

tableau_sex_ratio_zone <- data.frame(zone = character(), male = numeric(), femelle = numeric(), sex_ratio = numeric(), nb_ind_sexe = numeric() ,nb_ind_na = numeric())

for(z in unique(jeu_donnees_final$zone)){
  data_zone <- jeu_donnees_final[jeu_donnees_final$zone == z,]
  nb_ind_tot_zone <- length(unique(data_zone$id))
  data_zone <- data_zone[!is.na(data_zone$sexe),]
  sexe_ind <- data.frame(ind = character(), sexe = character())
  for(i in 1:length(data_zone$id)){
    if(data_zone$id[i] %in% sexe_ind$ind == F){
      sexe_ind <- rbind(sexe_ind, data.frame(ind = data_zone$id[i], sexe = data_zone$sexe[i]))
    } else {
      if(data_zone$sexe[i] %in% sexe_ind$sexe[sexe_ind$ind == data_zone$id[i]] == F){
        sexe_ind$sexe[sexe_ind$ind == data_zone$id[i]] <- "Données contradictoires"
      } else {
        next
      }
    }
  }
  nb_male <- 0
  nb_femelle <- 0
  bug <- c()
  for(j in 1:length(sexe_ind$ind)){
    if(sexe_ind$sexe[j] == "M"){
      nb_male <- nb_male + 1
    } else if (sexe_ind$sexe[j] == "F"){
      nb_femelle <- nb_femelle + 1
    } else{
      bug <- c(bug,sexe_ind$ind[j])
    }
  }
  ratio <- nb_male/nb_femelle
  ind_sexe <- nb_male+nb_femelle
  ind_na <- nb_ind_tot_zone - ind_sexe
  tableau_sex_ratio_zone <- rbind(tableau_sex_ratio_zone,data.frame(zone = z, male = nb_male, femelle = nb_femelle, sex_ratio = ratio, nb_ind_sexe = ind_sexe, nb_ind_na = ind_na))
}
tableau_sex_ratio_zone
########################################
#### Comparaison distance Haversine ####
########################################
  # Chargement library ----

library(geosphere)

  # Entrée en hibernation ----

    ## Création du sous-jeu de données ----

# Sous-jeu de données incluant seulement oct-déc
jeu_donnees_final_entree <- subset(jeu_donnees_final, mois %in% c(10,11,12))
# Contrôle qualité
unique(jeu_donnees_final_entree$mois) #Test si les autres mois ont bien été supprimés

# Passage des coordonnées au format numérique pour réaliser les calculs
jeu_donnees_final_entree$n <- as.numeric(jeu_donnees_final_entree$n)
jeu_donnees_final_entree$e <- as.numeric(jeu_donnees_final_entree$e)


    ## Script pour afficher la date d'entrée en hibernation de chaque individu ----

# Seuil de différence entre coordonnées
seuil <- 5

# Création des variable résultats vides
resultats_entree_hibernation <- data.frame(zone = character(),
                                           individu = character(),
                                           periode = character(),
                                           date_debut = as.Date(character()))
donnees_insuffisantes <- data.frame(zone = character(),
                                    individu = character(),
                                    periode = character())

# Estimation de la date d'entrée en hibernation
# Boucle appliquée à chaque zone
for(s in unique(jeu_donnees_final_entree$zone)){
  # Création d'un sous-jeu de données contenant uniquement les informations de ce zone
  data_zone <- jeu_donnees_final_entree[jeu_donnees_final_entree$zone == s, ]
  
  # Boucle appliquée à chaque individu
  for(ind in unique(data_zone$id)){
    # Création d'un sous-jeu de données contenant uniquement les informations de cet individu
    data_ind <- data_zone[data_zone$id == ind, ]
    
    #Boucle appliquée pour chaque période
    for(per in unique(jeu_donnees_final_entree$periode)){
      # Création d'un sous-jeu de données contenant uniquement les informations de cet individu à cette période
      data_ind_per <- data_ind[data_ind$periode == per, ]
      # Classement des dates par ordre croissant
      # Permet la comparaison entre dates successives
      data_ind_per <- data_ind_per[order(data_ind_per$j_julien), ]
      
      n <- nrow(data_ind_per) # Variable comptant le nombre de pointages (lignes) dans le sous-jeu de données
      if(n == 0){ # Vérification que l'individu a été pointé durant cette période
        resultats_entree_hibernation <- rbind(resultats_entree_hibernation, 
                                              data.frame(zone = s,
                                                         individu = ind,
                                                         periode = per, 
                                                         date_debut = "Pas pointée"))
        next
      } else if(n < 3){ # Vérification de l'existence d'au moins 3 points
        donnees_insuffisantes <- rbind(donnees_insuffisantes, 
                                       data.frame(zone = s,
                                                  individu = ind,
                                                  periode = per))
        resultats_entree_hibernation <- rbind(resultats_entree_hibernation, 
                                              data.frame(zone = s,
                                                         individu = ind,
                                                         periode = per, 
                                                         date_debut = "NA"))
        next
      } else { 
        for(i in 1:(n-2)){ # 2ème boucle vérifiant les différences entre les coordonnées
          dist1 <- distHaversine(c(data_ind_per$e[i], data_ind_per$n[i]), c(data_ind_per$e[i+1], data_ind_per$n[i+1]))
          dist2 <- distHaversine(c(data_ind_per$e[i], data_ind_per$n[i]), c(data_ind_per$e[i+2], data_ind_per$n[i+2]))
          
          dist <- max(dist1, dist2) # Distance max le point 1 et l'un des 2 points du triplets
          
          if(dist <= seuil){ # Test du respect des seuils
            resultats_entree_hibernation <- rbind(resultats_entree_hibernation, 
                                                  data.frame(zone = s,
                                                             individu = ind,
                                                             periode = per,
                                                             date_debut = data_ind$j_julien[i])) # enregistrement de la première date du triplet dans le fichier résultats
            break # arrêt de la seconde boucle afin de garder uniquement la première occurrence
          }
        }
        if(dist > seuil){
          resultats_entree_hibernation <- rbind(resultats_entree_hibernation, 
                                                data.frame(zone = s,
                                                           individu = ind,
                                                           periode = per,
                                                           date_debut = "ND"))
        }  
      }
    }
  }
}


    ## Résultats ----

      ### Brut ----

# Classement des résultats pour meilleur affichage
resultats_entree_hibernation <- resultats_entree_hibernation[order(resultats_entree_hibernation$periode), ]
resultats_entree_hibernation <- resultats_entree_hibernation[order(resultats_entree_hibernation$individu), ]
resultats_entree_hibernation <- resultats_entree_hibernation[order(resultats_entree_hibernation$zone), ]

# Affichage
resultats_entree_hibernation
donnees_insuffisantes

      ### Moyenne par zone ----

        #### Script ----

# Création variable résultat vide
resultats_moyenne_entree_hibernation <- data.frame(zone = character(),
                                                   periode = character(),
                                                   date_entree_moyenne = as.Date(character()))
# Boucle
for(s in unique(resultats_entree_hibernation$zone)){
  data_zone_entree_moyenne <- resultats_entree_hibernation[resultats_entree_hibernation$zone == s,]
  data_zone_entree_moyenne$date_debut <- as.numeric(data_zone_entree_moyenne$date_debut)
  for(per in unique(resultats_entree_hibernation$periode)){
    data_zone_per_entree_moyenne <- data_zone_entree_moyenne[data_zone_entree_moyenne$periode == per,]
    date_entree_moyenne <- mean(data_zone_per_entree_moyenne$date_debut,na.rm = TRUE)
    resultats_moyenne_entree_hibernation <- rbind(resultats_moyenne_entree_hibernation,
                                                  data.frame(zone = s,
                                                             periode = per,
                                                             date_entree_moyenne = date_entree_moyenne))
  }
}  
        #### Tableau ----

resultats_moyenne_entree_hibernation

      ### 95% des individus ----

        #### Script ----

# Création variable résultat vide
resultats_95_entree_hibernation <- data.frame(zone = character(),
                                              periode = character(),
                                              date_entree_95 = as.Date(character()))
# Boucle
for(s in unique(resultats_entree_hibernation$zone)){
  data_zone_entree_95 <- resultats_entree_hibernation[resultats_entree_hibernation$zone == s,]
  data_zone_entree_95$date_debut <- as.numeric(data_zone_entree_95$date_debut)
  data_zone_entree_95 <- data_zone_entree_95[order(data_zone_entree_95$date_debut),]
  for(per in unique(resultats_entree_hibernation$periode)){
    data_zone_per_entree_95 <- data_zone_entree_95[data_zone_entree_95$periode == per,]
    date_entree_95 <- data_zone_per_entree_95[!is.na(data_zone_per_entree_95$date_debut),]
    if(length(date_entree_95$date_debut) == 0){
      resultats_95_entree_hibernation <- rbind(resultats_95_entree_hibernation,
                                               data.frame(zone = s,
                                                          periode = per,
                                                          date_entree_95 = NA))
    } else {
      for (j in 1:length(date_entree_95$date_debut)){
        if(j/length(date_entree_95$date_debut) >= 0.95){
          resultats_95_entree_hibernation <- rbind(resultats_95_entree_hibernation,
                                                   data.frame(zone = s,
                                                              periode = per,
                                                              date_entree_95 = date_entree_95[j,"date_debut"]))
          break
        }
      }
    }
  }
}

        #### Tableau ----

resultats_95_entree_hibernation

    ## Exportation des résultats ----

library(writexl)
write_xlsx(resultats_entree_hibernation, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Comparaison distance Haversine points successifs/Entrée hibernation/Tableau_zone_individu_periode_V2activite.xlsx")
write_xlsx(resultats_moyenne_entree_hibernation, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Comparaison distance Haversine points successifs/Entrée hibernation/Tableau_zone_moyenne_periode_V2activite.xlsx")
write_xlsx(resultats_95_entree_hibernation, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Comparaison distance Haversine points successifs/Entrée hibernation/Tableau_zone_95_periode_V2activite.xlsx")

  # Sortie d'hibernation ----

    ## Création du sous-jeu de données ----

# Sous-jeu de données incluant seulement janv-avril
jeu_donnees_final_sortie <- subset(jeu_donnees_final, mois %in% c(1,2,3,4))
# Contrôle qualité
unique(jeu_donnees_final_sortie$mois) #Test si les autres mois ont bien été supprimés

# Passage des coordonnées au format numérique pour réaliser les calculs
jeu_donnees_final_sortie$n <- as.numeric(jeu_donnees_final_sortie$n)
jeu_donnees_final_sortie$e <- as.numeric(jeu_donnees_final_sortie$e)
jeu_donnees_final_sortie <- jeu_donnees_final_sortie[!is.na(jeu_donnees_final_sortie$n), ]
jeu_donnees_final_sortie <- jeu_donnees_final_sortie[!is.na(jeu_donnees_final_sortie$e), ]

    ## Script pour afficher la date de sortie d'hibernation de chaque individu ----

# Seuil de différence entre coordonnées
seuil <- 5

# Création des variable résultats vides
resultats_sortie_hibernation <- data.frame(zone = character(),
                                           individu = character(),
                                           periode = character(),
                                           date_fin = as.Date(character()))
donnees_insuffisantes <- data.frame(zone = character(),
                                    individu = character(),
                                    periode = character())

# Estimation de la date d'entrée en hibernation
# Boucle appliquée à chaque zone
for(s in unique(jeu_donnees_final_sortie$zone)){
  # Création d'un sous-jeu de données contenant uniquement les informations de ce zone
  data_zone <- jeu_donnees_final_sortie[jeu_donnees_final_sortie$zone == s, ]
  
  # Boucle appliquée à chaque individu
  for(ind in unique(data_zone$id)){
    # Création d'un sous-jeu de données contenant uniquement les informations de cet individu
    data_ind <- data_zone[data_zone$id == ind, ]
    
    #Boucle appliquée pour chaque période
    for(per in unique(jeu_donnees_final_sortie$periode)){
      # Création d'un sous-jeu de données contenant uniquement les informations de cet individu à cette période
      data_ind_per <- data_ind[data_ind$periode == per, ]
      # Classement des dates par ordre croissant
      # Permet la comparaison entre dates successives
      data_ind_per <- data_ind_per[order(data_ind_per$j_julien), ]
      
      n <- nrow(data_ind_per) # Variable comptant le nombre de pointages (lignes) dans le sous-jeu de données
      if(n == 0){ # Vérification que l'individu a été pointé durant cette période
        resultats_sortie_hibernation <- rbind(resultats_sortie_hibernation, 
                                              data.frame(zone = s,
                                                         individu = ind,
                                                         periode = per, 
                                                         date_fin = "Pas pointée"))
        next
      } else if(n >= 1 & n < 3){ # Vérification de l'existence d'au moins 3 points
        donnees_insuffisantes <- rbind(donnees_insuffisantes, 
                                       data.frame(zone = s,
                                                  individu = ind,
                                                  periode = per))
        resultats_sortie_hibernation <- rbind(resultats_sortie_hibernation, 
                                              data.frame(zone = s,
                                                         individu = ind,
                                                         periode = per, 
                                                         date_fin = "NA"))
        next
      } else { 
        for(i in 2:n){ # 2ème boucle vérifiant les différences entre les coordonnées
          dist <- distHaversine(c(data_ind_per$e[i-1], data_ind_per$n[i-1]), c(data_ind_per$e[i], data_ind_per$n[i]))
          
          if(dist > seuil){ # Test du dépassement
            resultats_sortie_hibernation <- rbind(resultats_sortie_hibernation, 
                                                  data.frame(zone = s,
                                                             individu = ind,
                                                             periode = per,
                                                             date_fin = data_ind$j_julien[i])) # enregistrement de la première date du triplet dans le fichier résultats
            break # arrêt de la seconde boucle afin de garder uniquement la première occurrence
          }
        }
        if(dist < seuil){
          resultats_sortie_hibernation <- rbind(resultats_sortie_hibernation, 
                                                data.frame(zone = s,
                                                           individu = ind,
                                                           periode = per,
                                                           date_fin = "ND"))
        }  
      }
    }
  }
}


    ## Résultats ----

      ### Brut ----

# Classement des résultats pour meilleur affichage
resultats_sortie_hibernation <- resultats_sortie_hibernation[order(resultats_sortie_hibernation$periode), ]
resultats_sortie_hibernation <- resultats_sortie_hibernation[order(resultats_sortie_hibernation$individu), ]
resultats_sortie_hibernation <- resultats_sortie_hibernation[order(resultats_sortie_hibernation$zone), ]

# Affichage
resultats_sortie_hibernation
donnees_insuffisantes

      ### Moyenne par zone ----

        #### Script ----
# Création variable résultat vide
resultats_moyenne_sortie_hibernation <- data.frame(zone = character(),
                                                   periode = character(),
                                                   date_sortie_moyenne = as.Date(character()))
# Boucle
for(s in unique(resultats_sortie_hibernation$zone)){
  data_zone_sortie_moyenne <- resultats_sortie_hibernation[resultats_sortie_hibernation$zone == s,]
  data_zone_sortie_moyenne$date_debut <- as.numeric(data_zone_sortie_moyenne$date_debut)
  for(per in unique(resultats_sortie_hibernation$periode)){
    data_zone_per_sortie_moyenne <- data_zone_sortie_moyenne[data_zone_sortie_moyenne$periode == per,]
    date_sortie_moyenne <- mean(data_zone_per_sortie_moyenne$date_debut,na.rm = TRUE)
    resultats_moyenne_sortie_hibernation <- rbind(resultats_moyenne_sortie_hibernation,
                                                  data.frame(zone = s,
                                                             periode = per,
                                                             date_sortie_moyenne = date_sortie_moyenne))
  }
}  
        #### Tableau ----
resultats_moyenne_sortie_hibernation

      ### 95% des individus ----

        #### Script ----

# Création variable résultat vide
resultats_95_sortie_hibernation <- data.frame(zone = character(),
                                              periode = character(),
                                              date_sortie_95 = as.Date(character()))
# Boucle
for(s in unique(resultats_sortie_hibernation$zone)){
  data_zone_sortie_95 <- resultats_sortie_hibernation[resultats_sortie_hibernation$zone == s,]
  data_zone_sortie_95$date_debut <- as.numeric(data_zone_sortie_95$date_debut)
  data_zone_sortie_95 <- data_zone_sortie_95[order(data_zone_sortie_95$date_debut),]
  for(per in unique(resultats_sortie_hibernation$periode)){
    data_zone_per_sortie_95 <- data_zone_sortie_95[data_zone_sortie_95$periode == per,]
    date_sortie_95 <- data_zone_per_sortie_95[!is.na(data_zone_per_sortie_95$date_debut),]
    if(length(date_sortie_95$date_debut) == 0){
      resultats_95_sortie_hibernation <- rbind(resultats_95_sortie_hibernation,
                                               data.frame(zone = s,
                                                          periode = per,
                                                          date_sortie_95 = NA))
    } else {
      for (j in 1:length(date_sortie_95$date_debut)){
        if(j/length(date_sortie_95$date_debut) >= 0.95){
          resultats_95_sortie_hibernation <- rbind(resultats_95_sortie_hibernation,
                                                   data.frame(zone = s,
                                                              periode = per,
                                                              date_sortie_95 = date_sortie_95[j,"date_debut"]))
          break
        }
      }
    }
  }
}  
        #### Tableau ----

resultats_95_sortie_hibernation
    ## Exportation des résultats ####
library(writexl)
write_xlsx(resultats_sortie_hibernation, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Comparaison distance Haversine points successifs/Entrée hibernation/Tableau_zone_individu_periode_V2activite.xlsx")
write_xlsx(resultats_moyenne_sortie_hibernation, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Comparaison distance Haversine points successifs/Entrée hibernation/Tableau_zone_moyenne_periode_V2activite.xlsx")
write_xlsx(resultats_95_sortie_hibernation, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Comparaison distance Haversine points successifs/Entrée hibernation/Tableau_zone_95_periode_V2activite.xlsx")


  # Calcul durée hibernation (pas utilisée donc pas optimisée) ----
resultats_hibernation_temp <- merge(resultats_entree_hibernation, resultats_sortie_hibernation, by = c("zone", "individu", "periode"), all = TRUE)
resultats_hibernation_temp$date_debut[is.na(resultats_hibernation_temp$date_debut)] <- "Pas pointée"
resultats_hibernation_temp$date_fin[is.na(resultats_hibernation_temp$date_fin)] <- "Pas pointée"
for(i in 1:length(resultats_hibernation_temp$date_fin)){
  if(resultats_hibernation_temp$date_debut[i] == "NA"){
    resultats_hibernation_temp$date_debut[i] <- NA
  }
  if(resultats_hibernation_temp$date_fin[i] == "NA"){
    resultats_hibernation_temp$date_fin[i] <- NA
  }
}

unique(resultats_hibernation_temp$date_fin)
pas_pointage <- c()
for (i in 1:length(resultats_hibernation_temp$individu)){
  if(!is.na(resultats_hibernation_temp$date_debut[i]) && resultats_hibernation_temp$date_debut[i] == "Pas pointée"){
    if(!is.na(resultats_hibernation_temp$date_fin[i]) && resultats_hibernation_temp$date_fin[i] == "Pas pointée"){
      pas_pointage <- c(pas_pointage, i)
    }
  }
}
resultats_hibernation <- resultats_hibernation_temp[-pas_pointage,]
resultats_hibernation$duree <- NA
resultats_hibernation$date_debut <- as.numeric(resultats_hibernation$date_debut)
resultats_hibernation$date_fin <- as.numeric(resultats_hibernation$date_fin)
for (i in 1:length(resultats_hibernation$individu)){
  if(is.na(resultats_hibernation$date_debut[i]) == F & is.na(resultats_hibernation$date_fin[i]) == F){
    resultats_hibernation$duree[i] <- 365 - (resultats_hibernation$date_debut[i]-resultats_hibernation$date_fin[i])
  } else {
    resultats_hibernation$duree[i] <- NA
  }
}
sum(is.na(resultats_hibernation$duree))*100/length(resultats_hibernation$duree)

library(writexl)
write_xlsx(resultats_hibernation, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Résumé/Tableau_dist_haversine.xlsx")

#############################################
#### Rupture de pente sur données brutes ####
#############################################
  # Test sur 1 individu ----

    ## Graphique dist_journ~j_julien ----

test_g106 <- jeu_donnees_final[jeu_donnees_final$id == "G106",]
test_g106 <- test_g106[order(test_g106$j_julien),]
test_g106$e <- as.numeric(test_g106$e)
test_g106$n <- as.numeric(test_g106$n)
test_g106$j_julien <- as.numeric(test_g106$j_julien)
test_dist_haversine_g106 <- data.frame(j_julien = numeric(), dist_journ = numeric())
for(i in 1:(nrow(test_g106)-1)){
  dist <- distHaversine(c(test_g106$e[i], test_g106$n[i]), c(test_g106$e[i+1], test_g106$n[i+1]))
  test_dist_haversine_g106 <- rbind(test_dist_haversine_g106,data.frame(j_julien = test_g106$j_julien[i+1], dist_journ = dist))
}
test_dist_haversine_g106 <- test_dist_haversine_g106[test_dist_haversine_g106$dist_journ < 400,]
ggplot() +
  geom_point(data = test_dist_haversine_g106, 
             aes(x = j_julien, y = dist_journ))

    ## Rupture sur la sortie d'hibernation ----

test_dist_haversine_g106_fin <- test_dist_haversine_g106[test_dist_haversine_g106$j_julien < 180,]
hist(test_dist_haversine_g106_fin$j_julien)
test_rupture_fin <- lm(dist_journ ~ j_julien,
                       data = test_dist_haversine_g106_fin)
seg_model_fin <- segmented(test_rupture_fin, seg.Z = ~j_julien, psi = list(j_julien = median(test_dist_haversine_g106_fin$j_julien)))

summary(seg_model_fin)
confint(seg_model_fin)
seg_model_fin$psi[, "Est."]
plot(test_dist_haversine_g106_fin$j_julien, test_dist_haversine_g106_fin$dist_journ)
plot(seg_model_fin, add = TRUE, col = "red")
# Pas bon car ne prend pas en compte la diff de fréq de pointages #

# Pareil pour le début
test_dist_haversine_g106_debut <- test_dist_haversine_g106[test_dist_haversine_g106$j_julien >= 180,]
test_rupture_debut <- lm(dist_journ ~ j_julien, data = test_dist_haversine_g106_debut)
library(segmented)
seg_model_debut <- segmented(test_rupture_debut, seg.Z = ~j_julien, psi = list(j_julien = mean(test_dist_haversine_g106_debut$j_julien)))
summary(seg_model_debut)

plot(test_dist_haversine_g106_debut$j_julien, test_dist_haversine_g106_debut$dist_journ)
plot(seg_model_debut, add = TRUE, col = "red")


    ## Utilisation bootstrap ----

library(boot)

boot_fun <- function(data, indices) {
  d <- data[indices, ]
  mod <- lm(dist_journ ~ j_julien, data = d)
  seg <- try(segmented(mod, seg.Z = ~j_julien,
                       psi = list(j_julien = median(d$j_julien))),
             silent = TRUE)
  
  if(inherits(seg, "try-error")) return(NA)
  
  return(seg$psi[,"Est."])
}

boot_res <- boot(test_dist_haversine_g106_fin, boot_fun, R = 200)
hist(boot_res$t, breaks = 50)

    ## Utilisation gam (je ne sais pas pourquoi) ----

library(mgcv)
gam_model <- gam(dist_journ ~ s(j_julien), data = test_dist_haversine_g106_fin)
plot(gam_model)

###########################################
#### Filtrage microhabitat automatique ####
###########################################
# Filtre ----

# expo == 1
# substrat et couv ne change pas
# cond_cach : "invisible", NA, "partiellement invisible", "enterree partiellement", "nichee", "enterree","cachee", "abritee"
# activite : "invisible", "cachee", "immo", "partiellement invisible", "enterree", "enterree partiellement"

jeu_donnees_final_filtre_expo <- head(jeu_donnees_final,0)
jeu_donnees_final_filtre_sub_couv <- head(jeu_donnees_final,0)
jeu_donnees_final_filtre_act <- head(jeu_donnees_final,0)
jeu_donnees_final_filtre_expo_sub_couv <- head(jeu_donnees_final,0)
jeu_donnees_final_filtre_expo_act <- head(jeu_donnees_final,0)
jeu_donnees_final_filtre_sub_couv_act <- head(jeu_donnees_final,0)
jeu_donnees_final_filtre_expo_sub_couv_act <- head(jeu_donnees_final,0)

for(z in unique(jeu_donnees_final$zone)){
  
  data_zone <- jeu_donnees_final[jeu_donnees_final$zone == z, ]
  
  for(ind in unique(data_zone$id)){
    
    data_ind <- data_zone[data_zone$id == ind, ]
    
    for(annee in unique(data_ind$annee)){
      
      data_ind_annee <- data_ind[data_ind$annee == annee, ]
      data_ind_annee <- data_ind_annee[order(data_ind_annee$j_julien), ]
      
      if(nrow(data_ind_annee) < 2) next
      
      # Gestion des NA (propre)
      data_ind_annee$couv[is.na(data_ind_annee$couv)] <- "NA"
      data_ind_annee$substrat[is.na(data_ind_annee$substrat)] <- "NA"
      data_ind_annee$expo[is.na(data_ind_annee$expo)] <- "NA"
      data_ind_annee$Cond_cach[is.na(data_ind_annee$Cond_cach)] <- "NA"
      data_ind_annee$activite[is.na(data_ind_annee$activite)] <- "NA"
      
      # Initialisation
      filtre_expo <- data_ind_annee[0, ]
      filtre_act <- data_ind_annee[0, ]
      filtre_sub_couv <- data_ind_annee[0, ]
      filtre_sub_couv_act <- data_ind_annee[0, ]
      filtre_expo_act <- data_ind_annee[0, ]
      filtre_expo_sub_couv <- data_ind_annee[0, ]
      filtre_expo_sub_couv_act <- data_ind_annee[0, ]
      
      for(i in 1:(nrow(data_ind_annee)-1)){
        
        ligne <- data_ind_annee[i, ]
        ligne_suiv <- data_ind_annee[i+1, ]
        
        # Conditions simplifiées
        cond_expo <- ligne$expo == "1" || ligne$expo == "NA"
        
        cond_act <- (ligne$Cond_cach %in% c("invisible", "partiellement invisible",
                                            "enterree partiellement", "enterree", "NA")) &&
          (ligne$activite %in% c("invisible", "cachee", "immo",
                                 "partiellement invisible",
                                 "enterree", "enterree partiellement"))
        
        cond_sub_couv <- (ligne$substrat == ligne_suiv$substrat) &&
          (ligne$couv == ligne_suiv$couv)
        
        # Application des filtres
        
        if(cond_expo){
          filtre_expo <- rbind(filtre_expo, ligne)
        }
        
        if(cond_act){
          filtre_act <- rbind(filtre_act, ligne)
        }
        
        if(cond_expo && cond_act){
          filtre_expo_act <- rbind(filtre_expo_act, ligne)
        }
        
        if(cond_sub_couv){
          filtre_sub_couv <- rbind(filtre_sub_couv, ligne)
        }
        
        if(cond_sub_couv && cond_act){
          filtre_sub_couv_act <- rbind(filtre_sub_couv_act, ligne)
        }
        
        if(cond_sub_couv && cond_expo){
          filtre_expo_sub_couv <- rbind(filtre_expo_sub_couv, ligne)
        }
        
        if(cond_sub_couv && cond_expo && cond_act){
          filtre_expo_sub_couv_act <- rbind(filtre_expo_sub_couv_act, ligne)
        }
      }
      
      # Ajout aux jeux globaux
      jeu_donnees_final_filtre_expo <- rbind(jeu_donnees_final_filtre_expo, filtre_expo)
      jeu_donnees_final_filtre_act <- rbind(jeu_donnees_final_filtre_act, filtre_act)
      jeu_donnees_final_filtre_sub_couv <- rbind(jeu_donnees_final_filtre_sub_couv, filtre_sub_couv)
      jeu_donnees_final_filtre_expo_act <- rbind(jeu_donnees_final_filtre_expo_act, filtre_expo_act)
      jeu_donnees_final_filtre_sub_couv_act <- rbind(jeu_donnees_final_filtre_sub_couv_act, filtre_sub_couv_act)
      jeu_donnees_final_filtre_expo_sub_couv <- rbind(jeu_donnees_final_filtre_expo_sub_couv, filtre_expo_sub_couv)
      jeu_donnees_final_filtre_expo_sub_couv_act <- rbind(jeu_donnees_final_filtre_expo_sub_couv_act, filtre_expo_sub_couv_act)
      
    }
  }
}

jeu_donnees_final_filtre_act <- jeu_donnees_final_filtre_act[jeu_donnees_final_filtre_act$mois %in% c(1,2,3,4,10,11,12),]
length(unique(jeu_donnees_final_filtre_act$id))
write_xlsx(jeu_donnees_final_filtre_act, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Jeu de données final/Jeu_de_données_final_filtrage_act.xlsx")

# Extrême maximal ----

resultat_date_filtrage <- data.frame(zone = character(), annee = numeric(), ind = character(), date_entree = numeric(), date_sortie = numeric())

for(z in unique(jeu_donnees_final_filtre_expo_sub_couv_act$zone)){
  
  data_zone <- jeu_donnees_final_filtre_expo_sub_couv_act[jeu_donnees_final_filtre_expo_sub_couv_act$zone == z, ]
  
  for(ind in unique(data_zone$id)){
    
    data_ind <- data_zone[data_zone$id == ind, ]
    
    for(annee in unique(data_ind$annee)){
      
      data_ind_annee <- data_ind[data_ind$annee == annee, ]
      data_ind_annee <- data_ind_annee[order(data_ind_annee$j_julien), ]
      
      data_ind_annee_debut_annee <- data_ind_annee[data_ind_annee$j_julien < 183,]
      data_ind_annee_fin_annee <- data_ind_annee[data_ind_annee$j_julien >= 183,]
      
      if(nrow(data_ind_annee_debut_annee) == 0){
        sortie <- NA
      } else {
        sortie <- data_ind_annee_debut_annee$j_julien[length(data_ind_annee_debut_annee$j_julien)]
      }
      
      if(nrow(data_ind_annee_fin_annee) == 0){
        entree <- NA
      } else {
        entree <- data_ind_annee_fin_annee$j_julien[1]
      }
      
      resultat_date_filtrage <- rbind(resultat_date_filtrage, data.frame(zone = z, annee = annee, ind = ind, date_entree = entree, date_sortie = sortie))
    }
  }
}

# Extrême minimal ----

resultat_date_filtrage <- data.frame(zone = character(), annee = numeric(), ind = character(), date_entree = numeric(), date_sortie = numeric())

for(z in unique(jeu_donnees_final_filtre_expo_sub_couv_act$zone)){
  
  data_zone <- jeu_donnees_final_filtre_expo_sub_couv_act[jeu_donnees_final_filtre_expo_sub_couv_act$zone == z, ]
  
  for(ind in unique(data_zone$id)){
    
    data_ind <- data_zone[data_zone$id == ind, ]
    
    for(annee in unique(data_ind$annee)){
      
      data_ind_annee <- data_ind[data_ind$annee == annee, ]
      data_ind_annee <- data_ind_annee[order(data_ind_annee$j_julien), ]
      
      data_ind_annee_debut_annee <- data_ind_annee[data_ind_annee$j_julien < 183,]
      data_ind_annee_fin_annee <- data_ind_annee[data_ind_annee$j_julien >= 183,]
      
      if(nrow(data_ind_annee_debut_annee) == 0){
        sortie <- NA
      } else {
        sortie <- data_ind_annee_debut_annee$j_julien[1]
      }
      
      if(nrow(data_ind_annee_fin_annee) == 0){
        entree <- NA
      } else {
        entree <- data_ind_annee_fin_annee$j_julien[length(data_ind_annee_fin_annee$j_julien)]
      }
      
      resultat_date_filtrage <- rbind(resultat_date_filtrage, data.frame(zone = z, annee = annee, ind = ind, date_entree = entree, date_sortie = sortie))
    }
  }
}


boxplot(resultat_date_filtrage$date_entree~resultat_date_filtrage$zone)
boxplot(resultat_date_filtrage$date_sortie~resultat_date_filtrage$zone)

########################################################
#### Analyse statistique filtre microhabitat manuel ####
########################################################
  # Création des diférents jeu de données ----

fichier_resultat <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Jeu données résultats/Date filtre manuel.xlsx")

fichier_resultat$zone[fichier_resultat$zone == "lac_redon"] <- "Lac Redon"
fichier_resultat$zone[fichier_resultat$zone == "callas"] <- "Callas"
fichier_resultat$zone[fichier_resultat$zone == "plaine_des_maures"] <- "Plaine"
fichier_resultat$sexe[fichier_resultat$sexe == "F"] <- "Femelle"
fichier_resultat$sexe[fichier_resultat$sexe == "M"] <- "Male"

fichier_resultat$log_entree <- log1p(fichier_resultat$j_julien_entree)
fichier_resultat$log_sortie <- log1p(fichier_resultat$j_julien_sortie)

fichier_resultat_entree <- fichier_resultat[!is.na(fichier_resultat$j_julien_entree),]
fichier_resultat_entree_sexe <- fichier_resultat_entree[!is.na(fichier_resultat_entree$sexe),]
fichier_resultat_sortie <- fichier_resultat[!is.na(fichier_resultat$j_julien_sortie),]
fichier_resultat_sortie_sexe <- fichier_resultat_sortie[!is.na(fichier_resultat_sortie$sexe),]
fichier_resultat_sexe <- fichier_resultat[!is.na(fichier_resultat$sexe),]

  # Test normalité ----

    ## Zone ----

unique(fichier_resultat$zone)

length(fichier_resultat_entree$j_julien_entree[fichier_resultat_entree$zone=="plaine_des_maures"])
length(fichier_resultat_entree$j_julien_entree[fichier_resultat_entree$zone=="callas"])
length(fichier_resultat_entree$j_julien_entree[fichier_resultat_entree$zone=="lac_redon"])
length(fichier_resultat_sortie$j_julien_sortie[fichier_resultat_sortie$zone=="plaine_des_maures"])
length(fichier_resultat_sortie$j_julien_sortie[fichier_resultat_sortie$zone=="callas"])
length(fichier_resultat_sortie$j_julien_sortie[fichier_resultat_sortie$zone=="lac_redon"])

tableau_zone_date_filtre_jeu_donnees <- data.frame(zone = unique(fichier_resultat$zone), date_entree = NA, date_sortie = NA)
for(i in 1:length(tableau_zone_date_filtre_jeu_donnees$zone)){
  tableau_zone_date_filtre_jeu_donnees$date_entree[i] <- length(fichier_resultat_entree$j_julien_entree[fichier_resultat_entree$zone==tableau_zone_date_filtre_jeu_donnees$zone[i]])
  tableau_zone_date_filtre_jeu_donnees$date_sortie[i] <- length(fichier_resultat_sortie$j_julien_sortie[fichier_resultat_sortie$zone==tableau_zone_date_filtre_jeu_donnees$zone[i]])
}
tableau_zone_date_filtre_jeu_donnees
write_xlsx(tableau_zone_date_filtre_jeu_donnees, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Analyse final/Filtre MH/Tableau_nombre_date_par_zone_filtreMH.xlsx")

boxplot(fichier_resultat$j_julien_entree~fichier_resultat$zone,xlab="Zone",ylab="entrée")
boxplot(fichier_resultat$j_julien_sortie~fichier_resultat$zone,xlab="Zone",ylab="sortie")

y <- rnorm(1000, mean(fichier_resultat$j_julien_entree[fichier_resultat$zone=="plaine_des_maures"],na.rm = T), sd(fichier_resultat$j_julien_entree[fichier_resultat$zone=="plaine_des_maures"],na.rm = T))
ks.test(fichier_resultat$j_julien_entree[fichier_resultat$zone=="plaine_des_maures"],y)
shapiro.test(fichier_resultat$j_julien_entree[fichier_resultat$zone=="callas"])
shapiro.test(fichier_resultat$j_julien_entree[fichier_resultat$zone=="lac_redon"])

y <- rnorm(1000, mean(fichier_resultat$j_julien_sortie[fichier_resultat$zone=="plaine_des_maures"],na.rm = T), sd(fichier_resultat$j_julien_sortie[fichier_resultat$zone=="plaine_des_maures"],na.rm = T))
ks.test(fichier_resultat$j_julien_sortie[fichier_resultat$zone=="plaine_des_maures"],y)
shapiro.test(fichier_resultat$j_julien_sortie[fichier_resultat$zone=="callas"])
shapiro.test(fichier_resultat$j_julien_sortie[fichier_resultat$zone=="lac_redon"])
#p-value<0.05 il y a donc une présence de différence

#Pourquoi il n'y a pas de distribution normale
hist(x = fichier_resultat$j_julien_entree[fichier_resultat$zone=="lac_redon"])
qqnorm(y=fichier_resultat$j_julien_entree[fichier_resultat$zone=="lac_redon"])
qqline(y=fichier_resultat$j_julien_entree[fichier_resultat$zone=="lac_redon"],col="red")

hist(x = fichier_resultat$j_julien_sortie[fichier_resultat$zone=="plaine_des_maures"])
qqnorm(y=fichier_resultat$j_julien_sortie[fichier_resultat$zone=="plaine_des_maures"])
qqline(y=fichier_resultat$j_julien_sortie[fichier_resultat$zone=="plaine_des_maures"],col="red")

hist(x = fichier_resultat$j_julien_sortie[fichier_resultat$zone=="lac_redon"])
qqnorm(y=fichier_resultat$j_julien_sortie[fichier_resultat$zone=="lac_redon"])
qqline(y=fichier_resultat$j_julien_sortie[fichier_resultat$zone=="lac_redon"],col="red")

hist(x = fichier_resultat$j_julien_sortie[fichier_resultat$zone=="callas"])
qqnorm(y=fichier_resultat$j_julien_sortie[fichier_resultat$zone=="callas"])
qqline(y=fichier_resultat$j_julien_sortie[fichier_resultat$zone=="callas"],col="red")


#On réalise un log+1 pour essayer de retrouver une distribution normale
#Log+1 pour éviter les cas de Log(0)= -l'infini et Log(0<x<1)= négatif
fichier_resultat$log_entree <- log1p(fichier_resultat$j_julien_entree)
fichier_resultat$log_sortie <- log1p(fichier_resultat$j_julien_sortie)

boxplot(fichier_resultat$log_entree~fichier_resultat$zone,xlab="Zone",ylab="Log(entrée)")
boxplot(fichier_resultat$log_sortie~fichier_resultat$zone,xlab="Zone",ylab="Log(sortie)")

y <- rnorm(1000, mean(fichier_resultat$log_entree[fichier_resultat$zone=="plaine_des_maures"],na.rm = T), sd(fichier_resultat$log_entree[fichier_resultat$zone=="plaine_des_maures"],na.rm = T))
ks.test(fichier_resultat$log_entree[fichier_resultat$zone=="plaine_des_maures"],y)
shapiro.test(fichier_resultat$log_entree[fichier_resultat$zone=="callas"])
shapiro.test(fichier_resultat$log_entree[fichier_resultat$zone=="lac_redon"])

y <- rnorm(1000, mean(fichier_resultat$log_sortie[fichier_resultat$zone=="plaine_des_maures"],na.rm = T), sd(fichier_resultat$log_sortie[fichier_resultat$zone=="plaine_des_maures"],na.rm = T))
ks.test(fichier_resultat$log_sortie[fichier_resultat$zone=="plaine_des_maures"],y)
shapiro.test(fichier_resultat$log_sortie[fichier_resultat$zone=="callas"])
shapiro.test(fichier_resultat$log_sortie[fichier_resultat$zone=="lac_redon"])
#Normalité toujours pas OK

    ## Sexe ----

length(fichier_resultat_sexe$j_julien_entree[fichier_resultat_sexe$sexe=="M"])
length(fichier_resultat_sexe$j_julien_entree[fichier_resultat_sexe$sexe=="F"])

length(fichier_resultat_entree_sexe$sexe[fichier_resultat_entree_sexe$sexe=="M"])
length(fichier_resultat_entree_sexe$sexe[fichier_resultat_entree_sexe$sexe=="F"])
length(fichier_resultat_sortie_sexe$sexe[fichier_resultat_sortie_sexe$sexe=="M"])
length(fichier_resultat_sortie_sexe$sexe[fichier_resultat_sortie_sexe$sexe=="F"])

tableau_sexe_date_filtre_jeu_donnees <- data.frame(sexe = unique(fichier_resultat_sexe$sexe), date_entree = NA, date_sortie = NA)
for(i in 1:length(tableau_sexe_date_filtre_jeu_donnees$sexe)){
  tableau_sexe_date_filtre_jeu_donnees$date_entree[i] <- length(fichier_resultat_entree_sexe$j_julien_entree[fichier_resultat_entree_sexe$sexe==tableau_sexe_date_filtre_jeu_donnees$sexe[i]])
  tableau_sexe_date_filtre_jeu_donnees$date_sortie[i] <- length(fichier_resultat_sortie_sexe$j_julien_sortie[fichier_resultat_sortie_sexe$sexe==tableau_sexe_date_filtre_jeu_donnees$sexe[i]])
}
tableau_sexe_date_filtre_jeu_donnees
write_xlsx(tableau_sexe_date_filtre_jeu_donnees, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Analyse finale/Filtre MH/Tableau_nombre_date_par_sexe_filtreMH.xlsx")

boxplot(fichier_resultat$j_julien_entree~fichier_resultat$sexe,xlab="Sexe",ylab="entrée")
boxplot(fichier_resultat$j_julien_sortie~fichier_resultat$sexe,xlab="Sexe",ylab="sortie")

ym <- rnorm(1000, mean(fichier_resultat_entree_sexe$j_julien_entree[fichier_resultat_entree_sexe$sexe=="M"],na.rm = T), sd(fichier_resultat_entree_sexe$j_julien_entree[fichier_resultat_entree_sexe$sexe=="M"],na.rm = T))
ks.test(fichier_resultat_entree_sexe$j_julien_entree[fichier_resultat_entree_sexe$sexe=="M"],ym)
yf <- rnorm(1000, mean(fichier_resultat_entree_sexe$j_julien_entree[fichier_resultat_entree_sexe$sexe=="F"],na.rm = T), sd(fichier_resultat_entree_sexe$j_julien_entree[fichier_resultat_entree_sexe$sexe=="F"],na.rm = T))
ks.test(fichier_resultat_entree_sexe$j_julien_entree[fichier_resultat_entree_sexe$sexe=="F"],yf)

ym <- rnorm(1000, mean(fichier_resultat_sortie_sexe$j_julien_sortie[fichier_resultat_sortie_sexe$sexe=="M"],na.rm = T), sd(fichier_resultat_sortie_sexe$j_julien_sortie[fichier_resultat_sortie_sexe$sexe=="M"],na.rm = T))
ks.test(fichier_resultat_sortie_sexe$j_julien_sortie[fichier_resultat_sortie_sexe$sexe=="M"],ym)
yf <- rnorm(1000, mean(fichier_resultat_sortie_sexe$j_julien_sortie[fichier_resultat_sortie_sexe$sexe=="F"],na.rm = T), sd(fichier_resultat_sortie_sexe$j_julien_sortie[fichier_resultat_sortie_sexe$sexe=="F"],na.rm = T))
ks.test(fichier_resultat_sortie_sexe$j_julien_sortie[fichier_resultat_sortie_sexe$sexe=="F"],yf)
#Normalité OK

  # Homoscédasticité ----

    ## Zone ----

fligner.test(fichier_resultat$j_julien_entree~fichier_resultat$zone)
fligner.test(fichier_resultat$j_julien_sortie~fichier_resultat$zone)
#Homoscédasticité pas OK

    ## Sexe ----

var.test(fichier_resultat_sexe$j_julien_entree~fichier_resultat_sexe$sexe)
var.test(fichier_resultat_sexe$j_julien_sortie~fichier_resultat_sexe$sexe)
#Homoscédasticité OK

  # Tests stats ----

    ## ANOVA zone ----

#Définition de l'ordre d'affichage des boxplots
fichier_resultat$zone <- factor(fichier_resultat$zone, levels = c("Plaine", "Callas", "Lac Redon"))

par(mfrow = c(1,2))
boxplot(fichier_resultat$j_julien_entree~fichier_resultat$zone, xlab = "Zones", ylab = "Date en jours julien", main = "Entrée", col = adjustcolor(c("red", "#418E4D", "blue"), alpha.f = 0.2), border = c("red", "#418E4D", "blue"), names = c("", "", ""), yaxt = "n")
axis(2, at = seq(0, max(fichier_resultat$j_julien_entree, na.rm = TRUE), by = 20),las = 1)
text(x = 1:3, y = par("usr")[3] - 9, labels = c("Plaine", "Callas", "Lac Redon"), col = c("red", "#418E4D", "blue"), xpd = TRUE)

boxplot(fichier_resultat$j_julien_sortie~fichier_resultat$zone,xlab="Zones",ylab="Date en jours julien", main = "Sortie", col = adjustcolor(c("red", "#418E4D", "blue"), alpha.f = 0.2), border = c("red", "#418E4D", "blue"), names = c("", "", ""), yaxt = "n")
axis(2, at = seq(0, max(fichier_resultat$j_julien_entree, na.rm = TRUE), by = 20),las = 1)
text(x = 1:3, y = par("usr")[3] - 10, labels = c("Plaine", "Callas", "Lac Redon"), col = c("red", "#418E4D", "blue"), xpd = TRUE)
par(mfrow = c(1,1))

kruskal.test(fichier_resultat$j_julien_entree~fichier_resultat$zone)
anova_1_zone_entree <- kruskal.test(fichier_resultat$j_julien_entree~fichier_resultat$zone) # car pas de normalité
anova_1_zone_entree$p.value
dunn.test(fichier_resultat$j_julien_entree, fichier_resultat$zone, method="bonferroni") #test post-oc
#Diff sinificative entre Pliane/Lac et, Callas/Lac
mean(fichier_resultat_entree$j_julien_entree[fichier_resultat_entree$zone == "Plaine"])
mean(fichier_resultat_entree$j_julien_entree[fichier_resultat_entree$zone == "Callas"])
mean(fichier_resultat_entree$j_julien_entree[fichier_resultat_entree$zone == "Lac Redon"])

kruskal.test(fichier_resultat$j_julien_sortie~fichier_resultat$zone)
anova_1_zone_sortie <- kruskal.test(fichier_resultat$j_julien_sortie~fichier_resultat$zone) # car pas de normalité
anova_1_zone_sortie$p.value
dunn.test(fichier_resultat$j_julien_sortie, fichier_resultat$zone, method="bonferroni") #test post-oc
# Pas de diff significative
mean(fichier_resultat_sortie$j_julien_sortie[fichier_resultat_sortie$zone == "Plaine"])
mean(fichier_resultat_sortie$j_julien_sortie[fichier_resultat_sortie$zone == "Callas"])
mean(fichier_resultat_sortie$j_julien_sortie[fichier_resultat_sortie$zone == "Lac Redon"])

    ## Student sexe ----
library(report)

par(mfrow = c(1,2))
boxplot(fichier_resultat$j_julien_entree~fichier_resultat$sexe, xlab = "Sexe", ylab = "Date en jours julien", main = "Entrée", yaxt = "n")
axis(2, at = seq(0, max(fichier_resultat$j_julien_entree, na.rm = TRUE), by = 20),las = 1)

boxplot(fichier_resultat$j_julien_sortie~fichier_resultat$sexe,xlab = "Sexe",ylab="Date en jours julien", main = "Sortie", yaxt = "n")
axis(2, at = seq(0, max(fichier_resultat$j_julien_entree, na.rm = TRUE), by = 20),las = 1)
par(mfrow = c(1,1))

t.test(fichier_resultat_sexe$j_julien_entree~fichier_resultat_sexe$sexe, var.equal=T)
#Pas de diff significative

test <- t.test(fichier_resultat_sexe$j_julien_sortie~fichier_resultat_sexe$sexe, var.equal=T)
report(test)
83.60656-75.78261
# différence significative de 1 semaine


    ## ANOVA zone et sexe ----

#Création d'une nouvelle colonne dans notre jeu de données
fichier_resultat_sexe$zonesexe <- paste(fichier_resultat_sexe$sexe,fichier_resultat_sexe$zone)

boxplot(fichier_resultat_sexe$j_julien_entree~fichier_resultat_sexe$zonesexe,xlab="Zone",ylab="entrée")
boxplot(fichier_resultat_sexe$j_julien_sortie~fichier_resultat_sexe$zonesexe,xlab="Zone",ylab="sortie")

kruskal.test(fichier_resultat_sexe$j_julien_entree,fichier_resultat_sexe$zonesexe,method="bonferroni")
dunn.test(fichier_resultat_sexe$j_julien_entree,fichier_resultat_sexe$zonesexe,method="bonferroni")
#Résultats cohérents à part F Callas

kruskal.test(fichier_resultat_sexe$j_julien_sortie,fichier_resultat_sexe$zonesexe,method="bonferroni")
dunn.test(fichier_resultat_sexe$j_julien_sortie,fichier_resultat_sexe$zonesexe,method="bonferroni")
#Aucunes diff significatives
#############################
#### Utilisation des GAM ####
#############################
  # 3 modèles possibles ----

    ## tw(link = "log") ----

# Fonction relevant les intersections entre le modèle et le seuil de 10m
calcul_intersect <- function(data_ind_annee, ind, annee){
  
  # Tri par date
  data_ind_annee <- data_ind_annee[order(data_ind_annee$j_julien), ]
  data_ind_annee <- data_ind_annee[!is.na(data_ind_annee$j_julien),]
  
  # Vérification nombre de points
  if(nrow(data_ind_annee) <= 20) return(NULL)
  
  # Calcul distances
  dist_haversine_ind_annee <- data.frame(j_julien = numeric(), dist_journ = numeric(), delta_t = numeric())
  
  for(i in 1:(nrow(data_ind_annee)-1)){
    
    delta_t <- data_ind_annee$j_julien[i+1] - data_ind_annee$j_julien[i]
    if(delta_t == 0) next  # sécurité contre les double points
    
    dist <- distHaversine(c(as.numeric(data_ind_annee$e[i]), as.numeric(data_ind_annee$n[i])),
                          c(as.numeric(data_ind_annee$e[i+1]), as.numeric(data_ind_annee$n[i+1]))) # as.numeric si jamais une coordonnee est au format character()
    
    dist_journ <- dist / delta_t
    
    dist_haversine_ind_annee <- rbind(dist_haversine_ind_annee, data.frame(j_julien = data_ind_annee$j_julien[i+1], dist_journ = dist_journ, delta_t = delta_t))
    
  }
  
  # Nettoyage
  dist_haversine_ind_annee <- dist_haversine_ind_annee[dist_haversine_ind_annee$dist_journ < 200, ] # Retrait des points aberrants
  dist_haversine_ind_annee$dist_journ[dist_haversine_ind_annee$dist_journ == 0] <- 1e-10 # Conversion des 0 en valeur positive très faible pour pouvoir appliquer le log 
  dist_haversine_ind_annee <- na.omit(dist_haversine_ind_annee) # sécurité contre les NAs qui feraient planter le modèle
  
  # Modèle GAM
  modele <- gam(dist_journ ~ s(j_julien),
                family = tw(link = "log"),
                weights = delta_t,
                method = "REML",
                data = dist_haversine_ind_annee)
  
  # Grille
  x_seq <- seq(min(dist_haversine_ind_annee$j_julien), max(dist_haversine_ind_annee$j_julien), length.out = 500)
  pred <- predict(modele, newdata = data.frame(j_julien = x_seq), type = "response")
  
  # Détection zone d'intersection
  diff <- pred - 10
  idx <- which(diff[-1] * diff[-length(diff)] < 0)
  if(length(idx) == 0) return(NULL)
  
  # Interpolation pour trouver le point précis
  x_intersect <- sapply(idx, function(i){
    x_seq[i] +
      (x_seq[i+1] - x_seq[i]) *
      abs(diff[i]) / abs(diff[i] - diff[i+1])
  })
  
  # Production du graphique
  png(filename = paste0("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Graphique modèle gam tw/plot_",ind,"_",as.character(annee),".png"))
  plot(dist_haversine_ind_annee$j_julien, dist_haversine_ind_annee$dist_journ, pch=16, col="grey", main = paste0(ind,"_",as.character(annee)))
  lines(x_seq, pred, col="blue", lwd=2)
  abline(h=10, col="red", lwd=2)
  dev.off()
  
  return(x_intersect)
}


# Création d'un ensemble de liste contenant les intersections de chaque individu à chaque année

liste_resultats <- list()
compteur <- 1

for(z in unique(jeu_donnees_final$zone)){
  
  data_zone <- jeu_donnees_final[jeu_donnees_final$zone == z, ]
  
  for(ind in unique(data_zone$id)){
    
    data_ind <- data_zone[data_zone$id == ind, ]
    
    for(annee in unique(data_ind$annee)){
      
      data_ind_annee <- data_ind[data_ind$annee == annee, ]
      
      x_intersect <- calcul_intersect(data_ind_annee, ind, annee)
      
      if(is.null(x_intersect)) next
      
      liste_resultats[[compteur]] <- list(
        zone = z,
        ind = ind,
        annee = annee,
        intersections = x_intersect
      )
      
      compteur <- compteur + 1
    }
  }
}


# Nombre max d'intersections
max_inter <- max(sapply(liste_resultats, function(x) length(x$intersections)))
max_inter

# Création tableau final
tableau_date_test <- data.frame(zone = sapply(liste_resultats, `[[`, "zone"),ind = sapply(liste_resultats, `[[`, "ind"), annee = sapply(liste_resultats, `[[`, "annee"), date_entree = NA, date_sortie = NA)

# Ajout colonnes intermédiaires
for(i in 1:(max_inter-2)){
  tableau_date_test[[paste0("inter", i)]] <- NA
}


# Remplissage du dataframe
for(i in seq_along(liste_resultats)){
  
  xi <- liste_resultats[[i]]$intersections
  
  tableau_date_test$date_sortie[i] <- round(xi[1],0)
  tableau_date_test$date_entree[i] <- round(xi[length(xi)],0)
  
  if(length(xi) == 1){
    if(xi[1] <= 183){
      tableau_date_test$date_entree[i] <- NA
    } else {
      tableau_date_test$date_sortie[i] <- NA
    }
  }
  
  if(length(xi) > 2){
    for(j in 2:(length(xi)-1)){
      tableau_date_test[i, paste0("inter", j-1)] <- round(xi[j],0)
    }
  }
}
tableau_date_test
write_xlsx(tableau_date_test, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Jeu données résultats/gam_tw.xlsx")

    ## Gamma(link = "log") ----

# Fonction relevant les intersections entre le modèle et le seuil de 10m
calcul_intersect <- function(data_ind_annee, ind, annee){
  
  # Tri par date
  data_ind_annee <- data_ind_annee[order(data_ind_annee$j_julien), ]
  data_ind_annee <- data_ind_annee[!is.na(data_ind_annee$j_julien),]
  
  # Vérification nombre de points
  if(nrow(data_ind_annee) <= 20) return(NULL)
  
  # Calcul distances
  dist_haversine_ind_annee <- data.frame(j_julien = numeric(), dist_journ = numeric(), delta_t = numeric())
  
  for(i in 1:(nrow(data_ind_annee)-1)){
    
    delta_t <- data_ind_annee$j_julien[i+1] - data_ind_annee$j_julien[i]
    if(delta_t == 0) next  # sécurité contre les double points
    
    dist <- distHaversine(c(as.numeric(data_ind_annee$e[i]), as.numeric(data_ind_annee$n[i])),
                          c(as.numeric(data_ind_annee$e[i+1]), as.numeric(data_ind_annee$n[i+1]))) # as.numeric si jamais une coordonnee est au format character()
    
    dist_journ <- dist / delta_t
    
    dist_haversine_ind_annee <- rbind(dist_haversine_ind_annee, data.frame(j_julien = data_ind_annee$j_julien[i+1], dist_journ = dist_journ, delta_t = delta_t))
    
  }
  
  # Nettoyage
  dist_haversine_ind_annee <- dist_haversine_ind_annee[dist_haversine_ind_annee$dist_journ < 200, ] # Retrait des points aberrants
  dist_haversine_ind_annee$dist_journ[dist_haversine_ind_annee$dist_journ == 0] <- 1e-10 # Conversion des 0 en valeur positive très faible pour pouvoir appliquer le log 
  dist_haversine_ind_annee <- na.omit(dist_haversine_ind_annee) # sécurité contre les NAs qui feraient planter le modèle
  
  # Modèle GAM
  modele <- gam(dist_journ ~ s(j_julien),
                family = Gamma(link = "log"),
                weights = delta_t,
                method = "REML",
                data = dist_haversine_ind_annee)
  
  # Grille
  x_seq <- seq(min(dist_haversine_ind_annee$j_julien), max(dist_haversine_ind_annee$j_julien), length.out = 500)
  pred <- predict(modele, newdata = data.frame(j_julien = x_seq), type = "response")
  
  # Détection zone d'intersection
  diff <- pred - 10
  idx <- which(diff[-1] * diff[-length(diff)] < 0)
  if(length(idx) == 0) return(NULL)
  
  # Interpolation pour trouver le point précis
  x_intersect <- sapply(idx, function(i){
    x_seq[i] +
      (x_seq[i+1] - x_seq[i]) *
      abs(diff[i]) / abs(diff[i] - diff[i+1])
  })
  
  # Production du graphique
  png(filename = paste0("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Graphique modèle gam gamma/plot_",ind,"_",as.character(annee),".png"))
  plot(dist_haversine_ind_annee$j_julien, dist_haversine_ind_annee$dist_journ, pch=16, col="grey", main = paste0(ind,"_",as.character(annee)))
  lines(x_seq, pred, col="blue", lwd=2)
  abline(h=10, col="red", lwd=2)
  dev.off()
  
  return(x_intersect)
}


# Création d'un ensemble de liste contenant les intersections de chaque individu à chaque année

liste_resultats <- list()
compteur <- 1

for(z in unique(jeu_donnees_final$zone)){
  
  data_zone <- jeu_donnees_final[jeu_donnees_final$zone == z, ]
  
  for(ind in unique(data_zone$id)){
    
    data_ind <- data_zone[data_zone$id == ind, ]
    
    for(annee in unique(data_ind$annee)){
      
      data_ind_annee <- data_ind[data_ind$annee == annee, ]
      
      x_intersect <- calcul_intersect(data_ind_annee, ind, annee)
      
      if(is.null(x_intersect)) next
      
      liste_resultats[[compteur]] <- list(
        zone = z,
        ind = ind,
        annee = annee,
        intersections = x_intersect
      )
      
      compteur <- compteur + 1
    }
  }
}


# Nombre max d'intersections
max_inter <- max(sapply(liste_resultats, function(x) length(x$intersections)))
max_inter

# Création tableau final
tableau_date_test <- data.frame(zone = sapply(liste_resultats, `[[`, "zone"),ind = sapply(liste_resultats, `[[`, "ind"), annee = sapply(liste_resultats, `[[`, "annee"), date_entree = NA, date_sortie = NA)

# Ajout colonnes intermédiaires
for(i in 1:(max_inter-2)){
  tableau_date_test[[paste0("inter", i)]] <- NA
}


# Remplissage du dataframe
for(i in seq_along(liste_resultats)){
  
  xi <- liste_resultats[[i]]$intersections
  
  tableau_date_test$date_sortie[i] <- round(xi[1],0)
  tableau_date_test$date_entree[i] <- round(xi[length(xi)],0)
  
  if(length(xi) == 1){
    if(xi[1] <= 183){
      tableau_date_test$date_entree[i] <- NA
    } else {
      tableau_date_test$date_sortie[i] <- NA
    }
  }
  
  if(length(xi) > 2){
    for(j in 2:(length(xi)-1)){
      tableau_date_test[i, paste0("inter", j-1)] <- round(xi[j],0)
    }
  }
}
tableau_date_test
write_xlsx(tableau_date_test, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Jeu données résultats/gam_gamma.xlsx")


    ## gaussian() ----

# Fonction relevant les intersections entre le modèle et le seuil de 10m
calcul_intersect <- function(data_ind_annee, ind, annee){
  
  # Tri par date
  data_ind_annee <- data_ind_annee[order(data_ind_annee$j_julien), ]
  data_ind_annee <- data_ind_annee[!is.na(data_ind_annee$j_julien),]
  
  # Vérification nombre de points
  if(nrow(data_ind_annee) <= 20) return(NULL)
  
  # Calcul distances
  dist_haversine_ind_annee <- data.frame(j_julien = numeric(), dist_journ = numeric(), delta_t = numeric())
  
  for(i in 1:(nrow(data_ind_annee)-1)){
    
    delta_t <- data_ind_annee$j_julien[i+1] - data_ind_annee$j_julien[i]
    if(delta_t == 0) next  # sécurité contre les double points
    
    dist <- distHaversine(c(as.numeric(data_ind_annee$e[i]), as.numeric(data_ind_annee$n[i])),
                          c(as.numeric(data_ind_annee$e[i+1]), as.numeric(data_ind_annee$n[i+1]))) # as.numeric si jamais une coordonnee est au format character()
    
    dist_journ <- dist / delta_t
    
    dist_haversine_ind_annee <- rbind(dist_haversine_ind_annee, data.frame(j_julien = data_ind_annee$j_julien[i+1], dist_journ = dist_journ, delta_t = delta_t))
    
  }
  
  # Nettoyage
  dist_haversine_ind_annee <- dist_haversine_ind_annee[dist_haversine_ind_annee$dist_journ < 200, ] # Retrait des points aberrants
  dist_haversine_ind_annee$dist_journ[dist_haversine_ind_annee$dist_journ == 0] <- 1e-10 # Conversion des 0 en valeur positive très faible pour pouvoir appliquer le log 
  dist_haversine_ind_annee <- na.omit(dist_haversine_ind_annee) # sécurité contre les NAs qui feraient planter le modèle
  
  # Modèle GAM
  modele <- gam(dist_journ ~ s(j_julien),
                family = gaussian(),
                weights = delta_t,
                method = "REML",
                data = dist_haversine_ind_annee)
  
  # Grille
  x_seq <- seq(min(dist_haversine_ind_annee$j_julien), max(dist_haversine_ind_annee$j_julien), length.out = 500)
  pred <- predict(modele, newdata = data.frame(j_julien = x_seq), type = "response")
  
  # Détection zone d'intersection
  diff <- pred - 10
  idx <- which(diff[-1] * diff[-length(diff)] < 0)
  if(length(idx) == 0) return(NULL)
  
  # Interpolation pour trouver le point précis
  x_intersect <- sapply(idx, function(i){
    x_seq[i] +
      (x_seq[i+1] - x_seq[i]) *
      abs(diff[i]) / abs(diff[i] - diff[i+1])
  })
  
  # Production du graphique
  png(filename = paste0("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Graphique modèle gam gaussian/plot_",ind,"_",as.character(annee),".png"))
  plot(dist_haversine_ind_annee$j_julien, dist_haversine_ind_annee$dist_journ, pch=16, col="grey", main = paste0(ind,"_",as.character(annee)))
  lines(x_seq, pred, col="blue", lwd=2)
  abline(h=10, col="red", lwd=2)
  dev.off()
  
  return(x_intersect)
}


# Création d'un ensemble de liste contenant les intersections de chaque individu à chaque année

liste_resultats <- list()
compteur <- 1

for(z in unique(jeu_donnees_final$zone)){
  
  data_zone <- jeu_donnees_final[jeu_donnees_final$zone == z, ]
  
  for(ind in unique(data_zone$id)){
    
    data_ind <- data_zone[data_zone$id == ind, ]
    
    for(annee in unique(data_ind$annee)){
      
      data_ind_annee <- data_ind[data_ind$annee == annee, ]
      
      x_intersect <- calcul_intersect(data_ind_annee, ind, annee)
      
      if(is.null(x_intersect)) next
      
      liste_resultats[[compteur]] <- list(
        zone = z,
        ind = ind,
        annee = annee,
        intersections = x_intersect
      )
      
      compteur <- compteur + 1
    }
  }
}


# Nombre max d'intersections
max_inter <- max(sapply(liste_resultats, function(x) length(x$intersections)))
max_inter

# Création tableau final
tableau_date_test <- data.frame(zone = sapply(liste_resultats, `[[`, "zone"),ind = sapply(liste_resultats, `[[`, "ind"), annee = sapply(liste_resultats, `[[`, "annee"), date_entree = NA, date_sortie = NA)

# Ajout colonnes intermédiaires
for(i in 1:(max_inter-2)){
  tableau_date_test[[paste0("inter", i)]] <- NA
}


# Remplissage du dataframe
for(i in seq_along(liste_resultats)){
  
  xi <- liste_resultats[[i]]$intersections
  
  tableau_date_test$date_sortie[i] <- round(xi[1],0)
  tableau_date_test$date_entree[i] <- round(xi[length(xi)],0)
  
  if(length(xi) == 1){
    if(xi[1] <= 183){
      tableau_date_test$date_entree[i] <- NA
    } else {
      tableau_date_test$date_sortie[i] <- NA
    }
  }
  
  if(length(xi) > 2){
    for(j in 2:(length(xi)-1)){
      tableau_date_test[i, paste0("inter", j-1)] <- round(xi[j],0)
    }
  }
}
tableau_date_test
write_xlsx(tableau_date_test, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Jeu données résultats/gam_gaussian.xlsx")


  # Choix du modèle le plus robuste entre gaussian, tw et gamma ----

    ## Détection modèle avec l'AIC le plus faible ----

      ### Fonction relevant le modèle avec l'AIC le plus faible ----

calcul_bon_modele <- function(data_ind_annee){
  
  # Tri par date
  data_ind_annee <- data_ind_annee[order(data_ind_annee$j_julien), ]
  data_ind_annee <- data_ind_annee[!is.na(data_ind_annee$j_julien),]
  
  # Vérification nombre de points
  if(nrow(data_ind_annee) <= 20) return(NULL)
  
  # Calcul distances
  dist_haversine_ind_annee <- data.frame(j_julien = numeric(), dist_journ = numeric(), delta_t = numeric())
  
  for(i in 1:(nrow(data_ind_annee)-1)){
    
    delta_t <- data_ind_annee$j_julien[i+1] - data_ind_annee$j_julien[i]
    if(delta_t == 0) next  # sécurité contre les double points
    
    dist <- distHaversine(c(as.numeric(data_ind_annee$e[i]), as.numeric(data_ind_annee$n[i])),
                          c(as.numeric(data_ind_annee$e[i+1]), as.numeric(data_ind_annee$n[i+1]))) # as.numeric si jamais une coordonnee est au format character()
    
    dist_journ <- dist / delta_t
    
    dist_haversine_ind_annee <- rbind(dist_haversine_ind_annee, data.frame(j_julien = data_ind_annee$j_julien[i+1], dist_journ = dist_journ, delta_t = delta_t))
    
  }
  
  # Nettoyage
  dist_haversine_ind_annee <- dist_haversine_ind_annee[dist_haversine_ind_annee$dist_journ < 500, ] # Retrait des points aberrants
  dist_haversine_ind_annee$dist_journ[dist_haversine_ind_annee$dist_journ == 0] <- 1e-10 # Conversion des 0 en valeur positive très faible pour pouvoir appliquer le log 
  dist_haversine_ind_annee <- na.omit(dist_haversine_ind_annee) # sécurité contre les NAs qui feraient planter le modèle
  
  # Modèles GAM
  gamma <- gam(dist_journ ~ s(j_julien),
               family = Gamma(link = "log"),
               weights = delta_t,
               method = "REML",
               data = dist_haversine_ind_annee)
  gaussian <- gam(dist_journ ~ s(j_julien),
                  family = gaussian(),
                  weights = delta_t,
                  method = "REML",
                  data = dist_haversine_ind_annee)
  tw <- gam(dist_journ ~ s(j_julien),
            family = tw(link = "log"),
            weights = delta_t,
            method = "REML",
            data = dist_haversine_ind_annee)
  aic_modele <- AIC(gamma, gaussian, tw)
  
  if(aic_modele["gamma","AIC"] == min(aic_modele$AIC)) return("gamma")
  if(aic_modele["gaussian","AIC"] == min(aic_modele$AIC)) return("gaussian")
  if(aic_modele["tw","AIC"] == min(aic_modele$AIC)) return("tw")
}


      ### Application de la fonction sur chaque individu à chaque année ----

nb_gamma <- 0
nb_gaussian <- 0
nb_tw <- 0

for(z in unique(jeu_donnees_final$zone)){
  
  data_zone <- jeu_donnees_final[jeu_donnees_final$zone == z, ]
  
  for(ind in unique(data_zone$id)){
    
    data_ind <- data_zone[data_zone$id == ind, ]
    
    for(annee in unique(data_ind$annee)){
      
      data_ind_annee <- data_ind[data_ind$annee == annee, ]
      
      modele_aic_le_plus_faible <- calcul_bon_modele(data_ind_annee)
      
      if(is.null(modele_aic_le_plus_faible)) next
      
      if(modele_aic_le_plus_faible == "gamma"){
        nb_gamma <- nb_gamma + 1
      }
      if(modele_aic_le_plus_faible == "gaussian"){
        nb_gaussian <- nb_gaussian + 1
      }
      if(modele_aic_le_plus_faible == "tw"){
        nb_tw <- nb_tw + 1
      }
    }
  }
}

      ### Tableau résumé ----

nb_modele_groupe <- c(nb_gamma, nb_gaussian, nb_tw)
resume_aic_modele <- data.frame(modele = c("gamma", "gaussian", "tw"), nb = NA, pourcentage = NA)
for(i in 1:3){
  resume_aic_modele$nb[i] <- nb_modele_groupe[i]
  resume_aic_modele$pourcentage[i] <- (resume_aic_modele$nb[i]*100)/(nb_gamma + nb_gaussian + nb_tw)
}
resume_aic_modele

    ## Détection modèle avec l'AIC le plus fort ----

      ### Fonction relevant le modèle avec l'AIC le plus fort ----

calcul_mauvais_modele <- function(data_ind_annee){
  
  # Tri par date
  data_ind_annee <- data_ind_annee[order(data_ind_annee$j_julien), ]
  data_ind_annee <- data_ind_annee[!is.na(data_ind_annee$j_julien),]
  
  # Vérification nombre de points
  if(nrow(data_ind_annee) <= 20) return(NULL)
  
  # Calcul distances
  dist_haversine_ind_annee <- data.frame(j_julien = numeric(), dist_journ = numeric(), delta_t = numeric())
  
  for(i in 1:(nrow(data_ind_annee)-1)){
    
    delta_t <- data_ind_annee$j_julien[i+1] - data_ind_annee$j_julien[i]
    if(delta_t == 0) next  # sécurité contre les double points
    
    dist <- distHaversine(c(as.numeric(data_ind_annee$e[i]), as.numeric(data_ind_annee$n[i])),
                          c(as.numeric(data_ind_annee$e[i+1]), as.numeric(data_ind_annee$n[i+1]))) # as.numeric si jamais une coordonnee est au format character()
    
    dist_journ <- dist / delta_t
    
    dist_haversine_ind_annee <- rbind(dist_haversine_ind_annee, data.frame(j_julien = data_ind_annee$j_julien[i+1], dist_journ = dist_journ, delta_t = delta_t))
    
  }
  
  # Nettoyage
  dist_haversine_ind_annee <- dist_haversine_ind_annee[dist_haversine_ind_annee$dist_journ < 500, ] # Retrait des points aberrants
  dist_haversine_ind_annee$dist_journ[dist_haversine_ind_annee$dist_journ == 0] <- 1e-10 # Conversion des 0 en valeur positive très faible pour pouvoir appliquer le log 
  dist_haversine_ind_annee <- na.omit(dist_haversine_ind_annee) # sécurité contre les NAs qui feraient planter le modèle
  
  # Modèles GAM
  gamma <- gam(dist_journ ~ s(j_julien),
               family = Gamma(link = "log"),
               weights = delta_t,
               method = "REML",
               data = dist_haversine_ind_annee)
  gaussian <- gam(dist_journ ~ s(j_julien),
                  family = gaussian(),
                  weights = delta_t,
                  method = "REML",
                  data = dist_haversine_ind_annee)
  tw <- gam(dist_journ ~ s(j_julien),
            family = tw(link = "log"),
            weights = delta_t,
            method = "REML",
            data = dist_haversine_ind_annee)
  aic_modele <- AIC(gamma, gaussian, tw)
  
  if(aic_modele["gamma","AIC"] == max(aic_modele$AIC)) return("gamma")
  if(aic_modele["gaussian","AIC"] == max(aic_modele$AIC)) return("gaussian")
  if(aic_modele["tw","AIC"] == max(aic_modele$AIC)) return("tw")
}


      ### Application de la fonction sur chaque individu à chaque année ----

nb_gamma_2 <- 0
nb_gaussian_2 <- 0
nb_tw_2 <- 0

for(z in unique(jeu_donnees_final$zone)){
  
  data_zone <- jeu_donnees_final[jeu_donnees_final$zone == z, ]
  
  for(ind in unique(data_zone$id)){
    
    data_ind <- data_zone[data_zone$id == ind, ]
    
    for(annee in unique(data_ind$annee)){
      
      data_ind_annee <- data_ind[data_ind$annee == annee, ]
      
      modele_aic_le_plus_fort <- calcul_mauvais_modele(data_ind_annee)
      
      if(is.null(modele_aic_le_plus_fort)) next
      
      if(modele_aic_le_plus_fort == "gamma"){
        nb_gamma_2 <- nb_gamma_2 + 1
      }
      if(modele_aic_le_plus_fort == "gaussian"){
        nb_gaussian_2 <- nb_gaussian_2 + 1
      }
      if(modele_aic_le_plus_fort == "tw"){
        nb_tw_2 <- nb_tw_2 + 1
      }
    }
  }
}

      ### Tableau résumé ----

nb_modele_groupe_2 <- c(nb_gamma_2, nb_gaussian_2, nb_tw_2)
resume_aic_modele_2 <- data.frame(modele = c("gamma", "gaussian", "tw"), nb = NA, pourcentage = NA)
for(i in 1:3){
  resume_aic_modele_2$nb[i] <- nb_modele_groupe_2[i]
  resume_aic_modele_2$pourcentage[i] <- (resume_aic_modele_2$nb[i]*100)/(nb_gamma_2 + nb_gaussian_2 + nb_tw_2)
}
resume_aic_modele_2
###################################################
#### Relevé des intersections GAM/seuil de 10m ####
###################################################
  # Fonction relevant les intersections entre le modèle et le seuil de 10m ----

calcul_intersect <- function(data_ind_annee, ind, annee){
  
  # Tri par date
  data_ind_annee <- data_ind_annee[order(data_ind_annee$j_julien), ]
  data_ind_annee <- data_ind_annee[!is.na(data_ind_annee$j_julien),]
  
  # Vérification nombre de points
  if(nrow(data_ind_annee) <= 20) return(NULL)
  
  # Calcul distances
  dist_haversine_ind_annee <- data.frame(j_julien = numeric(), dist_journ = numeric(), delta_t = numeric())
  
  for(i in 1:(nrow(data_ind_annee)-1)){
    
    delta_t <- data_ind_annee$j_julien[i+1] - data_ind_annee$j_julien[i]
    if(delta_t == 0) next  # sécurité contre les double points
    
    dist <- distHaversine(c(as.numeric(data_ind_annee$e[i]), as.numeric(data_ind_annee$n[i])),
                          c(as.numeric(data_ind_annee$e[i+1]), as.numeric(data_ind_annee$n[i+1]))) # as.numeric si jamais une coordonnee est au format character()
    
    dist_journ <- dist / delta_t
    
    dist_haversine_ind_annee <- rbind(dist_haversine_ind_annee, data.frame(j_julien = data_ind_annee$j_julien[i+1], dist_journ = dist_journ, delta_t = delta_t))
    
  }
  
  # Nettoyage
  dist_haversine_ind_annee <- dist_haversine_ind_annee[dist_haversine_ind_annee$dist_journ < 500, ] # Retrait des points aberrants
  dist_haversine_ind_annee$dist_journ[dist_haversine_ind_annee$dist_journ == 0] <- 1e-10 # Conversion des 0 en valeur positive très faible pour pouvoir appliquer le log 
  dist_haversine_ind_annee <- na.omit(dist_haversine_ind_annee) # sécurité contre les NAs qui feraient planter le modèle
  
  # Modèle GAM
  modele <- gam(dist_journ ~ s(j_julien),
                family = Gamma(link = "log"),
                weights = delta_t,
                method = "REML",
                data = dist_haversine_ind_annee)
  
  # Grille
  x_seq <- seq(min(dist_haversine_ind_annee$j_julien), max(dist_haversine_ind_annee$j_julien), length.out = 500)
  pred <- predict(modele, newdata = data.frame(j_julien = x_seq), type = "response")
  
  # Détection zone d'intersection
  diff <- pred - 10
  idx <- which(diff[-1] * diff[-length(diff)] < 0)
  if(length(idx) == 0) return(NULL)
  
  # Interpolation pour trouver le point précis
  x_intersect <- sapply(idx, function(i){
    x_seq[i] +
      (x_seq[i+1] - x_seq[i]) *
      abs(diff[i]) / abs(diff[i] - diff[i+1])
  })
  
  # Production du graphique
  png(filename = paste0("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Graphique modèle gam/plot_",ind,"_",as.character(annee),".png"))
  plot(dist_haversine_ind_annee$j_julien, dist_haversine_ind_annee$dist_journ, pch=16, col="grey", main = paste0(ind,"_",as.character(annee)))
  lines(x_seq, pred, col="blue", lwd=2)
  abline(h=10, col="red", lwd=2)
  dev.off()
  
  return(x_intersect)
}


  # Création d'un ensemble de listes contenant les intersections de chaque individu à chaque année ----

liste_resultats <- list()
compteur <- 1

for(z in unique(jeu_donnees_final$zone)){
  
  data_zone <- jeu_donnees_final[jeu_donnees_final$zone == z, ]
  
  for(ind in unique(data_zone$id)){
    
    data_ind <- data_zone[data_zone$id == ind, ]
    
    for(annee in unique(data_ind$annee)){
      
      data_ind_annee <- data_ind[data_ind$annee == annee, ]
      
      x_intersect <- calcul_intersect(data_ind_annee, ind, annee)
      
      if(is.null(x_intersect)) next
      
      liste_resultats[[compteur]] <- list(
        zone = z,
        ind = ind,
        annee = annee,
        intersections = x_intersect
      )
      
      compteur <- compteur + 1
    }
  }
}

  # Création tableau final ----

tableau_date_test <- data.frame(zone = sapply(liste_resultats, `[[`, "zone"),ind = sapply(liste_resultats, `[[`, "ind"), annee = sapply(liste_resultats, `[[`, "annee"), date_entree = NA, date_sortie = NA)

    ## Ajout colonnes intermédiaires ----

# Nombre max d'intersections

max_inter <- max(sapply(liste_resultats, function(x) length(x$intersections)))
max_inter

for(i in 1:(max_inter-2)){
  tableau_date_test[[paste0("inter", i)]] <- NA
}

  # Remplissage du tableau ----

for(i in seq_along(liste_resultats)){
  
  xi <- liste_resultats[[i]]$intersections
  
  tableau_date_test$date_sortie[i] <- round(xi[1],0)
  tableau_date_test$date_entree[i] <- round(xi[length(xi)],0)
  
  if(length(xi) == 1){
    if(xi[1] <= 183){
      tableau_date_test$date_entree[i] <- NA
    } else {
      tableau_date_test$date_sortie[i] <- NA
    }
  }
  
  if(length(xi) > 2){
    for(j in 2:(length(xi)-1)){
      tableau_date_test[i, paste0("inter", j-1)] <- round(xi[j],0)
    }
  }
}
tableau_date_test


###########################################################
#### Relevé des premiers points dépassant seuil de 10m ####
###########################################################
  # Fonction relevant les intersections avec le seuil de 10m ----

date_hibernation <- function(data_ind_annee, sexe, zone, ind, annee){
  
  # Tri par date
  data_ind_annee <- data_ind_annee[order(data_ind_annee$j_julien), ]
  data_ind_annee <- data_ind_annee[!is.na(data_ind_annee$j_julien),]
  
  # Vérification nombre de points
  if(nrow(data_ind_annee) <= 20) return(NULL)
  
  # Calcul distances
  dist_haversine_ind_annee <- data.frame(j_julien = numeric(), dist_journ = numeric(), delta_t = numeric())
  
  for(i in 1:(nrow(data_ind_annee)-1)){
    
    delta_t <- data_ind_annee$j_julien[i+1] - data_ind_annee$j_julien[i]
    if(delta_t == 0) next  # sécurité contre les double points
    
    dist <- distHaversine(c(as.numeric(data_ind_annee$e[i]), as.numeric(data_ind_annee$n[i])),
                          c(as.numeric(data_ind_annee$e[i+1]), as.numeric(data_ind_annee$n[i+1]))) # as.numeric si jamais une coordonnee est au format character()
    
    dist_journ <- dist / delta_t
    
    dist_haversine_ind_annee <- rbind(dist_haversine_ind_annee, data.frame(j_julien = data_ind_annee$j_julien[i+1], dist_journ = dist_journ, delta_t = delta_t))
    
  }
  
  # Nettoyage
  dist_haversine_ind_annee <- dist_haversine_ind_annee[dist_haversine_ind_annee$dist_journ < 200, ] # Retrait des points aberrants
  dist_haversine_ind_annee <- na.omit(dist_haversine_ind_annee) # sécurité contre les NAs qui feraient planter le modèle
  
  entree <- NA
  sortie <- NA
  
  for(i in 1:length(dist_haversine_ind_annee$dist_journ)){
    if(dist_haversine_ind_annee$dist_journ[i]>=10){
      if(i == 1) break
      sortie <- dist_haversine_ind_annee$j_julien[i]
      break
    }
  }
  for(i in 1:length(dist_haversine_ind_annee$dist_journ)-1){
    indice <- length(dist_haversine_ind_annee$dist_journ)-i
    if(dist_haversine_ind_annee$dist_journ[indice]>=10){
      entree <- dist_haversine_ind_annee$j_julien[indice+1]
      break
    }
  }
  
  # Production du graphique
  png(filename = paste0("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Graphique sans modèle/plot_",ind,"_",as.character(annee),".png"))
  plot(dist_haversine_ind_annee$j_julien, dist_haversine_ind_annee$dist_journ, pch=16, col="grey", main = paste0(ind,"_",as.character(annee)))
  abline(h=10, col="red", lwd=2)
  abline(v=entree, col="green", lty=2)
  abline(v=sortie, col="green", lty=2)
  dev.off()
  
  return(data.frame(sexe=sexe, zone=zone, id=ind, annee=annee, entree_hibernation=entree, sortie_hibernation=sortie))
}


  # Création du tableau contenant les intersections de chaque individu à chaque année ----

resultats_hibernation <- data.frame(sexe=character(), zone=character(), id=character(), annee=numeric(), entree_hibernation=numeric(), sortie_hibernation=numeric())

for(z in unique(jeu_donnees_final$zone)){
  
  data_zone <- jeu_donnees_final[jeu_donnees_final$zone == z, ]
  
  for(ind in unique(data_zone$id)){
    
    data_ind <- data_zone[data_zone$id == ind, ]
    
    for(annee in unique(data_ind$annee)){
      
      data_ind_annee <- data_ind[data_ind$annee == annee, ]
      
      donnees_hibernation <- date_hibernation(data_ind_annee, data_ind_annee$sexe[1], z, ind, annee)
      
      if(is.null(date_hibernation)) next
      
      resultats_hibernation <- rbind(resultats_hibernation, donnees_hibernation)
    }
  }
}

  # Exportation ----

write_xlsx(resultats_hibernation, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Jeu données résultats/sans_gam.xlsx")


#############################
#### Analyse statistique ####
#############################
  # Création des différents jeu de données ----

resultats_sans_gam <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Jeu données résultats/sans_gam.xlsx")

resultats_sans_gam$log_entree <- log1p(resultats_sans_gam$j_julien_entree)
resultats_sans_gam$log_sortie <- log1p(resultats_sans_gam$j_julien_sortie)

resultats_sans_gam_entree <- resultats_sans_gam[!is.na(resultats_sans_gam$j_julien_entree),]
resultats_sans_gam_entree_sexe <- resultats_sans_gam_entree[!is.na(resultats_sans_gam_entree$sexe),]
resultats_sans_gam_sortie <- resultats_sans_gam[!is.na(resultats_sans_gam$j_julien_sortie),]
resultats_sans_gam_sortie_sexe <- resultats_sans_gam_sortie[!is.na(resultats_sans_gam_sortie$sexe),]
resultats_sans_gam_sexe <- resultats_sans_gam[!is.na(resultats_sans_gam$sexe),]
resultats_sans_gam_plaine <- resultats_sans_gam[resultats_sans_gam$zone=="plaine_des_maures",]
resultats_sans_gam_plaine_entree <- resultats_sans_gam_plaine[!is.na(resultats_sans_gam_plaine$j_julien_entree),]
resultats_sans_gam_plaine_sortie <- resultats_sans_gam_plaine[!is.na(resultats_sans_gam_plaine$j_julien_sortie),]

  # Test normalité ----

    ## Zone ----

unique(resultats_sans_gam$zone)

length(resultats_sans_gam_entree$j_julien_entree[resultats_sans_gam_entree$zone=="plaine_des_maures"])
length(resultats_sans_gam_entree$j_julien_entree[resultats_sans_gam_entree$zone=="callas"])
length(resultats_sans_gam_entree$j_julien_entree[resultats_sans_gam_entree$zone=="lac_redon"])
length(resultats_sans_gam_sortie$j_julien_sortie[resultats_sans_gam_sortie$zone=="plaine_des_maures"])
length(resultats_sans_gam_sortie$j_julien_sortie[resultats_sans_gam_sortie$zone=="callas"])
length(resultats_sans_gam_sortie$j_julien_sortie[resultats_sans_gam_sortie$zone=="lac_redon"])

tableau_zone_date_sans_gam <- data.frame(zone = unique(resultats_sans_gam$zone), date_entree = NA, date_sortie = NA)
for(i in 1:length(tableau_zone_date_sans_gam$zone)){
  tableau_zone_date_sans_gam$date_entree[i] <- length(resultats_sans_gam_entree$j_julien_entree[resultats_sans_gam_entree$zone==tableau_zone_date_sans_gam$zone[i]])
  tableau_zone_date_sans_gam$date_sortie[i] <- length(resultats_sans_gam_sortie$j_julien_sortie[resultats_sans_gam_sortie$zone==tableau_zone_date_sans_gam$zone[i]])
}
tableau_zone_date_sans_gam
write_xlsx(tableau_zone_date_sans_gam, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Analyse final/Filtre 10m/Tableau_nombre_date_par_zone_filtre10m.xlsx")

boxplot(resultats_sans_gam$j_julien_entree~resultats_sans_gam$zone,xlab="Zone",ylab="entrée")
boxplot(resultats_sans_gam$j_julien_sortie~resultats_sans_gam$zone,xlab="Zone",ylab="sortie")

y <- rnorm(1000, mean(resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="plaine_des_maures"],na.rm = T), sd(resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="plaine_des_maures"],na.rm = T))
ks.test(resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="plaine_des_maures"],y)
shapiro.test(resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="callas"])
shapiro.test(resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="lac_redon"]) # Normalité OK

y <- rnorm(1000, mean(resultats_sans_gam$j_julien_sortie[resultats_sans_gam$zone=="plaine_des_maures"],na.rm = T), sd(resultats_sans_gam$j_julien_sortie[resultats_sans_gam$zone=="plaine_des_maures"],na.rm = T))
ks.test(resultats_sans_gam$j_julien_sortie[resultats_sans_gam$zone=="plaine_des_maures"],y)
shapiro.test(resultats_sans_gam$j_julien_sortie[resultats_sans_gam$zone=="callas"]) # Normalité OK
shapiro.test(resultats_sans_gam$j_julien_sortie[resultats_sans_gam$zone=="lac_redon"]) # Effectif trop faible
#p-value<0.05 il y a donc une présence de différence

#Pourquoi il n'y a pas de distribution normale
hist(x = resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="plaine_des_maures"])
qqnorm(y=resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="plaine_des_maures"])
qqline(y=resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="plaine_des_maures"],col="red")

hist(x = resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="callas"])
qqnorm(y=resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="callas"])
qqline(y=resultats_sans_gam$j_julien_entree[resultats_sans_gam$zone=="callas"],col="red")

hist(x = resultats_sans_gam$j_julien_sortie[resultats_sans_gam$zone=="plaine_des_maures"])
qqnorm(y=resultats_sans_gam$j_julien_sortie[resultats_sans_gam$zone=="plaine_des_maures"])
qqline(y=resultats_sans_gam$j_julien_sortie[resultats_sans_gam$zone=="plaine_des_maures"],col="red")


#On réalise un log+1 pour essayer de retrouver une distribution normale
#Log+1 pour éviter les cas de Log(0)= -l'infini et Log(0<x<1)= négatif
resultats_sans_gam$log_entree <- log1p(resultats_sans_gam$j_julien_entree)
resultats_sans_gam$log_sortie <- log1p(resultats_sans_gam$j_julien_sortie)

boxplot(resultats_sans_gam$log_entree~resultats_sans_gam$zone,xlab="Zone",ylab="Log(entrée)")
boxplot(resultats_sans_gam$log_sortie~resultats_sans_gam$zone,xlab="Zone",ylab="Log(sortie)")

y <- rnorm(1000, mean(resultats_sans_gam$log_entree[resultats_sans_gam$zone=="plaine_des_maures"],na.rm = T), sd(resultats_sans_gam$log_entree[resultats_sans_gam$zone=="plaine_des_maures"],na.rm = T))
ks.test(resultats_sans_gam$log_entree[resultats_sans_gam$zone=="plaine_des_maures"],y)
shapiro.test(resultats_sans_gam$log_entree[resultats_sans_gam$zone=="callas"])
shapiro.test(resultats_sans_gam$log_entree[resultats_sans_gam$zone=="lac_redon"])

y <- rnorm(1000, mean(resultats_sans_gam$log_sortie[resultats_sans_gam$zone=="plaine_des_maures"],na.rm = T), sd(resultats_sans_gam$log_sortie[resultats_sans_gam$zone=="plaine_des_maures"],na.rm = T))
ks.test(resultats_sans_gam$log_sortie[resultats_sans_gam$zone=="plaine_des_maures"],y)
shapiro.test(resultats_sans_gam$log_sortie[resultats_sans_gam$zone=="callas"])
shapiro.test(resultats_sans_gam$log_sortie[resultats_sans_gam$zone=="lac_redon"])
#Changement pour plianes des maures sortie sinon normalité toujours pas OK

    ## Sexe ----

length(resultats_sans_gam_sexe$j_julien_entree[resultats_sans_gam_sexe$sexe=="M"])
length(resultats_sans_gam_sexe$j_julien_entree[resultats_sans_gam_sexe$sexe=="F"])

length(resultats_sans_gam_entree_sexe$sexe[resultats_sans_gam_entree_sexe$sexe=="M"])
length(resultats_sans_gam_entree_sexe$sexe[resultats_sans_gam_entree_sexe$sexe=="F"])
length(resultats_sans_gam_sortie_sexe$sexe[resultats_sans_gam_sortie_sexe$sexe=="M"])
length(resultats_sans_gam_sortie_sexe$sexe[resultats_sans_gam_sortie_sexe$sexe=="F"])

tableau_sexe_date_sans_gam <- data.frame(sexe = unique(resultats_sans_gam_sexe$sexe), date_entree = NA, date_sortie = NA)
for(i in 1:length(tableau_sexe_date_sans_gam$sexe)){
  tableau_sexe_date_sans_gam$date_entree[i] <- length(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe==tableau_sexe_date_sans_gam$sexe[i]])
  tableau_sexe_date_sans_gam$date_sortie[i] <- length(resultats_sans_gam_sortie_sexe$j_julien_sortie[resultats_sans_gam_sortie_sexe$sexe==tableau_sexe_date_sans_gam$sexe[i]])
}
tableau_sexe_date_sans_gam
write_xlsx(tableau_sexe_date_sans_gam, "C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Analyse finale/Filtre 10m/Tableau_nombre_date_par_sexe_filtre10m.xlsx")

boxplot(resultats_sans_gam$j_julien_entree~resultats_sans_gam$sexe,xlab="Sexe",ylab="entrée")
boxplot(resultats_sans_gam$j_julien_sortie~resultats_sans_gam$sexe,xlab="Sexe",ylab="sortie")

ym <- rnorm(1000, mean(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="M"],na.rm = T), sd(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="M"],na.rm = T))
ks.test(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="M"],ym)
yf <- rnorm(1000, mean(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="F"],na.rm = T), sd(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="F"],na.rm = T))
ks.test(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="F"],yf)

ym <- rnorm(1000, mean(resultats_sans_gam_sortie_sexe$j_julien_sortie[resultats_sans_gam_sortie_sexe$sexe=="M"],na.rm = T), sd(resultats_sans_gam_sortie_sexe$j_julien_sortie[resultats_sans_gam_sortie_sexe$sexe=="M"],na.rm = T))
ks.test(resultats_sans_gam_sortie_sexe$j_julien_sortie[resultats_sans_gam_sortie_sexe$sexe=="M"],ym)
yf <- rnorm(1000, mean(resultats_sans_gam_sortie_sexe$j_julien_sortie[resultats_sans_gam_sortie_sexe$sexe=="F"],na.rm = T), sd(resultats_sans_gam_sortie_sexe$j_julien_sortie[resultats_sans_gam_sortie_sexe$sexe=="F"],na.rm = T))
ks.test(resultats_sans_gam_sortie_sexe$j_julien_sortie[resultats_sans_gam_sortie_sexe$sexe=="F"],yf)
#Normalité OK sauf pour mâle sortie

#Transformation log+1

resultats_sans_gam_entree_sexe$log_entree <- log1p(resultats_sans_gam_entree_sexe$j_julien_entree)
resultats_sans_gam_entree_sexe$log_sortie <- log1p(resultats_sans_gam_entree_sexe$j_julien_sortie)

resultats_sans_gam_sortie_sexe$log_entree <- log1p(resultats_sans_gam_sortie_sexe$j_julien_entree)
resultats_sans_gam_sortie_sexe$log_sortie <- log1p(resultats_sans_gam_sortie_sexe$j_julien_sortie)

ym <- rnorm(1000, mean(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="M"],na.rm = T), sd(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="M"],na.rm = T))
ks.test(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="M"],ym)
yf <- rnorm(1000, mean(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="F"],na.rm = T), sd(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="F"],na.rm = T))
ks.test(resultats_sans_gam_entree_sexe$j_julien_entree[resultats_sans_gam_entree_sexe$sexe=="F"],yf)

ym <- rnorm(1000, mean(resultats_sans_gam_sortie_sexe$log_sortie[resultats_sans_gam_sortie_sexe$sexe=="M"],na.rm = T), sd(resultats_sans_gam_sortie_sexe$log_sortie[resultats_sans_gam_sortie_sexe$sexe=="M"],na.rm = T))
ks.test(resultats_sans_gam_sortie_sexe$log_sortie[resultats_sans_gam_sortie_sexe$sexe=="M"],ym)
yf <- rnorm(1000, mean(resultats_sans_gam_sortie_sexe$j_julien_sortie[resultats_sans_gam_sortie_sexe$sexe=="F"],na.rm = T), sd(resultats_sans_gam_sortie_sexe$j_julien_sortie[resultats_sans_gam_sortie_sexe$sexe=="F"],na.rm = T))
ks.test(resultats_sans_gam_sortie_sexe$j_julien_sortie[resultats_sans_gam_sortie_sexe$sexe=="F"],yf)
#Normalité OK pour tous

  # Homoscédasticité ----

    ## Zone ----

fligner.test(resultats_sans_gam$j_julien_entree~resultats_sans_gam$zone)
fligner.test(resultats_sans_gam$j_julien_sortie~resultats_sans_gam$zone)
#Homoscédasticité pas OK

    ## Sexe ----

var.test(resultats_sans_gam_sexe$log_entree~resultats_sans_gam_sexe$sexe)
var.test(resultats_sans_gam_sexe$log_sortie~resultats_sans_gam_sexe$sexe)
#Homoscédasticité OK

  # Tests stats ----

    ## ANOVA zone ----

boxplot(resultats_sans_gam$j_julien_entree~resultats_sans_gam$zone,xlab="Zone",ylab="entrée")
anova_1_zone_entree <- kruskal.test(resultats_sans_gam$j_julien_entree~resultats_sans_gam$zone) # car pas de normalité
anova_1_zone_entree$p.value
dunn.test(resultats_sans_gam$j_julien_entree, resultats_sans_gam$zone, method="bonferroni") #test post-oc
#Seulement une diff entre Redon et Plaine

boxplot(resultats_sans_gam$j_julien_sortie~resultats_sans_gam$zone,xlab="Zone",ylab="sortie")
anova_1_zone_sortie <- kruskal.test(resultats_sans_gam$j_julien_sortie~resultats_sans_gam$zone) # car pas de normalité
anova_1_zone_sortie$p.value
dunn.test(resultats_sans_gam$j_julien_sortie, resultats_sans_gam$zone, method="bonferroni") #test post-oc
#Aucunes différences

    ## Student sexe ----

boxplot(resultats_sans_gam$j_julien_entree~resultats_sans_gam$sexe,xlab="Sexe",ylab="entrée")
boxplot(resultats_sans_gam$j_julien_sortie~resultats_sans_gam$sexe,xlab="Sexe",ylab="sortie")

test <- t.test(resultats_sans_gam_sexe$log_entree~resultats_sans_gam_sexe$sexe, var.equal=T)
report(test)

test_2 <- t.test(resultats_sans_gam_sexe$log_sortie~resultats_sans_gam_sexe$sexe, var.equal=T)
report(test_2)
# Pas de diff significative

    ## ANOVA zone et sexe ----

#Création d'une nouvelle colonne dans notre jeu de données
resultats_sans_gam_sexe$zonesexe <- paste(resultats_sans_gam_sexe$sexe,resultats_sans_gam_sexe$zone)

boxplot(resultats_sans_gam_sexe$j_julien_entree~resultats_sans_gam_sexe$zonesexe,xlab="Sexe",ylab="entrée")
boxplot(resultats_sans_gam_sexe$j_julien_sortie~resultats_sans_gam_sexe$zonesexe,xlab="Sexe",ylab="sortie")

kruskal.test(resultats_sans_gam_sexe$j_julien_entree,resultats_sans_gam_sexe$zonesexe,method="bonferroni")
dunn.test(resultats_sans_gam_sexe$j_julien_entree,resultats_sans_gam_sexe$zonesexe,method="bonferroni")
#Diff entre lac redon et la plaine n'est pas dû au sexe

kruskal.test(resultats_sans_gam_sexe$j_julien_sortie,resultats_sans_gam_sexe$zonesexe,method="bonferroni")
dunn.test(resultats_sans_gam_sexe$j_julien_sortie,resultats_sans_gam_sexe$zonesexe,method="bonferroni")
#Aucune diff remarqué malgré la p-value<0.05
######################################
#### Stats comparaison 2 méthodes ####
######################################
  # Création des différents jeu de données ----
resultats_sans_gam <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Jeu données résultats/sans_gam.xlsx")
resultats_sans_gam$methode <- "Seuil 10m"

resultats_sans_gam$log_entree <- log1p(resultats_sans_gam$j_julien_entree)
resultats_sans_gam$log_sortie <- log1p(resultats_sans_gam$j_julien_sortie)

resultats_sans_gam_entree <- resultats_sans_gam[!is.na(resultats_sans_gam$j_julien_entree),]
resultats_sans_gam_entree_sexe <- resultats_sans_gam_entree[!is.na(resultats_sans_gam_entree$sexe),]
resultats_sans_gam_sortie <- resultats_sans_gam[!is.na(resultats_sans_gam$j_julien_sortie),]
resultats_sans_gam_sortie_sexe <- resultats_sans_gam_sortie[!is.na(resultats_sans_gam_sortie$sexe),]
resultats_sans_gam_sexe <- resultats_sans_gam[!is.na(resultats_sans_gam$sexe),]
resultats_sans_gam_plaine <- resultats_sans_gam[resultats_sans_gam$zone=="plaine_des_maures",]
resultats_sans_gam_plaine_entree <- resultats_sans_gam_plaine[!is.na(resultats_sans_gam_plaine$j_julien_entree),]
resultats_sans_gam_plaine_sortie <- resultats_sans_gam_plaine[!is.na(resultats_sans_gam_plaine$j_julien_sortie),]

fichier_resultat <- read_excel("C:/Users/mathi/Documents/Cours/Master/Stage/DATA HIBERNATION/Zone de non droit/Résultats/Jeu données résultats/Date filtre manuel.xlsx")
fichier_resultat$date_entree <- NULL
fichier_resultat$date_sortie <- NULL
fichier_resultat$methode <- "Filtre microhabitat"

fichier_resultat$log_entree <- log1p(fichier_resultat$j_julien_entree)
fichier_resultat$log_sortie <- log1p(fichier_resultat$j_julien_sortie)

fichier_resultat_entree <- fichier_resultat[!is.na(fichier_resultat$j_julien_entree),]
fichier_resultat_entree_sexe <- fichier_resultat_entree[!is.na(fichier_resultat_entree$sexe),]
fichier_resultat_sortie <- fichier_resultat[!is.na(fichier_resultat$j_julien_sortie),]
fichier_resultat_sortie_sexe <- fichier_resultat_sortie[!is.na(fichier_resultat_sortie$sexe),]
fichier_resultat_sexe <- fichier_resultat[!is.na(fichier_resultat$sexe),]

comparaison_methode <- rbind(fichier_resultat, resultats_sans_gam)
comparaison_methode_entree <- comparaison_methode[!is.na(comparaison_methode$j_julien_entree),]
comparaison_methode_sortie <- comparaison_methode[!is.na(comparaison_methode$j_julien_sortie),]

  # Test normalité ----

length(resultats_sans_gam_entree$j_julien_entree)
length(resultats_sans_gam_sortie$j_julien_sortie)
length(fichier_resultat_entree$j_julien_entree)
length(fichier_resultat_sortie$j_julien_sortie)

par(mfrow = c(1,2))
boxplot(comparaison_methode$j_julien_entree ~ comparaison_methode$methode, xlab = "Méthodes", ylab = "Date en jours julien", main = "Entrée", yaxt = "n")
axis(2, at = seq(240, max(comparaison_methode$j_julien_entree, na.rm = TRUE), by = 20),las = 1)

boxplot(comparaison_methode$j_julien_sortie~comparaison_methode$methode,xlab="Méthodes",ylab="Date en jours julien", main = "Sortie", yaxt = "n")
axis(2, at = seq(0, max(comparaison_methode$j_julien_entree, na.rm = TRUE), by = 20),las = 1)
par(mfrow = c(1,1))

y <- rnorm(1000, mean(resultats_sans_gam$j_julien_entree,na.rm = T), sd(resultats_sans_gam$j_julien_entree,na.rm = T))
ks.test(resultats_sans_gam$j_julien_entree,y) # Normalité OK
y <- rnorm(1000, mean(resultats_sans_gam$j_julien_sortie,na.rm = T), sd(resultats_sans_gam$j_julien_sortie,na.rm = T))
ks.test(resultats_sans_gam$j_julien_sortie,y) # Normalité pas OK
y <- rnorm(1000, mean(fichier_resultat$j_julien_entree,na.rm = T), sd(fichier_resultat$j_julien_entree,na.rm = T))
ks.test(fichier_resultat$j_julien_entree,y) # Normalité OK
y <- rnorm(1000, mean(fichier_resultat$j_julien_sortie,na.rm = T), sd(fichier_resultat$j_julien_sortie,na.rm = T))
ks.test(fichier_resultat$j_julien_sortie,y) # Normalité pas OK

#Pourquoi il n'y a pas de distribution normale
hist(x = fichier_resultat$j_julien_sortie)
qqnorm(y=fichier_resultat$j_julien_sortie)
qqline(y=fichier_resultat$j_julien_sortie,col="red")

hist(x = resultats_sans_gam$j_julien_sortie)
qqnorm(y=resultats_sans_gam$j_julien_sortie)
qqline(y=resultats_sans_gam$j_julien_sortie,col="red")

#On réalise un log+1 pour essayer de retrouver une distribution normale
#Log+1 pour éviter les cas de Log(0)= -l'infini et Log(0<x<1)= négatif
y <- rnorm(1000, mean(resultats_sans_gam$log_sortie,na.rm = T), sd(resultats_sans_gam$log_sortie,na.rm = T))
ks.test(resultats_sans_gam$log_sortie,y) # Normalité pas OK
y <- rnorm(1000, mean(fichier_resultat$log_sortie,na.rm = T), sd(fichier_resultat$log_sortie,na.rm = T))
ks.test(fichier_resultat$log_sortie,y) # Normalité pas OK
# Rien ne change

  # Homoscédasticité ----

var.test(comparaison_methode$j_julien_entree~comparaison_methode$methode) #Homoscédasticité pas OK
fligner.test(comparaison_methode$j_julien_sortie~comparaison_methode$methode) #Homoscédasticité OK

  # Tests stats ----

    ## Student ----

par(mfrow = c(1,2))
boxplot(comparaison_methode$j_julien_entree ~ comparaison_methode$methode, xlab = "Méthodes", ylab = "Date en jours julien", main = "Entrée", yaxt = "n")
axis(2, at = seq(240, max(comparaison_methode$j_julien_entree, na.rm = TRUE), by = 20),las = 1)

boxplot(comparaison_methode$j_julien_sortie~comparaison_methode$methode,xlab="Méthodes",ylab="Date en jours julien", main = "Sortie", yaxt = "n")
axis(2, at = seq(0, max(comparaison_methode$j_julien_entree, na.rm = TRUE), by = 20),las = 1)
par(mfrow = c(1,1))

t.test(comparaison_methode$j_julien_entree~comparaison_methode$methode, var.equal=F)
test <- t.test(comparaison_methode$j_julien_entree~comparaison_methode$methode, var.equal=F)
report(test)
# Diff significative de 12 jours

wilcox.test(comparaison_methode$j_julien_sortie~comparaison_methode$methode)
test_2 <- wilcox.test(comparaison_methode$j_julien_sortie~comparaison_methode$methode)
report(test_2)
mean(comparaison_methode_sortie$j_julien_sortie[comparaison_methode_sortie$methode=="Seuil 10m"])
mean(comparaison_methode_sortie$j_julien_sortie[comparaison_methode_sortie$methode=="Filtre microhabitat"])
mean(comparaison_methode_sortie$j_julien_sortie[comparaison_methode_sortie$methode=="Seuil 10m"])-mean(comparaison_methode_sortie$j_julien_sortie[comparaison_methode_sortie$methode=="Filtre microhabitat"])
# Diff significative de 33 jours

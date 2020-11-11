#--------------------------------------#
# ACTIVATION DES LIRAIRIES NECESSAIRES #
#--------------------------------------#

install.packages("C50")
library(C50)
install.packages("randomForest")
library(randomForest)
install.packages("e1071")
library(e1071)
install.packages("naivebayes")
library(naivebayes)
install.packages("nnet")
library(nnet)
install.packages("kknn")
library(kknn)
install.packages("ggplot2")
library(ggplot2)
install.packages("ROCR")
library(ROCR)
install.packages("rvest")
library(rvest)
install.packages("ggplot2")
library(ggplot2)
install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("scales")
library(scales)
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)
install.packages("plotly")
library(plotly)
install.packages("digest")
library(digest)
install.packages("stringr")
library(stringr)
install.packages("sqldf")
library(sqldf)
install.packages("tree")
library(tree)
#--------------------------------#
  # PREPARATION DES DONNEES #
#Analyse exploratoire des données#
#--------------------------------#


#charger les fichiers
#direction
setwd("C:/Users/i.kergale/Documents/3a_S1/DataScience/Projet/DATA")

#création
catalogue <- read.csv("Catalogue.csv" , header = TRUE, sep = ",", dec = ".")
immatriculations <- read.csv("Immatriculations.csv" , header = TRUE, sep = ",", dec = ".")
marketing <- read.csv("Marketing.csv" , header = TRUE, sep = ",", dec = ".")
client <- read.csv("Clients_5.csv" , header = TRUE, sep = ",", dec = ".")

#Visualisation
str(catalogue)
str(immatriculations)
str(marketing)
str(client)

#-----------------#
#Catalogue à trier#
#-----------------#

names(catalogue)
attach(catalogue)
summary(catalogue)

t<-catalogue[occasion=="true",]
catalogue[occasion=="false",]

#filtres
catalogue<- filter( catalogue, marque!="?" & marque!="N/D" & marque!=" ")
catalogue<- filter( catalogue, nom!="?" & nom!="N/D" & nom!=" ")
catalogue<- filter( catalogue, puissance!="?" & puissance!="N/D" & puissance!=" ")
catalogue<- filter( catalogue, longueur!="?" & longueur!="N/D" & longueur!=" ")
catalogue<- filter( catalogue, nbPlaces!="?" & nbPlaces!="N/D" & nbPlaces!=" ")
catalogue<- filter( catalogue, nbPortes!="?" & nbPortes!="N/D" & nbPortes!=" ")
catalogue<- filter( catalogue, couleur!="?" & couleur!="N/D" & couleur!=" ")
catalogue<- filter( catalogue, occasion!="?" & occasion!="N/D" & occasion!=" ")
catalogue<- filter( catalogue, prix!="?" & prix!="N/D" & prix!=" ")

#RIEN A TRIER C'EST Un FICHIER PROPRE


#conversions des types

catalogue$puissance <- as.numeric(catalogue$puissance)
catalogue$longueur <- as.factor(catalogue$longueur)
catalogue$nbPlaces <- as.numeric(catalogue$nbPlaces)
catalogue$nbPortes <- as.numeric(catalogue$nbPortes)
catalogue$couleur <- as.factor(catalogue$couleur)
catalogue$occasion<- as.logical(catalogue$occasion)
catalogue$prix<- as.numeric(catalogue$prix)

#--------------#
#Client à trier#
#--------------#

attach(client)

#filtres qui remplace

#sexe
client<- filter( client, sexe!="?" & sexe!="N/D" & sexe!=" ")
#age
client<- filter( client, age!="?" & age!="N/D" & age!=" ")
#taux
client<- filter( client, taux!="?" & taux!="N/D" & taux!=" ")
#situationFamiliale
client<- filter( client, situationFamiliale!="?" & situationFamiliale!="N/D" & situationFamiliale!=" ")
#nbEnfantsACharge
client<- filter( client, nbEnfantsAcharge!="?" & nbEnfantsAcharge!="N/D" & nbEnfantsAcharge!=" ")
#2emeVoiture
client<- filter( client, X2eme.voiture!="?" & X2eme.voiture!="N/D" & X2eme.voiture!=" ")
#immatriculation
client<- filter( client, immatriculation!="?" & immatriculation!="N/D" & immatriculation!=" ")

#affichage pour vérifier si bien enlevées
client[client$sexe!="Masculin" & client$sexe!="M" &  client$sexe!="Féminin" &  client$sexe!="F" &  client$sexe!="Femme" &  client$sexe!="Homme" &  client$sexe!="?" &  client$sexe!="N/D",]
client[sexe=="?" &  sexe=="N/D" & sexe==" ",]
client[age=="?" &  age=="N/D" & age==" ",]

#remplacer les données coquilles dans sexe 

client$sexe <- str_replace(client$sexe, "Homme", "M")
client$sexe <- str_replace(client$sexe, "Masculin", "M")
client$sexe <- str_replace(client$sexe, "Féminin", "F")
client$sexe <- str_replace(client$sexe, "Femme", "F")

# les catégories existantes de situation familiale sont actuellement : Seul, Seule, Celibataire, Marié(e), En couple, Divorcée. 
#Nous allons remplacer tuos les "seul" et "seule" par celibataire --> NOOOOOOOOON finalement

#client$situationFamiliale <- str_replace(client$situationFamiliale, "Seul", "Célibataire")
#client$situationFamiliale <- str_replace(client$situationFamiliale, "Seule", "Célibataire")
#client$situationFamiliale <- str_replace(client$situationFamiliale, "Célibatairee", "Célibataire")
#vérifs
#client[client$situationFamiliale!="En Couple" & client$situationFamiliale!="Célibataire" & client$situationFamiliale!="Marié(e)" & client$situationFamiliale!="Divorcée",]
#client[client$situationFamiliale!="En Couple" & client$situationFamiliale!="Célibataire" & client$situationFamiliale!="Seule" & client$situationFamiliale!="Marié(e)" & client$situationFamiliale!="Seul" & client$situationFamiliale!="Divorcée",]

#les nb d'enfants sont ils anormaux?

client[client$nbEnfantsAcharge!="0" & client$nbEnfantsAcharge!="1" & client$nbEnfantsAcharge!="2" & client$nbEnfantsAcharge!="3" & client$nbEnfantsAcharge!="4",]

#il y a pas mal de client qui ont -1 enfants à charges donc on enlève ces lignes
client<- filter( client, nbEnfantsAcharge!="-1")

#vérification
client[nbEnfantsAcharge=="-1",]

#vérifions les taux :
client[client$taux<"0" ,]

#il y a pas mal de client qui ont -1 taux donc on enlève ces lignes
client<- filter( client, taux!="-1")

#non ne marche pas : 
#client <- subset(client, client$taux >=544)

client$taux <- as.integer(client$taux)

client <- filter(client, taux >= 544 )


summary(client)

#vérifions les ages :
client[client$age <"18" ,]
client[client$age >"84" ,]

#il y a pas mal de client qui ont -1 age donc on enlève ces lignes
client<- filter( client, age!="-1")

#vérifions 2eme voiture :
client[client$X2eme.voiture!="true" & client$X2eme.voiture!="false",]

#immatriculations :

#NA

na.omit(client)

#conversions des types

client$age <- as.numeric(client$age)
client$sexe <- as.factor(client$sexe)
client$taux <- as.numeric(client$taux)
client$situationFamiliale <- as.factor(client$situationFamiliale)
client$nbEnfantsAcharge <- as.numeric(client$nbEnfantsAcharge)
client$X2eme.voiture <- as.logical(client$X2eme.voiture)

#immatriculations déjà en chr


##on sait pas ça

# client[client$nbEnfantsAcharge!=integer(),]
# client[client$taux==integer(),]
# client[client$immatriculation!=chr(),]

#------------------------#
#Immatriculations à trier#
#------------------------#


immatriculations$nbPlaces <- as.numeric(immatriculations$nbPlaces)
immatriculations$nbPortes <- as.numeric(immatriculations$nbPortes)
immatriculations$couleur <- as.factor(immatriculations$couleur)
immatriculations$occasion<- as.logical(immatriculations$occasion)
immatriculations$prix<- as.numeric(immatriculations$prix)

#ATTENTION au format des immatriculations !
#A FAIRE

#-----------------#
#Marketing à trier#
#-----------------#

marketing$age <- as.numeric(marketing$age)
marketing$sexe <- as.factor(marketing$sexe)
marketing$taux <- as.numeric(marketing$taux)
marketing$situationFamiliale <- as.factor(marketing$situationFamiliale)
marketing$nbEnfantsAcharge <- as.numeric(marketing$nbEnfantsAcharge)
marketing$X2eme.voiture <- as.logical(marketing$X2eme.voiture)




#----------------------------------------------#
# lier les documents immatriculations et client#
#----------------------------------------------#

clientComplet <-merge(client, immatriculations, by="immatriculation")
#d'accord j'ai créé mon client Complet mais j'ai plus de ligne que de client 
#regardons le nombre de client qui ont 2 voitures:

client2voitures <- client[client$X2eme.voiture!="false",]

print(clientComplet)



#ATTENTION AUX DOUBLONS

#pour clients dans les immatriculations
doublons <- client[duplicated(client$immatriculation) =="TRUE",]
doublons
#18 doublons

#1er doublons
client[client$immatriculation == "1557 AB 48",]
#la même immat appartient à 2 personnes totalement différente
#voyons si dans immatriculations elle est double
immatriculations[immatriculations$immatriculation=="1557 AB 48",]

#elle correspond à 2 voitures différentes
#ce qui est un problème car après la liaison dans client complet : 
clientComplet[clientComplet$immatriculation=="1557 AB 48",]
# 1 immat créer 4 lignes dans client complet

#ce qui fait que les 18 doublons d'immat dans clients crééent 18*4 lignes =72 lignes en plus dans client Complet

#ON SUPPRIME LES DOUBLONS DANS CLIENT
client <- client[duplicated(client$immatriculation) =="FALSE",]
client_sans_doublons <- client[duplicated(client$immatriculation) =="FALSE",]

#doublons dans immatriculations:
doublons_immat <- immatriculations[duplicated(immatriculations$immatriculation)=="TRUE",]
doublons_immat

#on les enlève du coup :

immatriculations <- immatriculations[duplicated(immatriculations$immatriculation)=="FALSE",]
immatriculations_sans_doublons <- immatriculations[duplicated(immatriculations$immatriculation) =="FALSE",]

#on relie les 2 tables 
clientComplet <-merge(client, immatriculations, by="immatriculation")

#doublons dans clientComplet
clientComplet[duplicated(clientComplet$immatriculation)=="TRUE",]
#AUCUN DOUBLONS DANS CLIENT COMPLET

#----------#
#CATEGORIES#
#----------#

#nuage de point catégories de voiture

qplot(longueur, puissance, data= catalogue)

qplot(longueur, nbPlaces, data=catalogue)

qplot(longueur, prix, data=catalogue)

qplot(nbPlaces, prix, data=catalogue)

qplot(nbPlaces, nbPortes, data=catalogue)

qplot(longueur, nbPortes, data=catalogue) 

qplot(puissance, prix, data=catalogue, color=longueur)

qplot(nbPlaces, puissance, data=catalogue)

qplot(nom, prix, data=catalogue, color=longueur)


#je pense à 3 ou 4 catégories : citadines, familiale/simple, grande famille, sport

#doublons catalogue:
catalogue[duplicated(catalogue)=="TRUE",]
#aucuns

#PREMIER TEST
#citadines : les courtes
#familiale/simple : moyennes/ longue
#grande famille : 7 places
#sport : + de 200chv

# citadines <- catalogue[catalogue$longueur=="courte",]
# grandeFamille <- catalogue[catalogue$nbPlaces==7,]
# sport <- catalogue[catalogue$puissance >= 200 & catalogue$longueur=="très longue",]

modele_berline <- normale_familiale[duplicated(normale_familiale$nom)=="FALSE",]

qplot(longueur, data=modele_berline)

#DEUXIEME TEST
#critères : 
#citadines : les courtes
#sport : +de 300cv
#berline compact : moyennes
#berline : des longues mais pas de 7places
#berline confort : très longue mais supérieur à 190 et inférieur à 300

citadines <- catalogue[catalogue$longueur=="courte",]
grandeFamille <- catalogue[catalogue$nbPlaces==7,]
sport <- catalogue[catalogue$puissance >300,]
berline_confort <- catalogue[catalogue$longueur=="très longue" & catalogue$puissance> 190 & catalogue$puissance <300,]
berline_compact <- catalogue[catalogue$longueur=="moyenne",]
berline <- catalogue[catalogue$longueur=="longue" & catalogue$nbPlaces!=7,]
 

#faire des critères très précis pour pouvoir créer immatriculations$categorie

##ATTENTION il y a des incohérences --> par exemple : new beetle c'est pas 5 places mais 4 donc pas dans la bonne catégorie car on ne peut pas la conseiller à des familles (trop serrées à l'arrière), donc irait plus dans la catégorie citadine


#attribuer les catégories aux donénes de Immatriculations

#PREMIER TEST :
immatriculations$categorie <- ifelse(immatriculations$longueur =="courte", immatriculations$categorie <- "citadine", 
                                     ifelse(immatriculations$nbPlaces ==7, immatriculation$categorie <- "monospace", 
                                            ifelse(immatriculations$puissance >= 200 & immatriculations$longueur=="très longue", immatriculations$categorie <- "sport", immatriculations$categorie <- "normale_familiale")))

#DEUXIEME TEST :
immatriculations$categorie <- ifelse(immatriculations$longueur =="courte", immatriculations$categorie <- "citadine", 
                                     ifelse(immatriculations$nbPlaces ==7, immatriculation$categorie <- "monospace", 
                                            ifelse(immatriculations$puissance > 300, immatriculations$categorie <- "sport", 
                                                   ifelse(immatriculations$longueur =="très longue" & catalogue$puissance> 190 & catalogue$puissance <300, immatriculations$categorie <- "berline_confort", 
                                                          ifelse(immatriculations$longueur=="moyenne", immatriculations$categorie <- "berline_compact", immatriculations$categorie<- "berline")))))

#attention WARNINGs

#je me rend compte du coup qu'il n'y a aucune voiture de places, devons nous supposer que les familles de 4 enfants choisiront automatiquement un monospace?
immatriculations[immatriculations$nbPlaces == 7]
summary(immatriculations)

#on RE RELIE LES TABLES CLIENT ET IMMATRICULATIONS
clientComplet <-merge(client, immatriculations, by="immatriculation")
summary(clientComplet)


#suppression de la colonne immatriculation car pas utile 
#également le nbPlaces

clientComplet <- subset(clientComplet, select= -immatriculation)
clientComplet <- subset(clientComplet, select= -nbPlaces)

#et les autres? peut être garder que les colonnes qui correspondent à marketing

#création des ensembles d'apprentissage et de test:


##ATTENTION LE NOMBRE DS CLIENT COMPLET A CHANGE, A RECALCULER 2/3, 1/3

#2/3
client_EA <- clientComplet[1:65484,]

#pour voir si le nombre de pondération dépend de ça
client_EA_petit <- clientComplet[1:50000,]

#1/3
client_ET <- clientComplet[65485:98225,]

#------------#
#CLASSIFIEURS#
#------------#

#Suppression des variables inutiles

client_EA <- subset(client_EA, select = -nbPortes)
client_EA <- subset(client_EA, select = -longueur)
client_EA <- subset(client_EA, select = -puissance)
client_EA <- subset(client_EA, select = -marque)
client_EA <- subset(client_EA, select = -nom)
client_EA <- subset(client_EA, select = -couleur)
client_EA <- subset(client_EA, select = -occasion)
client_EA <- subset(client_EA, select = -prix)


client_ET <- subset(client_ET, select = -nbPortes)
client_ET <- subset(client_ET, select = -longueur)
client_ET <- subset(client_ET, select = -puissance)
client_ET <- subset(client_ET, select = -marque)
client_ET <- subset(client_ET, select = -nom)
client_ET <- subset(client_ET, select = -couleur)
client_ET <- subset(client_ET, select = -occasion)
client_ET <- subset(client_ET, select = -prix)



#-------------#
# NAIVE BAYES #
#-------------#

# Apprentissage du classifeur de type naive bayes

nb <- naive_bayes(client_EA$categorie~., client_EA)
nb

#Warning messages:
#  1: naive_bayes(): Feature age - zero probabilities are present. Consider Laplace smoothing. 
#2: naive_bayes(): Feature taux - zero probabilities are present. Consider Laplace smoothing. 
#3: naive_bayes(): Feature situationFamiliale - zero probabilities are present. Consider Laplace smoothing. 
#4: naive_bayes(): Feature nbEnfantsAcharge - zero probabilities are present. Consider Laplace smoothing. 
#5: naive_bayes(): Feature X2eme.voiture - zero probabilities are present. Consider Laplace smoothing. 

# Test du classifieur : classe predite

nb_class <- predict(nb, client_ET, type="class")
table(nb_class)

# Matrice de confusion
table( client_ET$categorie, nb_class)

# Test du classifieur : probabilites pour chaque prediction
nb_prob <- predict(nb, client_ET, type="prob")
nb_prob


#-------------#
# C5.0        #
#-------------#

client_EA$categorie <- as.factor(client_EA$categorie)

# Apprentissage du classifeur de type arbre de décision
tree_C50 <- C5.0(client_EA$categorie~., client_EA)
tree_C50
#-----------------#
# NEURAL NETWORKS #
#-----------------#


# Apprentissage du classifeur de type perceptron monocouche


classifieur_nn <- nnet(categorie~age + sexe +taux+ situationFamiliale+nbEnfantsAcharge+X2eme.voiture, client_EA_petit, size=6)
#Error in nnet.default(x, y, w, ...) : trop (6457) de pondérations



#---------------------#
# K-NEAREST NEIGHBORS #
#---------------------#

# Apprentissage et test simultanes du classifeur de type k-nearest neighbors
classifieur_knn <- kknn(categorie~., client_EA, client_ET)
# Error in if (response != "continuous") { : 
#     l'argument est de longueur nulle
classifieur_knn <- kknn(categorie~age + sexe +taux+ situationFamiliale+nbEnfantsAcharge+X2eme.voiture, client_EA, client_ET)
# Error in if (response != "continuous") { : 
#     l'argument est de longueur nulle

#------#
#R-PART#
#------#

classifieur_rpart <-rpart(categorie ~.,client_EA)

#erreur car ça fait planter


#----#
#TREE#
#----#
classifieur_tree <-tree(categorie~age + sexe +taux+nbEnfantsAcharge+X2eme.voiture,client_EA)


#erreur


#RANDOM FOREST#

classifieur_rf <- randomForest(categorie~., client_EA)

# Error in y - ymean : argument non numérique pour un opérateur binaire
# De plus : Warning messages:
#   1: In randomForest.default(m, y, ...) :
#   The response has five or fewer unique values.  Are you sure you want to do regression?
#   2: In mean.default(y) : argument is not numeric or logical: returning NA








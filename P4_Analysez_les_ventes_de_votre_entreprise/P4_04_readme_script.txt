Projet 4 : Analyser les ventes 	de votre entreprises

Dans ce projet, j'ai analysé les transactions et les clients d'une chaîne de librairie. J'ai eu accès à 3 fichiers qui détaillent les données sur les clients, les transactions et les produits.
A travers ce projet, j'ai donc appris à nettoyer les données ainsi qu'à les analyser à travers des graphiques et des méthodes d'analyse de corrélation. 

Mon rendu est composé de 2 scripts:
- P4_01_script_nettoyage : le script de nettoyage des données
- P4_02_script_analyse : le script d'analyse des données


1) P4_01_script_nettoyage
Ce script a pour fonction de nettoyer les fichiers sources de données et de les fusionner.
Libraries importés
- os 
- pandas
- matplotlib.pyplot
- seaborn
- numpy
- datetime
- scipy.stats

Je convertis d'abord les fichiers csv suivant en dataframes
- customers.csv
- transactions.csv
- products.csv

Par la suite, j'applique différents méthodes pour nettoyer les données :
- customers : Détection et suppressions des valeurs nulles / non-connues + conversion variable "birth" en format date + création variables quantitative et qualitative age
- transactions : Détection et suppressions des valeurs nulles / non-connues + conversion variable "categ" en format str
- products : Détection et suppressions des valeurs nulles / non-connues + Elimination des observations test (id_prod = “T_0”) + Conversion de la variable “Date” en format date et déclinaison en 4 formats (année,mois,jour,heure) + Discrétisations variable heure
 
Enfin, j'ai appliqué des jointures internes sur 3 fichiers sous le fichier data_livres que j'ai converti par la suite en csv

2) P4_02_script_analyse
Dans ce script, j'ai effectué l'analyse à travers le fichier data_livres qui comprend l'ensemble des données des 3 fichiers nettoyés.
Libraries importés
- os 
- pandas
- matplotlib.pyplot
- seaborn
- numpy
- datetime
- scipy.stats

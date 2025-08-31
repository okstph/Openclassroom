library("FactoMineR")
library("factoextra")
library("tidyverse")
library("funModeling")
library("Hmisc")
library("dplyr")
library("reshape2")
library("data.table")
library("corrplot")


#chargement de l'environnement de travail
setwd("/Users/OK/Desktop/Cours numérique/Cours Python/Parcours Data analyst Openclassrooms/Projet 5/R Script")


#chargement des datasets à partir des fichiers csv du site faostat
data <- read.csv("pop_volaille.csv", header = FALSE)
data_pop5 <- read.csv("evol_pop_5y.csv", header = FALSE)
data_volal <- read.csv("volaille_imp_food.csv",header = FALSE)
data_imp <- read.csv("imp_ttl.csv", header = TRUE)

#eliminations colonnes superflues dans data et evol_pop
data <- data[-c(1,2,3,4),]
data_volal <- data_volal[-c(1,2,3,4),]

#elimination ligne China Mainland
data <- subset(data,V1 != "China")
data_pop5 <- subset(data_pop5,V1 != "China")
data_volal <- subset(data_volal,V1 != "China")
data_imp <- subset(data_imp,Area != "China")

#nettoyage de data_pop
#éviter l'écriture scientifique et afficher avec 3 décimales seulement
options(digits=3)
options(scipen=999)

#renommage des colonnes
data <- data %>%
  rename(
    Area = V1,
    Disp_kcal_anim = V2,
    Disp_prot_anim = V3,
    Disp_kcal_ttl = V4,
    Disp_prot_ttl = V5,
  )

data_volal <- data_volal %>%
  rename(
    Area = V1,
    Disp_kcal_volal = V2,
    Import_volal = V3,

  )

data_pop5 <- data_pop5 %>%
  rename(
    Area = V1,
    Pop_2013 = V2,
    Pop_2018 = V3,
  )


#conversion des colonnes
data$Disp_kcal_anim <- as.numeric(as.character(data$Disp_kcal_anim))
data$Disp_prot_anim <- as.numeric(as.character(data$Disp_prot_anim))
data$Disp_kcal_ttl <- as.numeric(as.character(data$Disp_kcal_ttl))
data$Disp_prot_ttl <- as.numeric(as.character(data$Disp_prot_ttl))
data_volal$Disp_kcal_volal <- as.numeric(as.character(data_volal$Disp_kcal_volal))
data_volal$Import_volal <- as.numeric(as.character(data_volal$Import_volal))
data_imp$Import_1kt <- as.numeric(as.character(data_imp$Import_1kt))
data_pop5$Pop_2013 <- as.numeric(as.character(data_pop5$Pop_2013))
data_pop5$Pop_2018 <- as.numeric(as.character(data_pop5$Pop_2018))


write.csv(data_pib,"/Users/ACER/Desktop/Cours numerique/Cours Python/Parcours Data analyst Openclassrooms/Projet 5/R script/data_pib2.csv")


#ajout de la variable évol_pop13-18 qui mesure l'évolution de la pop entre 2018 et 2013
data_pop5 <- mutate(data_pop5, evol_pop18_13 = (((data_pop5$Pop_2018/data_pop5$Pop_2013)^(1/5) - 1)))
data_pop5$evol_pop18_13 <- as.numeric(as.character(data_pop5$evol_pop18_13))
data_pop5 <- data_pop5[-c(1),]


#ajout de la variable prop_prot_anim qui mesure la prop de nourriture anim dans la dispo protéine total du pays
data <- mutate(data, prop_prot_anim = ((data$Disp_prot_anim)/(data$Disp_prot_ttl)))


#jointure gauche sur data en prenant comme critère "Area"
data_fin <- merge(x = data, y = data_pop5, by = "Area",all.x = TRUE)
data_fin <- merge(x = data_fin, y = data_volal, by = "Area", all.x = TRUE)
data_fin <- merge(x = data_fin, y = data_imp, by = "Area", all.x = TRUE)
data_fin <- data_fin[-c(1,2,3,4),]
data_fin <- data_fin[,-c(7,8)]


#ajout de la variable volal_prop qui mesure la proportion de volaille dans la disp aliment en kcal 
data_fin <- mutate(data_fin, volal_prop = ((data_fin$Disp_kcal_volal/data_fin$Disp_kcal_ttl)))
#ajout de la variable anim_prop qui mesure la proportion de viande animale dans la disp aliment en kcal 
data_fin <- mutate(data_fin, anim_prop = ((data_fin$Disp_kcal_anim/data_fin$Disp_kcal_ttl)))
data_fin <- data_fin[,-c(9,10)]
head(data_fin)

#elimination des pays avec NA (Cote Ivoire : pas de données d'importation)
data_fin <- data_fin[complete.cases(data_fin),]


 #clustering
#normalize data_fin
data_clut <- data_fin[,-c(1)]
means <- apply(data_clut,2,mean)
sds <- apply(data_clut,2,sd)
nor <- scale(data_clut,center=means,scale=sds)

#intégrer nom des pays en index
rownames(data_clut) <- data_fin$Area
head(data_clut)

#calculate distance of nor
distance <- dist(nor)

  #classification hierarchique avec complete linkage
data_clut.hclust <- hclust(distance, method = "ward.D")
plot(data_clut.hclust,labels=data_fin$Area,main='Default from hclust',hang = -1)


#analyse membre de chaque cluster
membre <- cutree(data_clut.hclust,5)
table(membre)

#caractérisation cluster
aggregate(nor,list(membre),mean)
#caractérisation cluster avec centroides
carac_cluster <- aggregate(data_clut,list(membre),mean)
#enregistrement centroid
carac_cluster <- format(carac_cluster,digits=2,nsmall=2)
write.csv(carac_cluster,"/Users/ACER/Desktop/Cours numerique/Cours Python/Parcours Data analyst Openclassrooms/Projet 5/R Script/P5_04_centroides.csv")

#Ici, on peut distinguer donc 5 groupes
#1 : Groupe avec faible dispo alimentaire (Haiti, Liberia) + forte croiss démo
#2 : Groupe avec la plus forte dispo alimentaire (France, Pays Européen) et forte prop aliment animale
#3 : Groupe avec la plus forte croiss. démographique moy. sur 5 ans / dispo aliment moyenne
#4 : Groupe avec la plus forte prop. aliment. de volailes / dispo. alimentaire moy. / croiss demo moy.
#5 : Groupe avec forte dispo. alim. / forte vol. prop.  / forte croiss. démo. 

#conversion membre en dataframe
membre <- as.data.frame(membre)
membre <- cbind(Area = rownames(membre),membre)
rownames(membre) <- 1:nrow(membre)


#intégrer nom des pays en index
rownames(membre) <- data_fin$Area

#integration data pays avec cluster
#data_d <- list(data = data_clut,cluster=membre)
data_cluster<- merge(data_clut,membre,by=0)
row.names(data_cluster) <- data_cluster$Row.names 
data_cluster <- data_cluster[,-c(1,11)]
head(data_cluster)

#renommage des clusters de pays
data_cluster[data_cluster$membre == 1,]$membre<-"Pays pauvres"
data_cluster[data_cluster$membre == 2,]$membre<-"Pays Occidentaux"
data_cluster[data_cluster$membre == 3,]$membre<-"Pays en developpement "
data_cluster[data_cluster$membre == 4,]$membre<-"Pays avec forte consommation volaille"
data_cluster[data_cluster$membre == 5,]$membre<-"Pays avec forte croiss. demo."
data_cluster$membre<-as.factor(data_cluster$membre)


carac_cluster <- aggregate(data_cluster,list(membre),mean)


centroide<-group_by(data_cluster, membre)%>%
  summarise_if(is.numeric, function(x) round(mean(x),1))

write.csv(centroide,"/Users/OK/Desktop/Cours numérique/Cours Python/Parcours Data analyst Openclassrooms/Projet 5/R Script/P5_02_clusters.csv")

#enregistrement des pays selon le clusters 
pays_cluster <- select(data_cluster,membre)
write.csv(pays_cluster,file = "/Users/OK/Desktop/Cours numérique/Cours Python/Parcours Data analyst Openclassrooms/Projet 5/R Script/P5_02_clusters.csv")
?write.csv

#data.pr <- prcomp(data_cluster,center = TRUE, scale = TRUE)
data.pr <- PCA(data_clut,graph = FALSE)
summary(data.pr)

#scree plots
fviz_eig(data.pr,addlabels = TRUE, ylim = c(0,80))

#cercle de corrélation individus
#data_cluster$membre <- as.numeric(data_cluster$membre)
fviz_pca_ind(data.pr,var="black",addEllipses = TRUE,col.ind = as.character(data_cluster$membre))
str(data)


#cercle de corrélation variables
fviz_pca_var(data.pr, var="black")


#analyse relation dimensions-variables
data_var <- get_pca_var(data.pr)

head(data_var$cos2,5)
corrplot(data_var$cos2,is.corr=FALSE)


#analyse relation dimensions-indvidus
data_ind <- get_pca_ind(data.pr)


#projection cluster sur le cercle de corrélation des individus

#Test de moyenne de Shapiro sur Disp_kcal_ttl avec un risque alpha de 95%
shapiro.test(data_cluster$Disp_kcal_anim)
#p-value = 0.000008 -> on rejette l'hypothèse H0 de distribution normale
shapiro.test(data_cluster$Disp_prot_anim)
#p-value = 0.00003 -> on rejette l'hypothèse H0 de distribution normale
shapiro.test(data_cluster$Disp_prot_ttl)
#p-value = 0.03 -> on rejette l'hypothèse H0 de distribution normale
shapiro.test(data_cluster$Disp_kcal_ttl)
#p-value = 0.08 -> on accepte l'hypothèse H0 de distribution normale pour la variable Disp_kcal_ttl
shapiro.test(data_cluster$Disp_kcal_volal)
#p-value = 0.000000006 -> on rejette l'hypothèse H0 de distribution normale
shapiro.test(data_cluster$evol_pop18_13)
#p-value = 0.04 -> on rejette l'hypothèse H0 de distribution normale
shapiro.test(data_cluster$prop_prot_anim)
#p-value = 0.00002 -> on rejette l'hypothèse H0 de distribution normale
shapiro.test(data_cluster$anim_prop)
#p-value = 0.00002 -> on rejette l'hypothèse H0 de distribution normale
shapiro.test(data_cluster$volal_prop)
#p-value = 0.0000000008 -> on rejette l'hypothèse H0 de distribution normale

#Avec une p-value > 0.05, La variable Disp_kcal_ttl suit donc une loi normale
hist(data_cluster$Disp_kcal_ttl)


#extraction des pays des groupes dans 5 tables différentes
data_c_1 <- data_cluster %>% filter(membre == "Pays pauvres")%>%select(-"membre")
data_c_2 <- data_cluster %>% filter(membre == "Pays Occidentaux")%>%select(-"membre")
data_c_3 <- data_cluster %>% filter(membre == "Pays en developpement ")%>%select(-"membre")
data_c_4 <- data_cluster %>% filter(membre == "Pays avec forte consommation volaille")%>%select(-"membre")
data_c_5 <- data_cluster %>% filter(membre == "Pays avec forte croiss. demo.")%>%select(-"membre")

#sur tous les cluster choisis, check différence moyenne / variance
#test de normalité  
shapiro.test(data_c_1$Disp_kcal_ttl)
#p-value = 0.6
shapiro.test(data_c_2$Disp_kcal_ttl)
#p-value = 0.3
shapiro.test(data_c_3$Disp_kcal_ttl)
#p-value = 0.3
shapiro.test(data_c_4$Disp_kcal_ttl)
#p-value = 0.5
shapiro.test(data_c_5$Disp_kcal_ttl)
#p-value = O.9

#choix cluster : doit être justifier par une analyse de cercle de correlation 

data_1 <- PCA(data_c_1, graph = FALSE)
fviz_pca_ind(data_1,var="black",addEllipses = TRUE)
data_2 <- PCA(data_c_2, graph = FALSE)
fviz_pca_ind(data_2,var="black",addEllipses = TRUE)
data_3 <- PCA(data_c_3, graph = FALSE)
fviz_pca_ind(data_3,var="black",addEllipses = TRUE)
data_4 <- PCA(data_c_4, graph = FALSE)
fviz_pca_ind(data_4,var="black",addEllipses = TRUE)
data_5 <- PCA(data_c_5, graph = FALSE)
fviz_pca_ind(data_5,var="black",addEllipses = TRUE)

#intégrer les variables PIB ou d'autres variables pour remplacer les autres variables existantes
#revoir les outils d'analyse

#choix des 2 cluster Pays occident et Pays avec consommation volaille
#test de fisher/student(nécessite check normalisation) avec un risque alpha de 95%
var.test(data_c_2$Disp_kcal_ttl,data_c_4$Disp_kcal_ttl)
#p-value = 0.21
#On rejette l'hypothèse d'égalité des variance entre les 3 groupes de pays
#les 2 clusters sont bien distincts

#Liste des pays les plus intéressants au moins intéressants du point de vue économique
data_ind2<-data_2[["ind"]][["coord"]]%>%as.data.frame()
data_ind2$Area<-rownames(data_ind2)
data_ind2<-data_ind2%>%select(Dim.1)%>%arrange(desc(Dim.1))
data_ind2


data_ind4<-data_4[["ind"]][["coord"]]%>%as.data.frame()
data_ind4$Area<-rownames(data_ind4)
data_ind4<-data_ind4%>%select(Dim.1)%>%arrange(desc(Dim.1))
data_ind4

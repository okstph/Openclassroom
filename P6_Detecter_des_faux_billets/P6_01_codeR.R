library("tidyverse")
library("FactoMineR")
library("factoextra")
library("lattice")
library("caret")
library("dslabs")
library("dplyr")
library("ggplot2")
library("corrplot")
library("GGally")
library("gridExtra")

setwd("/Users/OK/Desktop/Cours num/Cours Python/Parcours Data analyst Openclassrooms/Projet 6/R_Script")

#intégration fichiers csv
data_billets <- read.csv("notes.csv")
typeof(data_billets)

#check valeurs manquantes
sapply(data_billets, function(x) sum(is.na(x)))

#statistiques descriptives
summary(data_billets)
table(data_billets$is_genuine)

#histogram
ggplot(data_billets,aes(x=diagonal)) + geom_histogram(color="black", fill = "white") + geom_density()
ggplot(data_billets,aes(x=height_left)) + geom_histogram(color="black", fill = "white") + geom_density()
ggplot(data_billets,aes(x=height_right)) + geom_histogram(color="black", fill = "white") + geom_density()
ggplot(data_billets,aes(x=margin_low)) + geom_histogram(color="black", fill = "white") + geom_density()
ggplot(data_billets,aes(x=margin_up)) + geom_histogram(color="black", fill = "white") + geom_density()
ggplot(data_billets,aes(x=length)) + geom_histogram(color="black", fill = "white") + geom_density()

#histogram avec distinction par billets vrais et faux
ggplot(data_billets,aes(x=diagonal,fill=is_genuine)) + geom_histogram(color="black", fill = "white") + geom_density()
ggplot(data_billets,aes(x=height_left,fill=is_genuine)) + geom_histogram(color="black", fill = "white") + geom_density()
ggplot(data_billets,aes(x=height_right,fill=is_genuine)) + geom_histogram(color="black", fill = "white") + geom_density()
ggplot(data_billets,aes(x=margin_low,fill=is_genuine)) + geom_histogram(color="black", fill = "white") + geom_density()
ggplot(data_billets,aes(x=margin_up,fill=is_genuine)) + geom_histogram(color="black", fill = "white") + geom_density()
ggplot(data_billets,aes(x=length,fill=is_genuine)) + geom_histogram(color="black", fill = "white") + geom_density()


#test de loi normales
shapiro.test(data_billets$diagonal)
shapiro.test(data_billets$height_left)
shapiro.test(data_billets$height_right)
shapiro.test(data_billets$margin_low)
shapiro.test(data_billets$margin_up)
shapiro.test(data_billets$length)
#distribution bimodale = nature des billets -> mettre de la couleur vrai-faux billets

#valeurs aberrantes des diagonales
val_ab_dia_idx <- boxplot.stats(data_billets$diagonal)$out
val_ab_dia_idx_ <- which(data_billets$diagonal %in% c(val_ab_dia_idx))
val_ab_dia_idx_
#index valeurs aberrantes 71/167

#valeurs aberrantes de height_left
val_ab_dia_hl <- boxplot.stats(data_billets$height_left)$out
val_ab_dia_hl_ <- which(data_billets$height_left %in% c(val_ab_dia_hl))
val_ab_dia_hl_

#valeurs aberrantes de height_right
val_ab_dia_hr <- boxplot.stats(data_billets$height_right)$out
val_ab_dia_hr_ <- which(data_billets$height_right %in% c(val_ab_dia_hr))
val_ab_dia_hr_
#index valeurs aberrantes 1


#valeurs aberrantes de margin_low
val_ab_dia_ml <- boxplot.stats(data_billets$margin_low)$out
val_ab_dia_ml_ <- which(data_billets$margin_low %in% c(val_ab_dia_ml))
val_ab_dia_ml_

#valeurs aberrantes de margin_up
val_ab_dia_mu <- boxplot.stats(data_billets$margin_up)$out
val_ab_dia_mu_ <- which(data_billets$margin_up %in% c(val_ab_dia_mu))
val_ab_dia_mu_
#index valeurs aberrantes 5

#valeurs aberrantes de length
val_ab_dia_lt <- boxplot.stats(data_billets$length)$out
val_ab_dia_lt_ <- which(data_billets$length %in% c(val_ab_dia_lt))
val_ab_dia_lt_
#du fait du nombre limité de valeurs aberrantes (4), j'ai choisi de ne pas les supprimer


#boxplot de length
ggplot(data = data_billets,aes(x = is_genuine, y = length)) + geom_boxplot(outlier.colour="black",outlier.shape=16,outlier.size=2,notch=FALSE)

#boxplot de diagonal
ggplot(data = data_billets,aes(x = is_genuine, y = diagonal)) + geom_boxplot(outlier.colour="black",outlier.shape=16,outlier.size=2,notch=FALSE)
#les diagonales des billets faux tendent à être légèrement inférieur aux billets vrais

#boxplot de height_left
ggplot(data = data_billets,aes(x = is_genuine, y = height_left)) + geom_boxplot(outlier.colour="black",outlier.shape=16,outlier.size=2,notch=FALSE)
#les côtés gauches des billets faux tendent à etre supérieure aux billets vrais avec une moyenne de 104,2 vs 104

#boxplot de height_right
ggplot(data = data_billets,aes(x = is_genuine, y = height_right)) + geom_boxplot(outlier.colour="black",outlier.shape=16,outlier.size=2,notch=FALSE)
#les côtés droits desbillets faux tendent à etre supérieure aux billets vrais avec une moyenne de 104,2 vs 103,8

#boxplot de margin_up
ggplot(data = data_billets,aes(x = is_genuine, y = margin_up)) + geom_boxplot(outlier.colour="black",outlier.shape=16,outlier.size=2,notch=FALSE)

#boxplot de margin_low
ggplot(data = data_billets,aes(x = is_genuine, y = margin_low)) + geom_boxplot(outlier.colour="black",outlier.shape=16,outlier.size=2,notch=FALSE)


#en observant l'histogram + boxplots, on observe que les billets faux se distinguent des vrais globalement par 
#une hauteur et une marge plus élevée
#une largeure plus faible


#corrplot
data_billets_ <- select(data_billets,-is_genuine)
data_billets_ <- cor(data_billets_)
corrplot(data_billets_, method = "color")

#Standardisation + Analyse ACP ()
data_billets_num <- select(data_billets,-is_genuine)
data_billets_num <- scale(data_billets_num, center = TRUE,scale = TRUE)
data_billets_acp <- PCA(data_billets_num,graph =FALSE)
#height_left fortement corrélé à height_right a été enlevé (cf.corrplot + cercles des variables)


#éboulis des valeurs propres
fviz_eig(data_billets_acp,addlabels = TRUE, ylim = c(0,80))


#cercle des corrélations des variables / reprendre toutes les variables
fviz_pca_var(data_billets_acp, var="black")

#cercle des corrélations des individus/billets
#colorer les billets vrais - faux
graph_true <- fviz_pca_ind(data_billets_acp, col.ind = data_billets$is_genuine,addEllipses = TRUE)
graph_true

#contribution des billets
fviz_contrib(data_billets_acp, choice="ind",fill.ind = data_billets$is_genuine, axes = 1 )

#enregistrer sous dataframe
db_acp_cluster <- as.data.frame(data_billets_acp$cluster)
names(db_acp_cluster)[1] <- "cluster"
db_acp_cluster$class <- as.factor(ifelse(db_acp_cluster=='1',"True","False"))



#classification K-means avec nombre de cluster = 2
data_billets_km <- kmeans(data_billets_num,2, nstart = 25)
fviz_cluster(data_billets_km, data = data_billets_num, geom = "point")


#enregistrement des cluster crées sous kmeans
db_kmeans_cluster <- as.data.frame(data_billets_km$cluster)
names(db_kmeans_cluster)[1] <- "class"
db_kmeans_cluster$class <- as.factor(ifelse(db_kmeans_cluster=='1',"True","False"))


#visualisation de la classification sur le plan factoriel de l'ACP
graph_km <- fviz_pca_ind(data_billets_acp, col.ind = db_kmeans_cluster$class,addEllipses = TRUE)
graph_km
#en reprenant le plan factoriel de l'ACP, la classification kmeans obtenue montre des clusters plus étroits vs la distribution réel des vrais/faux billets
grid.arrange(graph_true,graph_km,nrow=1)

#matrice de confusion sur la classification kmeans
confusionMatrix(table(data_billets$is_genuine,db_kmeans_cluster$class))
#sur la matrice de confusion, on distingue 0 faux positifs et 2 faux négatifs ???
#la précision du modèle est de 0,988 avec une p-value inférieure à 0.05 ???

#regression logistique 
#création d'une partition d'entrainement + test
table(data_billets$is_genuine)
trainIndex <- createDataPartition(data_billets$is_genuine,p=0.8,list = F)

billetsTrains <- data_billets[trainIndex,]
billetsTest <- data_billets[-trainIndex,]

#définition paramètre processus d'apprentissage
fitControl <- trainControl(method="none")

#apprentissage via régression logistique
billetsTrains_logit <- train(is_genuine ~ .,data = billetsTrains,method="glm",trControl=fitControl)


#application du modèle sur la partition test pour prediction
pred_billets <- predict(billetsTrains_logit,newdata = billetsTest)

#matrice de confusion pour calculer l
pred_bil_qual <- confusionMatrix(table(data = pred_billets, reference = billetsTest$is_genuine))
pred_bil_qual
#taux de succès de 0.971 avec une pvalue < 95%
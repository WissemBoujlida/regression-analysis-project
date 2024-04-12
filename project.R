# Tâche 1 : Importation des données :
data = read.csv(file = "C:\\Users\\Wissem\\Desktop\\R\\project\\Raisin.csv", 
                header = TRUE, sep = ',', dec = '.', na.strings = c("", NA))

View(data)
dim(data)
columns = c(names(data))
columns

# Tâche 2 : Pré-traitement des données :
# 1. Valeurs aberrantes :

# détecter les valeurs aberantes pour chaque colonne quantitative 
# en utilisant des boxplots

boxplot(data$Area, xlab = "Area")
boxplot(data$MajorAxisLength, xlab = "MajorAxisLength")
boxplot(data$MinorAxisLength, xlab = "MinorAxisLength")
boxplot(data$Eccentricity, xlab = "Eccentricity")
boxplot(data$ConvexArea, xlab = "ConvexArea")
boxplot(data$Extent, xlab = "Extent")
boxplot(data$Perimeter, xlab = "Perimeter")

# on remarque l'existence des valeurs aberrantes dans la plupart des colonnes.
# nous reviendrons plus tard pour les traiter.

# 2. Valeurs manquantes :

# Etudier le taux des valeurs manquantes dans chaque colonne :

cat("colonne","\tle taux des valeurs manquantes")
for (column in columns) {
  index_na = which(is.na(data[[column]]))
  cat (column,length(index_na)/825*100,"%\n")
}

# On constate que les valeurs manquantes existent principalement 
# dans les colonnes Eccentricity et Class.

# L’imputation des valeurs manquantes et des valeurs aberrantes :

# Commençons tout d'abord par remplacer les valeurs aberrantes par NA

numeric_columns = columns[! columns %in% c("Class")]

for (column in numeric_columns) {
  outliers = boxplot.stats(data[[column]])$out
  data[[column]][data[[column]] %in% outliers] = NA
}

# Finalement on va imputer les valeurs manquantes 
# et les valeurs aberrantes (remplacées par NA) en utilisant l'imputation KNN
# l'imputation KNN remplace les valeurs manquantes par la valeur moyenne des valeurs prises par les k voisins
# les plus proches. Cela va nous permettre d'approcher au mieux la valeur manquante et de consérver la cohérence des données.

# N'oublier pas de charger le package VIM
data = kNN(data, imp_var = FALSE)
View(data)

# Tâche 3 : Analyse univariée:

# Test de normalité pour chaqu’une des variables quantitatives.

shapiro.test(data$Area)
shapiro.test(data$MajorAxisLength)
shapiro.test(data$MinorAxisLength)
shapiro.test(data$Eccentricity)
shapiro.test(data$ConvexArea)
shapiro.test(data$Perimeter)

# D'après le test de shapiro, aucune variable quantitative 
# n'est distribuée normalement.

# Analyse des modalités de la variable qualitative Class
# N'oublier pas de charger le package ggplot2

ggplot(data=data,aes(x=Class,fill=Class))+geom_bar()


# Tâche 4 : Analyse bivariée :

# Etude de la corrélation entre les variables quantitatives deux à deux :

# Tracer deux variables quantitatives l'une en foction de l'autre
# peut nous montrer si'il existe une corrélation entre ces deux variables ou non.
# Mais on peut pas faire confiance à l'observation empirique de cette corrélation.
# puisque cette corrélation peut être dû à l'effet d'échantillonnage.
# donc il nous faut un test statistique 
# pour étudier la significativité de cette corrélation.
# On a déjà montré que aucune des variables quantitatives
# n'est normalement distribuée
# donc on peut pas appliquer le test de Pearson puisque ce dernier 
# impose la normalité des variables.
# donc on passe au test de Spearman.

plot(data$MajorAxisLength~data$Area)
# le tracé montre une corrélation entre les deux variables quantitatives Area et MajorAxisLength.
cor.test(data$Area, data$MajorAxisLength, method = c("spearman"), alternative = c("two.sided"), exact = FALSE)
# le test de Spearman valide la significativité de cette corrélation.
# => les deux variables quantitatives Area et MajorAxisLength sont bien corrélées.

# on va appliquer le même raisonnement pour toutes les variables quantitatives. 

pairs(data[,!(columns %in% c("Class"))])

# Etude de la corrélation entre la variable qualitative Class et les autres 
# variables quantitatives deux à deux:

# Lorsque on veut étudier la corrélation entre une variable qualitative et une variable quantitative
# on peut penser à un test d'ANOVA. le test d'ANOVA nécessite la normalité de la variable quantitative
# et donc il est impossible d'appliquer un test d'ANOVA pour notre cas de figure.
# Et par conséquence, on passe aux tests non paramétriques.
# Notre variable qualitative Class est à deux modalités.
# et donc on peut appliquer un test de wilcox afin d'étudier la corrélation entre la variable qualitative Class
# et les autres variables quantitatives.

index_Kecimen = which(data$Class == "Kecimen")
index_Besni = which(data$Class == "Besni")

wilcox.test(data$Area[index_Kecimen], data$Area[index_Besni])

# => le test de wilcox montre une corrélation entre la variable quantitative Area 
# et la variable qualitative Class.

# on va appliquer le même raisonnement pour toutes les variables quantitatives. 


# Tâche 5 : Régression linéaire :

# Regréssion de la variable MinorAxisLength en fonction de toutes les autres variables de la base de données :
linear_model = lm(MinorAxisLength~Area+MajorAxisLength+Eccentricity+ConvexArea+Extent+Perimeter+Class, data = data)
summary(linear_model)
# le test de Fisher affirme la significativé globale de notre modèle de régression.
# une bonne performance du modèle avec un R^2 = 94%

# test de normalité des residus
shapiro.test(residuals(linear_model))
# D'aprés le test de shapiro, les résidus de notre modèle de régression ne suivent pas une distribution normale
# Or la validité du modèle de régression linéaire impose la normalité des résidus ce qui n'est pas le cas.
# et donc le résultat du test de Fisher(qui impose lui aussi la normalité des résidus) ne va pas être considéré
# et par conséquence, on peut plus juger notre modèle.

# amélioration de la performance du modèle de régression :

# 1- éliminer les variables non significatives: ConvexArea, Class
linear_model = lm(MinorAxisLength~Area+MajorAxisLength+Eccentricity+Extent+Perimeter, data = data)
summary(linear_model)
shapiro.test(residuals(linear_model))
# => l élimination des variables non significatives n' a pas amélioré notre modèle.

#

# Réduction de dimension de la base de données en utilisant l'analyse en composantes
# principales (ACP)

# Encodage de la variable Class
data$Class = ifelse(data$Class == "Kecimen", 0, 1)
View(data)

# Pourquoi la standardisation des données est obligatoire pour une ACP?
# En fin de compte, ce que l'acp fait vraiment c'est projetter les données sur des axes (composantes) qui maximisent la variance.
# en effet, la variance d'une variable dépend de son unité et donc la variance sera affectée par l'échelle (ordre de grandeur) de la variable. 
# et donc sans standardisation, l'acp captera des variances imporantes due à l'échelle.
# et cela va tromper l'acp et donc diminuer sa performace.

# Centrage réduction des données
scaled_data = scale(data[,!(columns %in% c("Class"))], center = TRUE, scale = TRUE)
scaled_data = cbind(scaled_data, Class=c(data$Class))
names(scaled_data) = names(data)
View(scaled_data)

acp = prcomp(scaled_data[,!(columns %in% c("MinorAxisLength"))])
acp

explained_variance_ratio = summary(acp)[["importance"]]['Proportion of Variance',]
explained_variance_ratio = 100 * explained_variance_ratio
explained_variance_ratio

# On remarque bien que les 2 premières composantes principales contiennent 90% 
# de la quantitié total d'information.

# Regréssion linéaire de la variable MinorAxisLength en fonction des 2 premières composantes principales de l'ACP

linear_model_PCA= lm(scaled_data[,"MinorAxisLength"]~acp$x[,1:2])
summary(linear_model_PCA)
shapiro.test(residuals(linear_model_PCA))
# => la normalité des résidus est vérifiée
# le test de Fisher affirme la significativé globale de notre modèle de régression.
# une bonne performance du modèle avec un R^2 = 80%

# Tâche 6 : Régression linéaire généralisée :

hist(residuals(linear_model))
glm = glm(MinorAxisLength~Area+MajorAxisLength+Eccentricity+ConvexArea+Extent+Perimeter+Class,
          family = Gamma(), data = data)
summary(glm)

AIC(glm)
AIC(linear_model_PCA)
AIC(linear_model)

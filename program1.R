

library(factoextra)
library(cluster)
library(NbClust)
library(datasets)
library(ggplot2)


# iris --------------------------------------------------------------------

data(iris)
iris.scaled <- scale(iris[, -5]) #usunięcie kolumny 5 (klasa) i skalowanie
NbClust(iris.scaled, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all") #funkcja wspomagająca wybór na ile klastrów podzielić  dane


irisCluster <- kmeans(iris.scaled, centers=3, nstart = 20) #klasteryzacja iris | centers= ilość klastrów | nstart= ile pozycji startowych algorytmu


fviz_cluster(object=irisCluster, data = iris.scaled, geom = "point",stand = FALSE, choose.vars = c("Sepal.Length", "Sepal.Width"))#rysowanie
table(iris$Species, irisCluster$cluster)# macierz błędów porównanie czy klastry odpowiadają klasom

# klasteryzacja PAM (Partitioning Around Medoids)
library("cluster")
pam.res <- pam(iris.scaled, 3)#drugi argument na ile klastrów podzielić
pam.res$cluster
fviz_cluster(object=pam.res, data=iris.scaled, stand = FALSE, geom = "point", choose.vars = c("Sepal.Length", "Sepal.Width"))
table(pam.res$clustering, iris$Species)

# porównanie klasteryzacji
library(fpc)
#cluster.stats(d=dist(iris.scaled), irisCluster$cluster, pam.res$clustering) 
species <- as.numeric(iris$Species)#przekodowanie na wartości numeryczne
cluster.stats(d=dist(iris.scaled), species, pam.res$clustering) 

ClusterPurity <- function(clusters, classes) { #https://stackoverflow.com/questions/9253843/r-clustering-purity-metric
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

ClusterPurity(pam.res$clustering, species)
ClusterPurity(irisCluster$cluster, species)

# wine --------------------------------------------------------------------
wine = read.table("wine.data", header = FALSE, sep = ",")
names(wine) <- c("Class","Alcohol","Malic Acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
wine$`Class` <- factor(wine$`Class`)

wine.scaled <- scale(wine[, -1 ]) 
NbClust(wine.scaled, distance = "euclidean", min.nc = 2, max.nc = 15, method = "complete", index ="all") #wybór na ile klastrów podzielić 

wineCluster <- kmeans(wine.scaled, centers=3, nstart = 20) 

fviz_cluster(object=wineCluster, data = wine.scaled, geom = "point",stand = FALSE, choose.vars = c("Alcohol", "Malic Acid"))#rysowanie
table(wine$Class, wineCluster$cluster)# macierz błędów porównanie czy klastry odpowiadają klasom

# klasteryzacja PAM (Partitioning Around Medoids)
library("cluster")
pam.res <- pam(wine.scaled, 3)#drugi argument na ile klastrów podzielić
pam.res$cluster
fviz_cluster(object=pam.res, data=wine.scaled, stand = FALSE, geom = "point", choose.vars = c("Alcohol", "Malic Acid"))
table(pam.res$clustering, wine$Class)

# porównanie klasteryzacji
library(fpc)
#cluster.stats(d=dist(iris.scaled), irisCluster$cluster, pam.res$clustering) 
species <- as.numeric(wine$Class)#przekodowanie na wartości numeryczne bo się wysypuje
cluster.stats(d=dist(wine.scaled), species, pam.res$clustering) 
cluster.stats(d=dist(wine.scaled), species, wineCluster$cluster) 

ClusterPurity <- function(clusters, classes) { #https://stackoverflow.com/questions/9253843/r-clustering-purity-metric
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

ClusterPurity(pam.res$clustering, species)
ClusterPurity(wineCluster$cluster, species)


# glass -------------------------------------------------------------------

glass = read.csv("glass.data", header = FALSE, sep = ",")
names(glass) <- c("Id number","RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "Type_of_glass")
glass = glass[-1] ##usuwam pierwszą kolumnę ID
glass$`Type_of_glass` <- factor(glass$`Type_of_glass`)


glass.scaled <- scale(glass[, -10 ])
NbClust(glass.scaled, distance = "euclidean", min.nc = 2, max.nc = 15, method = "complete", index ="all") #wybór na ile klastrów podzielić 

glassCluster <- kmeans(glass.scaled, centers=10, nstart = 20) 

fviz_cluster(object=glassCluster, data = glass.scaled, geom = "point",stand = FALSE)#rysowanie
table(glass$Type_of_glass, glassCluster$cluster)# macierz błędów porównanie czy klastry odpowiadają klasom

# klasteryzacja PAM (Partitioning Around Medoids)
library("cluster")
pam.res <- pam(glass.scaled, 10)#drugi argument na ile klastrów podzielić
pam.res$cluster
fviz_cluster(object=pam.res, data=glass.scaled, stand = FALSE, geom = "point")
table(pam.res$clustering, glass$Type_of_glass)

# porównanie klasteryzacji
library(fpc)
#cluster.stats(d=dist(iris.scaled), irisCluster$cluster, pam.res$clustering) 
species <- as.numeric(glass$Type_of_glass)#przekodowanie na wartości numeryczne bo się wysypuje
cluster.stats(d=dist(glass.scaled), species, pam.res$clustering) 
cluster.stats(d=dist(glass.scaled), species, glassCluster$cluster) 

ClusterPurity <- function(clusters, classes) { #https://stackoverflow.com/questions/9253843/r-clustering-purity-metric
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

ClusterPurity(pam.res$clustering, species)
ClusterPurity(glassCluster$cluster, species)
# diabetes ----------------------------------------------------------------

diabetes = read.table("pima_indians_diabetes.txt", header = FALSE, sep = ",")
names(diabetes) <- c("No_pregnant", "Plasma_glucose", "Blood_pres", "Skin_thick", "Serum_insu", "BMI", "Diabetes_func", "Age", "Class")
diabetes$`Class` <- factor(diabetes$`Class`)

diabetes.scaled <- scale(diabetes[, -9 ]) 
NbClust(diabetes.scaled, distance = "euclidean", min.nc = 2, max.nc = 8, method = "complete", index ="all") #wybór na ile klastrów podzielić 

diabetesCluster <- kmeans(diabetes.scaled, centers=2, nstart = 20) 

fviz_cluster(object=diabetesCluster, data = diabetes.scaled, geom = "point",stand = FALSE)#rysowanie
table(diabetes$Class, diabetesCluster$cluster)# macierz błędów porównanie czy klastry odpowiadają klasom

# klasteryzacja PAM (Partitioning Around Medoids)
library("cluster")
pam.res <- pam(diabetes.scaled, 2)#drugi argument na ile klastrów podzielić
pam.res$cluster
fviz_cluster(object=pam.res, data=diabetes.scaled, stand = FALSE, geom = "point")
table(pam.res$clustering, diabetes$Class)

# porównanie klasteryzacji
library(fpc)
#cluster.stats(d=dist(iris.scaled), irisCluster$cluster, pam.res$clustering) 
species <- as.numeric(diabetes$Class)#przekodowanie na wartości numeryczne
cluster.stats(d=dist(diabetes.scaled), species, pam.res$clustering) 
cluster.stats(d=dist(diabetes.scaled), species, diabetesCluster$cluster) 

ClusterPurity <- function(clusters, classes) { #https://stackoverflow.com/questions/9253843/r-clustering-purity-metric
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

ClusterPurity(pam.res$clustering, species)
ClusterPurity(diabetesCluster$cluster, species)

# Cluster_data ------------------------------------------------------------


seeds = read.table("seeds_dataset.txt", header = FALSE, sep = "\t")
names(seeds) <- c("area A", "perimeter P", "compactness C", "length of kernel", "width of kernel", "asymmetry coefficient", "length of kernel groove","class")

seeds.scaled <- scale(seeds[,-8]) 
NbClust(seeds.scaled, distance = "euclidean", min.nc = 2, max.nc = 8, method = "complete", index ="all") #wybór na ile klastrów podzielić 

seedsCluster <- kmeans(seeds.scaled, centers=3, nstart = 20)

fviz_cluster(object=seedsCluster, data = seeds.scaled, geom = "point",stand = FALSE)#rysowanie
table(seeds$class, seedsCluster$cluster)# macierz błędów porównanie czy klastry odpowiadają klasom

# klasteryzacja PAM (Partitioning Around Medoids)
library("cluster")
pam.res <- pam(seeds.scaled, 3)#drugi argument na ile klastrów podzielić
pam.res$cluster
fviz_cluster(object=pam.res, data=seeds.scaled, stand = FALSE, geom = "point")
table(pam.res$clustering, seeds$class)

# porównanie klasteryzacji
library(fpc)
#cluster.stats(d=dist(iris.scaled), irisCluster$cluster, pam.res$clustering) 
species <- as.numeric(seeds$class)#przekodowanie na wartości numeryczne 
cluster.stats(d=dist(seeds.scaled), species, pam.res$clustering) 
cluster.stats(d=dist(seeds.scaled), species, seedsCluster$cluster) 

ClusterPurity <- function(clusters, classes) { #https://stackoverflow.com/questions/9253843/r-clustering-purity-metric
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

ClusterPurity(pam.res$clustering, species)
ClusterPurity(seedsCluster$cluster, species)


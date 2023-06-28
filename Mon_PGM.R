
# Funathon  2023
# juin 2023



# Lecture du fichier requirements.txt
requirements <- readLines("requirements_R.txt")

# Installation des packages
for (package in requirements) {
  install.packages(package)
}


library(aws.s3)
library(dplyr)
library(readr)

bucket <- "projet-funathon"
path_data <- "2023/sujet3/diffusion"

description_indiv <- s3read_using(read_delim, object = paste(path_data, "description-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
habitudes_indiv <- s3read_using(read_delim, object = paste(path_data, "habitudes-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
actphys_sedent <- s3read_using(read_delim, object = paste(path_data, "actphys-sedent.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
fpq <- s3read_using(read_delim, object = paste(path_data, "fpq.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)



library(ggplot2)
library(ggcorrplot)
library(sf)

# Histogramme des IMC

ggplot(data=description_indiv,aes(x=imc))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")

# Histogramme des niveaux de diplôme

ggplot(data=description_indiv,aes(x=diplome_interv))+ 
  geom_histogram(binwidth=1,color="black",fill="darkred")+
  labs(title="Histogramme des niveaux de diplôme",  # cette fois on rajoute des légendes
       x="Code du niveau de diplôme",
       y="Nombre d'individus")



description_indiv <- description_indiv %>% mutate(categorie_diplome=case_when(diplome_interv==1 ~ "Aucun diplôme, n'a jamais été scolarisé",
                                                                              diplome_interv==2 ~ "Aucun diplôme, scolarité s'est arrêtée à l'école primaire",
                                                                              diplome_interv==3 ~ "Aucun diplôme, scolarité s'est arrêtée au collège",
                                                                              diplome_interv==4 ~ "Aucun diplôme, scolarité s'est arrêtée au delà du collège",
                                                                              diplome_interv==5 ~ "Aucun diplôme, sans précision",
                                                                              diplome_interv==6 ~ "CEP",
                                                                              diplome_interv==7 ~ "CAP, BEP, BEPC, brevet élémentaire, brevet de compagnon",
                                                                              diplome_interv==8 ~ "Baccalauréat technologique ou professionnel,\nBrevet professionnel ou de technicien,\nBEA, BEC, BEI, BEH, capacité en droit",
                                                                              diplome_interv==9 ~ "Baccalauréat général",
                                                                              diplome_interv==10 ~ "Diplôme de 1er cycle universitaire (Bac +3, licence),\nBTS, DUT, DEST, DEUG, diplôme des professions\nsociales ou de la santé, d'infirmier",
                                                                              diplome_interv==11 ~ "Diplôme de 2ème cycle universitaire (Bac+4, Bac+5),\nMaster, Maîtrise, diplôme d'ingénieur,\nd'une grande école",
                                                                              diplome_interv==12 ~ "Diplôme de 3ème cycle universitaire (>Bac+5, doctorat),\ndiplôme de vétérinaire, médecin, pharmacien",
                                                                              diplome_interv==13 ~ "Refus",
                                                                              diplome_interv==14 ~ "Ne sait pas"))


# Tableau des fréquences de chaque catégorie de diplome
counts_diplome <- description_indiv %>% group_by(categorie_diplome) %>% summarise(n=n())

# Graphique en barres horizontales
ggplot(data=counts_diplome,aes(x=categorie_diplome,y=n))+
  geom_histogram(stat="identity")+
  coord_flip()+
  labs(title="Histogramme des niveaux de diplôme",
       x="Libelle du niveau de diplôme",
       y="Nombre d'individus")



# Recodage de la variable de type d'agglomération

description_indiv <- description_indiv %>% mutate(categorie_agglo = case_when(agglo_5cl==1 ~ "Rural",
                                                                              agglo_5cl==2 ~ "2000 - 19 999 hab",
                                                                              agglo_5cl==3 ~ "20 000 - 99 999 hab",
                                                                              agglo_5cl==4 ~ "+ 100 000 hab",
                                                                              agglo_5cl==5 ~ "Paris"))


counts_agglo <- description_indiv %>% group_by(categorie_agglo) %>% summarise(n=n())

# Générer le graphique en barres horizontales
ggplot(data=counts_agglo,aes(x=categorie_agglo,y=n))+
  geom_histogram(stat="identity")+
  coord_flip()+ 
  labs(title="Histogramme des types d'agglomération",
       x="Type d'agglomération",
       y="Nombre d'individus")

# Niveau de vie

description_indiv <- description_indiv %>% mutate(categorie_ruc=case_when(RUC_4cl==1 ~ "<900 €/mois/UC",
                                                                          RUC_4cl==2 ~ "[900-1 340[ €/mois/UC",
                                                                          RUC_4cl==3 ~ "[1 340-1 850[ €/mois/U",
                                                                          RUC_4cl==4 ~ ">=1 850 €/mois/UC"))

# Imc moyen par niveau de diplôme

imc_par_diplome <- description_indiv %>% group_by(categorie_diplome) %>% summarise(imc_moyen=mean(imc,na.rm=TRUE))

ggplot(data=imc_par_diplome,aes(x=categorie_diplome,y=imc_moyen))+
  geom_histogram(stat="identity")+
  coord_flip()+
  labs(title="IMC moyen par niveau de diplome",
       x="Catégorie de diplome",
       y="IMC moyen")



# Autoproduction par type d'agglomération
description_x_habitudes <- description_indiv %>% left_join(habitudes_indiv,by="NOIND")


autoprod_par_agglo <- description_x_habitudes %>% group_by(categorie_agglo) %>% summarise(part_autoprod=mean(autoproduction,na.rm=TRUE))
ggplot(data=autoprod_par_agglo,aes(x=categorie_agglo,y=part_autoprod))+
  geom_histogram(stat="identity")+
  labs(title="Part d'autoproduction par type d'agglomération",
       x="Type d'agglomération",
       y="Part d'autoproduction")




# Autoproduction par type d'agglomération
description_x_habitudes <- description_indiv %>% left_join(habitudes_indiv,by="NOIND")


autoprod_par_agglo <- description_x_habitudes %>% group_by(categorie_agglo) %>% summarise(part_autoprod=mean(autoproduction,na.rm=TRUE))
ggplot(data=autoprod_par_agglo,aes(x=categorie_agglo,y=part_autoprod))+
  geom_histogram(stat="identity")+
  labs(title="Part d'autoproduction par type d'agglomération",
       x="Type d'agglomération",
       y="Part d'autoproduction")




df_num <- description_x_habitudes %>% select(where(is.numeric))

df_num <- df_num %>% select(c("revenu","IA_score","imc","regime_vegetarien","poidsmax","fume","source_famille","jardin_potager","autoconsommation","consommation_bio"))

matrice_correlation <- model.matrix(~0+., data=df_num) %>% 
  cor(use="pairwise.complete.obs")
matrice_correlation %>%   ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=7)


##  carto



url <- "https://minio.lab.sspcloud.fr/projet-cartiflette/diffusion/shapefiles-test1/year%3D2022/administrative_level%3DREGION/crs%3D4326/FRANCE_ENTIERE%3Dmetropole/vectorfile_format%3D%27geojson%27/provider%3D%27IGN%27/source%3D%27EXPRESS-COG-CARTO-TERRITOIRE%27/raw.geojson"


region <- sf::st_read(url)

# Passons le fonds de carte dans le système de coordonnées de référence utilisé pour la FRance, Lambert 93 (code : 2154) au lieu de WGS 84
region <- region %>% st_transform(2154)


region$NOM_M
region <- region %>% mutate(NOM_M=ifelse(NOM_M=="CORSE", "PROVENCE-ALPES-COTE D'AZUR", NOM_M))



description_x_fpq = left_join(description_indiv, fpq, by="NOIND")


# Recodage de la variable région pour avoir les mêmes noms que dans notre fond de carte

description_x_fpq <- description_x_fpq %>% mutate(region_recode=case_when(region_adm_12cl==1 ~ "ILE-DE-FRANCE",
                                                                          region_adm_12cl==2 ~ "NORMANDIE",
                                                                          region_adm_12cl==3 ~ "CENTRE-VAL DE LOIRE",
                                                                          region_adm_12cl==4 ~ "PAYS DE LA LOIRE",
                                                                          region_adm_12cl==5 ~ "BRETAGNE",
                                                                          region_adm_12cl==6 ~ "HAUTS-DE-FRANCE",
                                                                          region_adm_12cl==7 ~ "GRAND EST",
                                                                          region_adm_12cl==8 ~ "BOURGOGNE-FRANCHE-COMTE",
                                                                          region_adm_12cl==9 ~ "AUVERGNE-RHONE-ALPES",
                                                                          region_adm_12cl==10 ~ "PROVENCE-ALPES-COTE D'AZUR",
                                                                          region_adm_12cl==11 ~ "OCCITANIE",
                                                                          region_adm_12cl==12 ~ "NOUVELLE-AQUITAINE",))


# Variable à représenter géographiquement : nombre de bière consommées par mois. 

biere_par_region <- description_x_fpq %>% group_by(region_recode) %>% summarise(freq_conso_biere_moyenne=mean(BA_biere_freq_M,na.rm=TRUE))
biere_par_region



# On crée un petit tableau avec nos régions et leurs attributs géographiques, 
# et surtout la variable qu'on vient de calculer (c'est-à-dire le nombre de bières consommées par mois par région en moyenne)

region_inca <- left_join(region,biere_par_region,by=c("NOM_M"="region_recode"))


region_inca$freq_conso_biere_moyenne

ggplot(data=region_inca) +
  geom_sf(aes(fill=freq_conso_biere_moyenne)) +
  scale_fill_continuous(low="yellow", high="Red", name="Nombre de bières consommées par mois en moyenne") +
  labs(title="Nombre de bières consommées par mois en moyenne par région")



##################################

# classif

habitudes_indiv_clustering_1 <- habitudes_indiv %>%
  select(-c(POPULATION, NOIND, periode_reference)) %>%  # Identifiants
  select_if(function(col) any(!is.na(col))) %>%  # Colonnes vides
  select_if(is.numeric) %>%  # Colonnes numériques à garder
  select_if(function(col) length(unique(col)) > 1) 


Mode <- function(x) {
  non_missing_values <- x[!is.na(x)]
  ux <- unique(non_missing_values)
  ux[which.max(tabulate(match(non_missing_values, ux)))]
}

clustering_2 <- habitudes_indiv_clustering_1 %>%
  mutate_all(~ifelse(is.na(.), Mode(.), .)) %>%
  select_if(function(col) length(unique(col)) > 1)


library(FactoMineR)
library(haven)
library(factoextra)
library(corrplot)



## partie ACP


## réalisation de l'ACP

acp2 <- PCA(clustering_2, graph=F,ncp=4)


fviz_screeplot(acp2, ncp=10)
## remarque : a priori l'information se trouve sur les 3 premiers axes.
## on peut laisser 5 (ncp par défaut)

fviz_pca_var(acp2, col.var="contrib",
             cex = 0.3,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             select.var=list(contrib = 15))

fviz_pca_var(acp2, col.var="contrib",
             axes = c(3,4),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             select.var=list(contrib = 15))




# Contributions des variables à PC1
fviz_contrib(acp2, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(acp2, choice = "var", axes = 2, top = 10)
# Contributions des variables à PC3
fviz_contrib(acp2, choice = "var", axes = 3, top = 10)
# Contributions des variables à PC4
fviz_contrib(acp2, choice = "var", axes = 4, top = 10)



CAH2<-HCPC(acp2,graph=FALSE,nb.clust=5)

 plot(CAH2, choice="tree")

classe <-  as.data.frame(CAH2$data.clust$clust) 


desp<- bind_cols(clustering_2,classe) %>% rename(classe=`CAH2$data.clust$clust`)

table(desp$classe) 


## description des classes.
r <- CAH2$desc.var$quanti

r1 <- r$`1`
r2 <- r$`2`
r3 <- r$`3`
r4 <- r$`4`



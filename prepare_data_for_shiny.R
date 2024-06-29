
library(sf)
library(dplyr)
library(tidyr)
library(reshape2)
library(mapsf)
library(mapview)
library(nngeo)

## Load circo


## Load data

base1<- readRDS("maps/mapdoniris_part1.RDS")
base2<- readRDS("maps/mapdoniris_part2.RDS")
base<-rbind(base1,base2) %>% filter(is.na(inscrits)==F)
don<-base   %>%
               mutate(FrontPopulaire = score_LFI+score_PCF+score_PS+score_EELV,
                      ExtremeDroite = score_RN+score_RECONQUETE,
                      CentreDroite = score_LR+score_LREM,
                      Divers = 100-ExtremeDroite-FrontPopulaire-CentreDroite,
                      TopBloc="Front Populaire",
                       MaxBloc = FrontPopulaire)
don$TopBloc[don$ExtremeDroite>don$FrontPopulaire] <- "Extreme Droite"
don$MaxBloc[don$ExtremeDroite>don$FrontPopulaire] <- don$ExtremeDroite[don$ExtremeDroite>don$FrontPopulaire]
don$TopBloc[don$CentreDroite>don$MaxBloc] <- "Centre et Droite"
don$MaxBloc[don$CentreDroite>don$MaxBloc] <- don$CentreDroite[don$CentreDroite>don$MaxBloc]



iris_don<-don %>%
             mutate(Circonscription_nom =libelle,
                    Commune = com_name,
                    Iris = iris_name,
                    Iris_code = iris_code,
                    Bloc_maximum = TopBloc,
                    nb_inscrits = inscrits,
                    nb_votants = votants,
                    nb_abstention=round(pct_abstention*inscrits/100),
                    tx_abstention = round(pct_abstention,1), 
                    nb_FrontPopulaire = round(FrontPopulaire*votants/100),
                    nb_ExtremeDroite = round(ExtremeDroite*votants/100),
                    nb_CentreDroite = round(CentreDroite*votants/100),
                    nb_Divers = round(Divers*votants/100),
                    pct_FrontPopulaire=round(FrontPopulaire,1),
                    pct_ExtremeDroite=round(ExtremeDroite,1),
                    pct_CentreDroite=round(CentreDroite,1),
                    pct_Divers=round(Divers,1)
                    )
iris_don<-iris_don[,47:63]
saveRDS(iris_don,"data_shiny/iris_don.RDS")

mapcirc<-readRDS("maps/mapcirc.RDS")
saveRDS(mapcirc,"data_shiny/mapcirc.RDS")



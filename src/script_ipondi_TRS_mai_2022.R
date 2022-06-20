

#chargement des packages
library(feather)
library(conflicted)
library(questionr)
library(haven)
library(labelled)
library(dplyr)
library(readr)
library(questionr)
library(tidyverse)
library(skimr)
library(reshape2)
library(stringr)
library(prettyR)
library(readxl)
library(data.table)


total_hh<-read_feather("total_hh1.feather") # base de données agrégées qui contient  le nombre total de ménages par iris (307 iris au total)

ind2<-read_feather("base_ind_men.feather") # base de données individus + ménages avce caractéristiques détaillées

#La base ind2 contient les variables renseignées dans les fichiers détails de l'INSEE. Certaines variables ont été recodées en plus. 


#dans l'échantillon de départ, on a  293572 individus répartis dans 307 iris ou équivalents.


#séparer les groupes


## Séparer les personnes vivant hors ménages (pvmh) et celles habitant dans des ménages (pvm)


ind2pvmhliste<- which(ind2$LPRM %in% c("Z"))

ind2pvmh<-ind2[(ind2pvmhliste),]

ind2pvm<-ind2[-(ind2pvmhliste),]



## sélectionner uniquement les ménages (la variable nhhid représente un identifiant ménage)

unique_households_ipondi_TRS<-ind2pvm[! duplicated (ind2pvm$nhhid), c(27,59,60)] #136.597 ménages et trois variables (ipondi, IRIS et nhhid)

#il existe des iris problématiques (17 iris pour lesquels il n'est pas possible de générer le TRS).

#En effet, pour certains IRIS, nous avons des IPONDI entiers (le plus souvent égal à 5). Par conséquent, le TRS ne peut pas être appliqué. 

#Nous allons séparer ces iris

list_irisp<-which(unique_households_ipondi_TRS$IRIS %in% c("440010000","440020000","440180101","440180102","440290101","440290102","440480000","440560000","440710000","440730000","440820000",
                                                           "440940000","441130000","441200000","442040101","442040102","442040103"))


unique_households_ipondi_TRS_ok<-unique_households_ipondi_TRS[-(list_irisp),] #ménages sur lesquels on peut appliquer le TRS



unique_households_ipondi_TRS_nok<-unique_households_ipondi_TRS[(list_irisp),] #ménages sur lesquels on ne peut pas appliquer le TRS


unique_households_ipondi_TRS_ok<-arrange(unique_households_ipondi_TRS_ok,IRIS,nhhid)

unique_households_ipondi_TRS_nok<-arrange(unique_households_ipondi_TRS_nok,IRIS,nhhid)


list_irisp_nok<- which(total_hh$IRIS %in% c("440010000","440020000","440180101","440180102","440290101","440290102","440480000","440560000","440710000","440730000","440820000",
                                            "440940000","441130000","441200000","442040101","442040102","442040103"))

total_hh_nok<-total_hh[c(list_irisp_nok),]

total_hh_ok<-total_hh[-(list_irisp_nok),]

total_hh_ok<-arrange(total_hh_ok,IRIS) # 290 IRIS

total_hh_nok<-arrange(total_hh_nok,IRIS) # 17 IRIS


 
# Nous allons générer le TRS pour les iris non problématiques


idlength=data.frame(IRIS=as.character(unique(unique_households_ipondi_TRS_ok$IRIS)))


ind2_iris_men_ipondi_TRS<-c()

list_poids_TRS<-c()

integ_ipondi<-c()

total_men1_TRS<-c()

pop_ipondi_TRS<-c()

total_men_TRS<-c()



int_trs_sum<- function(x){
  xv <- as.vector(x)
  xint <- floor(xv)
  r <- xv - xint
  def<-round(sum(r))
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}

int_expand_vector_TRS <- function(x){#expansion stage
  index <- 1:length(x)
  rep(index, round(x))
}

set.seed(22397)

for (i in 1:dim(idlength)[1])
{
  ind2_iris_men_ipondi_TRS[[i]]<-unique_households_ipondi_TRS_ok %>% 
    dplyr::filter(IRIS==idlength[i,1]) # avoir l'id de chaque ménage
  
  list_poids_TRS[[i]]<-ind2_iris_men_ipondi_TRS[[i]]$IPONDI #conserver les ipondi dans une liste pour chaque iris
  
  total_men1_TRS[[i]]<-total_hh_ok %>%
    dplyr::filter(IRIS==idlength[i,1]) # avoir pour chaque iris, le nombre de ménages total
  
  integ_ipondi[[i]]<-int_trs_sum(as.vector(list_poids_TRS[[i]]))
  
  total_men_TRS[[i]]<-int_expand_vector_TRS(integ_ipondi[[i]])
  
  pop_ipondi_TRS[[i]]<-ind2_iris_men_ipondi_TRS[[i]][total_men_TRS[[i]],] 
  
}

pop_hh_TRS<-data.frame( do.call(rbind, pop_ipondi_TRS)) # on a 394.463  ménages 



##########On va compléter par ce qui manque

idlength2=data.frame(IRIS=as.character(unique(unique_households_ipondi_TRS_nok$IRIS)))

ind2_iris_men_ipondi_TRS2<-c()

list_poids_TRS2<-c()

integ_ipondi2<-c()

total_men1_TRS2<-c()

pop_ipondi_TRS2<-c()

total_men_TRS2<-c()


int_expand_vector_TRS2 <- function(x){
  index <- 1:length(x)
  rep(index, round(x))
}




for (i in 1:dim(idlength2)[1])
{
  #cat("dim(idlength)[1]")
  #print(dim(idlength)[1])
  
  ind2_iris_men_ipondi_TRS2[[i]]<-unique_households_ipondi_TRS_nok %>% 
    dplyr::filter(IRIS==idlength2[i,1]) # avoir l'id de chaque ménage
  
  list_poids_TRS2[[i]]<-ind2_iris_men_ipondi_TRS2[[i]]$IPONDI #conserver les ipondi dans une liste pour chaque iris
  
  total_men1_TRS2[[i]]<-int_expand_vector_TRS2(list_poids_TRS2[[i]])
  
  pop_ipondi_TRS2[[i]]<-ind2_iris_men_ipondi_TRS2[[i]][total_men1_TRS2[[i]],] 
}

pop_hh_17_iris<-data.frame( do.call(rbind, pop_ipondi_TRS2)) # on a 23.600  ménages 


#Rassembler les deux bases (vérifier que l'on a bien 394.463 + 23600 = 418.O63 ménages)

pop_hh_TRS_unif<-bind_rows(pop_hh_TRS,pop_hh_17_iris)


#intégrer les individus

pop_hh_TRS<-arrange(pop_hh_TRS,IRIS,nhhid)

pop_hh_17_iris<-arrange(pop_hh_17_iris,IRIS,nhhid)

pop_hh_TRS_unif<-arrange(pop_hh_TRS_unif,IRIS,nhhid)

ind2<-arrange(ind2pvm,IRIS,nhhid)


pop_synt_ipondi_TRS<-inner_join(ind2,pop_hh_TRS_unif, by=c("IRIS", "nhhid")) # 


pop_synt_290_iris<-inner_join(ind2,pop_hh_TRS, by=c("IRIS", "nhhid")) # 


pop_synt_17_iris<-inner_join(ind2,pop_hh_17_iris, by=c("IRIS", "nhhid")) # 

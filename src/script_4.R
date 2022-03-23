
# Post-processing step and validation of results


### Post-processing step ----

res <-small_table %>%
  group_by(modalities) %>%
  mutate(
    prob1 = if (any(new_results > 1)) min(incomes[new_results > 1]) else Inf,
    box= ifelse(incomes >= prob1, 0, new_results)
  ) %>%
  dplyr::select(-prob1)

res<-as.data.frame(res)


repeat {
  res <- res %>% group_by(modalities)  %>% 
    dplyr::mutate(box_finales = box - dplyr::lag(box))
  if (all(res$box_finales >= 0, na.rm = TRUE)) {
    res <- ungroup(res)
    break
  } 
  res <- dplyr::filter(res, is.na(box_finales) | box_finales >= 0)
}
res<-na.omit(as.data.frame(res))

## Verification


length(which(res$box_finales<0))
length(which(res$new_results>1))

#No more incorrect values

## Allocate a specific income to each household of the synthetic population ----

#install.packages("questionr")

library(questionr)

res<-res%>%
  separate(modalities, c("ownership", "age", "size", "family_comp"), sep = " ")

res[,c(1:4)] <-lapply(res[,c(1:4)] , as.factor)

res$ownership<-factor(res$ownership, levels = c("Tenant", "Owner"))

res$family_comp<-factor(res$family_comp, levels = c ("Single_wom","Single_man","Couple_with_child","Couple_without_child","Single_parent","complex_hh"))
res<-arrange(res, ownership, age, size, family_comp)

res2<-res%>%
  group_by(ownership,age,size,family_comp)


occu_com2<-occu_com[,c(1:2)] %>%
  separate(total, c("ownership", "age", "size", "family_comp"), sep = " ")

sum(occu_com2$n) #157647 households in the synthetic population

occu_com2[,c(1:4)] <-lapply(occu_com2[,c(1:4)] , as.factor)

occu_com2$ownership<-factor(occu_com2$ownership, levels = c("Tenant", "Owner"))

occu_com2$family_comp<-factor(occu_com2$family_comp, levels = c ("Single_wom","Single_man","Couple_with_child","Couple_without_child","Single_parent","complex_hh"))

occu_com2<-arrange(occu_com2, ownership, age, size, family_comp)

occu_com3<-occu_com2%>%
  group_by(ownership,age,size,family_comp)


res_l<-group_split(res2)

occu_com2_l<-group_split(occu_com3)

tirage<-c()
dist_tirage<-c()


set.seed(1051)
for (i in 1:length(res_l)){
  tirage[[i]]<-sample(unique(res_l[[i]]$incomes),size =occu_com2_l[[i]]$n, prob=res_l[[i]]$box_finales, replace = TRUE )
  dist_tirage[[i]]<-table(tirage[[i]])
}


liste_rev <- unique(unlist(lapply(dist_tirage, names)))


matrice_estimation <- matrix(0, nrow = length(dist_tirage), ncol = length(liste_rev), 
                             dimnames = list(names(dist_tirage), liste_rev))

corresp <- lapply(dist_tirage, function(x) match(names(x), liste_rev))


matrice_estimation[cbind(rep(sequence(nrow(matrice_estimation)), sapply(corresp, length)),
                         unlist(corresp))] <- unlist(dist_tirage)

matrice_estimation<-matrice_estimation[,order(as.integer(colnames(matrice_estimation)))]

row.names(matrice_estimation)<-paste(occu_com2$ownership, occu_com2$age, occu_com2$size, occu_com2$family_comp)


view(matrice_estimation) # each row represent a modality and each column an income.


# Example:  1505 represents the total number of households "Tenant 0_29 1_pers Single_wom " with an income less than 8370.625 

sum(matrice_estimation[1,]);occu_com2[1,] #: in the synthetic population, the total number of households "Owner 30_39 1_pers Single_man" is 14613


tableau_estimation<-as.data.frame.table(matrice_estimation)

names(tableau_estimation)<-c("modalities","thresholds","total_hh")

tableau_estimation$thresholds<-as.character(tableau_estimation$thresholds)

tableau_estimation$thresholds<-as.numeric(tableau_estimation$thresholds)

tableau_estimation<-arrange(tableau_estimation,modalities,thresholds)

head(tableau_estimation)


newline2<-seq(nrow(tableau_estimation))%>%
  split (group_indices(tableau_estimation,modalities))%>%
  map(~c(NA,.x))%>%
  unlist

tableau_estimation2<-tableau_estimation[newline2,]


tableau_estimation2<-tableau_estimation2%>% 
  fill(modalities, .direction="up")%>%
  mutate(thresholds= if_else(is.na(thresholds), 0, thresholds))%>%
  mutate(total_hh= if_else(is.na(total_hh), 0, total_hh))


tableau_estimation2$thresholds_prev<-tableau_estimation2$thresholds
tableau_estimation2$thresholds_after<-NA


tableau_estimation2<-tableau_estimation2%>%
  group_by(modalities)%>%
  mutate(thresholds_after = dplyr::lag(thresholds))


attri_men<-list()

set.seed(1908)


for (i in 1:nrow(tableau_estimation2)){
  attri_men[[i]]<-runif(tableau_estimation2$total_hh[[i]], min = tableau_estimation2$thresholds_after[[i]], max=tableau_estimation2$thresholds_prev[[i]])
}

tableau_estimation_min<-tableau_estimation2[,1]


tableau_estimation_min_l<-split(tableau_estimation_min,seq(nrow(tableau_estimation_min)))
length(attri_men)
length(tableau_estimation_min_l)

attri_men[lengths(attri_men) == 0] <- NA

pop_synt_rev<-na.omit(do.call(rbind.data.frame, Map(data.frame, modalities=tableau_estimation_min_l, hh_income=attri_men)))

rownames(pop_synt_rev)<-NULL

pop_synt_rev$modal<-pop_synt_rev$modalities

pop_synt_rev<-arrange(pop_synt_rev,modalities, hh_income)%>%
  separate(modal, c("ownership", "age", "size", "family_comp"), sep = " ")


head(pop_synt_rev) # Each synthetic household has a specific income




#### Validation of results

## We first calculate simulated deciles from the specific incomes ----

total_population_decile_simulated<- quantile (pop_synt_rev$hh_income, probs =  seq(0.1,0.9,0.1)) #Entire population simulated deciles

dec_Ag<-aggregate(hh_income ~ age, pop_synt_rev, quantile, seq(0.1, 0.9, 0.1 ))

dec_St<-aggregate(hh_income ~ ownership, pop_synt_rev, quantile, seq(0.1, 0.9, 0.1 ))

dec_Co<-aggregate(hh_income ~ family_comp, pop_synt_rev, quantile, seq(0.1, 0.9, 0.1 ))

dec_Si<-aggregate(hh_income ~ size, pop_synt_rev, quantile, seq(0.1, 0.9, 0.1 ))

dec_Ag<-rename.variable(dec_Ag, "age", "modalities")
dec_St<-rename.variable(dec_St, "ownership", "modalities")
dec_Co<-rename.variable(dec_Co, "family_comp", "modalities")
dec_Si<-rename.variable(dec_Si, "size", "modalities")

sim1<-bind_rows(dec_Si, dec_Co, dec_Ag, dec_St)
sim2<-as.data.frame(sim1$hh_income)
sim3<-data.frame(t(total_population_decile_simulated))
names(sim2)<-c("D1","D2","D3", "D4","D5","D6","D7","D8","D9")
names(sim3)<-c("D1","D2","D3", "D4","D5","D6","D7","D8","D9")
sim4<-bind_rows(sim3,sim2)

simulated_deciles<-sim4[c(1:6,10,12,9,8,11,7,13:20),]
modality<-c("Entire_population",decile_total$modality)
simulated_deciles<-add_column(simulated_deciles, modality, .before = "D1")

view(simulated_deciles) # Table 3

## Observed deciles from Fiosofi ----

observed<-read_feather("deciles_filosofi.feather")
obs1<-data.frame(t(total_population_decile[1:9]))
obs1<-add_column(obs1, "Entire_population", .before = 1)
names(obs1)<-names(observed)
observed_deciles<-bind_rows(obs1,observed)
view(observed_deciles)




## Quantile-Quantile plots between simulated and observed deciles----

#install.packages("ggplot2")
#install.packages("reshape2")


library(ggplot2)
library(reshape2)
library(lattice)

obs_deciles<-melt(data=observed_deciles, id.vars = "modality", variable.name = "Deciles", value.name = "Observed")
sim_deciles<-melt(data=simulated_deciles,id.vars = "modality", variable.name = "Deciles", value.name = "Simulated")
complet_deciles<-merge(obs_deciles,sim_deciles, by=c("modality","Deciles"))
complet_deciles$modality<-factor(complet_deciles$modality,levels=c("1 person","2 persons","3 persons","4 persons", "5 persons or more", 
                                                              "0_29", "30_39","40_49","50_59","60_74","75_or_more",
                                                              "Single man","Single woman","Single parent family", "Couple  without children", "Couple  with children","Complex households",
                                                              "Owner","Tenant","Entire_population"))
p<-ggplot(complet_deciles,aes(Observed,Simulated,color=modality))
p+geom_point(size=1.25)+geom_abline()+facet_wrap(~modality)+ theme(legend.position = "none")+theme(axis.text=element_text(size=7))



## Absolute and relative errors ----

abs_errors<-abs(round(observed_deciles[,-1])-round(simulated_deciles[,-1]))

rel_errors<-round(((abs_errors/round(observed_deciles[,-1]))*100),digits=2)


abs_errors<-add_column(abs_errors, observed_deciles$modality, .before = 1) #table 4

rel_errors<-add_column(rel_errors, observed_deciles$modality, .before = 1) #table 5

abs_errors
rel_errors


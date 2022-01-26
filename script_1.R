
#Problem-solving heuristic :  step 1 (subsection 4.13)


## Synthetic population loading and visualization ------

#install.packages("feather") # : use this command if package feather is not yet installed

library(feather)

synth_pop<-read_feather("synth_pop_original.feather") #### commencer ici


str(synth_pop)
summary(synth_pop)

# The dataframe synth_pop is the synthetic household population for the city of nantes.
# We have 157,647 households. Each row of synth_pop is therefore a household.


Age.lv<-c("0_29", "30_39",  "40_49", "50_59", "60_74", "75_or_more")

Size.lv<-c("1_pers", "2_pers", "3_pers", "4_pers", "5_pers_or_more")

Owner.lv<-c("Tenant","Owner")

Fam_comp.lv<-c("Single_wom","Single_man","Couple_without_child","Couple_with_child","Single_parent","complex_hh")

dec_owner<-c("Owner","Tenant")

dec_fam_comp<-c("Single_man","Single_wom","Couple_without_child","Couple_with_child","Single_parent","complex_hh")

dec_age<-c("0_29", "30_39",  "40_49", "50_59", "60_74", "75_or_more")

dec_size<-c("1_pers", "2_pers", "3_pers", "4_pers", "5_pers_or_more")

dec_total<-c(dec_size,dec_fam_comp,dec_age,dec_owner)





#### Relative frequencies of the synthetic population ------


prob_simple<-function(x){
  A<-table(x)
  B<-prop.table(A)
  return(B)
}

prob_age<-c(prob_simple(synth_pop$age))


prob_comp<-c(prob_simple(x=synth_pop$family_comp))


prob_size<-c(prob_simple(x=synth_pop$size))


prob_owner<-c(prob_simple(x=synth_pop$ownership))

#modify the order of modalities

prob_comp1<-c(prob_comp[2], prob_comp[1], prob_comp[3], prob_comp[4], prob_comp[5], prob_comp[6])
prob_owner1<-c(prob_owner[2],prob_owner[1])


prob_size
prob_comp1
prob_age
prob_owner1




#### Estimation of joint probabilities ------


synth_pop2<-synth_pop

synth_pop2$total<-paste(synth_pop2$ownership,synth_pop2$age,synth_pop2$size,synth_pop2$family_comp)

synt_pop_comb<-expand.grid(ownership=levels(synth_pop$ownership),age=levels(synth_pop$age),size=levels(synth_pop$size),family_comp=levels(synth_pop$family_comp))

synt_pop_comb$total<-paste(synt_pop_comb$ownership,synt_pop_comb$age,synt_pop_comb$size,synt_pop_comb$family_comp)

#install.packages("dplyr") # : use this command if package dplyr is not yet installed

library(dplyr)

combi_diff<-anti_join(synt_pop_comb,synth_pop2,by="total")

combi_diff<-combi_diff%>%
  arrange(c(factor(family_comp,levels=dec_fam_comp)),(factor(size,levels=dec_size)),(factor(age,levels=dec_age)),(factor(ownership,levels=dec_owner)))

nrow(combi_diff)
head(combi_diff)

combi_diff$Probability<-0

#compter le nombre d'occurences de chaque combinaison dans la population synthétique et calculer la probabilité correspondante

occu_com<-synth_pop2%>%
  count(total)

occu_com$Probability<-occu_com$n/sum(occu_com$n)

synt_pop_comb1<-inner_join(synt_pop_comb,occu_com[,-2],by="total")

synt_pop_comb2<-bind_rows(synt_pop_comb1,combi_diff)

synt_pop_comb2<-synt_pop_comb2%>%
  arrange(c(factor(family_comp,levels=dec_fam_comp)),(factor(size,levels=dec_size)),(factor(age,levels=dec_age)),(factor(ownership,levels=dec_owner)))

head(synt_pop_comb2)

final_joint_proba<-synt_pop_comb2$Probability

length(which(final_joint_proba>0)) # 187 cross-modalities with non zero probabilities

sum(final_joint_proba)#1






























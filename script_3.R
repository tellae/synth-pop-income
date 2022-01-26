
#Problem-solving heuristic :  step 3 (subsection 4.15)

#install.packages("minxent") # : use this command if package minxent is not yet installed

library(minxent)

??minxent


### matrix of moment vector functions ----

all_combinations <-expand.grid(dec_owner,dec_age,dec_size,dec_fam_comp)

all_combinations$total<-paste(all_combinations$Var1,all_combinations$Var2,all_combinations$Var3,all_combinations$Var4)

#install.packages("stringr") # : use this command if package stringr is not yet installed

library(stringr)


Gfonction<-function(z){
  
  A<-matrix(nrow = length(dec_total), ncol=nrow(z), dimnames=list(c(dec_owner, dec_age, dec_size, dec_fam_comp),z$total))
  seq_noms<-rownames(A)
  for (i in 1:length(seq_noms)){
    A[i,]<-str_count(z[,5],seq_noms[i])
  }
  vec<-1
  B<- matrix(1,nrow = length(dec_total)+1, ncol=nrow(z), dimnames=list(c("constant row", dec_owner, dec_age, dec_size, dec_fam_comp),z$total)) 
  B[-vec,]<-A
  C<-B[-c(3,9,14,20),]# For each variable, delete one modality
  return(C)
}


Moment_mat<-Gfonction(z=all_combinations) # matrix of moment vector 

view(Moment_mat)


### Estimation of constraints on deciles (  equation (22)  )----

ybis<-(0:10)/10

finterp2<-function(x,z){
  A<-c()
  B<-c()
  C<-c()
  D<-c()
  x<-x[,-1]
  xbis<-split(x,seq(nrow(x)))
  for (i in 1:nrow(x)){
    B[[i]]<-rep(ybis,1)
    C[[i]]<-rep(z,1)
    A[[i]]<-approx(xbis[[i]], B[[i]], xout = C[[i]], method = "linear", ties = "ordered")$y
    D<-lapply(A,function (x) ifelse(is.na(x), 1.000000, x))    
    }
  return (D)
}

finterp_bis<-function(u,v,w){
  A<-as.list(v) 
  B<-Map("*",u,A)
  C <-as.data.frame (matrix(unlist(B),nrow=sapply(B,length), byrow = FALSE))
  names(C)<-names(v)
  C$Proba<-apply(C,1,sum)
  D<-C$Proba
 return(D)
}


ech_compo<-finterp2(x=decile_compo,z=vec_all_incomes) 

ech_statut<-finterp2(x=decile_statut,z=vec_all_incomes) 

ech_age<-finterp2(x=decile_age,z=vec_all_incomes) 

ech_size<-finterp2(x=decile_size,z=vec_all_incomes)


p_compo<-finterp_bis(u=ech_compo,v=prob_comp1,w=vec_all_incomes)

p_size<-finterp_bis(u=ech_size,v=prob_size,w=vec_all_incomes)

p_statut<-finterp_bis(u=ech_statut,v=prob_owner1,w=vec_all_incomes)

p_age<-finterp_bis(u=ech_age,v=prob_age,w=vec_all_incomes)


eta_fonction<-function(u,v,w){
  A<-as.list(v) 
  B<-Map("*",u,A)
  eta<-lapply(B, "/" , w)
}


constraint_composition<-eta_fonction(u=ech_compo,v=prob_comp1,w=p_compo)

constraint_statut<-eta_fonction(u=ech_statut,v=prob_owner1,w=p_statut)

constraint_size<-eta_fonction(u=ech_size,v=prob_size,w=p_size)

constraint_age<-eta_fonction(u=ech_age,v=prob_age,w=p_age)


l1 <- list(rep(1,length(vec_all_incomes))) 

eta_total<-c(l1,constraint_statut,constraint_age,constraint_size,constraint_composition)

eta_final<-eta_total[c(-3,-9,-14,-20)] # For each variable, delete one modality

eta_c<-matrix(unlist(eta_final),nrow=length(eta_final), byrow = TRUE)

# eta represents the vector of moment constraints

view(eta_c)



### Cross entropy minimization using minxent package ----



A<-qr(t(Moment_mat)) 

I<-A$pivot[1:A$rank] 

J=I[I>1] 


Vpj=as.matrix(final_joint_proba,length(final_joint_proba),1) 
CVpj=diag(final_joint_proba)- Vpj %*% t(Vpj) 
CVpjG=Moment_mat[J,]%*%CVpj%*%t(Moment_mat[J,])

#install.packages("boot") # : use this command if package boot is not yet installed

library(boot)


B=qr(CVpjG) 
B$rank 
Ii=c(1,J[B$pivot[1:B$rank]])
Jj=Ii[-1]
CVpjGr=Moment_mat[Jj,]%*%CVpj%*%t(Moment_mat[Jj,])
kappa(CVpjGr)


final_proba<-c()

for (i in 1:length(vec_all_incomes)){
  
  Iii=Ii
  repeat{
    Id=diag(eta_c[Iii,i])
    M=dim(Moment_mat[Iii,])
    mc=M[2] 
    ml=M[1] 
    Coef_objectif=c(rep(0,mc), rep(1,ml)) 
    M_equation= cbind(Moment_mat[Iii,], Id)
    result=simplex(
      a=Coef_objectif,
      A3=M_equation, 
      b3=eta_c[Iii,i])
    if (abs(result$value)<1e-8) {
      
      break}
    else{
      print(c(as.character(i), "incompatible system"))
      indice_contrainte_violee=which.max(result$soln[seq(mc+1,mc+ml)])
      Iii=Iii[-indice_contrainte_violee]    }
    
  }
  
  tryCatch (
    expr = {
      Sortie=minxent.multiple(q=final_joint_proba,G=Moment_mat[Iii,],eta = eta_c[Iii,i],lambda = rep(0,length(Iii)-1)) # mieux gérer les lambda
    },
    error = function(e){
      message(c(as.character(i), '/ Erreur on minxent.multiple ' )) 
      Iii=Iii[1:2]
      Sortie=minxent.single(q=final_joint_proba,G=Moment_mat[Iii,],eta_c = eta[Iii,i],lambda = rep(0))
    },
    warning = function(w){
      message('Caught a warning!')
      print(w)
    },
    finally = {
      final_proba[[i]]<-Sortie$Estimates #  
    })
}


final_proba_v<-c()

pop_synt_proba<-c() 

all_proba<-c()


list_proba_priori<-as.list(synt_pop_comb2[,5:6])


for (i in 1:length(vec_all_incomes)){
  
  final_proba_v[[i]]<-c(final_proba[[i]])
  
  pop_synt_proba[[i]]<-rep(list_proba_priori,1)
  
  all_proba[[i]]<-c(pop_synt_proba[[i]],list(final_proba_v[[i]]))
  
  names(all_proba[[i]])<-c("modalities","priori_distribution","posteriori_distribution")
  
}

compar_proba<-lapply(all_proba, as.data.frame)

# compar_proba is a list of 190 items (each item = one income). Each item contains:
#the priori distribution (joint probabilities in synthetic population)
## the sought probabilities (p distribution, step 3 of the script).


### Estimation of equation 2 paragraph 4.12 ----


nonzero_proba<-c()
compar_nonzero_proba<-c()

for (i in 1:length(vec_all_incomes)){
  
  nonzero_proba[[i]]<-which(compar_proba[[i]]$priori_distribution==0)
  
  compar_nonzero_proba [[i]]<-compar_proba[[i]][- nonzero_proba[[i]],] #We only keep the nonzero probabilities
  
  compar_nonzero_proba[[i]]$ratio<-compar_nonzero_proba[[i]]$posteriori_distribution/compar_nonzero_proba[[i]]$priori_distribution
  
  compar_nonzero_proba[[i]]$p_R<-p_R[i,2] 
  
  compar_nonzero_proba[[i]]$results<-compar_nonzero_proba[[i]]$ratio*compar_nonzero_proba[[i]]$p_R
  
  compar_nonzero_proba[[i]]$incomes<-p_R[i,1]
  
 }

table_c<-Reduce(rbind,compar_nonzero_proba)

# table_c contains the total number of estimated probabilities (190*187 = 35530) (paragraph 5.1)

head(table_c)

# Interpretation for the first row:
# the probability that a household has an income lower than 8370.625 given the 
# following characteristics  Owner, 0_29, 1_pers, Single_man is 0.06073943

## Estimation between two income values ----


##Reprise demain mardi

small_table <-table_c[,c(1,6,7)]

small_table$new_results<-small_table$results


small_table <-arrange(small_table,modalities, incomes)

library(purrr)
library(tidyr)

 newline<-seq(nrow(small_table))%>%
  split (group_indices(small_table,modalities))%>%
  map(~c(NA,.x))%>%
  unlist

small_table<-small_table[newline,]


small_table<-small_table%>% 
  fill(modalities, .direction="up") %>%
  mutate(incomes= if_else(is.na(incomes), 0, incomes))%>%
  mutate(new_results= if_else(is.na(new_results), 0, new_results))

small_table<-small_table[,-2]

small_table$box<-0


small_table_l<-small_table%>%
  group_split(modalities)

for (i in 1:length(small_table_l)){
  for(j in 2:192){
    small_table_l[[i]][j,4]<-(small_table_l[[i]][j,3])-(small_table_l[[i]][j-1,3])
  }
}


small_table1<-as.data.frame(Reduce(rbind,small_table_l))


length(which(small_table1$new_results>1)) # probabilities > 1
length(which(small_table1$box<0)) # negative probabilities


       
       





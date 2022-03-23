
#Problem-solving heuristic :  step 2 (subsection 4.13)


## FiLoSoFi database loading and visisualization ----

library(feather)

decile_total<-read_feather("deciles_filosofi.feather") 

decile_total # The dataframe decile_total represents the distribution of income deciles for the city of nantes.



## Income vector ----

#Set for each modality a minimum income=0 and a maximum income = D9*1.5

decile_total$D10<-decile_total$D9*1.5
D0<-0

library(tibble)

decile_total<-add_column(decile_total,D0,.before = "D1")

decile_size <- decile_total[c(1:5),]

decile_compo<- decile_total[c(6:11),]

decile_age<- decile_total[c(12:17),]

decile_statut<- decile_total[c(18:19),]


numbers<-1:10

for (i in numbers){
  numdeciles<-paste0("D", numbers)
}


VecR<-function(x){
  A<-as.data.frame(x[,c(numdeciles)])
  B<-as.matrix(A)
  C<-sort(as.vector(B), decreasing = FALSE)
  return(C)
}

vec_all_incomes<-VecR(x=decile_total)

vec_all_incomes # 190 modalities for the income

total_population_decile<-c(10303.48, 13336.07, 16023.85, 18631.33, 21262.67, 24188.00, 27774.44, 32620.00, 41308.00, 75090.00)



#Linear extrapolation of these 190 incomes from the total population deciles ----

finterp<-function(x){
  y<-(0:10)/10
  income<-vec_all_incomes
  proba1<-approx(x, y, xout = income, method = "linear", ties = "ordered")$y
  C<-data.frame(income,proba1)
  return (C)
}

p_R<-finterp(x=c(0,total_population_decile))  


# In p_R dataframe, we have a probability for each income





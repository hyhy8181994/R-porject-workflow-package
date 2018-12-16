#' Protein Molecular Weight Predictor
#'
#' This function allows you to calculate the molecular weight of protein based on the molecular weight of each amino acid. The output molecular weight is in kDa unit.
#' @param ps a string of amino acid sequence (blanks and numbers will be ignored)
#' @keywords protein weight
#' @return print the predicted molecular weight and the number of amino acids 
#' @export
#' @examples
#'mw("RNBC")
#'  #[1]"aa sequence length =  4"
#'  #[1]"Molecular weight prediction = 0.58 kDa"
mw<-function(ps){
  a<-NULL
  b<-NULL
  c<-NULL
  f<-0
  i<-1
  g<-NULL
  h<-NULL
  j<-NULL
  k<-NULL
  h<-gsub("\n","",ps)  #clear the unrecognised characters
  j<-gsub(" ","",h) #clear the blanks
  k<- gsub('[[:digit:]]+', "",j) #delete the numbers
  a<-toupper(unlist(strsplit(k,""))) #break the single string into single characters
  b<-length(a)    #count the length of vector
  print(paste("aa sequence length = ",b)) #print the length
  for (i in i:b){          #convert amino acids to their molecular weights
    d<-NULL
    e<-NULL
    d<-a[i]
    if (d=="A"){
      e<-89
    } else if (d=="R"){
      e<-174
    } else if (d=="N"){
      e<-132
    } else if (d=="D"){
      e<-133
    } else if (d=="B"){
      e<-133
    } else if (d=="C"){
      e<-121
    } else if (d=="Q"){
      e<-146
    } else if (d=="E"){
      e<-147
    } else if (d=="Z"){
      e<-147
    } else if (d=="G"){
      e<-75
    } else if (d=="H"){
      e<-155
    } else if (d=="I"){
      e<-131
    } else if (d=="L"){
      e<-131
    } else if (d=="K"){
      e<-146
    } else if (d=="M"){
      e<-149
    } else if (d=="F"){
      e<-165
    } else if (d=="P"){
      e<-115
    } else if (d=="S"){
      e<-105
    } else if (d=="T"){
      e<-119
    } else if (d=="W"){
      e<-204
    } else if (d=="Y"){
      e<-204
    } else if (d=="V"){
      e<-117
    } else {
      e<-110
    }
    f<-f+e      #count the total weight
  }
  g<-round((f+18.2)/1000,2)    #add the weight of water molecule and convert to kDa....round the final number
  print(paste("Molecular weight prediction =",g,"kDa"))
}


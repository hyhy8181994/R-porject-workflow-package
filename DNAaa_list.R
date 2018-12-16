#' DNA to amino acid converter
#'
#' This function allows you to convert DNA sequence of specific gene to amino acid sequence based on standard DNA codon table. The unrecognised DNA codon will be shown as "*" , and also the stop codon will not be shown in the sequence. Note: make sure the input DNA sequence is complete coding sequence (CDS).
#'
#'
#' @param bpseq   a string input of DNA sequence (optimally starts with ATG and ends with stop codon in 5'- 3' direction) [blanks and numbers will be ignored]
#' @keywords DNA
#' @return a list of amino acid number, length of gene, amino acid sequence.
#' @references NCBI DNA codon tables
#'
#' https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/index.cgi?chapter=tgencodes#SG1
#'
#' @export
#' @examples
#' DNAaa("ATGGTA")
#'
#' #the output
#' #$aa_number
#' [1] 2
#'
#' $length_of_gene
#' [1] 6
#'
#' $aa_seq
#' [1] "MV"
#'
#'
#'
#'
#'
DNAaa<-function(bpseq){
  a<-NULL
  b<-NULL
  c<-NULL
  d<-NULL
  e<-1
  f<-NULL
  h<-NULL
  j<-NULL
  k<-NULL
  l<-NULL
  m<-NULL
  n<-NULL
  list<-NULL
  a<-toupper(bpseq)
  l<-gsub("\n","",a)  #clear the unrecognised characters
  m<-gsub(" ","",l)   #clear the blanks
  n<-gsub('[[:digit:]]+', "",m) #clear the numbers
  b<-nchar(n)     #count number of characters in string
  c<-seq(1,b,by=3)    #create a vector with a sequence from 1 to the length of the string, by 3
  d<-sapply(c,function(ii){     #replace the numeric vector with input string
    substr(n,ii,ii+2)       #convert the gene sequence to codon
  })
  f<-length(d)     #count the number of codons
  for (e in e:f){    #convert the codons to amino acids according to standard DNA codon table
    g<-NULL
    if (d[e]=="TTT"){
      g<-"F"
    } else if (d[e]=="TTC"){
      g<-"F"
    } else if (d[e]=="TTA"){
      g<-"L"
    } else if (d[e]=="TTG"){
      g<-"L"
    } else if (d[e]=="CTT"){
      g<-"L"
    } else if (d[e]=="CTC"){
      g<-"L"
    } else if (d[e]=="CTA"){
      g<-"L"
    } else if (d[e]=="CTG"){
      g<-"L"
    } else if (d[e]=="ATT"){
      g<-"I"
    } else if (d[e]=="ATC"){
      g<-"I"
    } else if (d[e]=="ATA"){
      g<-"I"
    } else if (d[e]=="ATG"){
      g<-"M"
    } else if (d[e]=="GTT"){
      g<-"V"
    } else if (d[e]=="GTC"){
      g<-"V"
    } else if (d[e]=="GTA"){
      g<-"V"
    } else if (d[e]=="GTG"){
      g<-"V"
    } else if (d[e]=="TCT"){
      g<-"S"
    } else if (d[e]=="TCC"){
      g<-"S"
    } else if (d[e]=="TCA"){
      g<-"S"
    } else if (d[e]=="TCG"){
      g<-"S"
    } else if (d[e]=="CCT"){
      g<-"P"
    } else if (d[e]=="CCC"){
      g<-"P"
    } else if (d[e]=="CCA"){
      g<-"P"
    } else if (d[e]=="CCG"){
      g<-"P"
    } else if (d[e]=="ACT"){
      g<-"T"
    } else if (d[e]=="ACC"){
      g<-"T"
    } else if (d[e]=="ACA"){
      g<-"T"
    } else if (d[e]=="ACG"){
      g<-"T"
    } else if (d[e]=="GCT"){
      g<-"A"
    } else if (d[e]=="GCC"){
      g<-"A"
    } else if (d[e]=="GCA"){
      g<-"A"
    } else if (d[e]=="GCG"){
      g<-"A"
    } else if (d[e]=="TAT"){
      g<-"Y"
    } else if (d[e]=="TAC"){
      g<-"Y"
    } else if (d[e]=="TAA"){
      g<-" "
    } else if (d[e]=="TAG"){
      g<-" "
    } else if (d[e]=="TGA"){
      g<-" "
    } else if (d[e]=="CAT"){
      g<-"H"
    } else if (d[e]=="CAC"){
      g<-"H"
    } else if (d[e]=="CAA"){
      g<-"Q"
    } else if (d[e]=="CAG"){
      g<-"Q"
    } else if (d[e]=="AAT"){
      g<-"N"
    } else if (d[e]=="AAC"){
      g<-"N"
    } else if (d[e]=="AAA"){
      g<-"K"
    } else if (d[e]=="AAG"){
      g<-"K"
    } else if (d[e]=="GAT"){
      g<-"D"
    } else if (d[e]=="GAC"){
      g<-"D"
    } else if (d[e]=="GAA"){
      g<-"E"
    } else if (d[e]=="GAG"){
      g<-"E"
    } else if (d[e]=="TGT"){
      g<-"C"
    } else if (d[e]=="TGC"){
      g<-"C"
    } else if (d[e]=="TGG"){
      g<-"W"
    } else if (d[e]=="CGT"){
      g<-"R"
    } else if (d[e]=="CGC"){
      g<-"R"
    } else if (d[e]=="CGA"){
      g<-"R"
    } else if (d[e]=="CGG"){
      g<-"R"
    } else if (d[e]=="AGT"){
      g<-"S"
    } else if (d[e]=="AGC"){
      g<-"S"
    } else if (d[e]=="AGA"){
      g<-"R"
    } else if (d[e]=="AGG"){
      g<-"R"
    } else if (d[e]=="GGT"){
      g<-"G"
    } else if (d[e]=="GGC"){
      g<-"G"
    } else if (d[e]=="GGA"){
      g<-"G"
    } else if (d[e]=="GGG"){
      g<-"G"
    } else {
      g<-"*"
    }
    h<-c(h,g)  #put converted amino acids into string vector
  }
  j<-paste(h,seq="",collapse="") #paste them together
  k<-gsub(" ","",j) #clear the blanks
  list<- list("aa_number"=length(unlist(strsplit(k,""))), "length_of_gene" = b, "aa_seq" = k)
  return(list)
}

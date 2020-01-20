library(RepSeq)
#library(docstring)
library("argparse")
library('data.table')

# get file from directory
main<-function(){
  # example from vignette
  parser <- ArgumentParser(description='Process some integers')
  parser$add_argument('directory', type="character",help='file directory')
  parser$add_argument('--level', type="character", dest='level', help='V J VpJ')
  # parser$add_argument('--alpha', type="integer", nargs='+',default=c(0, 0.25, 0.5, 1.00001, 2, 4, 8, 16, 32, 64,Inf) ,help='an integer for the accumulator')
  
  args=parser$parse_args()
  print(args$directory)
  
  inputFolder <- list.files(args$directory, full.name = TRUE, pattern = ".txt")
  # #read MIXCR file and create a repseq object
  datatab <- readClonotypeSet(inputFolder, cores=2L, aligner="MiXCR", chain="A", sampleinfo=NULL, keep.ambiguous=FALSE, keep.unproductive=FALSE, aa.th=8)
  # # write to a file
  write.csv(datatab@sampleData,"sample_info.csv")
  print( typeof(basicIndices(datatab, "CDR3aa")))
  # # write to a file
  write.csv(basicIndices(datatab, args$level),"diversity_info.csv") 
  res<-renyiProfiles(datatab, level=args$level)
  write.csv(t(res),"renyi_Profile.csv") 
  
}
main()




basicIndices <- function(x, level=c("VpJ", "V", "J", "VJ", "CDR3aa")) {
  "
  
  
  "
  if (missing(x)) stop("x is missing.")
  if (!is.RepSeqExperiment(x)) stop("an object of class  is expected.")
  levelChoice <- match.arg(level)
  # summary function
  pastek <- function(y) list(shannon=.diversity(y, index="shannon"), simpson=.diversity(y, index="simpson"), berger_parker=.diversity(y, index="berger_parker"),
                             invsimpson=.diversity(y, index="invsimpson"),pielou=.diversity(y, index="pielou"),richness=.diversity(y, index="richness"), gini=.gini(y), 
                             chao1=.chao1(y)$chao.est, chao1.se=.chao1(y)$chao.se, ichao=iChao(y), chaowor=Chaowor(y))   
  out <- assay(x)[, .(count=sum(count)), by=c("lib", levelChoice)][, pastek(count), by="lib"]
  return(out)
}

diversity <- function(x, index=c("shannon", "simpson", "invsimpson", "berger_parker", "pielou", "richness"), norm=FALSE, base=exp(1)) {
  #  modified function of repseq package to add other diversity indices
  # compute: simpson, shannon, pielou, richness, gini, berger_parker, invsimpson
  # parameters:
  # x: a dataframe of count by sample ( lib)
  # index: diversity index to compute   
  # norm: boolean to normalise some diversity 
  # base: exp
  
  
  
  if (missing(x)) stop("data set x is required.")
  x <- x/sum(x)
  id <- match.arg(index)
 
  if (id == "shannon"|| id=="pielou") {
    x <- -x * log(x, base)
    
  }  else {
    x <- x * x
  }
  H <- sum(x, na.rm = TRUE)
  if (id=="pielou"){
    norm=FALSE
    H/log(length(x),base)
  }
  if (norm) H <- H/log(sum(x>0), base)
  if (id == "simpson") {
    H <- 1 - H
  } else if (id == "invsimpson") {
    H <- 1/H
  }
  if(id=="berger_parker"){
  H <- -log(max(x))
  }
  if (id=="richness"){
    H<-length(x)
  }
  return(H)
}


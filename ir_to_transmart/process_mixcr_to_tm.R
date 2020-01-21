library(RepSeq)
#library(docstring)
library("argparse")
library('data.table')

# get file from directory
main<-function(){
  # example from vignette
  parser <- ArgumentParser(description='Process some integers')
  parser$add_argument('directory', type="character",help='file directory')
  parser$add_argument('--level', type="character",default="VpJ", dest='level', help='V J VpJ')
  parser$add_argument('--chain', type="character",default="B", dest='chain', help='A B')
  
  # parser$add_argument('--alpha', type="integer", nargs='+',default=c(0, 0.25, 0.5, 1.00001, 2, 4, 8, 16, 32, 64,Inf) ,help='an integer for the accumulator')
  
  args=parser$parse_args()
  print(args$directory)
  
  inputFolder <- list.files(args$directory, full.name = TRUE, pattern = ".txt")
  # #read MIXCR file and create a repseq object
  datatab <- readClonotypeSet(inputFolder, cores=2L, aligner="MiXCR", chain=args$chain, sampleinfo=NULL, keep.ambiguous=FALSE, keep.unproductive=FALSE, aa.th=8)
  # # write to a file
  write.csv(datatab@sampleData,"sample_info.csv")
  print( typeof(basicIndices(datatab, "CDR3aa")))
  # # write to a file
  write.csv(basicIndices(datatab, args$level),"diversity_info.csv") 
  res<-renyiProfiles(datatab, level=args$level)
  write.csv(t(res),"renyi_Profile.csv") 
  
}
main()




basicIndicesA <- function(x, level=c("VpJ", "V", "J", "VJ", "CDR3aa")) {
  "
  
  
  "
  if (missing(x)) stop("x is missing.")
  if (!is.RepSeqExperiment(x)) stop("an object of class  is expected.")
  levelChoice <- match.arg(level)
  # summary function
  pastek <- function(y) list(shannon=diversityA(y, index="shannon"), simpson=diversityA(y, index="simpson"), berger_parker=diversityA(y, index="berger_parker"),
                             invsimpson=diversityA(y, index="invsimpson"),pielou=diversityA(y, index="pielou"),richness=diversityA(y, index="richness"), gini=gini(y), 
                             chao1=chao1(y)$chao.est, chao1.se=chao1(y)$chao.se, ichao=iChao(y), chaowor=Chaowor(y))   
  out <- assay(x)[, .(count=sum(count)), by=c("lib", levelChoice)][, pastek(count), by="lib"]
  return(out)
}

diversityA <- function(x, index=c("shannon", "simpson", "invsimpson", "berger_parker", "pielou", "richness"), norm=TRUE, base=exp(1)) {
  #  modified function of repseq package to add other diversity indices
  # compute: simpson, shannon, pielou, richness, gini, berger_parker, invsimpson
  # parameters:
  # @param x: a dataframe of count by sample ( lib)
  # @param index: diversity index to compute   
  # @param norm: boolean to normalise some diversity 
  # @param base: exponential 1 
  # @return the diversity indice needed for a repertoir
  
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
    H <-H/log(length(x),base)
  }
  if (norm) H <- H/log(sum(x>0), base)
  if (id == "simpson") {
    H <- 1 - H
  } else if (id == "invsimpson") {
    H <- 1/H
  }
  if(id=="berger_parker"){
  # berger parker
    H <- -log(max(x))
  }
  if (id=="richness"){
    #richness
    H<-length(x)
  }
  return(H)
}

gini <- function(x) {
  x <- sort(x)
  n <- length(x)
  out <- 1/n * (n + 1 - 2 * sum((n + 1 - 1:n) * x)/sum(x))
  out
}

# compute Chao1 indices
#
# function computes Chao indices
#
# @param x an object of class RepSeqExperiment
#
# 
chao1 <- function(x) {
  f1 <- sum(x == 1)
  f2 <- sum(x == 2)
  S <- sum(x > 0)
  #if (f2 > 0) est <- S + f1^2/(2*f2)
  #if (f2 == 0) est <- S + f1*(f1 - 1)/2
  est <- S + f1*(f1 - 1)/(2 * (f2 + 1))
  r <- f1/f2
  chao.var <- f2 * ((r^4)/4 + r^3 + (r^2)/2)
  chao.se <- sqrt(chao.var)
  #return(est)
  return(list(chao.est=est, chao.se=chao.se))
}

#.d50 <- function(x) {
#    
#
#}

#' Improved Chao1
#'
#' function computes the improve version of Chao1
#' @param x a vector of count.
#' @return improved Chao1 value (ref) Chao and Lin Chao, A. and Lin, C.-W. (2012) Nonparametric lower bounds for species richness and shared species richness under sampling without replacement. Biometrics,68, 912–921. 
#' @export
# @example
iChao <- function(x) {
  f1 <- sum(x == 1)
  f2 <- sum(x == 2)
  f3 <- sum(x == 3)
  f4 <- sum(x == 4)
  if (f4 == 0) f4 <- 1
  n <- sum(x)
  p1 <- (n-3)/n
  p2 <- (n-3)/(n-1)
  est <- chao1(x)$chao.est + p1*f3/(4*f4) * max(f1 - p2*f2*f3/(2*f4), 0)
  return(est)
}

#' Adjusted Chao1 for sampling without replacement 
#'
#' function compute the correct 
#' @param x a vector of counts
#' @return a value of Chao1
#' @export
# @example
Chaowor <- function(x) {
  f1 <- sum(x == 1)
  f2 <- sum(x == 2)
  S <- sum(x > 0)
  n <- sum(x)
  q <- n/S
  dn1 <- n*2*f2/(n-1)
  dn2 <- q*f1/(q-1)
  est <- S + (f1^2)/(dn1 + dn2)
  return(est)
}



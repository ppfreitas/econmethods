setwd("~/Dropbox/CIDE/Cheating_tests/Script_Tests")
library('CopyDetect')
library('mirt')


## Year data reading, change to 2009, 2010 or 2011
year <- 2011
dat <- read.csv(paste0('../',year,'.csv'), header = TRUE, stringsAsFactors = FALSE)

## Semester selection, either 2, 4 or 6
sem <- 2
datt <- subset(dat, semester == sem)
datt <- datt[,colSums(is.na(datt)) < nrow(datt)]

# schools
cct <- table(datt$clavecct)

# read exam key
key <- read.csv(paste0('key',semester,'_',year,'.csv'), header=FALSE, stringsAsFactors = FALSE)
key <- as.character(key[,1])
key[key == 'A'] <- '1'
key[key == 'B'] <- '2'
key[key == 'C'] <- '3'
key[key == 'D'] <- '4'
key <- as.numeric(key)

# calculate parameters, run this only one time then just read
#responses <- data.frame(key2binary(datt[datt$treatment=='C',-5:-1], key), stringsAsFactors = FALSE)
#est.ipar <- est(responses, model = "2PL", engine = "ltm")$est
#save(est.ipar, file = paste0('estipar',sem,'_',year,'.RData'))
#
load(paste0('estipar',sem,'_',year,'.RData'))

for(ii in 1:length(cct)){
  l.class <- table(datt$class[datt$clavecct == names(cct[ii])])
  for(j in 1:length(l.class)){
    if(l.class[j] > 1){
      aux <- subset(datt, clavecct == names(cct[ii]) & class == names(l.class[j]))
      responses <- data.frame(key2binary(aux[-5:-1], key), stringsAsFactors = FALSE)
      
      # construct pairs to be tested: The first element indicates the suspected copier examinee, and the second element indicates the suspected source examinee.
      pairs <- data.frame(expand.grid(X1 = 1:l.class[j], X2 = 1:l.class[j]), W=NA, GBT=NA, K=NA, K1=NA, K2=NA, S1=NA, S2=NA)
      pairs <- pairs[pairs$X1 != pairs$X2,]
      
      for(i in 1:dim(pairs)[1]){
        x <- CopyDetect1(data=responses, item.par=est.ipar, pair=c(pairs[i,1], pairs[i,2]))
        
        pairs[i,]$W=x$W.index$p.value
        pairs[i,]$GBT=x$GBT.index$p.value
        pairs[i,]$K=x$K.index$k.index
        pairs[i,]$K1=x$K.variants$K1.index
        pairs[i,]$K2=x$K.variants$K2.index
        pairs[i,]$S1=x$K.variants$S1.index
        pairs[i,]$S2=x$K.variants$S2.index
      }
      pairs[ ,1:2] <- subset(expand.grid(X1 = aux$folio, X2 = aux$folio), X1 != X2)
      #iteration to see where we are at
      print(ii/length(cct))
      print(j/length(l.class))
      
      name <- paste0(paste('Pairs', year, sem, names(cct[ii]), names(l.class[j]), sep = '_'),'.RData')
      save(pairs, file = name)
    }
  }
}

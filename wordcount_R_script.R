library(ggplot2)
library(zipfR)
library(gridExtra)

#need to substitute your path to the files to the variable path_to_files
path_to_files <- "~/wordcount/itds/train"
filenames <- list.files(path=path_to_files, pattern="*.csv")
names <- substr(filenames,1,12)
for(i in names){
  filepath <- file.path(path_to_files,paste(i,".csv",sep=""))
  assign(i, read.delim(filepath, header=TRUE,
                       colClasses=c("character","character","numeric"),
                       sep = ","))
}


#histograms (all in one plot) of raw data, first 10 highest frequency words
plot1 <- qplot(count, data=en.wordcount, geom="histogram",binwidth=1,xlim=c(1,10),xlab="bin",ylab="raw count",main= "en")
plot2 <- qplot(count, data=es.wordcount, geom="histogram",binwidth=1,xlim=c(1,10),xlab="bin",ylab="raw count",main= "es")
plot3 <- qplot(count, data=de.wordcount, geom="histogram",binwidth=1,xlim=c(1,10),xlab="bin",ylab="raw count",main= "de")
plot4 <- qplot(count, data=da.wordcount, geom="histogram",binwidth=1,xlim=c(1,10),xlab="bin",ylab="raw count",main= "da")
plot5 <- qplot(count, data=fr.wordcount, geom="histogram",binwidth=1,xlim=c(1,10),xlab="bin",ylab="raw count",main= "fr")
plot6 <- qplot(count, data=nl.wordcount, geom="histogram",binwidth=1,xlim=c(1,10),xlab="bin",ylab="raw count",main= "nl")
plot7 <- qplot(count, data=pl.wordcount, geom="histogram",binwidth=1,xlim=c(1,10),xlab="bin",ylab="raw count",main= "pl")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, ncol=3)

#function for creating a rank
Rank <- function(mydata, whichVar) {
  j <- unique(rev(sort(mydata[,whichVar])));  
  #use of anonymous function passed to sapply
  return(sapply(mydata[,whichVar],function(dd) which(dd==j)));
}

mylist <- list(en.wordcount = en.wordcount, es.wordcount = es.wordcount, da.wordcount = da.wordcount, de.wordcount = de.wordcount,
              fr.wordcount = fr.wordcount, pl.wordcount = pl.wordcount, nl.wordcount = nl.wordcount)  
xx <- sapply(names(mylist), function(x) Rank(mylist[[x]], "count"))

#add the ranks to the data frames
en.wordcount$rank <- unlist(with(xx, en.wordcount ))
es.wordcount$rank <- unlist(with(xx, es.wordcount ))
da.wordcount$rank <- unlist(with(xx, da.wordcount ))
de.wordcount$rank <- unlist(with(xx, de.wordcount ))
fr.wordcount$rank <- unlist(with(xx, fr.wordcount ))
nl.wordcount$rank <- unlist(with(xx, nl.wordcount ))
pl.wordcount$rank <- unlist(with(xx, pl.wordcount ))

#put all the data frames into one
words.train <- rbind(en.wordcount,es.wordcount,de.wordcount,da.wordcount,fr.wordcount,nl.wordcount,pl.wordcount)

#attempt log-log plot
plot(words.train$count[words.train$lang=="en"] ~ words.train$rank[words.train$lang=="en"], data = words.train, type="n", log = "xy", main = "Log-log Plot", xlab="rank", ylab="frequency")
abline( h = seq( 0, 500, 10 ), lty = 3, col = colors()[ 440 ] )
abline( v = seq( 0, 50, 10), lty = 3, col = colors()[ 440 ] )
points(words.train$count[words.train$lang=="en"] ~ words.train$rank[words.train$lang=="en"], data=words.train)

#create frequency spectrum matrices
freq.en <- tabulate(words.train$count[words.train$lang=="en"]) 
type.en <- 1:(length(freq.en))
fs.en <- data.frame(type.en,freq.en)

freq.es <- tabulate(words.train$count[words.train$lang=="es"])
type.es <- 1:(length(freq.es))
fs.es <- data.frame(type.es,freq.es)

freq.da <- tabulate(words.train$count[words.train$lang=="da"])
type.da <- 1:(length(freq.da))
fs.da <- data.frame(type.da,freq.da)

freq.de <- tabulate(words.train$count[words.train$lang=="de"])
type.de <- 1:(length(freq.de))
fs.de <- data.frame(type.de,freq.de)

freq.fr <- tabulate(words.train$count[words.train$lang=="fr"])
type.fr <- 1:(length(freq.fr))
fs.fr <- data.frame(type.fr,freq.fr)

freq.nl <- tabulate(words.train$count[words.train$lang=="nl"])
type.nl <- 1:(length(freq.nl))
fs.nl <- data.frame(type.nl,freq.nl)

freq.pl <- tabulate(words.train$count[words.train$lang=="pl"])
type.pl <- 1:(length(freq.pl))
fs.pl <- data.frame(type.pl,freq.pl)
 
head(fs.en)
head(fs.es)
head(fs.da)
head(fs.de)
head(fs.fr)
head(fs.nl)
head(fs.pl)

#zipfR includes handy utils to covert type frequency objects (in this case, the "count" column) to frequency spectra
??tfl2spc
??tfl
en.tfl <- tfl(en.wordcount[,c(3)])
es.tfl <- tfl(es.wordcount[,c(3)])
da.tfl <- tfl(da.wordcount[,c(3)])
de.tfl <- tfl(de.wordcount[,c(3)])
fr.tfl <- tfl(fr.wordcount[,c(3)])
nl.tfl <- tfl(nl.wordcount[,c(3)])
pl.tfl <- tfl(pl.wordcount[,c(3)])


#dfList <- list(en.tfl, es.tfl)
#dfList <- lapply(dfList, function(df) {
#  nm <-deparse(substitute(df))
#  print(nm)
#  summary(df)
#  } )
                         
#tfls <- c("en.tfl","es.tfl","da.tfl","de.tfl","fr.tfl","nl.tfl","pl.tfl")
#tfls
#for (t in list(tfls) )
#{ summary(t) }

summary(es.tfl)
summary(da.tfl)
summary(de.tfl)
summary()

plot(sort(en.tfl$f,decreasing=TRUE), log="xy",xlab="rank",ylab="frequency",main="en")
plot(sort(es.tfl$f,decreasing=TRUE),log="xy",xlab="rank",ylab="frequency",main="es")
plot(sort(da.tfl$f,decreasing=TRUE),log="xy",xlab="rank",ylab="frequency",main="da")
plot(sort(de.tfl$f,decreasing=TRUE),log="xy",xlab="rank",ylab="frequency",main="de")
plot(sort(fr.tfl$f,decreasing=TRUE),log="xy",xlab="rank",ylab="frequency",main="fr")
plot(sort(nl.tfl$f,decreasing=TRUE),log="xy",xlab="rank",ylab="frequency",main="nl")
plot(sort(pl.tfl$f,decreasing=TRUE),log="xy",xlab="rank",ylab="frequency",main="pl")

qplot(en.tfl$f, data=en.tfl, binwidth=1, log="xy", xlab="rank",ylab="frequency",main= "en")

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, ncol=3)

en.spc <- tfl2spc(en.tfl) #cool conversion to spectrum, with m, Vm, N, and V.
es.spc <- tfl2spc(es.tfl)
da.spc <- tfl2spc(da.tfl)
de.spc <- tfl2spc(de.tfl)
fr.spc <- tfl2spc(fr.tfl)
nl.spc <- tfl2spc(nl.tfl)
pl.spc <- tfl2spc(pl.tfl)

summary(en.spc)

#compute productivity of the train dataset (hapax legomena/sample size). 
Vm(en.spc,1) / N(en.spc)

plot(en.spc,m.max=10, log="x") #vx (number of words), y (frequency class)
zm <- lnre("zm", da.train.spc)
zm

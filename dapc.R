library(pegas)
library(adegenet)
library(poppr)
ssr<- read.csv2("C:/Users/glane/Downloads/poppr_all.csv",header = TRUE,sep = ",")
View(ssr)

ssr$PgtCAT4_2 <- paste(ssr$PgtCAT4_2 , ";", ssr$X)
ssr$PgtCAA98 <- paste(ssr$PgtCAA98  , ";", ssr$X.1)
ssr$Pgest24 <- paste(ssr$Pgest24 ,";", ssr$X.2)
ssr$Pgest325 <- paste(ssr$Pgest325 ,";", ssr$X.3)
ssr$PgtCAA39 <- paste(ssr$PgtCAA39 ,";", ssr$X.4)
ssr$Pgest227 <- paste(ssr$Pgest227 ,";", ssr$X.5)
ssr$Pgest341 <- paste(ssr$Pgest341 ,";", ssr$X.6)
ssr$PgtCAA53 <- paste(ssr$Pgest24 ,";", ssr$X.7)
ssr$PgtCAA80 <- paste(ssr$PgtCAA80 ,";", ssr$X.8)
ssr$Pgest21 <- paste(ssr$Pgest21 ,";", ssr$X.9)
ssr$PgtCAA49 <- paste(ssr$PgtCAA49 ,";", ssr$X.10)
ssr$PgtCAA93 <- paste(ssr$PgtCAA93 ,";", ssr$X.11)
ssr$Pgest109 <- paste(ssr$Pgest109 ,";", ssr$X.12)
ssr$Pgest173 <- paste(ssr$Pgest173 ,";", ssr$X.13)
ssr$PgtGAA8_1 <- paste(ssr$PgtGAA8_1 ,";", ssr$X.14)
ssr$Pgest142 <- paste(ssr$Pgest142 ,";", ssr$X.15)
ssr$Pgest59 <- paste(ssr$Pgest59 ,";", ssr$X.16)
ssr$Pgest353 <- paste(ssr$Pgest353 ,";", ssr$X.17)
ssr$Pgest318 <- paste(ssr$Pgest318 ,";", ssr$X.18)


ssr<-ssr %>% select(Isolate.id,Host,Area,Origin,Country,starts_with("P") )
View(ssr)

loci

obj <- df2genind(ssr,ncode = 2, ploidy = 2)
obj

is.genind(obj)

#splitStrata(obj)


#cln <- clonecorrect(obj, strata = ~Tree/Year, keep = 1:2)
#cln

prin_comp <- prcomp(obj, scale. = T)
prin_comp

#19PCs


clust<-find.clusters(obj,max.n.clust=40)
dapc1 <- dapc(obj, clust$grp)
scatter(dapc1)

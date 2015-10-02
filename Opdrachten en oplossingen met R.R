# Opdrachten en oplossingen

C = read.table(file="oec_vvs_komma.csv", sep = ";", dec = ",",header = TRUE,na.strings = "NA")

### afstanden: meest dichtbije
min(C$distsun, na.rm = TRUE) # parsec
3.26163344 * min(C$distsun, na.rm = TRUE) # lichtjaar
# welke is dat?
C[which.min(C$distsun),]
# info: http://www.openexoplanetcatalogue.com/planet/Alpha%20Centauri%20B%20b

Chabit = subset(C,inhabitzone==TRUE)
Chabit[which.min(Chabit$distsun),]
Chabit[which.min(Chabit$distsun),"distsun"]
Chabit[which.min(Chabit$distsun),"distsun"] * 3.26163344
# online meer info: http://www.openexoplanetcatalogue.com/planet/Kapteyn%20b/

### massa van planeten

mean(C$plntmass, na.rm = TRUE)
median(C$plntmass, na.rm = TRUE)

sum(C$plntmass[!is.na(C$plntmass)] < 1/317.83)

Ctransit = subset(C, discmeth == "transit")
min(Ctransit$plntmass, na.rm = TRUE)
317.83 * min(Ctransit$plntmass, na.rm = TRUE)
Ctransit[which.min(Ctransit$plntmass),]

### histogram
library(ggplot2)

ggplot(subset(C,discyear>1990), aes(as.factor(discyear), fill=inhabitzone)) + 
   geom_bar(binwidth=0.5, position="dodge") 

### postities in plot.ly gedaan

### ons zonnestelsel:
C$zonnestelsel = FALSE
planeten = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn",
            "Uranus", "Neptune", "Pluto")
C$zonnestelsel[C$plntid %in% planeten] = TRUE
xtabs(~zonnestelsel,C)
ggplot(C, aes(log(plntmass), log(period), colour=zonnestelsel)) +
   geom_point(size=5, alpha=0.4) + 
   geom_point(data = subset(C, zonnestelsel), aes(log(plntmass), log(period)), size=1, colour=I("black")) 



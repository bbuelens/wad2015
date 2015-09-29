# Neem een lokale kopie van de catalogus en bewaar die hier. 
# Doe al een aantal voorbewerkingen en selecties.

# Websites:
# http://www.openexoplanetcatalogue.com/
# https://github.com/OpenExoplanetCatalogue/open_exoplanet_catalogue

urlOec = "https://raw.githubusercontent.com/OpenExoplanetCatalogue/oec_tables/master/comma_separated/open_exoplanet_catalogue.txt"

C = read.table(urlOec, header=FALSE, sep =",", dec=".", comment.char="#")

class(C)
dim(C)

head(C)

#  1: Primary identifier of planet
#  2: Binary flag [0=no known stellar binary companion; 1=P-type binary (circumbinary); 2=S-type binary; 3=orphan planet (no star)]
#  3: Planetary mass [Jupiter masses]
#  4: Radius [Jupiter radii]
#  5: Period [days]
#  6: Semi-major axis [Astronomical Units]
#  7: Eccentricity
#  8: Periastron [degree]
#  9: Longitude [degree]
# 10: Ascending node [degree]
# 11: Inclination [degree]
# 12: Surface or equilibrium temperature [K]
# 13: Age [Gyr]
# 14: Discovery method
# 15: Discovery year [yyyy]
# 16: Last updated [yy/mm/dd]
# 17: Right ascension [hh mm ss]
# 18: Declination [+/-dd mm ss]
# 19: Distance from Sun [parsec]
# 20: Host star mass [Solar masses]
# 21: Host star radius [Solar radii]
# 22: Host star metallicity [log relative to solar]
# 23: Host star temperature [K]
# 24: Host star age [Gyr]

names = c("plntid", "binary", "plntmass", "plntradius", "period", "smaxis",
          "eccent", "periast", "longit", "ascnode", "inclin", "plnttemp",
          "plntage", "discmeth", "discyear", "lastupdt", "ra", "dec", 
          "distsun", "strmass", "strradius", "strmetal", "strtemp", "strage")
length(names)

names(C) = names

select = c("plntid", "binary", "plntmass", "plntradius", "period", "smaxis",
           # "eccent", "periast", "longit", "ascnode", "inclin", 
           "plnttemp",
           "plntage", "discmeth", "discyear", 
           # "lastupdt", 
           "ra", "dec", 
           "distsun", "strmass", "strradius", "strmetal", "strtemp", "strage")

C = C[, select]

# binary?
xtabs(~binary,C)
C = C[C$binary != 3,]
bin = rep(1, nrow(C))
bin[C$binary == 0] = 0
table(bin)
C$binary = bin
rm(bin)

# Is exoplanet in habitable zone?
# Code based on oec_web/oec_filters.py.
# Ref. http://adsabs.harvard.edu/abs/2007A%26A...476.1373S
isHabitable = function(semimajoraxis, startemp, starmass) { 
   starmass[is.na(starmass)] = 1
   lumin = starmass^(4.0)
   lumin[starmass > 2.0] = (starmass^(3.5))[starmass > 2.0]
   rel_temp = startemp - 5700
   Hzin = (0.68 - 2.7619e-5 * rel_temp - 3.8095e-9 * rel_temp^2) * sqrt(lumin)
   Hzout = (1.95 - 1.3786e-4 * rel_temp - 1.4286e-9 * rel_temp^2) * sqrt(lumin)
   return(semimajoraxis > Hzin & semimajoraxis < Hzout)
}

# semi major axis can be computed when missing
starM = C$strmass
smaMissing = is.na(C$smaxis) & !is.na(C$period)
sum(smaMissing)
starM[is.na(starM)] = 1
C$smaxis[smaMissing] = (((C$period[smaMissing]/6.283/365.25)^2) * 39.49 / (starM[smaMissing]))^(1/3)

allKnown = !is.na(C$smaxis) & !is.na(C$strtemp) # starmass gets set to 1 if NA
length(allKnown)
sum(allKnown)
C$inhabitzone = FALSE
C$inhabitzone[allKnown] = isHabitable(C$smaxis[allKnown], C$strtemp[allKnown], C$strmass[allKnown])
sum(C$inhabitzone)

# ra en dec numeriek maken
ra = levels(C$ra)[as.integer(C$ra)]
C$plntid[nchar(ra)==0]
hh = as.integer(substr(ra,1,2))
mm = as.integer(substr(ra,4,5))
ss = as.numeric(substr(ra,7,nchar(ra)))
ranum = (360 * hh/24) + (15 * mm/60) + (15 * ss/3600)

dec = levels(C$dec)[as.integer(C$dec)]
decsgn = substr(dec,1,1)
dd = as.integer(substr(dec,2,3))
mm = as.integer(substr(dec,5,6))
ss = as.numeric(substr(dec,8,nchar(ra)))
decnum = dd + (mm/60) + (ss/3600)
decnum[decsgn == "-"] = -1 * decnum[decsgn == "-"] 

C$ra = ranum
C$dec = decnum

# bestand schrijven als csv met ; als scheidingsteken voor excel
write.table(C, file = "oec_vvs_excel.csv", quote = FALSE, sep = ";", dec = ",", row.names = FALSE, col.names = TRUE, na = "")

# voor plotly:
write.table(C, file = "oec_vvs_plotly.csv", quote = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE, na = "")



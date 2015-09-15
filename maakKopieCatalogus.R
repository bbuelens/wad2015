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

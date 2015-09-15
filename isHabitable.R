# Is exoplanet in habitable zone?
# Code based on oec_web/oec_filters.py.

isHabitable = function()


def isHabitable(xmlPair):
   system, planet, star, filename = xmlPair
maxa = 0
if star is None:
   return False # no binary systems (yet)
semimajoraxis = getFloat(planet,"./semimajoraxis")
if semimajoraxis is None:
   hostmass = getFloat(star,"./mass",1.)
period = getFloat(planet,"./period",265.25)
semimajoraxis = pow(pow(period/6.283/365.25,2)*39.49/hostmass,1.0/3.0) 

temperature = getFloat(star,"./temperature")
spectralTypeMain = getText(star,"./spectraltype","G")[0]
if temperature is None:
   if spectralTypeMain=="O":
   temperature = 40000
if spectralTypeMain=="B":
   temperature = 20000
if spectralTypeMain=="A":
   temperature = 8500
if spectralTypeMain=="F":
   temperature = 6500
if spectralTypeMain=="G":
   temperature = 5500
if spectralTypeMain=="K":
   temperature = 4000
if spectralTypeMain=="M":
   temperature = 3000

rel_temp = temperature - 5700.

stellarMass = getFloat(star,"./mass")
if stellarMass is None:
   stellarMass = 1.

stellarRadius = getFloat(star,"./radius")
if stellarRadius is None or stellarRadius<0.01:
   stellarRadius = 1.
if spectralTypeMain=='O': 
   stellarRadius=10.
if spectralTypeMain=='B': 
   stellarRadius=3.0
if spectralTypeMain=='A': 
   stellarRadius=1.5
if spectralTypeMain=='F': 
   stellarRadius=1.3
if spectralTypeMain=='G': 
   stellarRadius=1.0
if spectralTypeMain=='K': 
   stellarRadius=0.8
if spectralTypeMain=='M': 
   stellarRadius=0.5

if stellarMass>2.:
   luminosity = pow(stellarMass,3.5)
else:
   luminosity = pow(stellarMass,4.)

HZinner2 = (0.68-2.7619e-9*rel_temp-3.8095e-9*rel_temp*rel_temp) *sqrt(luminosity);
#HZinner = (0.95-2.7619e-9*rel_temp-3.8095e-9*rel_temp*rel_temp) *sqrt(luminosity);
#HZouter = (1.67-1.3786e-4*rel_temp-1.4286e-9*rel_temp*rel_temp) *sqrt(luminosity);
HZouter2 = (1.95-1.3786e-4*rel_temp-1.4286e-9*rel_temp*rel_temp) *sqrt(luminosity);

if semimajoraxis>HZinner2 and semimajoraxis<HZouter2:
   return True
return False
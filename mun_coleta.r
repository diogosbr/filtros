

# instalando pacotes
packages = c( "rgdal", "raster", "maptools", "dismo")
for (p in setdiff(packages, installed.packages()[, "Package"])) {
  install.packages(p, dependencies = T)
}

#lendo pacotes
require(rgdal)
require(raster)
require(maptools)
require(dismo)

#Lendo os pontos 
#
manimax=gbif("Manilkara", "maxima")
#especie, lon, lat, municipio
pts=manimax[,c("species","lon","lat","municipality", "adm1")]
pts=na.exclude(pts)

#convertendo em um objeto 'spatial'
coordinates(pts)<- ~lon+lat

## Lendo o shape dos municípios do brasil
#br_mun=readOGR("./Shapes/brasil_mun_ibge/brasil_mun_ibge.shp")
br_mun=readShapeSpatial("./Shapes/brasil_mun_ibge/brasil_mun_ibge.shp")

br_mun$NOMEMUNICP
br_mun$NOMEUF

#atribuinto projeções aos shapes e aos pontos
proj4string(br_mun) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(pts) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#criando um data frame
pts1=as.data.frame(pts)

#extraindo dados dos shapes a partir dos pontos
muni_shape=over(pts,br_mun)[,c('NOMEMUNICP','NOMEUF')]
muni_shape[,1]=as.vector(muni_shape[,1])
muni_shape[,2]=as.vector(muni_shape[,2])
pts1=cbind(pts1,muni_shape)

for(i in 1:dim(pts1)[1]){
  pts1[,i]=tolower(pts1[,i])
}


pts1

pts1$municipality==pts1$NOMEMUNICP
pts1$adm1==pts1$NOMEUF





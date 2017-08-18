

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

micomira=gbif("Miconia", "mirabilis")
#especie, lon, lat, municipio
pts=manimax[,c("species","lon","lat","municipality", "adm1")]
pts=micomira[,c("species","lon","lat","municipality", "adm1")]
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

#função de Athos (https://pt.stackoverflow.com/questions/46473/remover-acentos)
rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}

pts1$municipality=rm_accent(pts1$municipality)

pts1$municipality==pts1$NOMEMUNICP
pts1$adm1==pts1$NOMEUF


pts1$filt="Ok"

for(i in 1:dim(pts1)[1]){
  if((pts1$municipality==pts1$NOMEMUNICP)[i]==FALSE){
    pts1[i,"filt"]="suspeito"
  }
    }

pts1

pts1$filt[pts1$municipality==pts1$NOMEMUNICP,]="ok"



#testando a função filt_municip ####

manimax=gbif("Manilkara", "maxima")
#especie, lon, lat, municipio
pts=manimax[,c("species","lon","lat","municipality", "adm1")]

micomira=gbif("Miconia", "mirabilis")
#especie, lon, lat, municipio
pts=micomira[,c("species","lon","lat","municipality", "adm1")]
pts1=na.exclude(pts)

source("https://raw.githubusercontent.com/diogosbr/filtros/master/filt_municip.R")
source("https://raw.githubusercontent.com/diogosbr/filtros/master/coord_invertida.R")

pts1
names(pts1)=c("species"   ,   "lat"     ,     "lon"      ,    "municipality" ,"adm1" )

teste=filt_municip(pts1)
head(teste)

teste2=invert(teste)

head(teste2)


pts1[1,2]=(-22.959222)
pts1[1,3]=(-43.27772)

pts1[2,2]=(41)
pts1[2,3]=(12)

teste3=filt(pts1)
teste3




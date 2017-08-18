
#Função para marcar ocorrências com município informado diferente da coordenada.

filt_municip = function(pts, shape.municipios){
  # instalando pacotes
  packages = c( "rgdal", "raster", "maptools", "dismo")
  for (p in setdiff(packages, installed.packages()[, "Package"])) {
    install.packages(p, dependencies = T)
  }
  
  #lendo pacotes
  require(rgdal)
  require(raster)
  #require(maptools)
  require(dismo)
  
  #pts=manimax[,c("species","lon","lat","municipality", "adm1")]
  pts=na.exclude(pts)
  
  #convertendo em um objeto 'spatial'
  coordinates(pts)<- ~lon+lat
  
  if(missing(shape.municipios)){
    br_mun=readOGR("./Shapes/brasil_mun_ibge/brasil_mun_ibge.shp")
  } else(br_mun=shape.municipios)
  
  #br_mun=readShapeSpatial("./Shapes/brasil_mun_ibge/brasil_mun_ibge.shp")
  
  #atribuinto projeções aos shapes e aos pontos
  br_mun <- spTransform(br_mun, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  proj4string(pts) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #criando um data frame
  pts1=as.data.frame(pts)
  
  #extraindo dados dos shapes a partir dos pontos
  muni_shape=over(pts,br_mun)[,c('NOMEMUNICP','NOMEUF')]
  muni_shape[,1]=as.vector(muni_shape[,1])
  muni_shape[,2]=as.vector(muni_shape[,2])
  pts1=cbind(pts1,muni_shape)
  pts1[,4]=as.vector(pts1[,4])
  pts1[,5]=as.vector(pts1[,5])
  
  for(i in 4:dim(pts1)[2]){
    pts1[,i]=tolower(pts1[,i])
    pts1[,i]=tolower(pts1[,i])
  }
  
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
  pts1$filt="Ok"
  
  for(i in 1:dim(pts1)[1]){
    if(is.na(pts1$municipality==pts1$NOMEMUNICP)[i]==TRUE){
      pts1[i,"filt"]="suspeito"
      pts1[i,"NOMEMUNICP"]="Fora do Brasil"
      pts1[i,"NOMEUF"]="Fora do Brasil"
    }
  }
  
  for(i in 1:dim(pts1)[1]){
    if((pts1$municipality==pts1$NOMEMUNICP)[i]==FALSE){
      pts1[i,"filt"]="suspeito"
    }
  }
  return(pts1)
}
  




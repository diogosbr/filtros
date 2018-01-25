
#Função para verificar se as coordenadas estão invretidas

invert = function(pts, shape.municipios){
  pts=pts[pts$filt=="suspeito",]
  
  #convertendo em um objeto 'spatial'
  #coordinates(pts)<- ~lon+lat
  coordinates(pts)<- ~lat+lon
  
  #criando um data frame
  pts1=as.data.frame(pts)
  
  if(missing(shape.municipios)){
    br_mun=readOGR("./Shapes/brasil_mun_ibge/brasil_mun_ibge.shp")
  } else(br_mun=shape.municipios)
  
  #atribuinto projeções aos shapes e aos pontos
  br_mun <- spTransform(br_mun, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  proj4string(pts) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
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
  
  for(i in 1:dim(pts1)[1]){
    if(is.na(pts1$municipality==pts1[,9])[i]==TRUE){
      pts1[i,"filt"]="suspeito"
      pts1[i,9]="Fora do Brasil"
      pts1[i,10]="Fora do Brasil"
    }
  }
  
  for(i in 1:dim(pts1)[1]){
    if((pts1$municipality==pts1[,9])[i]==TRUE){
      pts1[i,"filt"]="Invertidas"
    }
  }
  
  return(pts1)
}

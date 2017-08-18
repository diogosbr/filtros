# Filtros

##Filtro municipal

Função para marcar os registros que possuem o nome do municipio informado na etiqueta diferente do município adiquiridos através das coordenadas.

***filt_municip***


    filt_municip(pts, shape.municipios)



pts: tabela com 5 colunas, sendo:
 
> - *"species"* nome da espécie
> - *"lon" e "lat"*  longitude e latitude
> - *"municipality"* nome do município
> - *"adm1"* nome do estado

shape.municipios: o shape dos municípios do brasil do IBGE. Se não for informado, é carregado o shape do repositório. 
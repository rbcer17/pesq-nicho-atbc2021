library(rgdal)
library(raster)
library(dismo)

#delimitar uma projecao espacial para lat/long e para UTM:
longlat_WGS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


####################### 2. CARREGAR OS ARQUIVOS NECESSARIOS ########################
# Carregar os pontos de ocorrencia:
pontos_brutos = read.csv("C:/projetosR/Modelagem/analise/Crowned/Localities/GA_Verao.csv")
head(pontos_brutos)
str(pontos_brutos)

# Carregar a camada ambiental em 'asc' ja editada para servir como modelo
variavel = raster("C:/projetosR/Modelagem/analise/Crowned/Ms/Crowned/wc2.1_2.5m_bio_1.asc")
crs(variavel) = longlat_WGS
variavel
plot(variavel)

########################## 3. REMOVER PONTOS DUPLICADOS ############################
#numero de pontos brutos:
length(pontos_brutos[, 1]) #148

#remover duplicados
pontos_unicos = pontos_brutos[!duplicated(pontos_brutos[c("LONGITUDE","LATITUDE")]), ]

#numero de pontos unicos:
length(pontos_unicos[, 1]) #55





##################### 4. REMOVER PONTOS FORA DA AREA DE ESTUDO #####################
#numero de pontos unicos:
length(pontos_brutos[, 1]) #55

#selecionar apenas as colunas de lat e long:
names(pontos_unicos)
ocorrencia = pontos_unicos[,27:26] # lon/lat columns
head(ocorrencia)
str(ocorrencia)

#adicionar uma projecao
coordinates(ocorrencia) = ~LONGITUDE+LATITUDE #nome das colunas.
crs(ocorrencia) =  longlat_WGS

#extrair os valores da camada
valores = extract(variavel, ocorrencia)
head(valores)

#achar as posições onde não há valor, é um ponto fora da camada. NoData nos arquivos '.asc' é normalmente '-9999', mas você pode alterar o valor
i = which(valores != "-9999")
i #lines in the point_raw

#update the points raw and create a SpatialPoints for ocorrence points.
pontos_unicos_area = pontos_unicos
#numero de pontos restantes:
length(pontos_unicos_area[, 1]) #51





####### 5. REMOVER VIES AMOSTRAL QUE PODE LEVAR A AUTOCORRELACAO ESPACIAL ##########
#transformar os pontos em SpatialPoints
names(pontos_unicos_area)
ocorrencia = pontos_unicos_area[,27:26] # lon/lat columns
coordinates(ocorrencia) = ~LONGITUDE+LATITUDE #nome das colunas.
crs(ocorrencia) = longlat_WGS

#criar um buffer de 10Km ao redor dos pontos
buffer = circles(ocorrencia, d = 10000, lonlat=TRUE) #d é o raio do circulo em metros
plot(buffer)
class(buffer)

#converter os círculos em polígonos
buffer = polygons(buffer)
#rasterizar os circulos
buffer= rasterize(buffer, variavel)

#selecionar 1 ponto por cada circulo
sel = gridSample(ocorrencia, buffer, n=1)

#verificar o numero de pontos restantes:
length(pontos_unicos_area[,1]) #51
length(sel[,1]) #47

#salvar os pontos de ocorrencia corrigidos
sel = as.data.frame(sel)
write.csv(sel, "C:/projetosR/Modelagem/analise/Crowned/Localities/gaverao_corrigido.csv", row.names = FALSE)

##############################################################
# Downloading month temperature in a temporal series
# P. Prist, L. Tambosi, R. Muylaert
# Esse script é para extrair dados de Temperatura média por mês!
# Caso queira escolher outros dados, entre no site e escolha o caminho, e no fim vá para a aba expert mode
#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.GHCN_CAMS/.gridded/.deg0p5/.temp/#expert

#Data by source> Exemplo de T avg
## Na aba "Expert mode" escrever dentro do quadro 
# SOURCES .NOAA .NCEP .CPC .GHCN_CAMS .gridded .deg0p5 .temp
#  X -73.99046 -32.390935 RANGEEDGES
#  Y -33.74912 5.271891 RANGEEDGES
#  T (Jan 1993) (Dec 1995) RANGEEDGES
#  [X Y]average

# install.packages("rgdal")
# install.packages("RCurl")

rm(list=ls())

require(rgdal)
require(RCurl)
require(shapefiles)

# Read a polygon shapefile

mun15 <- readOGR("D://Coworks//Leptospirose//SP_muni.shp")

#Veja a extensão do shape

mun15@bbox

# Extraindo os polígonos em um for pela ID do município

#str(mun15@data[mun15@data$CD_GEOCMU=="1100015",])

quero <- as.character(mun15@data$codmunic)

i <- NULL

#i<-"1304401"
#Gere vários shapes para cada polígono que você quer extrair o dado zonal

setwd("D://Coworks//Leptospirose//zonal_covariates//NOAA_sp//")

	for(i in quero){
	print(i)	
	paste(i)
	shape <- mun15[mun15@data$codmunic==i,]
	writeOGR(shape, ".", paste("",i, sep=""), driver="ESRI Shapefile",
		overwrite_layer= TRUE) 
		}

################################################
#### indicar a pasta de trabalho e o periodo a ser analisado
# Timespan
begin <- 2000
end <- 2016

getwd()
####Vá na aba Data Files e clique em Text, copie o link de endereço eletrônico:
#Divida o link em partes, só removendo as coordenadas do bounding box
# Set data

part1<-"http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.GHCN_CAMS/.gridded/.deg0p5/.temp/X"
part2<-"RANGEEDGES/Y"
part3<-"RANGEEDGES/T/(Jan%202000)/(Dec%202016)/RANGEEDGES/%5BX/Y%5Daverage/data.ch"
n=1

################
#L ista de shapes

workdir <- "D://Coworks//Leptospirose//zonal_covariates//NOAA_sp//"

files_list <- as.data.frame(dir(workdir,pattern="*.shp"))

nrow(files_list)
dim(files_list)
head(table(files_list))
tail(table(files_list))

as.data.frame(NULL)->dados

# D:\Coworks\Leptospirose\zonal_covariates\NOAA_sp

for(n in 1:nrow(files_list))
	{
	#poly <- readOGR(paste(gsub('.dbf', '', files_list[n,1]), ".shp", sep=""))
	poly <- readOGR(paste(files_list[n,1]))
	#poly <- spTransform(poly, CRS=CRS("+proj=longlat +datum-WGS84 +towgs84"))
	tab <-		as.data.frame(as.numeric(as.character(	gsub(" ","",as.data.frame(strsplit(getURL(paste(part1,bbox(poly)[1,1],bbox(poly)[1,2],part2,bbox(poly)[2,1],bbox(poly)[2,2],part3, sep="/")),"\n"))[,1]))))
	tab$ano <- rep(seq(begin,end, 1), each=12)
	tab$mes <- rep(c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"), each=1)
	tab$codmunic <- gsub('.shp', '', files_list[n,1])
	colnames(tab) <- c("Temp","year","month","CD_GEOCMU")
	rbind(dados, tab)-> dados
	rm(poly)
	print(n)
	
	}

getwd()
head(dados)
tail(dados)

write.csv(dados, "SP_zonal_NOAA_temp_avg_2018_09_D21.csv", row.names= FALSE)

save.image(file="SP_zonal_NOAA_temp_avg_2018_09_D21.RData") 

#lista_cod <- mun15@data$CD_GEOCMU
#dir.create("../2018_NOAA_mun15")
#setwd("D://afericao_luke//integracao//NOAA//")
#diroutput <-"D://afericao_luke//integracao//NOAA//"

# Integration

#setwd("D:/Coworks/Leptospirose/zonal_covariates/NOAA_sp")
load("SP_zonal_NOAA_temp_avg_2018_09_D21.RData")


tail(table(dados$CD_GEOCMU, dados$year))
# 5300108

# Paulinha, descobri o pobrema do script do NOAA.. na real nao tinha problema hahaha..
# O que aconteceu é que minha tabela final tinha 1.6 milhoes de linhas, mas o csv lido no excel só le 1 milhao de linhas no meu pc. Entao eu nao via as outras linhas :)

nrow(dados) # 1.604.736 
# 1.6 milion lines

#######################
#     Spread         # 
######################

library(dplyr)
library(tidyr)

dados$month_year_mean <- paste(dados$month, dados$year, sep="_")
dados$month_year_mean

dados <- subset(dados, select= - c(month, year))

sum(is.na(dados$Temp))

head(dados)

dados_spread <- dados %>% spread(month_year_mean , Temp, drop= TRUE)

head(dados_spread)
ncol(dados_spread) 

hist(dados_spread$APR_1993)
summary(dados_spread)
# paul is awesome

# Rounding this

for( c in colnames(dados_spread[2: length(colnames(dados_spread))]))
{
  print(c)
  
  dados_spread[c] <- round(dados_spread[c], digits=2)
  #dados_spread[c] <- as.integer(dados_spread[,c])

  }

head(dados_spread)

tail(dados_spread)

str(dados_spread)

# Making mean temperature by month

names(dados_spread) <- gsub(x = names(dados_spread), pattern = "_", replacement = "y")  
names(dados_spread)

# convert to a data.table
require(data.table)

str(colnames(dados_spread))

DT <- data.table(dados_spread)

# the indices we wish to group
.index <- paste0("y",1993:2016)

str(.index)
#

# a list containing the names

name_list <- mapply(grep, pattern = as.list(.index ), MoreArgs = list(x= names(DT),value=T ), SIMPLIFY=F)
str(name_list)

# create the expression

name_list

# replacing _ by y helped .e to run..............
# DNT USE AS INDEX AN UNDERLINE NOR A NUMBER. Shit

.e <- parse(text= sprintf('list( %s)', paste(mapply(sprintf,  .index, lapply(name_list, paste, collapse = ', '), MoreArgs = list(fmt = '%s = mean(c(%s), na.rm = T)')), collapse = ',')))


DT[, eval(.e),by= CDyGEOCMU]

mean_temp <- DT[, eval(.e),by= CDyGEOCMU]

mean_temp_df <- as.data.frame(mean_temp)

###########################################
# Rounding MEAN TEMP

for( c in colnames(mean_temp_df [2: length(colnames(mean_temp_df ))]))
{
  print(c)
  
  mean_temp_df [c] <- round(mean_temp_df[c], digits = 2)
  #mean_temp_df[c] <- as.integer(mean_temp_df[,c])

  }

head( mean_temp_df)

# Round or not round?

summary(mean_temp_df$y1993)

library(plyr) 

# rename column
mean_temp_df <- rename(mean_temp_df, c('CDyGEOCMU'='CD_GEOCMU'))
mean_temp_dados_spread <- rename(dados_spread, c('CDyGEOCMU'='CD_GEOCMU'))

head(dados_spread)

head(mean_temp_df)

temp_spread_complete <- merge(mean_temp_df, mean_temp_dados_spread , by= c("CD_GEOCMU") )

head(temp_spread_complete )

names(temp_spread_complete) <- gsub(x = names(temp_spread_complete), pattern = "y", replacement = "t")  

head(temp_spread_complete)
# 

write.csv(temp_spread_complete,"CSV_temp_spread.csv", row.names = FALSE )
nrow(temp_spread_complete)



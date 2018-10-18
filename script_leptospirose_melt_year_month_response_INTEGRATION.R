# Integration with rainfall

setwd("D://Coworks//Leptospirose//zonal_covariates//rainfall//")
diroutput <- "D://Coworks//Leptospirose//zonal_covariates//rainfall//"

#dados em mm/day
#dados em mm/day
#dados em mm/day
#dados em mm/day
#dados em mm/day
#dados em mm/day
#dados em mm/day
#dados em mm/day
#dados em mm/day
#dados em mm/day
# Checar dados se baixou certinho

hist(dados$Pluv)

load("SP_zonal__ver2_prcp_est_2018_10_D12.RData")
setwd("D://Coworks//Leptospirose//input//year_month//")

pmelt <- read.table("cases_lepto_monthly_0016.cas", header=TRUE)


head(dados)
dados$Month_dec <- rep(1:12)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
dados$Year <- substrRight(dados$month_year_mean, 4)

dados$year_month <- factor(paste(dados$Year, dados$Month_dec, sep="/"))
names(dados) <- c("Pluv"  , "CODMUNIC"  ,     "month_year_mean" ,"Month_dec"  ,     "Year"        ,    "year_month" )

str(pmelt$year_month)

dados$month_year_mean <- NULL
dados$year_month <- NULL
str(dados) #chr , int, chr
str(pmelt)# int, int, int
dados$CODMUNIC <- as.integer(dados$CODMUNIC)
dados$Year <- as.integer(dados$Year)

int <- merge(pmelt, dados, by.x=c("CODMUNIC","Month_dec", "Year"), by.y=c("CODMUNIC", "Month_dec", "Year"), all.x= TRUE, all.y= TRUE)

head(int)

# Pop file
setwd("D://Coworks//Leptospirose//input//year_month//")
pop <- read.table("pop_CODMUNI6.pop", header=TRUE)

pop$Espacialidades <- NULL
pop$NOMEMUNICIPIO <- NULL
pop$Latitude <- NULL
pop$Longitude <- NULL
str(pop)
head(pop)
head(int)
pop$CODMUNIC7 <- pop$CODMUNIC
pop$CODMUNIC<- NULL
names(pop) <- c("Pop_2010",  "CODMUNIC", "CODMUNIC7")

intpop <- merge(int, pop, by.x=c("CODMUNIC"), by.y=c("CODMUNIC"), all.x= TRUE, all.y= TRUE)

head(intpop)
  
#

write.table(int, "INTEGRATION_geo_cases_lepto_monthly_integration_0016.geo",  row.names= FALSE)
write.table(int, "INTEGRATION_cas_cases_lepto_monthly_integration_0016.cas",  row.names= FALSE)

saveRDS(int, "cases_lepto_monthly_integration_0016.RData")



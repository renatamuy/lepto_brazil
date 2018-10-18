

setwd("D://Coworks//Leptospirose//")

require(data.table)

pm <- read.delim("lepto_pre_melt.txt")

table(pm$Year)

str(pm)
??melt
pm <- as.data.table(pm)
head(pm)
str(pm)
table(pm$Jan)
table(pm$Feb)

colnames(pm)
pm$Jan <- as.numeric(as.character(pm$Jan))
table(is.na(pm$Jan))
which(is.na(pm$Jan))
pm[1634,"Jan"]
pm[1634,"Jan"] <- 0
table(is.na(pm)

nrow(pm)

str(pm)
pmelt <- melt(pm, na.rm=FALSE, measure = 
                c("Jan", "Feb", "Mar", "Apr" , "May", "Jun",          
                  "Jul", "Aug", "Sep" , "Oct", "Nov", "Dec"), 
               value.name= c("Cases"), variable= c("Month"))

str(pmelt)
nrow(pmelt)
head(pmelt)


head(pmelt)
#pmelt$anomalie <- c("elnino","neutral", "lanina") # check!!!!! hot cold neutral

pmelt$anomalie <- NULL
# d$Month_dec_leadzero <- sprintf("%02d", d$Month_dec)


pmelt$Month_dec <-as.numeric(pmelt$Month)

pmelt$year_month <- paste(pmelt$Year, pmelt$Month_dec, sep="/")

head(pmelt)


setwd("D://Coworks//Leptospirose//zonal_covariates//")
load("zonal_covariates.RData")

summary(zonalsp)



# 7 digit to 6 digit

pmelt$CODMUNIC <-  as.integer(substr(pmelt$CODMUNIC,1,nchar(pmelt$CODMUNIC)-1))
setdiff(unique(zonalsp$CODMUNIC), unique(pmelt$CODMUNIC))
str(pmelt)
str(zonalsp)
int <- intersect(colnames(pmelt), colnames(zonalsp))
pmelt_covariates <- merge(pmelt, zonalsp, by.x= int, by.y= int, all.x= TRUE, all.y= TRUE)

pmelt_covariates 

plot(pmelt_covariates$Cases~ pmelt_covariates$eineutro)

# 
# Integration
setwd("D:/Coworks/Leptospirose/zonal_covariates/NOAA_sp")
load("SP_zonal_NOAA_temp_avg_2018_09_D21.RData")
head(dados)
head(pmelt)
dados$Month_dec <- c( rep(1:12))
dados$Year <- dados$year
dados$CODMUNIC <- as.integer(as.numeric(as.character(dados$CD_GEOCMU)))
  
dados$year <- NULL
dados$month <- NULL
dados$CD_GEOCMU <- NULL

summary(unique(as.integer(as.numeric(as.character(dados$CD_GEOCMU))) ))

summary(pmelt$CODMUNIC)


str(dados)
str(pmelt)

pmelt_covariates_t <- merge(pmelt_covariates, dados, by.x=c("CODMUNIC","Month_dec", "Year"), by.y=c("CODMUNIC", "Month_dec", "Year"), all.x= TRUE, all.y= TRUE)

nrow(pmelt_covariates_t)
table(pmelt_covariates_t$Year)

summary(pmelt_covariates_t)
pmelt_covariates_t[max(pmelt_covariates_t$ipvs),]
pmelt_covariates_t[min(pmelt_covariates_t$ipvs),]

head(pmelt_covariates_t)

# aprendi code > rename in scope (Marie)

pmelt_covariates_t$year_month <- paste(pmelt_covariates_t$Year, pmelt_covariates_t$Month_dec, sep="/")

head(pmelt_covariates_t)
write.table(pmelt_covariates_t, "cases_lepto_monthly_0016.cas",  row.names= FALSE)



# Steps
# 1. space only model clusters that could be explained by relief, drainage, vulnerability
# 2. inside cluster and outside cluster
# 3. time only model -- clusters that could be explained by temporal cof. (temp, rainfall, anomalies)
# 4. time only model -- clusters that could be explained by temporal cof. (temp, rainfall, anomalies)
# 5. space-time model -- clusters that could be explained by temporal cof. (temp, rainfall, anomalies)

# Renata
# get rainfall
# separate clusters from non clusters and deixar no jeito (Based in Rulli? et al.)
# Write the abstract
# Show doubts to David



str(pmelt_covariates_t)
# Scale covariates 

colnames(pmelt_covariates)
str(pmelt_covariates)

pmelt_covariates1 <- pmelt_covariates

pmelt_covariates1[,11:27] <- scale(pmelt_covariates1[,11:27])


summary(zonalsp$CODMUNIC)
summary(pmelt$CODMUNIC)
  

table(is.na(pmelt))


nrow(pm)
10965*12
nrow(pmelt)

write.table(pmelt, "cases_lepto_montly_0016.cas",  row.names= FALSE)
write.table(pmelt, "cases_lepto_montly_0016.geo",  row.names= FALSE)


#




# 0. Libraries and Workspace-----------------------------------------------
library(dplyr)
library(writexl)
library(readxl)
library(redcapAPI)
save.image("Workspace.Rda")
load("Workspace.Rda")

# 1. Data import ----------------------------------------------------------
data <- readRDS("Data/db_ars.rds")
id_confluence <- readRDS("Data/ids_confluence.RDS")

# Request
request <- readRDS("Request/Variables a proveer a Confluence.RDS")

# REDCAP connection
# Tomás: Este es mi token de Arsénico, por favor no compartir.
token_arsenico_final <- "63094D5DC8F7F19C027A69D6C4447EAC"
uri_arsenico_final <- "https://www.mauco.org/redcap/api/"

rcon <- redcapConnection(url = uri_arsenico_final, 
                         token = token_arsenico_final)

data_ars_rcon <- exportRecordsTyped(rcon = rcon,
                                    fields = c("ars_fecha_de_muerte", "causa_de_muerte_n_1_certif", "causa_de_muerte_n_2_certif", "causa_de_muerte_n_3_certif", "causa_de_muerte_n_4_certif", "causa_de_muerte_n_5_certif")) 


# 2. Data Wrangling -------------------------------------------------------
data_compact <- data[,c(2,4,11:15,1254,1256,1257,1805:1807,884)]

id_confluence$PersonID_sin_ASCL <- as.integer(id_confluence$PersonID_sin_ASCL)

data_ars_rcon$cec00_codigo_sin_ASCL <- gsub("[AaSsCcLl]", "", data_ars_rcon$cec00_codigo)
data_ars_rcon$cec00_codigo_sin_ASCL <- sub("^0*", "", data_ars_rcon$cec00_codigo_sin_ASCL)
data_ars_rcon$cec00_codigo_sin_ASCL <- as.integer(data_ars_rcon$cec00_codigo_sin_ASCL)

id_confluence <- id_confluence %>% left_join(data_ars_rcon[,2:8], by=c("PersonID_sin_ASCL" = "cec00_codigo_sin_ASCL"))
id_confluence <- id_confluence %>% left_join(data_compact, by=c("PersonID_sin_ASCL" = "ID_sin_ASCL") )

table(case_control = id_confluence$case_control,
      bc = id_confluence$organ,
      useNA = "always")

table(confluence = id_confluence$confluence)
id_confluence$death_age <- floor(lubridate::interval(id_confluence$dob, id_confluence$ars_fecha_de_muerte) / lubridate::years(1))

# 3. Confluence variables -------------------------------------------------

## 3.1. VitalStatus
id_confluence$VitalStatus <- ifelse(!is.na(id_confluence$ars_fecha_de_muerte), 1, 0)
table(vital_status = id_confluence$VitalStatus, useNA = "ifany")

id_confluence %>% 
  filter(VitalStatus == 1) %>% 
  View()

## 3.2. CauseDeath
id_confluence$CauseDeath <- NA
id_confluence$CauseDeath[which(id_confluence$VitalStatus == 0)] <- "777"

## 3.3. BrDeath
id_confluence$BrDeath <- NA
id_confluence$BrDeath[which(id_confluence$VitalStatus == 0)] <- "777"

id_confluence[which(grepl("w*\\s[Mm][Aa][Mm][Aa]", id_confluence$causa_de_muerte_n_5_certif)),] %>% View()
id_confluence$BrDeath[which(id_confluence$PersonID %in% id_confluence$PersonID[which(grepl("w*\\s[Mm][Aa][Mm][Aa]", id_confluence$causa_de_muerte_n_5_certif))])] <- 1

id_confluence[which(grepl("w*\\s[Mm][Aa][Mm][Aa]", id_confluence$causa_de_muerte_n_4_certif)),] %>% View()
id_confluence$BrDeath[which(id_confluence$PersonID %in% id_confluence$PersonID[which(grepl("w*\\s[Mm][Aa][Mm][Aa]", id_confluence$causa_de_muerte_n_4_certif))])] <- 1

id_confluence[which(grepl("w*\\s[Mm][Aa][Mm][Aa]", id_confluence$causa_de_muerte_n_3_certif)),] %>% View()
id_confluence$BrDeath[which(id_confluence$PersonID %in% id_confluence$PersonID[which(grepl("w*\\s[Mm][Aa][Mm][Aa]", id_confluence$causa_de_muerte_n_3_certif))])] <- 1

id_confluence[which(grepl("w*\\s[Mm][Aa][Mm][Aa]", id_confluence$causa_de_muerte_n_2_certif)),] %>% View()
id_confluence$BrDeath[which(id_confluence$PersonID %in% id_confluence$PersonID[which(grepl("w*\\s[Mm][Aa][Mm][Aa]", id_confluence$causa_de_muerte_n_2_certif))])] <- 1

id_confluence[which(grepl("w*\\s[Mm][Aa][Mm][Aa]", id_confluence$causa_de_muerte_n_1_certif)),] %>% View()
id_confluence$BrDeath[which(id_confluence$PersonID %in% id_confluence$PersonID[which(grepl("w*\\s[Mm][Aa][Mm][Aa]", id_confluence$causa_de_muerte_n_1_certif))])] <- 1

id_confluence$BrDeath[which(id_confluence$VitalStatus == 1 & is.na(id_confluence$BrDeath))] <- 0


# 4. Import CIE-10 codificados ------------------------------------------
cie_10 <- read_xlsx("Data/claudia_data_confluence_para_cie10_06 feb 2024.xlsx")

View(cie_10[, c(1:5,16:18,12,7:11)])
table(cie_10$VitalStatus, useNA = "ifany")
addmargins(table(cie_10$CauseDeath_CLAU))

# 5. Revisión 
## 5.1 CIE-10 -----------------------------------------------------------

cie_10$`CIE-10_CLAU`[which(cie_10$PersonID == "ASCL2373")] <- "G92"
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID %in% c("ASCL0684", "ASCL2081", "ASCL2461"))] <- "888"
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID %in% c("ASCL0446", "ASCL0512", "ASCL1369", "ASCL1488", "ASCL1685", "ASCL0571", "ASCL0863", "ASCL2918"))] <- "I46"
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID == "ASCL1641")] <- "C509"
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID == "ASCL0608")] <- "I10"
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID == "ASCL0400")] <- "N390"
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID == "ASCL0658")] <- "N185"
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID == "ASCL0716")] <- "L893"
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID == "ASCL2225")] <- "R092"
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID %in% c("ASCL2120", "ASCL2264"))] <- "R572"
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID == "ASCL0391")] <- "S721"
#Caso ppte ASCL0171, 5 causas de muerte, cancer de mama no se relaciona con las demas causas, no es posible definir causa de muerte principal
cie_10$`CIE-10_CLAU`[which(cie_10$PersonID == "ASCL0171")] <- "888"


## 5.2 Revisión BrDeath ---------------------------------------------------
cie_10$BrDeath <- ifelse(cie_10$VitalStatus == 1 & cie_10$`CIE-10_CLAU` == "C509", 1, 0)
cie_10$BrDeath[which(cie_10$VitalStatus == 0)] <- "777"


## 5.3 Revisión CauseDeath ------------------------------------------------
cie_10$CauseDeath[which(cie_10$VitalStatus == 1)] <- cie_10$`CIE-10_CLAU`[(which(cie_10$VitalStatus == 1))]

## 6. Join

id_confluence <- id_confluence[,1:22] %>% left_join(cie_10[,c(1,14,17,18)], by = "PersonID")
id_confluence$BrDeath[which(id_confluence$CauseDeath == "888")] <- "888"
colnames(id_confluence)[9] <- "DateEntry"

## 7. Modificacion CIE-10 (Se agrega el punto en la subclasificacion)
id_confluence <- id_confluence %>% mutate(CauseDeath = case_when(nchar(CauseDeath) == 3 ~ CauseDeath,
                                                                        nchar(CauseDeath) == 4 ~ paste0(substr(CauseDeath,1,3), ".",substr(CauseDeath,4,4))))



## Variable YearsToEnter. Se emplean las variables de fecha de encuesta, fecha de nacimiento y fecha de diagnostico de BC desde informe Anatomopatologico (Cancer de Mama)
data_fecha_informe_bc <- exportRecordsTyped(rcon = rcon,
                                            fields = c("ars_fecha_informe"))

data_fecha_informe_bc$cec00_codigo_sin_ASCL <- gsub("[AaSsCcLl]", "", data_fecha_informe_bc$cec00_codigo)
data_fecha_informe_bc$cec00_codigo_sin_ASCL <- sub("^0*", "", data_fecha_informe_bc$cec00_codigo_sin_ASCL)
data_fecha_informe_bc$cec00_codigo_sin_ASCL <- as.integer(data_fecha_informe_bc$cec00_codigo_sin_ASCL)

id_confluence <- id_confluence %>% left_join(data_fecha_informe_bc[,2:3], by = c("PersonID_sin_ASCL" = "cec00_codigo_sin_ASCL"))

id_confluence$diag_age_years <- mapply(function(x,y) as.character(trunc(as.numeric(difftime(x, y, units = "weeks"))/52.25)), id_confluence$ars_fecha_informe, id_confluence$dob)

id_confluence$YearsToEnter <- round(as.numeric(difftime(id_confluence$date, id_confluence$ars_fecha_informe, units = "days"))/365.25, digits = 3)

View(id_confluence[which(id_confluence$YearsToEnter < 0),c(1,9,26,15,21,27,28)])
View(id_confluence[which(id_confluence$case_control == 1 & is.na(id_confluence$YearsToEnter)),c(1,9,26,15,21,27,28)])
View(id_confluence[,c(1,9,26,15,21,27,28)])


## Recuperar valores de fecha de diagnóstico de Cancer de Mama
id_confluence$ars_fecha_informe <- as.Date(as.character(id_confluence$ars_fecha_informe))
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL0102")] <- as.Date("2015-06-02")
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL0126")] <- as.Date("2015-07-02") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL0129")] <- as.Date("1960-07-08") + (365.25*55) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL0351")] <- as.Date("1983-06-17") + (365.25*32) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL0410")] <- as.Date("2015-09-02") # Mediante IA
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL0467")] <- as.Date("1947-06-03") + (365.25*68) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL0861")] <- as.Date("2015-11-05") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL0863")] <- as.Date("2016-01-11") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL0961")] <- as.Date("2016-04-27") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL0992")] <- as.Date("2016-05-20") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL1284")] <- as.Date("2016-10-27") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL1358")] <- as.Date("2016-09-29") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL1483")] <- as.Date("2016-12-12") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL1801")] <- as.Date("1960-01-10") + (365.25*56) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL1804")] <- as.Date("2016-07-14") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL1827")] <- as.Date("1949-09-23") + (365.25*67) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL1850")] <- as.Date("1957-09-25") + (365.25*59) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL1855")] <- as.Date("1964-03-16") + (365.25*53) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL1884")] <- as.Date("2017-04-18") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL2061")] <- as.Date("2018-03-08") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL2172")] <- as.Date("2017-08-08") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL2192")] <- as.Date("1960-07-24") + (365.25*57) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL2240")] <- as.Date("2017-03-01") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL2398")] <- as.Date("2017-11-17") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL2494")] <- as.Date("1950-12-25") + (365.25*66) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL2505")] <- as.Date("1953-04-19") + (365.25*65) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL02539")] <- as.Date("2018-09-21") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL02566")] <- as.Date("2019-01-23") # Mediante Fecha informe examen BP en encuesta de reclutamiento 
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL02574")] <- as.Date("2019-01-24") # Mediante Fecha informe examen BP en encuesta de reclutamiento 
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL02586")] <- as.Date("2019-03-26") # Mediante Fecha informe examen BP en encuesta de reclutamiento 
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL02589")] <- as.Date("2019-02-12") # Mediante Fecha informe examen BP en encuesta de reclutamiento 
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL02885")] <- as.Date("2015-10-09") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL02958")] <- as.Date("1981-03-05") + (365.25*37) # Mediante Encuesta de Factores, usando fecha aprox. según edad de diagnóstico de BC 
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL03021")] <- as.Date("2018-10-23") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL03046")] <- as.Date("2019-02-19") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL03061")] <- as.Date("2019-01-31") # Mediante FC
id_confluence$ars_fecha_informe[which(id_confluence$PersonID == "ASCL03200")] <- as.Date("2019-03-22") # Mediante FC

id_confluence$DateDiagIndex <- id_confluence$ars_fecha_informe
## Base de datos seleccionada para confluence
db_confluence <- id_confluence %>% 
  select(PersonID, DateEntry, DateDiagIndex, YearsToEnter, VitalStatus, CauseDeath, BrDeath)


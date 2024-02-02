
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


<<<<<<< HEAD
=======
<<<<<<< HEAD
## Este comentario es una prueba.
>>>>>>> main


<<<<<<< HEAD



id_confluence %>% 
  pivot_longer(cols = starts_with("causa_de_muerte"), names_to = "cause_n", values_to = "motive") %>% 
  mutate(cie_cause_n = case_when((cause_n == "causa_de_muerte_n_1_certif")~"causa_cie10_1",
                                 (cause_n == "causa_de_muerte_n_2_certif")~"causa_cie10_2",
                                 (cause_n == "causa_de_muerte_n_3_certif")~"causa_cie10_3",
                                 (cause_n == "causa_de_muerte_n_4_certif")~"causa_cie10_4",
                                 (cause_n == "causa_de_muerte_n_5_certif")~"causa_cie10_5")) %>% 
  count(motive) %>% 
  View()










=======
## Otra cosa
## Este comentario es conflictivo
=======
>>>>>>> c6fc0f10c1b503f53c9c854ead68d56050887f8b
>>>>>>> main

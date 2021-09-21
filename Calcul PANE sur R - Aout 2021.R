library(readxl)
library(dplyr)
library(openxlsx)

CA_1819 <- read_xlsx("SACI/2021/DAR/CA_2010_2019.xlsx",sheet = 1)
CA_1617 <- read_xlsx("SACI/2021/DAR/CA_2010_2019.xlsx",sheet = 2)
CA_1015 <- read_xlsx("SACI/2021/DAR/CA_2010_2019.xlsx",sheet = 3)
CA_2020 <- read_xlsx("SACI/2021/DAR/CA_2010_2019.xlsx",sheet = 4)
CA_2021 <- read_xlsx("SACI/2021/DAR/CA_2010_2019.xlsx",sheet = 5)
CA_1019 <- rbind(CA_1015,CA_1617,CA_1819,CA_2020,CA_2021)
CA_2010_2021 <- CA_1019
CA_2010_2021$Effet <- as.Date(CA_2010_2021$Effet,origin = "1900/01/01")
CA_2010_2021$Echeance <- as.Date(CA_2010_2021$Echeance,origin = "1900/01/01")
CA_2010_2021$DATEEMISSION <- as.Date(CA_2010_2021$DATEEMISSION,origin = "1900/01/01")
CA_2010_2021$DureeContrat <- CA_2010_2021$Echeance - CA_2010_2021$Effet + 1
CA_2010_2021$DureeAnnee <- as.numeric(CA_2010_2021$DureeContrat/365.25)

AnneePoliceAcquisition<-function(DateEffet,DateFin,DebutAnnee,FinAnnee)
{
  # Renvoie l'annÃ©e police pour une annee donnees
  
  #DebutAnnee = as.Date(paste("01/01/",Annee,sep = ""), "%d/%m/%Y")
  #FinAnnee= as.Date(paste("31/12/",Annee,sep = ""), "%d/%m/%Y")
  
  DebutAnnee = as.Date(DebutAnnee, "%d/%m/%Y")
  FinAnnee= as.Date(FinAnnee, "%d/%m/%Y")
  
  DateEffet =as.Date(DateEffet, "%d/%m/%Y")
  DateFin =as.Date(DateFin, "%d/%m/%Y")
  
  AnneePoliceAcquisition=(DateEffet<FinAnnee & DateFin>DebutAnnee)*((sapply(as.numeric(FinAnnee-DateEffet),FUN=function(x){max(x,0)}) +   sapply(as.numeric(DateEffet-DebutAnnee),FUN=function(x){min(x,0)}) + sapply(as.numeric(DateFin-FinAnnee),FUN=function(x){min(x,0)})+1)/365.25)
  
  
  #  sapply(as.numeric(FinAnnee-DateEffet),FUN=function(x){max(x,0)})
  
  # min(as.numeric(DateEffet-DebutAnnee),0)
}
FonctionAnneePolAcquise <- function(data,Dateff,Datech,Andeb,Anfin,firstdate,secdate,tirddate,fourthdate){
  #data: La base sur laquelle 
  for (K in Andeb:Anfin) {
    data[[paste0(K,1)]] <- AnneePoliceAcquisition(data[[Dateff]],data[[Datech]],as.Date(paste0(K,firstdate)),as.Date(paste0(K,secdate)))*data[["CA"]]/data[["DureeAnnee"]]
    data[[paste0(K,2)]] <- AnneePoliceAcquisition(data[[Dateff]],data[[Datech]],as.Date(paste0(K,tirddate)),as.Date(paste0(K,fourthdate)))*data[["CA"]]/data[["DureeAnnee"]]
  }
  return(data)
}
#source("C:/Users/Konan Julien/OneDrive - Sanlam Emerging Markets/Sanlam/SACI/2021/DAR/Biblothèque Fonctions R",encoding="UTF-8")

Basemagique <- FonctionAnneePolAcquise(CA_2010_2021,"Effet","Echeance",2015,2021,"/01/01","/08/31","/09/01","/12/31")
BasePane <- Basemagique %>% filter(Annee >=2015)
write.csv2(BasePane,"SACI/2021/DAR/Estimation PANE/Sources_Données_PANE_082021_OkBon.csv")
write.xlsx(BasePane,"SACI/2021/DAR/Estimation PANE/Sources_Données_PANE_082021_OKBon.xlsx")

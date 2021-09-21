RatioSPMat<-function(X)
{
  sum(X[,1],na.rm=TRUE)/sum(X[,2],na.rm=TRUE)
}

RatioSP<-function(x,y)
{
  sum(x,na.rm=TRUE)/sum(y,na.rm=TRUE)
}


ExtractionAnnee<-function(ChaineDate)
{
  as.numeric(format(as.Date(ChaineDate, "%d/%m/%Y"),"%Y"))
  
}

Retraitement_police<-function(VecPolice)
{
  
  FormatPattern = "-|\\.|\\s|/"
  return(gsub(FormatPattern, "",VecPolice))
  
  
}

SumDouble <-function(X,na.rm)
{
  sum(as.double(X),na.rm = TRUE)
}


DureePolice<-function(DateEffet, DateFin)
{
  ## Cette fonction compre 1 pour une emissions sur 1 an, et 1/12 pour une police mensuelle
  as.numeric(as.Date(DateFin, "%d/%m/%Y")- as.Date(DateEffet, "%d/%m/%Y")+1) 
  
}
Exposition<-function(DateEffet, DateFin)
{
  ## Base_annuelle correpond au nombre de jours de référence dans une année
  ## Cette fonction compre 1 pour une emissions sur 1 an, et 1/12 pour une police mensuelle
  as.numeric(as.Date(DateFin, "%d/%m/%Y")- as.Date(DateEffet, "%d/%m/%Y")+1)/365.25  
  
}

AnneePoliceAcquisition<-function(DateEffet,DateFin,DebutAnnee,FinAnnee)
{
  # Renvoie l'année police pour une annee donnees
  
  #DebutAnnee = as.Date(paste("01/01/",Annee,sep = ""), "%d/%m/%Y")
  #FinAnnee= as.Date(paste("31/12/",Annee,sep = ""), "%d/%m/%Y")
  
  DebutAnnee = as.Date(DebutAnnee, "%d/%m/%Y")
  FinAnnee= as.Date(FinAnnee, "%d/%m/%Y")
  
  DateEffet =as.Date(DateEffet, "%d/%m/%Y")
  DateFin =as.Date(DateFin, "%d/%m/%Y")
  
  AnneePoliceAcquisition=(DateEffet<=FinAnnee & DateFin>=DebutAnnee)*
    ((sapply(as.numeric(FinAnnee-DateEffet),FUN=function(x){max(x,0)}) +
        sapply(as.numeric(DateEffet-DebutAnnee),FUN=function(x){min(x,0)}) + 
        sapply(as.numeric(DateFin-FinAnnee),FUN=function(x){min(x,0)})+1)/365.25)
    
  
  #  sapply(as.numeric(FinAnnee-DateEffet),FUN=function(x){max(x,0)})
  
  # min(as.numeric(DateEffet-DebutAnnee),0)
}

AnneePoliceProvision<-function(DateEffet,DateFin,DebutPrec)
{
  # Renvoie l'année police pour une annee donnees
  
  #DebutAnnee = as.Date(paste("01/01/",Annee,sep = ""), "%d/%m/%Y")
  #FinAnnee= as.Date(paste("31/12/",Annee,sep = ""), "%d/%m/%Y")
  
  DebutPrec = as.Date(DebutPrec, "%d/%m/%Y")
  #  FinAnnee= as.Date(FinAnnee, "%d/%m/%Y")
  
  DateEffet =as.Date(DateEffet, "%d/%m/%Y")
  DateFin =as.Date(DateFin, "%d/%m/%Y")
  
  # AnneePoliceAcquisition=(DateFin>DebutPrec)*((sapply(as.numeric(FinAnnee-DateEffet),FUN=function(x){max(x,0)})+   sapply(as.numeric(DateEffet-DebutAnnee),FUN=function(x){min(x,0)}) + sapply(as.numeric(DateFin-FinAnnee),FUN=function(x){min(x,0)})+1)/365.25)
  
  AnneePoliceProvision = (DateFin>DebutPrec)*((sapply(as.numeric(DateFin- DebutPrec),FUN=function(x){max(x,0)})+   sapply(as.numeric(DebutPrec-DateEffet),FUN=function(x){min(x,0)}) )/365.25)
  
  
  #  sapply(as.numeric(FinAnnee-DateEffet),FUN=function(x){max(x,0)})
  
  # min(as.numeric(DateEffet-DebutAnnee),0)
}

SumDoubleAcquisition <-function(X,Acquisition)
{
  sum(as.double(X*Acquisition),na.rm = TRUE)
}

Appliquer_EDP2 <- function(cle_unique,primes_net_coass,capitaux_consideree_coass, plein_conservation, capacite_edp, capacite_facob1, capacite_facob2)
{
  # cette function prend en input les details sur une garantie, les caractéristiques d'une police 
  # et les caractéristiques d'un traité edp et sort en output la repartition des engagements entre 
  #assureur et reassureur
  resultat=list()
  
  #cession EDP
  
  ##Table_Application_fin$cle_unique,Table_Application_fin$Primes_aggreges, Table_Application_fin$Capitaux_retenus_net_coass,Table_Application_fin$conservation_reduite,Table_Application_fin$capacite_edp,Table_Application_fin$capacite_facob1,Table_Application_fin$capacite_facob2
  # capitaux_consideree_coass= Table_Application_fin$Capitaux_retenus_net_coass
  #  capacite_edp=Table_Application_fin$capacite_edp
  #  plein_conservation=  Table_Application_fin$conservation_reduite
  
  
  # x=(capitaux_consideree_coass - plein_conservation)
  
  #z =capacite_edp
  
  #capitaux_cedes = sapply(capitaux_consideree_coass - plein_conservation , FUN =function(x){max(0,x)}  )
  
  # capitaux_cedes = sapply(capitaux_cedes min((max(0,x),y)
  capitaux_cedes = pmin(pmax(0,(capitaux_consideree_coass - plein_conservation)),capacite_edp)
  
  #  capitaux_cedes = min((max(0,(capitaux_consideree_coass - plein_conservation)),capacite_edp)
  
  taux_cession=capitaux_cedes/capitaux_consideree_coass
  
  excedent=pmax(0,capitaux_consideree_coass-capitaux_cedes-plein_conservation)
  
  resultat$retention_filiale=pmin(plein_conservation,capitaux_consideree_coass-capitaux_cedes)
  resultat$taux_retention=resultat$retention_filiale/capitaux_consideree_coass
  resultat$capitaux_cedes=capitaux_cedes
  resultat$taux_cession=taux_cession
  resultat$excedent=excedent
  resultat$prime_conservee=resultat$taux_retention*primes_net_coass
  
  #introduction facob
  if (excedent>0){resultat$facob="oui"}
  else {resultat$facob="non"}
  
  
  #fac1 
  montant_fac1=min(excedent,capacite_facob1)
  excedent_fac1=max(0, excedent-montant_fac1)
  taux_fac1=montant_fac1/capitaux_consideree_coass
  
  
  resultat$facob1=montant_fac1
  resultat$excedent_facob1=excedent_fac1
  resultat$taux_fac1=taux_fac1
  resultat$prime_fac1=taux_fac1*primes_net_coass
  
  
  #fac2
  montant_fac2=min(excedent_fac1,capacite_facob2)
  excedent_fac2=max(0,excedent_fac1-montant_fac2)
  taux_fac2=montant_fac2/capitaux_consideree_coass
  
  resultat$facob2=montant_fac2
  resultat$taux_fac2=taux_fac2
  resultat$prime_fac2=taux_fac2*primes_net_coass
  
  #fac
  taux_fac=excedent_fac2/capitaux_consideree_coass
  resultat$fac=excedent_fac2
  resultat$taux_fac=taux_fac
  resultat$prime_fac1=taux_fac*primes_net_coass
  
  
  resultat_edp=data.frame(resultat)
  
  # return(merge(x=tableau_donnees, y=resultat_edp))
  
}


#FONCTIONS POUR BCP AUTOMATIQUE


Appliquer_EDP <-function(Table)
{
  
  
  # Met en oeuvre un edp et sort des outputs (la conservation, la cession , l'excedent)
  # Cette table input doit avoir une colonne Capitaux consideres 
  Table$Retention = pmin(Table$Capitaux_consideres, Table$Plein_Conservation)
  Table$Taux_retention=Table$Retention/Table$Capitaux_consideres_100
  Table$Prime_conservee= Table$Taux_retention* Table$Prime_nette_100
  
  Table$Cession_EDP = pmin(Table$Capitaux_consideres - Table$Retention, Table$Capacite_EDP)
  Table$Taux_cession = Table$Cession_EDP/Table$Capitaux_consideres_100
  Table$Prime_cedee= Table$Taux_cession* Table$Prime_nette_100
  Table$Commission_cedee = Table$commission*  Table$Prime_cedee
  
  
  Table$Excedent = pmax(0,Table$Capitaux_consideres-Table$Cession_EDP-Table$Retention)
  
  
  
  return(Table )
  
}


Traitement_Preparatoire <-function(Table_Data_Informatique , Table_liste_activite, Table_Garantie, Table_Garantie_Traite,Table_reduction_traites_prop, Chemin_base_res,Collecte_Data)
{
  ### Cette fonction prend en input les donn?es de l'informatique
  ### Elle ressort en output les tables d'anomalies 
  ### ainsi que la data frame pret d'emploi pour la mise en oeuvre des edp
  
  
  
  #### AJout de la liste des activit?
  
  
  #fonction pour ajouter activité
  
  Table_Output= merge(Table_Data_Informatique, Table_liste_activite, by = "assure", all.x =TRUE , all.y = FALSE )
  Table_Output$activite[is.na(Table_Output$activite)] =" Non concernee par les r?ductions de capacit?s"
  Table_Output$code_activite[is.na(Table_Output$code_activite)] ="A"
  
  
  
  ### Ajout des codes garantis
  
  
  Table_Output=merge(Table_Output,Table_Garantie[ ,c("Coa_garantie","code_garantie","lib_garantie")], by="Coa_garantie",all.x =TRUE , all.y = FALSE )
  #### Extractions des garanties non prises en compte dans le BCP ####  
  Table_Garantie_nonTraite = Table_Output[Table_Output$code_garantie=="",]

  
  Table_Output= Table_Output[Table_Output$code_garantie!="",]
  
  
  ### Ajout des codes des trait?s
  
  Table_Output=merge(Table_Output,Table_Garantie_Traite, by="code_garantie",all.x =TRUE , all.y = FALSE)
  
  #### Agregation par niveau de trait?s,facob 1 et 2
  
  by_liste = c("assure","ancien_pol", "code_activite","activite" ,"code_traite", "code_facob_1","code_facob_2","date_effet","date_fin", "taux_net_coass" ,"risque_sunu_services")
  Table_Application=aggregate(Table_Output$pnette_100, by=Table_Output[,by_liste],sum, na.rm=TRUE)
  
  colnames(Table_Application)[dim(Table_Application)[2]]="Primes_aggreges"
  Table_Application$Capitaux_aggreges=aggregate(Table_Output$capital_100, by=Table_Output[,by_liste],max, na.rm=TRUE)$x
  Table_Application$Smp_aggreges=aggregate(Table_Output$smp_100, by=Table_Output[,by_liste],max, na.rm=TRUE)$x
  
  ############################### Retraitement des donnees collectees #####################################################
  

  #####Aggrégation de collecte data par traité 
  ##on merge par code_garantie avec bd_garantie_IT
  Data_collecte_garantie_IT=merge(Collecte_Data,Table_Garantie,by.x="Code_garantie",by.y="Coa_garantie",all.y=FALSE)
  ##On ajoute les codes des traités
  Data_collecte_FINAL=merge(Data_collecte_garantie_IT,Table_Garantie_Traite,by="code_garantie",all.y = FALSE)
  
  #####Retraitement des capitaux, smp et primes non renseignées
  
  Data_collecte_FINAL$Capitaux[is.na(Data_collecte_FINAL$Capitaux )]=0
  Data_collecte_FINAL$LCI.SMP[is.na(Data_collecte_FINAL$LCI.SMP )]=0
  Data_collecte_FINAL$Prime[is.na(Data_collecte_FINAL$Prime )]=0

  
  liste_aggrega = c("Anc_Police" ,"code_traite", "code_facob_1","code_facob_2","Effet","Echeance" ,"risque_sunu_services")

    colnames(Data_collecte_FINAL_aggrege)[dim(Data_collecte_FINAL_aggrege)[2]]="Primes_aggreges"
  Data_collecte_FINAL_aggrege$Capitaux_aggreges=aggregate(Data_collecte_FINAL$Capitaux, by=Data_collecte_FINAL[,liste_aggrega],max, na.rm=TRUE)$x
  Data_collecte_FINAL_aggrege$Smp_aggreges=aggregate(Data_collecte_FINAL$LCI.SMP, by=Data_collecte_FINAL[,liste_aggrega],max, na.rm=TRUE)$x
  
  #######Création des iduniques
  

  Table_Application$idunique=paste0(Table_Application$ancien_pol,Table_Application$code_traite,Table_Application$date_effet)
  Data_collecte_FINAL_aggrege$idunique=paste0(Data_collecte_FINAL_aggrege$Anc_Police,Data_collecte_FINAL_aggrege$code_traite,Data_collecte_FINAL_aggrege$Effet)
  
  indices=which(Table_Application$idunique %in% Data_collecte_FINAL_aggrege$idunique )
  
  Donnee_ressaisies=Table_Application[indices,]
  Tab_recap=merge(Donnee_ressaisies,Data_collecte_FINAL_aggrege,by="idunique",all.y = FALSE)
  vec=c("idunique","assure","ancien_pol","code_activite","activite","code_traite.x","code_facob_1.x","code_facob_2.x","date_effet","date_fin","taux_net_coass","risque_sunu_services.x","Capitaux_aggreges.y","Smp_aggreges.y","Primes_aggreges.y")
  Donnee_ressaisies=Tab_recap[,vec]
  colnames(Donnee_ressaisies)=c("idunique","assure","ancien_pol","code_activite","activite","code_traite","code_facob_1","code_facob_2","date_effet","date_fin","taux_net_coass","risque_sunu_services","Capitaux_aggreges","Smp_aggreges","Primes_aggreges")
  
  ######## Remplacement des donnees ressaisies dans tableApplication
  
  Table_Application[Table_Application$idunique %in% Donnee_ressaisies$idunique,]$Capitaux_aggreges=Donnee_ressaisies$Capitaux_aggreges                               
  Table_Application[Table_Application$idunique %in% Donnee_ressaisies$idunique,]$Smp_aggreges =Donnee_ressaisies$Smp_aggreges
  ################On ressort les primes aggrégées nulles
  Donnee_ressaisies_BP=Donnee_ressaisies[Donnee_ressaisies$Primes_aggreges!=0,]
  
  Table_Application[Table_Application$idunique %in% Donnee_ressaisies_BP$idunique,]$Primes_aggreges =Donnee_ressaisies_BP$Primes_aggreges                               
  
  ######## Ajout des donnees ressaisies et qui nétait pas dans le fichier de l'informatique 
  ####Est-ce nécessaire??
  
  #################################################################################################################
  
 liste_capitaux_anomalie = Table_Application[Table_Application$Capitaux_aggreges<=0,]
 Table_Application= Table_Application[Table_Application$Capitaux_aggreges>0,]
  
  ### Choix de la base de cession
  Table_Application$BaseCession = "Capitaux assures"
  Table_Application$BaseCession[(Table_Application$Smp_aggreges>0) & (Table_Application$Smp_aggreges< Table_Application$Capitaux_aggreges)] = "SMP"
  
  
  Table_Application$Capitaux_consideres_100 = (Table_Application$Capitaux_aggreges)*(Table_Application$BaseCession== "Capitaux assures") + (Table_Application$Smp_aggreges)*(Table_Application$BaseCession== "SMP")
  Table_Application$Capitaux_retenus_net_coass= eval_coassurance(Table_Application$Capitaux_consideres_100,Table_Application$taux_net_coass)
  
  colnames(Table_Application)[colnames(Table_Application)=="Primes_aggreges"] = "Prime_nette_100"
  Table_Application$Prime_Nette_coass =  eval_coassurance(Table_Application$Prime_nette_100,Table_Application$taux_net_coass) 
  
  
  Table_Application=merge(Table_Application,Table_reduction_traites_prop[,c("code_activite","reduction_edp", "reduction_facob1" , "reduction_facob2")],by="code_activite",all.x =TRUE , all.y = FALSE)
  
  Table_Application$id = c(1:nrow(Table_Application))
  write.table(liste_capitaux_anomalie,paste(Chemin_base_res,"/Anomalie/Septembre/liste_capitaux_anomalie.csv",sep="")  , dec="," , sep=";", row.names = FALSE)
  write.table(Table_Garantie_nonTraite,paste(Chemin_base_res,"/Anomalie/Septembre/Garantie_non_inclus.csv",sep="" ) , sep=";" , dec="," ,row.names = FALSE)
  write.table(Table_Application,paste(Chemin_base_res,"/Resulats/Resultat_Septembre/init_OutPut_agreg.csv",sep=""  ) , sep=";", dec=",",row.names = FALSE)
  
  return(Table_Application)
}


Traitement_BCP<-function(Table_Pret_BCP,bd_traite_edp, chemin_base_res )

{
  res=list()
  
  Table_Input_EDP = merge(Table_Pret_BCP[,c("id","code_traite","Capitaux_consideres_100", "Prime_nette_100","Capitaux_retenus_net_coass","reduction_edp")],bd_traite_edp,by="code_traite",all.x =TRUE , all.y = FALSE)
  colnames(Table_Input_EDP)[colnames(Table_Input_EDP)=="Capitaux_retenus_net_coass"] = "Capitaux_consideres"
  
  ############### On doit ajouter ici la condition selon laquelle la reduction ne s'applique que dans le traité incendie########################
  
  Table_Input_EDP$conservation=Table_Input_EDP$conservation*Table_Input_EDP$reduction_edp
  colnames(Table_Input_EDP)[colnames(Table_Input_EDP)=="conservation"]= "Plein_Conservation"
  
  
  Table_Input_EDP$Capacite_EDP=Table_Input_EDP$Capacite*Table_Input_EDP$reduction_edp
  
  res_EDP=Appliquer_EDP(Table_Input_EDP)
  res_EDP$Taux_commission=Table_Input_EDP$commission
  colnames(res_EDP)[c(15:19)] <- paste(  colnames(res_EDP)[c(15:19)], "EDP" , sep="_")
  
  ### Evaluation OUTPUT FACOB 1
  
  Table_Input_FACOB_1 = merge(Table_Pret_BCP[,c("id","code_facob_1","reduction_facob1","Capitaux_consideres_100","Prime_nette_100")],bd_traite_edp,by.x="code_facob_1",by.y = "code_traite",all.x =TRUE , all.y = FALSE)
  Table_Input_FACOB_1 = merge(Table_Input_FACOB_1,res_EDP[,c("id","Excedent_EDP")],by= "id",all.x =TRUE , all.y = FALSE)
  colnames(Table_Input_FACOB_1)[colnames(Table_Input_FACOB_1)=="Excedent_EDP"] = "Capitaux_consideres"
  colnames(Table_Input_FACOB_1)[colnames(Table_Input_FACOB_1)=="Primes_aggreges"] = "Prime_nette_100"
  
  Table_Input_FACOB_1$conservation=Table_Input_FACOB_1$conservation*Table_Input_FACOB_1$reduction_facob1
  colnames(Table_Input_FACOB_1)[colnames(Table_Input_FACOB_1)=="conservation"]= "Plein_Conservation"
  
  Table_Input_FACOB_1$Capacite_EDP=Table_Input_FACOB_1$Capacite*Table_Input_FACOB_1$reduction_facob1
  res_FACOB1=Appliquer_EDP(Table_Input_FACOB_1)
  res_FACOB1$Taux_commission=Table_Input_FACOB_1$commission
  colnames(res_FACOB1)[c(10,14:19)] <- paste(  colnames(res_FACOB1)[c(10,14:19)], "facob1" , sep="_")
  
  
  ### Evaluation OUTpUT FACOB 2
  
  Table_Input_FACOB_2 = merge(Table_Pret_BCP[,c("id","code_facob_2","reduction_facob2","Capitaux_consideres_100","Prime_nette_100")],bd_traite_edp,by.x="code_facob_2",by.y="code_traite",all.x =TRUE , all.y = FALSE)
  Table_Input_FACOB_2 = merge(Table_Input_FACOB_2,res_FACOB1[,c("id","Excedent_facob1")],by= "id",all.x =TRUE , all.y = FALSE)
  colnames(Table_Input_FACOB_2)[colnames(Table_Input_FACOB_2)=="Excedent_facob1"] = "Capitaux_consideres"
  
  Table_Input_FACOB_2$conservation=Table_Input_FACOB_2$conservation*Table_Input_FACOB_2$reduction_facob2
  colnames(Table_Input_FACOB_2)[colnames(Table_Input_FACOB_2)=="conservation"]= "Plein_Conservation"
  
  Table_Input_FACOB_2$Capacite_EDP=Table_Input_FACOB_2$Capacite*Table_Input_FACOB_2$reduction_facob2
  res_FACOB2=Appliquer_EDP(Table_Input_FACOB_2)
  
  res_FACOB2$Taux_commission=Table_Input_FACOB_2$commission
  
  colnames(res_FACOB2)[c(10,14:17,19)] <- paste(  colnames(res_FACOB2)[c(10,14:17,19)], "facob2" , sep="_")
  colnames(res_FACOB2)[18]<-"Placement_fac"
  res_FACOB2$taux_fac=res_FACOB2$Placement_fac/res_FACOB2$Capitaux_consideres_100
  res_FACOB2$prime_fac=res_FACOB2$taux_fac*res_FACOB2$Prime_nette_100
  
  
  Liste_col_Data = c("id","risque_sunu_services", "code_traite", "activite", "assure", "ancien_pol","date_effet", "date_fin", "Capitaux_aggreges", "Smp_aggreges", "Prime_nette_100","BaseCession" , "Capitaux_consideres_100", "taux_net_coass","Capitaux_retenus_net_coass", "Prime_Nette_coass", "reduction_edp")
  liste_col_res_data = c("id", "Plein_Conservation" , "Retention","Taux_retention","Prime_conservee","Capacite_EDP","Cession_EDP","Taux_cession_EDP","Prime_cedee_EDP", "Commission_cedee_EDP","Taux_commission_EDP")
  
  temp = merge(Table_Pret_BCP[,Liste_col_Data],res_EDP[,liste_col_res_data], by="id") 
  
  liste_col_res_facob1= c("id", "reduction_facob1","Capacite_EDP_facob1", "Cession_EDP_facob1", "Taux_cession_facob1", "Prime_cedee_facob1", "Commission_cedee_facob1","Taux_commission_facob1")
  temp = merge(temp, res_FACOB1[, liste_col_res_facob1], by="id" )
  
  liste_col_res_facob2= c("id", "reduction_facob2", "Capacite_EDP_facob2", "Cession_EDP_facob2", "Taux_cession_facob2", "Prime_cedee_facob2", "Commission_cedee_facob2","Taux_commission_facob2", "Placement_fac", "taux_fac", "prime_fac")
  Resultats_details = merge(temp, res_FACOB2[, liste_col_res_facob2], by="id" )
  res$Details=Resultats_details
  write.table(Resultats_details,paste(chemin_base_res,"/Resultat_Septembre/Resultats_details.csv",sep=""  ) , sep=";", dec=",",row.names = FALSE)
  
  
  ################## Agregation ############################## 
  
  Conser_agg=aggregate(Resultats_details$Prime_conservee,by=list(Risques=Resultats_details$risque_sunu_services),sum)
  colnames(Conser_agg)[dim(Conser_agg)[2]]="Total_Prime_conservees"
  
  Cession_Edp_agg=aggregate(Resultats_details$Prime_cedee_EDP,by=list(Risques=Resultats_details$risque_sunu_services),sum)
  colnames(Cession_Edp_agg)[dim(Cession_Edp_agg)[2]]="Total_Prime_EDP"
  
  commision_edp_agg=aggregate(Resultats_details$Commission_cedee_EDP,by=list(Risques=Resultats_details$risque_sunu_services,Taux_commission_EDP=Resultats_details$Taux_commission_EDP),sum)
  colnames(commision_edp_agg)[dim(commision_edp_agg)[2]]="Total_commission_EDP"
  
  Cession_facob1_agg=aggregate(Resultats_details$Prime_cedee_facob1,by=list(Risques=Resultats_details$risque_sunu_services),sum)
  colnames(Cession_facob1_agg)[dim(Cession_facob1_agg)[2]]="Total_Prime_facob1"
  
  commision_facob1_agg=aggregate(Resultats_details$Commission_cedee_facob1,by=list(Risques=Resultats_details$risque_sunu_services,Taux_commission_facob1=Resultats_details$Taux_commission_facob1),sum)
  colnames(commision_facob1_agg)[dim(commision_facob1_agg)[2]]="Total_commission_FACOB1"
  
  
  Cession_facob2_agg=aggregate(Resultats_details$Prime_cedee_facob2,by=list(Risques=Resultats_details$risque_sunu_services),sum)
  colnames(Cession_facob2_agg)[dim(Cession_facob2_agg)[2]]="Total_Prime_facob2"
  
  commision_facob2_agg=aggregate(Resultats_details$Commission_cedee_facob2,by=list(Risques=Resultats_details$risque_sunu_services,Taux_commission_facob2=Resultats_details$Taux_commission_facob2),sum)
  colnames(commision_facob2_agg)[dim(commision_facob2_agg)[2]]="Total_commission_FACOB2"
  
  Cession_fac_agg=aggregate(Resultats_details$prime_fac,by=list(Risques=Resultats_details$risque_sunu_services),sum)
  colnames(Cession_fac_agg)[dim(Cession_fac_agg)[2]]="Total_Prime_fac"
  
  
  
  Recapitulatif=merge(merge(merge(merge(merge(merge(merge(Conser_agg,Cession_Edp_agg),commision_edp_agg),Cession_facob1_agg),commision_facob1_agg),Cession_facob2_agg),commision_facob2_agg),Cession_fac_agg)
  Total=c(sum(Recapitulatif$Total_Prime_conservees),sum(Recapitulatif$Total_Prime_EDP),NA,sum(Recapitulatif$Total_commission_EDP),sum(Recapitulatif$Total_Prime_facob1),NA,sum(Recapitulatif$Total_commission_FACOB1),sum(Recapitulatif$Total_Prime_facob2),NA,sum(Recapitulatif$Total_commission_FACOB2),sum(Recapitulatif$Total_Prime_fac))  
  
  
  for(i in 2:ncol(Recapitulatif))
  {
    Recapitulatif[5,i]=Total[i-1]
  }
  Recapitulatif[5,1]="TOTAL"
  
  res$Recap =Recapitulatif
  
  write.table(Recapitulatif[c(2,3,1,4,5),],paste( chemin_base_res,"/Resultat_Septembre/Recapitulatif_resultats.csv",sep=""  ) , sep=";", dec=",",row.names = FALSE)
  
  return(res)
}

ControleDejaSaisi <-function(File_Saisi,FichierSituation)
{
  
  
}

Evaluation_Affaire_Critique <-function(Critere_Capitaux,Critere_Prime, VecEffet, VecFin,VecPrimePeriode,VecCapitaux)
{
  ## Cette fonction permet de dire si au regard de la prime et des capitaux cette police doit etre surverillee
  
  ### Critere_sur_prime
  #Calcul la prime annualisée
  
  Prime_Annuelle= VecPrimePeriode/Exposition(VecEffet,VecFin,365.25)
  
  
  return( (Prime_Annuelle>Critere_Prime)|(VecCapitaux>Critere_Capitaux) )
  
}

Evaluation_Capitaux_Autres <-function(Critere_Capitaux, Critere_Prime, liste_code_autres, VecIdUniquePolice, VecIdUniqueGaranties,  VecEffet, VecFin,VecPrimePeriode, VecCodeGaranties, VecCapitauxGaranties )
{
  ##Cette fonction permet de regarder les affaires qui ont des garanties autres importantes
  Res = list()
  cas_liste_autres= VecCodeGaranties %in% liste_code_autres
  ## Prime 
  Prime_Annuelle= VecPrimePeriode/Exposition(VecEffet,VecFin,365.25)
  
  ControleDoubleCritere= (Prime_Annuelle[cas_liste_autres]>Critere_Prime)|(VecCapitauxGaranties[cas_liste_autres] > Critere_Capitaux)
  # capitaux
  Res$VecBool =VecIdUniquePolice %in% VecIdUniqueGaranties[cas_liste_autres][ControleDoubleCritere]
  Res$VecIdUnique =VecIdUniquePolice[ Res$VecBool]# unique(VecIdUniqueGaranties[(VecCapitauxGaranties[ VecCodeGaranties %in% liste_code_autres] > Critere_Capitaux)])
  
  
  
  return(Res)
  
  
}



eval_coassurance<-function(montant,taux)
{ return(as.double(montant)*as.double(taux)/100)
}

capacite_reduite<-function(capacite,reduction) 
{return(as.double(capacite)*as.double(reduction))}



Aggrega_donnees<-function(Collecte_1,Collecte_2, chemin)
{
  res=list()
  #Cette function prend en input deux tableaux de données et ressort 
  #un tableau aggregé qui contient toutes les donnees en une seule fois 
  #et un tableau qui comporte les lignes se trouvant dans les deux tableaux pour verification
  
  #création de clés unique

#  Collecte_finale=Collecte_1
  
  Collecte_2$idunique %in%   Collecte_1$idunique
  bBool_verification= (sum(  Collecte_2$idunique %in%   Collecte_1$idunique)==0)
  
  if (bBool_verification==TRUE)
  {
    Collecte_finale = rbind(Collecte_1, Collecte_2)
    
  }
  
  
  
  
  write.table(Collecte_finale,paste(chemin,"Collecte_finale.csv",sep=""  ) , sep=";", dec=",",row.names = FALSE)
 # write.table(Tableau_verification,paste(Chemin,"Tableau_verification.csv",sep=""  ) , sep=";", dec=",",row.names = FALSE)
  
  res$Collecte_finale =Collecte_finale
  res$bBool_verification =bBool_verification
  return(res)
  
}

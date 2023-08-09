rm(list=ls())
setwd("S:\\Clients\\FNIH\\R\\Development\\Code\\modifications")
DIR_FILES <- "S:\\Clients\\FNIH\\R\\Raw_Data\\"
library(openxlsx)
library(readxl)
library(stringr)
library(cutpointr)
library(dplyr)
library(pROC)
library(ggplot2)
List<-c("SUBJID","ageyrs","platelet","Fibrosis","Advanced_Fibrosis","Cirrhosis", "Fibrosis_Excluding_Cirrhosis","NASH","NAS","At_Risk_NASH","dm2")
List_Endpoint<-List[-c(1:3)]
Def_Endpoint<-read.csv("S:\\Clients\\FNIH\\R\\Raw_Data\\FNIH_Endpoint_Definition.csv")

#####################################################################################
####################### #loading data and data handling ############################
#####################################################################################


#####################################################################################
####################### #loading data and data handling ############################
#####################################################################################

Siemens<- read.csv(paste0(DIR_FILES,"Nimble_Siemens_24FEB2021.csv"))
Nordic<-  read.csv(paste0(DIR_FILES,"Nimble_Nordic_16APR2021.csv")) # ELISA
#Nordic<-  read.csv(paste0(DIR_FILES,"\\NIMBLE_Nordic_04NOV2021\\Nimble_Nordic_04NOV2021.csv")) #COMBAS records
Labcorp<- read_excel(paste0(DIR_FILES,"NIMBLE_labcorp_18Mar2021.xls"));Labcorp<-Labcorp[-(7453:7455),];Labcorp$Result<-as.numeric(as.character(Labcorp$RESULT))
MetaDat<- read_excel(paste0(DIR_FILES,"nimble_metadata_28mar21.xlsx"))
CRNDat<- read_excel(paste0(DIR_FILES,"nimble_owl_clinds_7apr21.xlsx"),sheet = "Data")
GenfitDat<- read.csv(paste0(DIR_FILES,"Nimble_Genfit_19072021.csv"))
OWLDat<-read.csv(paste0(DIR_FILES,"NIMBLE_OWLiver Panels_Results.csv"))
FibrometerDat1<-read.csv(paste0(DIR_FILES,"fibrometer_LSM_CAP_Agile_FAST_FMVCTE.csv"))
FibrometerDat2<- read.csv(paste0(DIR_FILES,"fibrometer_results_V3G_NAFLD_CirrhoMeter_p.csv"))

PROC3<-read.csv("S:\\Clients\\FNIH\\R\\Raw_Data\\Nimble_Nordic_Cobas_04NOV2021.csv")# COMBAS records


#processing data
# why lapcrop list patient names twice, keep one is enough-> YES!!!
PATIENT_NAME<-do.call("rbind",strsplit(Labcorp$`PATIENT NAME`, "        "))
# sum(PATIENT_NAME[,1]==PATIENT_NAME[,2])==nrow(PATIENT_NAME)
# Labcorp$`PATIENT NAME`<-PATIENT_NAME[,1]
# unique(Labcorp$TEST_ORD_NAME) # 7 unque parameters from Labcorp data
# # [1] "Protein, Total"       "Albumin"              "Bilirubin, Total"     "Bilirubin, Direct"    "Alkaline Phosphatase"
# # [6] "AST (SGOT)"           "ALT (SGPT)"
# 
# unique(Nordic$LBTESTCD) # 6 unique parameters from Nordic data
# #[1] C3M   C4M   PROC3 PROC4 PROC5 PROC6
# 
# unique(Siemens$LBTESTCD) # Siemen data holds the ELF_TEST reslt

# unique(GenfitDat$LBTESTCD) 
# "A2M"   "HBA1C" "NIS4"  "NISX" 

### check the correct subject ID to use in order to merge data
Nordic$SUBJID<-do.call("rbind",strsplit(as.character(Nordic$BARCODE), "LS"))[,1]
Nordic$SUBJID<-do.call("rbind",strsplit(Nordic$SUBJID, "N"))[,2]

PROC3$SUBJID<-do.call("rbind",strsplit(as.character(PROC3$BARCODE), "LS"))[,1]
PROC3$SUBJID<-do.call("rbind",strsplit(PROC3$SUBJID, "N"))[,2]


Labcorp$SUBJID<-do.call("rbind",strsplit(as.character(Labcorp$`PATIENT NAME`), "L"))[,1]
Labcorp$SUBJID<-do.call("rbind",strsplit(Labcorp$SUBJID, "N"))[,2]

Siemens$SUBJID<-do.call("rbind",strsplit(as.character(Siemens$BARCODE), "LS"))[,1]
Siemens$SUBJID<-do.call("rbind",strsplit(Siemens$SUBJID, "N"))[,2]

#CRNDat$SUBJID<-do.call("rbind",strsplit(as.character(CRNDat$current_label), "LS"))[,1]
CRNDat$SUBJID<-do.call("rbind",strsplit(CRNDat$subjid, "N"))[,2]

GenfitDat$SUBJID<-do.call("rbind",strsplit(as.character(GenfitDat$BARCODE), "LS"))[,1]
GenfitDat$SUBJID<-do.call("rbind",strsplit(GenfitDat$SUBJID, "N"))[,2]



OWLDat$SUBJID<-do.call("rbind",strsplit(as.character(OWLDat$SAMPLE.ID), "LS"))[,1]
OWLDat$SUBJID<-do.call("rbind",strsplit(OWLDat$SUBJID, "N"))[,2]

FibrometerDat1$SUBJID<-do.call("rbind",strsplit(FibrometerDat1$SUBJID, "N"))[,2]
FibrometerDat2$SUBJID<-do.call("rbind",strsplit(FibrometerDat2$SUBJID, "N"))[,2]

MetaDat$SUBJID<-MetaDat$nash

FMVCTE_Cohort<-FibrometerDat1$SUBJID
FMVCTE_Cohort<-sub('.', '', FMVCTE_Cohort)

# #check how many unique subjects in each data
# length(unique(Siemens$SUBJID))
# length(unique(Labcorp$SUBJID))
# length(unique(Nordic$SUBJID))
# length(unique(MetaDat$SUBJID))
# 
# sum(unique(Nordic$SUBJID) %in% unique(Siemens$SUBJID))
# sum(unique(Nordic$SUBJID) %in% unique(Labcorp$SUBJID))
# sum(unique(Labcorp$SUBJID) %in% unique(Siemens$SUBJID))
# sum(unique(MetaDat$SUBJID) %in% unique(Siemens$SUBJID))


#get the endpoints ready and handling the data
MetaDat$Fibrosis <-if_else(MetaDat$fibrosisn>=2,1,0)
MetaDat$Advanced_Fibrosis <-if_else(MetaDat$fibrosisn>=3,1,0)
MetaDat$Cirrhosis<-if_else(MetaDat$fibrosisn>=4,1,0)
MetaDat$Fibrosis_Excluding_Cirrhosis<-if_else(MetaDat$fibrosisn>=2,1,0); 
MetaDat$Fibrosis_Excluding_Cirrhosis[MetaDat$Cirrhosis==1]<-NA # this will be used to exclude subjects who are cirrhosis
MetaDat$NASH<-if_else(MetaDat$anynash==1,1,0)
MetaDat$NAS<-if_else(MetaDat$nas>=4,1,0)
MetaDat$At_Risk_NASH<-if_else( (MetaDat$NASH==1 & MetaDat$NAS==1 & MetaDat$Fibrosis==1),1,0)



Siemens$ELF<-Siemens$LBORRES

# update PROC3 becusae new data was provided on Nov 4 2021
# PROC3<-Nordic[Nordic$LBTEST=="Pro-C3",]
# PROC3$'Pro-C3'<-PROC3$LBORRES
# PROC3<-PROC3[,c("SUBJID","Pro-C3")]

PROC3$'Pro-C3'<-PROC3$LBORRES
PROC3<-PROC3[,c("SUBJID","Pro-C3")]


PROC3_ELISA<-Nordic[Nordic$LBTEST=="Pro-C3",]
PROC3_ELISA$'Pro-C3-ELISA'<-PROC3_ELISA$LBORRES
PROC3_ELISA<-PROC3_ELISA[,c("SUBJID","Pro-C3-ELISA")]


PROC4<-Nordic[Nordic$LBTEST=="Pro-C4",]
PROC4$'Pro-C4'<-PROC4$LBORRES
PROC4<-PROC4[,c("SUBJID","Pro-C4")]


PROC5<-Nordic[Nordic$LBTEST=="Pro-C5",]
PROC5$'Pro-C5'<-PROC5$LBORRES
PROC5<-PROC5[,c("SUBJID","Pro-C5")]


PROC6<-Nordic[Nordic$LBTEST=="Pro-C6",]
PROC6$'Pro-C6'<-PROC6$LBORRES
PROC6<-PROC6[,c("SUBJID","Pro-C6")]

ALT<-Labcorp[Labcorp$TEST_ORD_NAME=="ALT (SGPT)",]
ALT$ALT<-ALT$Result
ALT<-merge(ALT, CRNDat[,c("SUBJID","alt")],all=TRUE)
ALT$ALT[is.na(ALT$ALT)]<-ALT$alt[is.na(ALT$ALT)]
ALT<-ALT[,c("SUBJID","ALT")]

AST<-Labcorp[Labcorp$TEST_ORD_NAME=="AST (SGOT)",]
AST$AST<-AST$Result
AST<-merge(AST, CRNDat[,c("SUBJID","ast")],all=TRUE)
AST$AST[is.na(AST$AST)]<-AST$ast[is.na(AST$AST)]
AST<-AST[,c("SUBJID","AST")]


TotalBilirubin<-Labcorp[Labcorp$TEST_ORD_NAME=="Bilirubin, Total",]
TotalBilirubin$TotalBilirubin<-TotalBilirubin$Result
TotalBilirubin<-merge(TotalBilirubin, CRNDat[,c("SUBJID")],by="SUBJID",all=TRUE)
TotalBilirubin<-TotalBilirubin[,c("SUBJID","TotalBilirubin")]




NIS4<-GenfitDat[GenfitDat$LBTESTCD=="NIS4",]
NIS4$NIS4<-NIS4$LBORRES
NIS4<-NIS4[,c("SUBJID","NIS4")]

OWLDat$OWL<-OWLDat$DIGANOSIS
OWL<-OWLDat[,c("SUBJID","OWL")]

#Agile4	Agile3Plus	FAST	FMVCTE	FMVCTE_light and FM3G V - FibrosisScore	CirrhosisScore	NAFLD - FibrosisScore
Agile4<-FibrometerDat1[,c("SUBJID","Agile4")]
Agile3Plus<-FibrometerDat1[,c("SUBJID","Agile3Plus")]
FAST<-FibrometerDat1[,c("SUBJID","FAST")]
FMVCTE<-FibrometerDat1[,c("SUBJID","FMVCTE")]
FMVCTE_light<-FibrometerDat1[,c("SUBJID","FMVCTE_light")]

NAFLD<-FibrometerDat2[,c("SUBJID","NAFLD...FibrosisScore")]; colnames(NAFLD)<-c("SUBJID","NAFLD")
CirrhosisScore<-FibrometerDat2[,c("SUBJID","CirrhosisScore")]
FM3GV<-FibrometerDat2[,c("SUBJID","FM3G.V...FibrosisScore")]; colnames(FM3GV)<-c("SUBJID","FM3GV")

#####################################################################################
################### creating the dataset ready for analysis #####################
#####################################################################################
Dat_ELF<-merge(Siemens[,c("SUBJID","LBTESTCD","ELF")], MetaDat[,List],by="SUBJID")
saveRDS(Dat_ELF,file="Dat_ELF.rds")

Dat_PROC3<-merge(PROC3, MetaDat[,List],by="SUBJID")
saveRDS(Dat_PROC3,file="Dat_PROC3.rds")


Dat_NIS4<-merge(NIS4, MetaDat[,List],by="SUBJID")
saveRDS(Dat_NIS4,file="Dat_NIS4.rds")



Dat_All<-merge(Siemens[,c("SUBJID","ELF")], MetaDat,by="SUBJID")
Dat_All<-merge(PROC3,Dat_All,by="SUBJID")
Dat_All<-merge(PROC3_ELISA,Dat_All,by="SUBJID")
Dat_All<-merge(PROC4,Dat_All,by="SUBJID")
Dat_All<-merge(PROC5,Dat_All,by="SUBJID")
Dat_All<-merge(PROC6,Dat_All,by="SUBJID")
Dat_All<-merge(AST,Dat_All,by="SUBJID")
Dat_All<-merge(ALT,Dat_All,by="SUBJID")
Dat_All<-merge(TotalBilirubin,Dat_All,by="SUBJID")
Dat_All<-merge(NIS4,Dat_All,by="SUBJID")
Dat_All<-merge(OWL,Dat_All,by="SUBJID")
Dat_All<-merge(Agile4,Dat_All,by="SUBJID",all.y = TRUE)
Dat_All<-merge(Agile3Plus,Dat_All,by="SUBJID",all.y = TRUE)
Dat_All<-merge(FAST,Dat_All,by="SUBJID",all.y = TRUE)
Dat_All<-merge(FMVCTE,Dat_All,by="SUBJID",all.y = TRUE)
Dat_All<-merge(FMVCTE_light,Dat_All,by="SUBJID",all.y = TRUE)
Dat_All<-merge(NAFLD,Dat_All,by="SUBJID",all.y = TRUE)
Dat_All<-merge(CirrhosisScore,Dat_All,by="SUBJID",all.y = TRUE)
Dat_All<-merge(FM3GV,Dat_All,by="SUBJID",all.y = TRUE)

Dat_All$AGE<-floor(Dat_All$ageyrs)
# calculating FIB-4
#FIB4<-Age (years)×AST (U/L)/[PLT(109/L)×ALT1/2 (U/L)].  
Dat_All$'FIB-4'<-(Dat_All$AGE*Dat_All$AST)/(Dat_All$platelet *sqrt(Dat_All$ALT))

# calculating ADPAT
#ADPAT<-exp(log10(Age*Proc-C3/sqrt(PLT)) + Diabetes.  
Dat_All$ADAPT<-exp(log((Dat_All$AGE*Dat_All$'Pro-C3-ELISA')/sqrt(Dat_All$platelet),10))+ Dat_All$dm2  

MetaData<-Dat_All

################################################################################################
### Summary table for all subjects
################################################################################################




Table_Fun_Mean<-function(MetaData,var){
  
  MetaData0<-MetaData[MetaData$fibrosisn==0,] 
  MetaData1<-MetaData[MetaData$fibrosisn==1,] 
  MetaData2<-MetaData[MetaData$fibrosisn==2,] 
  MetaData3<-MetaData[MetaData$fibrosisn==3,] 
  MetaData4<-MetaData[MetaData$fibrosisn==4,] 
  
  
  
  Tab_Total<-list()
  Tab_Total[[var]]<- "------------"
  Tab_Total[["Mean (SD)"]]<- paste0(format( round(mean(MetaData[,var],na.rm = TRUE),2), nsmall = 2),"(", format( round(sd(MetaData[,var],na.rm = TRUE),2), nsmall = 2),")" )
  Tab_Total[["Median (Min,Max)"]]<-paste0(format( round(median(MetaData[,var],na.rm = TRUE),2), nsmall = 2) , "(", format( round(min(MetaData[,var],na.rm = TRUE),2), nsmall = 2),", ", 
                                          format( round(max(MetaData[,var],na.rm = TRUE),2), nsmall = 2) ,")" )
  
  
  Tab0<-list()
  Tab0[[var]]<- "------------"
  Tab0[["Mean (SD)"]]<- paste0(format( round(mean(MetaData0[,var],na.rm = TRUE),2), nsmall = 2),"(", format( round(sd(MetaData0[,var],na.rm = TRUE),2), nsmall = 2),")" )
  Tab0[["Median (Min,Max)"]]<-paste0(format( round(median(MetaData0[,var],na.rm = TRUE),2), nsmall = 2) , "(", format( round(min(MetaData0[,var],na.rm = TRUE),2), nsmall = 2),", ", 
                                     format( round(max(MetaData0[,var],na.rm = TRUE),1), nsmall = 2) ,")" )
  
  
  
  Tab1<-list()
  Tab1[[var]]<- "------------"
  Tab1[["Mean (SD)"]]<- paste0(format( round(mean(MetaData1[,var],na.rm = TRUE),2), nsmall = 2),"(", format( round(sd(MetaData1[,var],na.rm = TRUE),2), nsmall = 2),")" )
  Tab1[["Median (Min,Max)"]]<-paste0(format( round(median(MetaData1[,var],na.rm = TRUE),2), nsmall = 2) , "(", format( round(min(MetaData1[,var],na.rm = TRUE),2), nsmall = 2),", ", 
                                     format( round(max(MetaData1[,var],na.rm = TRUE),2), nsmall = 2) ,")" )
  
  Tab2<-list()
  Tab2[[var]]<- "------------"
  Tab2[["Mean (SD)"]]<- paste0(format( round(mean(MetaData2[,var],na.rm = TRUE),2), nsmall = 2),"(", format( round(sd(MetaData2[,var],na.rm = TRUE),2), nsmall = 2),")" )
  Tab2[["Median (Min,Max)"]]<-paste0(format( round(median(MetaData2[,var],na.rm = TRUE),2), nsmall = 2) , "(", format( round(min(MetaData2[,var],na.rm = TRUE),2), nsmall = 2),", ", 
                                     format( round(max(MetaData2[,var],na.rm = TRUE),2), nsmall = 2) ,")" )
  
  Tab3<-list()
  Tab3[[var]]<- "------------"
  Tab3[["Mean (SD)"]]<- paste0(format( round(mean(MetaData3[,var],na.rm = TRUE),2), nsmall = 2),"(", format( round(sd(MetaData3[,var],na.rm = TRUE),2), nsmall = 2),")" )
  Tab3[["Median (Min,Max)"]]<-paste0(format( round(median(MetaData3[,var],na.rm = TRUE),2), nsmall = 2) , "(", format( round(min(MetaData3[,var],na.rm = TRUE),2), nsmall =2),", ", 
                                     format( round(max(MetaData3[,var],na.rm = TRUE),2), nsmall = 2) ,")" )
  
  
  
  Tab4<-list()
  Tab4[[var]]<- "------------"
  Tab4[["Mean (SD)"]]<- paste0(format( round(mean(MetaData4[,var],na.rm = TRUE),2), nsmall = 2),"(", format( round(sd(MetaData4[,var],na.rm = TRUE),2), nsmall = 2),")" )
  Tab4[["Median (Min,Max)"]]<-paste0(format( round(median(MetaData4[,var],na.rm = TRUE),2), nsmall = 2) , "(", format( round(min(MetaData4[,var],na.rm = TRUE),2), nsmall = 2),", ", 
                                     format( round(max(MetaData4[,var],na.rm = TRUE),2), nsmall = 2) ,")" )
  
  
  
  Table<-data.frame(#Grittiness=names(Tab),
    Overall =unlist(Tab_Total),
    Stage0=unlist(Tab0),
    Stage1=unlist(Tab1),
    Stage2=unlist(Tab2),
    Stage3=unlist(Tab3),
    Stage4=unlist(Tab4))
  
  # colnames( Table)<-  c("Histological (biopsy) classification:",
  #                      paste0("Positive = ",Endpoint_pos_var,";"),
  #                     paste0("Negative = ",Endpoint_neg_var))
  
  rownames(Table)<-c(var,paste0(var,"_Mean (SD)"),paste0(var,"_Median (Min,Max)"))
  
  return(Table)
}


#Age (yrs)
Age_Tab<-Table_Fun_Mean(MetaData=MetaData,var="ageyrs")

#Males (%)
Male_Tab<-c(round(mean(MetaData$male==1, na.rm=TRUE)*100,2), 
            round(mean(MetaData$male[MetaData$fibrosisn==0]==1, na.rm=TRUE)*100,2), 
            round(mean(MetaData$male[MetaData$fibrosisn==1]==1, na.rm=TRUE)*100,2),
            round(mean(MetaData$male[MetaData$fibrosisn==2]==1, na.rm=TRUE)*100,2),
            round(mean(MetaData$male[MetaData$fibrosisn==3]==1, na.rm=TRUE)*100,2),
            round(mean(MetaData$male[MetaData$fibrosisn==4]==1, na.rm=TRUE)*100,2))
Male_Tab<-matrix(Male_Tab, nrow=1, ncol=6)
rownames(Male_Tab)=c("Male(%)")
colnames(Male_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")

#Caucasian (n)
Caucasian_Tab<-c(sum(MetaData$racetxt=="white", na.rm=TRUE), 
                 sum(MetaData$racetxt[MetaData$fibrosisn==0]=="white", na.rm=TRUE), 
                 sum(MetaData$racetxt[MetaData$fibrosisn==1]=="white", na.rm=TRUE),
                 sum(MetaData$racetxt[MetaData$fibrosisn==2]=="white", na.rm=TRUE),
                 sum(MetaData$racetxt[MetaData$fibrosisn==3]=="white", na.rm=TRUE),
                 sum(MetaData$racetxt[MetaData$fibrosisn==4]=="white", na.rm=TRUE))
Caucasian_Tab<-matrix(Caucasian_Tab, nrow=1, ncol=6)
rownames(Caucasian_Tab)=c("Caucasian(n)")
colnames(Caucasian_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")



#BMI (kg/m2)
BMI_Tab<-Table_Fun_Mean(MetaData=MetaData,var="bmi") 

#T2DM (n)
T2DM_Tab<-c(sum(MetaData$dm2==1, na.rm=TRUE), 
            sum(MetaData$dm2[MetaData$fibrosisn==0]==1, na.rm=TRUE), 
            sum(MetaData$dm2[MetaData$fibrosisn==1]==1, na.rm=TRUE),
            sum(MetaData$dm2[MetaData$fibrosisn==2]==1, na.rm=TRUE),
            sum(MetaData$dm2[MetaData$fibrosisn==3]==1, na.rm=TRUE),
            sum(MetaData$dm2[MetaData$fibrosisn==4]==1, na.rm=TRUE))
T2DM_Tab<-matrix(T2DM_Tab, nrow=1, ncol=6)
rownames(T2DM_Tab)=c("T2DM(n)")
colnames(T2DM_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")



#Hypertension (n)
Hypertension_Tab<-c(sum(MetaData$tx_htn==1, na.rm=TRUE), 
                    sum(MetaData$tx_htn[MetaData$fibrosisn==0]==1, na.rm=TRUE), 
                    sum(MetaData$tx_htn[MetaData$fibrosisn==1]==1, na.rm=TRUE),
                    sum(MetaData$tx_htn[MetaData$fibrosisn==2]==1, na.rm=TRUE),
                    sum(MetaData$tx_htn[MetaData$fibrosisn==3]==1, na.rm=TRUE),
                    sum(MetaData$tx_htn[MetaData$fibrosisn==4]==1, na.rm=TRUE))
Hypertension_Tab<-matrix(Hypertension_Tab, nrow=1, ncol=6)
rownames(Hypertension_Tab)=c("Hypertension(n) ")
colnames(Hypertension_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")




#Total Cholesterol (mg/dl)
TotalCholesterol_Tab<-Table_Fun_Mean(MetaData=MetaData,var="cholesterol_t")


#LDL-C (mg/dl)
LDLC_Tab<-Table_Fun_Mean(MetaData=MetaData,var="cholesterol_ldl")

#Triglycerides (mg/dl)
Triglycerides_Tab<-Table_Fun_Mean(MetaData=MetaData,var="triglyc")


#eGFR (ml/min)
eGFR_Tab1<-Table_Fun_Mean(MetaData=MetaData,var="gfr_epi")
eGFR_Tab2<-Table_Fun_Mean(MetaData=MetaData,var="gfr_mdrd")


#HBA1C (%)
HBA1C_Tab<-Table_Fun_Mean(MetaData=MetaData,var="hba1c")

#Hemoglobin (mg/dl)
Hemoglobin_Tab<-Table_Fun_Mean(MetaData=MetaData,var="Hb")

#WBC (/mm3)
WBC_Tab<-Table_Fun_Mean(MetaData=MetaData,var="wbc")

#Platelets (/mm3)
Platelets_Tab<-Table_Fun_Mean(MetaData=MetaData,var="platelet")

#FIB-4
#MetaDat$FIB4<-(round(MetaDat$ageyrs,0)*MetaDat$AST)/(MetaDat$platelet *sqrt(MetaDat$ALT))
FIB4_Tab<-Table_Fun_Mean(MetaData=MetaData,var="FIB-4")

# #Total Bilirubin (mg/dl)
TotalBilirubin_Tab<-Table_Fun_Mean(MetaData=MetaData,var="TotalBilirubin")

# AST (IU/l)
AST_Tab<-Table_Fun_Mean(MetaData=MetaData,var="AST")

# ALT (IU/l)
ALT_Tab<-Table_Fun_Mean(MetaData=MetaData,var="ALT")

# Alk Phos (IU/l)
Alk_Phos_Tab=rep(NA,6)
Alk_Phos_Tab<-matrix(Alk_Phos_Tab, nrow=1, ncol=6)
rownames(Alk_Phos_Tab)=c("Alk Phos(n)")
colnames(Alk_Phos_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")



# INR
INR_Tab<-Table_Fun_Mean(MetaData=MetaData,var="inr")

# NAFL (n) (i.e., anynash==0)
NAFL_Tab<-c(sum(MetaData$anynash==0, na.rm=TRUE), 
            sum(MetaData$anynash[MetaData$fibrosisn==0]==0, na.rm=TRUE), 
            sum(MetaData$anynash[MetaData$fibrosisn==1]==0, na.rm=TRUE),
            sum(MetaData$anynash[MetaData$fibrosisn==2]==0, na.rm=TRUE),
            sum(MetaData$anynash[MetaData$fibrosisn==3]==0, na.rm=TRUE),
            sum(MetaData$anynash[MetaData$fibrosisn==4]==0, na.rm=TRUE))
NAFL_Tab<-matrix(NAFL_Tab, nrow=1, ncol=6)
rownames(NAFL_Tab)=c("NAFL(n)")
colnames(NAFL_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")



# Definite steatohepatitis (n)
Definite_Steatohepatitis_Tab=rep(NA,6)
Definite_Steatohepatitis_Tab<-matrix(Definite_Steatohepatitis_Tab, nrow=1, ncol=6)
rownames(Definite_Steatohepatitis_Tab)=c("Definite Steatohepatitis(n)")
colnames(Definite_Steatohepatitis_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")


# Borderline steatohepatitis (n)
Borderline_Steatohepatitis_Tab=rep(NA,6)
Borderline_Steatohepatitis_Tab<-matrix(Borderline_Steatohepatitis_Tab, nrow=1, ncol=6)
rownames(Borderline_Steatohepatitis_Tab)=c("Borderline Steatohepatitis(n)")
colnames(Borderline_Steatohepatitis_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")

# 
# Steatosis grade
Steatosis_Tab<-Table_Fun_Mean(MetaData=MetaData,var="steatosis")

# Lobular Inflammation
Lobular_Tab<-Table_Fun_Mean(MetaData=MetaData,var="lobular")

# Hepatocellular Ballooning
Hepatocellular_Balloon_Tab<-Table_Fun_Mean(MetaData=MetaData,var="balloon")

# NAFLD Activity Score
NAFLD_Tab<-Table_Fun_Mean(MetaData=MetaData,var="NAFLD")

# Time from biopsy to study entry (days)
Biopsy_Time_Tab<-Table_Fun_Mean(MetaData=MetaData,var="bxenr_days")

# Statin Use (n)
Statin_Tab<-c(sum(MetaData$tx_statin==1, na.rm=TRUE), 
              sum(MetaData$tx_statin[MetaData$fibrosisn==0]==1, na.rm=TRUE), 
              sum(MetaData$tx_statin[MetaData$fibrosisn==1]==1, na.rm=TRUE),
              sum(MetaData$tx_statin[MetaData$fibrosisn==2]==1, na.rm=TRUE),
              sum(MetaData$tx_statin[MetaData$fibrosisn==3]==1, na.rm=TRUE),
              sum(MetaData$tx_statin[MetaData$fibrosisn==4]==1, na.rm=TRUE))
Statin_Tab<-matrix(Statin_Tab, nrow=1, ncol=6)
rownames(Statin_Tab)=c("Statin Use(n)")
colnames(Statin_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")


# PPAR-?? agonist (n)
PPAR_Agonist_Tab=rep(NA,6)
PPAR_Agonist_Tab<-matrix(PPAR_Agonist_Tab, nrow=1, ncol=6)
rownames(PPAR_Agonist_Tab)=c("PPAR-Gama agonist(n)")
colnames(PPAR_Agonist_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")


# Vitamin E (n)
VitaminE_Tab=rep(NA,6)
VitaminE_Tab<-matrix(VitaminE_Tab, nrow=1, ncol=6)
rownames(VitaminE_Tab)=c("Vitamin E(n)")
colnames(VitaminE_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")

# ARB (n)
ARB_Tab=rep(NA,6)
ARB_Tab<-matrix(ARB_Tab, nrow=1, ncol=6)
rownames(ARB_Tab)=c("ARB(n)")
colnames(ARB_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")



Tab_All<-rbind(Age_Tab,
               Male_Tab,
               Caucasian_Tab,
               BMI_Tab,
               T2DM_Tab,
               Hypertension_Tab,
               TotalCholesterol_Tab,
               LDLC_Tab,
               Triglycerides_Tab,
               eGFR_Tab1,
               HBA1C_Tab,
               Hemoglobin_Tab,
               WBC_Tab,
               Platelets_Tab,
               FIB4_Tab,
               TotalBilirubin_Tab,
               AST_Tab,
               ALT_Tab,
               Alk_Phos_Tab,
               INR_Tab,
               NAFL_Tab,
               Definite_Steatohepatitis_Tab,
               Borderline_Steatohepatitis_Tab,
               Steatosis_Tab,
               Lobular_Tab,
               Hepatocellular_Balloon_Tab,
               NAFLD_Tab,
               Biopsy_Time_Tab,
               Statin_Tab,
               PPAR_Agonist_Tab,
               VitaminE_Tab,
               ARB_Tab)

#rownames(Tab_All)=c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")

#write.csv(Tab_All,file="C:/Users/Yongfang.Lu/OneDrive - Cytel/Desktop/FNIH/Table1_Demographic, Clinical and Laboratory Data from the Study Cohort.csv")



################################################################################################
### Summary table for subject from FMVCT dataset 396 subjects
################################################################################################
sel.row<-which(MetaDat$SUBJID %in% FMVCTE_Cohort==TRUE)
MetaData_FMVCTE<-MetaDat[sel.row, ]
MetaData<-as.data.frame(MetaData_FMVCTE)

#Age (yrs)
Age_Tab<-Table_Fun_Mean(MetaData=MetaData,var="ageyrs")

#Males (%)
Male_Tab<-c(round(mean(MetaData$male==1, na.rm=TRUE)*100,2), 
            round(mean(MetaData$male[MetaData$fibrosisn==0]==1, na.rm=TRUE)*100,2), 
            round(mean(MetaData$male[MetaData$fibrosisn==1]==1, na.rm=TRUE)*100,2),
            round(mean(MetaData$male[MetaData$fibrosisn==2]==1, na.rm=TRUE)*100,2),
            round(mean(MetaData$male[MetaData$fibrosisn==3]==1, na.rm=TRUE)*100,2),
            round(mean(MetaData$male[MetaData$fibrosisn==4]==1, na.rm=TRUE)*100,2))
Male_Tab<-matrix(Male_Tab, nrow=1, ncol=6)
rownames(Male_Tab)=c("Male(%)")
colnames(Male_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")

#Caucasian (n)
Caucasian_Tab<-c(sum(MetaData$racetxt=="white", na.rm=TRUE), 
                 sum(MetaData$racetxt[MetaData$fibrosisn==0]=="white", na.rm=TRUE), 
                 sum(MetaData$racetxt[MetaData$fibrosisn==1]=="white", na.rm=TRUE),
                 sum(MetaData$racetxt[MetaData$fibrosisn==2]=="white", na.rm=TRUE),
                 sum(MetaData$racetxt[MetaData$fibrosisn==3]=="white", na.rm=TRUE),
                 sum(MetaData$racetxt[MetaData$fibrosisn==4]=="white", na.rm=TRUE))
Caucasian_Tab<-matrix(Caucasian_Tab, nrow=1, ncol=6)
rownames(Caucasian_Tab)=c("Caucasian(n)")
colnames(Caucasian_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")



#BMI (kg/m2)
BMI_Tab<-Table_Fun_Mean(MetaData=MetaData,var="bmi") 

#T2DM (n)
T2DM_Tab<-c(sum(MetaData$dm2==1, na.rm=TRUE), 
            sum(MetaData$dm2[MetaData$fibrosisn==0]==1, na.rm=TRUE), 
            sum(MetaData$dm2[MetaData$fibrosisn==1]==1, na.rm=TRUE),
            sum(MetaData$dm2[MetaData$fibrosisn==2]==1, na.rm=TRUE),
            sum(MetaData$dm2[MetaData$fibrosisn==3]==1, na.rm=TRUE),
            sum(MetaData$dm2[MetaData$fibrosisn==4]==1, na.rm=TRUE))
T2DM_Tab<-matrix(T2DM_Tab, nrow=1, ncol=6)
rownames(T2DM_Tab)=c("T2DM(n)")
colnames(T2DM_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")



#Hypertension (n)
Hypertension_Tab<-c(sum(MetaData$tx_htn==1, na.rm=TRUE), 
                    sum(MetaData$tx_htn[MetaData$fibrosisn==0]==1, na.rm=TRUE), 
                    sum(MetaData$tx_htn[MetaData$fibrosisn==1]==1, na.rm=TRUE),
                    sum(MetaData$tx_htn[MetaData$fibrosisn==2]==1, na.rm=TRUE),
                    sum(MetaData$tx_htn[MetaData$fibrosisn==3]==1, na.rm=TRUE),
                    sum(MetaData$tx_htn[MetaData$fibrosisn==4]==1, na.rm=TRUE))
Hypertension_Tab<-matrix(Hypertension_Tab, nrow=1, ncol=6)
rownames(Hypertension_Tab)=c("Hypertension(n) ")
colnames(Hypertension_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")




#Total Cholesterol (mg/dl)
TotalCholesterol_Tab<-Table_Fun_Mean(MetaData=MetaData,var="cholesterol_t")


#LDL-C (mg/dl)
LDLC_Tab<-Table_Fun_Mean(MetaData=MetaData,var="cholesterol_ldl")

#Triglycerides (mg/dl)
Triglycerides_Tab<-Table_Fun_Mean(MetaData=MetaData,var="triglyc")


#eGFR (ml/min)
eGFR_Tab1<-Table_Fun_Mean(MetaData=MetaData,var="gfr_epi")
eGFR_Tab2<-Table_Fun_Mean(MetaData=MetaData,var="gfr_mdrd")


#HBA1C (%)
HBA1C_Tab<-Table_Fun_Mean(MetaData=MetaData,var="hba1c")

#Hemoglobin (mg/dl)
Hemoglobin_Tab<-Table_Fun_Mean(MetaData=MetaData,var="Hb")

#WBC (/mm3)
WBC_Tab<-Table_Fun_Mean(MetaData=MetaData,var="wbc")

#Platelets (/mm3)
Platelets_Tab<-Table_Fun_Mean(MetaData=MetaData,var="platelet")

#FIB-4
#MetaDat$FIB4<-(round(MetaDat$ageyrs,0)*MetaDat$AST)/(MetaDat$platelet *sqrt(MetaDat$ALT))
FIB4_Tab<-Table_Fun_Mean(MetaData=MetaData,var="FIB-4")

# #Total Bilirubin (mg/dl)
TotalBilirubin_Tab<-Table_Fun_Mean(MetaData=MetaData,var="TotalBilirubin")

# AST (IU/l)
AST_Tab<-Table_Fun_Mean(MetaData=MetaData,var="AST")

# ALT (IU/l)
ALT_Tab<-Table_Fun_Mean(MetaData=MetaData,var="ALT")

# Alk Phos (IU/l)
Alk_Phos_Tab=rep(NA,6)
Alk_Phos_Tab<-matrix(Alk_Phos_Tab, nrow=1, ncol=6)
rownames(Alk_Phos_Tab)=c("Alk Phos(n)")
colnames(Alk_Phos_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")



# INR
INR_Tab<-Table_Fun_Mean(MetaData=MetaData,var="inr")

# NAFL (n) (i.e., anynash==0)
NAFL_Tab<-c(sum(MetaData$anynash==0, na.rm=TRUE), 
            sum(MetaData$anynash[MetaData$fibrosisn==0]==0, na.rm=TRUE), 
            sum(MetaData$anynash[MetaData$fibrosisn==1]==0, na.rm=TRUE),
            sum(MetaData$anynash[MetaData$fibrosisn==2]==0, na.rm=TRUE),
            sum(MetaData$anynash[MetaData$fibrosisn==3]==0, na.rm=TRUE),
            sum(MetaData$anynash[MetaData$fibrosisn==4]==0, na.rm=TRUE))
NAFL_Tab<-matrix(NAFL_Tab, nrow=1, ncol=6)
rownames(NAFL_Tab)=c("NAFL(n)")
colnames(NAFL_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")



# Definite steatohepatitis (n)
Definite_Steatohepatitis_Tab=rep(NA,6)
Definite_Steatohepatitis_Tab<-matrix(Definite_Steatohepatitis_Tab, nrow=1, ncol=6)
rownames(Definite_Steatohepatitis_Tab)=c("Definite Steatohepatitis(n)")
colnames(Definite_Steatohepatitis_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")


# Borderline steatohepatitis (n)
Borderline_Steatohepatitis_Tab=rep(NA,6)
Borderline_Steatohepatitis_Tab<-matrix(Borderline_Steatohepatitis_Tab, nrow=1, ncol=6)
rownames(Borderline_Steatohepatitis_Tab)=c("Borderline Steatohepatitis(n)")
colnames(Borderline_Steatohepatitis_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")

# 
# Steatosis grade
Steatosis_Tab<-Table_Fun_Mean(MetaData=MetaData,var="steatosis")

# Lobular Inflammation
Lobular_Tab<-Table_Fun_Mean(MetaData=MetaData,var="lobular")

# Hepatocellular Ballooning
Hepatocellular_Balloon_Tab<-Table_Fun_Mean(MetaData=MetaData,var="balloon")

# NAFLD Activity Score
NAFLD_Tab<-Table_Fun_Mean(MetaData=MetaData,var="NAFLD")

# Time from biopsy to study entry (days)
Biopsy_Time_Tab<-Table_Fun_Mean(MetaData=MetaData,var="bxenr_days")

# Statin Use (n)
Statin_Tab<-c(sum(MetaData$tx_statin==1, na.rm=TRUE), 
              sum(MetaData$tx_statin[MetaData$fibrosisn==0]==1, na.rm=TRUE), 
              sum(MetaData$tx_statin[MetaData$fibrosisn==1]==1, na.rm=TRUE),
              sum(MetaData$tx_statin[MetaData$fibrosisn==2]==1, na.rm=TRUE),
              sum(MetaData$tx_statin[MetaData$fibrosisn==3]==1, na.rm=TRUE),
              sum(MetaData$tx_statin[MetaData$fibrosisn==4]==1, na.rm=TRUE))
Statin_Tab<-matrix(Statin_Tab, nrow=1, ncol=6)
rownames(Statin_Tab)=c("Statin Use(n)")
colnames(Statin_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")


# PPAR-?? agonist (n)
PPAR_Agonist_Tab=rep(NA,6)
PPAR_Agonist_Tab<-matrix(PPAR_Agonist_Tab, nrow=1, ncol=6)
rownames(PPAR_Agonist_Tab)=c("PPAR-Gama agonist(n)")
colnames(PPAR_Agonist_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")


# Vitamin E (n)
VitaminE_Tab=rep(NA,6)
VitaminE_Tab<-matrix(VitaminE_Tab, nrow=1, ncol=6)
rownames(VitaminE_Tab)=c("Vitamin E(n)")
colnames(VitaminE_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")

# ARB (n)
ARB_Tab=rep(NA,6)
ARB_Tab<-matrix(ARB_Tab, nrow=1, ncol=6)
rownames(ARB_Tab)=c("ARB(n)")
colnames(ARB_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")



Tab_All_FMVCTE<-rbind(Age_Tab,
                      Male_Tab,
                      Caucasian_Tab,
                      BMI_Tab,
                      T2DM_Tab,
                      Hypertension_Tab,
                      TotalCholesterol_Tab,
                      LDLC_Tab,
                      Triglycerides_Tab,
                      eGFR_Tab1,
                      HBA1C_Tab,
                      Hemoglobin_Tab,
                      WBC_Tab,
                      Platelets_Tab,
                      FIB4_Tab,
                      TotalBilirubin_Tab,
                      AST_Tab,
                      ALT_Tab,
                      Alk_Phos_Tab,
                      INR_Tab,
                      NAFL_Tab,
                      Definite_Steatohepatitis_Tab,
                      Borderline_Steatohepatitis_Tab,
                      Steatosis_Tab,
                      Lobular_Tab,
                      Hepatocellular_Balloon_Tab,
                      NAFLD_Tab,
                      Biopsy_Time_Tab,
                      Statin_Tab,
                      PPAR_Agonist_Tab,
                      VitaminE_Tab,
                      ARB_Tab)

#rownames(Tab_All)=c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")


#write.csv(Tab_All_FMVCTE,file="C:/Users/Yongfang.Lu/OneDrive - Cytel/Desktop/FNIH/SupplementalTable2_Baseline demographic, clinical and laboratory data for Fibrometer-VCTE cohort.csv")


st_Tab<-c(sum(MetaData$nashdx99==1, na.rm=TRUE), 
          sum(MetaData$racetxt[MetaData$fibrosisn==0]==1, na.rm=TRUE), 
          sum(MetaData$racetxt[MetaData$fibrosisn==1]==1, na.rm=TRUE),
          sum(MetaData$racetxt[MetaData$fibrosisn==2]==1, na.rm=TRUE),
          sum(MetaData$racetxt[MetaData$fibrosisn==3]==1, na.rm=TRUE),
          sum(MetaData$racetxt[MetaData$fibrosisn==4]==1, na.rm=TRUE))

st_Tab<-matrix(st_Tab, nrow=1, ncol=6)
rownames(st_Tab)=c("st(n)")
colnames(st_Tab)<-c("Overall","Stage0","Stage1","Stage2","Stage3","Stage4")
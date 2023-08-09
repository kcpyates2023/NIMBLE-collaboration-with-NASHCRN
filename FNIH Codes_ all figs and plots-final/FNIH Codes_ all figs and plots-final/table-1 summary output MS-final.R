rm(list=ls())
setwd("S:\\Clients\\FNIH\\R\\Development\\Code\\modifications")
DIR_FILES <- "S:\\Clients\\FNIH\\R\\Raw_Data\\"
library(openxlsx)
library(readxl)
library(tidyverse)
library(lubridate)


##  creating appropriate dataset for summary statistics from Metadat


MetaDat<- read_excel(paste0(DIR_FILES,"nimble_metadata_28mar21.xlsx"))
subject<- MetaDat$nash
bmi.subj<- c(which(subject=="1214"), which(subject=="1365"))
stage.fib<-MetaDat$fibrosisn
#age<-interval(MetaDat$dob, MetaDat$sampledt) / duration(num = 1, units = "years")
age<-floor(MetaDat$ageC)
gender<-factor(MetaDat$male, labels=c("Female","Male"))
race<- factor(MetaDat$raceH, labels=c("W-Non Hispanic","B-Non Hispanic","Others","Hispanic"))
NASH<-if_else(MetaDat$anynash==1,1,0)
NAFL<-if_else(MetaDat$anynash==0,1,0)
cvnames<-c("bmi","waist","dm2","htn","Hb","wbc","platelet","glucose","insulin","cholesterol_t",
           "cholesterol_ldl","cholesterol_hdl","triglyc","steatosis","lobular","balloon","portinflam","nas", "inr","nashdx99")

summary_data.md<- data.frame(subject,stage.fib,age,gender, race, NASH, NAFL,MetaDat[,colnames(MetaDat) %in% cvnames]  )
summary_data.md$bmi[bmi.subj]<-c(43.7, 49.7)


## creating appropriate dataset for summary statistics from Labcorp

Labcorp<- read_excel(paste0(DIR_FILES,"NIMBLE_labcorp_18Mar2021.xls"))
Labcorp<-Labcorp[-(7453:7455),]
PATIENT_NAME<-do.call("rbind",strsplit(Labcorp$`PATIENT NAME`, "        "))
Labcorp$`PATIENT.NAME`<-PATIENT_NAME[,1]
Labcorp$SUBJID<-do.call("rbind",strsplit(as.character(Labcorp$PATIENT.NAME), "L"))[,1]
Labcorp$SUBJID<-do.call("rbind",strsplit(Labcorp$SUBJID, "N"))[,2]

labdata<-Labcorp

#subject<-labdata$SUBJID 

ast<- subset(labdata, TEST_ORD_NAME=="AST (SGOT)", select =c("SUBJID","RESULT"))
ast$RESULT<- as.numeric(ast$RESULT)
astowl<-subset(read.csv(paste0(DIR_FILES,"nimble_owl_clinds_7apr21.csv")), select=c(current_label, ast))
astowl$SUBJID<-do.call("rbind",strsplit(as.character(astowl$current_label), "L"))[,1]
astowl$SUBJID<-do.call("rbind",strsplit(astowl$SUBJID, "N"))[,2]

astdata<- merge(astowl, ast, by="SUBJID", all=T)
ast.na.results<-which(is.na(astdata$RESULT) == T)
astdata$RESULT[ast.na.results]<-astdata$ast[ast.na.results]
ast.new<-astdata[,c(1,4)]
colnames(ast.new)[2]<- "AST"

alt<- subset(labdata, TEST_ORD_NAME=="ALT (SGPT)", select =c("SUBJID","RESULT"))
alt$RESULT<- as.numeric(alt$RESULT)
altowl<-subset(read.csv(paste0(DIR_FILES,"nimble_owl_clinds_7apr21.csv")), select=c(current_label, alt))
altowl$SUBJID<-do.call("rbind",strsplit(as.character(altowl$current_label), "L"))[,1]
altowl$SUBJID<-do.call("rbind",strsplit(altowl$SUBJID, "N"))[,2]

altdata<- merge(altowl, alt, by="SUBJID", all=T)
alt.na.results<-which(is.na(altdata$RESULT) == T)
altdata$RESULT[alt.na.results]<-altdata$alt[alt.na.results]
alt.new<-altdata[,c(1,4)]
colnames(alt.new)[2]<- "ALT"

alkphos<- subset(labdata, TEST_ORD_NAME=="Alkaline Phosphatase", select =c("SUBJID","RESULT"))
alkphos$RESULT<- as.numeric(alkphos$RESULT)
colnames(alkphos)[2]<- "alkphos"

bil.tot<- subset(labdata, TEST_ORD_NAME=="Bilirubin, Total", select =c("SUBJID","RESULT"))
bil.tot$RESULT[which(bil.tot$RESULT=="<0.2")]<-(0.2)/2
bil.tot$RESULT<- as.numeric(bil.tot$RESULT)
colnames(bil.tot)[2]<- "Bilirubin.Total"

albu<- subset(labdata, TEST_ORD_NAME=="Albumin", select =c("SUBJID","RESULT"))
albu$RESULT<- as.numeric(albu$RESULT)
colnames(albu)[2]<- "Albumin"

lab_list<-list(ast.new,alt.new,alkphos,bil.tot,albu)

lab <- Reduce(
  function(x, y, ...) merge(x, y,all = TRUE, ...), 
  lab_list
)

colnames(lab)[1]<- "subject"

summary_data.lab<-lab

## create the data and arrange

summary_data<- merge(summary_data.md,summary_data.lab, by="subject", all=TRUE)
mysum<- function(x)
{
  out.mean<- round(mean(x,na.rm=T),2)
  out.sd<-round(sd(x, na.rm=T),2)
  out<- paste(out.mean, "(", out.sd, ")")
  
}

## generate summary


with(summary_data, table(stage.fib))

with(summary_data,aggregate(age, by=list(stage.fib), FUN=mysum)) 

tab.gen<-with(summary_data, table(gender,stage.fib))
tab.gen
round(prop.table(tab.gen,2)*100,2)

tab.race<-with(summary_data, table(race,stage.fib))
tab.race
round(prop.table(tab.race,2)*100,2)

with(summary_data,aggregate(bmi, by=list(stage.fib), FUN=mysum)) 

with(summary_data,aggregate(waist, by=list(stage.fib), FUN=mysum)) 


tab.dm2<-with(summary_data, table(dm2,stage.fib))
tab.dm2
round(prop.table(tab.dm2,2)*100,2)


tab.htn<-with(summary_data, table(htn,stage.fib))
tab.htn
round(prop.table(tab.htn,2)*100,2)

with(summary_data,aggregate(AST, by=list(stage.fib), FUN=mysum)) 
with(summary_data,aggregate(ALT, by=list(stage.fib), FUN=mysum)) 
with(summary_data,aggregate(alkphos, by=list(stage.fib), FUN=mysum)) 
with(summary_data,aggregate(Bilirubin.Total, by=list(stage.fib), FUN=mysum)) 
with(summary_data,aggregate(inr, by=list(stage.fib), FUN=mysum)) 
with(summary_data,aggregate(Albumin, by=list(stage.fib), FUN=mysum)) 


with(summary_data,aggregate(Hb, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(wbc, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(platelet, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(glucose, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(insulin, by=list(stage.fib), FUN=mysum))


with(summary_data,aggregate(cholesterol_t, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(cholesterol_ldl, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(cholesterol_hdl, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(triglyc, by=list(stage.fib), FUN=mysum))



tab.nafl<-with(summary_data, table(NAFL,stage.fib))
tab.nafl
round(prop.table(tab.nafl,2)*100,2)


tab.nash<-with(summary_data, table(NASH,stage.fib))
tab.nash
round(prop.table(tab.nash,2)*100,2)



with(summary_data,aggregate(steatosis, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(balloon, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(lobular, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(portinflam, by=list(stage.fib), FUN=mysum))
with(summary_data,aggregate(nas, by=list(stage.fib), FUN=mysum))


with(summary_data, table(nashdx99,stage.fib))

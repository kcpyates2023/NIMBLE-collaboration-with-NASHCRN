### Create function for tables
#@@input are a FIT subjects which contains
# 1: the Biomarker data with name
# 2: the Endpoint data with name
# 3: AUCROC at the optimal cut-point
# 4: Youden_Index at the optimal cut-point
# 5: PredictionStat at the optimal cut-point, including TP, FP, TN, FN
# 6: Optimal cutpoint
# 7: Optimal cutpoint direction, i.e. >= or <=
# 8: Pvalue
# 9: Biomarker name
# 10: Endpoint name
# 11: endpoint_pos_var  
# 12: endpoint_neg_var  
#@@output a table summrizes the inportant info


### note we have lots of "**" to create pattern where later we can replace them with blankspace to create empty rows in the word documents


Table_Fun<-function(FIT){
 
  Biomarker<-FIT[["Biomarker"]]
  Endpoint<-FIT[["Endpoint"]]
  AUCROC<-FIT[["AUCROC"]]
  Youden_Index<- FIT[["Youden_Index"]]
  Optimal_cutpoint<-format(FIT[["Optimal_cutpoint"]] ,  nsmall = 1) 
  Optimal_cutpoint_direction<-FIT[["Optimal_cutpoint_direction"]] 
  PredictionStat<-FIT[["PredictionStat"]]
  Pvalue<-FIT[["Pvalue"]]
  Biomarker_name<- FIT[["Biomarker_name"]] 
  Endpoint_name<- FIT[["Endpoint_name"]] 
  Endpoint_pos_var<-FIT[["Endpoint_pos_var"]] 
  Endpoint_neg_var<-FIT[["Endpoint_neg_var"]]
  ci_format<- FIT[["CI"]]
  
  if(Pvalue<0.001){
    Pvalue<-"<0.001"
  }else{
    Pvalue=format(round(Pvalue,3), nsmall=3)
  }

  
  # TPR, FPR, PPV, NPV
  TP<-PredictionStat$tp
  TN<-PredictionStat$tn
  FP<-PredictionStat$fp
  FN<-PredictionStat$fn
  
  TPR<-round(TP*100/sum(TP+FN),1)# sensitivity
  FPR<-round(FP*100/sum(FP+TN),1)#1-specificity
  PPV<-round(TP*100/sum(TP+FP),1)
  NPV<-round(TN*100/sum(FN+TN),1)
  
  Tab_Total<-list()
  #Tab_Total[["        "]]<-as.character( paste0("Histological (biopsy) classification on", Endpoint_name))
  Tab_Total[[Biomarker_name]]<-as.character("Total")
  Tab_Total[["------------"]]<-"------------"
  Tab_Total[["n"]]<-as.character(length(Endpoint))
  Tab_Total[["Mean (SD)"]]<- paste0(format( round(mean(Biomarker,na.rm = TRUE),1), nsmall = 1)," (", format( round(sd(Biomarker,na.rm = TRUE),1), nsmall = 1),")" )
  Tab_Total[["Median"]]<-as.character(format( round(median(Biomarker,na.rm = TRUE),1), nsmall = 1) )
  Tab_Total[["Min, Max"]]<-paste0(format( round(min(Biomarker,na.rm = TRUE),1), nsmall = 1),", ", 
                                  format( round(max(Biomarker,na.rm = TRUE),1), nsmall = 1) )
  Tab_Total[["**"]]<-"**"
  Tab_Total[["Area Under the ROC Curve"]]<-format( round(AUCROC,3), nsmall = 3)
  Tab_Total[["95% CI of Area Under the ROC Curve"]]<-ci_format
  Tab_Total[["P-value [2]"]]<- as.character(Pvalue)# format( round(Pvalue,3) , nsmall = 3) 
  Tab_Total[["****"]]<-"****"
  Tab_Total[["Optimal cut-point [3]"]]<-paste0(Optimal_cutpoint_direction,Optimal_cutpoint )
  Tab_Total[["******"]]<-"******"
  Tab_Total[["Candidate Biomarker classification [1] at the optimal cut-point"]]<-"  "
  Tab_Total[["      **Positive"]]<-as.character(TP+FP )
  Tab_Total[["      **Negative"]]<-as.character(TN+FN)
  Tab_Total[["      **Total"]]<-as.character( TP+FP + TN+FN)
  Tab_Total[["********"]]<-"********"
  Tab_Total[["Performance Parameters at the optimal cut-point"]]<-"  "
  Tab_Total[["      **Sensitivity (%)"]]<-format(TPR , nsmall = 1) 
  Tab_Total[["      **Specificity (%)"]]<-format((100-FPR), nsmall = 1) 
  Tab_Total[["      **Youden Index"]]<-format( round(Youden_Index,3) , nsmall = 3) 
  Tab_Total[["      **Positive Predictive Value (%)"]]<-format(PPV, nsmall = 1) 
  Tab_Total[["      **Negative Predictive Value (%)"]]<-format(NPV, nsmall = 1) 
 
  
  Tab_Positive<-list()
  #Tab_Positive[["        "]]<-as.character( paste0("Histological (biopsy) classification on", Endpoint_name))
  Tab_Positive[[Biomarker_name]]<-as.character("Positive")
  Tab_Positive[["------------"]]<-"------------"
  Tab_Positive[["n"]]<-as.character(sum(Endpoint==1))
  Tab_Positive[["Mean (SD)"]]<- paste0(format( round(mean(Biomarker[Endpoint==1],na.rm = TRUE),1), nsmall = 1)," (", 
                                       format( round(sd(Biomarker[Endpoint==1],na.rm = TRUE),1), nsmall = 1),")" )
  Tab_Positive[["Median"]]<-as.character(format( round(median(Biomarker[Endpoint==1],na.rm = TRUE),1), nsmall = 1) )
  Tab_Positive[["Min, Max"]]<-paste0(format( round(min(Biomarker[Endpoint==1],na.rm = TRUE),1), nsmall = 1),", ", 
                                     format( round(max(Biomarker[Endpoint==1],na.rm = TRUE),1), nsmall = 1) )
  Tab_Positive[["**"]]<-"**"
  Tab_Positive[["Area Under the ROC Curve"]]<-"  "
  Tab_Positive[["95% CI of Area Under the ROC Curve"]]<-"  "
  Tab_Positive[["P-value [2]"]]<-"  "
  Tab_Positive[["****"]]<-"****"
  Tab_Positive[["Optimal cut-point [3]"]]<-"  "
  Tab_Positive[["******"]]<-"******"
  Tab_Positive[["Candidate Biomarker classification [1] at the optimal cut-point"]]<-"  "
  Tab_Positive[["      **Positive"]]<-as.character(TP)
  Tab_Positive[["      **Negative"]]<-as.character(FN)
  Tab_Positive[["      **Total"]]<-as.character(TP+FN)
  Tab_Positive[["********"]]<-"********"
  Tab_Positive[["Performance Parameters at the optimal cut-point"]]<-"  "
  Tab_Positive[["      **Sensitivity (%)"]]<-"  "
  Tab_Positive[["      **Specificity (%)"]]<-"  "
  Tab_Positive[["      **Youden Index"]]<-"  "
  Tab_Positive[["      **Positive Predictive Value (%)"]]<-"  "
  Tab_Positive[["      **Negative Predictive Value (%)"]]<-"  "
  
  
  Tab_Negative<-list()
  #Tab_Negative[["        "]]<-as.character( paste0("Histological (biopsy) classification on", Endpoint_name ))
  Tab_Negative[[Biomarker_name]]<-as.character("Negative")
  Tab_Negative[["------------"]]<-"------------"
  Tab_Negative[["n"]]<-as.character(sum(Endpoint==0))
  Tab_Negative[["Mean (SD)"]]<- paste0(format( round(mean(Biomarker[Endpoint==0],na.rm = TRUE),1), nsmall = 1)," (", 
                                       format( round(sd(Biomarker[Endpoint==0],na.rm = TRUE),1), nsmall = 1),")" )
  Tab_Negative[["Median"]]<-as.character(format( round(median(Biomarker[Endpoint==0],na.rm = TRUE),1), nsmall = 1) )
  Tab_Negative[["Min, Max"]]<-paste0(format( round(min(Biomarker[Endpoint==0],na.rm = TRUE),1), nsmall = 1),", ", 
                                     format( round(max(Biomarker[Endpoint==0],na.rm = TRUE),1), nsmall = 1) )
  Tab_Negative[["**"]]<-"**"
  Tab_Negative[["Area Under the ROC Curve"]]<-"  "
  Tab_Negative[["95% CI of Area Under the ROC Curve"]]<-"  "
  Tab_Negative[["P-value [2]"]]<-"  "
  Tab_Negative[["****"]]<-"****"
  Tab_Negative[["Optimal cut-point [3]"]]<-"  "
  Tab_Negative[["******"]]<-"******"
  Tab_Negative[["Candidate Biomarker classification [1] at the optimal cut-point"]]<-"  "
  Tab_Negative[["      **Positive"]]<-as.character(FP)
  Tab_Negative[["      **Negative"]]<-as.character(TN)
  Tab_Negative[["      **Total"]]<-as.character(FP+TN)
  Tab_Negative[["********"]]<-"********"
  Tab_Negative[["Performance Parameters at the optimal cut-point"]]<-"  "
  Tab_Negative[["      **Sensitivity (%)"]]<-"  "
  Tab_Negative[["      **Specificity (%)"]]<-"  "
  Tab_Negative[["      **Youden Index"]]<-"  "
  Tab_Negative[["      **Positive Predictive Value (%)"]]<-"  "
  Tab_Negative[["      **Negative Predictive Value (%)"]]<-"  "
  
  Table<-data.frame(#Grittiness=names(Tab),
    Positive =unlist(Tab_Positive),
    Negative=unlist(Tab_Negative),
    TOtal=unlist(Tab_Total))
  
  colnames( Table)<-c("**",
                      "Histological (biopsy) classification [1]",
                      "****")
  
  # colnames( Table)<-  c("Histological (biopsy) classification:",
  #                      paste0("Positive = ",Endpoint_pos_var,";"),
  #                     paste0("Negative = ",Endpoint_neg_var))
  
  return(Table)
}


# Tab_Positive[["        "]]<-"  " -> Tab_Positive[["------------"]]<-"------------"
# Tab_Total[["        "]]<-"  "  -> Tab_Total[["------------"]]<-"------------"
# Tab_Negative[["        "]]<-"  " ->  Tab_Negative[["------------"]]<-"------------"
### Create function Table_Fun_Comparison for tables presents the biomarker vs candiate variable in terms of endpoint diagnose
#@@input are a FIT_Biomarker and FIT_Candidate, one for comp_var(various biomarkers), 1 for candiate_var(ATS and FIB-4)  subjects which contains, each FIT objects contains following
# 1: the Biomarker data 
# 2: the Endpoint data  
# 3: AUCROC at the optimal cut-point
# 4: Youden_Index at the optimal cut-point
# 5: PredictionStat at the optimal cut-point, including TP, FP, TN, FN
# 6: Optimal cutpoint
# 7: Optimal cutpoint direction, i.e. >= or <=
# 8: Pvalue # this is P(AUCROC>0.5)
# 9: Biomarker name
# 10: Endpoint name
# 11: endpoint_pos_var  
# 12: endpoint_neg_var 
#@@out a table summrizes important info 

### note we have lots of "**" to create pattern where later we can replace them with blank-space to create empty rows in the word documents

Table_Fun_Comparison<-function(FIT_Biomarker=FIT_Biomarker, FIT_Candidate=FIT_Candidate){
 
  #########################################################
  ###Extract info from FIT objects 
  #########################################################  
  Biomarker_Biomarker<-FIT_Biomarker[["Biomarker"]]
  Endpoint_Biomarker<-FIT_Biomarker[["Endpoint"]]
  AUCROC_Biomarker<-FIT_Biomarker[["AUCROC"]] 
  Youden_Index_Biomarker<-FIT_Biomarker[["Youden_Index"]]
  Optimal_cutpoint_Biomarker<-format( FIT_Biomarker[["Optimal_cutpoint"]], nsmall = 1) 
  Optimal_cutpoint_direction_Biomarker<-FIT_Biomarker[["Optimal_cutpoint_direction"]] 
  PredictionStat_Biomarker<-FIT_Biomarker[["PredictionStat"]]
  Pvalue_Biomarker<-FIT_Biomarker[["Pvalue"]]
  Biomarker_name_Biomarker<- FIT_Biomarker[["Biomarker_name"]] 
  Endpoint_name_Biomarker<- FIT_Biomarker[["Endpoint_name"]] 
  Endpoint_pos_var_Biomarker<-FIT_Biomarker[["Endpoint_pos_var"]] 
  Endpoint_neg_var_Biomarker<-FIT_Biomarker[["Endpoint_neg_var"]] 
  AUCROC_Boot_Biomarker<-FIT_Biomarker[["AUCROC_Boot"]]
  SE_AUC_Biomarker<-FIT_Biomarker[["SE_AUCROC"]]
  ci_format_Biomarker<- FIT_Biomarker[["CI"]]
  
  
  Biomarker_Candidate<-FIT_Candidate[["Biomarker"]]
  Endpoint_Candidate<-FIT_Candidate[["Endpoint"]]
  AUCROC_Candidate<-FIT_Candidate[["AUCROC"]] 
  Youden_Index_Candidate<-FIT_Candidate[["Youden_Index"]]
  Optimal_cutpoint_Candidate<-format( FIT_Candidate[["Optimal_cutpoint"]], nsmall = 1) 
  Optimal_cutpoint_direction_Candidate<-FIT_Candidate[["Optimal_cutpoint_direction"]] 
  PredictionStat_Candidate<-FIT_Candidate[["PredictionStat"]]
  Pvalue_Candidate<-FIT_Candidate[["Pvalue"]]
  Biomarker_name_Candidate<- FIT_Candidate[["Biomarker_name"]] 
  Endpoint_name_Candidate<- FIT_Candidate[["Endpoint_name"]] 
  Endpoint_pos_var_Candidate<-FIT_Candidate[["Endpoint_pos_var"]] 
  Endpoint_neg_var_Candidate<-FIT_Candidate[["Endpoint_neg_var"]] 
  AUCROC_Boot_Candidate<-FIT_Candidate[["AUCROC_Boot"]]
  SE_AUC_Candidate<-FIT_Candidate[["SE_AUCROC"]]
  ci_format_Candidate<- FIT_Candidate[["CI"]]
  
  #########################################################
  ### Compute the TPR, FPR, PPV, NPV 
  #########################################################  
  TP_Biomarker<-PredictionStat_Biomarker$tp
  TN_Biomarker<-PredictionStat_Biomarker$tn
  FP_Biomarker<-PredictionStat_Biomarker$fp
  FN_Biomarker<-PredictionStat_Biomarker$fn
  TPR_Biomarker<-round(TP_Biomarker*100/sum(TP_Biomarker+FN_Biomarker),1) # sensitivity
  FPR_Biomarker<-round(FP_Biomarker*100/sum(FP_Biomarker+TN_Biomarker),1) #1-specificity
  PPV_Biomarker<-round(TP_Biomarker*100/sum(TP_Biomarker+FP_Biomarker),1) 
  NPV_Biomarker<-round(TN_Biomarker*100/sum(FN_Biomarker+TN_Biomarker),1)
  
  TP_Candidate<-PredictionStat_Candidate$tp
  TN_Candidate<-PredictionStat_Candidate$tn
  FP_Candidate<-PredictionStat_Candidate$fp
  FN_Candidate<-PredictionStat_Candidate$fn
  TPR_Candidate<-round(TP_Candidate*100/sum(TP_Candidate+FN_Candidate),1) # sensitivity
  FPR_Candidate<-round(FP_Candidate*100/sum(FP_Candidate+TN_Candidate),1) #1-specificity
  PPV_Candidate<-round(TP_Candidate*100/sum(TP_Candidate+FP_Candidate),1) 
  NPV_Candidate<-round(TN_Candidate*100/sum(FN_Candidate+TN_Candidate),1)
  
  
  #########################################################
  ### Compute the p value assuming the correlation is 0
  #########################################################  
  #note that direction in the roc function is oppsite to the optimal cutoff direction
  #"<": if the predictor values for the control group are lower or equal than the values of the case group (controls < t <= cases)
  if(substr(Optimal_cutpoint_direction_Biomarker,1,1) == ">"){
    Dummy_Biomarker<-if_else(Biomarker_Biomarker>= as.numeric(Optimal_cutpoint_Biomarker), 1,0)
    ROC_Biomarker<-roc(Endpoint_Biomarker,Biomarker_Biomarker, direction="<")
  }else{
    Dummy_Biomarker<-if_else(Biomarker_Biomarker<= as.numeric(Optimal_cutpoint_Biomarker), 1,0)
    ROC_Biomarker<-roc(Endpoint_Biomarker,Biomarker_Biomarker, direction=">")}
  
  
  
  if(substr(Optimal_cutpoint_direction_Candidate,1,1) == ">"){
    Dummy_Candidate<-if_else(Biomarker_Candidate>= as.numeric(Optimal_cutpoint_Candidate), 1,0)
    ROC_Candidate<-roc(Endpoint_Candidate,Biomarker_Candidate, direction="<")
  }else{
    Dummy_Candidate<-if_else(Biomarker_Candidate<= as.numeric(Optimal_cutpoint_Candidate), 1,0)
    ROC_Candidate<-roc(Endpoint_Candidate,Biomarker_Candidate, direction=">")}
  
  
  Pvalue_Comparison<-roc.test(ROC_Biomarker, ROC_Candidate, method="delong",alternative = "greater")$p.value
 
  if(Pvalue_Comparison<0.001){
    Pvalue_Comparison<-"<0.001"
  }else{
    Pvalue_Comparison=format(round(Pvalue_Comparison,3), nsmall=3)
  }
  #########################################################
  ###construct the tables for biomarker 
  #########################################################  
  Tab_Biomarker_Total<-list()
  Tab_Biomarker_Total[["  **"]]<-as.character("Total")
  Tab_Biomarker_Total[["---------"]]<-"---------"
  Tab_Biomarker_Total[["n"]]<-as.character(length(Endpoint_Biomarker))
  Tab_Biomarker_Total[["Mean (SD)"]]<- paste0(round(mean(Biomarker_Biomarker,na.rm = TRUE),1)," (", round(sd(Biomarker_Biomarker,na.rm = TRUE),1),")" )
  Tab_Biomarker_Total[["Median"]]<-format( round(median(Biomarker_Biomarker,na.rm = TRUE),1),nsmall=1) 
  Tab_Biomarker_Total[["Min, Max"]]<-paste0(format( round(min(Biomarker_Biomarker,na.rm = TRUE),1),nsmall=1),", ", 
                                            format( round(max(Biomarker_Biomarker,na.rm = TRUE),1),nsmall=1) )
  Tab_Biomarker_Total[["**"]]<-"**"
  Tab_Biomarker_Total[["Area Under the ROC Curve"]]<-format( round(AUCROC_Biomarker,3) ,nsmall=3)
  Tab_Biomarker_Total[["95% CI of Area Under the ROC Curve"]]<-ci_format_Biomarker
  Tab_Biomarker_Total[["P-value [2]"]]<-"  "
  Tab_Biomarker_Total[["****"]]<-"****"
  Tab_Biomarker_Total[["Optimal cut-point [3]"]]<-paste0(Optimal_cutpoint_direction_Biomarker,Optimal_cutpoint_Biomarker )
  Tab_Biomarker_Total[["******"]]<-"******"
  Tab_Biomarker_Total[["Candidate Biomarker classification [1] at the optimal cut-point"]]<-"  "
  Tab_Biomarker_Total[["      **Positive"]]<-as.character(TP_Biomarker+FP_Biomarker )
  Tab_Biomarker_Total[["      **Negative"]]<-as.character(TN_Biomarker+FN_Biomarker)
  Tab_Biomarker_Total[["      **Total"]]<-as.character( TP_Biomarker+FP_Biomarker + TN_Biomarker+FN_Biomarker)
  Tab_Biomarker_Total[["********"]]<-"********"
  Tab_Biomarker_Total[["Performance Parameters at the optimal cut-point"]]<-"  "
  Tab_Biomarker_Total[["      **Sensitivity (%)"]]<-format( TPR_Biomarker , nsmall=1)
  Tab_Biomarker_Total[["      **Specificity (%)"]]<-format( 100-FPR_Biomarker, nsmall=1)
  Tab_Biomarker_Total[["      **Youden Index"]]<-format( round(Youden_Index_Biomarker,3) , nsmall=3)
  Tab_Biomarker_Total[["      **Positive Predictive Value (%)"]]<-format( PPV_Biomarker, nsmall=1)
  Tab_Biomarker_Total[["      **Negative Predictive Value (%)"]]<-format( NPV_Biomarker, nsmall=1)
 
  
  Tab_Biomarker_Positive<-list()
  Tab_Biomarker_Positive[["  **"]]<-as.character("Positive")
  Tab_Biomarker_Positive[["---------"]]<-"---------"
  Tab_Biomarker_Positive[["n"]]<-as.character(sum(Endpoint_Biomarker==1))
  Tab_Biomarker_Positive[["Mean (SD)"]]<- paste0(format(round(mean(Biomarker_Biomarker[Endpoint_Biomarker==1],na.rm = TRUE),1),nsmall=1)," (", 
                                                 format(round(sd(Biomarker_Biomarker[Endpoint_Biomarker==1],na.rm = TRUE),1),nsmall=1),")" )
  Tab_Biomarker_Positive[["Median"]]<-format(round(median(Biomarker_Biomarker[Endpoint_Biomarker==1],na.rm = TRUE),1) ,nsmall=1)
  Tab_Biomarker_Positive[["Min, Max"]]<-paste0(format(round(min(Biomarker_Biomarker[Endpoint_Biomarker==1],na.rm = TRUE),1),nsmall=1),", ", 
                                               format(round(max(Biomarker_Biomarker[Endpoint_Biomarker==1],na.rm = TRUE),1),nsmall=1) )
  Tab_Biomarker_Positive[["**"]]<-"**"
  Tab_Biomarker_Positive[["Area Under the ROC Curve"]]<-"  "
  Tab_Biomarker_Positive[["95% CI of Area Under the ROC Curve"]]<-"  "
  Tab_Biomarker_Positive[["P-value [2]"]]<-"  "
  Tab_Biomarker_Positive[["****"]]<-"****"
  Tab_Biomarker_Positive[["Optimal cut-point [3]"]]<-"  "
  Tab_Biomarker_Positive[["******"]]<-"******"
  Tab_Biomarker_Positive[["Candidate Biomarker classification [1] at the optimal cut-point"]]<-"  "
  Tab_Biomarker_Positive[["      **Positive"]]<-as.character(TP_Biomarker)
  Tab_Biomarker_Positive[["      **Negative"]]<-as.character(FN_Biomarker)
  Tab_Biomarker_Positive[["      **Total"]]<-as.character(TP_Biomarker+FN_Biomarker)
  Tab_Biomarker_Positive[["********"]]<-"********"
  Tab_Biomarker_Positive[["Performance Parameters at the optimal cut-point"]]<-"  "
  Tab_Biomarker_Positive[["      **Sensitivity (%)"]]<-"  "
  Tab_Biomarker_Positive[["      **Specificity (%)"]]<-"  "
  Tab_Biomarker_Positive[["      **Youden Index"]]<-"  "
  Tab_Biomarker_Positive[["      **Positive Predictive Value (%)"]]<-"  "
  Tab_Biomarker_Positive[["      **Negative Predictive Value (%)"]]<-"  "
  
  
  Tab_Biomarker_Negative<-list()
  Tab_Biomarker_Negative[["  **"]]<-as.character("Negative")
  Tab_Biomarker_Negative[["---------"]]<- "---------"
  Tab_Biomarker_Negative[["n"]]<-as.character(sum(Endpoint_Biomarker==0))
  Tab_Biomarker_Negative[["Mean (SD)"]]<- paste0(format(round(mean(Biomarker_Biomarker[Endpoint_Biomarker==0],na.rm = TRUE),1),nsmall=1)," (", 
                                                 format(round(sd(Biomarker_Biomarker[Endpoint_Biomarker==0],na.rm = TRUE),1),nsmall=1),")" )
  Tab_Biomarker_Negative[["Median"]]<-format(round(median(Biomarker_Biomarker[Endpoint_Biomarker==0],na.rm = TRUE),1),nsmall=1 )
  Tab_Biomarker_Negative[["Min, Max"]]<-paste0(format(round(min(Biomarker_Biomarker[Endpoint_Biomarker==0],na.rm = TRUE),1),nsmall=1),", ",
                                               format(round(max(Biomarker_Biomarker[Endpoint_Biomarker==0],na.rm = TRUE),1),nsmall=1) )
  Tab_Biomarker_Negative[["**"]]<-"**"
  Tab_Biomarker_Negative[["Area Under the ROC Curve"]]<-"  "
  Tab_Biomarker_Negative[["95% CI of Area Under the ROC Curve"]]<-"  "
  Tab_Biomarker_Negative[["P-value [2]"]]<-"  "
  Tab_Biomarker_Negative[["****"]]<-"****"
  Tab_Biomarker_Negative[["Optimal cut-point [3]"]]<-"  "
  Tab_Biomarker_Negative[["******"]]<-"******"
  Tab_Biomarker_Negative[["Candidate Biomarker classification [1] at the optimal cut-point"]]<-"  "
  Tab_Biomarker_Negative[["      **Positive"]]<-as.character(FP_Biomarker)
  Tab_Biomarker_Negative[["      **Negative"]]<-as.character(TN_Biomarker)
  Tab_Biomarker_Negative[["      **Total"]]<-as.character(FP_Biomarker+TN_Biomarker)
  Tab_Biomarker_Negative[["********"]]<-"********"
  Tab_Biomarker_Negative[["Performance Parameters at the optimal cut-point"]]<-"  "
  Tab_Biomarker_Negative[["      **Sensitivity (%)"]]<-"  "
  Tab_Biomarker_Negative[["      **Specificity (%)"]]<-"  "
  Tab_Biomarker_Negative[["      **Youden Index"]]<-"  "
  Tab_Biomarker_Negative[["      **Positive Predictive Value (%)"]]<-"  "
  Tab_Biomarker_Negative[["      **Negative Predictive Value (%)"]]<-"  "
  
  
  #########################################################
  ###construct the tables for Candidate marker
  #########################################################  
  Tab_Candidate_Total<-list()
  Tab_Candidate_Total[["  **"]]<-as.character("Total")
  Tab_Candidate_Total[["---------"]]<-"---------"
  Tab_Candidate_Total[["n"]]<-as.character(length(Endpoint_Candidate))
  Tab_Candidate_Total[["Mean (SD)"]]<- paste0(format(round(mean(Biomarker_Candidate,na.rm = TRUE),1),nsmall=1)," (", 
                                              format(round(sd(Biomarker_Candidate,na.rm = TRUE),1),nsmall=1),")" )
  Tab_Candidate_Total[["Median"]]<-format( round(median(Biomarker_Candidate,na.rm = TRUE),1),nsmall=1 )
  Tab_Candidate_Total[["Min, Max"]]<-paste0(format(round(min(Biomarker_Candidate,na.rm = TRUE),1),nsmall=1),", ", 
                                            format(round(max(Biomarker_Candidate,na.rm = TRUE),1),nsmall=1) )
  Tab_Candidate_Total[["**"]]<-"**"
  Tab_Candidate_Total[["Area Under the ROC Curve"]]<-format(round(AUCROC_Candidate,3),nsmall=3 )
  Tab_Candidate_Total[["95% CI of Area Under the ROC Curve"]]<-ci_format_Candidate
  Tab_Candidate_Total[["P-value [2]"]]<-Pvalue_Comparison
  Tab_Candidate_Total[["****"]]<-"****"
  Tab_Candidate_Total[["Optimal cut-point [3]"]]<-paste0(Optimal_cutpoint_direction_Candidate,Optimal_cutpoint_Candidate )
  Tab_Candidate_Total[["******"]]<-"******"
  Tab_Candidate_Total[["Candidate Biomarker classification [1] at the optimal cut-point"]]<-"  "
  Tab_Candidate_Total[["      **Positive"]]<-as.character(TP_Candidate+FP_Candidate )
  Tab_Candidate_Total[["      **Negative"]]<-as.character(TN_Candidate+FN_Candidate)
  Tab_Candidate_Total[["      **Total"]]<-as.character( TP_Candidate+FP_Candidate + TN_Candidate+FN_Candidate)
  Tab_Candidate_Total[["********"]]<-"********"
  Tab_Candidate_Total[["Performance Parameters at the optimal cut-point"]]<-"  "
  Tab_Candidate_Total[["      **Sensitivity (%)"]]<-format( TPR_Candidate,nsmall=1)
  Tab_Candidate_Total[["      **Specificity (%)"]]<-format( (100-FPR_Candidate),nsmall=1)
  Tab_Candidate_Total[["      **Youden Index"]]<- format(round(Youden_Index_Candidate,3) ,nsmall=3)
  Tab_Candidate_Total[["      **Positive Predictive Value (%)"]]<-format( PPV_Candidate,nsmall=1)
  Tab_Candidate_Total[["      **Negative Predictive Value (%)"]]<-format( NPV_Candidate,nsmall=1)
  
  
  Tab_Candidate_Positive<-list()
  Tab_Candidate_Positive[["  **"]]<-as.character("Positive")
  Tab_Candidate_Positive[["---------"]]<-"---------"
  Tab_Candidate_Positive[["n"]]<-as.character(sum(Endpoint_Candidate==1))
  Tab_Candidate_Positive[["Mean (SD)"]]<- paste0(format( round(mean(Biomarker_Candidate[Endpoint_Candidate==1],na.rm = TRUE),1),nsmall=1)," (", 
                                                 format( round(sd(Biomarker_Candidate[Endpoint_Candidate==1],na.rm = TRUE),1),nsmall=1),")" )
  Tab_Candidate_Positive[["Median"]]<-format( round(median(Biomarker_Candidate[Endpoint_Candidate==1],na.rm = TRUE),1),nsmall=1 )
  Tab_Candidate_Positive[["Min, Max"]]<-paste0(format( round(min(Biomarker_Candidate[Endpoint_Candidate==1],na.rm = TRUE),1),nsmall=1),", ", 
                                               format( round(max(Biomarker_Candidate[Endpoint_Candidate==1],na.rm = TRUE),1),nsmall=1) )
  Tab_Candidate_Positive[["**"]]<-"**"
  Tab_Candidate_Positive[["Area Under the ROC Curve"]]<-"  "
  Tab_Candidate_Positive[["95% CI of Area Under the ROC Curve"]]<-"  "
  Tab_Candidate_Positive[["P-value [2]"]]<-"  "
  Tab_Candidate_Positive[["****"]]<-"****"
  Tab_Candidate_Positive[["Optimal cut-point [3]"]]<-"  "
  Tab_Candidate_Positive[["******"]]<-"******"
  Tab_Candidate_Positive[["Candidate Biomarker classification [1] at the optimal cut-point"]]<-"  "
  Tab_Candidate_Positive[["      **Positive"]]<-as.character(TP_Candidate)
  Tab_Candidate_Positive[["      **Negative"]]<-as.character(FN_Candidate)
  Tab_Candidate_Positive[["      **Total"]]<-as.character(TP_Candidate+FN_Candidate)
  Tab_Candidate_Positive[["********"]]<-"********"
  Tab_Candidate_Positive[["Performance Parameters at the optimal cut-point"]]<-"  "
  Tab_Candidate_Positive[["      **Sensitivity (%)"]]<-"  "
  Tab_Candidate_Positive[["      **Specificity (%)"]]<-"  "
  Tab_Candidate_Positive[["      **Youden Index"]]<-"  "
  Tab_Candidate_Positive[["      **Positive Predictive Value (%)"]]<-"  "
  Tab_Candidate_Positive[["      **Negative Predictive Value (%)"]]<-"  "
  
  
  Tab_Candidate_Negative<-list()
  Tab_Candidate_Negative[["  **"]]<-as.character("Negative")
  Tab_Candidate_Negative[["---------"]]<-"---------"
  Tab_Candidate_Negative[["n"]]<-as.character(sum(Endpoint_Candidate==0))
  Tab_Candidate_Negative[["Mean (SD)"]]<- paste0(format( round(mean(Biomarker_Candidate[Endpoint_Candidate==0],na.rm = TRUE),1),nsmall=1)," (", 
                                                 format( round(sd(Biomarker_Candidate[Endpoint_Candidate==0],na.rm = TRUE),1),nsmall=1),")" )
  Tab_Candidate_Negative[["Median"]]<-format( round(median(Biomarker_Candidate[Endpoint_Candidate==0],na.rm = TRUE),1),nsmall=1 )
  Tab_Candidate_Negative[["Min, Max"]]<-paste0(format( round(min(Biomarker_Candidate[Endpoint_Candidate==0],na.rm = TRUE),1),nsmall=1),", ",
                                               format( round(max(Biomarker_Candidate[Endpoint_Candidate==0],na.rm = TRUE),1),nsmall=1) )
  Tab_Candidate_Negative[["**"]]<-"**"
  Tab_Candidate_Negative[["Area Under the ROC Curve"]]<-"  "
  Tab_Candidate_Negative[["95% CI of Area Under the ROC Curve"]]<-"  "
  Tab_Candidate_Negative[["P-value [2]"]]<-"  "
  Tab_Candidate_Negative[["****"]]<-"****"
  Tab_Candidate_Negative[["Optimal cut-point [3]"]]<-"  "
  Tab_Candidate_Negative[["******"]]<-"******"
  Tab_Candidate_Negative[["Candidate Biomarker classification [1] at the optimal cut-point"]]<-"  "
  Tab_Candidate_Negative[["      **Positive"]]<-as.character(FP_Candidate)
  Tab_Candidate_Negative[["      **Negative"]]<-as.character(TN_Candidate)
  Tab_Candidate_Negative[["      **Total"]]<-as.character(FP_Candidate+TN_Candidate)
  Tab_Candidate_Negative[["********"]]<-"********"
  Tab_Candidate_Negative[["Performance Parameters at the optimal cut-point"]]<-"  "
  Tab_Candidate_Negative[["      **Sensitivity (%)"]]<-"  "
  Tab_Candidate_Negative[["      **Specificity (%)"]]<-"  "
  Tab_Candidate_Negative[["      **Youden Index"]]<-"  "
  Tab_Candidate_Negative[["      **Positive Predictive Value (%)"]]<-"  "
  Tab_Candidate_Negative[["      **Negative Predictive Value (%)"]]<-"  "
  
  
  
  
  #########################################################
  ###construct the entire summary tables  
  #########################################################  
  Table<-data.frame(#Grittiness=names(Tab),
    Positive_Biomarker =unlist(Tab_Biomarker_Positive),
    Negative_Biomarker=unlist(Tab_Biomarker_Negative),
    TOtal_Biomarker=unlist(Tab_Biomarker_Total),
    Positive_Candidate =unlist(Tab_Candidate_Positive),
    Negative_Candidate=unlist(Tab_Candidate_Negative),
    TOtal_Candidate=unlist(Tab_Candidate_Total))
  
  colnames( Table)<-c("**",Biomarker_name_Biomarker, "****",
                      "**",Biomarker_name_Candidate, "****")
  
  return(Table)
}

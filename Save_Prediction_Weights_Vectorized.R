work_CV_Weight<-get_CV_work(4)

Save_weights<-Weight_save


if(Save_weights){
  
  time_pred_Weight<-system.time({
  
    #gg<-1
    Header_progress<-paste0("generating pred weights for district:",District_Now,' ',one_of_dist_str)
    p_progress <- Progress$new(min=0,max=nrow(work_CV_Weight))
    p_progress$set(message =Header_progress ,value=0)
    
    cat("",sep='\n')
    
    cat("generating Model weights ..,",sep='\n')
    
    for (gg in 1:nrow(work_CV_Weight)){
      time_Weight_gen<-system.time({
      cat(paste0(gg," "),sep=',')
      
      week_Sub<-work_CV_Weight$beg_week[gg]:work_CV_Weight$end_week[gg]
      
      CV_data_Wt<-Dat_mod_Selected_with_Inla_groups |> 
        dplyr::mutate(Cases=case_when((year==work_CV_Weight$year[gg] & week %in% week_Sub)~NA,
                                      TRUE~Cases
        ))
      
      last_Dat_Year<-max(CV_data_Wt$year,na.rm=T)
      
      cv_idx<-with(CV_data_Wt,which(year==work_CV_Weight$year[gg] & week%in% week_Sub))
      
      #model_CV<-Sel_Vars(selected_Model_form_rw,"nbinomial",CV_data_Wt,theta_beg_Rw,T)
      
      #summary(model_CV)
      
      Nsamples<-1000
      
       
      
      #post_Samples <- inla.posterior.sample(Nsamples,model_CV)
      
      cv_gg_pref<-str_pad(gg,pad=0,side='left',width=2)
      
      pred_wts_Obj<-file.path(pred_weights_objs_pth,paste0('For_Pred_weights_',last_Dat_Year,'_',cv_gg_pref,'.rds'))
      
      wt_Obs<-readRDS(pred_wts_Obj)
      
      post_Samples <- wt_Obs$post_Samples
      
      #post_Samples <- inla.posterior.sample(n=Nsamples,result=model_CV,parallel.configs=F)
      
      
      post_Samples_eval <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor), post_Samples)
      
      Ypred_NB<-foreach(aa=1:1000,.combine =cbind)%do% rnbinom(nrow(CV_data_Wt), mu = exp(post_Samples_eval[-1,aa]), size = post_Samples_eval[1,aa])
      
      hyper_size_1000_One<-post_Samples[[1]]$hyperpar
      
      idx_size<-which(str_detect(names(hyper_size_1000_One),"size"))
      
      Size_1000<-foreach(aa=1:Nsamples,.combine =cbind)%do%as.numeric(post_Samples[[aa]]$hyperpar[idx_size])
      
      ID_spat_1000_One<-post_Samples[[1]]$latent
      
      
      idx_spat<-which(str_detect(rownames(ID_spat_1000_One),"ID_spat"))
      idx_Week<-which(str_detect(rownames(ID_spat_1000_One),"week"))
      
      idx_Int<-which(str_detect(rownames(ID_spat_1000_One),"Intercept"))
      
      inla_grp_Names1<-str_extract(rownames(ID_spat_1000_One),'Var[:number:]+_Inla_group:[:number:]+')
      inla_grp_Names2<-inla_grp_Names1[complete.cases(inla_grp_Names1)]
      grp_Num<-str_pad(str_remove(str_extract(inla_grp_Names2,':[:number:]+'),':'),width =2,pad=0,side='left')
      idx_Group_all<-which(complete.cases(inla_grp_Names1))
      
      inla_grp_Names<-str_replace(inla_grp_Names2,':[:number:]+',paste0('_',grp_Num))
      
      cat(paste0("InlaGrp_name:"),sep ='\n')
      #cat(inla_grp_Names)
      cat("",sep ='\n')
      
      rownames(ID_spat_1000_One)[idx_Group_all]<-inla_grp_Names
      
      for(ii in 1:length(inla_grp_Names)){
        
        assign(paste0('idx_',inla_grp_Names[ii]),which(str_detect(rownames(ID_spat_1000_One),inla_grp_Names[ii])))
        
      }
      
      rownames(ID_spat_1000_One)[get(paste0('idx_',inla_grp_Names[1]))]
      
      
      ID_spat_1000<-foreach(aa=1:Nsamples,.combine =cbind)%do%as.numeric(post_Samples[[aa]]$latent[idx_spat])
      dim(ID_spat_1000)
      
      Week_district_1000<-foreach(aa=1:Nsamples,.combine =cbind)%do%as.numeric(post_Samples[[aa]]$latent[idx_Week])
      dim(Week_district_1000)
      
      Int_1000<-foreach(aa=1:Nsamples,.combine =cbind)%do%as.numeric(post_Samples[[aa]]$latent[idx_Int])
      
      idx_Now_Grp_all<-foreach(ii=1:length(inla_grp_Names),.combine=c)%do% which(str_detect(rownames(ID_spat_1000_One),inla_grp_Names[ii]))
      
      all_Var_Inla_groupS<-foreach(aa=1:Nsamples,.combine =cbind)%do%as.numeric(post_Samples[[aa]]$latent[idx_Now_Grp_all])
      dim(all_Var_Inla_groupS)
      class(all_Var_Inla_groupS)
      
      
      names(CV_data_Wt)
      Max_IDy_data<-max(CV_data_Wt$ID_year)
      
      Weight_ID_data<-CV_data_Wt |> 
        dplyr::select(ID_spat,week,ID_year) |> 
        dplyr::filter(ID_year==Max_IDy_data) |> 
        unique()
      
      ID_spat_Dist<-unique(CV_data_Wt$ID_spat)
      Years_in_dat<-length(unique(CV_data_Wt$ID_year))
      Num_dist_data<-length(unique(CV_data_Wt$ID_spat))
      last_Dat_Year<-max(CV_data_Wt$year,na.rm=T)
      
      tt<-1
      
      dim(ID_spat_1000)
      
      
      Length_ID_spat<-length(idx_spat)
      
      N_id<-Length_ID_spat/(Num_dist_data*Years_in_dat)
      
      
      ID_spat_1000_DF<-ID_spat_1000
      
      row.names(ID_spat_1000_DF)<-1:Max_IDy_data
      colnames(ID_spat_1000_DF)
      
      ID_spat_1000_DF_long<-reshape2::melt(ID_spat_1000_DF) |> 
        dplyr::mutate(ID_spat=ID_spat_Dist,
                      Sample_N=as.numeric(str_extract(Var2,'[:number:]+'))) |> 
        dplyr::rename(coeff_spat1=value,
                      ID_year=Var1) |> 
        dplyr::select(ID_spat,ID_year,Sample_N,coeff_spat1)
      
      dim(ID_spat_1000)
      
      #Week_district_1000
      
      ID_district_week_DF<-Week_district_1000
      
      dim(ID_district_week_DF)
      
      row.names(ID_district_week_DF)<-1:52
      
      colnames(ID_district_week_DF)
      
      
      ID_district_week_DF_long<-reshape2::melt(ID_district_week_DF) |> 
        dplyr::mutate(ID_spat=ID_spat_Dist,
                      Sample_N=as.numeric(str_extract(Var2,'[:number:]+'))) |> 
        dplyr::rename(Week_effect=value,
                      week=Var1) |> 
        dplyr::select(ID_spat,week,Sample_N,Week_effect)
      
      
      #Coeffs_string<-paste0(inla_grp_Names,"_coeffs=",paste0(inla_grp_Names,'_1000[,tt]'),collapse=',')
      
      rownames(all_Var_Inla_groupS)<-inla_grp_Names
      
      all_Var_Inla_groupS[inla_grp_Names[1],]
      
      Coeffs_string<-paste0(inla_grp_Names,"_coeffs=",paste0("all_Var_Inla_groupS['",inla_grp_Names,"'",',tt]'),collapse=',')
      
      Coeffs_rw<-paste0(inla_grp_Names,"_coeffs",collapse=',')
      
      ##create fixed effect groups effects
      
      
      ID_var_group_coeffs_DF_a<-data.frame(t(all_Var_Inla_groupS),row.names =NULL)
      
      names(ID_var_group_coeffs_DF_a)<-paste0(inla_grp_Names,"_coeffs")
      
      ID_var_group_coeffs_DF<-data.frame(ID_spat=ID_spat_Dist,Var2=row.names(t(all_Var_Inla_groupS)),ID_var_group_coeffs_DF_a) |> 
        dplyr::mutate(Sample_N=as.numeric(str_extract(Var2,'[:number:]+'))) |> 
        dplyr::select(-Var2)
      
      dim(ID_district_week_DF)
      
      row.names(ID_district_week_DF)<-1:52
      
      
      Weight_ID_data_1000<-foreach(aa=1:1000,.combine =rbind)%do% data.frame(Weight_ID_data,Sample_N=aa)
      
      
      unique(Weight_ID_data_1000$Sample_N)
      
      Intercept_DF<-data.frame(Intercept=t(Int_1000),row.names =NULL) |> 
        dplyr::mutate(ID_spat=ID_spat_Dist,
                      Var2=row.names(t(Int_1000))) |> 
        dplyr::mutate(Sample_N=as.numeric(str_extract(Var2,'[:number:]+'))) |> 
        dplyr::select(-Var2)
      
      coeff_vars<-c("week","Sample_N","Intercept","coeff_spat1","Week_effect",paste0(inla_grp_Names,"_coeffs"),"pop_off")
      
      CV_data_coeffs<-Weight_ID_data_1000 |> 
        dplyr::left_join(Intercept_DF,by=c("ID_spat","Sample_N")) |> 
        dplyr::left_join(ID_spat_1000_DF_long,by=c("ID_year","ID_spat","Sample_N")) |> 
        dplyr::left_join(ID_district_week_DF_long,by=c("ID_spat","week","Sample_N")) |> 
        dplyr::left_join(ID_var_group_coeffs_DF,by=c("ID_spat","Sample_N")) |> 
        dplyr::mutate(pop_off=1) |> 
        dplyr::select(all_of(coeff_vars))
      
      
      pp<-unique(CV_data_coeffs$Var2)
      gc()
      
      coeff_matrix<-CV_data_coeffs |> 
        dplyr::filter(week==1) |> 
        dplyr::select(-week,-Sample_N) |> 
        as.matrix()
      
      weight_Len<-ncol(coeff_matrix)
      dim(coeff_matrix)
      
      pred_weights<-array(NA,c(Num_dist_data*52,weight_Len,1000))
      
      #ww<-1
      names(CV_data_coeffs)
      
      for(ww in 1:52){
        
        matrix_Input<-CV_data_coeffs |> 
          dplyr::filter(week==ww) |> 
          dplyr::select(-week,-Sample_N) |> 
          as.matrix()
        
        #class(matrix_Input)
        #dim(pred_weights[1,,])
        pred_weights[ww,,]<-t(matrix_Input)
      }
      
      dim(pred_weights)
      
      mn_pref<-str_pad(gg,pad=0,side='left',width=2)
      
      weight_name<-file.path(pred_weights_pth,paste0('Pred_weights_',last_Dat_Year,'_',mn_pref,'.rds'))
      
      list_out<-list(fitted_values=wt_Obs$model_CV$summary.fitted.values,
                     Predicted_CV=apply(Ypred_NB,1,mean),
                     pred_weights=pred_weights,
                     ns_size=as.numeric(Size_1000))
      
      
      saveRDS(list_out,weight_name,compress =T)
      rm(pred_weights)
      #rm(wt_Obs)
      gc()
      })
      
      
      cat("Time weight gen ..,",sep='\n')
      
      cat(paste0("time sec..",time_Weight_gen[3],' \n'))
      
      pctn_done<-paste0(round((gg/nrow(work_CV_Weight))*100,1),' %')
      
      one_of_str<-paste0(gg,' of ',nrow(work_CV_Weight))
      
      mess_mw<-paste0(one_of_str,' (',pctn_done,")")
      p_progress$set(value = gg, detail = mess_mw)
      
    }
    
    p_progress$close()
    #Cross_Validation(1)
    
    #foreach(aa=1:nrow(work_CV))%do% Cross_Validation(aa)
  })
  
  time_pred_Weight[3]/60
  ## save meta data run
  
  weight_Meta_out<-list(work_CV_Weight=work_CV_Weight,
                        inla_grp_Names=inla_grp_Names,
                        last_Dat_Year=last_Dat_Year)
  
  weight_meta_name<-file.path(pred_weights_pth,paste0('Pred_weights_meta.rds'))
  
  
  saveRDS(weight_Meta_out,weight_meta_name,compress =T)
  gc()
  
}


#dim(pred_weights)



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
      
      cv_idx<-with(CV_data_Wt,which(year==work_CV_Weight$year[gg] & week%in% week_Sub))
      
      model_CV<-Sel_Vars(selected_Model_form_rw,"nbinomial",CV_data_Wt,theta_beg_Rw,T)
      
      #summary(model_CV)
      
      Nsamples<-1000
      
       
      
      #post_Samples <- inla.posterior.sample(Nsamples,model_CV)
      
      post_Samples <- inla.posterior.sample(n=Nsamples,result=model_CV,parallel.configs=F)
      
      
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
      
      for(ii in 1:length(inla_grp_Names)){
        
        idx_Now<-which(str_detect(rownames(ID_spat_1000_One),inla_grp_Names[ii]))
        Val_1000<-foreach(aa=1:Nsamples,.combine =cbind)%do%as.numeric(post_Samples[[aa]]$latent[idx_Now])
        assign(paste0(inla_grp_Names[ii],'_1000'),Val_1000)
        gc()
      }
      
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
      
      #tt<-1
      
      get_Weights<-function(tt){
        
        Length_ID_spat<-length(idx_spat)
        
        N_id<-Length_ID_spat/(Num_dist_data*Years_in_dat)
        
        if(N_id==1){
          ID_spat_effect<-expand.grid(ID_spat=ID_spat_Dist,cat=c("coeff_spat1"),ID_year=1:Max_IDy_data) |> 
            dplyr::mutate(coeff=ID_spat_1000[,tt]) |> 
            dplyr::group_by(ID_spat,ID_year) |> 
            tidyr::spread(cat,coeff) |> 
            dplyr::ungroup() |> 
            data.frame()
        }else{
          ID_spat_effect<-expand.grid(ID_spat=ID_spat_Dist,cat=c("coeff_spat1","coeff_spat2"),ID_year=1:Max_IDy_data) |> 
            dplyr::mutate(coeff=ID_spat_1000[,tt]) |> 
            dplyr::group_by(ID_spat,ID_year) |> 
            tidyr::spread(cat,coeff) |> 
            dplyr::ungroup() |> 
            data.frame()
        }
        
        #Week_district_1000
        
        ID_district_week<-expand.grid(week=1:52,ID_spat=ID_spat_Dist,cat="Week_effect") |> 
          dplyr::mutate(coeff=Week_district_1000[,tt]) |> 
          dplyr::group_by(ID_spat,week) |> 
          tidyr::spread(cat,coeff) |> 
          dplyr::ungroup() |> 
          data.frame()
        
        ##link to 
        
        Coeffs_string<-paste0(inla_grp_Names,"_coeffs=",paste0(inla_grp_Names,'_1000[,tt]'),collapse=',')
        Coeffs_rw<-paste0(inla_grp_Names,"_coeffs",collapse=',')
        
        coeff_mat_cmd<-glue( 'CV_data_coeffs<-Weight_ID_data |> 
            dplyr::left_join(ID_spat_effect,by=c("ID_year","ID_spat")) |> 
            dplyr::left_join(ID_district_week,by=c("ID_spat","week")) |> 
            dplyr::mutate(
              Intercept=Int_1000[,tt],',
            Coeffs_string,
            ')')
        eval(parse(text=coeff_mat_cmd))
        
        
        coeff_matrix_cmd<-glue( 'coeff_matrix<-CV_data_coeffs |> 
        dplyr::mutate(a=1,coefspat=1,week=1,pop_off=1) |> ',
        'dplyr::select(Intercept,coeff_spat1,Week_effect,',
        Coeffs_rw,
        ',pop_off)|> ',
        ' as.matrix()')
        eval(parse(text=coeff_matrix_cmd))
        
        #dim(coeff_matrix)
        #colnames(coeff_matrix)
        
        weight_Len<-ncol(coeff_matrix)
        dim(coeff_matrix)
        
        weight_Array<-array(NA,c(Num_dist_data*52,weight_Len,1))
        weight_Array[,,1]<-coeff_matrix
        
        weight_Array
        
      }
      
      
      
      Obj_exp<-c("Int_1000","Weight_ID_data",paste0(inla_grp_Names,"_1000"))
      
     
        pred_weights<-foreach(aa=1:1000,.final =abind::abind,.packages =c("dplyr","stringr","glue"),
                              .export=Obj_exp)%do% get_Weights(aa)
      
      
      dim(pred_weights)
      
      mn_pref<-str_pad(gg,pad=0,side='left',width=2)
      
      weight_name<-file.path(pred_weights_pth,paste0('Pred_weights_',last_Dat_Year,'_',mn_pref,'.rds'))
      
      list_out<-list(fitted_values=model_CV$summary.fitted.values,
                     Predicted_CV=apply(Ypred_NB,1,mean),
                     pred_weights=pred_weights,
                     ns_size=as.numeric(Size_1000))
      
      
      saveRDS(list_out,weight_name,compress =T)
      rm(pred_weights)
      rm(model_CV)
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



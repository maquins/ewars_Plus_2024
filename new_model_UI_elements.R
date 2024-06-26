
## risk mapping elements

dist_cesar<-c(20001,20011,20013,20060,20228,20710,20770)

dist_SEL<-1:5

output_dist_new_model<-selectInput(inputId = 'district_new',
            label = 'District',
            choices = dist_SEL,
            selected =dist_SEL[1],
            selectize =T,
            multiple =F)


output_dist_seas_prev<-selectInput(inputId = 'district_seas',
                                   label = 'District',
                                   choices = dist_SEL,
                                   selected =dist_SEL[1],
                                   selectize =T,
                                   multiple =F)

output_dist_seas<-selectInput(inputId = 'district_seas',
                              label = 'District',
                              choices = dist_SEL,
                              selected =dist_SEL[1],
                              selectize =T,
                              multiple =F)

output_dist_validation<-selectInput(inputId = 'district_validation',
                              label = 'District',
                              choices = dist_SEL,
                              selected =dist_SEL[1],
                              selectize =T,
                              multiple =F)

output_dist_pros<-selectInput(inputId = 'district_prospective',
                                    label = 'District',
                                    choices = c(1:3,NA),
                                    selected =NA,
                                    selectize =T,
                                    multiple =F)

year_validation<-sliderInput(inputId = "new_model_Year_validation",
            label = "Model Year",
            min = 2019,
            max=2020,
            value =2020,
            sep='',
            step=1)

N_lags<-sliderInput(inputId = "nlags",
                             label = "Lag Weeks",
                             min = 4,
                             max=24,
                             value =12,
                             sep='',
                             step=1)

z_outbreak_New<-sliderInput(inputId = "z_outbreak_new",
                    label = "Z outbreak",
                    min = 1,
                    max=4,
                    value =1.2,
                    sep='',
                    step=0.1)


z_outbreak_Conditional<-conditionalPanel(
  condition = "input.Optimal_z == F",
  sliderInput(inputId = "z_outbreak_new",
              label = "Z outbreak",
              min = 1,
              max=4,
              value =1.2,
              sep='',
              step=0.1)
)

output_year_New_model<-inputPanel(column(12,
                              sliderInput(inputId = "new_model_Year_plot",
                                          label = "Year",
                                          min = 2015,
                                          max=2020,
                                          value =2017,
                                          sep='',
                                          step=1)),
                              column(12,offset=6,sliderInput(inputId = "new_model_Week_plot_spat",
                                          label = "Week",
                                          min = 1,
                                          max=52,
                                          step=1,
                                          sep='',
                                          value =35))
                              
              
)

Run_pros_Pred<-actionButton('run_Pred','Run Predictions',
             style="color: forestgreen; background-color:grey(0.5);
                                 padding: 10px 15px; height: 80px; cursor: pointer;
                                 font-size: 20px; font-weight: 400;")

surv.Input_prospective<-column(12,offset =2,
                               fileInput('dat_prospective', 'Upload prospective data', 
                                         accept=c('.xls','.xlsx','.csv')
                                         ))


header_Input_pros_prev<-fluidRow(inputPanel(surv.Input_prospective,
                                       column(12,offset=7,
                                              uiOutput("dist_pros"))))

header_Input_pros<-fluidRow(inputPanel(surv.Input_prospective,
                                       column(12,offset=7,
                                              output_dist_pros),
                                       column(12,offset=7,
                                              Run_pros_Pred)))


Upload_pros<-fluidRow(inputPanel(surv.Input_prospective))


dist_run_pred<-fluidRow(column(12,offset=0.5,
                               inputPanel(
                                 column(12,offset=0,
                                        output_dist_pros),
                                 column(12,offset=3,
                                        Run_pros_Pred))))


pros_new_out1<-tabPanel("Uploaded_data",
                        DT::dataTableOutput("uploaded_pros")
)

pros_new_out2<-tabPanel("Prediction table",
                        DT::dataTableOutput("prediction_tab_pros")
)

pros_new_out3<-tabPanel("Outbreak",
                        plotOutput("outbreak_plot_pros",width ="700px",height ="320px")
)

pros_new_out4<- tabPanel("Probability",
                         #inputPanel(uiOutput("sel_diss1")),
                         plotOutput("prob_plot_pros",width ="700px",height ="320px")
)

pros_new_out5<- tabPanel("Outbreak and Probability",
                         #inputPanel(uiOutput("sel_diss1")),
                         plotOutput("out_break_prob_plot_pros",width ="1400px",height ="600px")
)

pros_new_out6<- tabPanel("Response",
                         #inputPanel(uiOutput("sel_diss1")),
                         plotOutput("response_plot_pros",width ="1400px",height ="600px")
)

late_reponse_html<-HTML("<font size='2' color='red'> <b> 1. Late/emergency response: </b> </font>  <font size='2' > is technically declared when more than three consecutive ’Alarms’ take place! For instance, if the ‘Late/emergency response’ occurred on week 19 due to an alarm signal (outbreak probability exceeding the alarm threshold) happening at weeks 16, 17 and 18. If the given lag time was 12 weeks (i.e. disease outbreak is predicted to take place 12 weeks from week 19), then, due to this consecutive occurrence of alarm signals, the program notified a ‘Late/emergency response’ to be considered at week 31 (i.e 19+12). In this scenario, the dashboard already declared ‘Initial response’ at week 17 (since there had already been two consecutive alarm signals) and an ‘Early response’ in week 18 (since there had already been three consecutive alarm signals)!
                        </font> <br> <font size='2' > <b><i> Vector control team need full scale up of activities and response based on the country protocol.</i></b>  </font>" )

early_reponse_html<-HTML("<font size='2' color='red'> <b> 2. Early response: </b> </font>  <font size='2' > is declared when three consecutive alarm signals occur, to avoid ‘Late/emergency response’. Another example from next figure shows that early response in week 18 occurred due to an alarm signal (outbreak probability exceeding the alarm threshold) at weeks 16, 17 and again in week 18. Then, due to this consecutive occurrence of alarm signals, the program notified that an early response should be considered at week 18. In this scenario, the dashboard must have already declared ‘Initial response’ at week 17 since there had already been two consecutive alarm signals!
                        </font> <br> <font size='2' > <b><i> Vector control team need advance scale up of activities and response based on the country protocol.</i></b>  </font>" )

initial_reponse_html<-HTML("<font size='2' color='red'> <b> 3. Initial response:  </b> </font>  <font size='2' > is declared when two consecutive alarm signals occur. Let’s take an example from figure below, the initial response in weeks 17 occurred due to an alarm signal (outbreak probability exceeding the alarm threshold) happening at weeks 16 and 17. Then, due to this consecutive occurrence of alarm signal, the program notified that an initial response should be considered at week 17.
                        </font> <br> <font size='2' > <b><i> Vector control team need a start of scale up of activities and response based on the country protocol.</i></b>  </font>" )

no_reponse_html<-HTML("<font size='2' color='red'> <b> 4. No response:  </b> </font>  <font size='2' > is declared when there are no alarm signals or only one alarm signal in the current week.
                        </font> <br> <font size='2' > <b><i> Vector control team can continue their routine activities without the need to scale up!</i></b>  </font>" )



pros_new_out7<- tabPanel(HTML("<font size='2' color='green'><b>Response definitions</b></font> "),
                         br(),
                         br(),
                         late_reponse_html,
                         br(),
                         br(),
                    
                         early_reponse_html,
                         br(),
                         br(),
                         initial_reponse_html,
                         br(),
                         br(),
                         no_reponse_html
)

pros_meta_alarm_Ind<-uiOutput("pros_meta1")
pros_meta_sel_lags<-uiOutput("pros_meta2")

pros_meta_pred_dist<-uiOutput("pros_meta3")

pros_meta_zoutbreak<-uiOutput("pros_meta4")

pros_meta_alarm_thres<-uiOutput("pros_meta5")



plots_Tabs_DBII<-tabsetPanel(#pros_new_out1,
  #pros_new_out3,
  #pros_new_out4,
  pros_new_out5,
  pros_new_out6,
  pros_new_out7,
  pros_new_out2
)

row_elements_New_Model_prospective<-fluidRow(   
  shinydashboardPlus::box(width=12,
        column(2,
               Upload_pros,
               br(),
               br(),
               br(),
               br(),
               pros_meta_alarm_Ind,
               pros_meta_sel_lags,
               pros_meta_pred_dist,
               pros_meta_zoutbreak,
               pros_meta_alarm_thres),
        
        column(10,offset =0.5,
               dist_run_pred,
               plots_Tabs_DBII
               
        )
         
  )
)



output_graphs_New_Model<-tabsetPanel(
  tabPanel("Descriptive analysis",
            #fluidPage(inputPanel(column(12,offset=0,output_dist_new_model)),
                      fluidPage(
                                
             tabsetPanel(
  tabPanel("Tables",

  tableOutput("new_model_data_descriptives")
  
)
,

tabPanel("Plots",
      
     #     output_dist_new_model,
          uiOutput("new_model_data_descriptive_Plots")
         
)
)
)
)

,
tabPanel("Time_series",
         #   output_year_New_model,
         uiOutput("new_model_Time_series_Plots")
         
),
tabPanel("Spatial Plots",
         fluidPage(output_year_New_model,
                   tabsetPanel(
  tabPanel("Spatial_Covariate_Plots", 
           uiOutput("Error"),
           uiOutput("Spat_Covariate_Plots_new_Model")
            
  ),
  
  tabPanel("DIR",
           leafletOutput("spat_DIR_new_Model"),
  )
  )
  )
  ),
  tabPanel("Lag non linear plots",
           fluidPage(
             #column(12,
                            
                    #N_lags),
                     tabsetPanel(
                       tabPanel("Lag Countour Plots",
           uiOutput("lag_contour_Plots")
                       ),
           tabPanel("Var Slices",
                    uiOutput("var_Slices_Plots")
           )
                     )
           
  )
  
),
tabPanel("Seasonality",
         # prev fluidPage(inputPanel(column(12,offset=0,output_dist_seas)),
                   #fluidPage(inputPanel(column(12,offset=0,output_dist_seas)),
                             fluidPage(
                                       
                             
                   plotOutput("Seasonality_plot"))),
tabPanel("Model Validation",
         fluidPage(inputPanel(#column(12,offset=0,output_dist_validation),
                              #column(12,offset=0,plotOutput("fdff")),
                              column(12,offset=4),
                              column(12,offset=4,z_outbreak_Conditional),
                              #column(12,offset=4,checkboxInput("Optimal_z", "Optimal z\noutbreak", value = FALSE, width = NULL))),
                              column(12,offset=4,checkboxInput("Optimal_z", "Optimal z\noutbreak", value = TRUE, width = NULL))),
         
                   tabsetPanel(tabPanel("Model selection",
                                        uiOutput("lag_seleCtion_Tab")
                                        ),
                               tabPanel("Runin period",
                                        plotOutput("runin_ggplot_New_model"),
                                        dygraphOutput("runin_interactive_New_model")),
                               tabPanel("Validation_period",
                                        tabsetPanel(
                                 tabPanel("Plots",
                                        plotOutput("validation_ggplot_New_model"),
                                        dygraphOutput("validation_interactive_New_model")),
                                 tabPanel("Sensitivity/Specificity",
                                          #tableOutput("sen_spec_table_New_model")
                                          uiOutput("sen_spec_table_New_model"),
                                          uiOutput("sen_spec_table_Optimal")
                                 )
                                 )
                                        ))
                   )
      )
# id="to_show2",
# selected="Model Validation"
)



boundary.Input_prev<-column(12,offset =0,
                       fileInput('shape_file_new_Model', 'District sub-district boundary file (.shp file)',
                                 accept=c('.shp','.dbf','.shx','.prj'),
                                 multiple =T,
                                 width='70%'))

#tags$h4("Please enter a valid user name and password")

boundary.Input<-column(12,offset =0,
                       fileInput('shape_file_new_Model', HTML("<font size='2'>  District /sub-district    boundary file </font> <br/> <font size='2' color='red'>(upload all 4 files:shp,dbf,shx,prj) </font>"),
                                 accept=c('.shp','.dbf','.shx','.prj'),
                                 multiple =T,
                                 width='70%'))




surv.Input<-column(12,offset =0,
                   fileInput('dat_new_Model', HTML("<font size='2'> Choose surveillance data with spatial inputs </font>"), 
                             accept=c('.xls','.xlsx','.csv'),
                             width='70%'))



pop.Input<-column(12,offset =0,selectInput("population_New_model",
            "Variable for annual total Population",
            "population",multiple =F,
            width='70%'))


case.Input<-column(12,offset =0,selectInput("number_of_cases_New_model", 
            "Variable for the weekly number of outbreak",
            "weekly_hospitalised_cases",multiple =F,
            width='70%'))


alarm.Input<-column(12,offset =0,selectInput("alarm_indicators_New_model", selected =c("rainsum","meantemperature"),
            "Alarm indicator(s)",
            choices=c("rainsum","meantemperature"),multiple =T,
            width='70%'))

alarm_Spline.Input<-column(12,offset =0,selectInput("other_alarm_indicators_New_model", selected =c("rainsum","meantemperature"),
                                             "Other alarm indicator(s)",
                                             choices=" ",multiple =T,
                                             width='70%'))





header_Input<-fluidRow(boundary.Input,
                                  surv.Input,
                                  pop.Input,
                                  case.Input,
                                  alarm.Input,
                       alarm_Spline.Input,
                       N_lags,
                       year_validation)




dat_opts_new_Model<- tabPanel("Spatial data Upload",
                         column(12,
                                shinydashboardPlus::box(width=12,
                                                        ##choose point/ lat long data
                                                        
                                                        fileInput('shape_file_new_Model', 'District sub-district boundary file (.shp file)',
                                                                  accept=c('.shp','.dbf','.shx','.prj'),
                                                                  multiple =T),
                                                        ## choose aggregated or point data
                                                
                                                        
                                                        fileInput('dat_new_Model', 'Choose surveillance data with spatial inputs', 
                                                                  accept=c('.xls','.xlsx','.csv')),
                                                        uiOutput("Spat_error_new_Model"),
                                                        selectInput("population_New_model",
                                                                    "Variable for annual total Population",
                                                                    "population",multiple =F),
                                                        selectInput("number_of_cases_New_model", 
                                                                    "Variable for the weekly number of outbreak",
                                                                    "weekly_hospitalised_cases",multiple =F),
                                                        selectInput("alarm_indicators_New_model", selected =c("rainsum","meantemperature"),
                                                                    "Alarm indicator(s)",
                                                                    choices=c("rainsum","meantemperature"),multiple =T)
                                                        
                                                        
                                )
                         ))



row_elements_New_Model.prev<-fluidRow(     
  column(4,
         tabsetPanel(id="input_pan_new_Model",
                     dat_opts_new_Model
                     # sel_vars
         )),
  
  column(8,
         uiOutput("spat_Display_new_Model")
         )
)



save_M<-column(1,
               plotOutput("dd",height ="220px"),
       actionButton('run_mod','Run Model',
                    style="color: forestgreen; background-color:grey(0.5);
                                 padding: 10px 15px; height: 80px; cursor: pointer;
                                 font-size: 20px; font-weight: 400;")
)

## District Plus save Model Plus
save_M<-column(1,
               plotOutput("dd",height ="220px"),
               actionButton('run_mod','Run Model',
                            style="color: forestgreen; background-color:grey(0.5);
                                 padding: 10px 15px; height: 80px; cursor: pointer;
                                 font-size: 20px; font-weight: 400;")
)

Dist_and_saveM<-column(1,
                     
                       plotOutput("dd",height ="40px"),
                       #inputPanel(column(12,output_dist_new_model)),
                       column(12,output_dist_new_model),
                       
                       plotOutput("dd1",height ="60px"),
                       actionButton('run_mod','Run Model',
                                    style="color: forestgreen; background-color:grey(0.5);
                                 padding: 10px 15px; height: 80px; cursor: pointer;
                                 font-size: 20px; font-weight: 400;")
)



#output_graphs_New_Model

row_elements_New_Modela<-fluidRow(     
  column(3,
         header_Input),
  
  column(8,
         uiOutput("spat_Display_new_Model")
         
  ),
  save_M
)

District_header<-fluidRow(
  column(12,inputPanel(output_dist_new_model))
  
)

row_elements_New_Model<-fluidRow(
  
  column(3,
         header_Input),
  
  column(8,
         District_header,
         output_graphs_New_Model
         
  ),
  
  save_M
)

dashboad_elements_New_model<-tabPanel("Dashboard I",
                                         fluidPage(
                                           #District_header,
                                           row_elements_New_Model

                                         )
) 


dashboad_elements_New_model_pros<-tabPanel("Dashboard II",
         row_elements_New_Model_prospective
)
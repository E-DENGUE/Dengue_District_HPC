mod1 <- 'm_DHF_cases_hold ~ lag3_y +urban_dic +
           cluster *lag3_avg_min_daily_temp+
        f(t, model="ar1") + f(districtID, model="iid") +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod2 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density* urban_dic+ Inmigration_Rate*urban_dic  + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+
         lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") + f(districtID, model="iid") +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod3<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +cluster+
       urban_dic* lag3_total_rainfall_ab +  lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind  +
        f(t, model="ar1") + f(districtID, model="iid") +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod4 <- 'm_DHF_cases_hold ~ -1+lag3_y +prediomentent+urban_dic +
        Population_density + Inmigration_Rate + BI_larvae + HI_larvae + DI + Poverty_Rate +
        Hygienic_Water_Access + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") + f(districtID, model="iid") +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'





mod5 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density* urban_dic+ Inmigration_Rate*urban_dic + BI_larvae + HI_larvae + DI + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") + f(districtID, model="iid") +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

##Base model 


mod6 <- 'm_DHF_cases_hold ~ lag3_y+
        f(t, model="ar1") + f(districtID, model="iid") +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'




mod7 <- 'm_DHF_cases_hold ~ lag3_y +urban_dic +
           cluster *lag3_avg_min_daily_temp+
        f(t, model="ar1") +  f(districtID, model="bym", graph=MDR.adj) +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod8 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density* urban_dic+ Inmigration_Rate*urban_dic  + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+
         lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") +  f(districtID, model="bym", graph=MDR.adj) +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod9<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +cluster+
       urban_dic* lag3_total_rainfall_ab +  lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind  +
        f(t, model="ar1") +  f(districtID, model="bym", graph=MDR.adj,hyper=hyper.bym) +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod10 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density + Inmigration_Rate + BI_larvae + HI_larvae + DI + Poverty_Rate +
        Hygienic_Water_Access + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") +  f(districtID, model="bym", graph=MDR.adj) +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

##Base model
mod11 <- 'm_DHF_cases_hold ~ lag3_y  +
        f(t, model="ar1") +  f(districtID, model="bym", graph=MDR.adj) +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'





mod12 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density* urban_dic+ Inmigration_Rate*urban_dic + BI_larvae + HI_larvae + DI + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") + f(districtID, model="iid") +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'




##models from 13 to 24 does not work
##Type 2

mod13 <- 'm_DHF_cases_hold ~ lag3_y +urban_dic +
           cluster *lag3_avg_min_daily_temp+
        f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group= time_id4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod14<- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density* urban_dic+ Inmigration_Rate*urban_dic  + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+
         lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
      Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group= time_id4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod15<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +cluster+
       urban_dic* lag3_total_rainfall_ab +  lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind  +
       + f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group= time_id4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'




mod16 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density + Inmigration_Rate + BI_larvae + HI_larvae + DI + Poverty_Rate +
        Hygienic_Water_Access + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
         +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+ f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group= time_id4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'





mod17 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
  Population_density* urban_dic+ Inmigration_Rate*urban_dic + BI_larvae + HI_larvae + DI + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
  Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+prediomentent+
  lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
  lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp  +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+ f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group= time_id4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

##Base model
mod18 <- 'm_DHF_cases_hold ~ lag3_y+ f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group= time_id4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



#################Type 3

mod19<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +
           cluster *lag3_avg_min_daily_temp+
       f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f( time_id1,model="iid", group=districtID3,
control.group=list(model="besag",
graph=MDR.adj))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod20<- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density* urban_dic+ Inmigration_Rate*urban_dic  + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+
         lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
      Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f( time_id1,model="iid", group=districtID3,
control.group=list(model="besag",
graph=MDR.adj))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'




mod21<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +cluster+
       urban_dic* lag3_total_rainfall_ab +  lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind   
       + f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f( time_id1,model="iid", group=districtID3,
control.group=list(model="besag",
graph=MDR.adj))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'





mod22 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density + Inmigration_Rate + BI_larvae + HI_larvae + DI + Poverty_Rate +
        Hygienic_Water_Access + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
         +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+ f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f( time_id1,model="iid", group=districtID3,
control.group=list(model="besag",
graph=MDR.adj))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'




mod23 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
  Population_density* urban_dic+ Inmigration_Rate*urban_dic + BI_larvae + HI_larvae + DI + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
  Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+prediomentent+
  lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
  lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp  +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+ f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f( time_id1,model="iid", group=districtID3,
control.group=list(model="besag",
graph=MDR.adj))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

##Base model
mod24 <- 'm_DHF_cases_hold ~ lag3_y+ f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f( time_id1,model="iid", group=districtID3,
control.group=list(model="besag",
graph=MDR.adj))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

############Type 4

mod25<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +
           cluster *lag3_avg_min_daily_temp+
       f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod26<- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density* urban_dic+ Inmigration_Rate*urban_dic  + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+
         lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
      Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod27<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +cluster+
       urban_dic* lag3_total_rainfall_ab +  lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind  +
       + f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod28 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
        Population_density + Inmigration_Rate + BI_larvae + HI_larvae + DI + Poverty_Rate +
        Hygienic_Water_Access + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
         +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+ f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'





mod29 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +
  Population_density* urban_dic+ Inmigration_Rate*urban_dic + BI_larvae + HI_larvae + DI + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
  Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+prediomentent+
  lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
  lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp  +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+ f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


##Base model
mod30 <- 'm_DHF_cases_hold ~ lag3_y+ f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod31 <- 'm_DHF_cases_hold ~ lag3_y +urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
           cluster *lag3_avg_min_daily_temp+
        f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group=districtID4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod32 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density* urban_dic+ Inmigration_Rate*urban_dic  + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+
         lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
      Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group=districtID4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod33<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +cluster+lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
       urban_dic* lag3_total_rainfall_ab +  lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind  +
       + f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group=districtID4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'




mod34 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density + Inmigration_Rate + BI_larvae + HI_larvae + DI + Poverty_Rate +
        Hygienic_Water_Access + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
         +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+ f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group=districtID4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'





mod35 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
  Population_density* urban_dic+ Inmigration_Rate*urban_dic + BI_larvae + HI_larvae + DI + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
  Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+prediomentent+
  lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
  lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp  +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+ f(districtID,model="bym",graph=MDR.adj) +
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group=districtID4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

##Base model
mod36 <- 'm_DHF_cases_hold ~ lag3_y+ f(districtID,model="bym",graph=MDR.adj) +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
f( time_id2,model="rw2") +
f( time_id3,model="iid") +
f(districtID3,model="iid", group=districtID4,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


############Type 4

mod37<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
           cluster *lag3_avg_min_daily_temp+
       f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod38<- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density* urban_dic+ Inmigration_Rate*urban_dic  + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+
         lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
      Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod39<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +cluster+lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
       urban_dic* lag3_total_rainfall_ab +  lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind  +
       + f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod40 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density + Inmigration_Rate + BI_larvae + HI_larvae + DI + Poverty_Rate +
        Hygienic_Water_Access + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
         +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+ f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'





mod41 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
  Population_density* urban_dic+ Inmigration_Rate*urban_dic + BI_larvae + HI_larvae + DI + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
  Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+prediomentent+
  lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
  lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp  +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+ f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


##Base model
mod42 <- 'm_DHF_cases_hold ~ lag3_y+ f(districtID,model="bym",graph=MDR.adj) +
f(time_id1,model="rw2") +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
f(time_id2,model="iid") +
f(districtID4,model="besag", graph=MDR.adj,
group=time_id3,
control.group=list(model="rw2"))+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod43 <- 'm_DHF_cases_hold ~ lag3_y +urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
           cluster *lag3_avg_min_daily_temp+
        f(t, model="ar1") +  f(districtID, model="bym", graph=MDR.adj) +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod44 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density* urban_dic+ Inmigration_Rate*urban_dic  + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+
         lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") +  f(districtID, model="bym", graph=MDR.adj) +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod45<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +cluster+lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
       urban_dic* lag3_total_rainfall_ab +  lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind  +
        f(t, model="ar1") +  f(districtID, model="bym", graph=MDR.adj) +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod46 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density + Inmigration_Rate + BI_larvae + HI_larvae + DI + Poverty_Rate +
        Hygienic_Water_Access + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") +  f(districtID, model="bym", graph=MDR.adj) +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

##Base model
mod47 <- 'm_DHF_cases_hold ~ lag3_y +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        f(t, model="ar1") +  f(districtID, model="bym", graph=MDR.adj) +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'




mod48 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density* urban_dic+ Inmigration_Rate*urban_dic + BI_larvae + HI_larvae + DI + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") + f(districtID, model="iid") +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

##Dan's model mod39
mod49 <- 'm_DHF_cases_hold~   lag3_y + lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
                            f(districtID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                   scale.model = TRUE) +
                     lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
                        f(t, replicate=districtID3, model="ar1", hyper = hyper.ar1) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

mod50 <- 'm_DHF_cases_hold~   lag3_y + +urban_dic +
           cluster *lag3_avg_min_daily_temp+lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m +
                            f(districtID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                   scale.model = TRUE) +
                        f(t, replicate=districtID3, model="ar1", hyper = hyper.ar1) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod51 <- 'm_DHF_cases_hold~   lag3_y +prediomentent+urban_dic +
        Population_density* urban_dic+ Inmigration_Rate*urban_dic  + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+
         lag3_monthly_cum_ppt +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
                            f(districtID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                   scale.model = TRUE) +
                        f(t, replicate=districtID3, model="ar1", hyper = hyper.ar1) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod52 <- 'm_DHF_cases_hold~   lag3_y  +urban_dic +cluster+lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
       urban_dic* lag3_total_rainfall_ab +  lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind  +
                            f(districtID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                   scale.model = TRUE) +
                        f(t, replicate=districtID3, model="ar1", hyper = hyper.ar1) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod53 <- 'm_DHF_cases_hold~   lag3_y  +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density + Inmigration_Rate + BI_larvae + HI_larvae + DI + Poverty_Rate +
        Hygienic_Water_Access + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
                            f(districtID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                   scale.model = TRUE) +
                        f(t, replicate=districtID3, model="ar1", hyper = hyper.ar1) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod54 <- 'm_DHF_cases_hold ~ lag3_y +urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
           cluster *lag3_avg_min_daily_temp+
        f(t, model="ar1") + f(districtID, model="iid") +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod55 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density* urban_dic+ Inmigration_Rate*urban_dic  + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+
         lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") + f(districtID, model="iid") +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod56<- 'm_DHF_cases_hold ~ lag3_y +urban_dic +cluster+lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
       urban_dic* lag3_total_rainfall_ab +  lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind  +
        f(t, model="ar1") + f(districtID, model="iid") +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'



mod57 <- 'm_DHF_cases_hold ~ -1+lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density + Inmigration_Rate + BI_larvae + HI_larvae + DI + Poverty_Rate +
        Hygienic_Water_Access + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") + f(districtID, model="iid") +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'





mod58 <- 'm_DHF_cases_hold ~ lag3_y +prediomentent+urban_dic +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        Population_density* urban_dic+ Inmigration_Rate*urban_dic + BI_larvae + HI_larvae + DI + Poverty_Rate* lag3_total_rainfall_ab* Population_density +
        Hygienic_Water_Access*lag3_avg_min_daily_temp + Hygienic_Toilet_Access  + cluster+prediomentent+
        lag3_total_rainfall_ab + lag3_avg_daily_humid + lag3_monthly_cum_ppt +
        lag3_avg_min_daily_temp + lag3_avg_daily_wind + lag3_avg_max_daily_temp +
        f(t, model="ar1") + f(districtID, model="iid") +Monthly_Average_Income_Percapita+ lag3_number_of_outbreak_response+Total_Passenger+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

##Base model 

mod59 <- 'm_DHF_cases_hold ~ lag3_y +lag3_log_cum_inc_12m+lag3_log_cum_inc_24m+lag3_log_cum_inc_36m+
        f(t, model="ar1") + f(districtID, model="iid") +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

######## other models'
# sam as old mod33
mod60 <- 'm_DHF_cases_hold~   lag3_y + 
                            f(districtID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                    hyper = hyper.besag ,
                                   scale.model = TRUE) +
                     lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
                        f(t, replicate=districtID3, model="ar1", hyper = hyper.ar1) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

#this is same as the old model 39
mod61 <- 'm_DHF_cases_hold~   lag3_y + log_cum_inc_12m +log_cum_inc_24m +log_cum_inc_36m +
                            f(districtID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                    hyper = hyper.besag ,
                                   scale.model = TRUE) +
                     lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
                        f(t, replicate=districtID3, model="ar1", hyper = hyper.ar1) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


all.mods <- list('mod1'=mod6,'mod2'=mod60, 'mod3'=mod61)
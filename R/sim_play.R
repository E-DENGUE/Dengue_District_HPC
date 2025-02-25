library(tidyverse)


d2<-  readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3.rds')

#average rank of districts
district_rank <- d2 %>%
  group_by(district, year) %>%
  mutate(year=as.numeric(as.character(year)),
         max_month = if_else(m_DHF_cases==max(m_DHF_cases, na.rm=T),month, NA_real_  )) %>%
  filter(year <= 2014) %>%
  summarize(tot_cases_year=sum(m_DHF_cases),
            max_month = max(max_month, na.rm=T)
            ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(rankorder=rank(-1*tot_cases_year)) %>%
  ungroup() %>%
  arrange(year, rankorder) %>%
  group_by(district) %>%
  summarize(ave_rank=mean(rankorder),
            max_month= mean(max_month, na.rm=T)) %>%
  ungroup() %>%
  arrange(ave_rank) %>%
  mutate(district_order=row_number())

d3 <- d2 %>%
  left_join(district_rank, by='district') %>%
  mutate( max_month= round(max_month), 
        apr_start = if_else(district_order<=60 & month==4, m_DHF_cases*0.25,
                              if_else(district_order<=60 & month==5, m_DHF_cases*0.5,       
                                      if_else(district_order<=60 & month==6, m_DHF_cases*0.25,  
                                              m_DHF_cases
                              )   )),
          may_start = if_else(district_order<=60 & month==5, m_DHF_cases*0.25,
                              if_else(district_order<=60 & month==6, m_DHF_cases*0.5,       
                                      if_else(district_order<=60 & month==7, m_DHF_cases*0.25,  
                                              m_DHF_cases
                                      )   )),
          jun_start = if_else(district_order<=60 & month==6, m_DHF_cases*0.25,
                              if_else(district_order<=60 & month==7, m_DHF_cases*0.5,       
                                      if_else(district_order<=60 & month==8, m_DHF_cases*0.25,  
                                              m_DHF_cases
                                      )   )),
          jul_start = if_else(district_order<=60 & month==7, m_DHF_cases*0.25,
                              if_else(district_order<=60 & month==8, m_DHF_cases*0.5,       
                                      if_else(district_order<=60 & month==9, m_DHF_cases*0.25,  
                                              m_DHF_cases
                                      )   )),
          aug_start = if_else(district_order<=60 & month==8, m_DHF_cases*0.25,
                              if_else(district_order<=60 & month==9, m_DHF_cases*0.5,       
                                      if_else(district_order<=60 & month==10, m_DHF_cases*0.25,  
                                              m_DHF_cases
                                      )   )),
          opt_start = if_else(district_order<=60 & month==(max_month-1), m_DHF_cases*0.25,
                              if_else(district_order<=60 & month==(max_month+0), m_DHF_cases*0.5,       
                                      if_else(district_order<=60 & month==(max_month+1), m_DHF_cases*0.25,  
                                              m_DHF_cases
                                      )   ))
  )



pct_reduct <- d3 %>%
  group_by(year) %>%
  summarize(m_DHF_cases=sum(m_DHF_cases),
            apr_start = sum(apr_start),
            may_start = sum(may_start),
            jun_start = sum(jun_start),
            jul_start = sum(jul_start),
            aug_start = sum(aug_start),
            opt_start=sum(opt_start)
            ) %>%
  mutate(pct_reduct_apr = 100*(1 -  apr_start/m_DHF_cases),
         pct_reduct_may = 100*(1 -  may_start/m_DHF_cases),
         pct_reduct_jun = 100*(1 -  jun_start/m_DHF_cases),
         pct_reduct_jul = 100*(1 -  jul_start/m_DHF_cases),
         pct_reduct_aug = 100*(1 -  aug_start/m_DHF_cases),
         pct_reduct_optimal = 100*(1 -  opt_start/m_DHF_cases),
         year=as.numeric(as.character(year))
  ) %>%
  dplyr::select( year, starts_with('pct_reduct')) %>%
  filter(year>=2015)
         
write.csv(pct_reduct, './Data/pct_reduct.csv')       


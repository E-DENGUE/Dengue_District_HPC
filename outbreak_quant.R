library(dplyr)
library(MASS)
d2 <- readRDS('./Data/CONFIDENTIAL/cleaned_data.rds')

all.districts <- unique(d2$district) 

all.rlm <- lapply(all.districts, function(X){
d2a <- d2 %>%
    filter(district==X) %>%
  arrange(district, date) %>%
  mutate(inc = m_DHF_cases/pop*100000,
         log_inc=log((m_DHF_cases+0.5)/pop*100000),
         index=row_number(),
         sin12=sin(2*pi*index/12),
         cos12=cos(2*pi*index/12)
         )


p1 <- ggplot(d2a, aes(x=date, y=log_inc) )+
  geom_line()+
  theme_classic()


  mod1 = rlm(log_inc~sin12 + cos12, data=d2a)

  pred1 <- as.data.frame(exp(predict( mod1, newdata = d2a, interval = "prediction"))) %>%
    mutate(date=d2a$date) %>%
    left_join(d2a, by='date')
  return(pred1)
  })
  
e1 <- bind_rows(all.rlm) %>%
  group_by(district)%>%
  mutate(dist_id =dplyr::cur_group_id()) %>%
  ungroup()

p2 <-e1 %>% filter(dist_id>=2 & dist_id<=7) %>%
   ggplot( aes(x=date, y=inc) )+
    geom_line()+
    geom_line(aes(x=date, y=fit), col='red')+
   # geom_ribbon( aes(x=date, ymin=lwr, ymax=upr), fill='blue', alpha=0.2)+
    theme_classic()+
    facet_wrap(~district, nrow=3, ncol=2)
p2

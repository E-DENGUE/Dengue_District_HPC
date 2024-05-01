library(readxl)
library(dplyr)

d1 <- readxl::read_excel('C:/Users/DMW63/Downloads/240417_ED_Weekly DISTRICT data_f.xlsx') %>%
  reshape2::melt(., id.vars=c('Province','District','Year')) %>%
  filter(!is.na(value)) %>%
  rename(week=variable, year=Year) 

week_months <- d1 %>% 
  dplyr::select(year,week) %>% 
  distinct() %>%
  arrange(year, week) %>%
  mutate(date=seq.Date(from=as.Date('2005-12-26') ,length.out = n(),by='week'))

d2 <- d1 %>%
  left_join(week_months, by=c('week','year')) %>%
  dplyr::select(-year,-week)

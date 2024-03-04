
source('./R/predict.rlm.R')
source('./R/deseasonalize_climate.R')
source('./R/all_district_fwd1.R')
source('./R/scoring_func.R')
source('./0_specify_models.R')

#d2 <- readRDS('./Data/CONFIDENTIAL/cleaned_data.rds')
d2 <- readRDS('./Data/CONFIDENTIAL/full_data_with_new_boundaries_all_factors.rds') %>%
  rename(District_province = prov_dis)


MDR_NEW <- st_read(dsn = "./Data/shapefiles/MDR_NEW_Boundaries_Final.shp") %>%
 rename(District_province = Dstrct_p)  %>%
  mutate(District_province=gsub("_"," ",District_province , fixed='T'))

sort(MDR_NEW$District_province) ==sort(unique(d2$District_province))

# Remove island districts (no neighbors) from the dataset
spat_IDS <- MDR_NEW %>%
  dplyr::filter(VARNAME != "Kien Hai", 
                VARNAME != "Phu Quoc") %>%
  rename(district=VARNAME) %>%
  mutate(districtID= row_number(), district=toupper(district)) %>%
  as.data.frame() %>%
  dplyr::select(District_province,districtID) 

# Set the file path for the adjacency graph file
MDR.adj <- paste(getwd(), "/MDR.graph", sep = "")

date.test2 <- seq.Date(from=as.Date('2012-01-01') ,to=as.Date('2022-12-01') , by='month')



##Priors from Gibb ms 
# iid model 
hyper.iid = list(theta = list(prior="pc.prec", param=c(1, 0.01)))

# ar1 model
hyper.ar1 = list(theta1 = list(prior='pc.prec', param=c(0.5, 0.01)),
                 rho = list(prior='pc.cor0', param = c(0.5, 0.75)))

# bym model
hyper.bym = list(theta1 = list(prior="pc.prec", param=c(1, 0.01)),
                 theta2 = list(prior="pc.prec", param=c(1, 0.01)))

# bym2 model
# probability of SD of theta1 > 1 = 0.01
hyper.bym2 = list(theta1 = list(prior="pc.prec", param=c(1, 0.01)),
                  theta2 = list(prior="pc", param=c(0.5, 0.5)))

# hyperpriors for model grouping (iid / ar1) if used
# group.control.iid = list(model='iid', hyper = list(prec = list(prior='pc.prec',param=c(1, 0.01))))
# group.control.ar1 = list(model='ar1', hyper = list(theta1 = list(prior='pc.prec', param=c(1, 0.01)), rho = list(prior='pc.cor0', param = c(0.5, 0.75))))

# rw1/rw2 model: three levels of constraint on precision parameter 
# (puts more or less prior probability density on more or less wiggly)
hyper1.rw = list(prec = list(prior='pc.prec', param=c(0.1, 0.01))) # strictest smoothing; sd constrained to be low
hyper2.rw = list(prec = list(prior='pc.prec', param=c(0.3, 0.01))) # medium
hyper3.rw = list(prec = list(prior='pc.prec', param=c(1, 0.01))) # weaker (suggested INLA default) 
hyper4.rw = list(prec = list(prior='pc.prec', param=c(2, 0.01))) # weakest; sd can be quite wide 
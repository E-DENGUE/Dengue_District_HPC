## Heatmap code from Rory Gibb: https://github.com/rorygibb/dengue_vietnam_ms/blob/main/scripts_example/04_viz_trends.R
#map code from https://rpubs.com/nguyet/647153
#DTW code from Deus Thindwa https://github.com/deusthindwa/rsv.rebound.normal.seasonality.global/blob/main/script/12_dynTimeWarp.R

library(raster)
library(Hmisc)
library(ggdendro)
library(dplyr)
library(ggplot2)
library(dtwclust)
library(sf)
d1 <- readRDS('./Data/CONFIDENTIAL/full_climate_model.rds') %>%
  dplyr::select(-geometry) %>%
  mutate(date = paste(year, month, '01', sept='-'),
         year = as.factor(year)) %>%
  filter(VARNAME_2 != "Kien Hai", 
         VARNAME_2 != "Phu Quoc") %>%
  distinct(year, month, VARNAME_2, NAME_1, ENGTYPE_2, .keep_all = T) %>%
  arrange(month, year, VARNAME_2)%>%
  ungroup() %>%
  dplyr::select(year, month, DHF_incidence,district,m_DHF_cases,pop,avg_daily_temp,mean_ppt, avg_min_daily_temp, avg_max_daily_temp,avg_daily_humid, monthly_cum_ppt,
                mean_daily_temp,avg_min_daily_temp,avg_max_daily_temp,mean_max_temp,mean_min_temp,mean_humid) %>%
  ungroup() %>%
  arrange(district, year, month) %>%
  group_by(district) %>%
  mutate(date= as.Date(paste(year,month, '01',sep='-'), '%Y-%m-%d'),
         m_DHF_cases = if_else(!is.na(pop) & is.na(m_DHF_cases),0, m_DHF_cases ) ,
         log_Inc2 = log((m_DHF_cases+1)/pop),
         first_date=min(date),
         last_date =max(date),
  ) %>%
  ungroup() %>%
  filter(!is.na(district) &first_date==as.Date('2001-01-01') & last_date=='2018-12-01')  #filter out regions with partial time series

####HEATMAP

d1 %>%
ggplot() + 
  geom_tile(aes(x = date, y=district, fill=log_Inc2), width=31) + 
  scale_fill_gradientn(colors=viridisLite::turbo(200), na.value="grey60", name="Dengue\nincidence\n(log)") +
  theme_minimal() + 
  theme(axis.text.y = element_text(size=6), 
        axis.text.x = element_text(size=13), 
        panel.grid = element_blank(),
        legend.title = element_text(size=13), 
        legend.text = element_text(size=12.5),
        strip.text = element_text(size=14),
        axis.title = element_text(size=14), 
        axis.ticks.y = element_blank()) +
  xlab("Month") + ylab("District") 


#DTW 
#hierarchical clustering with dynamic time-warping (DTW)

d1.c <- d1 %>%
  reshape2::dcast(., date ~ district, value.var='log_Inc2') %>%
  dplyr::select_if(~ !any(is.na(.))) %>%
  dplyr::select(-date) %>%
  t()

dtw_hc <- dtwclust::tsclust(d1.c,
                            type = "hierarchical",
                            k = 2,
                            distance = "dtw_basic",
                            control = hierarchical_control(method = "average"),
                            args = tsclust_args(dist = list(window.size = 12), cent = dba)
)

clusters = cbind.data.frame('district'=row.names(d1.c), 'cluster'=dtw_hc@cluster)

d2 <- d1 %>%
  left_join(clusters, by='district') %>%
  arrange(cluster, district) %>%
  group_by(district) %>%
  mutate(district_order=cur_group_id()) %>%
  ungroup()

d2 %>%
  ggplot() + 
  geom_tile(aes(x = date, y=district_order, fill=log_Inc2), width=31) + 
  scale_fill_gradientn(colors=viridisLite::turbo(200), na.value="grey60", name="Dengue\nincidence\n(log)") +
  theme_minimal() + 
  theme(axis.text.y = element_text(size=6), 
        axis.text.x = element_text(size=13), 
        panel.grid = element_blank(),
        legend.title = element_text(size=13), 
        legend.text = element_text(size=12.5),
        strip.text = element_text(size=14),
        axis.title = element_text(size=14), 
        axis.ticks.y = element_blank()) +
  xlab("Month") + ylab("District") 


#plot ggdendrogram to show hierarchical clustering
labs <- ggdendro::label(dendro_data(as.dendrogram(dtw_hc))) %>%
  left_join(clusters, by=c('label'='district'))


B <-
  ggdendro::ggdendrogram(dtw_hc) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  labs(title = "(A)", x = "Time series hierarchical clustering", y = "Height") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  geom_point(data = labs, aes(x = x, y = 0, colour = cluster), size = 4) +
  theme(legend.position = c(0.9,0.8))
B


#extract data on clusters and their prototypes
hc_members <- as.data.frame((ggplot_build(plot(dtw_hc, type = "series", clus = c(1L:2L)))[["data"]])) %>%
  rename(panel_grp=PANEL)
hc_centroid <- as.data.frame((ggplot_build(plot(dtw_hc, type = "centroids", clus = c(1L:2L), lty=1))[["data"]]))

ggplot(hc_members, aes(x=x ,y=exp(y), group=group))+
  geom_line(alpha=0.1) +
  facet_wrap(~panel_grp) +
  theme_classic() +
  ylab('Group prototype')

ggplot(hc_centroid, aes(x=x ,y=exp(y), group=group, color=group))+
  geom_line() +
 # facet_wrap(~group) +
  theme_classic() +
  ylab('Group prototype')

###MAPING
## Get the Vietnam data – district level
##vnm = getData("GADM", country="Vietnam", level=2)

# Clean up data
#vn = fortify(vnm, regions="VARNAME_2")
#saveRDS(vn,'./Data/district_boundaries.rds')
vn <- readRDS('./Data/district_boundaries.rds')

vnm@data$id <- rownames(vnm@data)

vn     <- fortify(vnm) %>%
  left_join(vnm@data, by='id' ) %>%
  left_join(clusters, by=c('VARNAME_2'='district')) #%>%
 # filter(!is.na(cluster))

ggplot(data=vn, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=cluster), col="grey30", show.legend=F) +
  theme_minimal() +
  ylim(8,12)

#something not right--these provinces not in MDR but are still in there: 
#"Bắc Kạn    Bà Rịa - Vũng Tàu   "Bình Phước"       Hà Nội"  "Phú Thọ"  "Tây Ninh"    "Thừa Thiên Huế"    "Thanh Hóa"

#3Map code from Gibb 
# ================= key objects =================

# districts to be excluded (offshore)
# only ones with substantial dengue cases are in Kien Giang; could be worth exploring including them but thisis sufficient for now
offshore_areas = c(70154, 70339, 70273, 70355, 70698)

# districts shapefile for Vietnam
shp = st_read("./Data/shapefiles/dengue_districts_shapefile.shp") %>%
  dplyr::filter(!areaid %in% offshore_areas)

shp = cbind(shp, as.data.frame(st_coordinates(st_centroid(shp))) %>% dplyr::rename("longitude"=1, "latitude"=2))
shp = left_join(shp, clusters, by=c('areanameen'='district'))

p4 = ggplot() + 
  geom_sf(data=shp, aes(fill=cluster)) + 
  #maptheme +
  theme(legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        strip.text = element_text(size=13),
        legend.position="right")+
  theme_minimal()
p4

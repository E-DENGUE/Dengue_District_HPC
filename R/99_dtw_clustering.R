#DTW code from Deus Thindwa https://github.com/deusthindwa/rsv.rebound.normal.seasonality.global/blob/main/script/12_dynTimeWarp.R

library(raster)
library(Hmisc)
library(ggdendro)
library(dplyr)
library(ggplot2)
library(dtwclust)
library(cluster)
library(sf)
d2 <- readRDS('./Data/CONFIDENTIAL/full_data_with_new_boundaries_all_factors_cleaned.rds')

d1 <- d2%>% mutate(  log_df_rate = log((m_DHF_cases +1 ) / d2$pop * 100000))


d1$log_df_rate <- ifelse(is.na(d1$log_df_rate),
                         0, # Replace NA values with zero
                         d1$log_df_rate)



#Plot  dengue incidence rates across districts and time periods
#showing the variation of dengue incidence rates across districts and time periods
d1 %>%
  ggplot() + 
  geom_tile(aes(x = date, y=district, fill=log_df_rate), width=31) + 
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
  reshape2::dcast(., date ~ district, value.var='log_df_rate') %>%
  dplyr::select_if(~ !any(is.na(.))) %>%
  dplyr::select(-date) %>%
  t()


# Initialize variables to store silhouette scores
sil_scores <- numeric()

# Define range of k values to test
k_values <- 2:10  # You can adjust this range based on your data and computational resources

# Loop through different k values
for (k in k_values) {
  dtw_hc <- tsclust(d1.c,
                    type = "hierarchical",
                    k = k,
                    distance = "dtw_basic",
                    control = hierarchical_control(method = "average"),
                    args = tsclust_args(dist = list(window.size = 12), cent = dba)
  )
  
  dtw_dist_matrix <- proxy::dist(d1.c, method = "dtw")
  
  silhouette_result <- silhouette(dtw_hc@cluster, dtw_dist_matrix)
  sil_scores[k - 1]<- mean(silhouette_result[,3])
  
}

# Plot silhouette scores
plot(k_values, sil_scores, type = "b", xlab = "Number of clusters (k)", ylab = "Average silhouette width")


dtw_hc <- dtwclust::tsclust(d1.c,
                            type = "hierarchical",
                            k = 3,
                            distance = "dtw_basic",
                            control = hierarchical_control(method = "average"),
                            args = tsclust_args(dist = list(window.size = 12), cent = dba)
)

clusters = cbind.data.frame('district'=row.names(d1.c), 'cluster'=dtw_hc@cluster)

merged_data <- left_join(d1, clusters, by = c("district" = "district"))

# Replace NA values in the 'cluster' column with 1
merged_data$cluster[is.na(merged_data$cluster)] <- 1

d2 <- d1 %>%
  left_join(clusters, by='district') %>%
  arrange(cluster, district) %>%
  group_by(district) %>%
  mutate(district_order=cur_group_id()) %>%
  ungroup()

#plot ggdendrogram to show hierarchical clustering
labs <- ggdendro::label(dendro_data(as.dendrogram(dtw_hc))) %>%
  left_join(clusters, by=c('label'='district'))


B <-
  ggdendro::ggdendrogram(dtw_hc) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 2)) +
  labs(title = "(A)", x = "Time series hierarchical clustering", y = "Height") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  geom_point(data = labs, aes(x = x, y = 0, colour = cluster), size = 4) +
  theme(legend.position.inside = c(0.9,0.8))
B


#extract data on clusters and their prototypes
hc_members <- as.data.frame((ggplot_build(plot(dtw_hc, type = "series", clus = c(1L:3L)))[["data"]])) %>%
  dplyr::rename(panel_grp=PANEL)

hc_centroid <- as.data.frame((ggplot_build(plot(dtw_hc, type = "centroids", clus = c(1L:3L), lty=1))[["data"]]))


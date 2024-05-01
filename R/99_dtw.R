library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(shinydashboard)
library(lubridate)
library(patchwork)
library(plotly)
library(RColorBrewer)
library(shinydashboard)
library(mgcv)
library(tidyverse)
library(dtwclust)
library(ggdendro)
a1 <- readRDS('./Data/full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  arrange(district, date) %>%
  filter(date>='2004-07-01' & date <'2012-01-01') %>%
  mutate(seqwk = interval(min(date), date) %/% months(1))

#based on dtw code from Deus thindwa https://github.com/deusthindwa/rsv.rebound.normal.seasonality.global/blob/main/script/11_dtw.R
#split the dataset by country to form list of datasets
X= split(a1, a1$district)


#create empty list to store GAM models & fitted time series data for each country
Gmodels <- list()
DsTs <- list()
DsLog <- list()
Dshc <- list()

#run the GAM models where high number of knots are automatically selected via cross validation
for (i in names(X)) {
  Gmodels[[i]] <- gam(m_DHF_cases ~ s(x = seqwk, bs = "ps", k = 40),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
}

#iterate for each country, extract fitted case values
for (i in names(X)){
  DsTs[[i]] = data.frame(fitcases = (Gmodels[[i]]$fitted.values)) %>% 
    dplyr::mutate(seqwk = X[[i]]$seqwk,
                  datex = X[[i]]$date)
}

#create a list for hierarchical clustering
Dshc <- dplyr::bind_rows(DsTs, .id = "country") 
Dshc <- Dshc %>% spread(country, fitcases) %>% dplyr::select(everything(), -seqwk, -datex)
Dshc <- as.list(Dshc)

#====================================================================
#DYNAMIC TIME WARPING (DTW)
#====================================================================

#hierarchical clustering with dynamic time-warping (DTW)
dtw_hc <- dtwclust::tsclust(Dshc,
                            type = "hierarchical",
                            k = 5L,
                            preproc = zscore,
                            distance = "dtw_basic",
                            control = hierarchical_control(method = "average"),
                            trace = TRUE,
                            args = tsclust_args(dist = list(window.size = 74L), cent = dba)
)

#extract data on clusters and their prototypes
hc_members <- as.data.frame((ggplot_build(plot(dtw_hc, type = "series", clus = c(1L:9L)))[["data"]]))
hc_centroid <- as.data.frame((ggplot_build(plot(dtw_hc, type = "centroids", clus = c(1L:9L), lty=1))[["data"]]))

#plot ggdendrogram to show hierarchical clustering
labs <- label(dendro_data(as.dendrogram(dtw_hc)))
#labs$Cluster <- c(rep("4", 3), rep("2", 3), rep("1", 7), rep("3", 13))


  ggdendro::ggdendrogram(dtw_hc) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  labs(title = "(A)", x = "Time series hierarchical clustering", y = "Height") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
#  geom_point(data = labs, aes(x = x, y = 0, colour = Cluster), size = 4) +
  theme(legend.position = c(0.9,0.8))

#plot prototypes
dtwclustDS <-
  dplyr::rows_append(
    hc_members %>%
      dplyr::mutate(cat = "Cluster members") %>%
      dplyr::select(group, colour, x, y, PANEL, cat) %>%
      dplyr::rename("Cluster" = "PANEL"),
    
    hc_centroid %>% 
      dplyr::mutate(cat = "Prototypes", "group" = NA) %>%
      dplyr::select(group, colour, x, y, PANEL, cat) %>%
      dplyr::rename("Cluster" = "PANEL")) %>%
  
  mutate(Cluster = if_else(Cluster == 1, " Cluster 1",
                           if_else(Cluster == 2, "Cluster 2",
                                   if_else(Cluster == 3, "Cluster 3",
                                           if_else(Cluster == 4, "Cluster 4", NA_character_)))))

C <-
  dtwclustDS %>%
  dplyr::filter(cat == "Cluster members") %>%
  ggplot() +
  geom_line(aes(x = x, y = y, color = colour), size = 2) +
  facet_grid(cat ~ Cluster) +
  scale_colour_grey(start = 0.1, end = 0.8) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(axis.text.x = element_blank()) + 
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(strip.text.y = element_text(size = 18), strip.text.x = element_text(size = 18), strip.background = element_rect(fill="white")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(x = "", y = "z-normalised", title = "(B)")

D <-
  dtwclustDS %>%
  dplyr::filter(cat == "Prototypes") %>%
  ggplot() +
  geom_line(aes(x = x, y = y, color = Cluster), size = 2) +
  facet_grid(cat ~ Cluster) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(strip.text.y = element_text(size = 18), strip.text.x = element_text(size = 0), strip.background = element_rect(fill="white")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  labs(x = "Weeks since RSV epidemics in 2017", y = "z-normalised", title = "")

C
D
distmat <- dtw_hc@distmat
sim.mat <- 1-distmat/max(distmat) #similarity matrix
saveRDS(sim.mat,'./Data/tsclust_simmat.rds')

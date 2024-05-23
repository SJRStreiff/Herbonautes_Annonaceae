####################################################################################################
# Herbonautes part 2. Herbarium data: Herbonautes                                                  #
# Detail: Spatial comparisons and plots                                                            #
# Author: SJRS                                                                                     #
# Date: 2023-02-23                                                                                 #    
#                                                                                                  #
# ------------------------------------------------------------------------------------------------ #
# - produce gridded data (global scale, 1 deg resolution)                                          #    
# - Plot grids: species/specimens/collectors...                                                    #  
# - differences (spatial) between datasets                                                         # 
####################################################################################################

rm(list = ls()) 
`%notin%` <- Negate(`%in%`)

pac <- c('terra',
         'ggplot2',
         'tidyverse',
         'sf')
sapply(pac, require, character.only = TRUE)

# source('~/Sync/R_R-utils/plotting_utils.R')

wd <- '~/Sync/0_Papers/2024_Herbonautes_Adansonia/Supp_Inf_GH/' # working directory
setwd(wd)


# read data -- load objects from step 1
load('./data/P1_objects.Rdata')

P_spatial <- P_out_cln
G_spatial <- GBIF_out_cln

# world map data
mapplotdat <- map_data('world')

####################################################################################################
#####---------------------- Produce gridded data for different aspects ------------------###########

# known bug in sf, set this
sf_use_s2(FALSE)

#----------------------------------------------#
# run once, takes a couple of minutes.
# load objects saved from below
      # # make density grids
      # # number of specimens per grid
      # dens_grid_P <- Glob_Grid %>% st_join(P_spatial) %>%
      #   group_by(cellid, specimen...code) %>%
      #   tally() %>%
      #   filter(!is.na(specimen...code)) %>%
      #   ungroup() %>%
      #   mutate(overlap = ifelse(!is.na(specimen...code), 1, 0)) %>%
      #   group_by(cellid) %>%
      #   summarize(num_species = sum(overlap))
      # # number of species per grid
      # spp_grid_P <- Glob_Grid %>% st_join(P_spatial) %>%
      #   group_by(cellid, accepted_name) %>%
      #   tally() %>%
      #   filter(!is.na(accepted_name)) %>%
      #   ungroup() %>%
      #   mutate(overlap = ifelse(!is.na(accepted_name), 1, 0)) %>%
      #   group_by(cellid) %>%
      #   summarize(num_species = sum(overlap))
      # # number of collectors per grid
      # collector_grid_P <- Glob_Grid %>% st_join(P_spatial) %>%
      #   group_by(cellid, collector...collector) %>%
      #   tally() %>%
      #   filter(!is.na(collector...collector)) %>%
      #   ungroup() %>%
      #   mutate(overlap = ifelse(!is.na(collector...collector), 1, 0)) %>%
      #   group_by(cellid) %>%
      #   summarize(colSum = sum(overlap))
      # # GBIF
      # # number of specimens per grid
      # dens_grid_G <- Glob_Grid %>% st_join(G_spatial) %>%
      #   group_by(cellid, catalogNumber) %>%
      #   tally() %>%
      #   filter(!is.na(catalogNumber)) %>%
      #   ungroup() %>%
      #   mutate(overlap = ifelse(!is.na(catalogNumber), 1, 0)) %>%
      #   group_by(cellid) %>%
      #   summarize(num_species = sum(overlap))
      # # number of collectors per grid
      # collector_grid_G <- Glob_Grid %>% st_join(G_spatial) %>%
      #   group_by(cellid, recordedBy) %>%
      #   tally() %>%
      #   filter(!is.na(recordedBy)) %>%
      #   ungroup() %>%
      #   mutate(overlap = ifelse(!is.na(recordedBy), 1, 0)) %>%
      #   group_by(cellid) %>%
      #   summarize(colSum = sum(overlap))
      # save and or load grids to save time
      # save(dens_grid_G, dens_grid_P, spp_grid_P, collector_grid_P, file = 'P_grids.Rdata')
#----------------------------------------------#
load('data/P_grids.Rdata')


# get collection "hotspots"
max_dens_P <- dens_grid_P[dens_grid_P$num_species > 150,]


####################################################################################################
######-------------------------- Plotting preparation ------------------------------------##########

# just some themes to use in ggplot afterwards

dark_theme <- theme(
                plot.background = element_rect(fill = "black"),
                panel.background = element_rect(fill = "black"),
                panel.grid = element_blank(),
                line = element_blank(),
                rect = element_blank(),
                axis.text.x = element_text(size = 7, colour = 'white'),
                title = element_text(colour = 'white'),
                axis.text.y = element_text(size = 7, colour = 'white'),
                legend.title=element_text(size=8, colour = 'white'),
                legend.text=element_text(size=8, colour = 'white'))

# light ==> for white background
light_theme <- theme(
                  plot.background = element_rect(fill = "white"),
                  panel.background = element_rect(fill = "white"),
                  #panel.grid = element_blank(fill = 'black'),
                  line = element_blank(),
                  rect = element_blank(),
                  axis.text.x = element_text(size = 7, colour = 'black'),
                  title = element_text(colour = 'black'),
                  axis.text.y = element_text(size = 7, colour = 'black'),
                  legend.title=element_text(size=8, colour = 'black'),
                  legend.text=element_text(size=8, colour = 'black'))

colours <- viridis::magma(3, end = 0.9)
cols2 <- viridis::mako(3, end = 0.9)

####################################################################################################
######-------------------------------- Plot gridded data -------------------------------------######

# P grid plot SPECIES
plot_spp_grid_P <-  ggplot(spp_grid_P) +
  geom_map(data = mapplotdat, map = mapplotdat,
          aes(long, lat, map_id = region),
          color = "black", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         trans = 'log10', aesthetics = 'fill') +
  scale_x_continuous(breaks = waiver()) +
  coord_sf(xlim = c(-150, 180), ylim = c(-50, 50)) +
  #dark_theme + 
  light_theme +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
        # legend.justification=c(1,1), legend.position=c(1,1))+
  labs(x='', y = '',
       title = '', colour = 'black',
       fill='Species',  colour = 'black')
plot_spp_grid_P
# ggsave(filename = '0_plots/P_species_perGrid.eps', device = 'eps', width = 8, height = 4, units = 'in')

# P grid plot SPECIMENS
plot_specimen_grid_P <-
  ggplot(dens_grid_P) +
  geom_map(data = mapplotdat, map = mapplotdat,
          aes(long, lat, map_id = region),
          color = "black", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         trans = 'log10', aesthetics = 'fill') +
  scale_x_continuous(breaks = waiver()) +
  coord_sf(xlim = c(-150, 180), ylim = c(-50, 50)) +
  #dark_theme + 
  light_theme +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
        # legend.justification=c(1,1), legend.position=c(1,1))+
  labs(x='', y = '',
       title = '', colour = 'black',
       fill='Specimens',  colour = 'black')
plot_specimen_grid_P
# ggsave(filename = '0_plots/P_specimens_perGrid.eps', device = 'eps', width = 8, height = 4, units = 'in')

# P grid plot HIGH COLLECTION AREAS (>150 per pixel) 
# not very lisible and not used
plot_max_grid_P <-
  ggplot(max_dens_P) +
  geom_map(data = mapplotdat, map = mapplotdat,
          aes(long, lat, map_id = region),
          color = "black", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  coord_sf(xlim = c(-150, 180), ylim = c(-50, 50)) +
  #dark_theme + 
  light_theme +
  labs(x='', y = '',
       title = 'P maximum collections per grid', colour = 'black',
       colour = 'black')
plot_max_grid_P

# P grid plot COLLECTORS/BOTANISTS
col_grid_P <-
  ggplot(collector_grid_P) +
  geom_map(data = mapplotdat, map = mapplotdat,
          aes(long, lat, map_id = region),
          color = "black", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
  geom_sf(aes(fill = colSum), color = NA) +
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         trans = 'log10', aesthetics = 'fill') +
  coord_sf(xlim = c(-150, 180), ylim = c(-50, 50)) +
  light_theme +  
  # theme(legend.justification=c(1,1), legend.position=c(1,1))+
  labs(x='',y='', colour = 'white',
       title = '', colour = 'white',
       fill='Collectors',  colour = 'white')

col_grid_P
# ggsave(filename = '0_plots/P_collectors_perGrid.eps', device = 'eps', width = 17.5, height = 18, units = 'cm')

library(patchwork)
fig2 <- plot_specimen_grid_P / plot_spp_grid_P / col_grid_P
ggsave(fig2, filename = '0_plots/fig2.eps', device = 'eps', width = 17.5, height = 18, units = 'cm')

# GBIF grid plot SPECIES
grid_G <-ggplot(dens_grid_G) +
  geom_map(data = mapplotdat, map = mapplotdat,
          aes(long, lat, map_id = region),
          color = "lightgrey", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         trans = 'log10', aesthetics = 'fill') +
  coord_sf(xlim = c(-100, 150), ylim = c(-50, 50)) +
  light_theme +  
  labs(x='',y='', colour = 'white',
       title = 'G', colour = 'white',
       fill='Number of specimens',  colour = 'white')
grid_G
# ggsave(filename = '0_plots/G_specimens_perGrid.png', device = 'png', width = 8, height = 4, units = 'in')





####################################################################################################
######------------------- calculate overlap/differences between datasets ----------------------#####

# just to make sure, presence data (as opposed to number of specimens per pixel, here 1 or 0)
pres_P <- dens_grid_P %>% transmute(presence = ifelse(num_species >= 1, 1, 0)) 
pres_G <- dens_grid_G %>% transmute(presence = ifelse(num_species >= 1, 1, 0)) 

dens_only_P <- dens_grid_P[dens_grid_P$geometry %notin% dens_grid_G$geometry, ]
only_P_specimens <- sum(dens_only_P$num_species)

# get the difference between GBIF and P spatially
diff_vector <- which(pres_P$geometry %notin% pres_G$geometry)
P_no_GBIF <- pres_P[diff_vector,]
P_no_GBIF$data <- 'P'
only_GBIF <- pres_G[which(pres_G$geometry %notin% pres_P$geometry),]
only_GBIF$data <- 'GBIF'
both_GP   <- pres_G[which(pres_G$geometry %in% pres_P$geometry),]
both_GP$data <- 'P & GBIF'
presences <- rbind(P_no_GBIF, only_GBIF, both_GP) # spatial dataframe with presence and data source per pixel

# Plot the pixels where P data but no GBIF records
grid_diff_pres <-
  ggplot() +
  geom_map(data = mapplotdat, map = mapplotdat,
          aes(long, lat, map_id = region),
          color = "black", fill = "darkgrey", linewidth = 0.1) +
  geom_sf(data = presences, aes(fill = data), colour = 'transparent' ) +
  #viridis::scale_fill_viridis(option = 'viridis',begin = 0.05, end = 0.7, discrete = TRUE) +
  viridis::scale_fill_viridis(option = 'rocket',discrete = TRUE, begin = 0.1, end = 0.9) + #altenative colour scale
  scale_x_continuous(breaks = waiver()) +
  coord_sf(xlim = c(-120, 170), ylim = c(-50, 50)) +
  light_theme +  
  #dark_theme+
  labs(x='',y='',colour = 'white',
       title = '', colour = 'white',
       fill='Data source', colour = 'white')
grid_diff_pres
ggsave(filename = '~/Sync/0_Papers/2024_Herbonautes_Adansonia/FIgures_FINAL/Fig4.eps', device = 'eps', width = 8, height = 4, units = 'in')


# size of datasets (in particular new data/species)
length(dens_grid_G$cellid)
length(P_no_GBIF$presence)

####################################################################################################
#---------------------- only plot 'new' species ---------------------------------------------------#
# just used to explore

new_sp_P_sf <- P_spatial[P_spatial$accepted_name %notin% G_spatial$accepted_name,]

# P Specimen grid 
plot_new_spp_P <-
  ggplot(new_sp_P_sf) +
  geom_map(data = mapplotdat, map = mapplotdat,
           aes(long, lat, map_id = region),
           color = "black", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
  geom_sf(aes(fill = new_sp_P_sf), fill = 'red') +
  #scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
   #                      trans = 'log10', aesthetics = 'fill') +
  #scale_x_continuous(breaks = waiver()) +
  coord_sf(xlim = c(-100, 150), ylim = c(-50, 50)) +
  #dark_theme + 
  light_theme +
  labs(x='', y = '',
       title = 'P', colour = 'black',
       fill='Number of specimens \n per 1x1 degree square',  colour = 'black')
plot_new_spp_P




####################################################################################################
#----------------------  'new' species endemism ---------------------------------------------------#
# for checklist please contact authors.


# 
# CL <- read.csv('~/Sync/1_Annonaceae/0_GLOBAL/0_Annonaceae_CL/CL_crossrefd/CL_crossrefd_20230623_W_distr.csv',
#                sep = ';', head = T)
# CL_new <- CL[CL$accepted_name %in% new_sp_P_sf$accepted_name,]
# a quick visual inspection of the CL_new dataframe confirms that most species 'new' are rather restricted in 
# distribution










#


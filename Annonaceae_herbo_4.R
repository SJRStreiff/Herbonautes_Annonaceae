####################################################################################################
# Herbonautes part 4. Herbonautes and their impact on our                                          #
#            knowledge of biodiversity                                                             #
# Detail: Compare Hebonautes on smaller scale to GBIF AND Malagsy expert                           #
#         data only on Madagascar                                                                  #
# Author: SJRS                                                                                     #
# Date: 2023-10-18                                                                                 #
# ------------------------------------------------------------------------------------------------ #
# - Madagascar grid create                                                                         #
# - plot spatial data Madagascar                                                                   #
#                                                                                                  #
####################################################################################################

####################################################################################################
# !! For access to the species checklist and the Madagascar expert database, please contact
# the authors! (in prep, when published, link will be inserted)
# All code requiring this data is commented out in this version
####################################################################################################


rm(list = ls())

pac <- c('terra',
         'ggplot2',
         'tidyverse',
         'sf',
         'rworldxtra',
         'maps')
sapply(pac, require, character.only = TRUE)
`%notin%` <- Negate(`%in%`)

wd <- '~/Sync/0_Papers/2024_Herbonautes_Adansonia/Supp_Inf_GH/' # working directory
setwd(wd)

####################################################################################################
###------------------------------------------ load and read data --------------------------------###
load('./data/P1_objects.rdata')
#load('P_grids.Rdata')

data("countriesHigh")
mapdat    <- sf::st_as_sf(countriesHigh)
mapplotdat <- map_data('world')


####################################################################################################
# Checklist and Madagascar dataset available upon request from the authors
####################################################################################################
CL <- read.csv(file = '~/Sync/1_Annonaceae/0_GLOBAL/0_Annonaceae_CL/CL_crossrefd/CL_crossrefd_20230623_W_distr.csv', header = T, sep = ';')
ipni_dat <- CL[, c("ipni_pub","accepted_name", 'POWO_native_to')]

# read Madagascar data
Mada_dat <- read.csv(file = '~/Sync/1_Annonaceae/Y_DATA/3_distribution_data/M_madagascar/20231106_Mada_upd.csv', sep = ';')
Mada_s_str <- Mada_dat[Mada_dat$country == 'Madagascar',] # as we have some occurrences on surrounding islands
Mada_s_str <- Mada_s_str[Mada_s_str$cult_intro == 'NO',] # remove introduced species (doesn't change results)

# get species level determined subset
Mada_dat_detsp <- Mada_s_str[Mada_s_str$specific_epithet != '',]
print(paste('Of', length(Mada_dat$RecordedBy), 'records in the Madagascar dataset,',
            length(Mada_s_str$RecordedBy), 'records on Madagascar (s.str., i.e. not Mascarene islands),',
            length(Mada_dat_detsp$RecordedBy), 'records are determined to spp-level'))
Mada_dat_detsp$species <- paste(Mada_dat_detsp$genus, Mada_dat_detsp$specific_epithet)
# get all spp in this dataset
sp_Mada <- unique(Mada_dat_detsp$species)
# crossreference with POWO, assuming some discrepancies?
#write.table(sp_Mada, '00_Mada_check_taxa.csv', sep = ';', row.names = F)
sp_Mada_checked <- read.csv('~/Sync/1_Annonaceae/P_herbonautes/_OUT01_Mada_check_taxa.csv', sep =';', head=T)

# merge checked names. I know there are some spp in MADA which are very new and not yet on POWO, but so that we can compare
# between the datasets on a fairly comparablke level, we take the standardised somewhat outdated taxonomy
Mada_good_nom <- merge(Mada_dat_detsp, sp_Mada_checked)
Mada_s_str <- Mada_good_nom[Mada_good_nom$country == 'Madagascar',]
Mada_s_str <- Mada_s_str[Mada_s_str$status %in% c('ACCEPTED', 'UNPLACED', 'SYNONYM'),]

# the final/standardised tally of spp
sp_Mada <- unique(Mada_s_str$accepted_name)
Mada_dat_detsp <- Mada_s_str
####################################################################################################


# and get P and GBIF comparinson
P_mada <- P_out_cln[P_out_cln$country...country == 'Madagascar',]
# G has really badly referenced countries!
# G_mada <- GBIF_good_nom[GBIF_good_nom$countryCode == 'MG',]
# therefore get country from coordinates!
GBIF_good_nom$coord_country <- map.where(database="world", GBIF_good_nom$decimalLongitude, GBIF_good_nom$decimalLatitude)
G_mada <- GBIF_good_nom[GBIF_good_nom$coord_country == 'Madagascar',]
unique(G_mada$countryCode)

# get unique species
P_sp_mada <- unique(P_mada$accepted_name)
G_sp_mada <- unique(G_mada$accepted_name)
# print(paste('We have so many species: Expert:', length(sp_Mada), '; Paris:', length(P_sp_mada), '; GBIF:', length(G_sp_mada)))

# count species and compare
# mada_P_match <- P_sp_mada[P_sp_mada %in% sp_Mada]
# mada_P_noEXP <- P_sp_mada[P_sp_mada %notin% sp_Mada]
# mada_noP <- sp_Mada[sp_Mada %notin% P_sp_mada]
# #overlap
# mada_G_match <- G_sp_mada[G_sp_mada %in% sp_Mada]
# mada_noG <- sp_Mada[sp_Mada %notin% G_sp_mada]
# 
# # species missing according to CL
# P_missing_CL <- CL[CL$accepted_name %in% mada_noP,]
# print(paste('Species not in P dataset are of this age (median)',median(P_missing_CL$ipni_pub, na.rm = T), '(N=',length(mada_noP), 'species)'))
# 
# G_missing_CL <- CL[CL$accepted_name %in% mada_noG,]
# print(paste('Species not in GBIF dataset are of this age (median)',median(G_missing_CL$ipni_pub, na.rm = T), '(N=',length(mada_noG), 'species)'))


####################################################################################################
######------------------------------------ Spatialise expert dat ---------------------------########
# 
Mada_sf <- Mada_dat_detsp[!is.na(Mada_dat_detsp$ddlat),]
Mada_sf <- Mada_sf %>% st_as_sf(coords = c('ddlong', 'ddlat')) %>%
  st_set_crs('WGS84')

# P_mada # already geometry so no need to repeat. Loaded in P_grids.Rdata
G_sf <- G_mada[!is.na(G_mada$decimalLatitude),]
G_sf <- G_sf %>% st_as_sf(coords = c('decimalLongitude', 'decimalLatitude')) %>% 
  st_set_crs('WGS84')

####################################################################################################
######------------------------------------ Grid for Madagascar -----------------------------########


resolution <- 0.5 # test different params: 1 too large (we use that on global scale ;-) 
                  #                        smaller than 0.5 too small for some coordinates 

mada_grid = st_make_grid(Mada_sf, cellsize = resolution, what = "polygons", square = TRUE)
sf::sf_use_s2(FALSE)
# To sf and add grid ID
mada_grid_sf = st_sf(mada_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(mada_grid)))

# Mada EXP
mada_EXP_grid <- mada_grid_sf %>%
  st_join(Mada_sf) %>%
  group_by(grid_id) %>%
  summarize(num_species = length(unique(accepted_name)))
# mutate(overlap = ifelse(!is.na(accepted_name), 1, 0)) %>%
# summarize(num_species = sum(overlap))
mada_EXP_grid$n_points <- lengths(st_intersects(mada_EXP_grid, Mada_sf))
mada_EXP_sf <- filter(mada_EXP_grid, n_points > 0)
maxspp_EXP <- max(mada_EXP_sf$num_species)


# Mada P
P_mada_grid <- mada_grid_sf %>%
  st_join(P_mada) %>%
  group_by(grid_id) %>%
  summarize(num_species = length(unique(accepted_name)))
# mutate(overlap = ifelse(!is.na(accepted_name), 1, 0)) %>%
# summarize(num_species = sum(overlap))
P_mada_grid$n_points <- lengths(st_intersects(P_mada_grid, P_mada))
P_mada_sf <- filter(P_mada_grid, n_points > 0)
maxspp_P <- max(P_mada_sf$num_species)


# Mada G
G_mada_grid <- mada_grid_sf %>%
  st_join(G_sf) %>%
  group_by(grid_id) %>%
  summarize(num_species = length(unique(accepted_name)))
# mutate(overlap = ifelse(!is.na(accepted_name), 1, 0)) %>%
# summarize(num_species = sum(overlap))

G_mada_grid$n_points <- lengths(st_intersects(G_mada_grid, G_sf))
G_mada_sf <- filter(G_mada_grid, n_points > 0)
maxspp_G <- max(G_mada_sf$num_species)



####################################################################################################
######-------------------------- Plotting preparation ------------------------------------##########

# dark ==> black background, white writing, for in presentations with black backgrounds
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

# bounding box for plotting
bbox_mada <- coord_sf(xlim = c(42,51), ylim = c(-25, -10)) 

colours <- viridis::rocket(3, end = 0.9)
cols2 <- viridis::mako(3, end = 0.9)

####################################################################################################
######-------------------------------- Plot gridded data -------------------------------------######
breaks <- c(1, 3, 10, 30)
limits <- c(1, 40)
# P grid plot SPECIES
p_M_P <-  ggplot(P_mada_sf) +
  geom_map(data = mapplotdat, map = mapplotdat,
           aes(long, lat, map_id = region),
           color = "black", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]),
                         trans = 'log10', aesthetics = 'fill', breaks = breaks, limits) +
  # scale_x_continuous(breaks = waiver()) +
  bbox_mada +
  # dark_theme +
  light_theme +
  theme(legend.position="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x='', y = '')
# 
# # P grid plot RECORDS
# p_M_P_records <-  ggplot(P_mada_sf) +
#   geom_map(data = mapplotdat, map = mapplotdat,
#            aes(long, lat, map_id = region),
#            color = "black", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
#   geom_sf(aes(fill = n_points), color = NA) +
#   scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]),
#                          trans = 'log10', aesthetics = 'fill', breaks = breaks, limits) +
#   # scale_x_continuous(breaks = waiver()) +
#   bbox_mada +
#   # dark_theme +
#   light_theme +
#   theme(legend.position="none")+
#   labs(x='', y = '',
#        title = '', colour = 'black')
# p_M_P_records
# 

# ggsave(filename = '0_plots/Mada_P_dens.png', p_M_P, device = 'png',
       # width = 4, height = 6)

# GBIF grid plot SPECIES
p_M_G <-  ggplot(G_mada_sf) +
  geom_map(data = mapplotdat, map = mapplotdat,
           aes(long, lat, map_id = region),
           color = "black", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]),
                         trans = 'log10', aesthetics = 'fill', breaks = breaks, limits) +
  # scale_x_continuous(breaks = waiver()) +
  bbox_mada +
  # dark_theme +
  light_theme +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x='', y = '')
# p_M_G
# ggsave(filename = '0_plots/Mada_G_dens.png', p_M_G, device = 'png',
       # width = 4, height = 6)

# EXPERT grid plot SPECIES
p_EXP <-  ggplot(mada_EXP_sf) +
  geom_map(data = mapplotdat, map = mapplotdat,
           aes(long, lat, map_id = region),
           color = "black", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]),
                         trans = 'log10', aesthetics = 'fill', breaks = breaks, limits) +
  # scale_x_continuous(breaks = waiver()) +
  bbox_mada +
  # dark_theme +
  light_theme +
  theme(legend.position="none") +
  labs(x='', y = '')
# p_EXP
# ggsave(filename = '0_plots/Mada_Exp_dens.png', p_EXP, device = 'png',
       # width = 4, height = 6)
library(patchwork)
p_out <- p_EXP + p_M_P + p_M_G

p_out
ggsave(p_out, filename = '0_plots/fig6.eps', device = 'eps', width = 17.5, height = 12, units = 'cm')

# a worldmap used as reference in the figure
refworldmap <-  ggplot() +
  geom_map(data = mapplotdat, map = mapplotdat,
           aes(long, lat, map_id = region),
           color = "black", fill = "darkgrey",alpha = 0.5, linewidth = 0.1) +
  #geom_sf(aes(fill = num_species), color = NA) +
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]),
                         trans = 'log10', aesthetics = 'fill') +
  scale_x_continuous(breaks = waiver()) +
  #bbox_mada +
  # dark_theme +
  coord_sf(xlim = c(-120, 170), ylim = c(-50, 50)) +

  annotate(geom = "rect", ymax = -10, ymin = -27, xmax = 52, xmin = 42, colour = "red", fill = NA) +
  #geom_m
  light_theme +
  labs(x='', y = '',
       title = '', colour = 'black',
       fill='',  colour = 'black')
refworldmap
ggsave(filename = '0_plots/refworldmap.eps',refworldmap, device = 'eps',
       width = 8, height = 3)
# 
# 
# ####################################################################################################
# ######------------------- calculate overlap/differences between datasets ----------------------#####
# 
# # just to make sure, presence data (as opposed to number of specimens per pixel, here 1 or 0)
# pres_P <- P_mada_sf %>% transmute(presence = ifelse(num_species >= 1, 1, 0)) 
# pres_G <- G_mada_sf %>% transmute(presence = ifelse(num_species >= 1, 1, 0)) 
# pres_E <- mada_EXP_sf %>% transmute(presence = ifelse(num_species >= 1, 1, 0)) 
# 
# # as EXP based on P, P <= EXP
# only_EXP  <- Mada_sf[Mada_sf$geometry %notin% G_sf$geometry, ]
# # get the difference between GBIF and P and EXP
# # only EXP
# d_vec <- which(pres_E$mada_grid %notin% c(pres_G$mada_grid, pres_P$mada_grid))
# E_alone <- pres_E[d_vec,]
# E_alone$data <- 'EXP'
# # EXP & P
# P_E_no_GBIF <- pres_E[pres_E$mada_grid %in% pres_P$mada_grid & pres_E$mada_grid %notin% pres_G$mada_grid,]
# P_E_no_GBIF$data <- 'EXP & P'
# # all datasets
# all_E_P_G <- pres_P[which(pres_E$mada_grid %in% c(pres_G$mada_grid, pres_P$mada_grid)),]
# all_E_P_G$data <- 'ECP & P & G'
# 
# presences <- rbind(E_alone, P_E_no_GBIF, all_E_P_G ) # spatial dataframe with presence and data source per pixel
# 
# # Plot the pixels where P data but no GBIF records and EXP but no P / G
# grid_diff_pres <-
#   ggplot() +
#   geom_map(data = mapplotdat, map = mapplotdat,
#            aes(long, lat, map_id = region),
#            color = "black", fill = "darkgrey", linewidth = 0.1) +
#   geom_sf(data = presences, aes(fill = data), colour = 'transparent' ) +
#   #viridis::scale_fill_viridis(option = 'viridis',begin = 0.05, end = 0.7, discrete = TRUE) +
#   viridis::scale_fill_viridis(option = 'cividis',discrete = TRUE, begin = 0.1, end = 0.7) + #altenative colour scale
#   scale_x_continuous(breaks = waiver()) +
#   coord_sf(xlim = c(-120, 170), ylim = c(-50, 50)) +
#   light_theme +  
#   bbox_mada +
#   #dark_theme+
#   labs(x='',y='',colour = 'white',
#        title = '', colour = 'white',
#        fill='Data source', colour = 'white')
# grid_diff_pres
# 
# ggsave(filename = '0_plots/Mada_diff_datasets.png', grid_diff_pres, device = 'png',
#        width = 3, height = 6)
# 
# 
# ####################################################################################################
# ######------------------- housekeeping for next steps -----------------------------------------#####
# 
# Exp_mada_sf <- mada_EXP_sf
# Exp_mada_grid <- mada_EXP_grid
# Exp_out_cln <- Mada_dat_detsp
# G_out_cln <- GBIF_out_cln
# pres_Exp <- pres_E
# 
# save(G_mada_sf, P_mada_sf, Exp_mada_sf, 
#      G_mada_grid, P_mada_grid, Exp_mada_grid,
#      presences, pres_P,pres_G, pres_Exp,
#      G_out_cln, P_out_cln, Exp_out_cln,
#      file = 'mada_data.Rds')
# 
# #



# we want to create histograms for # specimens per pixel per dataset and repeat for species instead of specimens
# 
# ggplot() +
#   geom_histogram(data= G_mada_sf, aes(log(n_points)), col = 'red', fill = NA) + 
#   geom_histogram(data= P_mada_sf, aes(log(n_points)), col = 'green', fill = NA) +
#   geom_histogram(data= Exp_mada_sf, aes(log(n_points)), col = 'purple', fill = NA) 
# 
# ggplot() +
#   geom_histogram(data= G_mada_grid, aes((n_points)), col = 'red', fill = NA) + 
#   geom_histogram(data= P_mada_grid, aes((n_points)), col = 'green', fill = NA) +
#   geom_histogram(data= Exp_mada_grid, aes((n_points)), col = 'purple', fill = NA) 
# 
# 
# ggplot() + 
#   geom_density(data=G_mada_sf, aes(x=n_points), col = 'red') +
#   geom_density(data=P_mada_sf, aes(x=n_points), col = 'green') +
#   geom_density(data=Exp_mada_sf, aes(x=n_points), col = 'purple') 




####################################################################################################
# Herbonautes part 3. Herbarium data: Herbonautes                                                  #
# Detail: more spatial details, regional stats, collector stats, delving into 'new' spp            #
# Author: SJRS                                                                                     #
# Date: 2023-02-23                                                                                 #    
#                                                                                                  #    
# ------------------------------------------------------------------------------------------------ #
# - Explore major collectors                                                                       #    
# - explore records per country                                                                    #
# - Total Annonaceae records per species                                                           #
####################################################################################################

rm(list = ls()) 
`%notin%` <- Negate(`%in%`)

pac <- c('terra',
         'ggplot2',
         'tidyverse',
         'gridExtra',
         'sf',
         'rworldxtra')
sapply(pac, require, character.only = TRUE)


wd <- '~/Sync/0_Papers/2023_Herbonautes/Supp_Inf_GH/' # working directory

setwd(wd)

####---------- read data
load('./data/P1_objects.rdata')
load('./data/P_grids.Rdata')

data("countriesHigh")
mapdat    <- sf::st_as_sf(countriesHigh)


# for checklist please contact the authors
# CL <- read.csv(file = '~/Sync/1_Annonaceae/0_GLOBAL/0_Annonaceae_CL/CL_crossrefd/CL_crossrefd_20230623_W_distr.csv', header = T, sep = ';')
# ipni_dat <- CL[, c("ipni_pub","accepted_name", 'POWO_native_to')]



####################################################################################################
#####------------------------ Explore collection year distribution ----------------------------#####

# format collection year properly
# once for all taxonomic good data
P_dat_nom$col_year <- sapply(P_dat_nom$collect_date...collect_date, 
                             function(x) as.numeric(str_split(str_split(unique(x), ' -')[[1]][1], 
                                                              '/')[[1]][3]))
# and once for all georeferenced data
P_out_cln$col_year <- sapply(P_out_cln$collect_date...collect_date, 
                             function(x) as.numeric(str_split(str_split(unique(x), ' -')[[1]][1],
                                                              '/')[[1]][3]))

# histogram of collections binned per 5 years 
ggplot(P_dat_nom) + 
  geom_histogram(aes(col_year), binwidth = 5, na.rm = TRUE, col= 'black', fill = NA) +
  theme_classic() +
  labs(x='Collection Year', y = 'Number of specimens / 5 years',
     title = '', colour = 'black') +
  scale_x_continuous(limits = c(1735, 2022), 
                     breaks = c(1750, 1775, 1800, 1825, 1850, 
                                1875, 1900, 1925, 1950, 1975, 2000, 2015)) 

ggsave(filename = '0_plots/col_year_distr.tiff', 
       dpi = 300, device = 'tiff', width = 8, height = 4, units = 'in')



####################################################################################################
####---------------------------------- Collectors ----------------------------------------------####

# only one Poilane i know of/find in databases
P_out_cln$collector...collector[P_out_cln$collector...collector == 'Poilane'] <- 'Poilane, E.'

colcount_P <- NULL
colcount_P <- P_out_cln %>% dplyr::count(P_out_cln$collector...collector, name=NULL)
colnames(colcount_P)[1] <- 'collector'

colcount_P <- arrange(colcount_P, desc(n))
col10 <- head(colcount_P, n = 11)
col10 <- col10[!is.na(col10$collector), ]
top10_num_specimens <- sum(col10$n)

# top ten collector grid
highcolplot <-  ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = col10, aes(colour = collector))+
  labs(title = 'Top 10 collectors (3993 specimens)')

highcolplot


# go through top ten collectors individually 
colours <- viridis::viridis(3, end = 0.9)

plot_poilane <- ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = P_out_cln[P_out_cln$collector...collector == 'Poilane, E.',], aes(colour = col_year))+
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         aesthetics = 'colour') +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  coord_sf(xlim = c(116, 93), ylim = c(6, 26)) +
  labs(title = 'E. Poilane\'s collections (1447 specimens)')
plot_poilane

pierre_dat <- P_out_cln[P_out_cln$collector...collector %in% 'Pierre, L.',]
plot_pierre <- ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = P_out_cln[P_out_cln$collector...collector %in% 'Pierre, L.',], aes(colour = col_year))+
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         aesthetics = 'colour') +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  #coord_sf(xlim = c(130, 65), ylim = c(0, 26)) +
  labs(title = 'L. Pierre\'s collections (547 specimens)')
plot_pierre

plot_chevalier <- ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = P_out_cln[P_out_cln$collector...collector %in% 'Chevalier, A.',], aes(colour = col_year))+
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         aesthetics = 'colour') +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  coord_sf(xlim = c(130, -30), ylim = c(-10, 38)) + #-48.339844,-9.275622,123.046875,38.822591
  labs(title = 'A. Chevalier\'s collections (527 specimens)')
plot_chevalier

plot_letouzey <- ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = P_out_cln[P_out_cln$collector...collector %in% 'Letouzey, R.',], aes(colour = col_year))+
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         aesthetics = 'colour') +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  coord_sf(xlim = c(25, 0), ylim = c(-5, 15)) + #-48.339844,-9.275622,123.046875,38.822591
  labs(title = 'R. Letouzey\'s collections (450 specimens)')
plot_letouzey

plot_letestu <- ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = P_out_cln[P_out_cln$collector...collector %in% 'Le Testu, G.',], aes(colour = col_year))+
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         aesthetics = 'colour') +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  coord_sf(xlim = c(25, 0), ylim = c(-5, 15)) + #-48.339844,-9.275622,123.046875,38.822591
  labs(title = 'G. Le Testu\'s collections (378 specimens)')
plot_letestu

plot_capuron <- ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = P_out_cln[P_out_cln$collector...collector %in% 'Capuron, R.',], aes(colour = col_year))+
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         aesthetics = 'colour') +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  coord_sf(xlim = c(40, 52), ylim = c(-25, -10)) + #-48.339844,-9.275622,123.046875,38.822591
  labs(title = 'R. Capuron\'s collections (274 specimens)')
plot_capuron



plot_david <- ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = P_out_cln[P_out_cln$collector...collector %in% 'David, B.',], aes(colour = col_year))+
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         aesthetics = 'colour') +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  coord_sf(xlim = c(90, 140), ylim = c(-15, 15)) + #-48.339844,-9.275622,123.046875,38.822591
  labs(title = 'B. David\'s collections (246 specimens) -- just 2 years collecting!')
plot_david

plot_elmer <- ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = P_out_cln[P_out_cln$collector...collector %in% 'Elmer, A.D.E.',], aes(colour = col_year))+
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         aesthetics = 'colour') +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  coord_sf(xlim = c(110, 130), ylim = c(0, 20)) + #-48.339844,-9.275622,123.046875,38.822591
  labs(title = 'Elmer\'s collections (186 specimens)')
plot_elmer

plot_thorel <- ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = P_out_cln[P_out_cln$collector...collector %in% 'Thorel',], aes(colour = col_year))+
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         aesthetics = 'colour') +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  coord_sf(xlim = c(116, 93), ylim = c(6, 26)) +
  labs(title = 'Thorel\'s collections (186 specimens)')
plot_thorel


plot_ramos <- ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
  labs(x='Longitude',y='Latitude') +
  geom_sf(data = P_out_cln[P_out_cln$collector...collector %in% 'Ramos, M.',], aes(colour = col_year))+
  scale_colour_gradientn(colours = c(colours[1], colours[2], colours[3]), 
                         aesthetics = 'colour') +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
  coord_sf(xlim = c(110, 130), ylim = c(0, 20)) + #-48.339844,-9.275622,123.046875,38.822591
  labs(title = 'Ramos\' collections (186 specimens)')
plot_ramos

# arrange the collector-plots
gridExtra::grid.arrange(plot_poilane, plot_pierre, plot_cheval, 
                        plot_letouzey, plot_letestu, plot_capuron,
                        plot_david, plot_elmer, plot_thorel, nrow=3)

####################################################################################################
#---------------------------- collection years per country ----------------------------------------#

P_dat_nom$col_year <- sapply(P_dat_nom$collect_date...collect_date, 
                             function(x) as.numeric(str_split(str_split(unique(x), ' -')[[1]][1],
                                                              '/')[[1]][3]))

# get min, max , median and range of collection year per country
year_col <- P_dat_nom %>% 
  group_by(country...country) %>%
  mutate(start_col = min(col_year, na.rm = TRUE)) %>%
  mutate(end_col = max(col_year, na.rm = TRUE)) %>%
  mutate(median_col = median(col_year, na.rm = TRUE)) %>%
  mutate(col_range = ifelse(!is.na(start_col) && !is.na(end_col), end_col - start_col, NA)) 
# countr number of records per country
rec_by_countr <- P_dat_nom %>% 
  count(country...country, sort = TRUE)

dat_for_recby <- P_dat_nom[!is.na(P_dat_nom$country...country),]

rec_tt_by_country <- NULL
for(i in seq(1,2*length(rec_by_countr$country...country), by=2)){
  j <- (i+1)/2
  rec_tt_by_country$country[c(i,i+1)] <- rec_by_countr$country...country[j]
  tmp <- dat_for_recby[dat_for_recby$country...country == rec_by_countr$country...country[j],]
  print(unique(tmp$country...country))
  rec_tt_by_country$in_tt_col[i] <- TRUE
  rec_tt_by_country$poilane[i] <- TRUE
  rec_tt_by_country$n[i] <-length(tmp[tmp$collector...collector %in% col10$collector,'collector...collector'])
  rec_tt_by_country$n_poilane[i] <- length(tmp[tmp$collector...collector %in% 'Poilane, E.','collector...collector'])
  rec_tt_by_country$in_tt_col[i+1] <- FALSE
  rec_tt_by_country$poilane[i+1] <- TRUE
  rec_tt_by_country$n_poilane[i+1] <-length(tmp[tmp$collector...collector %notin% 'Poilane, E.','collector...collector'])
  rec_tt_by_country$n[i+1] <-length(tmp[tmp$collector...collector %notin% col10$collector,'collector...collector'])
  }
rec_tt_by_country <- as.data.frame(rec_tt_by_country)


# some countries that are/were french colonies
afr <- c("Madagascar", "Cameroun","Gabon","Côte d'Ivoire")
asi <- c( "Viet Nam", "Malaisie", "Cambodge" , "Indonésie")
ame <- c("Guyane Française", 'Mexique', 'Guyana', 'Brésil')
curr_col <- c('Guyane Française', 'Nouvelle Caledonie', 'Guadeloupe', 'Réunion')


# vertical lines are colionisation times (colonisation/independence)
t1 <- P_dat_nom[P_dat_nom$country...country %in% afr,]
ggplot() + geom_density(data = t1,
                        aes(col_year, colour = country...country), na.rm = TRUE) +
          geom_vline(aes(xintercept = c(1885, 1960), colour = 'Gabon')) +
          geom_vline(aes(xintercept = c(1914, 1960), colour = 'Cameroun')) +
          geom_vline(aes(xintercept = c(1886, 1960), colour = "Côte d'Ivoire")) +
          geom_vline(aes(xintercept = c(1896, 1975, 1979), colour = "Madagascar")) +
            ggtitle('Africa')

t2 <- P_dat_nom[P_dat_nom$country...country %in% asi,]
ggplot() + geom_density(data = t2,
                        aes(col_year, colour = country...country), na.rm = TRUE) +
  geom_vline(aes(xintercept = c(1862, 1954), colour = 'Viet Nam')) +
  geom_vline(aes(xintercept = c(1863, 1954), colour = 'Cambodge')) +
  geom_vline(aes(xintercept = c(1826, 1957), colour = 'Malaisie')) +
  geom_vline(aes(xintercept = c(1799, 1949), colour = 'Indonésie')) +
  ggtitle('Asia')

t3 <- P_dat_nom[P_dat_nom$country...country %in% ame,]
ggplot() + geom_density(data = t3,
                        aes(col_year, colour = country...country), na.rm = TRUE) +
  ggtitle('America')

# these are still french "outre-mer" departments
t4 <- P_dat_nom[P_dat_nom$country...country %in% curr_col,]
ggplot() + geom_density(data = t4,
                        aes(col_year, colour = country...country), na.rm = TRUE) +
  ggtitle('Outre-mer today')



unique(P_out_cln$continental_reg)
gridExtra::grid.arrange(p1,p2, ncol=2)


####################################################################################################
#---------------------------- collections per country ---------------------------------------------#


# top ten countries by number of specimens
t10_n_recs <- sum(rec_by_countr$n[1:10]) # top ten countries 60% of specimens!!


toptenCountries <- head(rec_by_countr, n=10)[,1]
topthreeCountries <- head(rec_by_countr, n=3)

print(paste('The top ten countries collected from are:'))
print((toptenCountries))

country <- list()

# for all 10 countries plot collection years
for(i in toptenCountries){
  print(i)
  country[[i]] <- P_dat_nom[P_dat_nom$country...country == i,] 
  }
for(i in toptenCountries){
  dat <- rec_tt_by_country[rec_tt_by_country$country == i,]
  fileoutname <- paste('year_tt_plot_',  i)
  p1 <- ggplot(dat) + 
    geom_histogram(aes(col_year), binwidth = 5, na.rm = TRUE) +
    theme_classic() +
    labs(x='Collection Year', y = 'Number of specimens / 5 years',
         title = 'Congo', colour = 'black') +
    scale_x_continuous(limits = c(1825, 2022), breaks = c( 1825, 1850, 
                                                           1875, 1900, 1925, 1950, 1975, 2000, 2015)) 
  p1            
    
    
}
plot <-  ggplot(country[[1]]) + 
                geom_histogram(aes(col_year), binwidth = 5, na.rm = TRUE) +
                theme_classic() +
                labs(x='Collection Year', y = 'Number of specimens / 5 years',
                     title = 'Mada', colour = 'black') +
                scale_x_continuous(limits = c(1825, 2022), breaks = c( 1825, 1850, 
                                                                      1875, 1900, 1925, 1950, 1975, 2000, 2015)) 
plot              

year_col <- year_col[,c("start_col", "end_col", "median_col", "col_range", "country...country")]
year_col <- unique(year_col)


# write.table(year_col, file = 'collection_years_by_country.tsv', row.names = F, sep = '\t')

####################################################################################################
#------------------- species description year vs collection year ----------------------------------#
# for this the species checklist is required. Please contact the authors for access.

# 
# sp_year_P <- NULL
# P_CL <- P_out_cln[P_out_cln$accepted_name %in% CL$accepted_name,]
# sp_year_P <- as.data.frame(unique(P_CL$accepted_name))
# colnames(sp_year_P) <- 'accepted_name'
# # get stats (min/max/median) collection year per species at P
# for(i in 1:length(sp_year_P$accepted_name)){
#   spp <- sp_year_P$accepted_name[i]
#   print(spp)
#   t1 <- P_out_cln[P_out_cln$accepted_name == spp, "col_year"]
#   sp_year_P$start[i] <- min(t1$col_year, na.rm = TRUE)
#   sp_year_P$end[i] <- max(t1$col_year, na.rm = TRUE)
#   sp_year_P$median[i] <- round(median(t1$col_year, na.rm = TRUE))
#   sp_year_P$range[i] <- sp_year_P$end[i] - sp_year_P$start[i]
# }
# # IPNI description year
# sp_P <- ipni_dat[ipni_dat$accepted_name %in% sp_year$accepted_name,]
# 
# sp_year <- base::merge(sp_year_P, sp_P, by.X = 'accepted_name', by.Y = 'accepted_name')
# 
# # number of species and the respective occurrences
# P_counts <- P_out_cln %>%
#   dplyr::count(accepted_name)
# sp_year <- merge(sp_year, P_counts, by.X = 'accepted_name', by.Y = 'accepted_name')
# 
# 
# spp_yr <- sp_year %>%
#   dplyr::group_by(ipni_pub) %>%
#   dplyr::count(accepted_name)
# spp_yr <- arrange(spp_yr, desc(ipni_pub))
# 
# 
# ggplot()+
#   geom_density(data = sp_year, aes(x = ipni_pub), colour = 'red') +
#   geom_density(data=sp_year, aes(x = median), colour = 'purple')
# 
# 
# ggplot() + 
#   geom_histogram(data = sp_year, aes(x = ipni_pub), 
#                  binwidth = 5,
#                  na.rm = TRUE,
#                  fill = 'red') +
#   geom_histogram(data = sp_year, aes(x = median), 
#                  binwidth = 5, 
#                  na.rm = TRUE,
#                  colour = 'purple', alpha = 0.5) +
#   theme_classic() +
#   labs(x='Collection Year', y = 'Number of specimens / 5 years',
#        title = '', colour = 'black') +
#   scale_x_continuous(limits = c(1735, 2022), breaks = c(1750, 1775, 1800, 1825, 1850, 
#                                                         1875, 1900, 1925, 1950, 1975, 2000, 2015)) 
# 


####################################################################################################
####--------------------------- Collectors and species -----------------------------------------####


col_P <- NULL
col_P <- na.omit(as.data.frame(unique(P_out_cln$collector...collector)))
colnames(col_P) <- 'collector'
# get stats (min/max/median) collection year per species at P
for(i in 1:length(col_P$collector)){
  spp <- col_P$collector[i]
  print(spp)
  t1 <- P_out_cln[P_out_cln$collector...collector == spp, "col_year"]
  col_P$start[i] <- min(t1$col_year, na.rm = TRUE)
  col_P$end[i] <- max(t1$col_year, na.rm = TRUE)
  col_P$median[i] <- round(median(t1$col_year, na.rm = TRUE))
  col_P$range[i] <- col_P$end[i] - col_P$start[i]

  }

colcount_P <- P_out_cln %>% dplyr::count(P_out_cln$collector...collector, P_out_cln$accepted_name, name=NULL)
colnames(colcount_P) <- c('collector', 'n', 'geometry')
col_year <- merge(col_P, colcount_P, by.X = 'collector', by.Y = 'collector')


####################################################################################################
####-------------- Species numbers per source and total for Annonaceae -------------------------####
# exploratory, not included in analyses

# sp_vecP <- unique(P_dat_nom$accepted_name)
# sp_vecG <-unique(GBIF_good_nom$accepted_name)
# allnames <- append(sp_vecP, sp_vecG)
# sp_vec <- unique(allnames)
# all_sppP <- as.data.frame(unique(sp_vecP))
# all_sppG <- as.data.frame(unique(sp_vecG))
# 
# colnames(all_sppP) <- 'spp'
# colnames(all_sppG) <- 'spp'
# 
# for(i in 1:length(sp_vecP)){
#   all_sppP$n[i] <- length(P_dat_nom[P_dat_nom$accepted_name == sp_vec[i],'accepted_name'])
# }
# all_sppP$dat <- 'P'
# for(i in 1:length(sp_vecG)){  
#   all_sppG$n[i] <- length(GBIF_good_nom[GBIF_good_nom$accepted_name == sp_vec[i],'accepted_name'])
# }
# all_sppG$dat <- 'G'
# 
# all_spp <- rbind(all_sppP, all_sppG)
# all_spp <- all_spp %>%
#   arrange(desc(n), .by_group = TRUE) %>%
#   group_by(spp)
# 
#   arrange(all_spp, desc(n))
# 
# as <- merge(all_sppG, all_sppP, by.x = 'spp', by.y = 'spp', all = T)
# 
# # get number of records per species
# for(i in 1:length(as$spp)){
#             as$num_rec[i] <- sum(as$n.x[i], as$n.y[i], na.rm = T)
# }
# # species more than 5/10 records
# as5 <- as[as$num_rec >=5, ]
# as10 <- as[as$num_rec >=10, ]
# 
# 
# ggplot(all_spp) +
#   geom_col(aes(x = spp, y = n, fill = dat))
# #
# 
# all_spp[,c(1,2)] %>% 
#   group_by(spp) %>% summarise(by = spp)
# 
# all_spp_cln <- all_spp[, lapply(.SD, as.numeric), by = 'spp']


#
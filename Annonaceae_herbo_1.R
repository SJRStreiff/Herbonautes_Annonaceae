####################################################################################################
# Herbonautes part 1. Herbarium data: Herbonautes                                                  #
# Detail: Format, clean and synthesise Paris Herbonautes dataset                                   #
#         Format, clean and prepare comparison data GBIF                                           #
# Author: SJRS                                                                                     #
# Date: 2023-02-23                                                                                 #
#                                                                                                  #
# ------------------------------------------------------------------------------------------------ #
# - Read and format data                                                                           #    
# - standardise data                                                                               #
# - separate taxa names, export for checking, reimport checked names and consolidate               #
# - split and count species by region                                                              #
# - format and check coordinates                                                                   #
####################################################################################################

rm(list = ls()) 

# a function i like to use.
`%notin%` <- Negate(`%in%`)

# what packages do we need?
pac <- c('plyr',
         #'tidyverse',
         'ggplot2',
         'stringr',
         'rworldxtra',
         'mapdata',
         'sf',
         'CoordinateCleaner',
         'ggrepel')


sapply(pac, require, character.only = TRUE)

dat_dir <- './data/' #directory where data files are located
wd <- '~/Sync/0_Papers/2024_Herbonautes_Adansonia/Supp_Inf_GH/' # working directory
setwd(wd)


####################################################################################################
####------------------------- Read data --------------------------------------------------------####

load(paste0(dat_dir, 'raw_data.Robj'))
# this contains:
#  - P_dat: raw Paris Herbonautes output, in one df
#  - GBIF_dat: entire GBIF download, `preserved specimens`; accessed 02 March 2023; https://doi.org/10.15468/dl.zamdu6; 
#  - GBIF_georef: subset of GBIF dat where coordinates not NA
# load(paste0(dat_dir, 'taxa_dat.Robj'))

load('~/Sync/herbo1.Robj')
# this contains:
# - dataframes referenced to POWO using the custom python script "POWO_check.py"
#    - P_tax Paris taxon references
#    - G_tax: GBIF taxon references

save(P_dat_raw, P_dat, GBIF_dat, GBIF_georef, file = './tmp_objects.Robj')

####################################################################################################
####------------------------- Check some very general stats ------------------------------------####


total_records <- length(P_dat$specimen...institute)
families_popping_in <- unique(P_dat$specimen...family) # check what other families are found?

# > First round of CLEANING ---------------####
print(paste('The size of the Paris dataset: ',dim(P_dat)[1],'/',dim(P_dat)[2] ))
# language changes, standardise
# TRUE / FALSE problems
P_dat[P_dat == 'VRAI'] <- TRUE 
P_dat[P_dat == 'FAUX'] <- FALSE 
P_dat[P_dat == 'oui'] <- TRUE 
P_dat[P_dat == 'non'] <- FALSE
P_dat[P_dat == 'true'] <- TRUE 
P_dat[P_dat == 'false'] <- FALSE 
# NA values
P_dat[P_dat == ''] <- NA
P_dat[P_dat == 'null'] <- NA
P_dat[P_dat == 'None'] <- NA

# get all records with a species level determination
P_dets <- P_dat[P_dat$specimen...specific_epithet %notin% c('indet.', 'null', 
                                                            'Indeterminee', 'Genre', 'sp.', ''),]
P_dets <- P_dets[P_dets$specimen...genus %notin% c('indet.', 'null', 
                                                   'Indeterminee', 'Genre', ''),]

# and all indet records for specific epithet
indet_sp_ep <- length(which(P_dat$specimen...specific_epithet %in% c('indet.', 'null', 
                                                                     'Indeterminee', 'Genre', '')))
# problems will be faced with e.g. <<Neo-uvaria vs Neouvaria vs Neo-Uvaria >>                     
indet_gen <- length(which(P_dat$specimen...genus %in% c('indet.', 'null', 'Indeterminee', 
                                                        'Genre', '')))

# number of individual collector names
num_collectors <- length(unique(P_dat$collector...collector))
print(paste('#> Number of individual collectors/botanists for (raw) P:', num_collectors))

# so on average a collector brings ~10 specimens of Annonaceae to P. nice
no_lat <- sum(is.na(P_dat$geo...position))

print('###> All the problem values are:')
unique(P_dat$country...no_info, na.rm = TRUE)

# 2 records have a duplicated barcode: length(unique(P_dat$specimen...code))
P_unuseable <- P_dat %>% dplyr::filter(!is.na(unusable...cause))
P_dat <- P_dat %>% dplyr::filter(is.na(unusable...cause))

####################################################################################################
      ####---------- Prep for checking taxonomy     ---------####
      # This was run once, and the output run through custom python scripts checking
      # the nomenclature against powo (powo.science.kew.org/). The Annonaceae nomenclature there is 
      # relatively up to date.
      # The check is done with the 'check_taxa.py' python script. 
      # the respective ourput is read back into R
####################################################################################################

P_dat$species <- paste(P_dat$specimen...genus, P_dat$specimen...specific_epithet)
# # reduced dataset
tax_check <- P_dat[,c("specimen...code", "species", "specimen...genus", "specimen...specific_epithet")]
# write.table(tax_check, file = 'prob_check2.csv', sep = ';', row.names = F)
num_taxa <- unique(tax_check$species)
#
tax_check_out <- tax_check#
uid <- as.data.frame(strsplit(as.character(Sys.time()), split = ' '))[1,]
out_filename <-  paste('~/Sync/1_Annonaceae/P_herbonautes/', uid, '_tax_check', sep ='')
# write.table(tax_check, file = paste('~/Sync/1_Annonaceae/P_herbonautes/', uid, 'tax_check', sep =''), sep = ';')

# write to csv
# write.table(tax_check, file = '~/Sync/1_Annonaceae/P_herbonautes/tax_check_dat.csv', sep =';', row.names = FALSE)

tax_gbif_check <- GBIF_georef[,c("gbifID", "scientificName", "genus", "species")]
tax_gbif_check$specific_epithet <- as.data.frame(str_split_fixed(tax_gbif_check$species, pattern = ' ', 2))[,1]
tax_gbif_check <- tax_gbif_check[,-4]
# write_delim(tax_gbif_check, file = paste('~/Sync/1_Annonaceae/P_herbonautes/', uid, 'tax_gbif_check', sep =''), delim = ';')
#


####################################################################################################
####------------------------------ Reinsert checked taxonomy -----------------------------------####

# read the file where all taxonomy has been checked against POWO with my custom POWO query scripts

#nom_check <- read.csv(file = 'P_taxa_POWO_queried.csv', sep =';', head=T)
nom_check <- P_tax
#delete
# # manually checked data which failed to match
# more_nom_check <- read.csv(file = 'P_tax_rev.csv', sep =';', head =T)
# colnames(more_nom_check) <- colnames(nom_check)
# # this is the final frame for reconciling nomenclature with P_dat
# nom_check <- rbind(nom_check, more_nom_check)
dim(nom_check)
dim(P_dat)

# these records could not be reconciliated with any taxonomy (missing determinations)
P_no_determination <- P_dat[P_dat$specimen...code %notin% nom_check$barcode,] # all these are "indet." or "NA" for specific_epithet

# merge the checked taxonomy and P_dat
P_dat_nom <- merge(P_dat, nom_check, by.x = 'specimen...code', by.y = 'barcode')
P_dat_nom[P_dat_nom == ''] <- NA
P_dat_det_bak <- P_dat_nom # backup dataframe
P_dat_nom <- P_dat_nom[P_dat_nom$status %in% c('ACCEPTED', 'UNPLACED', 'SYNONYM'),] # take names that matched something, drop all others
P_taxonomy_NA <- P_dat_det_bak[is.na(P_dat_det_bak$status),]
P_taxonomy_issues <- P_dat_det_bak[P_dat_det_bak$status %notin% c('ACCEPTED', 'UNPLACED', 'SYNONYM'),]

P_taxonomy_Big_issues <- P_taxonomy_issues[P_taxonomy_issues$specific_epithet != 'sp.' &
                                             P_taxonomy_issues$specific_epithet != 'Indet.' &
                                             P_taxonomy_issues$specific_epithet != 'indet.' ,]

P_taxonomy_indet <- P_taxonomy_issues[P_taxonomy_issues$specific_epithet == 'sp.' |
                                        P_taxonomy_issues$specific_epithet ==  'Indet.' |
                                        P_taxonomy_issues$specific_epithet ==  'indet.',]

# OUTPUT/LOGGING
print(paste('#> Indets (spp./indet.):', (dim(P_taxonomy_indet)[1]), 
            '#> Indets (nom. dubius.):', dim(P_taxonomy_Big_issues)[1],
            '#> Good names:', dim(P_dat_nom)[1]))


P_dat_cln <- P_dat_nom %>% dplyr::filter(specimen...family == 'Annonaceae' | specimen...family == 'ANNONACEAE') 
# and remove all data with other family dets (6 records....)

# repeat for GBIF

# G_nom_check <- read.csv(file = 'GBIF_taxa_POWO_queried.csv', sep =';', head=T)
G_nom_check <- G_tax
GBIF_good_nom <- merge(GBIF_georef, G_nom_check) 
GBIF_good_nom[GBIF_good_nom == ''] <- NA
GBIF_good_nom <- GBIF_good_nom[GBIF_good_nom$status %in% c('ACCEPTED', 'UNPLACED', 'SYNONYM'),] 


# Summarise taxonomic cleaning
print(paste('NO georeference P for', sum(is.na(P_dat_cln$geo...position)), 'records. These are now removed'))
print(paste('Number of (expert) determined specimens  P:', sum(!is.na(P_dat_cln$identifier...identifier))))

print(paste('Number of taxonomically resolved species GBIF:', length(GBIF_good_nom$gbifID)))
print(paste('Number of (expert) determined specimens  GBIF:', sum(!is.na(GBIF_good_nom$identifiedBy))))


####################################################################################################
####------------------- Collection general statistics ------- ----------------------------------####

# What's the deal with species coverage P vs GBIF 
# specimens per species

# number of specimens (rows) per species:
spcount_P <- NULL
spcount_P <- P_dat_cln %>% dplyr::count(P_dat_cln$accepted_name, name=NULL)
spcount_G <- NULL
spcount_G <- GBIF_good_nom %>%  dplyr::count(GBIF_good_nom$accepted_name, name=NULL)

# Digging a bit more into specimens per species 
# arrange in descending order
spcount_G <- arrange(spcount_G, desc(n))
spcount_P <- arrange(spcount_P, desc(n))  

# get all the spp with just one specimen
sparse_spp_G <- spcount_G[spcount_G$n == 1,]
sparse_spp_P <- spcount_P[spcount_P$n == 1,]

# noralised numbers per species
spcount_G$n_norm <- spcount_G$n / length(GBIF_good_nom$accepted_name)
spcount_P$n_norm <- spcount_P$n / length(P_dat_cln$accepted_name)

# percentages and total numbers 
spcount_G$sumperc <- NA
spcount_G$sumnum <- NA
for(i in 1:length(spcount_G$n)){
  spcount_G$sumperc[i] <- sum((spcount_G$n_norm[1:i]))
  spcount_G$sumnum[i]  <- sum((spcount_G$n[1:i]))
}
spcount_G$perc_sp <- seq(0,100, by = ((100 - 0)/(length(spcount_G[,1])-1)))

spcount_P$sumperc <- NA
spcount_P$sumnum <- NA
for(i in 1:length(spcount_P$n)){
  spcount_P$sumperc[i] <- sum((spcount_P$n_norm[1:i]))
  spcount_P$sumnum[i]  <- sum((spcount_P$n[1:i]))
}
spcount_P$perc_sp <- seq(0,100, by = ((100 - 0)/(length(spcount_P[,1])-1)))


# get the top ten species
topten_P <- head(spcount_P, n = 10L )
topten_G <- head(spcount_G, n = 10L )
# steps for plots below
steps <- seq(0.025, 1, by = 0.025)


# fractions os specimens per species per datase
# GBIF
length(spcount_G$`GBIF_good_nom$accepted_name`)/40
num <- NULL
num[1] <- 0
total_G <- NULL
for(i in 1:40){
  total_G$species_percentile[i] <- steps[i]
  stepsize <- length(spcount_G$`GBIF_good_nom$accepted_name`)/40
  num[i+1] <- num[i] + stepsize
  begin <- round(num[i]) 
  end <- round(num[i+1])
  total_G$sum[i] <- sum(spcount_G$n[begin:end])
  total_G$percentages[i] <- total_G$sum[i]/dim(GBIF_good_nom)[1]
  total_G$total_sum[i] <- sum(total_G$sum[1:i])
}
total_G <- as.data.frame(total_G)

# fractions os specimens per species per datase
# P
length(spcount_P$`P_dat_cln$accepted_name`)/40
num <- NULL
num[1] <- 0
total_P <- NULL
for(i in 1:40){
  total_P$species_percentile[i] <- steps[i]
  stepsize <- length(spcount_P$`P_dat_cln$accepted_name`)/40
  num[i+1] <- num[i] + stepsize
  begin <- round(num[i]) 
  end <- round(num[i+1])
  total_P$sum[i] <- sum(spcount_P$n[begin:end])
  total_P$percentages[i] <- total_P$sum[i]/dim(P_dat_cln)[1]
  total_P$total_sum[i] <- sum(total_P$sum[1:i])
}
total_P <- as.data.frame(total_P)



####################################################################################################
##########--- Figure: Fraction of specimens per fraction of species-------------------##############

points_G <- data.frame(
  x = c(5.35, 16.3, 34.6),
  y=  c(50, 75, 90),
  lab = c('50% - 103 spp', '75% - 310 spp','90% - 655 spp' )
)
tot_G_points <- data.frame(
  x = 100, y =100, lab = 'Total 1889 spp'
)
points_P <- data.frame(
  x = c(7.8, 21.5, 41.6),
  y=  c(50, 75, 90),
  lab = c('50% - 95 spp', '75% - 254 spp','90% - 499 spp')
)
tot_P_points <- data.frame(
  x = 100, y =100, lab = 'Total 1198 spp'
)


asymptotplot <- ggplot() +
  geom_line(data = spcount_G, aes(x = perc_sp, y = sumperc*100)) +
  geom_line(data = spcount_P, aes(x = perc_sp, y = sumperc*100), linetype = 2) +
  # geom_line(aes(x = 5.35, y = c(-0, 50))) +
  # geom_line(aes(x = 7.8, y = c(-0, 50)), linetype = 2) +
  # geom_line(aes(x = 16.3, y = c(-0, 75))) +
  # geom_line(aes(x = 21.5, y = c(-0, 75)), linetype = 2) +
  # geom_line(aes(x = 34.6, y = c(-0, 90))) +
  # geom_line(aes(x = 41.6, y = c(-0, 90)), linetype = 2) +
  geom_point(data = points_G, aes(x=x, y=y), size = 0)+
  # coord_cartesian(clip = "off") +
  geom_label_repel(data = points_G, 
                  aes(x= x, y=y,label = lab),
                  max.overlaps = Inf, 
                  nudge_x = -2, nudge_y = 10,
                  fill = 'white',
                  box.padding = 0.1,
                  segment.linetype = 1) +
  geom_label_repel(data = tot_G_points, 
                   aes(x= x, y=y,label = lab),
                   max.overlaps = Inf, 
                   nudge_x = -10, nudge_y = 3,
                   fill = 'white',
                   box.padding = 0.1,
                   xlim = c(-Inf, Inf),
                   ylim = c(-Inf, Inf),
                   segment.linetype = 1) +
  geom_point(data = points_P, aes(x=x, y=y), size = 0)+
  geom_label_repel(data = points_P, 
                   aes(x= x, y=y,label = lab),
                   max.overlaps = Inf, 
                   nudge_x = 20, nudge_y = -5,
                   fill = 'grey',
                   box.padding = 0.1,
                   segment.linetype = 2) +
  geom_label_repel(data = tot_P_points, 
                   aes(x= x, y=y,label = lab),
                   max.overlaps = Inf, 
                   nudge_x = -10, nudge_y = -8,
                   fill = 'grey',
                   box.padding = 0.1,
                   xlim = c(-Inf, Inf),
                   ylim = c(-Inf, Inf),
                   segment.linetype = 2) +
  labs(x = '% of species',
       y = 'cumulative % of specimens') +
#       title = 'Specimens per species') +
  theme_bw()

asymptotplot

#ggsave(filename = '0_plots/specimen_distributrion.png', device = 'png', width = 8, height = 4, units = 'in')
ggsave(filename = '0_plots/specimens_per_spp.eps', device = 'eps', dpi = 300,
       width = 8, height = 6, units = 'in')



####################################################################################################
##########----------------  get specimens from P in GBIF -----------------------------##############
# overlap is relatively small


P_i_G <- GBIF_dat %>% dplyr::filter(collectionCode == 'P')
P_i_G_Afr <- P_i_G %>% dplyr::filter(countryCode %in% c('BJ', 'CM', 'GA'))

barcodesPfull <- P_i_G$catalogNumber
barcodesPfull <- append(barcodesPfull, P_dat$specimen...code)

####################################################################################################
##########---------------- Checklist to get species POWO distribution ----------------##############
# the Checklist can be requested if required from the authors:
# serafin.streiff at ird.fr
# read Checklist
# CL <- read.csv(file = 'CL.csv', sep =';', head=T)
# # testing with complete/newer CL crossref
# 
# # extract just species to get an idea for the datasets
# speciesCL <- CL$spp
speciesP <- unique(P_dat_cln$accepted_name)
speciesG <- unique(GBIF_good_nom$accepted_name)


# # Species/Specimens per region 
# # check species P that are not in CL. # these are species that have not been updated on POWO, or other issues (typos, ...)
# P_sp_prob <- speciesP[speciesP %notin% speciesCL]
# G_sp_prop <- speciesG[speciesG %notin% speciesCL]
# P_prop_noG <- P_sp_prob[P_sp_prob %notin% G_sp_prop]


# P_noCL <- P_dat_cln[P_dat_cln$accepted_name %in% P_sp_prob,]
# P_prop_country <- P_noCL %>%
#   dplyr::group_by(country...country) %>%
#   dplyr::count(accepted_name)

  
  G_counts <- GBIF_good_nom %>%
  dplyr::group_by(continental_reg) %>%
  dplyr::count(accepted_name)

# define regions (queried from POWO: "POWO_native_to")
America <- c('Brazil Northeast', 'Brazil Southeast','Brazil North', 'Brazil West-Central', 
             'Ecuador', 'Peru', 'Bolivia', 'Colombia', 'Venezuela', 'Guyana', 'Suriname',
             'French Guiana', 'Panama', 'Costa Rica', 'Nicaragua', 'Honduras', 'El Salvador',
             'Guatemala', 'Belize', 'Mexico Gulf', 'Mexico Northeast',
             'Mexico Southeast', 'Mexico Southwest', 'Cuba', 'Jamaica',
             'Haiti', 'Dominican Republic', 'Puerto Rico', 'Trinidad and Tobago',
             'Texas', 'Florida', 'Georgia', 'Virginia', 'Pennsylvania', 'Mexico Northwest', 'California', 'Oregon')

Africa       <- c('Senegal', 'Guinea', 'Liberia', 'Ghana', 'Togo', 'Benin', 'Nigeria', 'Ivory Coast' ,
                  'Cameroon', 'Equatorial Guinea', 'Gabon','Rep Congo', 'Angola', 
                  'Congo', 'Zambia', 'Zaïre', 'Tanzania', 'Mozambique', 'Uganda', 'Kenya','Somalia',
                  'South Sudan', 'Ethiopia' )
Mada      <- c('Madagascar', 'Réunion', 'Mauritius')
SEA           <- c('India', 'Sri Lanka',
                   'Assam','China South-Central', 'China Southeast', 'East Himalaya', 
                   'Queensland','New Guinea', 'New Caledonia', 'Solomon Is.','Fiji',
                   'Philippines', 'Malaya', 'Myanmar', 'Thailand', 'Cambodia', 'Jawa', 
                   'Sulawesi', 'Sumatera', 'Borneo', 'Laos')

# sort species into regions 
biogeo_dat <- (rbind(G_nom_check, nom_check))
biogeo_dat <- unique(biogeo_dat[,c('accepted_name', "POWO_native_to")])

biogeo_dat$America <- as.numeric(sapply(strsplit(biogeo_dat$POWO_native_to, ","), function(x) any(x %in% America)))
biogeo_dat$Africa <- as.numeric(sapply(strsplit(biogeo_dat$POWO_native_to, ","), function(x) any(x %in% Africa)))
biogeo_dat$Mada <- as.numeric(sapply(strsplit(biogeo_dat$POWO_native_to, ","), function(x) any(x %in% Mada)))
biogeo_dat$SEA <- as.numeric(sapply(strsplit(biogeo_dat$POWO_native_to, ","), function(x) any(x %in% SEA)))

species <- list()
species[["America"]] <-  biogeo_dat[biogeo_dat$America == 1, "Genus.Species.epithet"]
species[["Africa"]] <-  biogeo_dat[biogeo_dat$Africa == 1, "Genus.Species.epithet"]                                                                                                                                                                                                                                                                                                                                 
species[["Mada"]] <-  biogeo_dat[biogeo_dat$Mada == 1, "Genus.Species.epithet"]
species[["SEA"]] <-  biogeo_dat[biogeo_dat$SEA == 1, "Genus.Species.epithet"]
# species is the taxon reference list

distr_P <- list()
distr_P[["Am_sp"]] <- P_dat_cln[sapply(P_dat_cln$accepted_name, 
                                       function(x) any(x %in% species[["America"]])), 
                                c("accepted_name","ddlat", "ddlong")]
distr_P[["Afr_sp"]] <-P_dat_cln[sapply(P_dat_cln$accepted_name, 
                                       function(x) any(x %in% species[["Africa"]])),
                                c("accepted_name","ddlat", "ddlong")]
distr_P[["Mada_sp"]] <- P_dat_cln[sapply(P_dat_cln$accepted_name, 
                                         function(x) any(x %in% species[["Mada"]])), 
                                  c("accepted_name","ddlat", "ddlong")]
distr_P[["SEA_sp"]] <- P_dat_cln[sapply(P_dat_cln$accepted_name, 
                                        function(x) any(x %in% species[["SEA"]])), 
                                 c("accepted_name","ddlat", "ddlong")]

# and fill back into working dataframes
P_dat_cln$continental_reg <- NA
P_dat_cln$continental_reg[P_dat_cln$accepted_name %in% species[['America']]] <- 'America'
P_dat_cln$continental_reg[P_dat_cln$accepted_name %in% species[['Africa']]] <- 'Africa'
P_dat_cln$continental_reg[P_dat_cln$accepted_name %in% species[['Mada']]] <- 'Madagascar'
P_dat_cln$continental_reg[P_dat_cln$accepted_name %in% species[['SEA']]] <- 'SE-Asia'

GBIF_good_nom$continental_reg <- NA
GBIF_good_nom$continental_reg[GBIF_good_nom$accepted_name %in% species[['America']]] <- 'America'
GBIF_good_nom$continental_reg[GBIF_good_nom$accepted_name %in% species[['Africa']]] <- 'Africa'
GBIF_good_nom$continental_reg[GBIF_good_nom$accepted_name %in% species[['Mada']]] <- 'Madagascar'
GBIF_good_nom$continental_reg[GBIF_good_nom$accepted_name %in% species[['SEA']]] <- 'SE-Asia'

# get number of species per region (and print)
print('PARIS: America') 
length(unique(distr_P$Am_sp$accepted_name))
pAm_dat <- unique(distr_P$Am_sp$accepted_name)
print('Africa')
length(unique(distr_P$Afr_sp$accepted_name))
pAfr_dat <- unique(distr_P$Afr_sp$accepted_name)
print('Mada')
length(unique(distr_P$Mada_sp$accepted_name))
pMa_dat <- unique(distr_P$Mada_sp$accepted_name)
print('SEA')
length(unique(distr_P$SEA$accepted_name))
pSEA_dat <- unique(distr_P$SEA_sp$accepted_name)

# and repeat for GBIF
distr_G <- list()
distr_G[["Am_sp"]] <- GBIF_good_nom[sapply(GBIF_good_nom$accepted_name, 
                                           function(x) any(x %in% species[["America"]])), 
                                    c("accepted_name","decimalLatitude", "decimalLongitude")]
distr_G[["Afr_sp"]] <-GBIF_good_nom[sapply(GBIF_good_nom$accepted_name, 
                                           function(x) any(x %in% species[["Africa"]])), 
                                    c("accepted_name","decimalLatitude", "decimalLongitude")]
distr_G[["Mada_sp"]] <- GBIF_good_nom[sapply(GBIF_good_nom$accepted_name, 
                                             function(x) any(x %in% species[["Mada"]])), 
                                      c("accepted_name", "decimalLatitude","decimalLongitude")]
distr_G[["SEA_sp"]] <- GBIF_good_nom[sapply(GBIF_good_nom$accepted_name, 
                                            function(x) any(x %in% species[["SEA"]])), 
                                     c("accepted_name","decimalLatitude", "decimalLongitude")]
print('GBIF: America') 
length(unique(distr_G$Am_sp$accepted_name))
gAm_dat <- unique(distr_G$Am_sp$accepted_name)

print('Africa')
length(unique(distr_G$Afr_sp$accepted_name))
gAfr_dat <- unique(distr_G$Afr_sp$accepted_name)
print('Mada')
length(unique(distr_G$Mada_sp$accepted_name))
gMa_dat <- unique(distr_G$Mada_sp$accepted_name)

print('SEA')
length(unique(distr_G$SEA$accepted_name))
gSEA_dat <- unique(distr_G$SEA_sp$accepted_name)


# species new for respective region with P
SEA_new <- pSEA_dat[pSEA_dat %notin% gSEA_dat] # 89 spp new for SEA
Afr_new <- pAfr_dat[pAfr_dat %notin% gAfr_dat] # 11 spp new for Afr
Am_new <- pAm_dat[pAm_dat %notin% gAm_dat] # 4 spp new for Am
Mada_new <- pMa_dat[pMa_dat %notin% gMa_dat] # 9 spp new for SEA
print(paste('NEW species: SEA', length(SEA_new), 'Afr', length(Afr_new), 
            'Am', length(Am_new), 'Mada', length(Mada_new)))


#######------------ improvement of P over GBIF -------------------##############
SEA_diff_perc <- ((length(SEA_new) + length(gSEA_dat))/length(species[['SEA']])) -
  (length(gSEA_dat))/length(species[['SEA']])



taxP <- unique(P_dat_cln$accepted_name)
taxG <- unique(GBIF_good_nom$accepted_name)

# species NOT found in both datasets
noG <- taxP[taxP %notin% taxG]
noP <- taxG[taxG %notin% taxP]



####################################################################################################
###--------------------- Plot spatial distribution of species -----------------------###############

data("countriesHigh")
mapdat    <- sf::st_as_sf(countriesHigh)

# make sure coordinates are numeric
GBIF_good_nom$decimalLongitude <- as.numeric(GBIF_good_nom$decimalLongitude)
GBIF_good_nom$decimalLatitude <- as.numeric(GBIF_good_nom$decimalLatitude)
# use coordinate cleaner to subset points that may fall in ocean (uncertainty or georeferencing error)
GBIF_geoclean <- clean_coordinates(GBIF_good_nom, 
                                   lon = 'decimalLongitude', 
                                   lat = 'decimalLatitude',
                                   species = 'species', 
                                   tests = c("seas", "zeros"))

P_dat_cln <- P_dat_cln[!is.na(P_dat_cln$ddlat),]
P_dat_cln$ddlat <- as.numeric(P_dat_cln$ddlat)
P_dat_cln$ddlong <- as.numeric(P_dat_cln$ddlong)

P_geoclean <- clean_coordinates(P_dat_cln, 
                                lon = 'ddlong', 
                                lat = 'ddlat',
                                species = 'species', 
                                tests = c("seas", "zeros"))


### quick and rough plot to see the distribution of problematic points. 
# For P this is only coastal points. GBIF has any coastal points, 
# but also some that are in the middle of the oceans (far from any islands....)

p1 <-  ggplot() +
  geom_sf(data = mapdat) +#, aes(x = long, y = lat)) +
    theme(plot.background = element_rect(fill = "blue"),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_text(size = 7)) +
    labs(x='Longitude',y='Latitude')
# p1 is basic map with no points yet
p1 + geom_point(data = P_geoclean[P_geoclean$.summary == FALSE,], 
              aes(x = ddlong, y = ddlat), pch = '+', colour = 'red')+
              labs(title = 'Paris data') # this is the map with problem points for Paris dataset

p1 + geom_point(data = GBIF_geoclean[GBIF_geoclean$.summary == FALSE,], 
              aes(x = decimalLongitude, y = decimalLatitude), 
              pch = '+', colour = 'red')+
              labs(title = 'GBIF data')


####################################################################################################
###--------------------- Check and verify problems with sea -- -----------------------###############

# revisit all points in the sea.
# If the points are within a buffer distance, snap onto closest coastline

# coastline shapes are downloaded from here: https://www.naturalearthdata.com/downloads/50m-physical-vectors/
clines <- read_sf('../Y_DATA/2_land-map_rasters/ne_50m_coastline/ne_50m_coastline.shp') #read shapefile

#make spatial features dataframes
gbif_sf <- GBIF_geoclean %>% st_as_sf(coords = c('decimalLongitude','decimalLatitude')) %>% 
                             st_set_crs('WGS84') 
p_sf <- P_geoclean %>% st_as_sf(coords = c('ddlong', 'ddlat')) %>% 
                       st_set_crs('WGS84')

# subset just the problematic points to test for distance to coastline
gbif_sf_tt <-gbif_sf[gbif_sf$.summary == FALSE,]
p_sf_tt <- p_sf[p_sf$.summary == FALSE,]

# go through points and get smallest distance to coastline
for(i in 1:length(gbif_sf_tt$gbifID)){
  testvalues <- st_as_sf(gbif_sf_tt[i,])
  gbif_sf_tt$coast_dist[i] <- min(st_distance(testvalues, clines))
  print(gbif_sf_tt$coast_dist[i])
  print(i)
}
for(i in 1:length(p_sf_tt$specimen...code)){
  p_sf_tt$coast_dist[i] <- min(st_distance(p_sf_tt[i,], clines))
  print(p_sf_tt$coast_dist[i])
  print(i)
}


# Now choosing a decent cutoff value...
# based on this data and the coordinate accuracy I went for 15000m
# any point closer than this is snapped onto coastline
# points further away are falgged as problematic and discarded

gbif_sf_tt$coast_dist <- as.numeric(gbif_sf_tt$coast_dist) 
print(paste('MEDIAN', median(gbif_sf_tt$coast_dist), 'm'))
print(paste('MEAN', mean(gbif_sf_tt$coast_dist), 'm'))
ggplot() + geom_density(aes(gbif_sf_tt$coast_dist))
gbif_tbs <- gbif_sf_tt[gbif_sf_tt$coast_dist <= 15000,]
print(paste('This many thrown out:', (dim(gbif_sf_tt[gbif_sf_tt$coast_dist > 15000,])[1])))
ggplot() + geom_density(aes(gbif_tbs$coast_dist))

p_sf_tt$coast_dist <- as.numeric(p_sf_tt$coast_dist) 
print(paste('MEDIAN', median(p_sf_tt$coast_dist), 'm'))
print(paste('MEAN', mean(p_sf_tt$coast_dist), 'm'))
ggplot() + geom_density(aes(p_sf_tt$coast_dist))
p_tbs <- p_sf_tt[p_sf_tt$coast_dist <= 15000,]
print(paste('This many thrown out:', (dim(p_sf_tt[p_sf_tt$coast_dist > 15000,])[1])))
ggplot() + geom_density(aes(p_tbs$coast_dist))


# now reintegrate the data that can be saved
gbif_tbs <- gbif_tbs[,-length(gbif_tbs)]
GBIF_out_cln <- rbind(gbif_sf[gbif_sf$.summary == TRUE,], gbif_tbs)

p_tbs <- p_tbs[,-length(p_tbs)]
P_out_cln <- rbind(p_sf[p_sf$.summary == TRUE, ], p_tbs )


####################################################################################################
########------------------------- Grid calculation preparation ----------------------------#########

w<-1 # resolution in degrees
# the following takes quite a while, so I save the R object so i can reload it when done once.
Glob_Grid <- mapdat %>%
  st_make_grid(cellsize = w) %>%
   #st_intersection(mapdat) %>%
  st_cast('POLYGON') %>%
   st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())
# # save this once done. This takes a good while!
# is saved below in "P1_objects.Rdata"

####################################################################################################
#########----------------------- Top ten spp per region   ----------------------------##############

P_counts <- P_dat_cln %>%
  dplyr::group_by(continental_reg) %>%
  dplyr::count(accepted_name)
P_counts <- arrange(P_counts, continental_reg, desc(n))

G_counts <- GBIF_good_nom %>%
  dplyr::group_by(continental_reg) %>%
  dplyr::count(accepted_name)
G_counts <- arrange(G_counts, continental_reg, desc(n))

Ptc <- P_counts$accepted_name
Gtc <- G_counts$accepted_name
total_counts <- merge(Ptc, Gtc)
tc <- rbind(P_counts[P_counts$n>=1,], G_counts[G_counts$n>=1,])
length(unique(tc$accepted_name))


####################################################################################################
########---------------------- Saving objects for further analyses ---------------------############

# Saving these datasets:
# - P_dat_raw: raw Paris dataframe
# - P_dat_nom: taxonomy checked
# - P_out_cln: georeferenced and totally cleaned
# - GBIF_out_cln: GBIF taxonomy and georeferenced cleaned
# - Glob_Grid: Global grid at 1 degree resolution

save(P_dat_raw, P_dat_nom,GBIF_good_nom,
     GBIF_out_cln, P_out_cln, Glob_Grid, file = './data/P1_objects.Rdata')

#######--------------------------------------------------------------------------------#############
####################################################################################################





 

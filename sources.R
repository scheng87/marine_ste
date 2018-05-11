library(shiny)
library(tidyr)
library(shinythemes)
library(shinyBS)
library(gplots)
library(RColorBrewer)
library(maptools)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(scales)
library(lattice)
library(car)
library(shinyjs)
library(grid)
library(leaflet)
library(reshape2)
library(stringr)
library(dplyr)

#setwd("~/Documents/github/forest_poverty_lite/")
#setwd("/srv/shiny-server/forest_poverty/")
map_data_final <- readRDS("data/map_data_final.rds")

source("functions.R")

# definitions <- read.csv("data/definitions.csv",header=TRUE)
# definitions <- as.data.frame(definitions)
# 
intlabels <- read.csv("data/codes.csv",header=TRUE)
intlabels <- as.data.frame(intlabels)

oecd <- read.csv("data/oecd.csv",header=TRUE)
regions <- read.csv("data/country_list2.csv", header=TRUE)
colnames(regions) <- c("COUNTRY", "REGION","CODE","SUBREGION","POINT")
reg <- read.csv("data/allcountries.csv",header=TRUE)

coordinates <- read.csv("data/country_lat_lon.csv",header=TRUE)
countries_shape <- read.csv("data/country_coordinates.csv",header=TRUE)

# impl_type <- c("Academic","Public sector","Research institute","Consultant","Non-profit","Private sector/industry","International Convention")
# design_type <- c("Experimental","Quasi experimental","Non-experimental","Systematic review")
# comp_type <- c("Control-intervention site comparisons","Before-after","Spatial comparator","Interrupted time series","Continuous time series","Group comparators (e.g. cultural/ethnic, demographic, socio-economic, users)","Presence/absence of intervention","Projects")
out_type <- c("fin","health","hum_cap","mon_dir","mon_val","mon_wage","nat_for","nat_land","phys_cap","phys_cons","soc_cap")
int_type <- c("agrofor","eco_ser","emp","for_mgmt","gov","hab_mgmt","hum_cap","instit","liv_alt","mark_acc","market","non_mon","prod_cap","soc_cap")
biome_type = c("T_TSTMBF", "T_TSTDBF", "T_TSTCF", "T_TBMF", "T_BFT", "T_MFWS", "T_DXS", "T_M")
study_types <- c("exp","quas_exp","non_exp","imp_ev","non_sys_rev","sys_rev","NA")
study_labels <- c("Experimental","Quasi-experimental","Non-experimental","Impact evaluation","Non-systematic review","Systematic review","Unknown")
int_labels =c("Agroforestry","Managing/enhancing eco. sys. serv.","Indiv. empowerment","Forest mgmt","Governance","Habitat mgmt","Human capital","Institutions & markets","Linked enterprises/livelihood alt.","Market forces","Market access","Non-monetary benefits","Produced capital","Social capital")
out_labels = c("Fin. cap. (credit, savings,debt)","Health","Hum. cap. (knowledge, skills)","Mon. income (direct sale)","Mon. income (value added)","Mon. income (wage labor)","Nat. cap. (forest assets)","Nat. cap (land assets)","Phys. cap. (material assets)","Phys. income (consumption)","Soc. cap.")
biome_labels = c("Tropical/Subtropical Moist Broadleaf Forests","Tropical/Subtropical Dry Broadleaf Forests","Tropical/Subtropical Coniferous Forests","Temperate Broadleaf & Mixed Forests","Temperate Coniferous Forests","Boreal Forests/Taiga","Mediterranean Forests, Woodlands & Scrubs","Deserts & Xeric Shrublands","Mangroves")


countries <- as.vector(reg$Country)



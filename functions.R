##Define functions

##Define functions

# assignIntLabel <- function(input,output){
#   rows <- c(1:nrow(input))
#   int_labels <- matrix(nrow=nrow(input),ncol=1)
#   rownames(int_labels) <- rows
#   colnames(int_labels) <- c("int_labels")
#   for (i in rows){
#     int <- as.vector(input$Int_type[i])
#     if (int == "for_mgmt") {
#       group <- "Forest management"
#     } else if (int == "agrofor") {
#       group <- "Agroforestry"
#     } else if (int == "hab_mgmt") {
#       group <- "Habitat management"
#     } else if (int == "gov") {
#       group <- "Governance"
#     } else if (int == "emp") {
#       group <- "Individual rights/empowerment"
#     } else if (int == "prod_cap") {
#       group <- "Produced capital"
#     } else if (int == "hum_cap") {
#       group <- "Human capital"
#     } else if (int == "soc_cap") {
#       group <- "Social capital"
#     } else if (int == "liv_alt") {
#       group <- "Linked enterprises & livelihood alternatives"
#     } else if (int == "market") {
#       group <- "Identifying & strengthening market forces"
#     } else if (int == "mark_acc") {
#       group <- "Increasing access to markets"
#     } else if (int == "eco_ser") {
#       group <- "Managing & enhancing ecosystem services"
#     } else if (int == "instit") {
#       group <- "Strengthening institutions & markets"
#     } else if (int == "non_mon") {
#       group <- "Identifying non-monetary benefits"
#     } else
#       group <- "Unknown"
#     int_labels[i,"int_labels"] <- group
#   }
#   int_labels <- as.data.frame(int_labels)
#   output <- bind_cols(input,int_labels)
#   output <- filter(output,!is.na(int_labels))
#   return(output)
# }

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

loadData <- function() {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Read the data
  gs_read_csv(sheet)
}

# assignBiomeLabels <- function(input,output){
#   rows <- c(1:nrow(input))
#   biome_labels <- matrix(nrow=nrow(input),ncol=1)
#   rownames(biome_labels) <- rows
#   colnames(biome_labels) <- c("biome_label")
#   
#   for (i in rows){
#     biome <- input$Biome.[i]
#     if (is.na(biome)) {
#       group <- "NA"
#     } else if (biome == "T_TSTMBF") {
#       group <- "Tropical/subtropical moist broadleaf forests"
#     } else if (biome == "T_TSTDBF") {
#       group <- "Tropical/subtropical dry broadleaf forests"
#     } else if (biome == "T_TSTCF") {
#       group <- "Tropical/subtropical coniferous forests"
#     } else if (biome == "T_TBMF") {
#       group <- "Temperate broadleaf & mixed forests"
#     } else if (biome == "T_TCF") {
#       group <- "Temperate coniferous forests"
#     } else if (biome == "T_BFT") {
#       group <- "Boreal forests/taiga"
#     } else if (biome == "T_MFWS") {
#       group <- "Mediterranean forests, woodlands, and scrublands"
#     } else if (biome == "T_DXS") {
#       group <- "Deserts"
#     } else if (biome == "T_M") {
#       group <- "Mangroves"
#     } 
#     biome_labels[i,"biome_label"] <- group
#   }
#   biome_labels <- as.data.frame(biome_labels)
#   output <- bind_cols(input,biome_labels)
#   output <- filter(output,!is.na(biome_labels))
#   return(output) 
# }
# 
# assignOutLabels <- function(input,output){
#   rows <- c(1:nrow(input))
#   out_labels <- matrix(nrow=nrow(input),ncol=1)
#   rownames(out_labels) <- rows
#   colnames(out_labels) <- c("out_label")
#   
#   for (i in rows){
#     out <- input$Out_subtype[i]
#     if (is.na(out)) {
#       group <- "NA"
#     } else if (biome == "T_TSTMBF") {
#       group <- "Tropical/subtropical moist broadleaf forests"
#     } else if (biome == "T_TSTDBF") {
#       group <- "Tropical/subtropical dry broadleaf forests"
#     } else if (biome == "T_TSTCF") {
#       group <- "Tropical/subtropical coniferous forests"
#     } else if (biome == "T_TBMF") {
#       group <- "Temperate broadleaf & mixed forests"
#     } else if (biome == "T_TCF") {
#       group <- "Temperate coniferous forests"
#     } else if (biome == "T_BFT") {
#       group <- "Boreal forests/taiga"
#     } else if (biome == "T_MFWS") {
#       group <- "Mediterranean forests, woodlands, and scrublands"
#     } else if (biome == "T_DXS") {
#       group <- "Deserts"
#     } else if (biome == "T_M") {
#       group <- "Mangroves"
#     } 
#     biome_labels[i,"biome_label"] <- group
#   }
#   biome_labels <- as.data.frame(biome_labels)
#   output <- bind_cols(input,biome_labels)
#   output <- filter(output,!is.na(biome_labels))
#   return(output) 
# }
# 
# assignAffilLabels
# assignCompLabels
# assignStudyLabels
# assignDesignLabels

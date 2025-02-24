# This script opens the various datasets from BIOL, converts and tidies them, and saves them as RData objects for use within the package.
library(tidyverse)
library(readxl)
library(ape)
library(janitor)

################################################################################################
# Fisher et al. 2001
################################################################################################
# Nice and simple dataset on marsupial body weight, life-span and population densities

marsupials <- read_excel("data_raw/Fisher2001/marsupial_population_density.xls")
# add lifespan from different file:
lifespan <- read_excel("data_raw/Fisher2001/full_marsupial_dataset.xlsx", sheet = 2, na = "_")
marsupials <- left_join(marsupials, lifespan, by = c("Family", "Genus name", "Species name"))
colnames(marsupials) <- c("Family", "Genus", "Species", "FemaleMass", "PopDensity", "MaxLifeSpan")

save(marsupials, file = "data/marsupials.RData")

# for practicing data import:
write_csv(marsupials, "data_raw/Fisher2001/marsupials.csv")

#load(file = "data/marsupials.RData")

################################################################################################
# Riginos et al. 2014
################################################################################################

# Convert phylogenetic tree into phylo object:

reefFishPhylogeny <- read.nexus("data_raw/Riginos2014/Constrained_chronogram.nex")
#plot(reefFishPhylogeny)
save(reefFishPhylogeny, file = "data/reefFishPhylogeny.RData")
#load(file = "data/reefFishPhylogeny.RData")

# Import and convert two other datasets from this paper:
reefFishDiversity <- read_csv("data_raw/Riginos2014/Reeffams_diversity.csv")
reefFishDiversity <- reefFishDiversity[-1]
save(reefFishDiversity, file = "data/reefFishDiversity.RData")

reefFishFST <- read_csv("data_raw/Riginos2014/Species_Fst.csv") |>
  select(Reference, Fam_Spp, Egg, Marker, FST)
save(reefFishFST, file = "data/reefFishFST.RData")

################################################################################################
# McGuigan et al. 2005
################################################################################################

rainbowfishG <- matrix(c(2.833, 2.690, 3.214, 3.980, 2.246, 3.538,
                         2.690, 2.816, 2.990, 3.704, 1.493, 3.493,
                         3.214, 2.990, 2.396, 3.971, 1.840, 2.809,
                         3.980, 3.704, 3.971, 5.709, 1.718, 4.225,
                         2.246, 1.493, 1.840, 1.718, 4.419, 1.662,
                         3.538, 3.493, 2.809, 4.225, 1.662, 3.111),
    nrow = 6,
    dimnames = list(c("bodyL", "predorsL", "headD", "bodyD", "caudPedL", "caudPedD"),
                    c("bodyL", "predorsL", "headD", "bodyD", "caudPedL", "caudPedD")))
save(rainbowfishG, file = "data/rainbowfishG.RData")


################################################################################################
# McGuigan et al. 2007
################################################################################################

# Matrices of phenotypic and genetic variance/covariances in Drosophila wing traits

drosophilaWingG <- as.matrix(read_csv("data_raw/McGuigan2007/McGuigan2007_Drosophila_Gmatrix.csv")[31:40, 4:13])
rownames(drosophilaWingG) <- colnames(drosophilaWingG)
save(drosophilaWingG, file = "data/drosophilaWingG.RData")

################################################################################################
# Yamamichi et al. 2018
################################################################################################

# four data frames in "untidy" format, each containing data for a single dependent variable

phytoplankton <- list(chlorophyllA = read_csv("data_raw/Yamamichi2018/fig/figS3/chla.csv"),
                      biovolume = read_csv("data_raw/Yamamichi2018/fig/figS3/biomass.csv"),
                      sestonCarbon = read_csv("data_raw/Yamamichi2018/fig/figS3/sestonC.csv"),
                      attenuation = read_csv("data_raw/Yamamichi2018/fig/figS3/attenuation.csv"))

save(phytoplankton, file = "data/phytoplankton.RData")


################################################################################################
# Letten et al. 2018
################################################################################################

# Nice dataset that's already in tidy format and should be great for ggplot examples

nectarYeast <- read_csv("data_raw/Letten2018/dryad-mixed-culture-exps.csv") |>
  mutate(CFUs = round(count * dilution / 50)) |>
  select(strain, rep, exp, treat.env, treat.bio, mixID, AA.mM, sucrose, timepoint, CFUs, validity, notes)

save(nectarYeast, file = "data/nectarYeast.RData")


################################################################################################
# Gardiner et al. 2019
################################################################################################

oxley <- list(trees = read_csv("data_raw/Gardiner2019/Oxley_Project_1/data/Oxley_tree_data_JoAE_22_08_19.csv"),
              soils = read_csv("data_raw/Gardiner2019/Oxley_Project_1/data/Oxley_soil_data_JoAE_22_08_19.csv"),
              plots = read_csv("data_raw/Gardiner2019/Oxley_Project_1/data/Oxley_plot_data_JoAE_22_08_19.csv"),
              species = read_csv("data_raw/Gardiner2019/Oxley_Project_1/data/Oxley_final_mixes_JoAE_22_08_19.csv"))
save(oxley, file = "data/oxley.RData")


################################################################################################
# Walter et al. 2018
################################################################################################

senecio <- read_csv("data_raw/Walter2018/Data_Exp1_Dmatrix.csv")
senecio$Ecotype[senecio$Ecotype == "Dune_N"] <- "Dune"
senecio$Ecotype[senecio$Ecotype == "Head_N"] <- "Headland"
save(senecio, file = "data/senecio.RData")


################################################################################################
# Brown 2019
################################################################################################

bunyas <- load("data_raw/Brown2019/bunyas.Rdata")
bunyas <- list(abundances = bunyas_abund_matrix,
               sites = bunyas_site_data,
               traits = bunyas_trait_data,
               trees = bunyas_tree_data)
save(bunyas, file = "data/bunyas.Rdata")

################################################################################################
# Eriksen et al. 2019
################################################################################################

copepods <- read_csv("data_raw/Eriksen2019/IMOS_Zooplankton_Abundance_and_Biomass_copepod_data.csv") |>
  filter(Project == "NRS") |>
  filter(!(StationName %in% c("Esperance", "PH4", "Ningaloo", "VBM100 - Bonney Coast"))) |>
  select(-Project, -Year_Local, -Month_Local, -Day_Local, -Time_Local24hr, -StationCode, -TripCode, -AshFreeBiomass_mgm3) |>
  select(-(CTDSST_degC:CTDSalinity_psu))

save(copepods, file = "data/copepods.RData")

################################################################################################
# Bird et al. 2025
################################################################################################

# Almost-published dataset (as of Jan 2025) from Annabel Smith's lab, containing camera trapping
# data of mammals in rainforest habitats affected by fire.

rainforestFire <- list(images = read_csv("data_raw/Bird2025/Raw.Image.Data.csv"),
                       sites = read_csv("data_raw/Bird2025/Site.Data.csv"))

rainforestFire$images <- rainforestFire$images |>
  select(!Trigger:Orientation) |>
  select(!c(Old_ID, Old_Rank, Habitat)) |>
  rename(Taxonomic_Rank = Taxinomic_Rank)

rainforestFire$sites <- rainforestFire$sites |>
  rename(Fire_habitat_category = Fire_habitat_catergory)

save(rainforestFire, file = "data/rainforestFire.RData", compress = "xz")

################################################################################################
# Ubide et al. 2022
################################################################################################

# Dataset of published chemical compositions of ocean islands.

dat <- read_excel("data_raw/Ubide2022/Ubide2022_SuppTables.xlsx",
                        sheet = "Table S6",
                        range = "A2:AV31503",
                        guess_max = 31502) |>
  clean_names() |>
  mutate(across(si_o2_wt_percent:loi_wt_percent, as.double))

refs <- read_excel("data_raw/Ubide2022/Ubide2022_SuppTables.xlsx",
                   sheet = "Table S6",
                   range = "A31509:A32875",
                   col_names = "key") |>
  slice(c(-1352, -1353)) |>
  mutate(key = ifelse(substr(key, 1, 1) =="[", key, paste0("[NA] ", key))) |>
  separate_wider_regex(key, pattern = c(key = "^.*?\\]", reference = "(?<=\\]).*")) |>
  mutate(reference = str_sub(reference, 2)) |>
  mutate(key = ifelse(key == "[NA]", NA, key))

volcanoes <- list(dat = dat, references = refs)

save(volcanoes, file = "data/volcanoes.RData", compress = "xz")

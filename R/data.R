# This file contains documentation for all datasets that are part of this package.
# Run "devtools::document()" to compile this into .Rd doc files.

#' Marsupial data
#'
#' A dataset containing the body weight, population density and maximum life-span of 102 species of marsupials.
#'
#' @format A data frame with 102 rows and 6 columns. Each row corresponds to a different species and is described by the following variables:
#' \describe{
#'   \item{Family}{The name of the family that the species belongs to}
#'   \item{Genus}{The name of the genus that the species belongs to}
#'   \item{Species}{The specific epithet of the species}
#'   \item{FemaleMass}{Mean female mass, in grams}
#'   \item{PopDensity}{Population density, as individuals per hectar}
#'   \item{MaxLifeSpan}{Maximum life-span, in years}
#' }
#' @source Fisher, D.O., Owens, I.P.F. & Johnson, C.N. The ecological basis of life history variation in marsupials. Ecology 82:3531-3540 (2001)
#'
#'     The data are also freely available on Ecological Archives E082-042-A1, at
#' \url{http://esapubs.org/archive/ecol/E082/042/appendix-A.htm}
"marsupials"


#' Reef fish phylogenetic tree
#'
#' A dataset specifying a dated phylogenetic tree of 154 species of reef fish.
#'
#' @format Phylogenetic tree as an object of class phylo, as specified in the ape package. When ape is loaded, the object will be displayed and handled as a phylogenetic tree. When ape is not loaded, this is simply a list with the following elements:
#' \describe{
#'   \item{edge}{matrix where each row corresponds to an edge (branch) of the tree and the two columns identify the node numbers that the branch connects}
#'   \item{edge.length}{Branch lengths in million years}
#'   \item{Nnode}{Number of nodes in the tree}
#'   \item{node.label}{Labels for some of the internal nodes in the tree}
#'   \item{tip.label}{Species names at the tips of the tree}
#' }
#' @source Riginos, C., Buckley, Y.M., Blomberg, S.P. & Treml, E.A. 2014.
#' The American Naturalist 184: 52-64 (2014)
"reefFishPhylogeny"


#' Reef fish FST data
#'
#' A dataset containing published estimates of FST values for 114 species of reef fish.
#'
#' @format A data frame with 114 rows and 12 columns. Each row corresponds to a different species and the most important variables are:
#' \describe{
#'   \item{Reference}{Reference number to original article (see source)}
#'   \item{Fam_Spp}{String containing family and species names}
#'   \item{Egg}{Reproductive strategy with respect to eggs}
#'   \item{Marker}{Type of genetic marker used in the study}
#'   \item{FST}{FST estimate}
#' }
#' @source Riginos, C., Buckley, Y.M., Blomberg, S.P. & Treml, E.A. 2014.
#' The American Naturalist 184: 52-64 (2014)
"reefFishFST"

#' Reef fish diversity data
#'
#' A dataset containing data on species richness and divergence of 55 families of reef fishes.
#'
#' @format A data frame with 55 rows and 6 columns. Each row corresponds to a different family and the variables are:
#' \describe{
#'   \item{Phyl_name}{String containing family and species names}
#'   \item{Spp_rich}{Species richness (number of species within the family)}
#'   \item{Divergence}{Maximum divergence in COI gene (an estimate for crown age)}
#'   \item{Egg}{Reproductive strategy with respect to eggs, with two states distinguished (pelagic or demersal)}
#'   \item{Egg_3}{Reproductive strategy with respect to eggs, with three states distinguished (pelagic, demersal or scatterer)}
#' }
#' @source Riginos, C., Buckley, Y.M., Blomberg, S.P. & Treml, E.A. 2014.
#' The American Naturalist 184: 52-64 (2014)
"reefFishDiversity"


#' Rainbowfish G matrix
#'
#' A matrix containing variances and covariances in morphological traits of the Lake Eacham rainbow fish (Melanotaenia eachamensis).
#'
#' @format A symmetrical 6x6 matrix where elements on the diagonal are estimated variances (x1000) and off-diagonal elements are covariances (x1000) in the following traits:
#' \describe{
#'   \item{bodyL}{Standard length}
#'   \item{predorsL}{Predorsal length}
#'   \item{headD}{Head depth}
#'   \item{bodyD}{Body depth}
#'   \item{caudPedL}{Caudal peduncle length}
#'   \item{caudPedD}{Caudal peduncle depth}
#' }
#' @source McGuigan, K., Chenoweth, S.F., Blows, M.W. Phenotypic divergence along lines of genetic variance. American Naturalist 165: 32–43 (2005)
#'
#' and
#'
#' McGuigan, K. Studying phenotypic evolution using multivariate quantitative genetics. Molecular Ecology 15: 883-896 (2006)
"rainbowfishG"


#' Drosophila G matrix
#'
#' A matrix containing estimated genetic variances and covariances in morphological wing traits of female Drosophila bunnanda flies.
#'
#' @format A symmetrical 10x10 matrix where elements on the diagonal are estimated variances and off-diagonal elements are covariances in ten interlandmark distances between nine wing landmarks (see Fig. 1 in McGuigan & Blows 2007).
#' @source McGuigan, K., Blows, M.W. The phenotypic and genetic covariance structure of Drosophilid wings. Evolution 61: 902–911 (2007)
"drosophilaWingG"


#' Phytoplankton data
#'
#' Data from an experiment investigating the impact of light on phytoplankton abundance.
#'
#' @format A list containing four data frames, one for each measured variable:
#' \describe{
#'   \item{chlorophyllA}{Chlorophyll A concentration (μg/L)}
#'   \item{biovolume}{Total biovolume of phytoplankton (μm3/mL)}
#'   \item{sestonCarbon}{Seston carbon (μmol/L)}
#'   \item{attennuation}{Light attenuation coefficient (1/m)}
#' }
#' Each of these data frame has pond ID in the first column, with two ponds each with low light exposure (202L and 219L), two ponds with medium light exposure (204M and 218M), and two controls with high light exposure (203H and 217H). The remaining columns contain the measured variable for each date of measurements.
#' @source Yamamichi, M., Kazama, T., Tokita, K., Katano, I., Doi, H., Yoshida, T., Hairston Jr, N.G. Urabe, J. A shady phytoplankton paradox: when phytoplankton increases under low light. Proceedings of the Royal Society B 285: 20181067 (2018)
#' \url{http://dx.doi.org/10.1098/rspb.2018.1067}
"phytoplankton"



#' Nectar yeast data
#'
#' Data from an experiment investigating growth of different nectar yeasts in both monoculture and competition.
#'
#' @format A data frame with 12 columns:
#' \describe{
#'   \item{strain}{Focal species of yeast, including Metschnikowia reukaufii (Mr), M. koreensis (Mk), M. gruessii (Mg) and Starmerella bombicola (Sb)}
#'   \item{rep}{Replicate experiment}
#'   \item{exp}{Experiment ID (one of two batches)}
#'   \item{treat.env}{Treatment environment, either constant (constant sucrose levels) or variable (fluctuating sucrose levels)}
#'   \item{treat.bio}{Monoculture (mono) or competition (comp) between two species}
#'   \item{mixID}{For competition experiments, the species identities of the two competing species}
#'   \item{AA.mM}{Amino acid concentration in mM}
#'   \item{sucrose}{Sucrose levels, osmotic pressure in percent}
#'   \item{timepoint}{Time in hours}
#'   \item{CFUs}{Number of yeast cells ("colony-forming units") per uL}
#'   \item{validity}{Should data be included in analysis?}
#'   \item{notes}{Notes}
#' }
#'
#' @source Letten, A.D. Dhami, M.K., Ke, P.J., T Fukami, T. Species coexistence through simultaneous fluctuation-dependent mechanisms. Proceedings of the National Academy of Sciences 115 (26), 6745-6750 (2018).
#' \url{https://www.pnas.org/content/115/26/6745.full}
"nectarYeast"


#' Oxley seedling survival data
#'
#' Data from an experiment investigating tree seedling survival and growth to inform subtropical rainforest restoration efforts
#'
#' @format A list containing four data frames:
#' \describe{
#'   \item{trees}{}
#'   \item{soils}{}
#'   \item{plots}{}
#'   \item{species}{}
#' }
#'
#' @source Gardiner, R., Shoo, L.P., Dwyer, J.M. Look to seedling heights, rather than functional traits, to explain survival during extreme heat stress in the early stages of subtropical rainforest restoration. Journal of Applied Ecology 56:2687-2697 (2019).
#' DOI: 10.1111/1365-2664.13505, \url{https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/1365-2664.13505}
"oxley"


#' Senecio lautus morphological data
#'
#' Data from an experiment where seeds from the Australian wildflower \emph{Senecio lautus} were collected, grown in glasshouses under uniform conditions, and a range of morphological traits measured. The seeds were collected from populations belonging to four distinct ecotypes (dune, headland, tableland and woodland) that may be in the process of forming separate species.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{Ecotype}{One of four ecotypes (dune, headland, tableland woodland)}
#'   \item{Population}{Population ID}
#'   \item{VegHeight}{Vegetative height of the plant, in mm}
#'   \item{MSL_W}{Ratio between main stem length and mean plant width}
#'   \item{SB}{Number of branches}
#'   \item{MSD}{Main stem diameter, in mm}
#'   \item{Area}{Leaf area, in mm2}
#'   \item{P2A2}{Leaf perimeter squared / area squared (an indicator of leaf complexity)}
#'   \item{Circularity}{Leaf circularity}
#'   \item{Nindents.Peri}{Number of leaf indents divided by leaf perimeter}
#'   \item{IndentWidth}{Leaf indent width, in mm}
#'   \item{IndentDepth}{Leaf ndent depth, in mm}
#' }
#'
#' @source Walter, G.M., Aguirre, J.D., Blows, M.W. & Ortiz-Barrientos, D. Evolution of Genetic Variance during Adaptive Radiation. The American Naturalist 191: E108–E128 (2018), \url{https://www.journals.uchicago.edu/doi/10.1086/696123}
"senecio"


#' Bunya Mountains tree data
#'
#' Tree community dataset from rainforests and vine thickets of the Bunya Mountains, QLD, Australia.
#'
#' @format A list containing four data frames:
#' \describe{
#'   \item{abundances}{Abundance matrix of tree species recorded at five locations and subplots therein.Each row represents a 25 x 25 m subplot within a location. The columns (except for columns 1 and 2) are the names of rainforest tree species. The values within cells are the recorded abundance of each species in each subplot.}
#'   \item{sites}{Topographically-corrected climate data for the five locations extracted from CSIRO dat portal (Harwood et al. 2016).}
#'   \item{traits}{Functional trait data for most of the species. Values are averages taken from multiple individuals of each species. Traits include: ‘log_lamina_sla’ = log-transformed specific leaf area (original units mm2/mg-1), ‘log_lamina_area’ = log-transformed lamina (leaf) area (’lamina’ is the leaf blade excluding petioles and rachis; original units mm2), ‘lamina_ldmc’ = leaf dry matter content (dry mass over fresh mass), ‘tlp ‘= leaf turgor loss point (MPa), ‘wd’ = wood density (fresh volume / dry mass), ‘log_t_b’ = log(xylem wall thickness / xylem lumen breadth) (a proxy for conduit reinforcement), ‘sqrt_mh’ = sqrt-transformed maximum height (potential height of each tree species; original units m).}
#'   \item{trees}{Tree-scale data with diameter measurements. One row per individual tree, with species identity and a range of diameter measurements: ‘max_diam’ = the diameter of the largest stem. For most trees that have a single stem, this is just the diameter at breast height (1.3 m), ‘ba_m2’ = the basal area of each stem. For multi-stemmed individuals this is the sum of the cross-sectional area of all stems, ‘combined_diam’ = the diameter equivalent of the basal area. For single-stemmed trees this is the same as ‘max_diam’.}
#'
#'   Dates: Tree survey data for ‘high’, ‘mid_east’ and ‘mid_west’ were collected by Don Butler in 2003. All trait data and tree survey data for ‘low_east’ and ‘low_west’ were collected by Alison Brown and John Dwyer in 2019.
#'
#'   Location: Bunya Mountain National Park, Queensland Australia. Five areas of rainfortest or vine thicket were surveyed along a topographic moisture gradient:
#' 'high' (easting = 359342, northing = 7027160)
#' 'low_east' (365676, 7028551)
#' 'low_west' (350888, 7030875)
#' 'mid_east' (360196, 7027612)
#' 'mid_west' (354971, 7031460)
#' All eastings and northings in WGS84, zone 56.
#' }
#'
#' @author Alison Brown \email{alison.brown25@gmail.com}
#' @author Don Butler \email{don.butler@des.qld.gov.au}
#' @author John Dwyer \email{j.dwyer2@uq.edu.au}
#'
#' @source Brown, A. 2019. How does moisture stress structure angiosperm communities in subtropical rainforests? Honours Thesis. The University of Queensland, Brisbane.
#' @source Harwood, T., R. Donohue, I. Harman, T. McVicar, N. Ota, J. Perry, and K. Williams. 2016. 9s climatology for continental Australia 1976-2005: Summary variables with elevation and radiative adjustment v3. CSIRO Data Collection.
"bunyas"


#' Copepod abundances
#'
#' Copepod abundance through time at several research stations across Australia.
#'
#' Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS).  It is operated by a consortium of institutions as an unincorporated joint venture, with the University of Tasmania as Lead Agent.
#'
#' @format A tibble containing the following columns:
#' \describe{
#'   \item{StationName}{}
#'   \item{Latidude}{}
#'   \item{Longitude}{}
#'   \item{SampleTime_UTC}{}
#'   \item{SampleTime_Local}{}
#'   \item{SampleDepth_m}{}
#'   \item{Biomass_mgm3}{}
#'   \item{Species abundances (estimated abundances of various copepod species, as number of individuals per m3)}{}
#'   }
#'
#' @source Eriksen, Ruth S., Claire H. Davies, Pru Bonham, Frank E. Coman, Steven Edgar, Felicity R. McEnnulty, David McLeod, et al. “Australia’s Long-Term Plankton Observations: The Integrated Marine Observing System National Reference Station Network.” Frontiers in Marine Science 6 (2019). https://doi.org/10.3389/fmars.2019.00161.
"copepods"


#' Rainforest fire and mammal diversity
#'
#' Camera trap data to on mammal communities in rainforest margins that are affected to varying degrees by wildfires.
#'
#'
#' @format A list containing two tibbles:
#' \describe{
#'   \item{images}{Table of camera images taken at different sites, and with identified animals for each picture.}
#'   \item{sites}{Table of sites, including geographical location, fire habitat category, and various other information.}
#'   }
#'
#' @source Bird, R.R., Zsoldos, R.R., Jimenez Sandoval, M.V., Watson, S.J. & Smith, A.L. Wildfire in rainforest margins is associated with variation in mammal diversity and habitat use. (under review)
"rainforestFire"

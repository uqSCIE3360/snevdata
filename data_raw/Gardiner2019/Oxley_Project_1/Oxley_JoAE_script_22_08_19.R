## This script compiles, tidies and formats raw data, runs all analyses and produces all figures in the manuscript

## The original analysis was conducted by RG as part of his Honours research at UQ (with help from JMD)

## Additional analyses, revision analyses and script tidying was done by JMD

rm(list=ls())

## packages we use
library(lme4)
library(MuMIn)
library(lmerTest)
library(sjPlot)
library(tidyverse)


## load our personalised functions
source("R/JMD_Oxley_functions.R")
## look at the names of these
ls()


##################################################
###### LOAD AND COMPILE THE VARIOUS DATASETS #####
##################################################

## species scale data (taxonomic information)
spec_data<-read_csv("Data/Oxley_final_mixes_JoAE_22_08_19.csv") %>%
        select(species_map_code, species, genus, family)


### TRAITS ###
## load leaf trait data
leaf_traits<-read_csv("Data/Trait data/leaf_traits_28_08_17.csv") %>%
group_by(species) %>%
summarise(sla = exp(mean(log(sla_mm2_mg), na.rm=T)),
			ldmc = mean(ldmc, na.rm=T),
			leaf_area = exp(mean(log(leaf_area_mm2), na.rm=T)),
			leaflet_area = exp(mean(log(leaflet_area_mm2), na.rm=T)))
			

## wood density and root traits
wood_root_traits<-read_csv("Data/Trait data/wood_&_root_traits_28_08_17.csv") %>%
group_by(species) %>%
summarise(wd = mean(wood_density_mg_mm3, na.rm=T),
			srl = exp(mean(log(srl_m_g), na.rm=T)),
			rtd = exp(mean(log(rtd_mg_mm3), na.rm=T)))

## data to calculate average root diameters			
root_mean_diams<-read_csv("Data/Trait data/mean_root_diam_data.csv") %>%
			group_by(species) %>%
			summarise(species_mean_root_diams=mean(mean_diam_mm))
			

## combine into a convenient dataframe called traits
traits<-leaf_traits %>%
      left_join(wood_root_traits, by='species') %>%
      mutate(compound = if_else(leaf_area>leaflet_area, 1, 0),
            sqrt_leaflet_area = sqrt(leaflet_area),
            log_sla = log(sla),
            log_srl = log(srl),
            std_compound = (compound - mean(compound)) / (2*sd(compound)),
            std_sqrt_leaflet_area = scale(sqrt(leaflet_area))[,1],
            std_log_sla = scale(log_sla)[,1])               

trait_pca<-princomp(data.frame(traits[c("log_sla", "ldmc", "sqrt_leaflet_area", "compound", "log_srl", "rtd", "wd")]), cor = TRUE)

pdf(file="Outputs/Figure_1_trait_PCA.pdf", height=6, width=6,useDingbats=F)   
biplot(trait_pca) ## the version in the text has been formatted additionally (obviously)
dev.off()

summary(trait_pca)
write.csv(loadings(trait_pca), "Outputs/Table_S2_trait_loadings.csv")


### SOIL ###
soil_data<-read_csv("Data/Oxley_soil_data_JoAE_22_08_19.csv")

soil_pca<-princomp(data.frame(soil_data[c(13,14:20)]), cor = TRUE)

pdf(file="Outputs/Figure_S2_soil_PCA.pdf", height=6, width=6,useDingbats=F) 
biplot(soil_pca)
dev.off()

write.csv(loadings(soil_pca), "Outputs/Table_S2_soil_loadings.csv")

## add standadised PCA axes to soil data
soil_data$std_soil_pc1<-scale(soil_pca$scores[,1])[,1]	
soil_data$std_soil_pc2<-scale(soil_pca$scores[,2])[,1]	
soil_data$std_soil_pc3<-scale(soil_pca$scores[,3])[,1]



## final join to tree scale data and some removal of non-study species
## we will also standardise the predictors 
## tree-scale data
trees<-read_csv("Data/Oxley_tree_data_JoAE_22_08_19.csv") %>%
  ## divide plots into quadrants to match to soil samples
  mutate(quadrant = if_else(x<15 & y<15, "S1", if_else(x<15 & y>15, "S2", if_else(x>15 & y>15, "S3", "S4"))))

## plots 13 and 14 were not square, so quadrants were slightly different
## JMD's tidyverse skills failed here
trees$quadrant[trees$plot==13 | trees$plot==14]<-with(trees[trees$plot==13 | trees$plot==14,], 
                                             if_else(x<21.5 & y<10, "S1", if_else(x<21.5 & y>10, "S2", 
                                                                                  if_else(x>21.5 & y>10, "S3", "S4"))))

## plot scale data
plot_elevation_data<-trees %>%
  group_by(plot) %>%
  summarise(plot_min_elevation = min(plant_elevation, na.rm=T),
            plot_mean_elevation = mean(plant_elevation, na.rm=T))

plot_data<-read_csv("Data/Oxley_plot_data_JoAE_22_08_19.csv") %>%
  left_join(plot_elevation_data, by='plot') %>%
  mutate(first_mon_period_days = difftime(strptime(date_first_monitor, format="%d/%m/%Y"), 
                                          strptime(date_first_mapped, format="%d/%m/%Y"), units="days")) %>%
  select(paddock, plot_pair, plot, mix, day_1_tmax, plot_min_elevation, plot_mean_elevation, first_mon_period_days)


## and here we continue with the data compiling
trees<- trees %>%
    left_join(plot_data, by = 'plot') %>%
    mutate(relative_elevation = plant_elevation - plot_min_elevation) %>%
    left_join(spec_data, by = 'species_map_code') %>%
    left_join(traits, by = c("species_map_code" = "species")) %>%
    left_join(soil_data, by = c("plot", "quadrant")) %>%
    filter(!is.na(trees$species_map_code) & species_map_code!="UNKNOWN" & species_map_code!="ACMA" & species_map_code!="JAPS") %>%
    mutate(std_sqrt_height_cm_when_mapped = scale(sqrt(height_cm_when_mapped))[,1],
          std_day_1_tmax = scale(day_1_tmax)[,1],
          std_relative_elevation = scale(relative_elevation)[,1],
          std_plot_mean_elevation = scale(plot_mean_elevation)[,1],
          daily_height_increment = (height_cm_first_monitor - height_cm_when_mapped) / as.numeric(first_mon_period_days),
          plot_factor = as.factor(plot),
          block_factor = as.factor(plot_pair),
          paddock_factor = as.factor(paddock))




#################################
## NOW WE START DOING ANALYSES ##
#################################

#####################
## SURVIVAL MODELS ##
#####################

## was there an overall difference between species mixtures (fast versus hardy mixes) in survival
## treatment (species mix) effect
mix_model<-glmer(alive_first_monitor ~ -1 + mix + (1|paddock_factor/block_factor/plot_factor) + (mix|block_factor), family=binomial, data=trees)
summary(mix_model)
## extract the estimated probabilities
plogis(fixef(mix_model))

## are the mixtures significantly different in overall survival probabilities?
mix_model_test<-glmer(alive_first_monitor ~ mix + (1|paddock_factor/block_factor/plot_factor) + (mix|block_factor), family=binomial, data=trees)
summary(mix_model_test)


surv.mod<-glmer(alive_first_monitor ~ 	
										## Environmental variables
										std_day_1_tmax + std_soil_pc1 + std_soil_pc3 + std_plot_mean_elevation + std_relative_elevation + 
										std_day_1_tmax:std_soil_pc1 + std_day_1_tmax:std_soil_pc3 + 
										std_day_1_tmax:std_plot_mean_elevation + std_day_1_tmax:std_relative_elevation +
										std_sqrt_height_cm_when_mapped + I(std_sqrt_height_cm_when_mapped^2) + 
										std_log_sla + std_sqrt_leaflet_area + std_compound +
										std_sqrt_height_cm_when_mapped:std_log_sla + I(std_sqrt_height_cm_when_mapped^2):std_log_sla +
										std_sqrt_height_cm_when_mapped:std_sqrt_leaflet_area + I(std_sqrt_height_cm_when_mapped^2):std_sqrt_leaflet_area +
										std_sqrt_height_cm_when_mapped:std_compound + I(std_sqrt_height_cm_when_mapped^2):std_compound +
										(1|paddock_factor/block_factor/plot_factor) + 
										(std_sqrt_height_cm_when_mapped + I(std_sqrt_height_cm_when_mapped^2)|species_map_code), family=binomial, data=trees)

summary(surv.mod)
r.squaredGLMM(surv.mod)


####################
##### FIGURE 2a ####
####################
pdf(file="Outputs/Figure_2a_survival_coef_plot.pdf", height=12, width=6,useDingbats=F)   
plot_model(surv.mod, type = "est", transform=NULL, order.terms=c(1:5,11:14,6:10,15,17,19,16,18,20), group.terms=c(1,1,1,1,1,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2), colors = c("darkgoldenrod", "darkcyan"), title = "", 
axis.labels=rev(c("Max. temperature", "Nutrients (soil PC1)", "Bulk density (soil PC3)", "Mean elevation of plot", "Rel. elevation within plot",
	"Max. temperature x Nutrients (soil PC1)", "Max. temperature x Bulk density (soil PC3)", "Max. temperature x Mean elevation of plot", "Max. temperature x Rel. elevation within plot",
	"sqrt(Seedling height)", "Seedling height", "log(SLA)", "sqrt(Lamina area)", "Compound leaves", 
	"sqrt(Seedling height) x log(SLA)", "sqrt(Seedling height) x sqrt(Lamina area)", "sqrt(Seedling height) x Compound leaves", 
	"Seedling height x log(SLA)", "Seedling height x sqrt(Lamina area)", "Seedling height x Compound leaves")))
dev.off()





###################
##### FIGURE 3 ####
###################

pdf(file=paste("Outputs/Figure_3_temp_BD_interaction.pdf", sep=""), height=6, width=6,useDingbats=F)   

par(mar=c(4, 4, 2, 2), mgp=c(2.25, 0.75, 0))

## blank plot
with(trees, plot(jitter(alive_first_monitor, amount=0.05) ~ std_soil_pc3, ylab="Probability of survival", xlab="Soil PC3", xaxt="n", type="n", ylim=c(0.1, 0.8), cex.lab=1.25, xlim=c(-2.3,2)))

## axis in original soil PCA units
axis(side=1, at=seq(-2, 2.1, 0.5), labels=round(un.std.func(seq(-2, 2.1, 0.5), mean(trees$soil_pc3, na.rm=T), sd(trees$soil_pc3, na.rm=T)), 1))

##
plot.temp<-1.75	## hot planting day (in standardised units)
plot.height<- -0.25 ## we held initial heights constant at ~17cm for this plot

## prediction for hot day using our customised lmer predict function (adapted from Ben Bolker's code at https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html)
hot.pred<-lmer.predict(mod=surv.mod, newdat=data.frame(1, plot.temp, 0, seq.func(trees$std_soil_pc3), 0, 0, plot.height, plot.height^2, 0, 0, 0, 0, seq.func(trees$std_soil_pc3)*plot.temp, 0, 0, 
				plot.height*0, plot.height^2*0, plot.height*0, plot.height^2*0, plot.height*0, plot.height^2*0), se.mult=1.96, binom=T, poisson=F)[c(20:80),]

## plot 95% CIs using our customised function
plot.CI.func(x.for.plot=seq.func(trees$std_soil_pc3)[20:80], pred=hot.pred$y, upper=hot.pred$phi, lower=hot.pred$plo, env.colour="tomato", env.trans=50, line.colour="tomato", line.weight=3, line.type=1)

## add points and bars based on binned data using our 'logistic.plot' function
with(trees[trees$std_day_1_tmax>0.5,], logistic.plot(x=std_soil_pc3, y=alive_first_monitor,n.bins=25, colour="tomato", point.type=19))

## now add the same for the cool planting day
plot.temp<- -0.5 ## average temp planting day (in standardised units)
cold.pred<-lmer.predict(mod=surv.mod, newdat=data.frame(1, plot.temp, 0, seq.func(trees$std_soil_pc3), 0, 0, plot.height, plot.height^2, 0, 0, 0, 0, seq.func(trees$std_soil_pc3)*plot.temp, 0, 0, 
				plot.height*0, plot.height^2*0, plot.height*0, plot.height^2*0, plot.height*0, plot.height^2*0), se.mult=1.96, binom=T, poisson=F)[c(1:95),]
plot.CI.func(x.for.plot=seq.func(trees$std_soil_pc3)[1:95], pred=cold.pred$y, upper=cold.pred$phi, lower=cold.pred$plo, env.colour="dodgerblue4", env.trans=50, line.colour="dodgerblue4", line.weight=3, line.type=1)
with(trees[trees$std_day_1_tmax<=-0.5,], logistic.plot(x=std_soil_pc3, y=alive_first_monitor,n.bins=25, colour="dodgerblue4", point.type=19))

## and some cosmetic text
text(c(-2.3, -2.3), c(0.79, 0.76), c("Average maximum temperature", "Above average maximum temperature"), col=c("dodgerblue4", "tomato"), pos=4)
mtext("High soil BD", side=1, line = 2.1, adj=0)
mtext("Low soil BD", side=1, line = 2.1, adj=1)
arrows(-0.65, -0.01, -1.5, -0.01, xpd = TRUE, length=0.1)
arrows(0.375, -0.01, 1.25, -0.01, xpd = TRUE, length=0.1)

dev.off()




###################
##### FIGURE 4 ####
###################

## This PDF produces the panels that comprise Figure 4
## We arranged the panels in traitspace using illustrator (lazy? perhaps)
## first filter and select from the trait dataframe to get what we want
## remove Jagera that was planted later
traits_used<-traits%>%
  filter(species!="JAPS")%>%
  select(species, std_log_sla, std_sqrt_leaflet_area, std_compound)

## set x limits that will apply to all panels
all_spec_x_limits<-quantile(na.omit(trees$std_sqrt_height_cm_when_mapped), p=c(0.00005, 0.99995))

## now make the figure using an old-fashioned loop
pdf(file=paste("Outputs/Figure_4_component_plots.pdf", sep=""), height=12, width=18,useDingbats=F)   
par(mfrow=c(4,6), mar=c(4.5, 4, 4, 2), mgp=c(2.6,0.8,0)) 

for (i in 1:dim(traits_used)[1]){
  spec_plot_data<-droplevels(subset(trees, species_map_code==traits_used$species[i]))
  spec_x_limits<-range(na.omit(spec_plot_data$std_sqrt_height_cm_when_mapped))
  
  ## blank plot
  with(spec_plot_data, plot(jitter(alive_first_monitor, amount=0.05) ~ std_sqrt_height_cm_when_mapped, font.main=3, type="n",
                            ylab="Probability of survival", xlab="Height when mapped (cm)",xlim=all_spec_x_limits, 
                            main=paste0(traits_used$species[i], "; n = ", dim(spec_plot_data)[1]), xaxt="n"))
  ## add x axis in original height units (cm)
  axis(side=1, at=seq(-2.5, 3, 0.5), labels=round((un.std.func(seq(-2.5, 3, 0.5), mean(sqrt(trees$height_cm_when_mapped), na.rm=T), sd(sqrt(trees$height_cm_when_mapped), na.rm=T))^2), 0))
  
  ## add the rectangle indicating the 25-35 cm height interval
  rect(0.36, -0.08, 1.0, 1.08, col="grey90", border=NA)
  
  ## add the black points using our custum 'logistic.plot" function
  with(spec_plot_data, logistic.plot(std_sqrt_height_cm_when_mapped, alive_first_monitor, 7, "black", 19))
  
  ## add jittered binary points
  with(spec_plot_data, points(jitter(alive_first_monitor, amount=0.05) ~ std_sqrt_height_cm_when_mapped, col="grey"))
  
  ## grab the vector of trait values that we want to use
  sel_traits<-as.numeric(traits_used[i,c("std_log_sla", "std_sqrt_leaflet_area", "std_compound")])
  
  ## use curve to add the model predictions
  ## fixed-effects predictions first
  curve(plogis(cbind(1,0,0,0,0,0,x, x^2, sel_traits[1], sel_traits[2], sel_traits[3], 0,0,0,0, x*sel_traits[1], x^2*sel_traits[1], x*sel_traits[2], x^2*sel_traits[2], x*sel_traits[3], x^2*sel_traits[3])%*%fixef(surv.mod)), add=T, lwd=3, col="orange", lty=3)		
  curve(plogis(cbind(1,0,0,0,0,0,x, x^2, sel_traits[1], sel_traits[2], sel_traits[3], 0,0,0,0, x*sel_traits[1], x^2*sel_traits[1], x*sel_traits[2], x^2*sel_traits[2], x*sel_traits[3], x^2*sel_traits[3])%*%fixef(surv.mod)), add=T, lwd=3, col="orange", lty=1, from=spec_x_limits[1], to=spec_x_limits[2])
  
  ## random effects predictions
  species_randoms_full_mod<-as.numeric(coef(surv.mod)$species_map_code[i,])
  curve(plogis(cbind(1,0,0,0,0,0,x, x^2, sel_traits[1], sel_traits[2], sel_traits[3], 0,0,0,0, x*sel_traits[1], x^2*sel_traits[1], x*sel_traits[2], x^2*sel_traits[2], x*sel_traits[3], x^2*sel_traits[3])%*%species_randoms_full_mod), add=T, lwd=3, col="tomato", lty=3)
  curve(plogis(cbind(1,0,0,0,0,0,x, x^2, sel_traits[1], sel_traits[2], sel_traits[3], 0,0,0,0, x*sel_traits[1], x^2*sel_traits[1], x*sel_traits[2], x^2*sel_traits[2], x*sel_traits[3], x^2*sel_traits[3])%*%species_randoms_full_mod), add=T, lwd=3, col="tomato", lty=1, from=spec_x_limits[1], to=spec_x_limits[2])
  
  ## now run separate models for each species and use a quadratic if the squared term is significant
  spec_mod1<-glmer(alive_first_monitor ~ std_sqrt_height_cm_when_mapped + I(std_sqrt_height_cm_when_mapped^2) + (1|plot), family=binomial, data=spec_plot_data)
  spec_mod2<-glmer(alive_first_monitor ~ std_sqrt_height_cm_when_mapped + (1|plot), family=binomial, data=spec_plot_data)
  if(summary(spec_mod1)$coef[3,4]<0.05){
    spec_mod<-spec_mod1
    curve(plogis(cbind(1,x,x^2)%*%fixef(spec_mod)), add=T, lwd=3, col="black", lty=3)
    curve(plogis(cbind(1,x,x^2)%*%fixef(spec_mod)), add=T, lwd=3, col="black", lty=1, from=spec_x_limits[1], to=spec_x_limits[2])
    mtext("(significant quadratic)", cex=0.75)
  } else{spec_mod<-spec_mod2
  curve(plogis(cbind(1,x)%*%fixef(spec_mod)), add=T, lwd=3, col="black", lty=3)
  curve(plogis(cbind(1,x)%*%fixef(spec_mod)), add=T, lwd=3, col="black", lty=1, from=spec_x_limits[1], to=spec_x_limits[2])
  }
}

dev.off()



############
## GROWTH ##
############

## now for height growth (of plants that survived)

## remove stems that lost tissue for various reasons or height could not be reliably measured
notes_to_remove<-c("height of living", "leaning", "leaning and flowering", "leaning and re-shooting from base", "re-shot from base", "recent growth has died", "slight lean","snapped top", 
  "snapped top and reshot from base", "top dead and re-shot above base", "top dead and re-shot from base", "top eaten", "top snapped and re-shot above base")

## filter using these comments
living_and_growing <- trees %>%
        filter(alive_first_monitor == 1 & !notes_first_monitor %in% notes_to_remove) %>%
        filter(daily_height_increment>=0) ## still a few negative increments to be removed


##################
## GROWTH MODEL ##
##################

### full model ##
growth_mod<-lmer(sqrt(daily_height_increment) ~ 
										## Environmental variables
										std_day_1_tmax + std_soil_pc1 + std_soil_pc3 + std_plot_mean_elevation + std_relative_elevation + 
										std_day_1_tmax:std_soil_pc1 + std_day_1_tmax:std_soil_pc3 + std_day_1_tmax:std_plot_mean_elevation + std_day_1_tmax:std_relative_elevation + 
										## plant characteristics
										std_sqrt_height_cm_when_mapped + I(std_sqrt_height_cm_when_mapped^2) + 
										std_log_sla + std_sqrt_leaflet_area + std_compound +	 
										(1|plot_pair/plot) + (std_sqrt_height_cm_when_mapped + I(std_sqrt_height_cm_when_mapped^2)|species) + (1|genus), data=living_and_growing)

summary(growth_mod)
r.squaredGLMM(growth_mod)

## diagnostics
plot(growth_mod) ## zero growth is not handled very well, but larger values look ok
qqnorm(resid(growth_mod)); qqline(resid(growth_mod))


####################
##### FIGURE 2b ####
####################
pdf(file="Outputs/Figure_2b_growth_coef_plot.pdf", height=8, width=6,useDingbats=F)  
plot_model(growth_mod, type = "std", order.terms=c(1:5,11:14,6:10), group.terms=c(1,1,1,1,1,2,2,2,2,2,1,1,1,1), colors = c("darkgoldenrod", "darkcyan"), title = "", axis.lim =c(-0.5,1),
axis.labels=rev(c("Max. temperature", "Nutrients (soil PC1)", "Bulk density (soil PC3)", "Mean elevation of plot", "Rel. elevation within plot",
	"Max. temperature x Nutrients (soil PC1)", "Max. temperature x Bulk density (soil PC3)", "Max. temperature x Mean elevation of plot", "Max. temperature x Rel. elevation within plot",
	"sqrt(Seedling height)", "Seedling height", "Seedling economics (trait PC1)", "Leaf area (trait PC2)", "Compound leaves (trait PC3)")))

dev.off()







##################################
### GRAPHICAL ABSTRACT FIGURE ####
##################################

## blank plot
with(trees, plot(alive_first_monitor ~ std_sqrt_height_cm_when_mapped, font.main=3, type="n",
                          ylab="Probability of Survival", xlab="Initial Seedling Height (cm)",xlim=all_spec_x_limits, xaxt="n"))

## add x axis in original height units (cm)
axis(side=1, at=seq(-2.5, 3, 0.5), labels=round((un.std.func(seq(-2.5, 3, 0.5), mean(sqrt(trees$height_cm_when_mapped), na.rm=T), sd(sqrt(trees$height_cm_when_mapped), na.rm=T))^2), 0))

## add the rectangle indicating the 25-35 cm height interval
rect(0.36, -0.03, 1.0, 1.03, col="grey90", border=NA)

for (i in 1:dim(traits_used)[1]){
  spec_plot_data<-droplevels(subset(trees, species_map_code==traits_used$species[i]))
  spec_x_limits<-range(na.omit(spec_plot_data$std_sqrt_height_cm_when_mapped))
  
  ## grab the vector of trait values that we want to use
  sel_traits<-as.numeric(traits_used[i,c("std_log_sla", "std_sqrt_leaflet_area", "std_compound")])
  
  ## random effects predictions
  species_randoms_full_mod<-as.numeric(coef(surv.mod)$species_map_code[i,])
  curve(plogis(cbind(1,0,0,0,0,0,x, x^2, sel_traits[1], sel_traits[2], 
                     sel_traits[3], 0,0,0,0, x*sel_traits[1], x^2*sel_traits[1], x*sel_traits[2], 
                     x^2*sel_traits[2], x*sel_traits[3], x^2*sel_traits[3])%*%species_randoms_full_mod), 
        add=T, lwd=0.5, col="dodgerblue4", from=spec_x_limits[1], to=spec_x_limits[2])
}

## use curve to add the model predictions
## fixed-effects predictions first
x.for.plot<-seq.func(trees$std_sqrt_height_cm_when_mapped)
overall.pred<-lmer.predict(mod=surv.mod, newdat=data.frame(1, 0, 0, 0, 0, 0, x.for.plot, x.for.plot^2, 0, 0, 0, 0, 0*0, 0, 0, 
                                                           x.for.plot*0, x.for.plot^2*0, x.for.plot*0, x.for.plot^2*0, x.for.plot*0, x.for.plot^2*0), se.mult=1.96, binom=T, poisson=F)
plot.CI.func(x.for.plot=seq.func(trees$std_sqrt_height_cm_when_mapped), pred=overall.pred$y, upper=overall.pred$phi, lower=overall.pred$plo, 
             env.colour="dodgerblue4", env.trans=50, line.colour="dodgerblue4", line.weight=4, line.type=1)






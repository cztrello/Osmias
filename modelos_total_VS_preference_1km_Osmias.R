library(DHARMa)
library(lme4)
library(ggplot2)
library(lme4)
library(MASS)
library(MuMIn)
library(visreg)
library(stargazer)
library(jtools)
library(margins)
library(dplyr)
library(glmmADMB) 
library(outliers)
library(ggpubr)

#Esto es para instalar el paquete que me permite ajustar a una familia beta mis datos del cv ya que son proporciones entre 0 y 1 sin llegar a estos dos.
install_packages("R2admb")
devtools::install_github("bbolker/glmmadmb")

#Primero hago un dataframe para todos los datos de tamaño de celdas y otro para todos los datos 
#de distancia intertegular para poder hacer un modelo mixto con el factor especie como caña random.

Sexo_specie_it_total_cane<-read.csv("Sexo_specie_it_total_cane.csv")

Sexo_specie_it_total_cane<-split(Sexo_specie_it_total_cane, Sexo_specie_it_total_cane$Sex, drop=TRUE)

Sexo_specie_it_total_cane<-as.data.frame(Sexo_specie_it_total_cane)
Sexo_specie_it_total_cane_female<-Sexo_specie_it_total_cane[[1]]
Sexo_specie_it_total_cane_male<-Sexo_specie_it_total_cane[[2]]
Sexo_specie_it_total_cane_male<-as.data.frame(Sexo_specie_it_total_cane_male)
Sexo_specie_it_total_cane_female<-as.data.frame(Sexo_specie_it_total_cane_female)

dataframe_from_bombus_to_join_osmia<-read.csv("data_from_bombus_to_join_osmia")
abundance_percent_1km<-read.csv("~/Documents/Osmias/abundancia_percent_osmia_dataframe.csv")
abundance_percent_1km<-abundance_percent_1km[,c(5,6,7)]
colnames(abundance_percent_1km)[3]<-"Code"
colnames(data_from_bombus_to_join_osmia)[1]<-"Code"
data_from_bombus_to_join_osmia<-merge(data_from_bombus_to_join_osmia, abundance_percent_1km, by="Code")
data_from_bombus_to_join_osmia<-unique(data_from_bombus_to_join_osmia)
str(Canes)
Canes_to_join_with_size<-Canes[,c(1,15,30)]
Canes_to_join_to_sex<-Canes_to_join_with_size[,c(1,3)]
Canes_for_parasited<-Canes[,c(1,19,30)]
str(Canes_to_join_to_sex)
Canes_to_join_to_sex<-unique(Canes_to_join_to_sex)
Canes_to_join_solitary_bees<-merge(Canes_to_join_to_sex, data_from_bombus_to_join_osmia, by="Code")
Canes_to_join_solitary_bees_sex_it_female<-merge(Sexo_specie_it_total_cane_female, Canes_to_join_solitary_bees, by="number")
Canes_to_join_solitary_bees_sex_it_male<-merge(Sexo_specie_it_total_cane_male, Canes_to_join_solitary_bees, by="number")
Canes_for_parasited<-merge(Canes_for_parasited, data_from_bombus_to_join_osmia, by="Code")
head(Canes_for_parasited)
#Modelos hembras IT todos juntos de todas las especies
Canes_to_join_solitary_bees_sex_it_female$temp_mean<-scale(Canes_to_join_solitary_bees_sex_it_female$temp_mean)
Canes_to_join_solitary_bees_sex_it_female$abundancia_relativa<-scale(Canes_to_join_solitary_bees_sex_it_female$abundancia_relativa)
Canes_to_join_solitary_bees_sex_it_female$abundancia_relativa_1km<-scale(Canes_to_join_solitary_bees_sex_it_female$abundancia_relativa_1km)
str(Canes_to_join_solitary_bees_sex_it_female)

Canes_to_join_solitary_bees_sex_it_male$temp_mean<-scale(Canes_to_join_solitary_bees_sex_it_male$temp_mean)
Canes_to_join_solitary_bees_sex_it_male$abundancia_relativa<-scale(Canes_to_join_solitary_bees_sex_it_male$abundancia_relativa)
Canes_to_join_solitary_bees_sex_it_male$abundancia_relativa_1km<-scale(Canes_to_join_solitary_bees_sex_it_male$abundancia_relativa_1km)
str(Canes_to_join_solitary_bees_sex_it_male)

Canes_to_join_solitary_bees_sex_it_female$number<-as.factor(Canes_to_join_solitary_bees_sex_it_female$number)
Canes_to_join_solitary_bees_sex_it_male$number<-as.factor(Canes_to_join_solitary_bees_sex_it_male$number)

Canes_to_join_solitary_bees_sex_it_female <- within(Canes_to_join_solitary_bees_sex_it_female, sample <- factor(number:Specie))
Canes_to_join_solitary_bees_sex_it_male <- within(Canes_to_join_solitary_bees_sex_it_male, sample <- factor(number:Specie))


Canes_to_join_solitary_bees_sex_it_male<-Canes_to_join_solitary_bees_sex_it_male[complete.cases(Canes_to_join_solitary_bees_sex_it_male),]
Canes_to_join_solitary_bees_sex_it_female<-Canes_to_join_solitary_bees_sex_it_female[complete.cases(Canes_to_join_solitary_bees_sex_it_female),]


it_female_total<-glmer(IT ~ temp_mean * abundancia_relativa_1km + (1|number) + (1|sample) , data = Canes_to_join_solitary_bees_sex_it_female, na.action="na.fail")  
summary(it_female_total)
par(mfrow = c(2, 2))  
plot(it_female_total)
simulationOutput <- simulateResiduals(fittedModel = it_female_total)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)


it.totaldredge_female<-dredge(it_female_total, rank=AICc, fixed = ~ temp_mean+abundancia_relativa_1km)
dd1km1s<-subset(it.totaldredge_female, delta < 2)
interact_plot(it_female_total, pred = temp_mean, modx = abundancia_relativa_1km, plot.points = TRUE, interval=TRUE, int.width = 0.3, modx.labels = "Flower Abundance", x.label= "Mean temperature",  y.label= "IT",color.class = "BrBG")





it_male_total<-glmer(IT ~ temp_mean * abundancia_relativa_1km + (1|number) +  (1|sample), data = Canes_to_join_solitary_bees_sex_it_male, na.action="na.fail")  
summary(it_male_total)
par(mfrow = c(2, 2))  
plot(it_male_total)
simulationOutput <- simulateResiduals(fittedModel = it_male_total)
plotSimulatedResiduals(simulationOutput = simulationOutput)
it.totaldredge_male<-dredge(it_male_total, rank=AICc, fixed = ~ temp_mean+abundancia_relativa_1km)
dd1km2s<-subset(it.totaldredge, delta < 2)

interact_plot(it_male_total, pred = temp_mean, modx = abundancia_relativa_1km, plot.points = TRUE, interval=TRUE, int.width = 0.3, modx.labels = "Flower Abundance", x.label= "Mean temperature",  y.label= "IT",color.class = "BrBG")


#Modelos mixtos solo osmia abundancias relativas 1km

Canes_solitary_bee_sex_it_male_osmia_bicornis<- subset(Canes_to_join_solitary_bees_sex_it_male,Specie=="Osmia bicornis")
Canes_solitary_bee_sex_it_female_osmia_bicornis<- subset(Canes_to_join_solitary_bees_sex_it_female,Specie=="Osmia bicornis")


it_male_bicornis<-glmer(IT ~ temp_mean * abundancia_relativa_1km + (1|number) , data = Canes_solitary_bee_sex_it_male_osmia_bicornis, na.action="na.fail")  
summary(it_male_bicornis)
par(mfrow = c(2, 2))  
plot(it_male_bicornis)
simulationOutput <- simulateResiduals(fittedModel = it_male_bicornis)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)

it.bicornisdredge_male<-dredge(it_male_bicornis, rank=AICc, fixed = ~ temp_mean+abundancia_relativa_1km)
#Me dice que el mejor modelo es sin la interacción en el caso de los machos
it_male_bicornis<-glmer(IT ~ temp_mean + abundancia_relativa_1km + (1|number) , data = Canes_solitary_bee_sex_it_male_osmia_bicornis, na.action="na.fail")  
summary(it_male_bicornis)
par(mfrow = c(2, 2))  
plot(it_male_bicornis)
simulationOutput <- simulateResiduals(fittedModel = it_male_bicornis)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)

visreg(it_male_bicornis, scale=("response"), breaks= 3)

visreg(it_male_bicornis, pred = temp_mean, modx = abundancia_relativa_1km, plot.points = TRUE, interval=TRUE, int.width = 0.3, modx.labels = "Flower Abundance", x.label= "Mean temperature",  y.label= "IT",color.class = "BrBG")


##Problemas con los residuos en las hembras

it_female_bicornis<-glmer(IT ~ temp_mean * abundancia_relativa_1km + (1|number) , data = Canes_solitary_bee_sex_it_female_osmia_bicornis, na.action ="na.fail")  
summary(it_female_bicornis)
par(mfrow = c(2, 2))  
plot(it_female_bicornis)
simulationOutput <- simulateResiduals(fittedModel = it_female_bicornis)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)


it.bicornisdredge_female<-dredge(it_female_bicornis, rank=AICc, fixed = ~ temp_mean+abundancia_relativa_1km)
dd1km1s<-subset(it.totaldredge_female, delta < 2)
interact_plot(it_female_bicornis, pred = temp_mean, modx = abundancia_relativa_1km, plot.points = TRUE, interval=TRUE, int.width = 0.3, modx.labels = "Flower Abundance", x.label= "Mean temperature",  y.label= "IT",color.class = "BrBG")

#Ahora con las abundancias a 2km y sin filtrar preferencias

#solo Bicornis

it_male_bicornis_2km<-glmer(IT ~ temp_mean * abundancia_relativa + (1|number) , data = Canes_solitary_bee_sex_it_male_osmia_bicornis, na.action="na.fail")  
summary(it_male_bicornis_2km)
par(mfrow = c(2, 2))  
plot(it_male_bicornis_2km)
simulationOutput <- simulateResiduals(fittedModel = it_male_bicornis_2km)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)

it.bicornisdredge_male<-dredge(it_male_bicornis_2km, rank=AICc, fixed = ~ temp_mean+abundancia_relativa)
#Me dice que el mejor modelo es sin la interacción en el caso de los machos
it_male_bicornis_2km<-glmer(IT ~ temp_mean + abundancia_relativa + (1|number) , data = Canes_solitary_bee_sex_it_male_osmia_bicornis, na.action="na.fail")  
summary(it_male_bicornis)
par(mfrow = c(2, 2))  
plot(it_male_bicornis)
simulationOutput <- simulateResiduals(fittedModel = it_male_bicornis)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)

visreg(it_male_bicornis_2km, scale=("response"), breaks= 3)


it_female_bicornis_2km<-glmer(IT ~ temp_mean * abundancia_relativa + (1|number) , data = Canes_solitary_bee_sex_it_female_osmia_bicornis, na.action ="na.fail")  
summary(it_female_bicornis)
par(mfrow = c(2, 2))  
plot(it_female_bicornis_2km)
simulationOutput <- simulateResiduals(fittedModel = it_female_bicornis_2km)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)


it.bicornisdredge_female_2km<-dredge(it_female_bicornis_2km, rank=AICc, fixed = ~ temp_mean+abundancia_relativa)
visreg(it_female_bicornis_2km, scale=("response"), breaks= 3)

#Con todas las especies

it_female_total_2km<-glmer(IT ~ temp_mean * abundancia_relativa + (1|number) + (1|sample) , data = Canes_to_join_solitary_bees_sex_it_female, na.action="na.fail")  
summary(it_female_total_2km)
par(mfrow = c(2, 2))  
plot(it_female_total_2km)
simulationOutput <- simulateResiduals(fittedModel = it_female_total_2km)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)


it.totaldredge_female<-dredge(it_female_total_2km, rank=AICc, fixed = ~ temp_mean + abundancia_relativa)
dd1km1s<-subset(it.totaldredge_female, delta < 2)
visreg(it_female_total_2km, scale=("response"), breaks= 3)


#Tendría que escalar la IT?
it_male_total_2km<-glmer(IT ~ temp_mean * abundancia_relativa + (1|number) +  (1|sample), data = Canes_to_join_solitary_bees_sex_it_male, na.action="na.fail")  
summary(it_male_total_2km)
par(mfrow = c(2, 2))  
plot(it_male_total_2km)
simulationOutput <- simulateResiduals(fittedModel = it_male_total_2km)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
it.totaldredge_male<-dredge(it_male_total_2km, rank=AICc, fixed = ~ temp_mean+abundancia_relativa)

#Sale que no hay interacción a 2km
it_male_total_2km<-glmer(IT ~ temp_mean + abundancia_relativa + (1|number) +  (1|sample), data = Canes_to_join_solitary_bees_sex_it_male, na.action="na.fail")  

visreg(it_male_total_2km, scale=("response"), breaks= 4)

#Ahora realizo lo mismo pero con el tamaño de la celda
head(Canes_to_join_with_size)
head(Canes_to_join_solitary_bees_size)



 
Canes_to_join_with_size<- Canes_to_join_with_size[complete.cases(Canes_to_join_with_size),]

Canes_to_join_solitary_bees_size<-merge(Canes_to_join_with_size, data_from_bombus_to_join_osmia, by="Code")
Sexo_specie_it_total<-rbind(Sexo_specie_it_total_cane_male,Sexo_specie_it_total_cane_male)
number_specie<-Sexo_specie_it_total[,c(1,4)]
number_specie<-unique(number_specie)
str(number_specie)
Canes_to_join_solitary_bees_size<-merge(Canes_to_join_solitary_bees_size,number_specie , by="number")
#El problema aquí es que solo tengo la certeza de determinadas especies cuando han emergido, las otras quedan como no se sabe que especie es
#y eso tendría que verlo con los datos y asumir que son esas especies, por eso al final parece que solo hubiese 70 cañas.

Canes_to_join_solitary_bees_size$temp_mean<-scale(Canes_to_join_solitary_bees_size$temp_mean)
Canes_to_join_solitary_bees_size$abundancia_relativa<-scale(Canes_to_join_solitary_bees_size$abundancia_relativa)
Canes_to_join_solitary_bees_size$abundancia_relativa_1km<-scale(Canes_to_join_solitary_bees_size$abundancia_relativa_1km)
str(Canes_to_join_solitary_bees_size)

str(Canes_to_join_solitary_bees_size)
Canes_to_join_solitary_bees_size$number<-as.factor(Canes_to_join_solitary_bees_size$number)

Canes_to_join_solitary_bees_size <- within(Canes_to_join_solitary_bees_size, sample <- factor(number:Specie))
Canes_to_join_solitary_bees_size<- Canes_to_join_solitary_bees_size[complete.cases(Canes_to_join_solitary_bees_size),]
#He detectado outliers bastantes
quantiles <- quantile(Canes_to_join_solitary_bees_size$size, probs = c(.25, .75))
range <- 1.5 * IQR(Canes_to_join_solitary_bees_size$size)
Canes_to_join_solitary_bees_subset <- subset(Canes_to_join_solitary_bees_size,
                     Canes_to_join_solitary_bees_size$size > (quantiles[1] - range) & Canes_to_join_solitary_bees_size$size < (quantiles[2] + range))


Canes_size_total<-glmer(size ~ temp_mean * abundancia_relativa_1km + (1|number) + (1|sample) , data = Canes_to_join_solitary_bees_subset, na.action="na.fail")  
summary(Canes_size_total)
par(mfrow = c(2, 2))  
plot(Canes_size_total)
simulationOutput <- simulateResiduals(fittedModel = Canes_size_total)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
Canes_size_total_1km<-dredge(Canes_size_total, rank=AICc, fixed = ~ temp_mean+abundancia_relativa_1km)
interact_plot(Canes_size_total, pred = temp_mean, modx = abundancia_relativa_1km, plot.points = TRUE, interval=TRUE, int.width = 0.3, modx.labels = "Flower Abundance", x.label= "Mean temperature",  y.label= "Chamber size",color.class = "PuOr")

#Parece que no hay interacción entre los factores a 2km..

Canes_size_total_2km<-glmer(size ~ temp_mean * abundancia_relativa + (1|number) + (1|sample) , data = Canes_to_join_solitary_bees_size, na.action="na.fail")  
summary(Canes_size_total)
par(mfrow = c(2, 2))  
plot(Canes_size_total)
simulationOutput <- simulateResiduals(fittedModel = Canes_size_total)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
Canes_size_total_2km_dredge<-dredge(Canes_size_total_2km, rank=AICc, fixed = ~ temp_mean + abundancia_relativa)
Canes_size_total_2km<-glmer(size ~ temp_mean + abundancia_relativa + (1|number) + (1|sample) , data = Canes_to_join_solitary_bees_size, na.action="na.fail")  
visreg(Canes_size_total_2km, scale=("response"), breaks= 4)


#Podría relacionar a mayor tamaño de celda media mayor tamaño de IT media para extrapolar las ITS?
#Como haría para localizar si son machos o hembras mediante un procedimiento por R

#caso solo de osmia bicornis
Media_sexo_caña_total_bicornis<-subset(Media_sexo_specie_caña_total, Specie=="Osmia bicornis")
Correlation_chamber_size_It<-merge(Osmia_summarise_size,Media_sexo_caña_total_bicornis, by="number")

cor.test(Correlation_chamber_size_It$mean_cells, Correlation_chamber_size_It$mean_it)
cor.test(Correlation_chamber_size_It$mean_cells, Correlation_chamber_size_It$number_cells)
cor.test(Correlation_chamber_size_It$number_cells, Correlation_chamber_size_It$mean_it)


plot(Correlation_chamber_size_It$mean_cells, Correlation_chamber_size_It$mean_it)
plot(Correlation_chamber_size_It$mean_cells, Correlation_chamber_size_It$number_cells)
plot(Correlation_chamber_size_It$number_cells, Correlation_chamber_size_It$mean_it)

subset_outliers<-Correlation_chamber_size_It[c(-3,-48),]


cor.test(subset_outliers$mean_cells, subset_outliers$mean_it)
cor.test(subset_outliers$mean_cells, subset_outliers$number_cells)
cor.test(subset_outliers$number_cells, subset_outliers$mean_it)


plot(subset_outliers$mean_cells, subset_outliers$mean_it)
plot(subset_outliers$mean_cells, subset_outliers$number_cells)
plot(subset_outliers$number_cells, subset_outliers$mean_it)

p<-ggscatter(subset_outliers, x = "mean_cells", y = "mean_it", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "mean_cells", ylab = "mean_it")
print(p)

p<-ggscatter(subset_outliers, x = "mean_cells", y = "number_cells", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "mean_cells", ylab = "number_cells")
print(p)

p<-ggscatter(subset_outliers, x = "number_cells", y = "mean_it", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "number_cells", ylab = "mean_it")
print(p)

#Según estos resultados no podría extrapolar de momento la IT

#Modelos para los parásitos con osmia bicornis porque al resto debería analizar si coincide el tapón con las que emergieron.
str(Canes_for_parasited)
Canes_for_parasited<-Canes[,c(1,19,30)]
Canes_for_parasited<-merge(Canes_for_parasited, data_from_bombus_to_join_osmia, by="Code")
Canes_for_parasited<- Canes_for_parasited[complete.cases(Canes_for_parasited),]
Canes_for_parasited<-unique(Canes_for_parasited)
Canes_for_parasited$parasited<-as.factor(Canes_for_parasited$parasited)
Canes_for_parasited$temp_mean<-scale(Canes_for_parasited$temp_mean)
Canes_for_parasited$abundancia_relativa<-scale(Canes_for_parasited$abundancia_relativa)
Canes_for_parasited$abundancia_relativa_1km<-scale(Canes_for_parasited$abundancia_relativa_1km)




Canes_for_parasited_model<-glmer(parasited ~ temp_mean * abundancia_relativa + (1|Code), data = Canes_for_parasited, family="binomial", na.action="na.fail") 
head(Canes_for_parasited)
summary(Canes_for_parasited_model)
par(mfrow = c(2, 2))  
plot(Canes_for_parasited_model)
simulationOutput <- simulateResiduals(fittedModel = Canes_for_parasited_model)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
Canes_parasited_2km<-dredge(Canes_for_parasited_model, rank=AICc, fixed = ~ temp_mean + abundancia_relativa)
interact_plot(Canes_for_parasited_model, pred = temp_mean, modx = abundancia_relativa)

Canes_for_parasited_model_1km<-glmer(parasited ~ temp_mean * abundancia_relativa_1km + (1|Code), data = Canes_for_parasited, family="binomial", na.action="na.fail")  
summary(Canes_for_parasited_model_1km)
par(mfrow = c(2, 2))  
plot(Canes_for_parasited_model_1km)
simulationOutput <- simulateResiduals(fittedModel = Canes_for_parasited_model_1km)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
Canes_parasited_1km<-dredge(Canes_for_parasited_model_1km, rank=AICc, fixed = ~ temp_mean + abundancia_relativa_1km)
Canes_for_parasited_model_1km<-glmer(parasited ~ temp_mean + abundancia_relativa_1km + (1|number), data = Canes_for_parasited, family="binomial", na.action="na.fail")  
summary(Canes_for_parasited_model_1km)
visreg(Canes_for_parasited_model_1km, scale="response", breaks=1)
#Es más importante la abundancia a 2km que a 1km en el caso de la interacción?
#A mayor escala tienes mayores abundancia y por tanto más insectos, cuando te acercas a mayor abundancia este efecto se diluye.

#Habría que corregir por la densidad de abejas dentro de la caña, porque eso va a aumentar la probabilidad, y más si existen mayor número de especies en la caña.

#Ahora probemos con modelos mixtos pero por medias para solo Osmias bicornis.

Canes_osmia_bicornis<-rbind(Canes_solitary_bee_sex_it_male_osmia_bicornis,Canes_solitary_bee_sex_it_female_osmia_bicornis)
Canes_for_parasited<-Canes[,c(1,19,30)]
Canes_for_parasited<-merge(Canes_for_parasited, data_from_bombus_to_join_osmia, by="Code")
Canes_for_parasited<- Canes_for_parasited[complete.cases(Canes_for_parasited),]
Canes_osmia_bicornis<-Canes_osmia_bicornis[,c(1,4,5,6)]
Canes_osmia_bicornis<-unique(Canes_osmia_bicornis)

Canes_parasited_bicornis<-unique(Canes_parasited_bicornis)
Canes_parasited_bicornis_for_model<-merge(Canes_osmia_bicornis, Canes_for_parasited, by="number")
Canes_parasited_bicornis_for_model<-unique(Canes_parasited_bicornis_for_model)
Canes_parasited_bicornis_for_model<-Canes_parasited_bicornis_for_model[,c(-2,-3)]
Canes_parasited_bicornis_for_model<-unique(Canes_parasited_bicornis_for_model)
str(Canes_parasited_bicornis_for_model)
Canes_parasited_bicornis_for_model$parasited<-as.factor(Canes_parasited_bicornis_for_model$parasited)
Canes_parasited_bicornis_for_model$temp_mean<-scale(Canes_parasited_bicornis_for_model$temp_mean)
Canes_parasited_bicornis_for_model$abundancia_relativa<-scale(Canes_parasited_bicornis_for_model$abundancia_relativa)
Canes_parasited_bicornis_for_model$abundancia_relativa_1km<-scale(Canes_parasited_bicornis_for_model$abundancia_relativa_1km)
levels(Canes_parasited_bicornis_for_model$Code.x)

Canes_for_parasited_model_bicornis_1km<-glmer(parasited ~ temp_mean * abundancia_relativa_1km + (1|Code.x), data = Canes_parasited_bicornis_for_model, family="binomial", na.action="na.fail")  
summary(Canes_for_parasited_model_bicornis_1km)
par(mfrow = c(2, 2))  
plot(Canes_for_parasited_model_bicornis_1km)
simulationOutput <- simulateResiduals(fittedModel = Canes_for_parasited_model_bicornis_1km)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
Canes_parasited_1km<-dredge(Canes_for_parasited_model_bicornis_1km, rank=AIC, fixed = ~ temp_mean + abundancia_relativa_1km)
Canes_for_parasited_model_bicornis_1km<-glmer(parasited ~ temp_mean + abundancia_relativa_1km + (1|Code.x), data = Canes_parasited_bicornis_for_model, family="binomial", na.action="na.fail")  
visreg(Canes_for_parasited_model_bicornis_1km, scale="response", breaks=1)

Canes_for_parasited_model_bicornis_2km<-glmer(parasited ~ temp_mean * abundancia_relativa + (1|Code.x), data = Canes_parasited_bicornis_for_model, family="binomial", na.action="na.fail")  
summary(Canes_for_parasited_model_bicornis_2km)
par(mfrow = c(2, 2))  
plot(Canes_for_parasited_model_bicornis_2km)
simulationOutput <- simulateResiduals(fittedModel = Canes_for_parasited_model_bicornis_2km)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
Canes_parasited_2km<-dredge(Canes_for_parasited_model_bicornis_2km, rank=AICc, fixed = ~ temp_mean + abundancia_relativa)
Canes_for_parasited_model_bicornis_2km<-glmer(parasited ~ temp_mean + abundancia_relativa + (1|Code.x), data = Canes_parasited_bicornis_for_model, family="binomial", na.action="na.fail")  
visreg(Canes_for_parasited_model_bicornis_2km, scale="response", breaks=1)

str(Canes_parasited_bicornis)
head(Canes_parasited_bicornis)
head(Canes_for_parasited)

#Ahora con los datos de medias de celdas, abortos y numero de celdas de osmia bicornis


dataframe_osmia_bicornis_semifinal<-read.csv("dataframe_osmia_bicornis_semifinal.csv")
dataframe_osmia_bicornis_semifinal$temp_mean<-c(scale(dataframe_osmia_bicornis_semifinal$temp_mean))
dataframe_osmia_bicornis_semifinal$abundancia_relativa<-c(scale(dataframe_osmia_bicornis_semifinal$abundancia_relativa))
dataframe_osmia_bicornis_semifinal$abundancia_relativa_1km<-c(scale(dataframe_osmia_bicornis_semifinal$abundancia_relativa_1km))
dataframe_osmia_bicornis_semifinal$parasited<-as.factor(dataframe_osmia_bicornis_semifinal$parasited)
#Este último solo en caso de meterlo como factor en el caso de los parásitos
dataframe_osmia_bicornis_semifinal$buffer_space<-c(scale(dataframe_osmia_bicornis_semifinal$buffer_space))


str(dataframe_osmia_bicornis_semifinal)
dataframe_osmia_bicornis_semifinal<- dataframe_osmia_bicornis_semifinal[complete.cases(dataframe_osmia_bicornis_semifinal$number_cells),]

#numero de celdas
number_cells_model_bicornis<-glm( number_cells ~ temp_mean * abundancia_relativa_1km, data = dataframe_osmia_bicornis_semifinal, na.action="na.fail")  
summary(number_cells_model_bicornis)
par(mfrow = c(2, 2))  
plot(number_cells_model_bicornis)
simulationOutput <- simulateResiduals(fittedModel = number_cells_model_bicornis)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
number_cells_model_bicornis<-dredge(number_cells_model_bicornis, rank=AICc, fixed = ~ temp_mean + abundancia_relativa_1km)
number_cells_model_bicornis<-glm(number_cells ~ temp_mean + abundancia_relativa_1km, data = dataframe_osmia_bicornis_semifinal, na.action="na.fail")  
visreg(number_cells_model_bicornis, scale="response", breaks=3)

number_cells_model_bicornis_2km<-glm( number_cells ~ temp_mean * abundancia_relativa, data = dataframe_osmia_bicornis_semifinal, na.action="na.fail")  
summary(number_cells_model_bicornis_2km)
par(mfrow = c(2, 2))  
plot(number_cells_model_bicornis_2km)
simulationOutput <- simulateResiduals(fittedModel = number_cells_model_bicornis_2km)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
number_cells_model_bicornis<-dredge(number_cells_model_bicornis_2km, rank=AICc, fixed = ~ temp_mean + abundancia_relativa)
number_cells_model_bicornis_2km<-glm(number_cells ~ temp_mean + abundancia_relativa, data = dataframe_osmia_bicornis_semifinal, na.action="na.fail")  
visreg(number_cells_model_bicornis_2km, scale="response", breaks=3)

#influencia

dataframe_osmia_bicornis_semifinal<- dataframe_osmia_bicornis_semifinal[complete.cases(dataframe_osmia_bicornis_semifinal$mean_cells),]
dataframe_osmia_bicornis_semifinal$mean_cells<- as.integer(dataframe_osmia_bicornis_semifinal$mean_cells)


mean_cells_model_bicornis<-glm.nb( mean_cells ~ temp_mean * abundancia_relativa_1km, data = dataframe_osmia_bicornis_semifinal, na.action="na.fail")  
summary(mean_cells_model_bicornis)
par(mfrow = c(2, 2))  
plot(mean_cells_model_bicornis)
simulationOutput <- simulateResiduals(fittedModel = mean_cells_model_bicornis)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
mean_cells_model_bicornis<-dredge(mean_cells_model_bicornis, rank=AICc, fixed = ~ temp_mean + abundancia_relativa_1km)
mean_cells_model_bicornis<-glm.nb(mean_cells ~ temp_mean + abundancia_relativa_1km, data = dataframe_osmia_bicornis_semifinal, na.action="na.fail")  
visreg(mean_cells_model_bicornis, scale="response", breaks=3)

mean_cells_model_bicornis_2km<-glm.nb( mean_cells ~ temp_mean * abundancia_relativa, data = dataframe_osmia_bicornis_semifinal, na.action="na.fail")  
summary(mean_cells_model_bicornis_2km)
par(mfrow = c(2, 2))  
plot(mean_cells_model_bicornis_2km)
simulationOutput <- simulateResiduals(fittedModel = mean_cells_model_bicornis_2km)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
mean_cells_model_bicornis_2km<-dredge(mean_cells_model_bicornis_2km, rank=AICc, fixed = ~ temp_mean + abundancia_relativa)
mean_cells_model_bicornis_2km<-glm.nb(mean_cells ~ temp_mean + abundancia_relativa, data = dataframe_osmia_bicornis_semifinal, na.action="na.fail")  
visreg(mean_cells_model_bicornis_2km, scale="response", breaks=3)

#relación parásitos con el tamaño de área de buffer ( ver si también el área de buffer tiene relación con temperatura o con las otras dos)
dataframe_osmia_bicornis_semifinal<-read.csv("dataframe_osmia_bicornis_semifinal.csv")
dataframe_osmia_bicornis_semifinal<- dataframe_osmia_bicornis_semifinal[complete.cases(dataframe_osmia_bicornis_semifinal$buffer_space),]

parasited<- glmer( parasited ~ buffer_space + (1| Code), data = dataframe_osmia_bicornis_semifinal, family= "binomial", na.action="na.fail")  
summary(parasited)
par(mfrow = c(2, 2))  
plot(parasited)
simulationOutput <- simulateResiduals(fittedModel = parasited)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
visreg(parasited, scale="response", breaks=3)
parasited<-dredge(parasited, rank=AICc, fixed = ~ buffer_space + temp_mean)

interact_plot(parasited, pred = buffer_space, modx = temp_mean)

#Existe relación entre la temperatura media y el espacio del buffer

buffer<- glm( internal_space ~ temp_mean , data = dataframe_osmia_bicornis_semifinal, na.action="na.fail")  
summary(buffer)
par(mfrow = c(2, 2))  
plot(buffer)
simulationOutput <- simulateResiduals(fittedModel = buffer)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
visreg(buffer, scale="response", breaks=3)

dataframe_osmia_bicornis_semifinal<- dataframe_osmia_bicornis_semifinal[complete.cases(dataframe_osmia_bicornis_semifinal$parasited),]

#Tendría que poner en esta todos los códigos de separación?
parasited<- glmer( parasited ~ temp_mean , data = dataframe_osmia_bicornis_semifinal, family="binomial", na.action="na.fail")  
summary(parasited)
par(mfrow = c(2, 2))  
plot(parasited)
simulationOutput <- simulateResiduals(fittedModel = parasited)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
visreg(parasited, scale="response", breaks=3)
parasited<-dredge(parasited, rank=AICc, fixed = ~ buffer_space + temp_mean)


#Modelo con el número de abortos
str(dataframe_osmia_bicornis_semifinal_aborts)
Canes<-unique(Canes)
Canes_for_aborts<-Canes[,c(1,21,30)]
Canes_for_aborts[is.na(Canes_for_aborts)] <- 0
head(Canes_for_aborts)
str(Canes_for_aborts)
Canes_for_aborts<-unique(Canes_for_aborts)
Canes_for_aborts<- Canes_for_aborts[complete.cases(Canes_for_aborts$Aborts),]
Canes_for_aborts<-merge(Canes_for_aborts, data_from_bombus_to_join_osmia, by="Code")
Canes_for_aborts$temp_mean<-c(scale(Canes_for_aborts$temp_mean))
Canes_for_aborts$abundancia_relativa<-c(scale(Canes_for_aborts$abundancia_relativa))
Canes_for_aborts$abundancia_relativa_1km<-c(scale(Canes_for_aborts$abundancia_relativa_1km))

aborts_model<- glmer( Aborts ~ temp_mean * abundancia_relativa_1km + (1|number) , data = Canes_for_aborts, family="poisson", na.action="na.fail")  
summary(aborts_model)
par(mfrow = c(2, 2))  
plot(aborts_model)
simulationOutput <- simulateResiduals(fittedModel = aborts_model)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
aborts_model<-dredge(aborts_model, rank=AICc, fixed = ~ temp_mean + abundancia_relativa_1km + (1|number) )
aborts_model<- glmer( Aborts ~ temp_mean + abundancia_relativa_1km + (1|number) , data = Canes_for_aborts, family="poisson", na.action="na.fail")  
visreg(aborts_model, scale="response", breaks=3)

aborts_model<- glmer( Aborts ~ temp_mean * abundancia_relativa + (1|number) , data = Canes_for_aborts, family="poisson", na.action="na.fail")  
summary(aborts_model)
par(mfrow = c(2, 2))  
plot(aborts_model)
simulationOutput <- simulateResiduals(fittedModel = aborts_model)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
aborts_model<-dredge(aborts_model, rank=AICc, fixed = ~ temp_mean + abundancia_relativa + (1|number) )
interact_plot(aborts_model, pred = temp_mean, modx = abundancia_relativa,color.class = "BrBG", outcome.scale = "response")








#Para hallar el coeficiente de variación divido la desviación estándar entre la media de cada una de las distancias

cv_workers<-(dataframe_final_bombus$Intworkers_sd)/(dataframe_final_bombus$Intworkers_mean)
cv_males<-(dataframe_final_bombus$Intmales_sd)/(dataframe_final_bombus$Intmales_mean)
cv_queens<-(dataframe_final_bombus$Intqueens_sd)/(dataframe_final_bombus$Intqueens_mean)

variance_workers<-(dataframe_final_bombus$Intworkers_sd)^2
variance_males<-(dataframe_final_bombus$Intmales_sd)^2
variance_queens<-(dataframe_final_bombus$Intqueens_sd)^2

cor.test(dataframe_final_bombus$Intworkers_mean, variance_workers)
cor.test(dataframe_final_bombus$Intmales_mean, variance_males)
cor.test(dataframe_final_bombus$Intqueens_mean, variance_queens)

plot(dataframe_final_bombus$Intworkers_mean, dataframe_final_bombus$Intworkers_sd)
plot(dataframe_final_bombus$Intmales_mean, (dataframe_final_bombus$Intmales_sd))
plot(dataframe_final_bombus$Intqueens_mean, dataframe_final_bombus$Intqueens_sd)

View(dataframe_osmia_bicornis_semifinal)

#Estos cv son proporciones continuas por lo que no se deben analizar mediante modelos de poisson.
#Utiilizar beta regression, un modelo tipico para este tipo de proporciones y está localizado entre 0 y 1, 
#Para proporciones continuas debe ser así, sino quasibinomial con logit link.

head(individualmsub)
#Visualizo un resumen descriptivo de los datos
summary(individualmsub)
#Compruebo que tengo todos los niveles el factor
levels(individualmsub$Code)
#cambio el nombre al factor
colnames(individualmsub)[1]<-"code"
variance<-data.frame(variance_workers,variance_males,variance_queens, dataframe_final_bombus$code)
colnames(variance)[4]<-"code"
View(dataframe_final_bombus_redo)
individualmsub
#Creacion de subsets relativos al tamaño para evitar los NA y poder aplicar la función dredge

temp_intmales <- subset(individualmsub, select = c( "Intmales" ,"code" ))
head(temp_intmales)
summary(temp_intmales)
temp_intmalestotal<- temp_intmales[complete.cases(temp_intmales),]
head(temp_intmalestotal)
summary(temp_intmalestotal)
str(temp_intmalestotal)


temp_intworkers <- subset(individualmsub, select = c( "Intworkers" ,"code" ))
head(temp_intworkers)
summary(temp_intworkers)
temp_intworkerstotal<- temp_intworkers[complete.cases(temp_intworkers),]
head(temp_intworkerstotal)
summary(temp_intworkerstotal)
str(temp_intworkerstotal)

temp_intqueens <- subset(individualmsub, select = c( "Intqueens" ,"code" ))
head(temp_intqueens)
summary(temp_intqueens)
temp_intqueenstotal<- temp_intqueens[complete.cases(temp_intqueens),]
head(temp_intqueenstotal)
summary(temp_intqueenstotal)
str(temp_intqueenstotal)


temp_paraindv <- subset(dataframe_final_bombus_redo, select = c("temp_mean2", "pesticida_consumido_redo2", "abundancia_relativa_1km2", "Wi2" ,"code" ))
head(temp_paraindv)
summary(temp_paraindv)
temp_paraindv2 <- temp_paraindv[complete.cases(temp_paraindv),]
head(temp_paraindv2)
summary(temp_paraindv2)
str(temp_paraindv2)

wempty <- subset(dataframe_final_bombus_redo, select = c("temp_mean", "Wemptyqueen"))
summary(wempty)
wempty2 <- wempty[complete.cases(wempty),]
head(wempty2)
summary(wempty2)
str(wempty2)

wempty2<-wempty2[-4,]

dataframe_intertegular_mixmodels_males<-merge(temp_intmalestotal,temp_paraindv2, by="code")
dataframe_intertegular_mixmodels_males<-merge(dataframe_intertegular_mixmodels_males,variance, by="code")
View(dataframe_intertegular_mixmodels_males)
dataframe_intertegular_mixmodels_workers<-merge(temp_intworkerstotal,temp_paraindv2, by="code")
dataframe_intertegular_mixmodels_workers<-merge(dataframe_intertegular_mixmodels_workers,variance, by="code")
View(dataframe_intertegular_mixmodels_workers)
dataframe_intertegular_mixmodels_queens<-merge(temp_intqueenstotal,temp_paraindv2, by="code")
dataframe_intertegular_mixmodels_queens<-merge(dataframe_intertegular_mixmodels_queens,variance, by="code")
View(dataframe_intertegular_mixmodels_queens)


temp_varianceworkers <- dataframe_intertegular_mixmodels_workers[complete.cases(dataframe_intertegular_mixmodels_workers),]
head(temp_varianceworkers)
summary(temp_varianceworkers)
str(temp_varianceworkers)

temp_variancequeens <- dataframe_intertegular_mixmodels_queens[complete.cases(dataframe_intertegular_mixmodels_queens),]
head(temp_variancequeens)
summary(temp_variancequeens)
str(temp_variancequeens)

temp_variancemales <- dataframe_intertegular_mixmodels_males[complete.cases(dataframe_intertegular_mixmodels_males),]
head(temp_variancemales)
summary(temp_variancemales)
str(temp_variancemales)




#hselecciono variables relativas al dataframe
head(dataframe_final_bombus_redo)
View(dataframe_final_bombus_redo)
head(dataframe_final_bombus_redo[,c(1,12,21,24,74,75,86,87,88,89,90,91,92,93)])

dataframe_intertegular_mixmodels<- dataframe_final_bombus_redo[,c(1,12,21,24,74,75,86,87,88,89,90,91,92,93)]
dataframe_intertegular_mixmodels<-merge(dataframe_intertegular_mixmodels,individualmsub, by="code")
dataframe_intertegular_mixmodels<-merge(dataframe_intertegular_mixmodels,variance, by="code")
View(dataframe_intertegular_mixmodels)

dataframe_intertegular_mixmodels<-dataframe_intertegular_mixmodels[,c(-18,-19,-20)]
head(dataframe_intertegular_mixmodels)
#generar un dataframe solo con los que están disponbiles sin tener NAs para los modelos mixtos




dataframe_intertegular_mixmodels_males<-merge(temp2,individualmsub, by="code")
dataframe_intertegular_mixmodels<-merge(dataframe_intertegular_mixmodels_males,variance, by="code")
View(dataframe_intertegular_mixmodels_males)





#Aplico subsets de NA para que no me den fallos los dredge ni los modelos con el uso de na.omit

new_dataframe_final_bombusN.queens <- dataframe_final_bombus[!is.na(dataframe_final_bombus$N.queens),]
new_dataframe_final_bombusN.males <- dataframe_final_bombus[!is.na(dataframe_final_bombus$N.males),]
new_dataframe_final_bombusN.workers <- dataframe_final_bombus[!is.na(dataframe_final_bombus$N.workers),]
new_dataframe_final_bombusWlarves <- dataframe_final_bombus[!is.na(dataframe_final_bombus$Wlarves),]
new_dataframe_final_bombusIntworkers_mean <- dataframe_final_bombus[!is.na(dataframe_final_bombus$Intworkers_mean),]
new_dataframe_final_bombusIntmales_mean <- dataframe_final_bombus[!is.na(dataframe_final_bombus$Intmales_mean),]
new_dataframe_final_bombusIntqueens_mean <- dataframe_final_bombus[!is.na(dataframe_final_bombus$Intqueens_mean),]
new_dataframe_intertegular_mixmodelsIntworkers <- dataframe_intertegular_mixmodels[!is.na(dataframe_intertegular_mixmodels$Intworkers),]
new_dataframe_intertegular_mixmodelsIntmales<- dataframe_intertegular_mixmodels[!is.na(dataframe_intertegular_mixmodels$Intmales),]
new_dataframe_intertegular_mixmodelsIntqueens <- dataframe_intertegular_mixmodels[!is.na(dataframe_intertegular_mixmodels$Intqueens),]
new_dataframe_intertegular_mixmodelscv_workers <- dataframe_intertegular_mixmodels[!is.na(dataframe_intertegular_mixmodels$cv_workers),]
new_dataframe_intertegular_mixmodelscv_males<- dataframe_intertegular_mixmodels[!is.na(dataframe_intertegular_mixmodels$cv_males),]
new_dataframe_intertegular_mixmodelscv_queens <- dataframe_intertegular_mixmodels[!is.na(dataframe_intertegular_mixmodels$cv_queens),]

######Modelos mixtos####

#i1<-glmer(Intworkers ~ temp_mean * Treatment * abundancia_relativa + (1|code) , data = dataframe_intertegular_mixmodels, family=gaussian)  
#summary(i1)
#par(mfrow = c(2, 2))  
#plot(i1)
#simulationOutput <- simulateResiduals(fittedModel = i1)
#plotSimulatedResiduals(simulationOutput = simulationOutput)


#i2<-glmer(Intqueens ~ temp_mean * Treatment * abundancia_relativa + (1|code) , data = dataframe_intertegular_mixmodels, family=gaussian)  
#summary(i2)
#par(mfrow = c(2, 2))  
#plot(i2)
#simulationOutput <- simulateResiduals(fittedModel = i2)
#plotSimulatedResiduals(simulationOutput = simulationOutput)


#i3<-glmer(Intmales ~ temp_mean * Treatment * abundancia_relativa + (1|code) , data = dataframe_intertegular_mixmodels, family=gaussian)  
#summary(i3)
#par(mfrow = c(2, 2))  
#plot(i3)
#simulationOutput <- simulateResiduals(fittedModel = i3)
#plotSimulatedResiduals(simulationOutput = simulationOutput)


#######Modelos mixtos escalados######
#Sería un modelo mixto cruzado.
#Encuentro la varianza residual que es mayor que la varianza de la intercepta aleatoria, es decir hay más variabilidad dentro de las colonias que entre las diferentes colonias.

#¡¡¡¡¡¡¡Esto es importante !!!!!!! This maximal model was simplified by removing non-significant fixed terms until no further reduction in AIC score could be obtained. Due to issues associated with calculating P values from mixed effects models with a Gaussian error structure, we used Markov Chain Monte Carlo (MCMC) resampling to estimate P-values, though these gave qualitatively identical results to the less- conservative P-values obtained from the traditional t-test for each parameter estimate using the upper bounds of degrees of freedom. The MCMC procedure was carried 
#out using the pvals.fnc function in the languageR package (Baayen 2010) for the R environment.#

i1.1cat<-glmer(Intworkers ~ temp_mean2 * Treatment * abundancia_relativa_1km2 + (1|code) , data = dataframe_intertegular_mixmodels)  
summary(i1.1cat)
par(mfrow = c(2, 2))  
plot(i1.1)
simulationOutput <- simulateResiduals(fittedModel = i1.1cat)
plotSimulatedResiduals(simulationOutput = simulationOutput)


#correlación inducida y tamaño muestral efectivo
#Solo explica un 21% de la variación
#varianza residual/varianza residual+varianza intercepta= correlación inducida
#tamaño muestral efectivo. 

i2.1cat<-glmer(Intqueens ~ temp_mean2 * Treatment * abundancia_relativa_1km2 + (1|code) , data = dataframe_intertegular_mixmodels)  
summary(i2.1cat)
par(mfrow = c(2, 2))  
plot(i2.1)
simulationOutput <- simulateResiduals(fittedModel = i2.1cat)
plotSimulatedResiduals(simulationOutput = simulationOutput)

i3.1cat<-glmer(Intmales ~ temp_mean2 * Treatment * abundancia_relativa_1km2+ (1|code) , data = dataframe_intertegular_mixmodels, family=gaussian)  
summary(i3.1cat)
par(mfrow = c(2, 2))  
plot(i3.1)
simulationOutput <- simulateResiduals(fittedModel = i3.1cat)
plotSimulatedResiduals(simulationOutput = simulationOutput)

i1.cont<-glmer(Intworkers ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 + Wi2 + (1|code) , data = dataframe_intertegular_mixmodels_workers, family=gaussian, na.action="na.fail")  
summary(i1.cont)
par(mfrow = c(2, 2))  
plot(i1.c)
simulationOutput <- simulateResiduals(fittedModel = i1.cont)
plotSimulatedResiduals(simulationOutput = simulationOutput)


i2.cont<-glmer(Intqueens ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 + Wi2 + (1|code) , data = dataframe_intertegular_mixmodels_queens, family=gaussian, na.action="na.fail")  
summary(i2.cont)
par(mfrow = c(2, 2))  
plot(i2.c)
simulationOutput <- simulateResiduals(fittedModel = i2.cont)
plotSimulatedResiduals(simulationOutput = simulationOutput)

#los machos no van demasiado bien aquí el ajuste de los residuales.

i3.cont<-glmer(Intmales ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 + Wi2 +  (1|code) , data = dataframe_intertegular_mixmodels_males, family=gaussian, na.action="na.fail")  
summary(i3.cont)
par(mfrow = c(2, 2))  
plot(i3.c)
simulationOutput <- simulateResiduals(fittedModel = i3.cont)
plotSimulatedResiduals(simulationOutput = simulationOutput)

dd1kmi1<- dredge(i1.cont,na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)

dd1kmi2<- dredge(i2.cont, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)

dd1kmi3<- dredge(i3.cont, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)

subset(dd1kmi1, delta < 2)
subset(dd1kmi2, delta < 2)
subset(dd1kmi3, delta < 2)

###para el caso de la varianza

library(dplyr)

temp_variancemales<-temp_variancemales[ ,c(-2)]

variance_dataframe<-unique(temp_variancemales)
temp_variance_dataframe<-merge(variance_dataframe, dataframe_final_bombus_redo, by="code")
variance_dataframe<-temp_variance_dataframe[,c(1,2,3,5,6,7,8,103)]
variance_dataframe<-as.data.frame(variance_dataframe)
colnames(variance_dataframe)[2]<-"temp_mean2"
colnames(variance_dataframe)[3]<-"pesticida_consumido_redo2"
colnames(variance_dataframe)[4]<-"Wi2"

indvi1.cont<-glm(variance_workers ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 + Wi2 , data =variance_dataframe, family=gaussian, na.action="na.fail")  
summary(i1.cont)
par(mfrow = c(2, 2))  
plot(i1.c)
simulationOutput <- simulateResiduals(fittedModel = i1.cont)
plotSimulatedResiduals(simulationOutput = simulationOutput)


iv1km2.cont<-glm(variance_queens ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 + Wi2, data = variance_dataframe, family=gaussian, na.action="na.fail")  
summary(i2.cont)
par(mfrow = c(2, 2))  
plot(i2.c)
simulationOutput <- simulateResiduals(fittedModel = i2.cont)
plotSimulatedResiduals(simulationOutput = simulationOutput)


iv1km3.cont<-glm(variance_males ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 + Wi2 , data = variance_dataframe, family=gaussian, na.action="na.fail")  
summary(i3.cont)
par(mfrow = c(2, 2))  
plot(i3.c)
simulationOutput <- simulateResiduals(fittedModel = i3.cont)
plotSimulatedResiduals(simulationOutput = simulationOutput)

dd1kmiv1km1<- dredge(indv1km1.cont ,na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)

dd1kmiv1km2<- dredge(iv1km2.cont, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)

dd1kmiv1km3<- dredge(iv1km3.cont, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)

subset(dd1kmiv1, delta < 2)
subset(dd1kmiv2, delta < 2)
subset(dd1kmiv3, delta < 2)



#####Modelos mixtos con el continúo#######
#Me los pide el modelo como integers.
str(dataframe_intertegular_mixmodels)
dataframe_intertegular_mixmodels$cv_workers<-as.integer(dataframe_intertegular_mixmodels$cv_workers)
dataframe_intertegular_mixmodels$cv_males<-as.integer (dataframe_intertegular_mixmodels$cv_males)
dataframe_intertegular_mixmodels$cv_queens<-as.integer (dataframe_intertegular_mixmodels$cv_queens)


#####Seleccion Modelos glms con el continúo#######


out.put_nqueens<-model.sel(b1,b1.2,b1.1, rank=AIC)
out.put_wlarves<-model.sel(b2,b2.2,b2.1, rank=AIC)
out.put_n_males<-model.sel(b3,b3.2,b3.1, rank= AIC)
out.put_n_workers<-model.sel(b4,b4.2,b4.1, rank=AIC)
out.put_intworkers<-model.sel(b5,b5.2,b5.1, i1.1cat, i1.cont, rank=AIC)
out.put_intqueens<-model.sel(b6,b6.2,b6.1, i2.1cat, i2.cont, rank=AIC)
out.put_intmales<-model.sel(b7,b7.2,b7.1, i3.1cat, i3.cont, rank=AIC)


####promediado de modelos continuo#####

pesticida_consumido2
str(new_dataframe_final_bombusN.queens)

options(na.action = "na.fail")

###Preguntar nacho si esta es la interacción###

cor.test(cv.)
str(abundancia_percent)
dataframe_final_bombus_redo<-merge(dataframe_final_bombus_redo, abundancia_percent_to_dataframe_bombus_final_redo)
abundancia_relativa_1km2<-scale(dataframe_final_bombus_redo$abundancia_relativa_1km)
pesticida_consumido_redo2<-scale(dataframe_final_bombus_redo$pesticida_consumido_final_redo)
temp_mean2<-scale(dataframe_final_bombus_redo$temp_mean)
abundancia_relativa2<-scale(dataframe_final_bombus_redo$abundancia_relativa)
Wi2<-scale(dataframe_final_bombus_redo$Wi)
dataframe_final_bombus_redo<-cbind(dataframe_final_bombus_redo,pesticida_consumido_redo2, by=code)
dataframe_final_bombus_redo<-cbind(dataframe_final_bombus_redo, temp_mean2)
dataframe_final_bombus_redo<-cbind(dataframe_final_bombus_redo, abundancia_relativa2)
dataframe_final_bombus_redo<-cbind(dataframe_final_bombus_redo, abundancia_relativa_1km2)
dataframe_final_bombus_redo<-cbind(dataframe_final_bombus_redo, Wi2)
#ccambiar el NA de pesticida que hay en viveros norte por el valor de 0 escalado ya que al final no se pudo tratar
dataframe_final_bombus_redo$pesticida_consumido_redo2[23] <- -0.86998937




temp <- subset(dataframe_final_bombus_redo, select = c("Intmales_mean", "temp_mean2", "pesticida_consumido_redo2", "abundancia_relativa_1km2", "Wi2" ,"code" ))
head(temp)
summary(temp)
temp2 <- temp[complete.cases(temp),]
head(temp2)
summary(temp2)
str(temp2)



temp_N.queens <- subset(dataframe_final_bombus_redo, select = c("N.queens", "temp_mean2", "pesticida_consumido_redo2", "abundancia_relativa_1km2", "Wi2"))
head(temp_N.queens )
summary(temp_N.queens )
temp2_Nqueens <- temp_N.queens[complete.cases(temp_N.queens),]
head(temp2_Nqueens)
summary(temp2_Nqueens)
str(temp2_Nqueens)

temp_wlarves<- subset(dataframe_final_bombus_redo, select = c("Wlarves", "temp_mean2", "pesticida_consumido_redo2", "abundancia_relativa_1km2", "Wi2"))
head(temp_wlarves)
summary(temp_wlarves)
temp2_wlarves <- temp_wlarves[complete.cases(temp_wlarves),]
head(temp2_wlarves)
summary(temp2_wlarves)
str(temp2_wlarves)

temp_N.workers<- subset(dataframe_final_bombus_redo, select = c("N.workers", "temp_mean2", "pesticida_consumido_redo2", "abundancia_relativa_1km2", "Wi2"))
head(temp_N.workers)
summary(temp_N.workers)
temp2_N.workers <- temp_N.workers[complete.cases(temp_N.workers),]
head(temp_N.workers)
summary(temp_N.workers)
str(temp_N.workers)

temp_N.males <- subset(dataframe_final_bombus_redo, select = c("N.males", "temp_mean2", "pesticida_consumido_redo2", "abundancia_relativa_1km2", "Wi2"))
head(temp_N.males)
summary(temp_N.males)
temp2_N.males <- temp_N.males[complete.cases(temp_N.males),]
head(temp2_N.males)
summary(temp2_N.males)
str(temp2_N.males)

temp_Intworkers_mean <- subset(dataframe_final_bombus_redo, select = c("Intworkers_mean", "temp_mean2", "pesticida_consumido_redo2", "abundancia_relativa_1km2", "Wi2","code"))
head(temp_Intworkers_mean)
summary(temp_Intworkers_mean)
temp_Intworkers_mean2 <- temp_Intworkers_mean[complete.cases(temp_Intworkers_mean),]
head(temp_Intworkers_mean2)
summary(temp_Intworkers_mean2)
str(temp_Intworkers_mean2)

temp_Intqueens_mean<- subset(dataframe_final_bombus_redo, select = c("Intqueens_mean", "temp_mean2", "pesticida_consumido_redo2", "abundancia_relativa_1km2", "Wi2", "code"))
head(temp_Intqueens_mean)
summary(temp_Intqueens_mean)
temp_queens_mean2 <- temp_Intqueens_mean[complete.cases(temp_Intqueens_mean),]
head(temp_queens_mean2)
summary(temp_queens_mean2)
str(temp_queens_mean2)

head(dataframe_final_bombus_redo)

temp_empty_queens<- subset(dataframe_final_bombus_redo, select = c("Wemptyqueen", "temp_mean2", "pesticida_consumido_redo2", "abundancia_relativa_1km2", "Wi2", "code"))
head(temp_empty_queens)
summary(temp_empty_queens)
temp_empty_queens2 <- temp_empty_queens[complete.cases(temp_empty_queens),]





fm1km1 <- glm.nb(N.queens ~ temp_mean2*pesticida_consumido_redo2*abundancia_relativa_1km2+Wi2, data = temp2_Nqueens, na.action="na.fail")
summary(temp2_Nqueens)
simulationOutput <- simulateResiduals(fittedModel = fm1km1)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
dd1km1<-dredge(fm1km1, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
dd1km1s<-subset(dd1km1, delta < 2)

variable_dependiente<-c("N.queens", "Wlarves", "N.workers", "N.males", "Intworkers_mean", "Intmales_mean", "Intqueens_mean")
seleccion_de_modelos<-rbind(dd1km1s,dd1km2s,dd1km3s,dd1km4s,dd1km5s,dd1km6s,dd1km7s)

dd1km1s<-as.data.frame(dd1km1s)

probe_interaction(fm1km1, pred = temp_mean2, modx = abundancia_relativa_1km2, mod2 = pesticida_consumido_redo2)

#####three continuous variables######



###############################


fm1km2 <- glm.nb(Wlarves~ temp_mean2*pesticida_consumido_redo2*abundancia_relativa_1km2+Wi2, data =  temp2_wlarves, na.action="na.fail")
simulationOutput <- simulateResiduals(fittedModel = fm1km2)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
dd1km2 <- dredge(fm1km2, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
dd1km2s<-subset(dd1km2, delta < 2)
tidy(dd1km2)

fm1km3<- glm.nb(N.workers~ temp_mean2*pesticida_consumido_redo2*abundancia_relativa_1km2+Wi2, data = temp2_N.workers, na.action="na.fail")
simulationOutput <- simulateResiduals(fittedModel = fm1km3)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
dd1km3<-dredge(fm1km3, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
dd1km3s<-subset(dd1km3, delta < 2)

fm1km4 <- glm(N.males~ temp_mean2*pesticida_consumido_redo2*abundancia_relativa_1km2+Wi2, data = temp2_N.males, na.action="na.fail")
simulationOutput <- simulateResiduals(fittedModel = fm1km4)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)

dd1km4<-dredge(fm1km4, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
dd1km4s<-subset(dd1km4, delta < 2)

fm1km5 <- glm(Intworkers_mean~ temp_mean2*pesticida_consumido_redo2*abundancia_relativa_1km2+Wi2, data =temp_Intworkers_mean2, na.action="na.fail")
simulationOutput <- simulateResiduals(fittedModel = fm1km5)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
dd1km5 <- dredge(fm1km5, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
dd1km5s<-subset(dd1km5, delta < 2)

#No me deja tirar el dredge en este modelo
fm1km6 <- glm(Intmales_mean~ temp_mean2*pesticida_consumido_redo2*abundancia_relativa_1km2+Wi2, data = temp2, na.action="na.fail")
simulationOutput <- simulateResiduals(fittedModel = fm1km6)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
dd1km6 <- dredge(fm1km6, na.fail, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
dd1km6s<-subset(dd1km6, delta < 2)


fm1km7 <- glm(Intqueens_mean~ temp_mean2*pesticida_consumido_redo2*abundancia_relativa_1km2+Wi2, data =temp_queens_mean2, na.action="na.fail")
simulationOutput <- simulateResiduals(fittedModel = fm1km7)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
dd1km7 <- dredge(fm1km7, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
dd1km7s<-subset(dd1km7, delta < 2)

fm1km8<- glm(Wemptyqueen~ temp_mean2*pesticida_consumido_redo2*abundancia_relativa_1km2+Wi2, data =temp_empty_queens2, na.action="na.fail")
simulationOutput <- simulateResiduals(fittedModel = fm1km8)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
dd1km8 <- dredge(fm1km8, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
dd1km8s<-subset(dd1km8, delta < 2)

fm1km9<- glm(Wemptyqueen~ temp_mean2*abundancia_relativa_1km2, data =temp_empty_queens2, na.action="na.fail")
simulationOutput <- simulateResiduals(fittedModel = fm1km9)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
dd1km9 <- dredge(fm1km9, na.omit, rank=AIC, fixed = ~ temp_mean2 + abundancia_relativa_1km2)
dd1km8s<-subset(dd1km8, delta < 2)

fm1km10<- glm.nb(N.queens~ N.workers * Intworkers_mean + Wi2, data = dataframe_final_bombus_redo, na.action="na.fail")
simulationOutput <- simulateResiduals(fittedModel = fm1km10)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
dd1km10 <- dredge(fm1km10, na.omit, rank=AIC, fixed = ~ N.workers + Intworkers_mean + Wi2)
dd1km10s<-subset(dd1km10, delta < 2)

fm1km11<- glm.nb(N.queens~ Intworkers_mean + Wi2, data = dataframe_final_bombus_redo, na.action="na.fail")
fm1km12<- glm.nb(N.queens~ variance_workers + Wi2, data = dataframe_final_bombus_redo, na.action="na.fail")
summary(fm1km11)
summary(fm1km12)
visreg(fm1km11, scale=("response"), breaks= 3)
visreg(fm1km12, scale=("response"), breaks= 3)



simulationOutput <- simulateResiduals(fittedModel = fm1km10)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
dd1km10 <- dredge(fm1km10, na.omit, rank=AIC, fixed = ~ N.workers + Intworkers_mean + Wi2)
dd1km10s<-subset(dd1km10, delta < 2)



simulationOutput <- simulateResiduals(fittedModel = fm1km8)
plotSimulatedResiduals(simulationOutput = simulationOutput)

#Me quedo con las dos primers filas de los modelos para ver que peso explican
dd1km1s[c(1:2,0)]
dd1km2s[c(1:2,0)]
dd1km3s[c(1:2,0)]
dd1km4s[c(1:2,0)]
dd1km5s[c(1:2,0)]
dd1km6s[c(1:2,0)]
dd1km7s[c(1:2,0)]
dd1km8s[c(1:2,0)]



stargazer(dd1km1s, dd1km2s, dd1km3s, dd1km4s, type = "html", align=TRUE, out="seleccion_models_glm_three_interactions_numbers.htm")
stargazer(dd1km5s, dd1km7s, type = "html", align=TRUE, out="seleccion_models_glm_three_interactions_intertegular.htm")
stargazer(dd1km1sc, dd1km2sc, dd1km3sc, dd1km4sc, type = "html", align=TRUE, out="seleccion_models_glm_three_interactions_numbers_categorical.htm")
stargazer(dd1km5sc, dd1km7sc, type = "html", align=TRUE, out="seleccion_models_glm_three_interactions_intertegular_categorical.htm")

###Este script es para el ploteo de las interacciones, ES DECIR VA DESPUÉS DEL DE LOS MODELOS###

m1<-dd1km1s[c(1,0)]
m2<-dd1km2s[c(1,0)]
m3<-dd1km3s[c(1,0)]
m4<-dd1km4s[c(1,0)]
m5<-dd1km5s[c(1,0)]
m6<-dd1km6s[c(1,0)]
m7<-dd1km7s[c(1,0)]
m8<-dd1km8s[c(1,0)]


#Modelos definitivos y ploteos

modelo_plot1<- glm.nb(N.queens ~ temp_mean2 + (pesticida_consumido_redo2*abundancia_relativa_1km2), data = temp2_Nqueens, na.action="na.exclude")
summary(modelo_plot1)
modelo_plot1$coefficients
glance1<-glance(modelo_plot1)
modelo_plot2<- glm.nb(Wlarves ~ temp_mean2 *abundancia_relativa_1km2 + pesticida_consumido_redo2 + Wi2, data = temp2_wlarves, na.action="na.exclude")
summary(modelo_plot2)
glance2<-glance(modelo_plot2)
modelo_plot3<- glm.nb(N.workers ~ (pesticida_consumido_redo2*abundancia_relativa_1km2)+(temp_mean2*abundancia_relativa_1km2)+Wi2, data = temp2_N.workers, na.action="na.exclude")
summary(modelo_plot3)
glance3<-glance(modelo_plot3)
modelo_plot4<- glm.nb(N.males ~ temp_mean2+ pesticida_consumido_redo2 + abundancia_relativa_1km2, data = temp2_N.males, na.action="na.exclude")
summary(modelo_plot4)
glance4<-glance(modelo_plot4)
modelo_plot5<- glm(Intworkers_mean ~ temp_mean2+ pesticida_consumido_redo2 + abundancia_relativa_1km2, data = temp_Intworkers_mean2, na.action="na.exclude")
summary(modelo_plot5)
glance5<-glance(modelo_plot5)
modelo_plot6<- glm(Intmales_mean ~ temp_mean2+ (pesticida_consumido_redo2*abundancia_relativa_1km2), data = temp2, na.action="na.exclude")
summary(modelo_plot6)
glance6<-glance(modelo_plot6)
modelo_plot7<- glm(Intqueens_mean ~ (temp_mean2*pesticida_consumido_redo2)+abundancia_relativa_1km2, data = temp_queens_mean2, na.action="na.exclude")
summary(modelo_plot7)
glance7<-glance(modelo_plot7)
modelo_plot8<- glm (Wemptyqueen ~ temp_mean2+ pesticida_consumido_redo2 + abundancia_relativa_1km2 + Wi2, data = temp_empty_queens2, na.action="na.exclude")
summary(modelo_plot8)
glance8<-glance(modelo_plot8)
modelsdeviance<-rbind(glance1,glance2,glance3,glance4,glance5,glance6,glance7,glance8)
View(modelsdeviance)
explained<-((modelsdeviance$null.deviance-modelsdeviance$deviance)/modelsdeviance$null.deviance)*100
modelsdeviance<-cbind(modelsdeviance,explained)
variable_dependiente<-c("N.queens", "Wlarves", "N.workers", "N.males", "Intworkers_mean", "Intmales_mean", "Intqueens_mean", "Weight_empty_queens")
modelsdeviance<-cbind(modelsdeviance,variable_dependiente)



interact_plot(modelo_plot2, pred = temp_mean2, modx = abundancia_relativa_1km2, plot.points = TRUE, interval=TRUE, int.width = 0.3, modx.labels = "Flower Abundance", x.label= "Mean temperature",  y.label= "Wlarves",color.class = "BrBG")
interact_plot(modelo_plot1, pred = abundancia_relativa_1km2, modx = pesticida_consumido_redo2, plot.points = TRUE, interval=TRUE, int.width = 0.3, modx.labels = "Pesticide consumed", x.label= "Flower Abundance", y.label= "Number of queens", color.class = "PuOr")
probe_interaction(modelo_plot3, pred = abundancia_relativa_1km2, modx= temp_mean2, mod2=pesticida_consumido_redo2, plot.points=TRUE, interval= TRUE, int.width= 0.3, mod2vals= "plus-minus", color.class = "PuOr",)
v4<-visreg(modelo_plot4, scale="response", breaks=3, jitter=TRUE)
plot(v4, layout=c(3,1))
v5<-visreg(modelo_plot5, scale="response", breaks=3, jitter=TRUE)
plot(v5, layout=c(3,1))
interact_plot(modelo_plot6, pred = abundancia_relativa_1km2, modx = pesticida_consumido_redo2, plot.points = TRUE, interval=TRUE, int.width = 0.3, modx.labels = "Pesticide consumed", x.label= "Flower abundance", y.label= "Intertegular males", color.class = "PuOr")
interact_plot(modelo_plot7, pred = temp_mean2, modx = pesticida_consumido_redo2 ,plot.points = TRUE, interval=TRUE, int.width = 0.3, y.label= "Intertegular queens", modx.labels = "Pesticide consumed", x.label= "Mean temperature", color.class = "PuOr")
v8<-visreg(modelo_plot8, scale="response", breaks=3, jitter=TRUE)
plot(v8, layout=c(3,1))

#Ploteo interacciones de varianza en la intertegular



indv1km1.cont<-glm(variance_workers ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 + Wi2, data = variance_dataframe, family=gaussian, na.action="na.fail")  
summary(indv1km1.cont)
simulationOutput <- simulateResiduals(fittedModel = indv1km1.cont)
plotSimulatedResiduals(simulationOutput = simulationOutput)
iv2.cont<-glm(variance_queens ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 + Wi2, data = variance_dataframe, family=gaussian, na.action="na.fail")  
summary(iv2.cont)
simulationOutput <- simulateResiduals(fittedModel = iv2.cont)
plotSimulatedResiduals(simulationOutput = simulationOutput)
iv3.cont<-glm(variance_males ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 + Wi2 , data = variance_dataframe, family=gaussian, na.action="na.fail")
summary(iv3.cont)
simulationOutput <- simulateResiduals(fittedModel = iv3.cont)
plotSimulatedResiduals(simulationOutput = simulationOutput)

dd1kmv1cont<- dredge(indv1km1.cont, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
subset(dd1kmv1cont, delta < 2)
dd1kmv2cont<- dredge(iv2.cont, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
subset(dd1kmv2cont, delta < 2)
dd1kmv3cont<- dredge(iv3.cont, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
subset(dd1kmv3cont, delta < 2)

indv1km1final.cont<-glm(variance_workers ~ temp_mean2 + pesticida_consumido_redo2 + abundancia_relativa_1km2, data = variance_dataframe, family=gaussian, na.action="na.fail")  
iv2.contfinal<-glm(variance_queens ~ (temp_mean2 * abundancia_relativa_1km2)+ pesticida_consumido_redo2 , data = variance_dataframe, family=gaussian, na.action="na.fail")  
iv3.contfinal<-glm(variance_males ~ temp_mean2 + pesticida_consumido_redo2 + abundancia_relativa_1km2 , data = variance_dataframe, family=gaussian, na.action="na.fail")



visreg(indv1km1final.cont,scale="response", breaks=3, jitter=TRUE)
interact_plot(iv2.contfinal, pred = temp_mean2, modx = abundancia_relativa_1km2, plot.points = TRUE, modxvals = "plus-minus")
visreg(iv3.contfinal,scale="response", breaks=3, jitter=TRUE)

#ploteo todos los individuos con el modelo mixto y la colonia como random en los que no me salían interacciones



i1.cont<-glmer(Intworkers ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2  + (1|code) + Wi2 , data = dataframe_intertegular_mixmodels_workers, family=gaussian, na.action="na.fail")  
dd1kmi1<- dredge(i1.cont, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
subset(dd1kmi1, delta < 2)
i2.cont<-glmer(Intqueens ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 + (1|code) + Wi2 , data = dataframe_intertegular_mixmodels_queens, family=gaussian, na.action="na.fail")  
dd1kmi2<- dredge(i2.cont, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
subset(dd1kmi2, delta < 2)
i3.cont<-glmer(Intmales ~ temp_mean2 * pesticida_consumido_redo2 * abundancia_relativa_1km2 +(1|code) + Wi2 , data = dataframe_intertegular_mixmodels_males, family=gaussian, na.action="na.fail")  
dd1kmi3<- dredge(i3.cont, na.omit, rank=AIC, fixed = ~ temp_mean2+pesticida_consumido_redo2+abundancia_relativa_1km2)
subset(dd1kmi3, delta < 2)

#No existen interacciones

i1.cont<-glmer(Intworkers ~ temp_mean2 + pesticida_consumido_redo2 + abundancia_relativa_1km2  + (1|code) , data = dataframe_intertegular_mixmodels_workers, family=gaussian, na.action="na.fail")  

0.1003^2/ (0.3852^2+0.1033^2)

i2.cont<-glmer(Intqueens ~ temp_mean2 + pesticida_consumido_redo2 + abundancia_relativa_1km2 + (1|code) , data = dataframe_intertegular_mixmodels_queens, family=gaussian, na.action="na.fail")  
summary(i2.cont)

0.076^2/ (0.13104^2+0.076^2)

i3.cont<-glmer(Intmales ~ temp_mean2 + pesticida_consumido_redo2 + abundancia_relativa_1km2 +(1|code) , data = dataframe_intertegular_mixmodels_males, family=gaussian, na.action="na.fail")  
summary(i3.cont)
0.1287^2/ (0.1638^2+0.1287^2)

visreg(i1.cont, scale="response")
visreg(i2.cont, scale="response")
visreg(i3.cont, scale="response")


###Ploteo lo del material y métodos
dataframe_final_bombus_redo$abundancia_relativa
dataframe_final_bombus_redo$pesticida_consumido_final_redo
dataframe_final_bombus_redo$temp_mean

plot(temp_mean ~ Orientation, data = dataframe_final_bombus_redo)
scatter(abundancia_relativa ~ Location, data=dataframe_final_bombus_redo)
dataframe_final_bombus_redo3<-dataframe_final_bombus_redo
as.numeric(levels(dataframe_final_bombus_redo3$abundancia_relativa))[dataframe_final_bombus_redo3$abundancia_relativa]
dataframe_final_bombus_redo3$abundancia_relativa<-as.numeric(as.character(dataframe_final_bombus_redo3$abundancia_relativa))
str(dataframe_final_bombus_redo3)
round(dataframe_final_bombus_redo3$abundancia_relativa, 2)
plot(pesticida_consumido_final_redo )
x <- ggplot(dataframe_final_bombus_redo3, aes(y = Location, x = abundancia_relativa)) + geom_point(stat = "identity")
plot(x) 

dataframe_plotting_pesticide <- dataframe_final_bombus_redo3 %>% filter(Treatment== "Treatment") 
dataframe_plotting_pesticide<-dataframe_plotting_pesticide[-24,]
dataframe_plotting_pesticide[order(dataframe_plotting_pesticide$pesticida_consumido_final_redo),]
b<-ggscatter(dataframe_plotting_pesticide, x = "pesticida_consumido_final_redo", y = "code", 
             add1km = "reg.line",
             xlab = "pesticida consumido", ylab = "code")
print(b)

p1<-ggplot(dataframe_plotting_pesticide, aes(x=code, y=pesticida_consumido_final_redo)) +
  geom_point()

p2<-ggplot(dataframe_plotting_pesticide, aes(x=reorder (code, -pesticida_consumido_final_redo), y=pesticida_consumido_final_redo)) +
  geom_point()
print(p2)

require("gridExtra")
grid.arrange(arrangeGrob(p1, p2))

str(dataframe_final_bombus_redo)

p<-ggplot(plotting_temperature, aes(x=N_S, y=temp_mean))+ geom_boxplot(notch=TRUE, outlier.colour = "red",) +geom_jitter() +  annotate("text", label = "1.62 ºC ", x = 1.5, y = 18, size = 5, colour = "blue")
print(p)

plotting_temperature<-dataframe_final_bombus_redo[,c(12,21,25)]
library('dplyr')
tapply(plotting_temperature$N_S, summary)
plotting_temperature %>% group_by(N_S) %>% summarize(median=median(temp_mean))
summary(plotting_temperature)
21.62556-20.83559

N_S<-c("N","S","N","S","S","N","S","N","S","N","S","N","N","S","N","S","N","S","N","S","S","S","N","S","N",	"S",	"S",	"N",	"S",	"N",	"N",	"N",	"S",	"S",	"N",	"S",	"N",	"N",	"S",	"N",	"S",	"S",	"N",	"S", "N",	"N",	"N",	"S")
plotting_temperature<-cbind(plotting_temperature, N_S)

write.csv(plotting_temperature, "plot_temperatura.csv")


#Para la abundancia Martinazo e Hinojos son los extremos.


facet(k, facet.by = c( "Treatment"),
      short.panel.labs = FALSE)

###Para hallar los datos del modelo primero la ecuación####

library(broom)



#Tablas 

stargazer(modelsdeviance,type = "html", summary = FALSE, rownames = TRUE, out="modelsdeviance.htm")
stargazer(modelo_plot1, modelo_plot2, modelo_plot3, modelo_plot4, modelo_plot5, modelo_plot6, modelo_plot7, type = "html", summary = FALSE, rownames = TRUE, intercept.bottom=FALSE, out="modelos_gráficas.htm")
stargazer(i1.cont,i2.cont , i3.cont, type = "html", summary = FALSE, rownames = TRUE, intercept.bottom=FALSE, out="intertegular_random_colon_models.htm")
stargazer(indvi1.cont, iv1km2.cont, iv1km3.cont, type = "html", summary = FALSE, rownames = TRUE, intercept.bottom=FALSE, out="variance_models.htm")




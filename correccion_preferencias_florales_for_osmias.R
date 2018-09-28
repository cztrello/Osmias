library(reshape2)
library(dplyr)
library(tidyr)
library(plyr)

#######pruebas#####

temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
colnames (disolve_coberturaQ1)[25] <- "area"
colnames(Disuelto_CHOZA.HUERTA.TEJADA)[27] <- "AREA_RECAL"

Disuelto_UPO$ID<-rep(8)
Disuelto_ROCINA$ID<-rep(9)
Disuelto.VIVEROS$ID<-rep(3)
Disuelto_CHARENA$ID<-rep(5)
Disuelto_HINOJOS$ID<-rep(7)
Disuelto_GUADIAMAR$ID<-rep(11)
Disuelto_MARTINAZO$ID<-rep(10)
Disuelto_DEHESA.NUEVA$ID<-rep(6)
Disuelto_CORIA.DEL.RIO$ID<-rep(4)
Disuelto_MUSEO.FORESTAL$ID<-rep(12)
Disuelto_DEHESA.DE.ABAJO$ID<-rep(2)
Disuelto_CHOZA.HUERTA.TEJADA$ID<-rep(1)



area1km<-rbind(Disuelto_UPO, Disuelto_ROCINA, Disuelto.VIVEROS, Disuelto_CHARENA, Disuelto_HINOJOS, Disuelto_GUADIAMAR, Disuelto_MARTINAZO, Disuelto_DEHESA.NUEVA, Disuelto_CORIA.DEL.RIO, Disuelto_MUSEO.FORESTAL, Disuelto_DEHESA.DE.ABAJO, Disuelto_CHOZA.HUERTA.TEJADA) 
areas1km_cuantificacion <- spread(area1km, key = "DESC_OCUPA", value = "value")
areas1km_cuantificacion<-areas1km_cuantificacion[,-2]
View(areas1km_cuantificacion)
areas1km_cuantificacion[is.na(areas1km_cuantificacion)] <- 0

#operaciones con la abundnacia de plantas#
areas1km_definitivo<-areas1km_cuantificacion
write.csv(areas1km_definitivo, "areas_1km_final.csv")


areas_1km<-read.csv("~/Documents/bombus/data/areas_usos_1km/areas_1km_final.csv", sep=";")
#transpongo la columna con los datos a 1 km
View(areas_1km)
str(areas_1km)
areas1km_transposed<-t(areas_1km)
View(areas1km_transposed)
#para poner la primera row como head
colnames(areas1km_transposed) <- areas1km_transposed[1, ]
areas1km_transposed <- areas1km_transposed[-1, ]
View(areas1km_transposed)
Location_n<-c(1,2,3,4,5,6,7,8,9,10,11,12)
areas1km_transposed<-cbind(Location_n,areas1km_transposed)
areas1km_final<-areas1km_transposed


write.csv(preferencias, "preferencias.csv")
preferencias_en_general<-read.csv("~/Documents/bombus/datos_definitivos/preferencia_flores_bombus.csv", sep=";")
preferencias_osmias<-preferencias_en_general[,2]

####rehago las áreas y las multiplico por las abundancias solo por las preferencias florales##


#Abundance<-read.csv("datos_definitivos/Abundance_flowers.csv", 
                    header=TRUE, sep=";", 
                    na.strings=c(""," ","NA")) #la session en la carpeta corregidas)
#head(Abundance)
#Listo el número de familias que tengo
#l<-levels(Abundance$Familia)
#l<-as.data.frame(l)
#z<-levels(Abundance$Land.use)
#z<-as.data.frame(z)

#especifico que son factores
#Abundance$Location_n<-as.factor(Abundance$Location_n)
#Abundance$Specie<-as.factor(Abundance$Specie)
#is.factor

# Abundancias totales por localidad y por familia en cada localidad
#summarise_abundance<-Abundance %>%
  #na.omit() %>%
  #group_by(Locality, Familia) %>% 
  #summarise_each(funs(sum(., na.rm = TRUE), 
                      #max(., na.rm = TRUE)) , Number)
#summarise_abundance
# por usos del suelo
#summarise_landuse<-Abundance %>%
  #na.omit() %>%
  #group_by(Land.use) %>% 
  #summarise_each(funs(sum(., na.rm = TRUE), 
                      #mean(., na.rm = TRUE)) , Number)
#summarise_landuse

# por familia y usos del suelo

Abundance2<-read.csv("datos_definitivos/Abundance_flowers_sinmartinazo.csv", 
                     header=TRUE, sep=";", 
                     na.strings=c(""," ","NA")) #la session en la carpeta corregidas)
head(Abundance2)
levels(Abundance2$Locality)

#Esta es la que vale, con la que me quedo!!!!!
#summarise_landuse_correcion<-Abundance2 %>%
#na.omit() %>%
#group_by(Locality, Land.use, Familia) %>% 
#summarise_each(funs(sum(., na.rm = TRUE), 
#                      mean(., na.rm = TRUE)) , Number)
#summarise_landuse_correcion
#head(summarise_landuse_correcion)

#write.csv(summarise_landuse_correcion, file = "summarise_landuse_correcion_sinmartinazo.csv")
View(summarise_landuse_correcion)

#Correciones de las medias de usos dobles en Hinojos, Dehesa de abajo, Coria del río y Choza Huerta tejada
#Estas correciones se obtienen a traves de la familia y el numero por localidad. En el caso de que haya familias nuevas en una se añaden a la media.
# Es decir, tiene un ligero sesgo.
clasificacion_summarise_redo2 <- read.csv("~/Documents/bombus/datos_definitivos/correccion_clasificacion_summarise_redo2.csv", sep=";",na.strings=c(""," ","NA"), comment.char="#") 
clasificacion_summarise_recibe_sinmartinazo <- read.csv("~/Documents/bombus/datos_definitivos/correccion_clasificacion_summarise_recibe_sinmartinazo.csv", sep=";", na.strings=c(""," ","NA"), comment.char="#")
View(clasificacion_summarise_redo2)

#clasificacion_summarise_redo2<-read.csv("datos_definitivos/correcion_clasificacion_summarise_redo2.csv", 
#                    header=TRUE, sep=";", 
#                     na.strings=c(""," ","NA")) #la session en la carpeta corregidas)
#head(clasificacion_summarise_redo2)
#clasificacion_summarise_recibe<-read.csv("datos_definitivos/correcion_clasificacion_summarise_recibe_sinmartinazo.csv", 
#                                       header=TRUE, sep=";", 
# na.strings=c(""," ","NA")) #la session en la carpeta corregidas)
library(dplyr)
landuse_pegar<-clasificacion_summarise_redo2 %>%
  group_by(Locality, Land.use, Familia) %>% 
  summarise(sum=sum(Number_sum))
str(landuse_pegar)

landuse_pegar_recibe<-clasificacion_summarise_recibe_sinmartinazo%>%
  #na.omit() %>%
  group_by(Locality, Land.use, Familia) %>% 
  summarise(sum=sum(Number_sum))
str(landuse_pegar_recibe)

#hago una fusion de dataframes
landuse_summarise_bind_sinmartinazo<-rbind(landuse_pegar, landuse_pegar_recibe)
landuse_summarise_bind_sinmartinazo<-as.data.frame(landuse_summarise_bind_sinmartinazo)
str(landuse_summarise_bind_sinmartinazo)
landuse_summarise_bind_temp_sinmartinazo<-read.csv("~/Documents/bombus/datos_definitivos/landuse_summarise_bind_temp_sinmartinazo.csv", sep=",", comment.char="#")

#Quiero saber cuántos tipos de categorias he medido por localidad
head(landuse_summarise_bind_sinmartinazo)
numero_usosdelsuelo_por_localidades<- landuse_summarise_bind_sinmartinazo%>%
  #na.omit() %>%
  group_by(Land.use) %>% 
  summarise(no_rows = length(Land.use))


View(numero_usosdelsuelo_por_localidades)

#De aquí elimino las que tienen las familias 

landuse_summarise_osmia_preference <- landuse_summarise_bind_sinmartinazo %>%  
  filter(Familia  != "Aristolochiaceae" & Familia  != "Euphorbiaceae" & Familia  !="Anacardiaceae")

colnames(landuse_summarise_osmia_preference)[3]<- "Familia"   


library(dplyr)
#no_cuadran<-anti_join(landuse_summarise_bind_sinmartinazo, summarise_landuse_correcion)
#cuadran<-semi_join(landuse_summarise_bind_sinmartinazo, summarise_landuse_correcion)
#str(cuadran)
#str(no_cuadran)
#str(landuse_summarise_bind_sinmartinazo)
#str(summarise_landuse_correcion)
#View(summarise_landuse_correcion)
#View(landuse_summarise_bind_sinmartinazo)
#levels(summarise_landuse_correcion$Locality)
#View(no_cuadran)
#View(cuadran)

#unique(landuse_summarise_bind_sinmartinazo,summarise_landuse_correcion)
# Hago por las áreas de los usos del suelo. Las áreas vacías de información las relleno con las medias de los otros sitios
# de manera que extrapolo que hay en el resto y hallo la abundancia floral en el buffer en cada localidad.
####Compruebo que areas de uso del suelo tengo en el de abundancias y familias

levels_landuse_osmia_preference<-sapply(landuse_summarise_osmia_preference$Land.use, levels)
str(landuse_summarise_osmia_preference$Land.use)
vec<-landuse_summarise_osmia_preference$Land.use
vec<-as.data.frame(vec)
summary(vec, maxsum=200)


#Calculo la suma de cada uno de los usos del suelo por localidad y le agrego un id de localidad
#para asemejarlo al dataframe del total ordenado que se llamará después cuantificacion area

landuse_suma_abundancias_per_landuse_osmia_preference<-landuse_summarise_osmia_preference %>%
  group_by(Land.use) %>% 
  summarise(sum(sum), mean(mean(sum)))
landuse_suma_abundancias_per_landuse_osmia_preference
View(landuse_suma_abundancias_per_landuse_osmia_preference)
write.csv(landuse_suma_abundancias_per_landuse_osmia_preference,"landuse_abundancias_por_usopre_osmias_preference.csv")


# Por area del uso del suelo
landuse_suma_abundancias_por_localidad_uso_osmia_preference<-landuse_summarise_osmia_preference %>%
  group_by(Locality,Land.use) %>% 
  summarise(sum_total=sum(sum))%>%
  arrange(sum_total)
landuse_suma_abundancias_por_localidad_uso_osmia_preference
View(landuse_suma_abundancias_por_localidad_uso)
landuse_suma_abundancias_por_uso<-as.data.frame(landuse_suma_abundancias_por_uso)
write.csv(landuse_suma_abundancias_por_uso,"landuse_abundancias_por_usopre.csv")


landuse_suma_abundancias_por_landuse_osmia_preference<-landuse_summarise_osmia_preference %>%
  group_by(Land.use) %>% 
  summarise(suma=sum(sum), mean(mean(sum))) %>% 
  arrange(suma)
landuse_suma_abundancias_por_localidad_osmia_preference

as.data.frame(landuse_suma_abundancias_por_uso2)
write.csv(landuse_suma_abundancias_por_uso2,"landuse_abundancias_por_uso.csv")

media_abundancia_usos_osmias_preference<-merge(landuse_suma_abundancias_per_landuse_osmia_preference, numero_usosdelsuelo_por_localidades)
colnames(media_abundancia_usos_osmias_preference)[2]<-"suma"
colnames(media_abundancia_usos_osmias_preference)[4]<-"numero"

media_abundancia_usos_osmias_preference<-media_abundancia_usos_osmias_preference[,-3]
str(media_abundancia_usos_osmias_preference)
media_abundancia_usos_osmias_preference$mean<- media_abundancia_usos_osmias_preference$suma/media_abundancia_usos_osmias_preference$numero
write.csv(media_abundancia_usos_osmias_preference, "media_abundancia_usos_osmias_preference.csv")
media_abundancia_usos_osmias_preference<-read.csv("~/Documents/Bombus/media_abundancia_usos_osmias_preference.csv", header= TRUE, sep= ",")
media_abundancia_usos_bombus_preference<-read.csv("~/Documents/Bombus/media_abundancia_usos_bombus_preference.csv", header= TRUE, sep= ";")

##################para después por si hay que completar datos###########################              


library(dplyr)

#riqueza por familias area
#¿¿¿No debería de estimarlas mediante esto: Estimating Species Richness CHAO1 and ACE  en las áreas que no conozco??? 
# We can also estimate the number of species in our forest fragments. To calculate these abundance-based richness diversity estimators, we can use "estimateR" function. We will get the observed number of species in each sample, and the estimated number following Chao1 and ACE estimators.


landuse_riqueza_area_uso_del_suelo_osmias_preference<-landuse_summarise_osmias_preference %>%
  group_by(Land_use, Familia) %>% 
  summarise(Count= n())
View(landuse_riqueza_area_uso_del_suelo_osmias_preference)

landuse_riqueza_area_uso_del_suelo_count_osmias_preference<-landuse_summarise_osmias_preference %>%
  group_by(Land.use) %>% 
  summarise(Count= n())
View(landuse_riqueza_area_uso_del_suelo_count_osmias_preference)

landuse_riqueza_area_uso_del_suelo<-as.data.frame(landuse_riqueza_area_uso_del_suelo)
temp2<-landuse_riqueza_area_uso_del_suelo$Land.use
as.data.frame(temp2)
temp2<-table(temp2)
richness_usos_del_suelo<-as.data.frame(temp2)
write.csv(richness_usos_del_suelo, 'richness_usos_del_suelo.csv')


#Ahora extrapolo estas riquezas y estas abundancias a un csv principal


##############recover de nuevo###########################

View(areas1km_final)
#sustituyo los espacios por puntos para que no de problemas a la hora de hacer el gather

library(tidyr)
names(areas1km_final)
areas1km_final<-transform(areas1km_final,dymmyvar=1)
areas1km_final$dymmyvar <- NULL
names(areas1km_final)

cuantificacion_areas <- gather(areas1km_final, key = "area", value = "number", AGRCOLA.GANADERO:VEGETACIN.LAGUNAR)
cuantificacion_areas
head(cuantificacion_areas) 
#Compruebo que está la frecuencia de 12
x<-data.frame(cuantificacion_areas$area)
cuantificacion_areas_area<-summary(x, maxsum = 1500)
cuantificacion_areas_area<-as.data.frame(cuantificacion_areas_area)
cuantificacion_areas_area

#cargo la correspondencia con las abundancias.

correspondencia_abundancias_osmia_preference<- read.csv2("~/Documents/Osmias/correspondencia_abundancias_osmia_preference.csv", header=FALSE, sep=";", encoding="latin1", dec=",")
correspondencia_abundancias_osmia_preference$V2<-as.numeric(as.character(correspondencia_abundancias_osmia_preference$V2))
#genero un número para multiplicar cada área del uso del suelo por el número de localidades que aparece
# en la cuantificación de las areas.
list1<-1:111
list2 <- rep(12,length(list1))
n<-12
list3 <- cbind(correspondencia_abundancias_osmia_preference, list2)
str(list3)
#ahora lo replico 13 veces.
correspondencia_abundancias_expanded <- list3[rep(seq(nrow(list3)), list3$list2), 1:2]
str(correspondencia_abundancias_expanded)
colnames(correspondencia_abundancias_expanded)[1]<-"area"
data1<-correspondencia_abundancias_expanded$area
data2<-as.factor(cuantificacion_areas$area)
add_levels_to_cuantificacion_areas<-setdiff(levels(data1), levels(data2))
add_levels_to_cuantificacion_areas<-as.data.frame(add_levels_to_cuantificacion_areas)
colnames(add_levels_to_cuantificacion_areas)[1]<-"area"
number<-rep(0,31)
Location_n<-rep(1:12,31)
Location_n<-sort(Location_n)
add_levels_to_cuantificacion_areas<-cbind(add_levels_to_cuantificacion_areas, number)
add_levels_to_cuantificacion_areas<-add_levels_to_cuantificacion_areas%>% slice(rep(row_number(), 12))
add_levels_to_cuantificacion_areas<-cbind(add_levels_to_cuantificacion_areas, Location_n)
add_levels_to_cuantificacion_areas<-add_levels_to_cuantificacion_areas[,-3]
area<-rep("MATORRAL.DENSO.ARBOLADO..CONIFERAS.EUCALIPTOS",12)
V2<-c(41,41,41,41,41,41,41,41,41,41,41,41)
add_to_cuantificacion_areas2<-cbind(area,V2)
add_to_cuantificacion_areas2<-as.data.frame(add_to_cuantificacion_areas2)
str(add_to_cuantificacion_areas2)
add_to_cuantificacion_areas2$V2<-as.numeric(as.character(add_to_cuantificacion_areas2$V2))
cuantificacion_areas_final_preference<-rbind(add_to_cuantificacion_areas2, correspondencia_abundancias_expanded)


levels(cuantificacion_areas$area)
str(cuantificacion_areas)
library(dplyr)
cuantificacion_areas$area<-as.factor(cuantificacion_areas$area)
#abundance_in_areas_of_1km_buffer<-semi_join(cuantificacion_areas, cuantificacion_areas_final_preference)
abundance_in_areas_of_1km_buffer<-semi_join(cuantificacion_areas_final_preference, cuantificacion_areas)
levels(abundance_in_areas_of_1km_buffer$area)
View(abundance_in_areas_of_1km_buffer)
#abundance_in_areas_of_1km_buffer<-unique(abundance_in_areas_of_1km_buffer)
abundance_in_areas_of_1km_buffer$V2<-as.numeric(as.character(abundance_in_areas_of_1km_buffer$V2))
abundance_in_areas_of_1km_buffer$area<-as.character(abundance_in_areas_of_1km_buffer$area)
cuantificacion_areas$number<-as.numeric(as.character(cuantificacion_areas$number))
abundance_in_areas_of_1km_buffer<-abundance_in_areas_of_1km_buffer[order(abundance_in_areas_of_1km_buffer$area),]
cuantificacion_areas<-cuantificacion_areas[order(cuantificacion_areas$area),]

area_total<-aggregate(cuantificacion_areas$number, by=list(Location_n=cuantificacion_areas$Location_n), FUN=sum)
area_total$Location_n<-as.numeric(as.character(area_total$Location_n))
area_total<-area_total[order(area_total$Location_n),]
area_total$Location_n<-as.factor(area_total$Location_n)
area_total<-area_total%>% slice(rep(row_number(), 81))
str(area_total)


cuantificacion_areas$
cuantificacion_areas
str(cuantificacion_areas)
levels_cuantificacionareas<-levels(cuantificacion_areas$area)
levels_abundance_in_areas<-levels(abundance_in_areas_of_1km_buffer$area)
setdiff(levels_abundance_in_areas,levels_cuantificacionareas)
str(abundance_in_areas_of_1km_buffer)

abundance_in_areas_of_1km_buffer<-as.data.frame(abundance_in_areas_of_1km_buffer)
abundance_in_areas_of_1km_buffer <- abundance_in_areas_of_1km_buffer[order(area),] 

combinacion<-cbind(cuantificacion_areas, abundance_in_areas_of_1km_buffer)
head(combinacion)
str(combinacion)
colnames(cuantificacion_areas)
colnames(cuantificacion_areas_final_preference)
combinacion$V2<-as.numeric(as.character(combinacion$V2))
combinacion<-cbind(combinacion, area_total)
colnames(combinacion)[7]<-"area_total"
combinacion$number<-as.integer(combinacion$number)
head(combinacion_areatotal)
head(combinacion)
library(dplyr)
combinacion<-mutate(combinacion, percent = number * 100/area_total)

#multiplico cada una de de los valores por la correspondencia de las abundancias

matriz_abundancias_uso_localidad_preferences<- combinacion %>% mutate(abundancia_real = (V2/10) * percent)
head(matriz_abundancias_uso_localidad_preferences)
summary(matriz_abundancias_uso_localidad_preferences)


###########lo mismo para las riquezas

#list1<-1:111
#list2 <- rep(13,length(list1))
#list3 <- cbind(correspondencia_riquezas, list2)
#str(list3)
#ahora lo replico 13 veces.
#correspondencia_riquezas_expanded <- list3[rep(seq(nrow(list3)), list3$list2), 1:2]
#str(correspondencia_riquezas_expanded)
#str(cuantificacion_areas)
#combinacion_riquezas<-cbind(cuantificacion_percent,correspondencia_riquezas_expanded)
#head(combinacion_riquezas)
#multiplico cada una de de los valores por la correspondencia de las abundancias, dividido por 10 que son los metros cuadrados

#matriz_riquezas_uso_localidad<- combinacion_riquezas %>% mutate(riqueza_real = (V2/10) * percent)
#head(matriz_riquezas_uso_localidad)
#View(matriz_riquezas_uso_localidad)



#qué localidades tienen más abundancia
library(dplyr)
matriz_abundancias_uso_localidad_preferences$Location_n<-as.factor(matriz_abundancias_uso_localidad_preferences$Location_n)
str(matriz_abundancias_uso_localidad_preferences)
colnames(matriz_abundancias_uso_localidad_preferences)
matriz_abundancias_uso_localidad_preferences  %>%
  group_by(Location_n)%>% 
  summarise(abundancia_total=sum(abundancia_real))
head(matriz_abundancias_uso_localidad_preferences)
View(matriz_abundancias_uso_localidad_preferences)

matriz_abundancias_uso_localidad_arrange<-aggregate(matriz_abundancias_uso_localidad_preferences$abundancia_real, by=list(Location_n=matriz_abundancias_uso_localidad_preferences$Location_n), FUN=sum)

colnames(matriz_riquezas_uso_localidad_arrange$Location_n, abundancia)


str(matriz_abundancias_uso_localidad_preferences_arrange)
#
head(matriz_abundancias_uso_localidad_preferences)

matriz_abundancias_uso_localidad_count<-matriz_abundancias_uso_localidad_preferences%>% group_by(Location_n)%>%
  filter (number !=0)%>% 
  tally()%>% 
  arrange(desc(n))


####lo mismo para la diversidad

#list1<-1:111
#list2 <- rep(13,length(list1))
#list3 <- cbind(correspondencia_shanon, list2)
#str(list3)
#ahora lo replico 13 veces.
#correspondencia_shanon_expanded<- list3[rep(seq(nrow(list3)), list3$list2), 1:2]
#str(correspondencia_shanon_expanded)
#str(cuantificacion_areas)
#combinacion_shanon<-cbind(cuantificacion_areas,correspondencia_shanon_expanded)
#head(combinacion_shanon)
#multiplico cada una de de los valores por la correspondencia de las abundancias

#matriz_shanon_uso_localidad<- combinacion_shanon %>% mutate(shanon_real = (V2/10) * number)
#head(matriz_shanon_uso_localidad)
#View(matriz_shanon_uso_localidad)



#qu´é localidades tienen más abundancia
#matriz_shanon_uso_localidad_arrange<-matriz_riquezas_shanon_localidad  %>%
 # group_by(AREAS_PERCENTS) %>% 
  #summarise(riqueza_total=sum(riqueza_real)) %>% 
  #arrange(desc(riqueza_total))
#matriz_shanon_uso_localidad_arrange 


#matriz_shanon_uso_localidad_count<-matriz_shanon_uso_localidad%>% group_by(AREAS_PERCENTS)%>%
  #filter (number !=0)%>% 
  #tally()%>% 
  #arrange(desc(AREAS_PERCENTS))

##Divido la abundancia y la riqueza por el número de áreas medidas####
x1<-matriz_abundancias_uso_localidad_arrange 

x2<-matriz_abundancias_uso_localidad_count
x2<-as.data.frame(x2)


#x3[order(match(x3[,1],x2[,1])),]

dataframe_abundancia_relative<-merge(x1,x2)
names(dataframe_abundancia_relative)=c("Location_n", "abundancia_total_preference_osmias", "n")
dataframe_abundancia_relative<- transform(dataframe_abundancia_relative, abundancia_relativa= abundancia_total_preference_osmias / n)
dataframe_abundancia_relative$Locality<-c("Choza huerta tejada","Martinazo", "Guadiamar", "Museo forestal", "Dehesa de abajo", "Viveros", "Hampa", "Charena", "Dehesa nueva", "Hinojos", "UPO", "Rocina")
abundance_in_areas_of_1km_buffer<-dataframe_abundancia_relative[order(dataframe_abundancia_relative$abundancia_relativa),]

abundancia_percent<-dataframe_abundancia_relative[rep(1:nrow(dataframe_abundancia_relative),each=4),]
abundancia_percent<-rename(abundancia_percent, c("abundancia_relativa" = "abundancia_relativa_1km"))
code <- c( "1N", "1S", "1N", "1S", "10N", "10S", "10N", "10S","11N", "11S", "11N", "11S","12N", "12S", "12N", "12S","2N", "2S", "2N", "2S","3N", "3S", "3N", "3S","4N", "4S", "4N", "4S","5N", "5S", "5N", "5S","6N", "6S", "6N", "6S","7N", "7S", "7N", "7S","8N", "8S", "8N", "8S","9N", "9S", "9N", "9S")
abundancia_percent<-cbind(abundancia_percent, code)
abundancia_percent<-as.data.frame(abundancia_percent)
View(abundancia_percent)

write.csv(abundancia_percent,"abundancia_percent_osmia_dataframe.csv")



library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

####Primera aproximación a los datos#####
Canes <- read.csv("~/Documents/Osmias/Canes.csv", sep=";", quote="\"", stringsAsFactors=FALSE)
Canes<-Canes[c(1:1716),]
Datos_distancia_intertegular <- read.csv("~/Documents/Osmias/Datos_distancia_intertegular.csv", sep=";")
Datos_species_distancia_2 <- read.csv("~/Documents/Osmias/Datos_species_distancia_2.csv", sep=";", comment.char="#")
Datos_species_distancia <- read.csv("~/Documents/Osmias/Datos_species_distancia.csv", sep=";")
Sexo_specie_osmias <- read.csv("~/Documents/Osmias/Sexo_specie_osmias .csv", sep=";")
Sexo_specie_osmias_reubicacion <- read.csv("~/Documents/Osmias/Sexo_specie_osmias_reubicacion.csv", sep=";")
Sexo_specie_prueba_filtrado<-read.csv("~/Documents/Osmias/Sexo_specie_prueba_filtrado.csv", sep=";")
colnames(Sexo_specie_osmias_reubicacion)[1]<-"ID"

str(Canes)
extract_forCanes<-Canes[,c(1,3,4)]
#Primero monto un bucle para poner cada una de las clasificaciones de NS para poder hacer
#un merge con los datos de temperatura y de paisaje de los bombus.


code<-NULL
for (n in 1:nrow(Canes)) {
  if (Canes$Location[n]== "vivero" ) { 
    code[n]<-3} else if (Canes$Location[n]== "UPO" ) { 
      code[n]<-8}else if (Canes$Location[n]== "Rocina" ) { 
        code[n]<-9}else if (Canes$Location[n]== "Martinazo" ) { 
          code[n]<-10}else if (Canes$Location[n]== "hinojos" ) { 
            code[n]<-7}else if (Canes$Location[n]== "guadiamar" ) { 
              code[n]<-11}else if (Canes$Location[n]== "dehesa nueva" ) { 
                code[n]<-6}else if (Canes$Location[n]== "Dehesa de abajo" ) { 
                  code[n]<-2}else if (Canes$Location[n]== "coria del rio" ) { 
                    code[n]<-4}else if (Canes$Location[n]== "choza huerta tejada" ) { 
                      code[n]<-1}else if (Canes$Location[n]== "Charena" ) { 
                        code[n]<-5}}
CODE_ORIENTATION<-NULL
for (n in 1:nrow(Canes)) {
  if (Canes$Orientation[n]== "norte" ) { 
    CODE_ORIENTATION[n]<-"N"} else if (Canes$Orientation[n]== "sur" ) { 
      CODE_ORIENTATION[n]<-"S"}}

Code<-paste(code,CODE_ORIENTATION, sep="")

Canes<-cbind(Canes,Code)
View(Canes)


combined<-anti_join(Sexo_specie_osmias_reubicacion,Sexo_specie_osmias, by="Specie")
specie_sexo_unico<-unique(Sexo_specie_prueba_filtrado)
specie_sexo_unico_solo_numeros<-unique(Sexo_specie_prueba_filtrado$ID)
str(specie_sexo_unico)
str(Datos_distancia_intertegular)
Datos_species_distancia<-Datos_species_distancia[,-3]
colnames(Datos_species_distancia)[1]<-"ID"

Datos_specie<-merge(Datos_species_distancia, Datos_species_distancia_2, by="ID")
colnames(Datos_specie)[2]<-"IT"
colnames(Datos_specie)[5]<-"Specie"
Datos_specie<-Datos_specie[,c(-3,-4,-7)]
Datos_distancia_intertegular<-Datos_distancia_intertegular[,c(1:2)]
Datos_specie$IT<-as.numeric(Datos_specie$IT)
Sexo_specie_it<-full_join(specie_sexo_unico, Datos_distancia_intertegular, by=NULL)
Datos_specie<-Datos_specie[,c(1,4,3,2)]
Sexo_specie_it[,3]
#comprobando por el número de caña y por el tipo completo los datos que faltan de la especie en 8
for (n in 1:nrow(Sexo_specie_it)) {
  if ( Sexo_specie_it$ID[n]== "75.6"  || Sexo_specie_it$ID[n]== "68.4" 
  || Sexo_specie_it$ID[n]== "124.2" || Sexo_specie_it$ID[n]== "114.4" ) { 
    Sexo_specie_it$Specie[n]<-"Osmia bicornis"}  else if 
  ( Sexo_specie_it$ID[n]== "58.14"  || Sexo_specie_it$ID[n]== "3a.6" 
    || Sexo_specie_it$ID[n]== "49.8" || Sexo_specie_it$ID[n]== "23.2" 
    || Sexo_specie_it$ID[n]== "23.1") { 
    Sexo_specie_it$Specie[n]<-"Osmia latreillei"} else if
  ( Sexo_specie_it$ID[n]== "129.13") { 
    Sexo_specie_it$Specie[n]<-"Osmia brevicornis"}else if
  ( Sexo_specie_it$ID[n]== "5a.2") { 
    Sexo_specie_it$Specie[n]<-"Osmia submicans"}}
#Añado esta columna para identificar donde están los duplicados
b <- Datos_specie[,1]
b <- b==1
b <- cumsum(b)
Datos_specie<-cbind(Datos_specie,b)
b <- Sexo_specie_it[,1]
b <- b==3
b <- cumsum(b)
b<-replace(b,b=="0","1")
Sexo_specie_it<-cbind(Sexo_specie_it,b)
#Reordeno el dataframe por sus columnas para que puedan encajarse con un rbind

str(b)
Datos_individuales<-rbind(Datos_specie, Sexo_specie_it)
str(Datos_individuales)
#Curro se llevo dos especies a identiicar que no están pasadas. Importante localizarlas.
str(specie_sexo_unico)
str(Datos_distancia_intertegular)
str(Datos_specie)
str(Sexo_specie_it)
#junto los dos dataframes
str(Sexo_specie_it)
Sexo_specie_final<-unique(Datos_individuales)
View
View(Sexo_specie_final)
str(Sexo_specie_final_ID)
#Encontrar los duplicados por si existen posibles errores al pasar los datos
#Esto es solo una prueba!
number_of_duplicates<-Sexo_specie_final[duplicated(Sexo_specie_final$ID),]
View(Sexo_specie_final)
View(Sexo_specie_it)
View(Datos_specie)
table(number_of_duplicates$ID)
exclusivos_datos_specie<-anti_join(Datos_specie, Sexo_specie_it, by="ID")
exclusivos_totales_prueba<-rbind(exclusivos_datos_specie, Sexo_specie_it)
str(exclusivos_totales_prueba)
Sexo_specie_final_prueba<-unique(exclusivos_totales_prueba)
str(Sexo_specie_final_prueba)
number_of_duplicates_prueba<-Sexo_specie_final_prueba[duplicated(Sexo_specie_final_prueba$ID),]
#De los datos repetidos en el caso de que sean erroneos el sexo, se revisan, en el caso de que sean medidas repetidas
#realizo la media. En este caso sería en los números 11.5, 11.4, 11.3,11.2,11.1.


Cribado_Osmias_sexo_specie<-subset(Sexo_specie_final_prueba, ID!=128.2 & ID!=94.1 & ID!=86.3 & ID!=75.12 & 
           ID!=61.7& ID!=58.1& ID!=152.3& ID!=129.18& ID!=112.10& ID!=11.4& ID!=11.3& ID!=103.1)

#Hago las medias a los que sí se puede
#CUIDADO, SI ESTÁ GGPLOT2 CARGADO TIENE INCOMPATIBILIDADES CON DPLYR Y SOLO HACE LA MEDIA DE UNA FILA.
#DESCARGAR PLYR.
Sexo_specie_final_prueba_medias<-Cribado_Osmias_sexo_specie%>% 
                  filter(ID=="11.5" | ID =="11.2" |  ID=="11.1") %>%
                      group_by(ID, Sex, Specie)%>%
                      summarise (mean_it= mean (IT)) 
Sexo_specie_final_prueba_medias<-as.data.frame(Sexo_specie_final_prueba_medias)


#Hago por caña tanto media IT de macho como IT de hembra, para unirlo al dataframe anterior
Sexo_specie_final_prueba_completo_a_unir_a_medias<-Cribado_Osmias_sexo_specie%>% 
  filter(ID!="11.5" | ID !="11.2" |  ID!="11.1")

Sexo_specie_final_prueba_completo_a_unir_a_medias<-as.data.frame(Sexo_specie_final_prueba_completo_a_unir_a_medias)
Numbers_per_specie <-Sexo_specie_final_prueba_completo_a_unir_a_medias%>%
  group_by(Specie) %>%
  summarise(no_rows = length(Specie))
Number_per_specie<-as.data.frame(Numbers_per_specie)

Sexo_specie_final_prueba_completo_a_unir_a_medias<-Sexo_specie_final_prueba_completo_a_unir_a_medias[,c(-5)]
colnames(Sexo_specie_final_prueba_medias)[4]<-"IT"
Sexo_specie_final<-rbind(Sexo_specie_final_prueba_medias,Sexo_specie_final_prueba_completo_a_unir_a_medias)
#En el caso de que los duplicados coincidan en todo menos en su número, hago medias de su IT
#debido a que son medidas repetidas.
#En el caso a que no coincidan he de revisarlas.



#Separo por caña la medida y la especie
Sexo_specie_it_total_cane<-Sexo_specie_final %>% separate(ID, 
                c("number", "Individual"))


Sexo_specie_it_total_cane$number<-as.numeric(Sexo_specie_it_total_cane$number)

write.csv(Sexo_specie_it_total_cane, "Sexo_specie_it_total_cane.csv")

#Medias por caña y por especie

Media_sexo_specie_caña<-Sexo_specie_it_total_cane %>% group_by(number,Sex,Specie) %>% filter(!is.na(IT)) %>% summarise (mean_it = mean(IT))
Media_sexo_specie_caña_total<-Sexo_specie_it_total_cane %>% group_by(number,Specie) %>% filter(!is.na(IT)) %>% summarise (mean_it = mean(IT))

View(Media_sexo_specie_caña)

#Solo voy a trabajar con el de Osmias bicornis de momento
           Osmia_bicornis<-subset(Media_sexo_specie_caña, Specie=="Osmia bicornis")
           Osmia_bicornis_media_total<-subset(Media_sexo_specie_caña_total, Specie=="Osmia bicornis")
           
           Osmia_bicornis_splitted<-split(Osmia_bicornis, Osmia_bicornis$Sex, drop=TRUE)
           Osmia_bicornis_splitted<-as.data.frame(Osmia_bicornis_splitted)
           Osmia_bicornis_female<-Osmia_bicornis_splitted[[1]]
           Osmia_bicornis_male<-Osmia_bicornis_splitted[[2]]
           Osmia_bicornis_male<-as.data.frame(Osmia_bicornis_male)
           Osmia_bicornis_female<-as.data.frame(Osmia_bicornis_female)
           colnames(Osmia_bicornis_male)[4]<-"mean_male"
           colnames(Osmia_bicornis_female)[4]<-"mean_female"
           Mean_male<-Osmia_bicornis_male[,c(1,4)]
           Mean_female<-Osmia_bicornis_female[,c(1,4)]
           Mean_total<-Osmia_bicornis_media_total[,c(1,3)]
           merge1<-full_join(Mean_male,Mean_female, by="number")
           Dataframe_mean_male_female_total_osmia_bicornis<-full_join(merge1,Mean_total, by="number")
           colnames(Dataframe_mean_male_female_total_osmia_bicornis)[4]<-"mean_it_bicornis"
           Dataframe_mean_male_female_total_osmia_bicornis<-full_join(merge1,Mean_total, by="number")
           Dataframe_total_osmia_bicornis<-merge(Dataframe_mean_male_female_total_osmia_bicornis,Canes, by="number")
           head(Dataframe_total_osmia_bicornis)
           View(Dataframe_total_osmia_bicornis)

           #Lo mismo para el resto de especies
           
           Osmia_latreillei<-subset(Media_sexo_specie_caña, Specie=="Osmia latreillei")
           Osmia_latreillei_media_total<-subset(Media_sexo_specie_caña_total, Specie=="Osmia latreillei")
           
           Osmia_latreillei_splitted<-split(Osmia_latreillei, Osmia_latreillei$Sex, drop=TRUE)
           Osmia_latreillei_splitted<-as.data.frame(Osmia_latreillei_splitted)
           Osmia_latreillei_female<-Osmia_latreillei_splitted[[1]]
           Osmia_latreillei_male<-Osmia_latreillei_splitted[[2]]
           Osmia_latreillei_male<-as.data.frame(Osmia_latreillei_male)
           Osmia_latreillei_female<-as.data.frame(Osmia_latreillei_female)
           colnames(Osmia_latreillei_male)[4]<-"mean_male_latreillei"
           colnames(Osmia_latreillei_female)[4]<-"mean_female_latreillei"
           Mean_male_latreillei<-Osmia_latreillei_male[,c(1,4)]
           Mean_female_latreillei<-Osmia_latreillei_female[,c(1,4)]
           Mean_total_latreillei<-Osmia_latreillei_media_total[,c(1,3)]
           merge_latreillei<-full_join(Mean_male_latreillei,Mean_female_latreillei, by="number")
           Dataframe_mean_male_female_total_osmia_latreillei<-full_join(merge_latreillei,Mean_total_latreillei, by="number")
           colnames(Dataframe_mean_male_female_total_osmia_latreillei)[4]<-"mean_it_latreillei"
           Dataframe_total_osmia_latreillei<-merge(Dataframe_mean_male_female_total_osmia_latreillei,Canes, by="number")
           
           Dataframe_total_osmia_bicornis_latreillei<-left_join(Dataframe_total_osmia_bicornis,Dataframe_total_osmia_latreillei, by="number")
           head(Dataframe_total_osmia_bicornis_latreillei)
           str(Dataframe_total_osmia_bicornis_latreillei)
           View(Dataframe_total_osmia_bicornis)
           
#!!!!!!!No sé como hacer para que estén correlacionados las medidas de los machos y hembras para ver si se corresponden con la caña.

Dataframe_total_osmia_bicornis$vestib<-as.numeric(Dataframe_total_osmia_bicornis$vestib)          
Dataframe_total_osmia_bicornis$vestib3<-as.numeric(Dataframe_total_osmia_bicornis$vestib3)          
#Voy a tener que transformar los NAs a cero en los vestibulos y el resto porque no los capta.
#Medir el volumen que ocuparon, 25cm de caña vacías versus el tamaño de diámetro y total ocupado (vestíbulos, chambers, tapón..)

Osmia_summarise_NA_cero<-Dataframe_total_osmia_bicornis %>% mutate(vestib =if_else(is.na(vestib),0,vestib),
                                                                   vestib2 =if_else(is.na(vestib2),0,vestib2),
                                                                   vestib3 =if_else(is.na(vestib3),0,vestib3),
                                                                   vestib4 =if_else(is.na(vestib4),0,vestib4),
                                                                   cap.s =if_else(is.na(cap.s),0,cap.s),
                                                                   final.space =if_else(is.na(final.space),0,final.space))

View(Osmia_summarise_NA_cero)
View(Osmia_summarise_vestib)

Osmia_summarise_vestib<-Osmia_summarise_NA_cero %>% 
mutate (internal_space=vestib+vestib2+vestib3+vestib4+final.space, buffer_space= cap.s + internal_space )

Osmia_summarise_size<-Osmia_summarise_vestib %>% group_by(number,a.b) %>% filter(!is.na(size)) %>% 
  summarise (number_cells = length(size), mean_cells=mean(size))  
str(Dataframe_total_osmia_bicornis) 
str(Dataframe_total_osmia_bicornis)           

Osmia_summarise_size<-as.data.frame(Osmia_summarise_size)  
Osmia_summarise_size$number<-as.numeric(Osmia_summarise_size$number) 


Osmia_bicornis_semifinal<-full_join(Osmia_summarise_vestib, Osmia_summarise_size, by=c("number","a.b"))
str(Osmia_bicornis_semifinal)
Osmia_bicornis_semifinal2_aborts<-Osmia_bicornis_semifinal %>% group_by(number,a.b) %>% filter(!is.na(Aborts)) %>% 
  summarise (number_of_aborts = sum(Aborts))  
Osmia_bicornis_semifinal2_aborts<-as.data.frame(Osmia_bicornis_semifinal2_aborts)

Osmia_bicornis_semifinal<-Osmia_bicornis_semifinal[,c(-17,-18,-20,-21,-24,-26,-27,-28,-29,-30,-31,-32)]

head(Osmia_bicornis_semifinal)  
Osmia_bicornis_semifinal2_aborts_to_join<-Osmia_bicornis_semifinal %>% group_by(number,a.b)

head(Osmia_bicornis_semifinal2_aborts_to_join)
View(Osmia_bicornis_semifinal2_aborts_to_join)
Osmia_bicornis_semifinal2_aborts_to_join<-unique(Osmia_bicornis_semifinal2_aborts_to_join)

Code<-paste(code,CODE_ORIENTATION, sep="")

Osmia_bicornis_semifinal2_aborts_to_join<-as.data.frame(Osmia_bicornis_semifinal2_aborts_to_join)


dataframe_final_bombus2 <- read.csv("~/Documents/bombus/dataframe_final_bombus2.csv")

data_from_bombus_to_join_osmia<-dataframe_final_bombus2[,c(3,13,75,76,77)]
write.csv(data_from_bombus_to_join_osmia, "data_from_bombus_to_join_osmia.csv")
abundance_percent_1km<-read.csv("~/Documents/Osmias/abundancia_percent_osmia_dataframe.csv")
abundance_percent_1km<-abundance_percent_1km[,c(5,6,7)]
colnames(abundance_percent_1km)[3]<-"Code"
colnames(data_from_bombus_to_join_osmia)[1]<-"Code"
data_from_bombus_to_join_osmia<-merge(data_from_bombus_to_join_osmia, abundance_percent_1km, by="Code")
data_from_bombus_to_join_osmia<-unique(data_from_bombus_to_join_osmia)
dataframe_osmia_bicornis_semifinal<-merge(Osmia_bicornis_semifinal2_aborts_to_join, data_from_bombus_to_join_osmia, by="Code")
dataframe_osmia_bicornis_semifinal_aborts<-merge(dataframe_osmia_bicornis_semifinal, Osmia_bicornis_semifinal2_aborts, by= c("number","a.b"))
dataframe_osmia_bicornis_semifinal_summary<-dataframe_osmia_bicornis_semifinal %>% group_by(Code) 

write.csv(dataframe_osmia_bicornis_semifinal,"dataframe_osmia_bicornis_semifinal.csv")

str(dataframe_osmia_bicornis_semifinal)

#Algunos ploteos sin hacer summary por code.

p<-ggscatter(dataframe_osmia_bicornis_semifinal, x = "abundancia_relativa", y = "number_cells", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "abundancia_realtiva", ylab = "number_cells")
print(p)

p<-ggscatter(dataframe_osmia_bicornis_semifinal, x = "abundancia_relativa", y = "mean_cells", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "abundancia_realtiva", ylab = "number_cells")
print(p)

p<-ggscatter(Osmia_bicornis_semifinal, x = "abundancia_relativa", y = "number_cells", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "abundancia_realtiva", ylab = "number_cells")
print(p)


p<-ggscatter(dataframe_osmia_bicornis_semifinal, x = "riqueza_relativa", y = "number_cells", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "riqueza relativa", ylab = "Intmales_mean")
print(p)
p<-ggscatter(dataframe_osmia_bicornis_semifinal, x = "temp_mean", y = "buffer_space", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "temp_mean", ylab = "Intmales_mean")
print(p)
p<-ggscatter(dataframe_osmia_bicornis_semifinal, x = "abundancia_relativa", y = "mean_it", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "abundancia_relativa", ylab = "mean_it_total")
print(p)

p<-ggscatter(dataframe_osmia_bicornis_semifinal, x = "temp_mean", y = "mean_it", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "temp_mean", ylab = "Int_mean")
print(p)
p<-ggscatter(dataframe_osmia_bicornis_semifinal, x = "temp_range", y = "buffer_space", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "temp_range", ylab = "buffer_space")
print(p)

View(dataframe_osmia_bicornis_semifinal_aborts)


p<-ggscatter(dataframe_osmia_bicornis_semifinal_aborts, x = "abundancia_relativa", y = "number_of_aborts", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "abundancia_relativa", ylab = "number_of_aborts")
print(p)


z<-ggscatter(dataframe_osmia_bicornis_semifinal_aborts, x = "buffer_space", y = "number_of_aborts", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "buffer_space", ylab = "number_cells")
print(z)

z<-ggscatter(dataframe_osmia_bicornis_semifinal_aborts, x = "buffer_space", y = "number_of_aborts", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "buffer_space", ylab = "number_cells")
print(z)

str(dataframe_osmia_bicornis_semifinal_aborts)

z<-ggscatter(dataframe_osmia_bicornis_semifinal_aborts, x = "buffer_space", y = "number_of_aborts", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "buffer_space", ylab = "number_cells")
print(z)

z<-ggscatter(dataframe_osmia_bicornis_semifinal_aborts, x = "buffer_space", y = "number_of_aborts", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "buffer_space", ylab = "number_cells")
print(z)

z<-ggscatter(dataframe_osmia_bicornis_semifinal_aborts, x = "cap.s", y = "number_of_aborts", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "cap.s", ylab = "number_of_aborts")
print(z)

z<-ggscatter(dataframe_osmia_bicornis_semifinal_aborts, x = "cap.s", y = "number_of_aborts", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "cap.s", ylab = "number_cells")
print(z)

#####Con todo el dataframe completo de osmia bicornis para no tener solo medias###

Osmia_bicornis_todo<-subset(Sexo_specie_it_total_cane, Specie=="Osmia bicornis")

Osmia_bicornis_splitted_todo<-split(Osmia_bicornis_todo, Osmia_bicornis_todo$Sex, drop=TRUE)
Osmia_bicornis_splitted_todo<-as.data.frame(Osmia_bicornis_splitted_todo)
Osmia_bicornis_female<-Osmia_bicornis_splitted_todo[[1]]
Osmia_bicornis_male<-Osmia_bicornis_splitted_todo[[2]]
Osmia_bicornis_male<-as.data.frame(Osmia_bicornis_male)
Osmia_bicornis_female<-as.data.frame(Osmia_bicornis_female)
merge1<-rbind(Osmia_bicornis_male,Osmia_bicornis_female)
Dataframe_total_osmia_bicornis_todo<-merge(merge1,Canes, by="number")
head(Dataframe_total_osmia_bicornis_todo)

Dataframe_osmia_bicornis_todo<-merge(Dataframe_total_osmia_bicornis_todo, data_from_bombus_to_join_osmia, by="Code")
head(Dataframe_osmia_bicornis_todo)
Dataframe_osmia_bicornis_todo<-Dataframe_osmia_bicornis_todo[,c(1,2,3,4,5,6,35,36,37,38)]
Dataframe_osmia_bicornis_todo<-unique(Dataframe_osmia_bicornis_todo)
View(Dataframe_osmia_bicornis_todo)
bicornis_a<-Dataframe_osmia_bicornis_todo[Dataframe_osmia_bicornis_todo$Sex == "Male",]
bicornis_f<-Dataframe_osmia_bicornis_todo[Dataframe_osmia_bicornis_todo$Sex == "Female",]

str(Dataframe_osmia_bicornis_todo)

###!!!He incluido los valores que ponia David que no eran muy precisos.
p<-ggscatter(bicornis_a, x = "abundancia_relativa", y = "IT", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "abundancia_realtiva", ylab = "IT")
print(p)

p<-ggscatter(bicornis_a, x = "temp_mean", y = "IT",
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "temp_mean", ylab = "IT")
print(p)

p<-ggscatter(bicornis_f, x = "abundancia_relativa", y = "IT", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "abundancia_realtiva", ylab = "IT")
print(p)

p<-ggscatter(bicornis_f, x = "temp_mean", y = "IT",
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "temp_mean", ylab = "IT")
print(p)

p<-ggscatter(bicornis_f, x = "abundancia_relativa", y = "IT", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "abundancia_realtiva", ylab = "IT")
print(p)

p<-ggscatter(bicornis_f, x = "temp_range", y = "IT",
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "temp_range", ylab = "IT")
print(p)


p<-ggscatter(bicornis_a, x = "temp_range", y = "IT",
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "temp_range", ylab = "IT")
print(p)





































Grupo_sexo_specie_caña_orientation_osmia_bicornis<-Dataframe_total_osmias %>% group_by(Orientation) %>% filter(!is.na(Mean_total)) 

d<-ggplot(Dataframe_total_osmia_bicornis,aes(Orientation,Mean_total))
print(d)

Grupo_sexo_specie_caña<-Dataframe_total_osmias %>% group_by(Location) %>% filter(!is.na(size)) %>% summarise (number_cells = n(size))


Grupo_sexo_specie_caña<-Dataframe_total_osmias %>% group_by(Orientation) %>% filter(!is.na(size)) %>% summarise (number_cells = n(size))

d+geom_boxplot()+geom_jitter()

str(Dataframe_total_osmias)

Grupo_sexo_specie_caña<-Dataframe_total_osmias %>% group_by(number,Sex,Specie) %>% filter(!is.na(IT)) %>% summarise (mean_it = mean(IT))


Grupo_sexo_specie_caña<-Dataframe_total_osmias %>% group_by(number,Sex) %>% filter(!is.na("ID")) %>% summarise (count_ID= count(ID))




str(Sexo_specie_osmias)
str(Datos_distancia_intertegular)
str(Canes)
head(Datos_specie)
head(Datos_distancia_intertegular)
head(Sexo_specie_it)
str(Datos_specie)
str(Sexo_specie_it)
head(Canes)
str(Canes)
head(Datos_distancia_intertegular)
View(Sexo_specie_it)

#Transformar todos los caracteres como factores de manera rápida
Canes<-as.data.frame(unclass(Canes))
levels(Canes$l)



Canes$Aborts<-as.numeric(Canes$Aborts)
summary(Canes)


  Aborts_per_zone<-Canes %>% group_by(Location,Orientation) %>% filter(!is.na(Aborts)) %>% summarise (sum_aborts = sum(Aborts))
  #Esto significa que a las abejas se les da peor en estos lugares 
  Aborts_mean<-Canes %>% group_by(Location) %>% filter(!is.na(Aborts)) %>% summarise (mean_aborts = mean(Aborts)) %>%  arrange(mean_aborts)
  
  Aborts_median<-Canes %>% group_by(Location) %>% filter(!is.na(Aborts)) %>% summarise (median_aborts = median(Aborts)) %>%  arrange(median_aborts)
  
  Aborts_mean_byt<-Canes %>% group_by(Location,Orientation) %>% filter(!is.na(Aborts)) %>% summarise (mean_aborts = mean(Aborts)) %>%  arrange(mean_aborts)
  
  
  a<-ggplot(Aborts_median,aes(Location,median_aborts))
  b<-ggplot(Aborts_per_zone,aes(Orientation,sum_aborts))
  c<-ggplot(Aborts_mean_byt,aes(Orientation,mean_aborts))
  d<-ggplot(Grupo_sexo_specie_caña,aes(Orientation,mean_it))
  a+geom_point(aes(reorder(Location, median_aborts), y=median_aborts))
  b+geom_boxplot()+geom_jitter()
  c+geom_boxplot()+geom_jitter()
  d+geom_boxplot()+geom_jitter()
  

  
  Celdas_orientation<-Canes %>% group_by(Location,Orientation) %>% filter(!is.na(Aborts)) %>% summarise (sum_aborts = sum(Aborts))
  
  Celdas_orientation<-Canes %>% group_by(number,Orientation, Location) %>% filter(!is.na(size)) %>% filter(specie="Osmia bicornis") %>% count(size)
  
  Celdas_orientation2<-Celdas_orientation %>% group_by(number,Location,Orientation) %>% summarise (sum_cells=sum(n))
  
  Celdas_location<-Celdas_orientation2%>% group_by(Location,Orientation) %>% summarise (sum_cells=mean(sum_cells))
  
  Celdas_location_mean<-Celdas_orientation2%>% group_by(Location,Orientation) %>% summarise (mean_cells=mean(sum_cells))
  
  #Solo para osmia bicornis
  
  Celdas_orientation<-Canes %>% group_by(number,Orientation, Location) %>% filter(!is.na(size)) %>% count(size)
  

  Celdas_orientation2<-Celdas_orientation %>% group_by(number,Location,Orientation) %>% summarise (sum_cells=sum(n))
  
  Celdas_location<-Celdas_orientation2%>% group_by(Location,Orientation) %>% summarise (sum_cells=mean(sum_cells))
  
  Celdas_location_mean<-Celdas_orientation2%>% group_by(Location,Orientation) %>% summarise (mean_cells=mean(sum_cells))
  
  
  z_orientation<-ggplot(Celdas_location,aes(Orientation, sum_cells))
  y_orientation<-ggplot(Celdas_location_mean,aes(Orientation, mean_cells)) 
  z_orientation+geom_boxplot()+geom_jitter()
  y_orientation+geom_boxplot()+geom_jitter()
  
  
  
  
  
  
  
  z<-ggplot(Celdas_location,aes(Location, sum_cells))
  y<-ggplot(Celdas_location_mean,aes(Location, mean_cells))
  b<-ggplot(Aborts_per_zone,aes(Orientation,sum_aborts))
  c<-ggplot(Aborts_mean_byt,aes(Orientation,mean_aborts))
  d<-ggplot(Grupo_sexo_specie_caña,aes(Orientation,mean_it))
  a+geom_point(aes(reorder(Location, median_aborts), y=median_aborts))
  b+geom_boxplot()+geom_jitter()
  c+geom_boxplot()+geom_jitter()
  d+geom_boxplot()+geom_jitter()
  z+geom_boxplot(aes(reorder(Location, sum_cells), y=sum_cells))+geom_jitter()
  y+geom_boxplot(aes(reorder(Location, mean_cells), y=mean_cells))+geom_jitter()
  
  
  
  head(Canes)
  str(Canes)
  
#############                         ##########
  
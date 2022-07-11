library(tidyverse)

#view data
head()
tail()
View()
names()

glimpse()


###########################################
### 01 select , filter and mutate       ###
###########################################

View(starwars)
sw <- starwars

sw %>% 
  dplyr::select(gender, mass, height, species) %>% 
  dplyr::filter(species == "Human") %>% 
  na.omit() %>% 
  dplyr::mutate(height = height / 100) %>% 
  dplyr::mutate(IMM = mass / height^2) %>% 
  dplyr::group_by(gender) %>% 
  dplyr::summarise(median_IMM = median(IMM), mean_IMM = mean(IMM))



###########################################
###       02 types of data              ###
###########################################
glimpse(sw)

sw$gender <- as.factor(sw$gender)

glimpse(sw)

sw$mass <- as.integer(sw$mass)

glimpse(sw)


levels(sw$gender)

sw$gender <- factor(sw$gender, 
                    levels = c("masculine", "feminine"))

glimpse(sw)
levels(sw$gender)

sw$log <- sw$mass > 200
class(sw$log)
glimpse(sw)


################################################
### 03 rename variables, and reorder columns ###
################################################

sw <- starwars%>% 
  dplyr::select(name, mass, height) %>% 
  dplyr::rename(nombre = name, masa = mass, altura = height)

################################################
###         04 create a new varible          ###
################################################

sw <- starwars %>% 
  dplyr::select(name, height, mass, gender) %>% 
  dplyr::rename(nombre =  name, altura = height, masa = mass, genero = gender) %>% 
  dplyr::select(nombre, genero, altura, masa)%>% 
  na.omit() %>% 
  dplyr::mutate(altura = round(altura / 100, 2), masa = round(as.double(masa), 2)) %>% 
  dplyr::filter(genero %in% c("masculine", "feminine")) %>% 
  dplyr::mutate(genero = recode(genero,
                                 masculine = "H",
                                 feminine  = "M")) %>% 
  dplyr::mutate(size = altura > 1 & masa > 75) %>% 
  dplyr::mutate(size = if_else(size == TRUE, "Grande", "Pequeño")) %>% 
  dplyr::filter(size == "Pequeño")


################################################
###         05 filter and subset             ###
################################################
view(msleep)
d <- msleep %>% 
  dplyr::select(name, sleep_total) %>% 
  dplyr::rename(nombre = name, dormir_total  =sleep_total) %>% 
  dplyr::filter( !dormir_total > 18)

d1 <- msleep %>% 
  dplyr::select(name, order, bodywt, sleep_total) %>% 
  dplyr::rename(nombre = name, especie = order, masa = bodywt, dormir_total = sleep_total) %>%
  dplyr::mutate(masa = round(as.double(masa),0) * 1, dormir_total = round(as.double(dormir_total),0)*1 ) %>% 
  dplyr::filter(especie == "Primates" | masa > 20)

d2 <- msleep %>% 
  dplyr::select(name, sleep_total) %>% 
  dplyr::rename(nombre = name, dormir_total = sleep_total) %>%
  dplyr::mutate(dormir_total = round(as.double(dormir_total),1)*1 ) %>% 
  dplyr::filter(nombre %in% c("Cow", "Dog", "Goat"))

d4 <- msleep %>% 
  dplyr::select(name, sleep_total) %>% 
  dplyr::rename(nombre = name, dormir_total = sleep_total) %>%
  dplyr::mutate(dormir_total = round(as.double(dormir_total),1)*1 ) %>% 
  dplyr::filter(between(dormir_total, 16, 18))


d5 <- msleep %>% 
  dplyr::select(name, sleep_total) %>% 
  dplyr::rename(nombre = name, dormir_total = sleep_total) %>%
  dplyr::mutate(dormir_total = round(as.double(dormir_total),1)*1 ) %>% 
  dplyr::filter(near(dormir_total, 17, tol  = 0.5))

d6 <- msleep %>% 
  dplyr::select(name, conservation, sleep_total) %>% 
  dplyr::rename(nombre = name, conservacion = conservation, dormir_total = sleep_total) %>% 
  dplyr::filter(is.na(conservacion))

d7 <- msleep %>% 
  dplyr::select(name, conservation, sleep_total) %>% 
  dplyr::rename(nombre = name, conservacion = conservation, dormir_total = sleep_total) %>% 
  dplyr::filter(!is.na(conservacion))


################################################
###         06 function and objects in R     ###
################################################
mi_edad <- 45
tu_edad <- 34
sum(mi_edad, - tu_edad)
view(cars)
plot(cars)
hist(cars$speed)
attach(cars)
hist(dist)
summary(cars)
summary(cars$speed)
summary(dist)
class(speed)  
length(dist)
unique(speed)
head(cars)
tail(cars)
subset <- cars[3:6 , 1:2]
?median
median(dist)
new_data <- c(2,3,4,56,65, NA, 43,34,23, NA)
median(new_data, na.rm = TRUE)

################################################
###         07 explore our data using R      ###
################################################

?starwars
dim(starwars)
str(starwars)
glimpse(starwars)
head(starwars)
tail(starwars)
attach(starwars)
names(starwars)
length(starwars)

class(hair_color)
length(hair_color)
unique(hair_color)

table(hair_color)
sort(table(hair_color), decreasing = TRUE)
sort(table(hair_color), decreasing = FALSE)
View(sort(table(hair_color), decreasing = TRUE))
barplot(sort(table(hair_color), decreasing = TRUE))

starwars %>% 
  dplyr::select(hair_color) %>% 
  dplyr::count(hair_color) %>% 
  dplyr::arrange(desc(n)) %>% 
  view() 

length(hair_color)

starwars[is.na(hair_color),]

view(starwars[is.na(hair_color),])

is.na(hair_color)

class(height)
length(height)
summary(height)
boxplot(height)
hist(height)

################################################
###           08 clean our data using R      ###
################################################

#VARIABLES TYPES
view(starwars)
glimpse(starwars)
class(starwars$gender)
unique(starwars$gender)

starwars$gender <- as.factor(starwars$gender)
class(starwars$gender)
levels(starwars$gender)

attach(starwars)
 gender <- factor(gender,
                  levels = c("masculine", "feminine"))
levels(gender) 
glimpse(starwars)

#SELECT VARIABLES 
starwars %>% 
  dplyr::select(name, height, ends_with("color")) %>% 
  names()

#FILTER OBSERVATIONS
unique(hair_color)
starwars %>%
  dplyr::select(name, height, ends_with("color")) %>% 
  dplyr::mutate(height = height /100) %>% 
  dplyr::filter(hair_color %in% c("blond", "brown") & height < 1.80 ) %>% 
  view()
 
#MISSING DATA
round(mean(height, na.rm = TRUE),2)

starwars %>% 
  dplyr::select(name, gender, hair_color, height)
#87 x 4

starwars %>% 
  dplyr::select(name, gender, hair_color, height) %>% 
  na.omit()
#73 x 4

starwars %>% 
  dplyr::select(name, gender, hair_color, height) %>% 
  dplyr::filter(complete.cases(.))
#73 x 4

starwars %>% 
  dplyr::select(name, gender, hair_color, height) %>% 
  dplyr::filter(!complete.cases(.))

starwars %>% 
  dplyr::select(name, gender, hair_color, height) %>% 
  dplyr::filter(!complete.cases(.)) %>% 
  drop_na(height)

starwars %>% 
  dplyr::select(name, gender, hair_color, height) %>% 
  dplyr::filter(!complete.cases(.)) %>%
  dplyr::mutate(hair_color =  replace_na(hair_color, "Metal"), 
                height = replace_na(height, 0)) %>% 
  dplyr::mutate(gender = recode(gender,
                                masculine = "H",
                                feminine  = "M",), 
                gender = replace_na(as.character(gender), "TODES"),
                gender = as.factor(gender))

#DUPLICATE 
name <- c("peter", "jhon", "andrew", "peter")
edad <- c(22,33,44,22)
friends <- data.frame(name, edad)
duplicated(friends)

friends[!duplicated(friends),]

friends %>% 
  distinct()

#RECORDING VARIABLES 
starwars %>% 
  dplyr::select(name, gender)

starwars %>% 
  dplyr::select(name, gender) %>% 
  dplyr::mutate(gender = recode(gender,
                                "masculine" = 1,
                                "feminine" = 0))

starwars %>% 
  dplyr::select(name, gender) %>% 
  dplyr::mutate(nro_gender = recode(gender,
                                "masculine" = 1,
                                "feminine" = 0)) %>% 
  dplyr::mutate(nro_gender = as.numeric(nro_gender))

################################################
###     09 manipulate your data using R      ###
################################################

?msleep
view(msleep)
glimpse(msleep)
attach(msleep)

#rename variable
msleep %>%
  dplyr::rename(consevacion = conservation)

#reorden variables 
msleep %>% 
  dplyr::select(genus, name, vore, everything())

#change a variable type
class(vore)
unique(vore)
msleep$vore <- as.factor(msleep$vore)
glimpse(msleep)


msleep %>% 
  dplyr::mutate(vore = as.character(vore)) %>% 
  glimpse()

#select variable to work with

names(msleep)

msleep %>% 
  select(2:4,
         awake,
         starts_with("sleep"),
         contains("wt")) %>% 
  names()

#filter and arrange data
unique(order)

msleep %>% 
  dplyr::filter(order %in% c("Carnivora", "Rodentia") & sleep_total <8) %>% 
  dplyr::select(name, order, sleep_total) %>% 
  dplyr::arrange(-sleep_total) %>% 
  view()
#variable numerica


msleep %>% 
  dplyr::filter(order %in% c("Carnivora", "Rodentia") & sleep_total <8) %>% 
  dplyr::select(name, order, sleep_total) %>% 
  dplyr::arrange(order) %>% 
  view()
#varible cualitativa


#change observations (mutate)

msleep %>% 
  dplyr::select(name, order, brainwt) %>% 
  dplyr::mutate(brainwt = brainwt * 1000) %>% 
  view()

msleep %>% 
  dplyr::select(name, order, brainwt) %>% 
  dplyr::mutate(brainwt_1000 = brainwt * 1000) %>% 
  view()

#conditional changes (if_else)
##logical vector based on a condition

brainwt
brainwt > 0.01

size_brain <- msleep %>% 
  dplyr::select(name, brainwt) %>% 
  drop_na(brainwt) %>% 
  dplyr::mutate(brain_size = if_else(brainwt > 0.01,
                "large",
                "small")) %>% 
  view()

#recode data and rename a varible
##change observations of "large" and "small" into

size_brain <- msleep %>% 
  dplyr::select(name, brainwt) %>% 
  drop_na(brainwt) %>% 
  dplyr::mutate(brain_size = if_else(brainwt > 0.01,
                                     "large",
                                     "small")) %>% 
  dplyr::mutate(brain_size_num = recode(brain_size,
                                        "large" = 1,
                                        "small" = 0)) %>% 
  view()


#reshape the data from wide to long or long to wide
library(gapminder)
view(gapminder)
data <- dplyr::select(gapminder, country, year, lifeExp)
view(data)

width_data <- data %>% 
  pivot_wider(names_from = year, values_from = lifeExp) %>% 
  view()

long_data <- width_data %>% 
  pivot_longer(2:13,
               names_to = "year",
               values_to = "lifeExp") %>% 
  view()


################################################
###     10 summarise your data using R       ###
################################################
glimpse(msleep)

#describe the spread, centrality and variance or your data
min(awake)
max(awake)
range(awake)
IQR(awake)
mean(awake)
median(awake)
var(awake)
?IQR

#summary selected variables
summary(msleep)
summary(sleep_total)


msleep %>% 
 dplyr::select(sleep_total, brainwt) %>% 
 summary

msleep %>% 
  drop_na(vore) %>% 
  dplyr::group_by(vore) %>% 
  dplyr::summarise(minimo = min(sleep_total),
                   media = mean(sleep_total),
                   maximo = max(sleep_total),
                   rango = max(sleep_total) - min(sleep_total),
                   desvStd = var(sleep_total)) %>% 
  dplyr::arrange(minimo) %>% 
  view()

#creating contingency table 
library(MASS)
attach(Cars93)
glimpse(Cars93)
autos <- Cars93

table(Origin)
table(AirBags, Origin)
addmargins(table(AirBags, Origin))
addmargins(table(AirBags, Origin),1)

table(AirBags, Origin)
prop.table(table(AirBags, Origin),2)*100 #porcentaje en columna o en linea 1 o 2
round(prop.table(table(AirBags, Origin),2)*100, 2)

autos %>% 
  dplyr::group_by(Origin, AirBags) %>% 
  dplyr::summarise(number = n()) %>% 
  pivot_wider(names_from = Origin,
              values_from = number) %>% 
  view

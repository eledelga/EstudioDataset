pacman::p_load(pacman,tm,SnowballC,tidyverse)
estudiopoblacion <- read_tsv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT_mx6N6e4RZ8b-19FtH7lbKjTNbKr0h5kIPWn2UeLOQK5CDkVhydJ-EOIDIIFSzgl4_neCISN64ppe/pub?output=tsv")


# Elena Delgado del Rey ---------------------------------------------------

names(estudiopoblacion)
names (estudiopoblacion) = c("Year", "Poblacion", "Nacimientos", "Muertes", "Cambio natural", "Tasa cruda de natalidad/1000",
                           "Tasa cruda de mortalidad/1000", "Cambio natural/1000", "Tasa total de fertilidad")
names(estudiopoblacion)


# Gráficos ----------------------------------------------------------------

summary(estudiopoblacion)

histograma1<-hist(x = estudiopoblacion$`Tasa total de fertilidad`, main = "Histograma de la tasa de fertilidad", 
     xlab = "Tasa de fertilidad", ylab = "Frecuencia",col = "pink")


histograma2<-hist(x = estudiopoblacion$`Tasa cruda de natalidad/1000` ,main = "Histograma de tasa de natalidad", 
     xlab = "Tasa de natalidad", ylab = "Frecuencia",col = "blue")

histograma3<-hist(x = estudiopoblacion$`Tasa cruda de mortalidad/1000` ,main = "Histograma de tasa de mortalidad", 
                  xlab = "Tasa de mortalidad", ylab = "Frecuencia",col = "purple")

estudiopoblacion$Poblacion <- as.factor(estudiopoblacion$Poblacion)

plot(x = estudiopoblacion$Poblacion, y = estudiopoblacion$Year ,main = "Poblacion conforme pasan los years", 
     xlab = "Poblacion", ylab = "Year",col = "blue")

plot(x = estudiopoblacion$`Tasa total de fertilidad`, y = estudiopoblacion$Poblacion ,main = "Fertilidadvs. poblacion", 
     xlab = "fertilidad", ylab = "Poblacion",col = "pink")

plot(x = estudiopoblacion$`Tasa cruda de mortalidad/1000`, y = estudiopoblacion$Poblacion ,main = "Mortalidad vs. poblacion", 
     xlab = "mortalidad", ylab = "Poblacion",col = "blue")

plot(x = estudiopoblacion$`Tasa cruda de natalidad/1000`, y = estudiopoblacion$Poblacion ,main = "Natalidad vs. poblacion", 
     xlab = "natalidad", ylab = "Poblacion",col = "limegreen")


# Segundo dataset  --------------------------------------------------------


poblacionprovincia <- read_tsv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTJ9uVEDCjv8fGN9DXG-gS8K0C3YSPYcNEkVVo_clX5zSP4X6iFgfthBFwl1oMdMUYjSJKEnIXmuxWm/pub?gid=0&single=true&output=tsv")

names(poblacionprovincia)
names (poblacionprovincia) = c("Posicion", "Provincia", "ComunidadAutonoma", "Edadmedia", "Hombres", "Mujeres")

names(poblacionprovincia)

summary(poblacionprovincia)

# Gráficos ----------------------------------------------------------------

ggplot(poblacionprovincia, aes(x=Edadmedia))+geom_bar()

poblacionprovincia %>% filter(poblacionprovincia[6] == max(poblacionprovincia[6]))

poblacionprovincia %>% filter(poblacionprovincia[5] == max(poblacionprovincia[5]))

poblacionprovincia %>% filter(poblacionprovincia[4] == max(poblacionprovincia[4]))

poblacionprovincia %>% filter(poblacionprovincia[6] == min(poblacionprovincia[6]))

poblacionprovincia %>% filter(poblacionprovincia[5] == min(poblacionprovincia[5]))

poblacionprovincia %>% filter(poblacionprovincia[4] == min(poblacionprovincia[4]))

# Estudioedades -----------------------------------------------------------

edad <- read_tsv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTJ9uVEDCjv8fGN9DXG-gS8K0C3YSPYcNEkVVo_clX5zSP4X6iFgfthBFwl1oMdMUYjSJKEnIXmuxWm/pub?gid=1831686064&single=true&output=tsv")

edad$'2017' <- as.factor(edad$'2017')
plot(x = edad$Edad , y = edad$'2017',main = "Edades en el 2017", 
     xlab = "Edad", ylab = "Year",col = "blue")

edad$'2018' <- as.factor(edad$'2018')
plot(x = edad$Edad , y = edad$'2018',main = "Edades en el 2018", 
     xlab = "Edad", ylab = "Year",col = "pink")


# Cuarto dataset ----------------------------------------------------------


estudios <- read.csv("https://www.donostia.eus/datosabiertos/recursos/poblacion-nivel-estudios/poblacionpornivelestudios.csv")

names(estudios)


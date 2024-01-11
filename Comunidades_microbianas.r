# Cargar datos ####
# Sitio A
sitioA.1 <- read.delim("CIBCM_100.tsv", header=FALSE)
sitioA.2 <- read.delim("CIBCM_103.tsv", header=FALSE)

# Sitio B
sitioB.1 <- read.delim("VP-BT.tsv", header=FALSE)

# Sitio C
sitioC.1 <- read.delim("VP-FC.tsv", header=FALSE)

# Sitio D
sitioD.1 <- read.delim("VP-TI-1.tsv", header=FALSE)

# Sitio E
sitioE.1 <- read.delim("VP-TO-1.tsv", header=FALSE)
sitioE.2 <- read.delim("VP-TO-2.tsv", header=FALSE)
sitioE.3 <- read.delim("VP-TO-4.tsv", header=FALSE)

# Modificar DataFrames por aparte ####
# Agregar columnas de sitio y muestra

# Sitio A
sitioA.1$sitio <- factor("Volcán Miravalles")
sitioA.2$sitio <- factor("Volcán Miravalles")
sitioA.1$muestra <- factor("Hornillas")
sitioA.2$muestra <- factor("Rio Perdido")

# Sitio B
sitioB.1$sitio <- factor("Volcán Platanar")
sitioB.1$muestra <- factor("La Palmera 40.6")

# Sitio C
sitioC.1$sitio <- factor("Volcán Platanar")
sitioC.1$muestra <- factor("La Palmera 43.3")

# Sitio D
sitioD.1$sitio <- factor("Volcán Platanar")
sitioD.1$muestra <- factor("Río San Rafael 48.8")

# Sitio E
sitioE.1$sitio <- factor("Volcán Platanar")
sitioE.2$sitio <- factor("Volcán Platanar")
sitioE.3$sitio <- factor("Volcán Platanar")

sitioE.1$muestra <- factor("Río San Rafael 56.7")
sitioE.2$muestra <- factor("Río San Rafael 36.9")
sitioE.3$muestra <- factor("Río San Rafael 64.2")

# En las muestras hay algunos datos de virus y animales, estos no son de nuestro interes. 
# Dado la forma del set de datos, no nos queda otra que borrarlos "manualmente"
library(dplyr)
sitioA.1 <- sitioA.1 |>
                  slice(-(901:930))

sitioA.2 <- sitioA.2 |>
                  slice(-(1001:1038))

sitioB.1 <- sitioB.1 |>
            slice(-c(1142:1171, 1187:1204))

sitioC.1 <- sitioC.1 |>
            slice(-c(1289:1341))

sitioD.1 <- sitioD.1 |>
            slice(-c(951:980, 1017:1026))

sitioE.1 <- sitioE.1 |>
            slice(-c(1072:1101, 1143:1156))

sitioE.2 <- sitioE.2 |>
            slice(-c(1522:1558))

sitioE.3 <- sitioE.3 |>
            slice(-c(884:926))


# Unión y modificación del DataFrame ####

# Unir por filas 
diversidadCrudos <- dplyr::bind_rows(sitioA.1, sitioA.2,
                                     sitioB.1, sitioC.1, sitioD.1,
                                     sitioE.1, sitioE.2, sitioE.3)
# Cantidad de filas
nrow(diversidadCrudos)

# Cambiar nombre de columnas
colnames(diversidadCrudos) <- c("porcentaje_fragmentos_raiz",
                                "cantidad_fragmentos_raiz",
                                "fragmentos_asignados", 
                                "codigo_rango",
                                "NCBI_id",
                                "nombre_cientifico",
                                "sitio", 
                                "muestra")
# NCBI_id debe ser un factor o un string 
diversidadCrudos$NCBI_id <- as.character(diversidadCrudos$NCBI_id)

# Estructura de los datos
summary(diversidadCrudos)
head(diversidadCrudos, n= 10)


# Porcentaje de fragmentos clasificados por sitio y muestra 
fragmentosClasificados  <- diversidadCrudos |>
                        filter(nombre_cientifico == "root") |>
                        select(sitio, muestra, porcentaje_fragmentos_raiz, cantidad_fragmentos_raiz)


# Podemos ver que los datos de "nombre-cientifico" tienen espacios en blancos al comienzo, los vamos a borrar 
diversidadCrudos$nombre_cientifico <- stringr::str_trim(diversidadCrudos$nombre_cientifico)

# Hacemos una tabla bonita con los fragmentos clasificados 
colnames(fragmentosClasificados) <- c("Sitio    ", "Muestra",
                                      "Porcentaje de fragmentos (%)",
                                      "Cantidad de fragmentos")

formattable::formattable(fragmentosClasificados, 
                          align =c("c","c","c","c"),
                          list(`Indicator Name` = formattable::formatter(
                            "span", style = ~ style(color = "grey",font.weight = "bold")) 
                          ))


tablaDiversidadCrudos <- diversidadCrudos |>
                              group_by(sitio, muestra) |>
                              summarise(
                                filos = n_distinct(ifelse(codigo_rango == "P", nombre_cientifico, NA), na.rm = T),
                                clases = n_distinct(ifelse(codigo_rango == "C", nombre_cientifico, NA), na.rm = T),
                                ordenes = n_distinct(ifelse(codigo_rango == "O", nombre_cientifico, NA), na.rm = T),
                                familias = n_distinct(ifelse(codigo_rango == "F", nombre_cientifico, NA), na.rm = T), 
                                generos = n_distinct(ifelse(codigo_rango == "G", nombre_cientifico, NA), na.rm = T),
                                especies = n_distinct(ifelse(codigo_rango == "S", nombre_cientifico, NA), na.rm = T)
                              ) 

names(tablaDiversidadCrudos) <- c("Sitio", "Muestra", "Filo",
                                  "Clase", "Orden", "Familia", 
                                  "Género", "Especie")

# Tabla antes de filtrar por grupos con cantidad de fragmentos >= 25
formattable::formattable(tablaDiversidadCrudos, 
                         align =c("l", "l", rep("c", 6) ),
                         list(`Indicator Name` = formattable::formatter(
                              "span", style = ~ style(color = "grey",font.weight = "bold")) 
                          ))

# Vamos a filtar considerando cantidad de fragmentos >= 25
datosFiltradros <- diversidadCrudos |>
                      filter(fragmentos_asignados >= 25)

nrow(datosFiltradros) #Nos quedan 1098 filas

# Cantidad de individuos por grupo despues de filtrar ####
tablaDiversidadFiltrados <- datosFiltradros |>
                                group_by(sitio, muestra) |>
                                summarise(
                                  filos = n_distinct(ifelse(codigo_rango == "P", nombre_cientifico, NA), na.rm = T),
                                  clases = n_distinct(ifelse(codigo_rango == "C", nombre_cientifico, NA), na.rm = T),
                                  ordenes = n_distinct(ifelse(codigo_rango == "O", nombre_cientifico, NA), na.rm = T),
                                  familias = n_distinct(ifelse(codigo_rango == "F", nombre_cientifico, NA), na.rm = T), 
                                  generos = n_distinct(ifelse(codigo_rango == "G", nombre_cientifico, NA), na.rm = T),
                                  especies = n_distinct(ifelse(codigo_rango == "S", nombre_cientifico, NA), na.rm = T)
                                ) 


names(tablaDiversidadFiltrados) <- c("Sitio", "Muestra", "Filo",
                                    "Clase", "Orden", "Familia", 
                                    "Género", "Especie")

formattable::formattable(tablaDiversidadFiltrados, 
                         align =c("l", "l", rep("c", 6) ),
                         list(`Indicator Name` = formattable::formatter(
                           "span", style = ~ style(color = "grey",font.weight = "bold")) 
                         ))


library("vegan") #Librería para análisis de ecología 

# Filtrar por especie 
especies <- datosFiltradros |>
            filter(codigo_rango == "S")
# Preparar el la matriz con la que trabaja librería Vegan
diversidadAncho <- especies |> 
                      select(sitio, muestra, fragmentos_asignados, nombre_cientifico) |>
                      tidyr::pivot_wider(names_from = nombre_cientifico, 
                                  values_from = fragmentos_asignados,
                                  values_fill = 0
                                  )
anchoMuestras <- diversidadAncho |> select(-sitio)


#anchoMuestras$muestra <- as.factor(anchoMuestras$muestra)

#evels(anchoMuestras$muestra)

names(anchoMuestras) <- gsub(" ", "_", names(anchoMuestras))

#specnumber(anchoMuestras)
#specnumber(especies$nombre_cientifico, groups = especies$muestra)


# Anova // No tiene mucho sentido 
anova1 <- aov(fragmentos_asignados ~ muestra, data = especies)
summary(anova1)


# Diversidad alfa #### 
#https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf

#Shanon
shanonDiv <- diversity(anchoMuestras[,2:ncol(anchoMuestras)], index = "shannon", 
                groups = anchoMuestras$muestra)


#Simpson
simpsoDiv <- diversity(anchoMuestras[,2:ncol(anchoMuestras)], index = "simpson", 
              groups = anchoMuestras$muestra)

#invsimpson
invsimpso <- diversity(anchoMuestras[,2:ncol(anchoMuestras)], index = "invsimpson", 
              groups = anchoMuestras$muestra)





#Pielou's evenness
shanonDiv/log(specnumber(anchoMuestras[,2:ncol(anchoMuestras)]))

anchoMuestras2 <- anchoMuestras[,2:ncol(anchoMuestras)]

row.names(anchoMuestras2) <- c("Hornillas", "Rio Perdido", "La Palmera 40.6", 
                               "La Palmera 43.3", "Rio San Rafael 48.8", 
                               "Rio San Rafael 56.7", "Rio San Rafael 36.9",
                               "Rio San Rafael 64.2")

#Riqueza absoluta de especies
especies |>
  group_by(muestra) |> summarise(especies = n_distinct(nombre_cientifico))


# Rarefacción ####
# Esto es para ver riquezas
rarefy(anchoMuestras2, 
       min(rowSums(anchoMuestras[,2:ncol(anchoMuestras)]))) 


# Diversidad beta ####
# Sørensen index

# Entre muestras // No tiene mucho
betaMuestras <- vegdist(anchoMuestras[,2:ncol(anchoMuestras)], binary=TRUE)
mean(betaMuestras)

# Entre sitios
miravalles <- diversidadAncho |> filter(sitio == "Volcán Miravalles")
platanar <- diversidadAncho |> filter(sitio == "Volcán Platanar")

# Beta Miravalles 
betaMiravalles <- vegdist(miravalles[,3:ncol(miravalles)], binary=TRUE)
mean(betaMiravalles)

# Beta platanar
betaPlatanar <- vegdist(platanar[,3:ncol(platanar)], binary=TRUE)
mean(betaPlatanar) #tener cuidado porque son diferentes tamaños de muestras


#specpool(anchoMuestras[,2:ncol(anchoMuestras)])


# NMDS ####
NMDS <- metaMDS(anchoMuestras2, 
             distance="bray",
             k=2,
             try = 1000) # Numero de dimensiones que queremos, en este caso 2

NMDS$stress #Mejor stress alcanzado 

stressplot(object = NMDS, lwd = 5) # ver interpretación en  


# Gráfico 
diferencias2Dimensiones <- data.frame(NMDS$points)
diferencias2Dimensiones$Sitio <- diversidadAncho$sitio
diferencias2Dimensiones$Muestra <- diversidadAncho$muestra



# Tema
tema <- theme(axis.title.x = element_text(vjust = -0.6, size = 15, face = "bold"),
                axis.title.y = element_text(vjust = + 2.5, size = 15, face = "bold"),
                axis.text.x = element_text(size = 8),
                axis.text.y = element_text(size = 8),
                # Hide panel borders and remove grid lines
                # panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Remove panel background
                panel.background = element_blank(),
                #
                #axis.line = element_line(colour = "black", 
                #                        size = 0.1, linetype = "solid"),
                panel.border = element_rect(color = "black",
                                            fill = NA,
                                            size =0.2))

library(ggalt)
library(ggrepel)
ggplot(diferencias2Dimensiones, 
     aes(x = MDS1, y = MDS2)) +
     geom_point() +
  geom_label_repel(aes(label = Muestra),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_encircle(data = diferencias2Dimensiones,
               aes(fill = Sitio,
                   colour = Sitio),
               alpha = 0.2, s_shape = 1, expand=0.01) +
     tema



# Permanova 
adonis2(formula = anchoMuestras2 ~ diversidadAncho$sitio, method = "bray", permutations = 10000)
adonis2(formula = anchoMuestras2 ~ diversidadAncho$sitio)
#No hay diferencias entre volcanes 
#No tiene validez por el tamaño de muestra


# Datos familias

familias <-datosFiltradros |> 
           filter(codigo_rango == "F")

View(familias |> 
      group_by(nombre_cientifico, muestra) |>
      summarise(sum(fragmentos_asignados))
)  #Abundancias relativas

library(RColorBrewer)
colorRampPalette(21)

paleta = colorRampPalette(brewer.pal(12, "Paired"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

# Gráfico 3 especies más frecuente 
espMasFrecuentes <- especies |>
                      group_by(muestra) |>
                      arrange(desc(fragmentos_asignados)) |>        # Ordenar el dataframe de forma descendente por la columna x
                        slice_head(n = 3)   
espMasFrecuentes


ggplot(data = espMasFrecuentes) +
  geom_bar(aes(x = muestra, y = fragmentos_asignados, fill = nombre_cientifico),
           stat = "identity") +
  scale_y_continuous(trans='sqrt') +
  scale_fill_manual("Nombre Científico", values = paleta(20)) + 
  labs(x = "Muestra", y = "Raíz Cuadrada de los Fragmentos") + 
  tema
  

familiaRelativos <- datosFiltradros |>
                    filter(codigo_rango == "F") |>
                    group_by(muestra) |>
                    mutate(porcentaje = fragmentos_asignados / sum(fragmentos_asignados))

ggplot(data = familiaRelativos) +
  geom_bar(aes(x = muestra, y = porcentaje, fill = nombre_cientifico),
           stat = "identity") +
  scale_fill_manual("Familia", values = paleta(33)) + 
  labs(x = "Muestra", y = "Abundancia Relativa") + 
  tema +
  theme(axis.text.y = element_text(angle = 45, hjust = 1))

                           
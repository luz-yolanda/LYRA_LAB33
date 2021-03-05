#Hecho con gusto por Luz Yolanda Rivera Álvarez. UAEH

#CARACTERISTICAS DE LOS AUTOS
#Base de datos del propio Rstudio
?mtcars
head(mtcars)
#identificar qué tipos de datos se tienen
class(mtcars)

#vemos que mtcars es un data.frame, por lo tanto lo convertimos en matriz para poder hacer un mapa de calor
#llamada mtcars_matrix
mtcars_matrix <- data.matrix(mtcars)
write.csv(mtcars_matrix,file = "mtcars_matrix.csv")
#verificamos que la conversión a matriz se realizó, preguntando con class, qué tipo de datos son.

class(mtcars_matrix)

#hacer el mapa de calor con la función heatmap.
heatmap(mtcars_matrix)

#?Se parece a lo que esperabas?
# Mire la página de ayuda de la función y lea la descripción del scale argumento de heatmap

?heatmap


#La escala es importante: los valores deben centrarse y escalarse en filas o columnas. 
#En nuestro caso, queremos visualizar altibajos en cada variable, que está en columnas.

heatmap(mtcars_matrix, scale = "colum")

# crear nuestra propia paleta-gama de colores o coloreta, del color más ligero al más intenso
colores_blue<- colorRampPalette(c ("lightblue", "cornflowerblue", "navyblue" )) (256)

#La misma instrucción ahora le indico que los colores col, serán los de la gama de colores que cree blue 
#vamos como Maserati Bora tiene máyor valor en caballos de fuerza que las otras marcas de coches
#con AMBOS denogramas por default, se ordenan las variables por cluster de pertenencia
heatmap(mtcars_matrix, 
        scale = "colum",
        col= colores_blue)

#Eliminar dendrogramas
#El dendrograma de columna realmente no tiene sentido para este conjunto de datos, es el horizontal
#no nos interesa el dendograma de las características, no interesa el de los tipos de coche Rowv
#y Colvse puede configurar para NAeliminar dendrogramas, lo que también significa que los datos 
#no se reorganizarn de acuerdo con el método de agrupación.
#NOS INTERESA COMO SE CLUSTEAN LOS TIPOS DE COCHE, NO LAS CARACTERÍSTICAS DE LOS COCHES POR QUE LOS DATOS NO ESTAN NORMALIZADOS

heatmap(mtcars_matrix, 
        scale = "colum",
        col= colores_blue,
        Colv = NA ,
        margins = c(5 , 10),
        xlab = "especificación de características",
        ylab = "modelos de autos",
        main = "Mapa de Calor")

#puedo checar los nombres de las columnas de mi matriz y veo que no hay cambios.
# se respeta el orden de las columnas, porque se borr? el dendrograma de las columnas, ya no se ordena por cluster
# no se respeta el orden de los renglones, porque estos si tienen clusteo
colnames(mtcars_matrix)


#image(1:6,1,as.matrix(1:6), col = rainbow (6), xlab="Leyenda", ylab="", xaxt="n", yaxt="n", bty="n")

#paletas por defecto
# rainbow, heat.colors, terrain.colors, topo.colors, cm.colors
#llamamos a librería a viridis
library(viridis)

#cambiamos de color a "viridis"
heatmap(mtcars_matrix, 
        scale = "colum",
        col= viridis_pal(option = "viridis") (6) ,
        Colv = NA ,
        margins = c(5 , 10),
        xlab = "especificación de características",
        ylab = "modelos de autos",
        main = "Mapa de Calor")

#cambiamos ahora a magma
heatmap(mtcars_matrix, 
        scale = "colum",
        col= viridis_pal(option = "magma") (6) ,
        Colv = NA ,
        margins = c(5 , 10),
        xlab = "especificación de características",
        ylab = "modelos de autos",
        main = "Mapa de Calor")
#cambiamos los colores a las opciones que tiene sin viridis
#col = viridis_pal(option = "viridis") (6))
# viridis, magma, plasma, cividis, inferno
heatmap(mtcars_matrix, 
        scale = "colum",
        col= rainbow (6) ,
        Colv = NA ,
        margins = c(5 , 10),
        xlab = "especificación de características",
        ylab = "modelos de autos",
        main = "Mapa de Calor")

#para poder distinguir los valores altos de los valores bajos
# nos damos cuenta que el valor más alto es el rojo y el más alto es el rosa
image(1:6,1,as.matrix(1:6), col = rainbow (6), xlab="Leyenda", ylab="", xaxt="n", yaxt="n", bty="n")

write.csv(c,file = "mtcars_matrix.csv")

##################################################################################################

datos <- mtcars

# Para que las variables sean comparables bajo un mismo esquema de colores se
# estandarizan.
datos <- scale(datos)

#graficar el denograma llamado modelo
modelo<- hclust(dist(datos), method = "ward.D2")
plot(modelo)


# Calcular matriz de distancias
m.distancia <- get_dist(datos, method = "euclidean")

rect.hclust(modelo,k=4, border= "blue")

heatmap(x = datos, scale = "none",
        distfun = function(x){dist(x, method = "euclidean")},
        hclustfun = function(x){hclust(x, method = "average")},
        cexRow = 0.7)

colores1 <- colorRampPalette(c("red", "white", "blue"))(256)

heatmap(x = datos, scale = "none", col = colores_blue, cexRow = 0.7)

colores_blue <- colorRampPalette(c("lightblue", "cornflowerblue", "navyblue"))(256)

# Paleta de color viridis
library(viridis)
colores2 <- viridis(256)
heatmap(x = datos, scale = "none", col = colores2,
        distfun = function(x){dist(x, method = "euclidean")},
        hclustfun = function(x){hclust(x, method = "average")},
        cexRow = 0.7)

#Es posible añadir información adicional (annotate) en las filas o columnas con los argumentos RowSideColors y ColSideColors. Por ejemplo, supóngase que los primeros 16 coches proceden de China y los 16 últimos de América.
# Se codifica con color naranja a los coches procedentes de China y con morado a
# los de América
colores2 <- viridis(256)
heatmap(x = datos, scale = "none", col = colores2,
        distfun = function(x){dist(x, method = "euclidean")},
        hclustfun = function(x){hclust(x, method = "average")},
        RowSideColors = rep(c("orange", "purple"), each = 16))






# read the csv
df = read.csv("MarketingDirecto.csv")

# get the summary 
summary(df)

# get the df structure, columns and data types
str(df)

# Ordenamiento de niveles de variables categoricas ordinales
df$Edad <- factor(df$Edad, levels = c("Joven", "Media", "Adulta"), ordered=T)
df$Historial <- factor(df$Historial, levels = c("Bajo", "Medio", "Alto"),ordered=T)
str(df)

########################################################################################################################
#  a) Presentar y visualizar la distribución del historial de compra del cliente. Analizar
########################################################################################################################

barplot(table(df$Historial), main="Distribución del historial de compra del cliente", 
        xlab="", col = 35:75,
        ylab="Frecuencia")

#install.packages("tidyverse")
library(tidyverse) 

fi <- prop.table(table(df$Historial))
df2 <- as.data.frame(fi)
# Barras simples
pie <- ggplot(df2, aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(stat="identity", width=1)
# Conversión a "pie" (coordenadas polares)
pie <- pie + coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Freq*100), "%")),
            position = position_stack(vjust = 0.5))
# Cambio de escala de colores
pie <- pie + scale_fill_manual(values=c("#61d04e", "#2297e6", "#29e2e5"))
# Remover y añadir etiquetas/título
pie <- pie + labs(x = NULL, y = "", fill = "",
                  title = "")
# Limpiar el formato
pie <- pie + theme_classic() + theme(axis.line = element_blank(),
                                     axis.text = element_blank(),
                                     axis.ticks = element_blank(),
                                     plot.title = element_text(hjust = 0.5, 
                                                               color = "#666666")) 
pie
########################################################################################################################
# b) Analizar y visualizar si existe asociación entre el historial de compra y el género del cliente
########################################################################################################################

table_b = table(df$Genero, df$Historial)

barplot(t(table_b), col=35:37, beside = TRUE,
        xlab="Genero",
        ylab="Frecuencia",
        main="Distribucion del historial de compra segun Genero")
legend("topright",legend=levels(df$Historial),col=35:75, pch=15,title="Historial de Compra")

########################################################################################################################
# c) Presentar y visualizar la distribución del número de catálogos enviados. Analizar los resultados
########################################################################################################################

barplot(table(df$Catalogos), main="Distribución de número de catálogos enviados", 
        xlab="Catalogos", col = 35:75,
        ylab="Frecuencia")

fi <- prop.table(table(df$Catalogos))
df2 <- as.data.frame(fi)
# Barras simples
pie <- ggplot(df2, aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(stat="identity", width=1)

# Conversión a "pie" (coordenadas polares)
pie <- pie + coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Freq*100), "%")),
            position = position_stack(vjust = 0.5))       
# Cambio de escala de colores
pie <- pie + scale_fill_manual(values=c("#61d04e", "#2297e6", "#29e2e5","#cd0abc"))
# Remover y añadir etiquetas/título
pie <- pie + labs(x = NULL, y = "", fill = "Catalogos",
                  title = "")
# Limpiar el formato
pie <- pie + theme_classic() + theme(axis.line = element_blank(),
                                     axis.text = element_blank(),
                                     axis.ticks = element_blank(),
                                     plot.title = element_text(hjust = 0.5, 
                                                               color = "#666666")) 
pie 

########################################################################################################################
# d) Presentar y visualizar la distribución del salario de los clientes. Analizar los resultados
########################################################################################################################
hist(df$Salario,main="Distribución de salario de los clientes", xlab="Salario", ylab="Frecuencia")
boxplot(df$Salario)

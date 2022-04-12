# load the df ozone
install.packages("mlbench")
install.packages("VIM")
install.packages("naniar")
install.packages("missMDA")
install.packages("Amelia")
install.packages("mice")
install.packages("missForest")
install.packages("FactoMineR")
install.packages("finalfit")
install.packages('DMwR2')

library(mlbench)
data("Ozone")
df_ozone = Ozone
########################################################################################################################
# a) ¿Cuántas filas con valores perdidos hay en el conjunto de datos? 
#    ¿Qué porcentaje de todos los valores es?
########################################################################################################################
total_rows = nrow(df_ozone)
total_rows_non_na = nrow(na.omit(df_ozone))
total_rows_na = total_rows - total_rows_non_na
total_rows_na

percent_all_values = (total_rows_na / total_rows)*100
percent_all_values
########################################################################################################################
# b) ¿Cuántos atributos (columnas) hay sin ningún dato perdido? 
#    Describir la cantidad y el porcentaje de datos perdidos que presenta cada atributo.
########################################################################################################################
library(naniar)
gg_miss_var(df_ozone)


# columns with no data lost
columns_with_data_lost <- which(colSums(is.na(df_ozone))!=0)
columns_with_data_lost

columns_with_no_data_lost <- which(colSums(is.na(df_ozone))==0)
columns_with_no_data_lost

# Cantidad de valores perdidos en las columnas
nperdidos <- colSums(is.na(df_ozone[,columns_with_data_lost]))
nperdidos


library(VIM) #visualize the pattern of NAs
mp = aggr(df_ozone, numbers=TRUE)
summary(mp)

# get a  descriptive analisys from the ozone dataframe
library(finalfit)
df_ozone[,columns_with_data_lost] %>%
  ff_glimpse()

# get the missing data distribution
df_ozone %>%
  missing_plot()

########################################################################################################################
# c) ¿Cuántos patrones de datos perdidos distintos presenta el conjunto de datos? 
#    ¿Cuál es el que ocurre con mayor frecuencia? Justificar la respuesta usando una gráfica apropiada.
########################################################################################################################
library(finalfit)
df_ozone %>%
  missing_pattern()

library(VIM) #visualize the pattern of NAs
mp = aggr(df_ozone, prop=FALSE, numbers=TRUE)
summary(mp)

#######################################################################################################################
# d) ¿Es posible identificar algún patrón de datos perdidos 
#    (por ejemplo, la pérdida de información en alguna variable parece estar asociada a otra)?
########################################################################################################################
matrixplot(df_ozone, sortby = "V9")
matrixplot(df_ozone, sortby = "V7")
matrixplot(df_ozone, sortby = "V5")

library(VIM)
vars1 = c("V4","V7", "V10", "V12",  "V9")
VIM::pbox(df_ozone[,vars1], pos=1)   


########################################################################################################################
# e) Se desea realizar la imputación de la variable 4 (V4). 
#    Identificar a qué tipo de mecanismo se puede atribuir la presencia de estos datos perdidos y realizar la imputación.
#########################################################################################################################
library("Hmisc")
# get a matrix correlation 
df_ozone.rcorr = rcorr(as.matrix(df_ozone))
df_ozone.rcorr

# plot the matrix in order to review the v4 and others
matrixplot(df_ozone, sortby = "V4")

# print the missing data 
df_oze2 <- df_ozone[is.na(df_ozone$V4),]
df_oze2

library(VIM)
# execute imputation
df_oze <- VIM::kNN(data=df_ozone, variable=c("V4"), k=5)
# print the missing data before imputation
aggr(df_ozone, numbers=TRUE, prop=c(TRUE,FALSE))
# print the missing data after imputation
aggr(df_oze, delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))


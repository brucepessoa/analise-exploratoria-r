################################################################################
## TRABALHO DE R - MÉDIA DE ALUNOS POR TURMA DA EDUCAÇÃO BÁSICA E FUNDAMENTAL
## PROFESSORES: SÉRGIO MONTEIRO E MANUEL MARTINS
##
## INTEGRANTES: 
## BRUCE PESSOA MATR: 2021000189
## BRUNO LOSSE MATR: 2015101674
## CLÓVIS DE SOUZA MATR: 2021000178

################################################################################


##################################################################
## INSTALACAO DE PACOTES
##################################################################

packages <- c("tidyverse", "ggplot2", "corrplot","readxl","writexl","dplyr","plyr","data.table","outliers")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages()))) }

##################################################################
## CARREGAMENTO DE PACOTES
##################################################################

library(tidyverse)
library(ggplot2)
library(corrplot)
library(readxl)
library(writexl)
library(dplyr)
library(plyr) 
library(data.table)
library(outliers)


####################################################################
## DEFININDO AS VARIÁVEIS
####################################################################

soma<-0
cont<-0
k<-1
ano<-c(1,2,3,4,5,6,7,8,9)
tam<-length(ano)

xn = "NORTE!P9:x24039"
xn1 = "NORDESTE-EXT MA E BA!P9:x42021"
xn2 = "NORDESTE - SOMENTE MA E BA!P9:x34418"
xco = "CENTRO-OESTE!P9:x9660"
xse = "SUDESTE!P9:x56352"
xs = "SUL!P9:x24072"

x<-xn
####################################################################
## FUNCAO MEDIA
####################################################################

media = function(x)
        {  
          ###Importação do dataset###
          ds <- read_excel("d:\\Unicarioca\\Trabalho\\escolas_media_alunos_turma_2010.xls",range = x, na="--")
          ds <- na.omit(ds)
          
          medias<-vector()

          while(k<=tam)
          {
            soma<-sum(ds[k],na.rm=TRUE)
            media<-mean(ds[[k]],na.rm=TRUE)      
            medias[k]<-media
            k<-k+1
          }
          return(medias)
        }

##################################################################
## FUN??O MEDIANA
##################################################################

mediana = function(x)
        { 
          ###Importação do dataset###
          ds <- read_excel("d:\\Unicarioca\\Trabalho\\escolas_media_alunos_turma_2010.xls",range = x, na="--")
          ds <- na.omit(ds) 

          M<-vector()

          while(k<=tam)
          {
            M[k]<-median(ds[[k]],na.rm=TRUE)
            k<-k+1
          }
          return(M)
        }

##################################################################
## FUN??O MINIMO
##################################################################

minimo = function(x)
        {  
          ###Importação do dataset###
          ds <- read_excel("d:\\Unicarioca\\Trabalho\\escolas_media_alunos_turma_2010.xls",range = x, na="--")
          ds <- na.omit(ds)
          
          M<-vector()

          while(k<=tam)
          {
            M[k]<-min(ds[[k]],na.rm=TRUE)
            k<-k+1
          }
          return(M)
        }

##################################################################
## FUN??O MAXIMO
##################################################################

maximo = function(x)
        {  
          ###Importação do dataset###
          ds <- read_excel("d:\\Unicarioca\\Trabalho\\escolas_media_alunos_turma_2010.xls",range = x, na="--")
          ds <- na.omit(ds)

          M<-vector()

          while(k<=tam)
          {
            M[k]<-max(ds[[k]],na.rm=TRUE)
            k<-k+1
          }
          return(M)
        }

##################################################################
## FUN??O DESVIO PADR?O
##################################################################

desvio = function(x)
        {  
          ###Importação do dataset###
          ds <- read_excel("d:\\Unicarioca\\Trabalho\\escolas_media_alunos_turma_2010.xls",range = x, na="--")
          ds <- na.omit(ds)

          M<-vector()

          while(k<=tam)
          {
            M[k]<-sd(ds[[k]],na.rm=TRUE)
            k<-k+1
          }
          return(M)
        }



##################################################################
## FUN??O OUTLIER SUPERIOR
##################################################################

outliersSuperior= function(x)
        {  
          ###Importação do dataset###
          ds <- read_excel("d:\\Unicarioca\\Trabalho\\escolas_media_alunos_turma_2010.xls",range = x, na="--")
          ds <- na.omit(ds)

          M<-vector()

          while(k<=tam)
          {
            M[k]<-outlier(ds[[k]],opposite=TRUE)
            k<-k+1
          }
          return(M)
        }


##################################################################
## FUNCAO OUTLIER INFERIOR
##################################################################

outliersInferior= function(x)
        {  
          ###Importacao do dataset###
          ds <- read_excel("d:\\Unicarioca\\Trabalho\\escolas_media_alunos_turma_2010.xls",range = x, na="--")
          ds <- na.omit(ds)

          M<-vector()

          while(k<=tam)
          {
            M[k]<-outlier(ds[[k]])
            k<-k+1
          }
          return(M)
        }


##################################################################
## CARREGAMENTO DE FUNCAO QUARTIS
##################################################################


Quartis = function(x){
  
  ###Importacao do dataset###
  ds <- read_excel("d:\\Unicarioca\\Trabalho\\escolas_media_alunos_turma_2010.xls",range = x, na="--")
  ds <- na.omit(ds)

  M<-vector()
  N<-vector()
  
  while(k<=tam)
  {
    p= paste("QUARTIS", ano[k])
    print(p)
    print(quantile(ds[[k]],type=4))
    k<-k+1
  }
  
}



####################################################################
## IMPRIMINDO A MEDIA DE TODAS AS SERIES POR REGIAO
####################################################################

MEDIA_NORTE = round(media(xn))
MEDIA_NORDESTE_1 = round(media(xn1))
MEDIA_NORDESTE_2 = round(media(xn2))
MEDIA_CENTRO_OESTE = round(media(xco))
MEDIA_SUDESTE = round(media(xse))
MEDIA_SUL = round(media(xs))

print(MEDIA_NORTE)
print(MEDIA_NORDESTE_1)
print(MEDIA_NORDESTE_2)
print(MEDIA_CENTRO_OESTE)
print(MEDIA_SUDESTE)
print(MEDIA_SUL)

#################################################################
## IMPRIMINDO MEDIA TOTAL DE CADA REGIAO
#################################################################
mediatotal_n <- round(sum(MEDIA_NORTE)/9)
mediatotal_n1 <- round(sum(MEDIA_NORDESTE_1)/9)
mediatotal_n2 <- round(sum(MEDIA_NORDESTE_2)/9)
mediatotal_co <- round(sum(MEDIA_CENTRO_OESTE)/9)
mediatotal_se <- round(sum(MEDIA_SUDESTE)/9)
mediatotal_s <- round(sum(MEDIA_SUL)/9)

vecmedia<-vector()   # vetor com m?dia de cada regi?o
vecmedia<-c(mediatotal_n,mediatotal_n1,mediatotal_n2,mediatotal_co,mediatotal_se,mediatotal_s)
vecmedia
totmedias<-mean(mediatotal_n,mediatotal_n1,mediatotal_n2,mediatotal_co,mediatotal_se,mediatotal_s)
totmedias

##################################################################
##IMPRIMINDO A MEDIANA
##################################################################

MEDIANA_NORTE = round(mediana(xn))
MEDIANA_NORDESTE_1 = round(mediana(xn1))
MEDIANA_NORDESTE_2 = round(mediana(xn2))
MEDIANA_CENTRO_OESTE = round(mediana(xco))
MEDIANA_SUDESTE = round(mediana(xse))
MEDIANA_SUL = round(mediana(xs))

print(MEDIANA_NORTE)
print(MEDIANA_NORDESTE_1)
print(MEDIANA_NORDESTE_2)
print(MEDIANA_CENTRO_OESTE)
print(MEDIANA_SUDESTE)
print(MEDIANA_SUL)


##################################################################
##IMPRIMINDO MINIMO
##################################################################

MINIMO_NORTE = round(minimo(xn))
MINIMO_NORDESTE_1 = round(minimo(xn1))
MINIMO_NORDESTE_2 = round(minimo(xn2))
MINIMO_CENTRO_OESTE = round(minimo(xco))
MINIMO_SUDESTE = round(minimo(xse))
MINIMO_SUL = round(minimo(xs))

print(MINIMO_NORTE)
print(MINIMO_NORDESTE_1)
print(MINIMO_NORDESTE_2)
print(MINIMO_CENTRO_OESTE)
print(MINIMO_SUDESTE)
print(MINIMO_SUL)


##################################################################
##IMPRIMINDO O MAXIMO
##################################################################

MAXIMO_NORTE = round(maximo(xn))
MAXIMO_NORDESTE_1 = round(maximo(xn1))
MAXIMO_NORDESTE_2 = round(maximo(xn2))
MAXIMO_CENTRO_OESTE = round(maximo(xco))
MAXIMO_SUDESTE = round(maximo(xse))
MAXIMO_SUL = round(maximo(xs))

print(MAXIMO_NORTE)
print(MAXIMO_NORDESTE_1)
print(MAXIMO_NORDESTE_2)
print(MAXIMO_CENTRO_OESTE)
print(MAXIMO_SUDESTE)
print(MAXIMO_SUL)


##################################################################
##IMPRIMINDO O DESVIO PADRAO
##################################################################

DESVIO_NORTE = round(desvio(xn))
DESVIO_NORDESTE_1 = round(desvio(xn1))
DESVIO_NORDESTE_2 = round(desvio(xn2))
DESVIO_CENTRO_OESTE = round(desvio(xco))
DESVIO_SUDESTE = round(desvio(xse))
DESVIO_SUL = round(desvio(xs))

print(DESVIO_NORTE)
print(DESVIO_NORDESTE_1)
print(DESVIO_NORDESTE_2)
print(DESVIO_CENTRO_OESTE)
print(DESVIO_SUDESTE)
print(DESVIO_SUL)



##################################################################
## IMPRIMINDO OUTLIER SUPERIO/INFERIOR
##################################################################

OUTLIER_S_NORTE = round(outliersSuperior(xn))
OUTLIER_I_NORTE = round(outliersInferior(xn))
OUTLIER_S_NORDESTE_1= round(outliersSuperior(xn))
OUTLIER_I_NORDESTE_1= round(outliersInferior(xn))
OUTLIER_S_NORDESTE_2= round(outliersSuperior(xn))
OUTLIER_I_NORDESTE_2= round(outliersInferior(xn))
OUTLIER_S_CENTRO_OESTE= round(outliersSuperior(xn))
OUTLIER_I_CENTRO_OESTE= round(outliersInferior(xn))
OUTLIER_S_SUDESTE= round(outliersSuperior(xn))
OUTLIER_I_SUDESTE= round(outliersInferior(xn))
OUTLIER_S_SUL= round(outliersSuperior(xn))
OUTLIER_I_SUL= round(outliersInferior(xn))

print(OUTLIER_I_NORTE)
print(OUTLIER_S_NORTE)
print(OUTLIER_I_NORDESTE_1)
print(OUTLIER_S_NORDESTE_1)
print(OUTLIER_I_NORDESTE_2)
print(OUTLIER_S_NORDESTE_2)
print(OUTLIER_I_CENTRO_OESTE)
print(OUTLIER_S_CENTRO_OESTE)
print(OUTLIER_I_SUDESTE)
print(OUTLIER_S_SUDESTE)
print(OUTLIER_I_SUL)
print(OUTLIER_S_SUL)


##################################################################
## IMPRESSAO DA FUNCAO QUARTIS
##################################################################

Quartis(xn)
Quartis(xn1)
Quartis(xn2)
Quartis(xco)
Quartis(xse)
Quartis(xs)


###################################################
### GRÁFICO DE BARRAS DAS MÉDIAS POR CADA REGIÃO
###################################################

############ CONFIGURAÇÃO DO GRÁFICO ##############

DF <- list(MEDIA_NORTE,MEDIA_NORDESTE_1,MEDIA_NORDESTE_2,MEDIA_SUDESTE,MEDIA_SUL,MEDIA_CENTRO_OESTE)
DF
###TRANSFORMANDO EM DATAFRAME
DF = as.data.frame(do.call(rbind, DF)) 

### TRANSPONDO A MATRIZ
DF <- transpose(DF)

###REDEFINE COLUNAS E LINHAS
colnames(DF) <- c("NORTE","NORDESTE_1","NORDESTE_2","SUDESTE","SUL","CENTRO_OESTE")

DF$SERIES = c("1º Ano","1º série/ 2º ano","2ª série/ 3º ano","3ª série/ 4º ano","4ª série/ 5º ano","5ª série/ 6º ano","6ª série/ 7º ano","7ª série/ 8º ano","8ª série/ 9º ano")

###AUMENTA A MARGEM DE BAIXO DO GR?FICO
par(mar=c(10,6,4,10), mgp=c(9,1,0))

###PLOTANDO O GRÁFICO DE BARRAS

#NORTE
graf_norte<- barplot(DF$NORTE , border=F , names.arg=DF$SERIES , 
                  las=2 , 
                  xlab = "séries",
                  ylab = "Média de alunos por turma",
                  col=c(rgb(0.3,0.1,0.4,0.6), rgb(0.4,0.5,0.4,0.6) , rgb(0.5,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6) , rgb(0.6,0.5,0.4,0.6), rgb(0.7,0.5,0.4,0.6), rgb(0.8,0.5,0.4,0.6), rgb(0.9,0.5,0.4,0.6)),
                  ylim=c(0,30),
                  main="NORTE" )
y_norte<-DF$NORTE
text(graf_norte, y_norte, labels = as.character(y_norte))

#NORDESTE_1
graf_nordeste1<- barplot(DF$NORDESTE_1 , border=F , names.arg=DF$SERIES , 
                     las=2 , 
                     xlab = "séries",
                     ylab = "Média de alunos por turma",
                     col=c(rgb(0.3,0.1,0.4,0.6), rgb(0.4,0.5,0.4,0.6) , rgb(0.5,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6) , rgb(0.6,0.5,0.4,0.6), rgb(0.7,0.5,0.4,0.6), rgb(0.8,0.5,0.4,0.6), rgb(0.9,0.5,0.4,0.6)),
                     ylim=c(0,30),
                     main="NORDESTE (Exceto MA e BA)" )
y_nordeste1<-DF$NORDESTE_1
text(graf_nordeste1, y_nordeste1, labels = as.character(y_nordeste1))

#NORDESTE_2
graf_nordeste2<- barplot(DF$NORDESTE_2 , border=F , names.arg=DF$SERIES , 
                         las=2 , 
                         xlab = "séries",
                         ylab = "Média de alunos por turma",
                         col=c(rgb(0.3,0.1,0.4,0.6), rgb(0.4,0.5,0.4,0.6) , rgb(0.5,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6) , rgb(0.6,0.5,0.4,0.6), rgb(0.7,0.5,0.4,0.6), rgb(0.8,0.5,0.4,0.6), rgb(0.9,0.5,0.4,0.6)),
                         ylim=c(0,30),
                         main="NORDESTE (Somente MA e BA)" )
y_nordeste2<-DF$NORDESTE_2
text(graf_nordeste2, y_nordeste2, labels = as.character(y_nordeste2))

#CENTRO_OESTE
graf_centro_oeste<- barplot(DF$CENTRO_OESTE , border=F , names.arg=DF$SERIES , 
                     las=2 , 
                     xlab = "séries",
                     ylab = "Média de alunos por turma",
                     col=c(rgb(0.3,0.1,0.4,0.6), rgb(0.4,0.5,0.4,0.6) , rgb(0.5,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6) , rgb(0.6,0.5,0.4,0.6), rgb(0.7,0.5,0.4,0.6), rgb(0.8,0.5,0.4,0.6), rgb(0.9,0.5,0.4,0.6)),
                     ylim=c(0,30),
                     main="CENTRO_OESTE" )
y_centro_oeste<-DF$CENTRO_OESTE
text(graf_centro_oeste, y_centro_oeste, labels = as.character(y_centro_oeste))

#SUDESTE
graf_sudeste<- barplot(DF$SUDESTE , border=F , names.arg=DF$SERIES , 
                            las=2 , 
                            xlab = "séries",
                            ylab = "Média de alunos por turma",
                            col=c(rgb(0.3,0.1,0.4,0.6), rgb(0.4,0.5,0.4,0.6) , rgb(0.5,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6) , rgb(0.6,0.5,0.4,0.6), rgb(0.7,0.5,0.4,0.6), rgb(0.8,0.5,0.4,0.6), rgb(0.9,0.5,0.4,0.6)),
                            ylim=c(0,30),
                            main="SUDESTE" )
y_sudeste<-DF$SUDESTE
text(graf_sudeste, y_sudeste, labels = as.character(y_sudeste))

# SUL
graf_sul<- barplot(DF$SUL , border=F , names.arg=DF$SERIES , 
                       las=2 , 
                       xlab = "séries",
                       ylab = "Média de alunos por turma",
                       col=c(rgb(0.3,0.1,0.4,0.6), rgb(0.4,0.5,0.4,0.6) , rgb(0.5,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6) , rgb(0.6,0.5,0.4,0.6), rgb(0.7,0.5,0.4,0.6), rgb(0.8,0.5,0.4,0.6), rgb(0.9,0.5,0.4,0.6)),
                       ylim=c(0,30),
                       main="SUL" )
y_sul<-DF$SUL
text(graf_sul, y_sul, labels = as.character(y_sul))


################################################################
## GRAFICO MEDIA - TOTAL POR REGIAO
################################################################

#### Gráfico com barplot ####
barra<-barplot(vecmedia, xlab="Regioes", col="tomato", ylab="Media", ylim=c(0,30))
text(x = barra, y = vecmedia-1, labels = vecmedia)

#### Gráfico com ggplot ####
data_frame <- data.frame(col_regiao = c("NORTE","NORDESTE_1","NORDESTE_2","SUDESTE","SUL","CENTRO_OESTE"),col_medias = vecmedia)
print(data_frame)        

ggplot(data_frame, aes(x = col_regiao, y = col_medias, fill = col_regiao)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = col_medias), vjust = 0)

#####################################################################
##### SUMÁRIO
#####################################################################

resumo_n<-summary(DF$NORTE)
resumo_n1<-summary(DF$NORDESTE_1)
resumo_n2<-summary(DF$NORDESTE_2)
resumo_co<-summary(DF$CENTRO_OESTE)
resumo_se<-summary(DF$SUDESTE)
resumo_s<-summary(DF$SUL)


resumo_n
resumo_n1
resumo_n2
resumo_co
resumo_se
resumo_s



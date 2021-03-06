## ----setup, include=FALSE------------------------------------------------
# Limpando o workspace

rm(list = ls())

# Configura��es Globais

knitr::opts_chunk$set(echo = FALSE, warning  = F, fig.width = 12, fig.height = 7, message = F, results = 'hide')
options(digits=3, knitr.kable.NA = 'NA')

# Definindo Diret�rio

setwd('C:/Documentos/Trabalho de Macro/R')

# Pacotes

require(dplyr)
require(WDI)
require(ggplot2)
require(rworldmap)
require(RColorBrewer)

## Alterando Tema do GGPLOT2 (Personaliza��o dos Gr�ficos)

theme_update(# axis labels
             axis.title = element_text(size = 20), # Tamanho da fonte dos Eixos
             # tick labels
             axis.text = element_text(size = 20),  # Tamanho da fonte dos textos dos labs
             # title 
             title = element_text(size = 20))      # Tamanho da fonte do T�tulo

# Definindo os pa�ses que irei trabalhar

continente <- c('BR','VE','AR','BO','CL','CO','EC','GY','PY','PE','SR','UY')
pais <- 'BR'

# Baixando os dados do Banco Mundial

# Atrav�s da fun��o WDI consigo abaixar diretamente dados do World Development Indicators do Banco Mundial

data <- data.frame(WDI(country = continente, indicator = c('NY.GDP.PCAP.KD.ZG','NY.GDP.PCAP.KD','NY.GDS.TOTL.ZS','UIS.EA.6.AG25T99','SP.POP.GROW','NE.GDI.TOTL.ZS'),
                       start = 1975,
                       end = 2009,
                       extra = T))

# Data Clean

namecols <- c('Simbolos',
              'Pais',
              'Ano',
              'Crescimento do PIB (Anual %)',
              'GDP per Capita',
              'Poupan�a (% GDP)',
              'Capital Humano (% Pop.)',
              'Crescimento Pop. (%)',
              'Forma��o de Capital Bruta (% GDP)',
              'SIMB',
              'Regiao',
              'capital',
              'longitude',
              'latitude',
              'renda',
              'Emprestimo')

colnames(data) <- namecols

# Separando os dados para o Brasil x Am�rica do Sul

br <- data %>% filter(data$Simbolos == pais) %>% arrange(Ano)
southamerica <- aggregate(data = data, `Crescimento do PIB (Anual %)` ~ Ano, FUN = mean) %>% arrange(Ano)

## ----Dist. Renda 1975----------------------------------------------------

# Dados para a forma��o do Mapa Mundi

datamap <- WDI(indicator = 'NY.GDP.PCAP.KD', start = 1975, end = 1975) # Pegando os dados do WDI para 1975

# Aglomerando os dados para o formato espacial

sPDF <- joinCountryData2Map(datamap,
                            joinCode = 'ISO2',
                            nameJoinColumn = 'iso2c',
                            mapResolution = 'coarse')

# Parametro para definir o intervalo de renda que ser� mostrado no mapa #Aconselha-se a n�o alterar

classInt <- classInt::classIntervals(sPDF[["NY.GDP.PCAP.KD"]], n=8, style="jenks")
catMethod <- classInt[['brks']]

# Pegando as cores do pacote RColorBrewer

colourPalette <- RColorBrewer::brewer.pal(8, 'RdPu')

# Criando o Mapa

mapParams <- mapCountryData(sPDF,                    # Dados
               nameColumnToPlot = 'NY.GDP.PCAP.KD',  # Vari�vel que ser� utilizada
               addLegend = F,                        # Retirar legenda
               catMethod = catMethod,                # Definindo o m�todo de separa��o do intervalo
               colourPalette = colourPalette,        # Definindo as cores do intervalo
               oceanCol = 'lightblue',               # Cor do Oceano
               mapTitle = 'Renda Per Capita - 1975', # T�tulo do Gr�fico
               missingCountryCol = 'darkgray',       # Definindo a cor para pa�ses que n�o possuem dados
               borderCol = 'gray')                   # Definindo a cor das bordas

# Chamando uma fun��o para adicionar uma legenda externa

do.call(addMapLegend,
        c(mapParams,
          legendLabels = 'all',
          legendWidth = 0.5,       # Tamanho da Legenda (propor��o)
          legendInterval = 'data', # Inter. (data = singifica que ir� utilizar a referencia dada no mapParams)
          legendMar = 2))          # Definindo a altura da legenda

# Plotando o gr�fico

mapParams   


## ----Dist. Renda 2009----------------------------------------------------
datamap <- WDI(indicator = 'NY.GDP.PCAP.KD', start = 2009, end = 2009)

sPDF <- joinCountryData2Map(datamap,
                            joinCode = 'ISO2',
                            nameJoinColumn = 'iso2c',
                            mapResolution = 'coarse')

# Parametro

classInt <- classInt::classIntervals(sPDF[["NY.GDP.PCAP.KD"]], n=8, style="jenks")
catMethod <- classInt[['brks']]

# Pegando as cores do pacote RColorBrewer

colourPalette <- RColorBrewer::brewer.pal(8, 'RdPu')

# Criando o Mapa

mapParams <- mapCountryData(sPDF,
               nameColumnToPlot = 'NY.GDP.PCAP.KD',
               addLegend = F,
               catMethod = catMethod,
               colourPalette = colourPalette,
               oceanCol = 'lightblue',
               mapTitle = 'Renda Per Capita - 2009',
               missingCountryCol = 'gray',
               borderCol = 'gray')

do.call(addMapLegend,
        c(mapParams,
          legendLabels = 'all',
          legendWidth = 0.5,
          legendInterval = 'data',
          legendMar = 2))

mapParams

## ----graph 1-2-----------------------------------------------------------

# Mapa Latin America
p1 <- ggplot(southamerica, aes(x = Ano)) +                # Definindo southamerica como os dados
  geom_line(aes(y = `Crescimento do PIB (Anual %)`)) +    # Gr�fico de Linha em que y = Cresc. PIB
  xlab('Ano') +                                           # Definindo o nome para o eixo x
  ylab('Tx. Cresc. PIB per Capita (%)') +
  ggtitle('Am�rica do Sul - 1975-2009') +                 # T�tulo do Gr�fico
  geom_hline(yintercept = 0, color = 'blue', linetype = 'dashed') + # Linha reta no 0
  geom_hline(yintercept = mean(southamerica$`Crescimento do PIB (Anual %)`) + 1.96*sd(southamerica$`Crescimento do PIB (Anual %)`),
             linetype = 'dashed',
             color = 'red') +
  geom_hline(yintercept = mean(southamerica$`Crescimento do PIB (Anual %)`) - 1.96*sd(southamerica$`Crescimento do PIB (Anual %)`),
             linetype = 'dashed',
             color = 'red') +
  theme_bw()

# Brazil

p2 <- ggplot(br, aes(x = Ano)) +
  geom_line(aes(y = `Crescimento do PIB (Anual %)`)) +
  xlab('Ano') +
  ylab('Tx. Cresc. PIB per Capita (%)') +
  ggtitle('Brasil - 1975-2009') +
  geom_hline(yintercept = 0, color = 'blue', linetype = 'dashed') +
  geom_hline(yintercept = mean(br$`Crescimento do PIB (Anual %)`) + 1.96*sd(br$`Crescimento do PIB (Anual %)`),
             linetype = 'dashed',
             color = 'red') +
  geom_hline(yintercept = mean(br$`Crescimento do PIB (Anual %)`) - 1.96*sd(br$`Crescimento do PIB (Anual %)`),
             linetype = 'dashed',
             color = 'red') +
  theme_bw()

gridExtra::grid.arrange(p1,p2, nrow = 1)



## ----Aggregate, include = F----------------------------------------------

# Agregando (tirando a m�dia das vari�veis) por Ano

## Am�rica do Sul

aggregated <- aggregate(data = data, cbind(`Crescimento do PIB (Anual %)`,`Forma��o de Capital Bruta (% GDP)`, `GDP per Capita`, `Crescimento Pop. (%)`) ~ Ano , FUN = mean)

## Brasil

aggregatedBR <- aggregate(data = data %>% filter(Pais == 'Brazil'), cbind(`Crescimento do PIB (Anual %)`,`Forma��o de Capital Bruta (% GDP)`, `GDP per Capita`, `Crescimento Pop. (%)`) ~ Ano , FUN = mean)


## ------------------------------------------------------------------------

# regress�o do Cresc. PIB em rela��o a GDP per Capita

gy.rgdp <- lm(data = aggregated , `Crescimento do PIB (Anual %)` ~ `GDP per Capita`)

# Plotando um relat�rio

pander::pander(coef(summary(gy.rgdp)))


## ----Tabela 1------------------------------------------------------------

# Agregando para um n�mero menor de vari�veis

aggrSA <- aggregate(data = data, cbind(`Crescimento do PIB (Anual %)`, `GDP per Capita`) ~ Pais, FUN = mean) %>%
  arrange(desc(`Crescimento do PIB (Anual %)`))

# Transformando esses dados agregados em uma tabela

knitr::kable(aggrSA, caption = 'Informa��es')

## ----Tabela 2------------------------------------------------------------

# Filtrando os dados para os pa�ses ricos e pobres
## OBS: dependendo dos seus dados, as posi��es dos cinco �ltimos podem mudar.

fiverichestgdp <- mean(aggrSA$`Crescimento do PIB (Anual %)`[1:5])    # GDP Growth 5 mais ricos
fiverichestgdpg <- mean(aggrSA$`GDP per Capita`[1:5])                 # GDP 5 mais ricos

fivepoorestgdp <- mean(aggrSA$`Crescimento do PIB (Anual %)`[7:11])   # GDP Growth 5 mais pobres 
fivepoorestgdpg <- mean(aggrSA$`GDP per Capita`[7:11])                # GDP 5 mais pobres


# Construindo um Dataframe com os dados dos 5 pa�ses mais ricos e mais pobres

rankcountry <- cbind(rbind(fivepoorestgdp, fivepoorestgdpg),
                       rbind(fiverichestgdp, fiverichestgdpg))

# Parametros para personifica��o da tabela

rownames(rankcountry) <- c('Growth','GDP Per capita')
colnames(rankcountry) <- c('Menores','Maiores')

# Mostrando os dados dos pa�ses mais ricos e mais pobres em tabela

knitr::kable(rankcountry, caption = 'Compara��o entre GDP')

## ----graph 3-4-----------------------------------------------------------

# Agregando os n�veis de Poupan�a por Ano para a Am�rica do Sul

savingsSA <- aggregate(data = data, `Poupan�a (% GDP)` ~ Ano, FUN = mean) %>% arrange(Ano)


p3 <- ggplot(savingsSA, aes(x = Ano)) +
  geom_line(aes(y = `Poupan�a (% GDP)`)) +
  xlab('Ano') +
  ylab('Poupan�a (% GDP)') +
  ggtitle('Am�rica do Sul, 1975-2009') +
  geom_hline(yintercept = mean(savingsSA$`Poupan�a (% GDP)`), color = 'blue', linetype = 'dashed') +
  theme_bw()

p4 <- ggplot(br, aes(x = Ano)) +
  geom_line(aes(y = `Poupan�a (% GDP)`)) +
  xlab('Ano') +
  ylab('Poupan�a (% GDP)') +
  ggtitle('Brasil, 1975-2009') +
  geom_hline(yintercept = mean(br$`Poupan�a (% GDP)`), color = 'blue', linetype = 'dashed') +
  theme_bw()

# Juntando os gr�ficos (2 em 1)

gridExtra::grid.arrange(p3,p4, nrow = 1)


## ----Tabela 3------------------------------------------------------------

# Agregando diversas vari�veis por ano, no intuito de mostrar uma tabela

savings <- aggregate(data = data, cbind(`Poupan�a (% GDP)`,`Crescimento do PIB (Anual %)`, `GDP per Capita`, `Crescimento Pop. (%)`,`Capital Humano (% Pop.)`) ~ Pais, FUN = mean) %>%
  arrange(desc(`Poupan�a (% GDP)`))

# Denominando o nome das colunas

colnam <- c('Pa�s','Poup. (%)','GDP Growth','GDP p.c.', 'Cresc. Pop. (%)','Cap. Hum. (% Pop.)')

# Mostrando a tabela com as informa��es dos dados acima

knitr::kable(savings, caption = 'Estat�sticas - M�dia Hist�rica', col.names = colnam)

## ----graph 5-------------------------------------------------------------
p5 <- ggplot(data = data %>% filter(Ano == 2009), aes(x = `Poupan�a (% GDP)`, y = `GDP per Capita`)) +
        geom_point(shape = 1, color = 'red') +
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        xlab('Poupan�a (% GDP)') +
        ylab('GDP per Capita') +
        ggtitle('Am�rica do Sul - 2009')

p5

## ----graph 6-------------------------------------------------------------
p6 <- ggplot(data = data %>% filter(Ano == 2009),
             aes(x = `Capital Humano (% Pop.)`, y = `GDP per Capita`)) +
        geom_point(shape = 1, color = 'red')+
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        xlab('Capital Humano (% Pop.)') +
        ylab('GDP per Capita') +
        ggtitle('Am�rica do Sul - 2009 - Capital Humano')

p6

## ----graph 7-8-----------------------------------------------------------

p7 <- ggplot(data = aggregated,
             aes(y = `Crescimento do PIB (Anual %)`, x = `Forma��o de Capital Bruta (% GDP)`)) +
        geom_point(shape = 1, color = 'red') +
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        ggtitle('Am�rica do Sul - 1975-2009')

p8 <- ggplot(data = aggregatedBR,
             aes(y = `Crescimento do PIB (Anual %)`, x = `Forma��o de Capital Bruta (% GDP)`)) +
        geom_point(shape = 1, color = 'red') +
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        ggtitle('Brasil - 1975-2009')

# Plotando os gr�ficos juntos
gridExtra::grid.arrange(p7,p8, nrow = 1)

## ----graph 9-10----------------------------------------------------------
p9 <- ggplot(data = aggregated,
             aes(y = `Crescimento do PIB (Anual %)`, x = `Crescimento Pop. (%)`)) +
        geom_point(shape = 1, color = 'red') +
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        ggtitle('Am�rica do Sul - 1975-2009')

p10 <- ggplot(data = aggregatedBR,
             aes(y = `Crescimento do PIB (Anual %)`, x = `Crescimento Pop. (%)`)) +
        geom_point(shape = 1, color = 'red') +
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        ggtitle('Brasil - 1975-2009')

gridExtra::grid.arrange(p9,p10, nrow = 1)



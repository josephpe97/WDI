## ----setup, include=FALSE------------------------------------------------
# Limpando o workspace

rm(list = ls())

# Configurações Globais

knitr::opts_chunk$set(echo = FALSE, warning  = F, fig.width = 12, fig.height = 7, message = F, results = 'hide')
options(digits=3, knitr.kable.NA = 'NA')

# Definindo Diretório

setwd('C:/Documentos/Trabalho de Macro/R')

# Pacotes

require(dplyr)
require(WDI)
require(ggplot2)
require(rworldmap)
require(RColorBrewer)

## Alterando Tema do GGPLOT2 (Personalização dos Gráficos)

theme_update(# axis labels
             axis.title = element_text(size = 20), # Tamanho da fonte dos Eixos
             # tick labels
             axis.text = element_text(size = 20),  # Tamanho da fonte dos textos dos labs
             # title 
             title = element_text(size = 20))      # Tamanho da fonte do Título

# Definindo os países que irei trabalhar

continente <- c('BR','VE','AR','BO','CL','CO','EC','GY','PY','PE','SR','UY')
pais <- 'BR'

# Baixando os dados do Banco Mundial

# Através da função WDI consigo abaixar diretamente dados do World Development Indicators do Banco Mundial

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
              'Poupança (% GDP)',
              'Capital Humano (% Pop.)',
              'Crescimento Pop. (%)',
              'Formação de Capital Bruta (% GDP)',
              'SIMB',
              'Regiao',
              'capital',
              'longitude',
              'latitude',
              'renda',
              'Emprestimo')

colnames(data) <- namecols

# Separando os dados para o Brasil x América do Sul

br <- data %>% filter(data$Simbolos == pais) %>% arrange(Ano)
southamerica <- aggregate(data = data, `Crescimento do PIB (Anual %)` ~ Ano, FUN = mean) %>% arrange(Ano)

## ----Dist. Renda 1975----------------------------------------------------

# Dados para a formação do Mapa Mundi

datamap <- WDI(indicator = 'NY.GDP.PCAP.KD', start = 1975, end = 1975) # Pegando os dados do WDI para 1975

# Aglomerando os dados para o formato espacial

sPDF <- joinCountryData2Map(datamap,
                            joinCode = 'ISO2',
                            nameJoinColumn = 'iso2c',
                            mapResolution = 'coarse')

# Parametro para definir o intervalo de renda que será mostrado no mapa #Aconselha-se a não alterar

classInt <- classInt::classIntervals(sPDF[["NY.GDP.PCAP.KD"]], n=8, style="jenks")
catMethod <- classInt[['brks']]

# Pegando as cores do pacote RColorBrewer

colourPalette <- RColorBrewer::brewer.pal(8, 'RdPu')

# Criando o Mapa

mapParams <- mapCountryData(sPDF,                    # Dados
               nameColumnToPlot = 'NY.GDP.PCAP.KD',  # Variável que será utilizada
               addLegend = F,                        # Retirar legenda
               catMethod = catMethod,                # Definindo o método de separação do intervalo
               colourPalette = colourPalette,        # Definindo as cores do intervalo
               oceanCol = 'lightblue',               # Cor do Oceano
               mapTitle = 'Renda Per Capita - 1975', # Título do Gráfico
               missingCountryCol = 'darkgray',       # Definindo a cor para países que não possuem dados
               borderCol = 'gray')                   # Definindo a cor das bordas

# Chamando uma função para adicionar uma legenda externa

do.call(addMapLegend,
        c(mapParams,
          legendLabels = 'all',
          legendWidth = 0.5,       # Tamanho da Legenda (proporção)
          legendInterval = 'data', # Inter. (data = singifica que irá utilizar a referencia dada no mapParams)
          legendMar = 2))          # Definindo a altura da legenda

# Plotando o gráfico

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
  geom_line(aes(y = `Crescimento do PIB (Anual %)`)) +    # Gráfico de Linha em que y = Cresc. PIB
  xlab('Ano') +                                           # Definindo o nome para o eixo x
  ylab('Tx. Cresc. PIB per Capita (%)') +
  ggtitle('América do Sul - 1975-2009') +                 # Título do Gráfico
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

# Agregando (tirando a média das variáveis) por Ano

## América do Sul

aggregated <- aggregate(data = data, cbind(`Crescimento do PIB (Anual %)`,`Formação de Capital Bruta (% GDP)`, `GDP per Capita`, `Crescimento Pop. (%)`) ~ Ano , FUN = mean)

## Brasil

aggregatedBR <- aggregate(data = data %>% filter(Pais == 'Brazil'), cbind(`Crescimento do PIB (Anual %)`,`Formação de Capital Bruta (% GDP)`, `GDP per Capita`, `Crescimento Pop. (%)`) ~ Ano , FUN = mean)


## ------------------------------------------------------------------------

# regressão do Cresc. PIB em relação a GDP per Capita

gy.rgdp <- lm(data = aggregated , `Crescimento do PIB (Anual %)` ~ `GDP per Capita`)

# Plotando um relatório

pander::pander(coef(summary(gy.rgdp)))


## ----Tabela 1------------------------------------------------------------

# Agregando para um número menor de variáveis

aggrSA <- aggregate(data = data, cbind(`Crescimento do PIB (Anual %)`, `GDP per Capita`) ~ Pais, FUN = mean) %>%
  arrange(desc(`Crescimento do PIB (Anual %)`))

# Transformando esses dados agregados em uma tabela

knitr::kable(aggrSA, caption = 'Informações')

## ----Tabela 2------------------------------------------------------------

# Filtrando os dados para os países ricos e pobres
## OBS: dependendo dos seus dados, as posições dos cinco últimos podem mudar.

fiverichestgdp <- mean(aggrSA$`Crescimento do PIB (Anual %)`[1:5])    # GDP Growth 5 mais ricos
fiverichestgdpg <- mean(aggrSA$`GDP per Capita`[1:5])                 # GDP 5 mais ricos

fivepoorestgdp <- mean(aggrSA$`Crescimento do PIB (Anual %)`[7:11])   # GDP Growth 5 mais pobres 
fivepoorestgdpg <- mean(aggrSA$`GDP per Capita`[7:11])                # GDP 5 mais pobres


# Construindo um Dataframe com os dados dos 5 países mais ricos e mais pobres

rankcountry <- cbind(rbind(fivepoorestgdp, fivepoorestgdpg),
                       rbind(fiverichestgdp, fiverichestgdpg))

# Parametros para personificação da tabela

rownames(rankcountry) <- c('Growth','GDP Per capita')
colnames(rankcountry) <- c('Menores','Maiores')

# Mostrando os dados dos países mais ricos e mais pobres em tabela

knitr::kable(rankcountry, caption = 'Comparação entre GDP')

## ----graph 3-4-----------------------------------------------------------

# Agregando os níveis de Poupança por Ano para a América do Sul

savingsSA <- aggregate(data = data, `Poupança (% GDP)` ~ Ano, FUN = mean) %>% arrange(Ano)


p3 <- ggplot(savingsSA, aes(x = Ano)) +
  geom_line(aes(y = `Poupança (% GDP)`)) +
  xlab('Ano') +
  ylab('Poupança (% GDP)') +
  ggtitle('América do Sul, 1975-2009') +
  geom_hline(yintercept = mean(savingsSA$`Poupança (% GDP)`), color = 'blue', linetype = 'dashed') +
  theme_bw()

p4 <- ggplot(br, aes(x = Ano)) +
  geom_line(aes(y = `Poupança (% GDP)`)) +
  xlab('Ano') +
  ylab('Poupança (% GDP)') +
  ggtitle('Brasil, 1975-2009') +
  geom_hline(yintercept = mean(br$`Poupança (% GDP)`), color = 'blue', linetype = 'dashed') +
  theme_bw()

# Juntando os gráficos (2 em 1)

gridExtra::grid.arrange(p3,p4, nrow = 1)


## ----Tabela 3------------------------------------------------------------

# Agregando diversas variáveis por ano, no intuito de mostrar uma tabela

savings <- aggregate(data = data, cbind(`Poupança (% GDP)`,`Crescimento do PIB (Anual %)`, `GDP per Capita`, `Crescimento Pop. (%)`,`Capital Humano (% Pop.)`) ~ Pais, FUN = mean) %>%
  arrange(desc(`Poupança (% GDP)`))

# Denominando o nome das colunas

colnam <- c('País','Poup. (%)','GDP Growth','GDP p.c.', 'Cresc. Pop. (%)','Cap. Hum. (% Pop.)')

# Mostrando a tabela com as informações dos dados acima

knitr::kable(savings, caption = 'Estatísticas - Média Histórica', col.names = colnam)

## ----graph 5-------------------------------------------------------------
p5 <- ggplot(data = data %>% filter(Ano == 2009), aes(x = `Poupança (% GDP)`, y = `GDP per Capita`)) +
        geom_point(shape = 1, color = 'red') +
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        xlab('Poupança (% GDP)') +
        ylab('GDP per Capita') +
        ggtitle('América do Sul - 2009')

p5

## ----graph 6-------------------------------------------------------------
p6 <- ggplot(data = data %>% filter(Ano == 2009),
             aes(x = `Capital Humano (% Pop.)`, y = `GDP per Capita`)) +
        geom_point(shape = 1, color = 'red')+
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        xlab('Capital Humano (% Pop.)') +
        ylab('GDP per Capita') +
        ggtitle('América do Sul - 2009 - Capital Humano')

p6

## ----graph 7-8-----------------------------------------------------------

p7 <- ggplot(data = aggregated,
             aes(y = `Crescimento do PIB (Anual %)`, x = `Formação de Capital Bruta (% GDP)`)) +
        geom_point(shape = 1, color = 'red') +
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        ggtitle('América do Sul - 1975-2009')

p8 <- ggplot(data = aggregatedBR,
             aes(y = `Crescimento do PIB (Anual %)`, x = `Formação de Capital Bruta (% GDP)`)) +
        geom_point(shape = 1, color = 'red') +
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        ggtitle('Brasil - 1975-2009')

# Plotando os gráficos juntos
gridExtra::grid.arrange(p7,p8, nrow = 1)

## ----graph 9-10----------------------------------------------------------
p9 <- ggplot(data = aggregated,
             aes(y = `Crescimento do PIB (Anual %)`, x = `Crescimento Pop. (%)`)) +
        geom_point(shape = 1, color = 'red') +
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        ggtitle('América do Sul - 1975-2009')

p10 <- ggplot(data = aggregatedBR,
             aes(y = `Crescimento do PIB (Anual %)`, x = `Crescimento Pop. (%)`)) +
        geom_point(shape = 1, color = 'red') +
        geom_smooth(method = 'lm', linetype = 'dashed') +
        theme_bw() +
        ggtitle('Brasil - 1975-2009')

gridExtra::grid.arrange(p9,p10, nrow = 1)



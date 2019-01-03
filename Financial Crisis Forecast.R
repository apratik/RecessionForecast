# Cargamos los paquetes necesarios
library(Quandl)
library(forecast)
library(e1071)

library(quantmod)
library(xts)
library(TTR)
library(ggplot2)
library(cowplot)

library(RColorBrewer)

library(tseries)
library(lubridate)

# Info a descargar desde Quandl
tickets <- c("ISM/MAN_PMI",
             "FRED/GS10", 
             "FRED/TB3MS", 
             "FRED/UNRATE",
             "FRED/USREC")

nombres.Tickets <- c("PMI",
                     "10YB", 
                     "3MB", 
                     "UnEmpRate",
                     "CRISIS")

periodos.Forecast <- 36

# Creamos la BBDD con la data macro
data.Set <- NULL
j <- 1
for (i in tickets){
  data.Set <- cbind(data.Set, Quandl(i, 
                                     api_key = "YOUR QUANDL API KEY", 
                                     type = "xts", 
                                     collapse = "daily"))
  print(paste("Descargando: ", nombres.Tickets[j], sep = "")) 
  j <- j + 1
}

data.Set <- na.omit(data.Set)
colnames(data.Set) <- nombres.Tickets

# Creamos el vector de fechas completo (con historico y forecast)
periodos.Proyectar <- 250
Fechas    <- seq(start(data.Set), by = "month", length.out = nrow(data.Set) + periodos.Proyectar)
Fecha.Fin <- end(data.Set) # Ultima Fecha de la data historica

# Adecuamos los formatos de los datos descargados
data.Set <- as.data.frame(data.Set)

# Vamos a unir los data.frame data.Set y BBDD.Forecast
num.Columnas <- ncol(data.Set) - 1
num.Filas    <- nrow(data.Set) + periodos.Proyectar

BBDD.Grafico <- data.frame(matrix(0,num.Filas,num.Columnas))
row.names(BBDD.Grafico) <- Fechas

# Proyectamos las variables para crear la BBDD Test
PMI <- auto.arima(data.Set$PMI)
pred.PMI <- forecast(PMI, h = periodos.Proyectar)
PMI.Grafico <- cbind(pred.PMI$fitted, pred.PMI$mean)

  for(j in 1:num.Filas){
    if(is.na(PMI.Grafico[j,1])){
      BBDD.Grafico[j,1] <- PMI.Grafico[j,2]
    }else{
      BBDD.Grafico[j,1] <- PMI.Grafico[j,1]
    }
  }

Bond10Y <- auto.arima(data.Set$`10YB`)
pred.Bond10Y <- forecast(Bond10Y, h = periodos.Proyectar)
Bond10Y.Grafico <- cbind(pred.Bond10Y$fitted, pred.Bond10Y$upper[,1])

  for(j in 1:num.Filas){
    if(is.na(Bond10Y.Grafico[j,1])){
      BBDD.Grafico[j,2] <- Bond10Y.Grafico[j,2]
    }else{
      BBDD.Grafico[j,2] <- Bond10Y.Grafico[j,1]
    }
  }

Bond3M <- auto.arima(data.Set$`3MB`)
pred.Bond3M <- forecast(Bond3M, h = periodos.Proyectar)
Bond3M.Grafico <- cbind(pred.Bond3M$fitted, pred.Bond3M$upper[,1])

  for(j in 1:num.Filas){
    if(is.na(Bond3M.Grafico[j,1])){
      BBDD.Grafico[j,3] <- Bond3M.Grafico[j,2]
    }else{
      BBDD.Grafico[j,3] <- Bond3M.Grafico[j,1]
    }
  }

UnEmpRate <- auto.arima(data.Set$UnEmpRate)
pred.UnEmpRate <- forecast(UnEmpRate, h = periodos.Proyectar)
UnEmpRate.Grafico <- cbind(pred.UnEmpRate$fitted, pred.UnEmpRate$mean)

  for(j in 1:num.Filas){
    if(is.na(UnEmpRate.Grafico[j,1])){
      BBDD.Grafico[j,4] <- UnEmpRate.Grafico[j,2]
    }else{
      BBDD.Grafico[j,4] <- UnEmpRate.Grafico[j,1]
    }
  }

colnames(BBDD.Grafico) <- c("PMI", "10Y", "3M", "UnEmpRate")
BBDD.Grafico           <- na.omit(BBDD.Grafico)
BBDD.Grafico$Fechas    <- Fechas
BBDD.Grafico$Slope     <- BBDD.Grafico$`10Y` - BBDD.Grafico$`3M`
BBDD.Grafico$UnEmpRate <- BBDD.Grafico$UnEmpRate/100

# Graficamos las variables a analizar (con su historico y su forecast)

  # Recuadro grafico de las recesiones
  recessions.df = read.table(textConnection(
    "Peak, Trough
    1953-08-01, 1954-05-01
    1957-08-01, 1958-04-01
    1960-04-01, 1961-02-01
    1969-12-01, 1970-11-01
    1973-11-01, 1975-03-01
    1980-01-01, 1980-07-01
    1981-07-01, 1982-11-01
    1990-07-01, 1991-03-01
    2001-03-01, 2001-11-01
    2007-12-01, 2009-06-01"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)

  BBDD.Grafico <- BBDD.Grafico[1:(nrow(data.Set) + periodos.Forecast), ]
  
  # Recuadro grafico del forecast
  forecast.df = data.frame(cbind(as.character(Fecha.Fin), 
                                 last(row.names(BBDD.Grafico))))

  colnames(forecast.df) <- c("Peak", "Trough")
  forecast.df$Peak      <- as.Date(forecast.df$Peak)
  forecast.df$Trough    <- as.Date(forecast.df$Trough)
  
g.emp<-
  ggplot(data=BBDD.Grafico, aes(x = Fechas, y = UnEmpRate))+
  geom_rect(data=recessions.df, inherit.aes=FALSE,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), 
            fill='lightblue', alpha=0.5) +
  geom_rect(data=forecast.df, inherit.aes=FALSE,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), 
            fill='yellow', alpha=0.5) +
  theme_minimal() +
  geom_line(color = "royalblue") +
  geom_hline(yintercept = 0, color = "black") +
  theme(plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic",size=9),
        plot.title=element_text(face="bold",size=14)) +
  labs(x = "",
       y = "",
       title = "Unemployment",
       subtitle = "Evolution of the unemployment in USA",
       capiton = "By: Carlos Jimenez") + scale_y_continuous(labels = scales::percent)

g.pmi<-
  ggplot(data=BBDD.Grafico, aes(x=Fechas,y=PMI))+
  geom_rect(data=recessions.df, inherit.aes=FALSE,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), 
            fill='lightblue', alpha=0.5)+theme_minimal() +
  geom_rect(data=forecast.df, inherit.aes=FALSE,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), 
            fill='yellow', alpha=0.5) +
  geom_line(color="royalblue") +
  labs(x="",y="",title="PMI",
       subtitle="Purchasing Managers' Index (PMI)") +
  geom_hline(yintercept=0,color="black") +
  theme(plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic",size=9),
        plot.title=element_text(face="bold",size=14)) +
  geom_hline(yintercept = 50, col = "red") 

g.slope<-
  ggplot(data=BBDD.Grafico, aes(x=Fechas,y=Slope))+
  geom_rect(data=recessions.df, inherit.aes=FALSE,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), 
            fill='lightblue', alpha=0.5) +
  geom_rect(data=forecast.df, inherit.aes=FALSE,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), 
            fill='yellow', alpha=0.5) +
  theme_minimal() +
  geom_line(color="royalblue") +
  labs(x="",y="",title="Yield curve slope",
       subtitle="10-year minus 3-month U.S. Treasury rates") +
  geom_hline(yintercept=0,color="black") +
  theme(plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic",size=9),
        plot.title=element_text(face="bold",size=14))

a <- plot_grid(g.slope,g.pmi,g.emp, nrow=3, ncol = 1)
a

# Creamos la BBDD con la data a analizar y a entrenar el modelo
Training.Set <- data.frame(data.Set$PMI,
                           data.Set$`10YB` - data.Set$`3MB`,
                           data.Set$UnEmpRate,
                           data.Set$CRISIS)

row.names(Training.Set) <- NULL
colnames(Training.Set)  <- c("PMI",
                             "Slope",
                             "UnEmpRate",
                             "CRISIS")

# Escalamos y Normalizamos la data para el Machine Learning
Training.Set[,-ncol(Training.Set)] <- as.data.frame(scale(Training.Set[,-ncol(Training.Set)]))

classifier <- glm(formula = CRISIS ~ .,
                  family = binomial,
                  data = Training.Set)

prob <- classifier[["fitted.values"]] # Valores obtenidos del modelo

# Creamos el testset para realizar el forecast
Test.Set <- data.frame(pred.PMI$mean,
                       pred.Bond10Y$upper[,2] - pred.Bond3M$upper[,2],
                       pred.UnEmpRate$mean)

colnames(Test.Set) <- c("PMI", "Slope", "UnEmpRate")
Test.Set <- as.data.frame(scale(Test.Set))

prob_pred = predict(classifier, 
                    type = 'response', 
                    newdata = Test.Set) # Predicción

total.Probabilidades <- c(prob, prob_pred)

BBDD.Prob <- data.frame(Fechas, total.Probabilidades)
colnames(BBDD.Prob) <- c("Fechas", "Prob")

# Creamos el codigo de colores
BBDD.Prob$col <- 0
for(i in 1:nrow(BBDD.Prob)){
  if(BBDD.Prob$Prob[i] < 0.20){
    BBDD.Prob$col[i] <- "green" 
  }else if(BBDD.Prob$Prob[i] > 0.20 & BBDD.Prob$Prob[i] < 0.50){
    BBDD.Prob$col[i] <- "yellow" 
  }else if(BBDD.Prob$Prob[i] > 0.50 & BBDD.Prob$Prob[i] < 1.00){
    BBDD.Prob$col[i] <- "red" 
  }
}

# Graficamos los resultados de la Prediccion
  # Buscamos el máximo valor futuro
  fila.proyeccion <- nrow(BBDD.Prob) - periodos.Proyectar
  fila.final      <- nrow(BBDD.Prob)
  
  maximo         <- max(BBDD.Prob[fila.proyeccion:fila.final,2])
  fecha.Recesion <- BBDD.Prob[BBDD.Prob$Prob == maximo, 1]

# Ajustamos la longitud de la BBDD para poder graficar
BBDD.Prob <- BBDD.Prob[1:(nrow(data.Set) + periodos.Forecast), ]

p <- ggplot(data = BBDD.Prob, aes(x = Fechas, y = Prob)) + 
      geom_line(aes(colour = col, group = 1), size = 0.9) +
      scale_colour_identity() + 
      geom_rect(data = recessions.df, inherit.aes = FALSE,
                aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
                fill = 'lightblue', alpha = 0.4) +
      theme_minimal() +
      labs(x = "", y = "", title = "Probability of a Recession in the United States",
           subtitle = paste("Based on a Machine Learning algorithm that forecast the odds of a recession.\nThe recession could start in: ", fecha.Recesion, " - Prob: ", round(maximo*100, digits = 2), "%", sep = ""),
           caption = "Created by: Carlos Jimenez (@cjimenezdiaz). \nThe variables analyzed in this study are: PMI, the Slope of the Yield Curve and the Unemployment Rate") +
      geom_hline(yintercept = 0,color = "black") +
      theme(plot.caption = element_text(hjust = 0),
            plot.subtitle = element_text(face = "italic",size = 9),
            plot.title = element_text(face = "bold", size = 14)) + theme(legend.position = "none") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(labels = scales::percent)

p

ggsave(file = "Graficos Creados/Probability-Recession.png", plot = p, dpi = 300, width = 8, height = 5)
ggsave(file = "Graficos Creados/Variables-Recession.png", plot = a, dpi = 300, width = 8, height = 5)

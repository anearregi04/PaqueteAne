
################################################################################
# En este script se presentan las funciones que se han usado durante los ejercicios del curso #
# *****************************************************************************#
# Autor/a: Ane Arregi Beitia #
# Email: aarregi015@ikasle.ehu.eus #
# Fecha: 17-09-2026 #
################################################################################# 
utils::globalVariables(c("X", "Y", "Z"))

#' Discretiza un vector numerico mediante puntos de corte 
#'
#' Divide un vector numerico en intervalos definidos por los puntos de corte. 
#'
#' @param x Vector numerico que se quiere discretizar.
#' @param cut.points Vector numerico con los puntos de corte.
#' @return Un factor que indica el intervalo al que pertenece cada valor.
#' @export
discretize <- function(x,cut.points){  #La funcion recibe un vector x y una vector con los puntos de corte que se utilizaran para crear los intervalos
  cut.points <- sort(cut.points) #Por si estan desordenados
  limites <- c(-Inf, cut.points, Inf) #Ademas de los puntos definidos, anadimos -Inf e Inf a los puntos de corte para definir los limites inferior y superior de todos los intervalos.
  x.moztuta <- cut(x,limites,labels = paste0("I", 1:(length(cut.points) + 1))) #Se utiliza la funcion cut() para asignar cada valor de x al intervalo correspondiente. Se crean n+1 intervalos, siendo n la longitud de cut.points, y se etiquetan como I1, I2, ...
  return(x.moztuta)
}



#' Discretiza las columnas numericas de un conjunto de datos 
#' 
#' Aplica la funcion discretize() a todas las columnas numericas del conjunto de datos.
#' 
#' @param datos Data frame que se quiere discretizar. 
#' @param cut.points Vector numerico con los puntos de corte. 
#' @return Un data frame con las columnas numericas discretizadas. 
#' @export
discretize.dataset <- function(datos,cut.points){
  resultado <- datos
  
  for (i in 1:ncol(datos)) {
    if (is.numeric(datos[[i]])) {
      resultado[[i]] <- discretize(datos[[i]], cut.points) #Vamos pasando a la funcion anterior todas las columnas, una a una.
    }
  }
  return(resultado)
}



#' Discretiza un vector mediante intervalos de igual amplitud 
#'  
#' Divide el rango de un vector numerico en un numero determinado de intervalos con la misma amplitud. 
#'  
#' @param x Vector numerico que se quiere discretizar. 
#' @param num.bins Numero de intervalos que se quieren crear. 
#' @return Una lista con los valores categorizados, los intervalos y los puntos de corte. 
#' @export
discretizeEW <- function(x,num.bins){  #La funcion recibe u vector x tipo numerico y un numero num.bins que indica el numero de intervalos
  luzeera <- (max(x)-min(x))/num.bins  #Calculamos la amplitud de cada rango
  
  #Creamos vectores y listas vacias que rellenaremos posteriormente
  listaI <- c(1:length(x)) 
  intervalos <- list(1:num.bins)   
  nombres.intervalos <- c(1:num.bins)
  puntos.corte <- c(1:(num.bins-1))
  
  
  for (i in 1:num.bins){
    #Calculamos los limites inferior y superiores para cada rango
    limite.inf <- min(x) + (i-1)*luzeera 
    limite.sup <- min(x) + i*luzeera
    
    intervalos[[i]]<- c(limite.inf,limite.sup) # Una lista de vectores donde cada elemento es el limite inferior y el limite superior de cada rango.
    
    #Para mejorar la legibilidad del resultado, se quieren indicar los limites correspondientes a cada intervalo. Se tienen que distingir los escenarios de las esquinas para indicar los infinitos. 
    if (i==1){
      nombres.intervalos[i] <- paste0("I", i, " = (-Inf, ", limite.sup, "]")
    }
    else if (i == num.bins) {
      nombres.intervalos[i] <- paste0("I", i, " = (", limite.inf, ", Inf)")
    }
    else{
      nombres.intervalos[i] <- paste0("I", i, "=(", limite.inf, ",", limite.sup, "]")
    }
    if(i<(num.bins)){
      puntos.corte[i] <- limite.sup
    }
  }
  
  j <- 1 #Contador que indicara en que indice de x estamos.
  for (i in x){ #Aqui estamos buscando a que rango pertenece cada elemento.
    for (contador in 1:num.bins){ 
      if (i >= intervalos[[contador]][1]  && i <= intervalos[[contador]][2]){
        listaI[j] <- paste0("I",contador)
      }
    }
    j <- j + 1
  }
  
  listaI <- factor(listaI, levels = paste0("I", 1:num.bins)) #convertimos a tipo factor
  
  #Devolvemos una lista con los valores categorizados, los intervalos y los puntos de corte.
  return(list(valores = listaI,
              intervalos = nombres.intervalos,
              puntos.corte = paste0("Puntos de corte: ", paste(puntos.corte, collapse = ", "))))   
}



#' Discretiza las columnas numericas mediante intervalos de igual amplitud 
#'  
#' Aplica discretizeEW() a todas las columnas numericas de un conjunto de datos. 
#'  
#' @param datos Data frame que se quiere discretizar. 
#' @param num.bins Numero de intervalos que se quieren crear. 
#' @return Un data frame con las columnas numericas discretizadas. 
#' @export
discretizeEW.dataset <- function(datos, num.bins) {
  resultado <- datos
  for (i in 1:ncol(datos)) {
    if (is.numeric(datos[[i]])) {
      resultado[[i]] <- discretizeEW(datos[[i]], num.bins)$valores #Vamos pasando a la funcion anterior todas las columnas, una a una.
    }
  }
  return(resultado)
}



#' Discretiza un vector mediante intervalos de igual frecuencia 
#' 
#' Ordena los valores y los divide en grupos con un numero aproximadamente igual de observaciones.
#' 
#' @param x Vector numerico que se quiere discretizar. 
#' @param num.bins Numero de intervalos que se quieren crear. 
#' @return Una lista con los valores categorizados y los grupos formados. 
#' @export
discretizeEF <- function(x, num.bins = 3) {
  x.ordenado <- order(x) #Ordenamos el vector para poder sacar mas facilmente los grupos. x.ordenado indica el indice de los valores ordenados.
  
  #En caso de que el numero de elementos sea multiplo del numero de grupos, todos los grupos tendran los mismos elementos. 
  #Esto lo podemos comprobar mirando el resto de la division. 
  tamano.v <- length(x) %/% num.bins 
  resto <- length(x) %% num.bins 
  
  #Creamos grupos vacios para poder llenarlos con los resultados. 
  resultado <- character(length(x))
  grupos <-  character(num.bins)
  
  inicio <- 1 #Contador que ira de 1 y se le ira sumando el numero de elementos que hay en cada grupo
  
  for (i in 1:num.bins) {
    
    #Si el grupo numero i es mas grande que (num.bins - resto) significa que ese grupo recibira un elemento extra.
     
    if (i > (num.bins-resto)) {
      fin <- inicio + tamano.v
    } 
    #Por ejemplo, si tenemos 7 elementos y queremos hacer 5 grupos.
    #tamano.v=7%/5=1
    #resto=7%%5 =2
    #num.bins-resto = 5-2 = 3
    #Los tres primeros grupos tendran el tamano de tamano.v (1), y los dos ultimos grupos (I4 e I5) tendran un elemento mas (2). 
    
    
    #Si el numero de elementos es multiplo del numero de grupos, resto sera 0 y todos los grupos tendran el mismo tamano.    
    else {
      fin <- inicio + tamano.v - 1  #Se resta 1 porque 'inicio' tambien cuenta como una posicion 

    }
    
    posiciones <- x.ordenado[inicio:fin]  #Seleccionamos las posiciones de x que corresponden al grupo actual, desde la posicion 'inicio' hasta la posicion 'fin'.

    resultado[posiciones] <- paste0("I", i) #Asignamos cada valor a su intervalo
    grupos[i] <- paste0("I", i, "=(",paste(x[posiciones], collapse = ", "),")") #Guardamos los valores que forman cada grupo I
    
    inicio <- fin + 1 #Ahora el nuevo inicio del siguiente grupo sera el siguiente indice del valor "fin" guardado anteriormente
  }
  
  #Devolvemos una lista con los valores categorizados y los grupos formados.
  return(list(valores = factor(resultado),
    grupos = grupos)) 
}



#' Discretiza las columnas numericas mediante intervalos de igual frecuencia 
#'  
#' Aplica discretizeEF() a todas las columnas numericas de un conjunto de datos. 
#' 
#' @param datos Data frame que se quiere discretizar. 
#' @param num.bins Numero de intervalos que se quieren crear. 
#' @return Un data frame con las columnas numericas discretizadas. 
#' @export
discretizeEF.dataset <- function(datos, num.bins = 3) { 
  resultado <- datos
  for (i in 1:ncol(datos)) {
    if (is.numeric(datos[[i]])) {
      resultado[[i]] <- discretizeEF(datos[[i]], num.bins)$valores #Vamos pasando a la funcion anterior todas las columnas, una a una.
    }
  }
  return(resultado)
}



#' Calcula la entropia de una variable categorica
#' 
#' Calcula la entropia de una variable de tipo factor a partir de sus frecuencias. 
#' 
#' @param x Variable de tipo factor. 
#' @return El valor de la entropia. 
#' @export
entropy <- function (x){
  #Comprobamos que x sea una variable de tipo factor
  if (!is.factor(x)){
    stop("x debe ser una variable discretizada de tipo factor")
  }
  frecuencias <- table(x) #Table nos cuenta las veces que aparece cada variable
  probabilidades <- frecuencias / length(x) #Calculamos las probabilidades
  H <- -sum(probabilidades * log2(probabilidades)) #Aplicamos la formula de la entropia
  return(H)
}

#' Calcula la varianza de un vector numerico 
#' 
#' Calcula la varianza muestral de un vector numerico 
#' 
#' @param x Vector numerico 
#' @return El valor de la varianza. 
#' @export
varianza <- function(x){
  #Aplicamos la formula para la varianza
  n <- length(x)
  media <- mean(x)
  suma <- 0
  for (i in 1:n){
    suma <- suma + (x[i] - media)^2
  }
  return((1/(n-1) * suma))
}



#' Calcula un punto de la curva ROC 
#' 
#' Calcula la tasa de verdaderos positivos y la tasa de falsos positivos para un determinado punto de corte. 
#' 
#' @param datos Data frame que contiene las columnas valor y etiqueta. 
#' @param num.corte Punto de corte utilizado para realizar la clasificacion. 
#' @return Un vector con los valores FPR y TPR. 
#' @export
calcular_punto_ROC <- function(datos, num.corte) {
  predicho <- ifelse(datos$valor < num.corte, FALSE, TRUE) #Le pasamos un numero de corte. Si el valor en datos es mas pequeno que eso, es FALSE, sino TRUE. 
  
  #Aplicamos las formulas
  TP <- sum(predicho == TRUE & datos$etiqueta == TRUE)
  FP <- sum(predicho == TRUE & datos$etiqueta == FALSE)
  TN <- sum(predicho == FALSE & datos$etiqueta == FALSE)
  FN <- sum(predicho == FALSE & datos$etiqueta == TRUE)
  
  TPR <- TP / (TP + FN)
  FPR <- FP / (FP + TN)
  
  return(c(FPR = FPR, TPR = TPR))
}



#' Calcula una integral mediante la regla del trapecio 
#'  
#' Realiza una aproximacion numerica de una integral utilizando la regla del trapecio. 
#'  
#' @param x Vector con los valores del eje x. 
#' @param y Vector con los valores del eje y. 
#' @return Una aproximacion numerica de la integral. 
#' @export
integraOptimizada <- function(x, y) {#Esta funcion calcula una aproximacion numerica de una integral usando la regla del trapecio

  delta.x <- diff(x)
  mean.y <- rowMeans(cbind(y[-1], y[-length(y)]))
  return(sum(delta.x * mean.y))
}



#' Calcula el area bajo la curva ROC 
#'  
#' Calcula una aproximacion del area bajo la curva ROC utilizando diferentes puntos de corte. 
#' 
#' @param x Vector numerico con los valores de la variable predictora. 
#' @param clase Vector logico con las clases reales. 
#' @return Una aproximacion del area bajo la curva ROC. 
#' @export
AUC <- function(x, clase) {
  
  datos <- data.frame(valor = x,etiqueta = clase) 
  cortes <- seq(min(x, na.rm = TRUE),max(x, na.rm = TRUE),length.out = 100) #Para calcular la curva ROC hay que ir cambiando los puntos de corte
  puntos <- data.frame(FPR = numeric(), TPR = numeric()) #Creamos un dataset vacio
  
  for (corte in cortes) {
    punto <- calcular_punto_ROC(datos, corte) #Para cada punto de corte calculamos el punto 
    puntos <- rbind(puntos,data.frame(FPR = punto["FPR"],TPR = punto["TPR"]))
  }
  
  #Para utilizar la funcion integraOptimizada los puntos tienen que estar ordenados. 
  puntos.ordenados <- puntos[order(puntos$FPR), ] 
  
  FPR <- puntos.ordenados$FPR
  TPR <- puntos.ordenados$TPR
  auc <- integraOptimizada(FPR, TPR) 
  
  return(auc)
}



#' Calcula diferentes metricas para las variables de un conjunto de datos 
#'  
#' Calcula la varianza y el AUC para variables numericas y la entropia para variables categoricas. 
#' 
#' @param dataset Data frame que contiene las variables que se quieren analizar.
#' @param clase Nombre de la variable que contiene la clase. 
#' @return Un data frame con las metricas calculadas para cada variable. 
#' @export
calcular_metricas <- function(dataset, clase) {
  resultados <- data.frame()
  for (nombre in names(dataset)) {
    
    #Saltamos la variable clase porque este solamente es para comparar
    if (nombre == clase) {
      next
    }
    
    x <- dataset[[nombre]] #Accedemos a cada columna individualmente
    
    if (is.factor(x)) { #Si esa columna es de tipo factor calculamos la entropia
      
      resultado <- data.frame(variable = nombre,tipo = "discreta",varianza = NA,AUC = NA,entropia = entropy(x))
      
    } 
    if (is.numeric(x)){ # Si la columna es numerica calculamos la varianza y AUC
      
      resultado <- data.frame(variable = nombre,tipo = "continua",varianza = varianza(x),AUC = AUC(x, dataset[[clase]]),entropia = NA)
    }
    else{
      resultado <- data.frame(variable = nombre,tipo = "-",varianza = NA ,AUC = NA ,entropia = NA)
    }
    
    resultados <- rbind(resultados, resultado)
  }
  
  return(resultados)
}



#' Normaliza un vector numerico 
#'  
#' Transforma los valores de un vector numerico al intervalo entre 0 y 1. 
#'  
#' @param x Vector numerico que se quiere normalizar. 
#' @return El vector normalizado. 
#' @export
normalizar <- function(x){
  x_norm <- (x-min(x))/(max(x)-min(x)) #Aplicamos la formula para normalizar
  return (x_norm)
}



#' Normaliza las columnas numericas de un conjunto de datos 
#'  
#' Aplica la funcion normalizar() a todas las columnas numericas.
#'  
#' @param datos Data frame que se quiere normalizar. 
#' @return Un data frame con las columnas numericas normalizadas. 
#' @export
normalizar.dataset <- function(datos){
  resultado <- datos
  
  for (i in 1:ncol(datos)) {
    if (is.numeric(datos[[i]])) {
      resultado[[i]] <- normalizar(datos[[i]]) #Aplicamos la funcion normalizar para cada valor numerico del dataset
    }
  }
  
  return(resultado)
}



#' Estandariza un vector numerico 
#' 
#' Estandariza un vector utilizando su media y desviacion estandar.
#'  
#' @param x Vector numerico que se quiere estandarizar. 
#' @return El vector estandarizado. 
#' @export
estandarizar <- function(x){
  varianza.x <- varianza(x)
  media <- sum(x)/length(x)
  x_estand <-(x-media)/varianza.x #Aplicamos la formula para estandarizar
  return(x_estand)
}



#' Estandariza las columnas numericas de un conjunto de datos 
#'  
#' Aplica la funcion estandarizar() a todas las columnas numericas. 
#' 
#' @param datos Data frame que se quiere estandarizar. 
#' @return Un data frame con las columnas numericas estandarizadas. 
#' @export
estandarizar.dataset <- function(datos){
  resultado <- datos
  
  for (i in 1:ncol(datos)) {
    if (is.numeric(datos[[i]])) { #comprobamos que es numerico
      resultado[[i]] <- estandarizar(datos[[i]]) #Aplicamos la funcion estandarizar para cada valor numerico del dataset
    }
  }
  
  return(resultado)
}



#' Filtra variables segun diferentes metricas 
#' 
#' Selecciona las variables que superan los umbrales establecidos para la varianza, el AUC y la entropia. 
#'  
#' @param data Data frame que contiene las metricas calculadas. 
#' @param umbral_varianza Umbral minimo de varianza para las variables continuas. 
#' @param umbral_auc Umbral minimo de AUC para las variables continuas. 
#' @param umbral_entropia Umbral minimo de entropia para las variables discretas. 
#' @return Un vector con los nombres de las variables seleccionadas. 
#' @export
filtrado_variables<- function(data, umbral_varianza,umbral_auc, umbral_entropia){
  variables <- data$variable[
    (data$tipo == "continua" & data$varianza > umbral_varianza & data$AUC > umbral_auc) | #Guarda los valores continuos que superan el umbral
      (data$tipo == "discreta" & data$entropia > umbral_entropia)]
  return(variables)
}



#' Calcula la correlacion o la informacion mutua entre dos variables 
#'  
#' Calcula la correlacion de Pearson si ambas variables son numericas y la informacion mutua si ambas son factores.
#'  
#' @param x Primera variable. 
#' @param y Segunda variable. 
#' @return La correlacion de Pearson, la informacion mutua o NA si los tipos son mixtos. 
#' @export
cor_par <- function(x, y) { #Teniendo dos variables nos calcula la correlacion o la informacion mutua
  #Variables numericas
  if (is.numeric(x) && is.numeric(y)) {
    #Comprobar que tienen la misma longitud
    if (length(x) != length(y)) {
      stop("las dos columnas deben tener la misma longitud")
    }
    
    #Medias
    media_x <- mean(x)
    media_y <- mean(y)
    
    #Calculamos la correlacion de Pearson
    numerador <- sum((x - media_x) * (y - media_y))
    denominador <- sqrt(sum((x - media_x)^2) *sum((y - media_y)^2))
    r <- numerador / denominador
    return(r)
    }
  
  #Variables categoricas
  if (is.factor(x) && is.factor(y)) {
    
    tabla <- table(x, y)
    pxy <- tabla / sum(tabla)
    px <- rowSums(pxy)
    py <- colSums(pxy)
    
    mi <- 0
    
    for (i in 1:nrow(pxy)) {
      for (j in 1:ncol(pxy)) {
        if (pxy[i, j] > 0) {
          mi <- mi + pxy[i, j] * 
            log(pxy[i, j] / (px[i] * py[j]))
        }
      }
    }
    return(mi)
  }
  return(NA) #Para los pares numerico-categorico devolvemos NA
}



#' Calcula la correlacion o informacion mutua entre todas las parejas de variables
#'  
#' Calcula una matriz aplicando cor_par() a cada pareja de variables. 
#'  
#' @param datos Data frame cuyas variables se quieren comparar. 
#' @return Una matriz con las correlaciones o informaciones mutuas. 
#' @export
cor_dataset <- function(datos) {
  n <- ncol(datos)
  resultado <- matrix(NA,nrow = n,ncol = n,dimnames = list(names(datos), names(datos))) #Creamos una matriz vacia donde vamos a meter los resultados

  for (i in 1:n) {
    for (j in 1:n) {
      resultado[i, j] <- cor_par(datos[[i]], datos[[j]]) #Para cada par, calculamos la correlacion utilizando la funcion cor_par
      }
  }
  return(resultado)
}



#' Representa graficamente una curva ROC 
#' 
#' Calcula diferentes puntos de una curva ROC y los representa graficamente. 
#' 
#' @param x Vector numerico con los valores de la variable predictora. 
#' @param clase Vector logico con las clases reales. 
#' @return No devuelve ningun objeto; genera una representacion grafica de la curva ROC. 
#' @export
#' @importFrom graphics abline
grafica_ROC <- function(x, clase) {
  #Lo del principio es lo mismo que la funcion AUC()
  datos <- data.frame(valor = x, etiqueta = clase)
  cortes <- seq(min(x, na.rm = TRUE),max(x, na.rm = TRUE),length.out = 100)
  puntos <- data.frame(FPR = numeric(), TPR = numeric())
  
  for (corte in cortes) {
    punto <- calcular_punto_ROC(datos, corte)
    puntos <- rbind(puntos,data.frame(FPR = punto["FPR"],TPR = punto["TPR"]))
  }
  puntos.ordenados <- puntos[order(puntos$FPR), ]

  #Cuando ordenamos los puntos, graficamos: 
  plot(puntos.ordenados$FPR,puntos.ordenados$TPR,type = "l",xlim = c(0, 1),ylim = c(0, 1),xlab = "FPR",ylab = "TPR",main = "Curva ROC")
  
  abline(a = 0,b = 1,lty = 2)
}



#' Representa graficamente una matriz de correlaciones 
#' 
#' Calcula la matriz de correlaciones o informacion mutua y la representa mediante un mapa de calor. 
#' 
#' @param datos Data frame cuyas variables se quieren comparar.
#' @return Un grafico de tipo mapa de calor. 
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2 labs theme_minimal theme element_text element_blank 
plot_correlaciones <- function(datos) {
  #Sacamos la matriz de correlaciones utilizando la funcion cor_dataset
  matriz_cor <- cor_dataset(datos)
  
  #Convertimos la matriz a formato largo y anadimos los valores de la matriz 
  datos_heatmap <- expand.grid(X = rownames(matriz_cor), Y = colnames(matriz_cor))
  datos_heatmap$Z <- as.vector(matriz_cor)
  
  #Heatmap, codigo ogido desde R Graph Gallery
  ggplot(datos_heatmap, aes(x = X, y = Y, fill = Z)) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(is.na(Z), "", round(Z, 2)))) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      na.value = "grey90"
    ) +
    labs(x = "",y = "",fill = "Valor",title = "Matriz de correlaciones / informacion mutua"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}
getwd()

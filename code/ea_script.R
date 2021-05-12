#CARGAR DATOS
hotel.booking = read.csv("./data/hotel_bookings.csv", header = TRUE, stringsAsFactors= FALSE, sep = ",")

#INSPECCIONAR DATOS

#Ver todos los datos del dataframe
View(hotel.booking)

#Ver las 5 primeros filas del dataframe
head(hotel.booking, 5)

#Ver las 5 ultimas filas del dataframe
tail(hotel.booking, 5)

#Ver nombres de las columnas del dataframe
names(hotel.booking)

#Al revisar los datos nos damos cuenta que existe mÃºltiples valores que funcionarÃ­an mejor como factor.
str(hotel.booking)

#Con Summary corroboramos que hay datos numÃ©ricos que estan cumpliendo la funciÃ³n de factor y deben ser corregidos, ademÃ¡s
#se nota la presencia de algunos NAs en Children, sin embargo la limpieza necesaria se harÃ¡ luego.
summary(hotel.booking)

hotel.booking$is_canceled = as.factor(hotel.booking$is_canceled)
hotel.booking$arrival_date_year = as.factor(hotel.booking$arrival_date_year)
hotel.booking$arrival_date_week_number = as.factor(hotel.booking$arrival_date_week_number)
hotel.booking$arrival_date_day_of_month = as.factor(hotel.booking$arrival_date_day_of_month)
hotel.booking$is_repeated_guest = as.factor(hotel.booking$is_repeated_guest)
hotel.booking$hotel = as.factor(hotel.booking$hotel)
hotel.booking$arrival_date_month = as.factor(hotel.booking$arrival_date_month)
hotel.booking$meal = as.factor(hotel.booking$meal)
hotel.booking$country = as.factor(hotel.booking$country)
hotel.booking$market_segment = as.factor(hotel.booking$market_segment)
hotel.booking$distribution_channel = as.factor(hotel.booking$distribution_channel)
hotel.booking$reserved_room_type = as.factor(hotel.booking$reserved_room_type)
hotel.booking$assigned_room_type = as.factor(hotel.booking$assigned_room_type)
hotel.booking$deposit_type = as.factor(hotel.booking$deposit_type)
hotel.booking$agent = as.factor(hotel.booking$agent)
hotel.booking$company = as.factor(hotel.booking$company)
hotel.booking$customer_type = as.factor(hotel.booking$customer_type)
hotel.booking$reservation_status = as.factor(hotel.booking$reservation_status)
hotel.booking$reservation_status_date = as.factor(hotel.booking$reservation_status_date)

#Se convirtieron estas variables a Factor y se realizÃ³ una verificaciÃ³n posterior
str(hotel.booking)

#Finalmente comprobamos que los datos estÃ¡n correctos en cuanto y podemos proceder con el anÃ¡lisis y selecciÃ³n
#de visualizaciones.
summary(hotel.booking)


#LIMPIEZA DE DATOS

#Limpiar NAs

#Con summary identificamos que columnas presentan valores NAs
summary(hotel.booking)

#En este caso vemos que children tiene 4 valores NAs.
#Consideramos que lo mejor serÃ­a reemplazar esos valores por 0, esto ya que lo mÃ¡s probable es que no se haya colocado un valor
#debido a que no habÃ­an niÃ±os.
hotel.booking.limpio <- hotel.booking
hotel.booking.limpio$children[is.na(hotel.booking.limpio$children)] <- 0

#Revisamos si los NAs fueron eliminados
hotel.booking.limpio$children[is.na(hotel.booking.limpio$children)]

#Corregir outliers

#Revisamos cada columna numÃ©rica para verificar la presencia de outliers
boxplot(hotel.booking.limpio$lead_time)$out
boxplot(hotel.booking.limpio$stays_in_weekend_nights)$out
boxplot(hotel.booking.limpio$stays_in_week_nights)$out
boxplot(hotel.booking.limpio$adults)$out
boxplot(hotel.booking.limpio$children)$out
boxplot(hotel.booking.limpio$babies)$out
boxplot(hotel.booking.limpio$previous_cancellations)$out
boxplot(hotel.booking.limpio$previous_bookings_not_canceled)$out
boxplot(hotel.booking.limpio$booking_changes)$out
boxplot(hotel.booking.limpio$days_in_waiting_list)$out
boxplot(hotel.booking.limpio$adr)$out
boxplot(hotel.booking.limpio$required_car_parking_spaces)$out
boxplot(hotel.booking.limpio$total_of_special_requests)$out

#En adr vemos que existe un valor negativo lo cual es imposible, por lo que en este caso lo reemplazaremos por 
#el mismo valor en positivo
hotel.booking.limpio$adr[hotel.booking.limpio$adr < 0] <- 6.38

#Luego hacemos una revisiÃ³n de los datos
View(hotel.booking.limpio)

#Utilizamos la funciÃ³n aprendida en clase para corregir la mayorÃ­a de outliers
replace_outliers <-function(x, removeNA = TRUE){
  qrts <-quantile(x, probs = c(0.25, 0.75), na.rm = removeNA)
  #si el outlier esta por debajo del cuartil 0.5 o por arriba de 0.95
  caps <-quantile(x, probs = c(.05, .95), na.rm = removeNA)
  #Calculamos el rango intercuartilico
  iqr <-qrts[2]-qrts[1]
  #Calculamos el 1.5 veces el rango intercuartiligo (iqr)
  altura <-1.5*iqr
  #reemplazamos del vector los outliers por debajo de 0.05 y 0.095
  x[x<qrts[1]-altura] <-caps[1]
  x[x>qrts[2]+altura] <-caps[2]
  x
}

#Lo aplicamos a cada columna que presentaba outliers en los que sabemos que va a funcionar
hotel.booking.limpio$lead_time <- replace_outliers(hotel.booking.limpio$lead_time)
hotel.booking.limpio$stays_in_weekend_nights <- replace_outliers(hotel.booking.limpio$stays_in_weekend_nights)
hotel.booking.limpio$stays_in_week_nights <- replace_outliers(hotel.booking.limpio$stays_in_week_nights)
hotel.booking.limpio$adr <- replace_outliers(hotel.booking.limpio$adr)
hotel.booking.limpio$total_of_special_requests <- replace_outliers(hotel.booking.limpio$total_of_special_requests)

summary(hotel.booking)
summary(hotel.booking.limpio)

#Revisamos cada columna manualmente y hacemos correciones de outliers
table(hotel.booking.limpio$required_car_parking_spaces)
hotel.booking.limpio$required_car_parking_spaces[hotel.booking.limpio$required_car_parking_spaces > 1] <- 1

table(hotel.booking.limpio$adults)
hotel.booking.limpio$adults[hotel.booking.limpio$adults > 3] <- 3

table(hotel.booking.limpio$children)
hotel.booking.limpio$children[hotel.booking.limpio$children > 2] <- 2

table(hotel.booking.limpio$babies)
hotel.booking.limpio$babies[hotel.booking.limpio$babies > 1] <- 1

table(hotel.booking.limpio$previous_cancellations)
hotel.booking.limpio$previous_cancellations[hotel.booking.limpio$previous_cancellations > 1] <- 1

table(hotel.booking.limpio$booking_changes)
hotel.booking.limpio$booking_changes[hotel.booking.limpio$booking_changes > 2] <- 2

table(hotel.booking.limpio$previous_bookings_not_canceled)
hotel.booking.limpio$previous_bookings_not_canceled[hotel.booking.limpio$previous_bookings_not_canceled > 1] <- 1

table(hotel.booking.limpio$days_in_waiting_list)
hotel.booking.limpio$days_in_waiting_list[hotel.booking.limpio$days_in_waiting_list > 1] <- 1

#En la mayorÃ­a de los casos a pesar de agrupar lo datos no se elminaron por completo los outliers, esto debido a la gran
#concentraciÃ³n en valores como el 0, que para eliminar outliers conllevarÃ­a pasar todos los valores a 0
#lo cual consideramos no serÃ­a lo mÃ¡s Ã³ptimo para el anÃ¡lisis.


#VISUALIZACIONES

#GrÃ¡ficas propuestas de la TA 1
counts = table(hotel.booking$customer_type, hotel.booking$reserved_room_type)
barplot(counts, col=c("green","yellow","blue","red"), legend = c("Contrato", "Grupo", "Transitorio", "Transitorio asociado"), main = "Tipo de cliente por habitación reservada")
table(hotel.booking$customer_type, hotel.booking$reserved_room_type)
#Esta grÃ¡fica nos ayuda a verificar dos cosas, primero cuales son los tipos de habitaciÃ³n mÃ¡s reservados en relaciÃ³n con el tipo de cliente, vemos que en todos los tipos de habitaciÃ³n
#predomina el transitorio, ademÃ¡s nos permite conocer los tipos de habitaciÃ³n mÃ¡s solicitadas en las reservas, notando una clara victoria por parte del tipo A seguido del tipo D.

counts2 = table(hotel.booking$reservation_status, hotel.booking$deposit_type)
barplot(counts2, col=c("blue","red","green"), legend = c("Cancelado", "Check-out", "No se muestra"), main = "Estado de la reservación por el tipo de deposito")
table(hotel.booking$reservation_status, hotel.booking$deposit_type)
#Esta grÃ¡fica nos ayuda a revisar por el estado de la reservaciÃ³n que tipo de deposito predomina y notamos algo increible, en el caso de los que no hacen ningÃºn depÃ³sito la gran mayorÃ­a
#termina completando su estadÃ­a y retirandose, sin embargo en los realizan un depÃ³sito completo que no tiene lugar a devoluciones se ve que predomina por mucho la cantidad de clientes
#que cancelaron.

counts3 =  table(hotel.booking$is_canceled, hotel.booking$hotel)
barplot(counts3, col=c("blue","red"), legend = c("Continuaron", "Cancelado"), main = "Cancelacion de reserva por tipo de hotel")
table(hotel.booking$is_canceled, hotel.booking$hotel)
#Esta grafica nos permite ver en tipo de hotel hay mas cancelaciones.

table(hotel.booking$arrival_date_month)
barplot(table(hotel.booking$arrival_date_month), main = "Reservas totales por mes", names= c("April", "August", "December", "February", "January", "July", "June", "March", "May", "November", "October", "September"))
#Esta grafica nos ayuda a ver el volumen de usuarios por cada mes y de esta manera estar mas preparados para cuando los meses de mayor afluencia lleguen.

counts4=table(hotel.booking$is_repeated_guest,hotel.booking$market_segment)
barplot(counts4,col = c("blue","green"),legend=c("No Constante","Constante"),main="Clientes frecuentes por origen de la reserva")
table(hotel.booking$is_repeated_guest,hotel.booking$market_segment)
# Esta grafica permite observar que tipo de segmento de mercado es mas constante o repetitivo(siendo en este caso con un mayor porcentaje el corporativo).


#Preguntas del Examen Parcial

counts5 = table(hotel.booking$is_canceled, hotel.booking$hotel)
counts5 = data.frame(counts5)
ggplot(counts5, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(position = 'dodge',stat='identity') +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Número de reservas canceladas y no canceladas por hotel") +
  xlab("Hotel") + ylab("# Reservas") + labs(fill = "Cancelado (0=Si, 1=No)")
#a

ggplot(counts6, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(position = 'dodge',stat='identity') +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Número de reservas por mes y año") +
  xlab("Año") + ylab("# Reservas") + labs(fill = "Mes")
#b


counts7 = table(hotel.booking.limpio$arrival_date_year[hotel.booking.limpio$arrival_date_year == '2016'], hotel.booking.limpio$arrival_date_month[hotel.booking.limpio$arrival_date_year == '2016'])
barplot(counts7, main = "Reservas por mes en el año 2016")
#c , d

hotel.booking.analisis <- hotel.booking.limpio
hotel.booking.analisis$with_kids[hotel.booking.limpio$children > 0 | hotel.booking$babies > 0] <- 1
hotel.booking.analisis$with_kids[hotel.booking.limpio$children == 0 & hotel.booking$babies == 0] <- 0
hotel.booking.analisis$with_kids <- as.factor(hotel.booking.analisis$with_kids)
summary(hotel.booking.analisis)
counts8 = table(hotel.booking.analisis$with_kids, hotel.booking.analisis$arrival_date_year)
barplot(counts8, col=c("blue","red"), legend = c("Sin niños","Con niños"), main = "Reservas que incluyan niños y/o bebés")
#e

hotel.booking.analisis$with_parking[hotel.booking.limpio$required_car_parking_spaces > 0] <- 1
hotel.booking.analisis$with_parking[hotel.booking.limpio$required_car_parking_spaces == 0] <- 0
hotel.booking.analisis$with_parking <- as.factor(hotel.booking.analisis$with_parking)
summary(hotel.booking.analisis)
counts9 = table(hotel.booking.analisis$with_parking, hotel.booking.analisis$arrival_date_year)
barplot(counts9, col=c("blue","red"), legend = c("Sin parqueo","Con parqueo"), main = "Reservas que requerían espacio para auto")
#f


counts10 = table(hotel.booking.limpio$is_canceled, hotel.booking.limpio$arrival_date_month)
barplot(counts10, col=c("blue","red"), legend = c("Cancelada","No cancelada"), main = "Cantidad de reservas cancenladas por mes")
#g

#GENERAR .CSV

write.csv(hotel.booking.limpio,'./data/hotel_bookings_limpio.csv', na="NA",row.names=FALSE)

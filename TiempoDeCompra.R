      ### Tiempo de Compra ###
### Roger Cristobal y Marc l√≥pez ###

#### Carga de librerias y paquetes ####
install.packages("shiny")
install.packages("dplyr")
if(!require("tidyverse")) {
  install.packages("tidyverse", repos="https://cloud.r-project.org/",
                   quiet=TRUE, type="binary")
  library("tidyverse")
}
install.packages("chron")
install.packages("plotly")
install.packages("forcats")

library(shiny)
library(dplyr)
library(chron)
library(plotly)
library(forcats)

#### Lectura de datos ####
TrainData <- read.table("train_trips.csv", sep=",", dec=".", quote = "\"'",
                        header=TRUE, skip = 0, na.strings = "NA")
OrderData <- read.table("order_items.csv", sep=",", dec=".", quote = "\"'",
                        header=TRUE, skip = 0, na.strings = "NA")
TestData <- read.table("datasets_70809_149763_test_trips.csv", sep=",", dec=".", quote = "\"'",
                       header=TRUE, skip = 0, na.strings = "NA")

## Transformamos los datos de formato ##
TrainData$shopping_started_at <- as.POSIXct(TrainData$shopping_started_at)
TrainData$shopping_ended_at <- as.POSIXct(TrainData$shopping_ended_at)
TrainData$DayOfWeek <- weekdays.POSIXt(TrainData$shopping_started_at)
# TrainData$Difference <- TrainData$shopping_ended_at - TrainData$shopping_started_at

# 

resumOrder <- OrderData %>%
  group_by(trip_id) %>%
  summarise(TotalQuantity = sum(quantity)
            # ,num_dept_visited = sum(count(OrderData, wt = OrderData$department_name))
  )

resumTrain <- TrainData %>%
  group_by(trip_id, store_id) %>%
  summarise(shopping_time = difftime(shopping_ended_at,shopping_started_at, units = "mins"),
            DayWeek = DayOfWeek)

allTrainData <- merge(resumOrder,resumTrain, by="trip_id")

#Barplot de temps mig de compra per tenda
data1 <- allTrainData %>%
  group_by(store_id) %>%
  summarise(meanTime = mean(shopping_time)
  )
p <- barplot(height = as.numeric(data1$meanTime), names = data1$store_id,
             xlab = "Tienda",
             ylab = "Tiempo medio de compra",
             main = "Tiempo medio empleado para comprar en una tienda"
             )
text(p, as.numeric(data1$meanTime)+1.2, as.character(round(data1$meanTime, digits = 2)), cex = 1)


#Barplot de temps de compra per dia
data2 <- allTrainData %>%
  group_by(DayWeek) %>%
  summarise(shop_time = sum(shopping_time)
  )

p <- barplot(height = as.numeric(data2$shop_time), names = data2$DayWeek,
             xlab = "DÌa de la semana",
             ylab = "Tiempo total de compra",
             main = "Tiempo total empleado seg˙n el dia de la semana"
             ) 
text(p, as.numeric(data2$shop_time)+0.5e5, as.character(data2$shop_time), cex = 1)

allTrainData$TimePerProduct <- as.numeric(allTrainData$shopping_time/allTrainData$TotalQuantity)

shapiro.test(allTrainData$TimePerProduct[1:4000])
p0 <- ggplot(allTrainData,
             aes(x = TotalQuantity ,y = TimePerProduct)) + geom_line()
ggplotly(p0)

qqline()

ggplot(allTrainData, aes(sample = TimePerProduct)) + geom_qq() + geom_qq_line() + theme_classic()



# Reorder following the value of another column:
#data2 %>%
 # mutate(name = fct_reorder(DayWeek, desc(shop_time))) %>%
  #mutate(name = fct_relevel(DayWeek, 
                #            "lunes", "martes", "miÈrcoles", 
               #             "jueves", "viernes", "s·bado", 
                #            "domingo")) %>%
#arrange(shop_time) %>%
  #mutate(name = factor(DayWeek, levels=c("lunes", "martes", "miÈrcoles", 
               #                          "jueves", "viernes", "s·bado", 
               #                          "domingo"))) %>%
  #ggplot( aes(x=shop_time, y=DayWeek)) +
  #geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  #coord_flip() +
  #xlab("") +
  #theme_bw()

#p <- data2 %>%
  #mutate(name = fct_relevel(DayWeek, 
    #                        "lunes", "martes", "miÈrcoles", 
     #                       "jueves", "viernes", "s·bado", 
    #                        "domingo")) %>%
 # ggplot( aes(x=DayWeek, y=shop_time)) +
  #geom_bar(stat="identity") +
 # xlab("")



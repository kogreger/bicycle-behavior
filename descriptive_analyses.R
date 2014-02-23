#
#  Descriptive analyses of the PFlow data, aimed at more detailed analyses of 
#  the bicycle usage behavior.
#
#  Author: Konstantin Greger
#  Date:   23 Feb 2014
#

# initialization
#install.packages("ggplot2")
#install.packages("RPostgreSQL")
library(ggplot2)
library(RPostgreSQL)

# variable declaration
transportationModesJA <- factor(c("徒歩", "自転車", "原動機付自転車", "自動二輪車", "タクシー", "乗用車", "軽乗用車", "貨物自動車", "自家用バス", "路線バス", "モノレール・新交通", "鉄道・地下鉄", "停滞"))
transportationModesEN <- factor(c("walking", "bicycle", "moped", "motorcycle", "taxi", "car", "minivan", "truck", "private bus", "public bus", "monorail", "train, subway", "stationarity"))

# prepare and establish connection to PostgreSQL database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, 
                 host = "127.0.0.1", 
                 port = 5432, 
                 dbname = "maindb", 
                 user = "postgres", 
                 password = "postgres"
                 )

# read number of subtrips per transportation mode
rs <- dbSendQuery(con, 
                  "SELECT means AS mode, COUNT(*) AS subtrips FROM tky08.subtrip GROUP BY means ORDER BY means"
                  )
dfSubtripsPerTransportationMode <- fetch(rs, n = -1)
dbClearResult(rs)
dfSubtripsPerTransportationMode$mode <- factor(dfSubtripsPerTransportationMode$mode, 
                                               labels = transportationModesEN
                                               )
dfSubtripsPerTransportationMode$mode <- factor(dfSubtripsPerTransportationMode$mode, 
                                               levels = rev(levels(dfSubtripsPerTransportationMode$mode))
                                               )
pSubtripsPerTransportationMode <- ggplot(dfSubtripsPerTransportationMode, aes(mode, subtrips))
pSubtripsPerTransportationMode <- pSubtripsPerTransportationMode + layer(
    geom = "bar", 
    stat = "identity"
    ) +
    coord_flip()
pSubtripsPerTransportationMode

# clean up
dbDisconnect(con)
dbUnloadDriver(drv)
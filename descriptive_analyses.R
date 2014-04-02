#
#  Descriptive analyses of the PFlow data, aimed at more detailed analyses of 
#  the bicycle usage behavior.
#
#  Author: Konstantin Greger
#  Date:   23 Mar 2014
#

# initialization
library(ggplot2)
library(RColorBrewer)
library(RPostgreSQL)
library(scales)

# custom theme
old_theme <- theme_update(plot.background = element_blank(), 
                          panel.background = element_blank(), 
                          panel.grid.major = element_line(color = "#CCCCCC"), 
                          panel.grid.minor = element_line(color = "#CCCCCC", linetype = "dotted"), 
                          axis.line = element_line(color = "#000000"), 
                          axis.text.x = element_text(color = "#000000"), 
                          axis.text.y = element_text(color = "#000000", hjust = 1), 
                          axis.title.x = element_text(color = "#000000", face = "bold"), 
                          axis.title.y = element_text(color = "#000000", face = "bold", angle = 90)
                          )

# variable declaration
#ageGroupsJP <- factor(c("0歳以上5歳未満", "5歳以上10歳未満", "10 歳以上 15 歳未満", "15 歳以上 20 歳未満", "20 歳以上 25 歳未満", "25 歳以上 30 歳未満", "30 歳以上 35 歳未満", "35 歳以上 40 歳未満", "40 歳以上 45 歳未満", "45 歳以上 50 歳未満", "50 歳以上 55 歳未満", "55 歳以上 60 歳未満", "60 歳以上 65 歳未満", "65 歳以上 70 歳未満", "70 歳以上 75 歳未満", "75 歳以上 80 歳未満", "80 歳以上 85 歳未満", "85 歳以上"))
#ageGroupsEN <- factor(c("under 4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85 years and older"))
#gendersJP <- factor(c("男性", "女性", "不明"))
#gendersEN <- factor(c("male", "female", "n/a"))
#transportationModesJP <- factor(c("徒歩", "自転車", "原動機付自転車", "自動二輪車", "タクシー", "乗用車", "軽乗用車", "貨物自動車", "自家用バス", "路線バス", "モノレール・新交通", "鉄道・地下鉄", "停滞"))
transportationModesEN <- factor(c("walking", "bicycle", "moped", "motorcycle", "taxi", "car", "minivan", "truck", "private bus", "public bus", "monorail", "train, subway", "stationarity"))
#groupedTransportationModesJP <- factor(c("徒歩", "自転車", "自動二輪車", "タクシー", "乗用車", "貨物自動車", "バス", "鉄道", "停滞"))
groupedTransportationModesEN <- factor(c("walking", "bicycle", "motorcycle", "taxi", "car", "truck", "bus", "train", "stationarity"))
#occupationsJP <- factor(c("農林水産業従事者", "生産工程・労務作業者", "販売従事者", "サービス職業従事者", "運輸・通信従事者", "保安職業従事者", "事務従事者", "専門的・技術的職業従事者", "管理的職業従事者", "その他職業", "園児・小学生・中学", "高校生", "大学生・短大生・各種専門学校生", "主婦・主夫(職業従事者を除く)", "無職", "その他", "不明"))
#occupationsEN <- factor(c("agriculture, forestry & fishery", "production", "sales", "service", "transportation & communication", "security", "clerical", "professional & technical", "management", "other occupation", "kindergarten, elementary & junior high school", "senior high school", "university, college & vocational school", "housewife, househusband", "unemployed, retired", "other", "n/a"))
#tripPurposesJP <- factor(c("勤務先へ(帰社を含む)", "通学先へ(帰校を含む)", "自宅へ", "買物へ", "食事・社交・娯楽へ(日常生活圏内)", "観光・行楽・レジャーへ(日常生活圏外)", "通院", "送迎", "その他の私用へ", "販売・配達・仕入・購入先へ", "打合せ・会議・集金・往診へ", "作業・修理へ", "農林漁業作業へ", "その他の業務へ", "その他"))
#tripPurposesEN <- factor(c("to work", "to school", "to home", "for shopping", "for dining, entertainment", "for sightseeing, leisure", "for hospital visit", "pick-up, sending-off", "for other personal errand", "for work-related delivery, purchase", "for work-related meeting", "for work-related operation", "for agriculture-related work", "for other work-related activity", "other"))

# prepare and establish connection to PostgreSQL database
drv <- dbDriver("PostgreSQL")
# local connection string
con <- dbConnect(drv, 
                 host = "127.0.0.1", 
                 port = 5432, 
                 dbname = "maindb", 
                 user = "postgres", 
                 password = "postgres"
                 )
# remote connection string
# sh: ssh -L 1111:localhost:5432 konstantingreger@imacmny.no-ip.org
# (cf. http://stackoverflow.com/questions/16835761/postgresql-and-ssh-tunnel)
# test: ssh -f -N -L 3333:foo.com:5432 joe@foo.com
# (cf. http://stackoverflow.com/questions/19433895/postgres-tunneling-ssh-l-in-background?rq=1)
# con <- dbConnect(drv, 
#                  host = "localhost", 
#                  port = 1111, 
#                  dbname = "maindb", 
#                  user = "postgres", 
#                  password = "postgres"
#                  )


# 01 # number of subtrips per person
rs <- dbSendQuery(con, 
                  "SELECT pid, COUNT(*) AS subtrips, SUM(distance) AS totdist FROM tky08.subtrip GROUP BY pid"
                  )
df <- fetch(rs, n = -1)
dbClearResult(rs)
pSubtripsPerPerson <- ggplot(df, 
                              aes(x = subtrips)
                             ) + 
    geom_histogram(binwidth = 1, 
                   fill = "#660099"
                   ) + 
    labs(title = "Subtrips per Person", 
         x = "subtrips", 
         y = "frequency"
         ) + 
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(1, max(df$subtrips, na.rm = TRUE) * 1.1), 
                       breaks = c(1:4,6:9,seq(0, max(df$subtrips, na.rm = TRUE) * 1.1, 5)), 
                       labels = comma
                       ) + 
    scale_y_continuous(expand = c(0, 0), 
                       labels = comma
                       ) +
    geom_vline(aes(xintercept = mean(df$subtrips, na.rm = TRUE)), 
               color = "red", 
               linetype = "dashed", 
               size = 1
               )
pSubtripsPerPerson
summary(df)


# 02 # total length of subtrips per person
pSubtripLengthPerPerson <- ggplot(df, 
                                  aes(x = totdist / 1000)
                                  ) + 
    geom_histogram(origin = 0, 
                   binwidth = sd(df$totdist / 1000, na.rm = TRUE), 
                   fill = "#660099"
                   ) + 
    labs(title = "Accumulated Length of Subtrips per Person", 
         x = "accumulated length of subtrips [km]", 
         y = "frequency"
         ) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(seq(0, max(df$totdist / 1000, na.rm = TRUE) * 1.1, 100)), 
                       labels = comma
                       ) + 
    scale_y_continuous(expand = c(0, 0), 
                       labels = comma
                       ) +
    geom_vline(aes(xintercept = mean(df$totdist / 1000, na.rm = TRUE)), 
               color = "red", 
               linetype = "dashed", 
               size = 1
               )
pSubtripLengthPerPerson
summary(df$totdist / 1000)


# 03 # number of subtrips per transportation mode
rs <- dbSendQuery(con, 
                  "SELECT means AS mode, COUNT(*) AS subtrips, SUM(distance) totdist FROM tky08.subtrip GROUP BY means ORDER BY means"
                  )
df <- fetch(rs, n = -1)
dbClearResult(rs)
df2 <- df # -> 03b/03c

# 03a # 13 original transportation modes
df$mode <- factor(df$mode, labels = transportationModesEN)  # assign verbose labels
df$mode <- factor(levels(df$mode), levels = rev(levels(df$mode)))  # flip order for plot
pSubtripsPerTransportationMode <- ggplot(df, 
                                         aes(mode, subtrips)
                                         ) + 
    layer(geom = "bar", 
          stat = "identity"
          ) + 
    labs(title = "Subtrips per Transportation Mode", 
         x = "transportation mode", 
         y = "subtrips"
         ) + 
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, max(df$subtrips, na.rm = TRUE) * 1.1), 
                       breaks = seq(0, max(df$subtrips, na.rm = TRUE) * 1.1, 500000), 
                       labels = comma
                       ) + 
    coord_flip()
pSubtripsPerTransportationMode
df

# 03b # 8 grouped transportation modes
df[3,2:3] <- colSums(subset(df[3:4,], select=c("subtrips", "totdist")), na.rm = TRUE)  # group "moped" and "motorcycle"
df[6,2:3] <- colSums(subset(df[6:7,], select=c("subtrips", "totdist")), na.rm = TRUE)  # group "car" and "minivan"
df[9,2:3] <- colSums(subset(df[9:10,], select=c("subtrips", "totdist")), na.rm = TRUE)  # group "private bus" and "public bus"
df[11,2:3] <- colSums(subset(df[11:12,], select=c("subtrips", "totdist")), na.rm = TRUE)  # group "monorail" and "train, subway"
df <- df[c(1, 2, 3, 5, 6, 8, 9, 11, 13), ]  # remove unnecessary modes
df$mode <- factor(df$mode, labels = groupedTransportationModesEN)  # assign verbose labels
df$mode <- factor(levels(df$mode), levels = rev(levels(df$mode)))  # flip order for plot
df <- subset(df, mode != "stationarity")  # remove stationarity
pSubtripsPerGroupedTransportationMode <- ggplot(df, 
                                                aes(mode, subtrips, fill = mode)
                                                ) + 
    layer(geom = "bar", 
          stat = "identity"
          ) + 
    labs(title = "Subtrips per Transportation Mode", 
         x = "transportation mode", 
         y = "subtrips"
         ) + 
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, max(df$subtrips, na.rm = TRUE) * 1.1), 
                       breaks = seq(0, max(df$subtrips, na.rm = TRUE) * 1.1, 100000), 
                       labels = comma
                       ) + 
    scale_fill_brewer(palette = "Dark2") + 
    theme(legend.position = "none") + 
    coord_flip()
pSubtripsPerGroupedTransportationMode
df

# 03c # pie chart of 8 grouped transportation modes
labels <- paste(df$mode, 
                round(df$subtrips / sum(df$subtrips) * 100, 1), 
                sep = ": "
                )
labels <- paste(labels,"%",sep="")
pie(df$subtrips, 
    labels = labels, 
    col = c("#666666", "#a6761d", "#e6ab02", "#66a61e", "#e7298a", "#7570b3", "#d95f02", "#1b9e77"), 
    clockwise = TRUE, 
    init.angle = 220, 
    edges = 360, 
    main = "Subtrips per Transportation Mode"
    )


# 04a # total length of subtrips per grouped transportation mode
pSubtripLengthPerGroupedTransportationMode <- ggplot(df, 
                                                     aes(mode, 
                                                         totdist / 1000, 
                                                         fill = mode
                                                         )
                                                     ) + 
    layer(geom = "bar", 
          stat = "identity"
          ) + 
    labs(title = "Accumulated Length of Subtrips per Transportation Mode", 
         x = "transportation mode", 
         y = "accumulated length of subtrips [km]"
         ) + 
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, max(df$totdist / 1000, na.rm = TRUE) * 1.1), 
                       breaks = seq(0, max(df$totdist / 1000, na.rm = TRUE) * 1.1, 1000000), 
                       labels = comma
                       ) + 
    scale_fill_brewer(palette = "Dark2") + 
    theme(legend.position = "none") + 
    coord_flip()
pSubtripLengthPerGroupedTransportationMode
df

# 04b # pie chart of 8 grouped transportation modes
labels <- paste(df$mode, 
                round(df$totdist / sum(df$totdist) * 100, 1), 
                sep = ": "
)
labels <- paste(labels,"%",sep="")
pie(df$totdist, 
    labels = labels, 
    col = c("#666666", "#a6761d", "#e6ab02", "#66a61e", "#e7298a", "#7570b3", "#d95f02", "#1b9e77"), 
    clockwise = TRUE, 
    init.angle = 70, 
    edges = 360, 
    main = "Accumulated Length of Subtrips per Transportation Mode"
)


# 05 # total length of subtrips per grouped transportation mode
rs <- dbSendQuery(con, 
                  "SELECT means AS mode, distance FROM tky08.subtrip"
                  )
df <- fetch(rs, n = -1)
dbClearResult(rs)
df$subtrips[3] <- df$subtrips[3] + df$subtrips[4]  # group "moped" and "motorcycle"
df$subtrips[6] <- df$subtrips[6] + df$subtrips[7]  # group "car" and "minivan"
df$subtrips[9] <- df$subtrips[9] + df$subtrips[10]  # group "private bus" and "public bus"
df$subtrips[11] <- df$subtrips[11] + df$subtrips[12]  # group "monorail" and "train, subway"
df <- subset(df, 
             !(mode %in% c(4, 7, 10, 12))
             ) # remove unnecessary modes
df$mode <- factor(df$mode, 
                  labels = groupedTransportationModesEN
                  )  # assign verbose labels
df$mode <- factor(df$mode, 
                  levels = rev(levels(df$mode))
                  )  # flip order for ggplot bar chart
df <- subset(df, 
             mode != "stationarity"
             )  # remove stationarity
pSubtripLengthPerTransportationMode <- ggplot(df, 
                                              aes(x = mode, 
                                                  y = distance / 1000, 
                                                  fill = mode
                                                  )
                                              ) + 
    geom_boxplot() + 
    guides(fill = FALSE) + 
    labs(title = "Length of Subtrips per Transportation Mode", 
         x = "transportation mode", 
         y = "length of subtrips [km]"
         ) + 
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, max(df$distance / 1000, na.rm = TRUE) * 1.1), 
                       breaks = seq(0, max(df$distance / 1000, na.rm = TRUE) * 1.1, 50), 
                       labels = comma
                       ) + 
    scale_fill_brewer(palette = "Dark2") + 
    theme(legend.position = "none") +
    stat_summary(fun.y = mean, 
                 geom = "point", 
                 shape = 23, 
                 size = 4) + 
    coord_flip()
pSubtripLengthPerTransportationMode


# 06 # started subtrips per hour
rs <- dbSendQuery(con, 
                  "SELECT date_part('hour', sdate) AS shour, means AS mode, COUNT(*) AS subtrips FROM tky08.subtrip WHERE sdate IS NOT NULL GROUP BY shour, mode"
                  )
df <- fetch(rs, n = -1)
dbClearResult(rs)
pStartedSubtripsPerHour <- ggplot(df, 
                                  aes(x = shour, y = subtrips)
                                  ) + 
    geom_bar(stat = "identity",
             fill = "#660099"
             ) + 
    labs(title = "Started Subtrips per Hour", 
         x = "hour", 
         y = "subtrips"
         ) + 
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(0, 23), 
                       breaks = c(0:23, 1), 
                       labels = comma
                       ) + 
    scale_y_continuous(expand = c(0, 0), 
                       labels = comma
                       ) + 
    theme(legend.position = "none")
pStartedSubtripsPerHour


# 07a # started subtrips per hour and transportation mode
df$mode <- factor(df$mode, labels = transportationModesEN[1:12])  # assign verbose labels (ex. 97)
pStartedSubtripsPerHourAndTransportationMode <- ggplot(df, 
                                                       aes(x = shour, y = subtrips)
                                                       ) + 
    geom_bar(stat = "identity") + 
    labs(title = "Started Subtrips per Hour and Transportation Mode", 
         x = "hour", 
         y = "subtrips"
         ) + 
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(0, 23), 
                       breaks = c(0:23, 1), 
                       labels = comma
                       ) + 
    scale_y_continuous(expand = c(0, 0), 
                       labels = comma
                       ) + 
    theme(legend.position = "none") +
    facet_wrap(~ mode)
pStartedSubtripsPerHourAndTransportationMode


# 07b # started subtrips per hour and grouped transportation mode
rs <- dbSendQuery(con, 
                  "SELECT date_part('hour', sdate) AS shour, means AS mode, COUNT(*) AS subtrips FROM tky08.subtrip WHERE sdate IS NOT NULL GROUP BY shour, mode"
)
df <- fetch(rs, n = -1)
dbClearResult(rs)
for (i in seq(0, 23)) {
    cat(i)
    df[ which(df$shour == i & df$mode == 3), 3] <- 
        ifelse(length(df[ which(df$shour == i & df$mode == 3), 3]) > 0, 
               df[ which(df$shour == i & df$mode == 3), 3], 
               0) + 
        ifelse(length(df[ which(df$shour == i & df$mode == 4), 3]) > 0, 
               df[ which(df$shour == i & df$mode == 4), 3], 
               0)  # group "moped" and "motorcycle"
    df[ which(df$shour == i & df$mode == 6), 3] <- 
        ifelse(length(df[ which(df$shour == i & df$mode == 6), 3]) > 0, 
               df[ which(df$shour == i & df$mode == 6), 3], 
               0) + 
        ifelse(length(df[ which(df$shour == i & df$mode == 7), 3]) > 0, 
               df[ which(df$shour == i & df$mode == 7), 3], 
               0)  # group "car" and "minivan"
    df[ which(df$shour == i & df$mode == 9), 3] <- 
        ifelse(length(df[ which(df$shour == i & df$mode == 9), 3]) > 0, 
               df[ which(df$shour == i & df$mode == 9), 3], 
               0) + 
        ifelse(length(df[ which(df$shour == i & df$mode == 10), 3]) > 0, 
               df[ which(df$shour == i & df$mode == 10), 3], 
               0)  # group "private bus" and "public bus"
    df[ which(df$shour == i & df$mode == 12), 3] <- 
        ifelse(length(df[ which(df$shour == i & df$mode == 11), 3]) > 0, 
               df[ which(df$shour == i & df$mode == 11), 3], 
               0) + 
        ifelse(length(df[ which(df$shour == i & df$mode == 12), 3]) > 0, 
               df[ which(df$shour == i & df$mode == 12), 3], 
               0)  # group "monorail" and "train, subway"
}
df <- subset(df, !(mode %in% c(4, 7, 10, 11))) # remove unnecessary modes
df$mode <- factor(df$mode, labels = groupedTransportationModesEN[1:8])  # assign verbose labels (ex. 97)
df$mode <- factor(df$mode, 
                  levels = rev(levels(df$mode))
                  )  # flip order for ggplot bar chart
pStartedSubtripsPerHourAndGroupedTransportationMode <- ggplot(df, 
                                                              aes(x = shour, 
                                                                  y = subtrips, 
                                                                  fill = factor(mode)
                                                                  )
                                                              ) + 
    geom_bar(stat = "identity") + 
    labs(title = "Started Subtrips per Hour and Transportation Mode", 
         x = "hour", 
         y = "subtrips"
         ) + 
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(0, 23), 
                       breaks = c(0:23, 1), 
                       labels = comma
                       ) + 
    scale_y_continuous(expand = c(0, 0), 
                       labels = comma
                       ) + 
    scale_fill_brewer(palette = "Dark2") + 
    theme(legend.position = "none") +
    facet_wrap(~ mode)
pStartedSubtripsPerHourAndGroupedTransportationMode


# clean up
for (connection in dbListConnections(drv)) {
    dbDisconnect(connection)   
}
dbUnloadDriver(drv)
theme_set(old_theme)
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
library(scales)

# variable declaration
ageGroupsJP <- factor(c("0歳以上5歳未満", "5歳以上10歳未満", "10 歳以上 15 歳未満", "15 歳以上 20 歳未満", "20 歳以上 25 歳未満", "25 歳以上 30 歳未満", "30 歳以上 35 歳未満", "35 歳以上 40 歳未満", "40 歳以上 45 歳未満", "45 歳以上 50 歳未満", "50 歳以上 55 歳未満", "55 歳以上 60 歳未満", "60 歳以上 65 歳未満", "65 歳以上 70 歳未満", "70 歳以上 75 歳未満", "75 歳以上 80 歳未満", "80 歳以上 85 歳未満", "85 歳以上"))
ageGroupsEN <- factor(c("under 4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85 years and older"))
gendersJP <- factor(c("男性", "女性", "不明"))
gendersEN <- factor(c("male", "female", "n/a"))
transportationModesJP <- factor(c("徒歩", "自転車", "原動機付自転車", "自動二輪車", "タクシー", "乗用車", "軽乗用車", "貨物自動車", "自家用バス", "路線バス", "モノレール・新交通", "鉄道・地下鉄", "停滞"))
transportationModesEN <- factor(c("walking", "bicycle", "moped", "motorcycle", "taxi", "car", "minivan", "truck", "private bus", "public bus", "monorail", "train, subway", "stationarity"))
groupedTransportationModesJP <- factor(c("徒歩", "自転車", "自動二輪車", "タクシー", "乗用車", "貨物自動車", "バス", "鉄道", "停滞"))
groupedTransportationModesEN <- factor(c("walking", "bicycle", "motorcycle", "taxi", "car", "truck", "bus", "train", "stationarity"))
occupationsJP <- factor(c("農林水産業従事者", "生産工程・労務作業者", "販売従事者", "サービス職業従事者", "運輸・通信従事者", "保安職業従事者", "事務従事者", "専門的・技術的職業従事者", "管理的職業従事者", "その他職業", "園児・小学生・中学", "高校生", "大学生・短大生・各種専門学校生", "主婦・主夫(職業従事者を除く)", "無職", "その他", "不明"))
occupationsEN <- factor(c("agriculture, forestry & fishery", "production", "sales", "service", "transportation & communication", "security", "clerical", "professional & technical", "management", "other occupation", "kindergarten, elementary & junior high school", "senior high school", "university, college & vocational school", "housewife, househusband", "unemployed, retired", "other", "n/a"))
tripPurposesJP <- factor(c("勤務先へ(帰社を含む)", "通学先へ(帰校を含む)", "自宅へ", "買物へ", "食事・社交・娯楽へ(日常生活圏内)", "観光・行楽・レジャーへ(日常生活圏外)", "通院", "送迎", "その他の私用へ", "販売・配達・仕入・購入先へ", "打合せ・会議・集金・往診へ", "作業・修理へ", "農林漁業作業へ", "その他の業務へ", "その他"))
tripPurposesEN <- factor(c("to work", "to school", "to home", "for shopping", "for dining, entertainment", "for sightseeing, leisure", "for hospital visit", "pick-up, sending-off", "for other personal errand", "for work-related delivery, purchase", "for work-related meeting", "for work-related operation", "for agriculture-related work", "for other work-related activity", "other"))

# prepare and establish connection to PostgreSQL database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, 
                 host = "127.0.0.1", 
                 port = 5432, 
                 dbname = "maindb", 
                 user = "postgres", 
                 password = "postgres"
                 )


# number of subtrips per transportation mode
rs <- dbSendQuery(con, 
                  "SELECT means AS mode, COUNT(*) AS subtrips FROM tky08.subtrip GROUP BY means ORDER BY means"
                  )
dfSubtripsPerTransportationMode <- fetch(rs, n = -1)
dbClearResult(rs)
dfSubtripsPerGroupedTransportationMode <- dfSubtripsPerTransportationMode

# - 13 original transportation modes
dfSubtripsPerTransportationMode$mode <- factor(dfSubtripsPerTransportationMode$mode, 
                                               labels = transportationModesEN
                                               )
dfSubtripsPerTransportationMode$mode <- factor(dfSubtripsPerTransportationMode$mode, 
                                               levels = rev(levels(dfSubtripsPerTransportationMode$mode))
                                               )
pSubtripsPerTransportationMode <- ggplot(dfSubtripsPerTransportationMode, 
                                         aes(mode, subtrips)) + 
    layer(
        geom = "bar", 
        stat = "identity"
        ) + 
    labs(
        title = "Subtrips per Transportation Mode", 
        x = "transportation mode", 
        y = "subtrips"
        ) + 
    scale_y_continuous(
        labels = comma
        ) + 
    coord_flip()
#pSubtripsPerTransportationMode

# - 9 grouped transportation modes
dfSubtripsPerGroupedTransportationMode$subtrips[3] <- dfSubtripsPerGroupedTransportationMode$subtrips[3] + dfSubtripsPerGroupedTransportationMode$subtrips[4]  # group "moped" and "motorcycle"
dfSubtripsPerGroupedTransportationMode$subtrips[6] <- dfSubtripsPerGroupedTransportationMode$subtrips[6] + dfSubtripsPerGroupedTransportationMode$subtrips[7]  # group "car" and "minivan"
dfSubtripsPerGroupedTransportationMode$subtrips[9] <- dfSubtripsPerGroupedTransportationMode$subtrips[9] + dfSubtripsPerGroupedTransportationMode$subtrips[10]  # group "private bus" and "public bus"
dfSubtripsPerGroupedTransportationMode$subtrips[11] <- dfSubtripsPerGroupedTransportationMode$subtrips[11] + dfSubtripsPerGroupedTransportationMode$subtrips[12]  # group "monorail" and "train, subway"
dfSubtripsPerGroupedTransportationMode <- subset(dfSubtripsPerGroupedTransportationMode, !(mode %in% c(4, 7, 10, 12)))
dfSubtripsPerGroupedTransportationMode$mode <- factor(dfSubtripsPerGroupedTransportationMode$mode, 
                                                      labels = groupedTransportationModesEN
                                                      )
dfSubtripsPerGroupedTransportationMode$mode <- factor(dfSubtripsPerGroupedTransportationMode$mode, 
                                                      levels = rev(levels(dfSubtripsPerGroupedTransportationMode$mode))
                                                      )
dfSubtripsPerGroupedTransportationMode <- subset(dfSubtripsPerGroupedTransportationMode, mode != "stationarity")  # remove stationarity
pSubtripsPerGroupedTransportationMode <- ggplot(dfSubtripsPerGroupedTransportationMode, 
                                                aes(mode, subtrips)) + 
    layer(
        geom = "bar", 
        stat = "identity"
    ) + 
    labs(
        title = "Subtrips per Transportation Mode", 
        x = "transportation mode", 
        y = "subtrips"
    ) + 
    scale_y_continuous(
        labels = comma
    ) + 
    coord_flip()
pSubtripsPerGroupedTransportationMode


# clean up
for (connection in names(dbListConnections(drv))) {
    dbDisconnect(connection)   
}
dbUnloadDriver(drv)
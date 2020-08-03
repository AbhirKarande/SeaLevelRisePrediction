install.packages("lubridate")
library(lubridate)
ShortDT[, c("hour1", "hour2") := tstrsplit(`hour`, ":", fixed=TRUE)]
ShortDT$mth <- month(as.POSIXct(DT$date, format="%m/%d/%Y"))
ShortDT <- ShortDT[, mth:= month(date)]

ShortDT$year <- format(as.Date(ShortDT$date, format="%d/%m/%Y"),"%Y")

ShortDT <- ShortDT[, year:= year(date)]

ShortDT$WL <- ShortDT$`Water Level`
ShortDT <- ShortDT[,SL_mth:= mean(WL), date][!is.na(SL_mth)]

NewShortDT <- ShortDT[,min(SL_mth),date]
#for monthly mean data
ShortDT <- ShortDT[,SL_mth:= mean(WL), .(year,mth) ][!is.na(SL_mth)]

ShortDT$WT <- ShortDT$WATERTEMP

ShortDT <- ShortDT[,WTemp_mth:= mean(WT), .(year,mth)][!is.na(WTemp_mth)]

ShortDT <- ShortDT[,AT_mth:= mean(AT), .(year,mth)][!is.na(AT_mth)]

ShortDT <- ShortDT[,BARO_mth:= mean(BARO), .(year,mth)][!is.na(BARO_mth)]

ShortDT <- ShortDT[,GUSTS_mth:= mean(GUSTS), .(year,mth)][!is.na(GUSTS_mth)]

WaterTemp <- ShortDT[,min(WTemp_mth), .(year,mth)][!is.na(year)]

NewShortDT <- ShortDT[ ,max(SL_mth), .(year,mth)][!is.na(year)]

model <- lm(DV_test2~Effi_Succeed, data=ddf1)

summary(model)

ddf2$pre <- predict(model, ddf2)

setDT(ddf2)

ddf2 <- ddf2[,dif:=pre-DV_test2]

plot(ddf2$dif)

SSE <- sum( ddf2$dif^ 2)


             
library('tseries')
plot(NewDT)
plot.ts(NewDT$V1)

write.csv(NewDT, "D:\\R\\SLRmthMean.csv")

plot(NewDT$year, NewDT$V1)

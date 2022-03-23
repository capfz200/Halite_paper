library("data.table")
library("tidyverse")
library("scales")
library("lubridate")
library("forecast")
library("transformr")
library("doParallel")
library("foreach")
library("timeDate")
library("dplyr")
library("mgcv")
library("ggplot2")



############################
### PROCESS SALAR GRANDE ###
############################
## IMPORT PAR ##
sg.par <- fread("SG/SG-North/Hilltop/SG_North_Hilltop_PAR.csv", data.table=F)

# generate yearday, and 24hr time
sg.par$date <- str_split_fixed(sg.par[,2], pattern=" ", n = 3)[,1]
sg.par$time12 <- str_split_fixed(sg.par[,2], pattern=" ", n = 2)[,2]
sg.par$ampm <- str_split_fixed(sg.par[,2], pattern=" ", n = 3)[,3]
head(sg.par)

# using timeDate package
sg.par$yearday <- dayOfYear(as.timeDate(sg.par$date))
sg.par$time <- format(strptime(sg.par$time12, "%I:%M:%S %p"), format="%H:%M:%S")
sg.par$dayhour <- (as.numeric(as.POSIXct(paste("2014-01-01", sg.par$time))) - as.numeric(as.POSIXct("2014-01-01 0:0:0")))/3600
head(sg.par)

sg.par <- sg.par[,c(7,9,3)]
colnames(sg.par) <- c("yearday", "dayhour", "PAR")
head(sg.par)
str(sg.par)

fwrite(sg.par, file="3.data.processed/sg.processed.PAR.csv")

## IMPORT TEMP AND RH ##
sg <- fread("SG/SG-North/Hilltop/combo.csv", data.table=F)
head(sg)

sg$date <- str_split_fixed(sg[,6], pattern=" ", n = 3)[,1]
sg$time <- str_split_fixed(sg[,6], pattern=" ", n = 2)[,2]
head(sg)

# using timeDate package
sg$yearday <- dayOfYear(as.timeDate(sg$date))
sg$dayhour <- (as.numeric(as.POSIXct(paste("2014-01-01", sg$time))) - as.numeric(as.POSIXct("2014-01-01 0:0:0")))/3600
head(sg)

sg <- sg[,c(1:5,11,12,7,8)]
colnames(sg) <- c("year", "location1", "location2", "halite", "position", "yearday", "dayhour", "RH", "Temp")
head(sg)
str(sg)


# CREATE SGMELT WITH PAR DATA 
# melt sg
sg.melt <- reshape2::melt(sg, measure.vars=c("Temp", "RH"), variable.name="variable", value.name="value")
head(sg.melt)

# melt sg.par
head(sg.par)
sg.par.melt <- reshape2::melt(sg.par, measure.vars="PAR", variable.name="variable", value.name="value")
head(sg.par.melt)

# adjust sg.par.melt
sg.par.melt$year <- rep(2020, nrow(sg.par.melt))
sg.par.melt$location1 <- rep("SG", nrow(sg.par.melt))
sg.par.melt$location2 <- rep("Native", nrow(sg.par.melt))
sg.par.melt$halite <- rep("Air", nrow(sg.par.melt))
sg.par.melt$position <- rep("Air", nrow(sg.par.melt))

# optional: log PAR
# sg.par.melt$value <- log(sg.par.melt$value)

# merge and re-factor some levels
sg.melt <- dplyr::bind_rows(sg.melt, sg.par.melt)
str(sg.melt)
sg.melt$year <- factor(sg.melt$year)
sg.melt$location1 <- factor(sg.melt$location1)
sg.melt$location2 <- factor(sg.melt$location2)
sg.melt$halite <- factor(sg.melt$halite)
sg.melt$position <- factor(sg.melt$position)
sg.melt$variable <- factor(sg.melt$variable)
str(sg.melt)

fwrite(sg, file="3.data.processed/sg.processed.csv")
fwrite(sg.melt, file="3.data.processed/sg.processed.melt.csv")



####################
### PROCESS ALMA ###
####################
## IMPORT PAR ##
alma.par <- fread("ALMA/AL_PAR.csv", data.table=F)

# generate yearday, and 24hr time
alma.par$date <- str_split_fixed(alma.par[,2], pattern=" ", n = 3)[,1]
alma.par$time12 <- str_split_fixed(alma.par[,2], pattern=" ", n = 2)[,2]
alma.par$ampm <- str_split_fixed(alma.par[,2], pattern=" ", n = 3)[,3]
head(alma.par)

# using timeDate package
alma.par$yearday <- dayOfYear(as.timeDate(alma.par$date))
alma.par$time <- format(strptime(alma.par$time12, "%I:%M:%S %p"), format="%H:%M:%S")
alma.par$dayhour <- (as.numeric(as.POSIXct(paste("2014-01-01", alma.par$time))) - as.numeric(as.POSIXct("2014-01-01 0:0:0")))/3600
head(alma.par)

alma.par <- alma.par[,c(7,9,3)]
colnames(alma.par) <- c("yearday", "dayhour", "PAR")
head(alma.par)
str(alma.par)

fwrite(alma.par, file="3.data.processed/alma.processed.PAR.csv")

## IMPORT TEMP AND RH ##
alma <- fread("ALMA/combo.csv", data.table=F)
head(alma)

alma$date <- str_split_fixed(alma[,6], pattern=" ", n = 3)[,1]
alma$time <- str_split_fixed(alma[,6], pattern=" ", n = 2)[,2]
head(alma)

# using timeDate package
alma$yearday <- dayOfYear(as.timeDate(alma$date))
alma$dayhour <- (as.numeric(as.POSIXct(paste("2014-01-01", alma$time))) - as.numeric(as.POSIXct("2014-01-01 0:0:0")))/3600
head(alma)

alma <- alma[,c(1:5,11,12,7,8)]
colnames(alma) <- c("year", "location1", "location2", "halite", "position", "yearday", "dayhour", "RH", "Temp")
head(alma)
str(alma)


# CREATE almaMELT WITH PAR DATA 
# melt alma
alma.melt <- reshape2::melt(alma, measure.vars=c("Temp", "RH"), variable.name="variable", value.name="value")
head(alma.melt)

# melt alma.par
head(alma.par)
alma.par.melt <- reshape2::melt(alma.par, measure.vars="PAR", variable.name="variable", value.name="value")
head(alma.par.melt)

# adjust alma.par.melt
alma.par.melt$year <- rep(2020, nrow(alma.par.melt))
alma.par.melt$location1 <- rep("Alma", nrow(alma.par.melt))
alma.par.melt$location2 <- rep("Transplant", nrow(alma.par.melt))
alma.par.melt$halite <- rep("Air", nrow(alma.par.melt))
alma.par.melt$position <- rep("Air", nrow(alma.par.melt))

# optional: log PAR
# alma.par.melt$value <- log(alma.par.melt$value)

# merge and re-factor some levels
alma.melt <- dplyr::bind_rows(alma.melt, alma.par.melt)
str(alma.melt)
alma.melt$year <- factor(alma.melt$year)
alma.melt$location1 <- factor(alma.melt$location1)
alma.melt$location2 <- factor(alma.melt$location2)
alma.melt$halite <- factor(alma.melt$halite)
alma.melt$position <- factor(alma.melt$position)
alma.melt$variable <- factor(alma.melt$variable)
str(alma.melt)

fwrite(alma, file="3.data.processed/alma.processed.csv")
fwrite(alma.melt, file="3.data.processed/alma.processed.melt.csv")



########################
### PROCESS CHANARAL ###
########################
## IMPORT PAR ##
cha.par <- fread("Chanaral/CH_PAR.csv", data.table=F)

# generate yearday, and 24hr time
cha.par$date <- str_split_fixed(cha.par[,2], pattern=" ", n = 3)[,1]
cha.par$time12 <- str_split_fixed(cha.par[,2], pattern=" ", n = 2)[,2]
cha.par$ampm <- str_split_fixed(cha.par[,2], pattern=" ", n = 3)[,3]
head(cha.par)

# using timeDate package
cha.par$yearday <- dayOfYear(as.timeDate(cha.par$date))
cha.par$time <- format(strptime(cha.par$time12, "%I:%M:%S %p"), format="%H:%M:%S")
cha.par$dayhour <- (as.numeric(as.POSIXct(paste("2014-01-01", cha.par$time))) - as.numeric(as.POSIXct("2014-01-01 0:0:0")))/3600
head(cha.par)

cha.par <- cha.par[,c(7,9,3)]
colnames(cha.par) <- c("yearday", "dayhour", "PAR")
head(cha.par)
str(cha.par)

fwrite(cha.par, file="3.data.processed/cha.processed.PAR.csv")

## IMPORT TEMP AND RH ##
cha <- fread("Chanaral/combo.csv", data.table=F)
head(cha)

cha$date <- str_split_fixed(cha[,6], pattern=" ", n = 3)[,1]
cha$time <- str_split_fixed(cha[,6], pattern=" ", n = 2)[,2]
head(cha)

# using timeDate package
cha$yearday <- dayOfYear(as.timeDate(cha$date))
cha$dayhour <- (as.numeric(as.POSIXct(paste("2014-01-01", cha$time))) - as.numeric(as.POSIXct("2014-01-01 0:0:0")))/3600
head(cha)

cha <- cha[,c(1:5,11,12,7,8)]
colnames(cha) <- c("year", "location1", "location2", "halite", "position", "yearday", "dayhour", "RH", "Temp")
head(cha)
str(cha)


# CREATE CHAMELT WITH PAR DATA 
# melt cha
cha.melt <- reshape2::melt(cha, measure.vars=c("Temp", "RH"), variable.name="variable", value.name="value")
head(cha.melt)

# melt cha.par
head(cha.par)
cha.par.melt <- reshape2::melt(cha.par, measure.vars="PAR", variable.name="variable", value.name="value")
head(cha.par.melt)

# adjust cha.par.melt
cha.par.melt$year <- rep(2020, nrow(cha.par.melt))
cha.par.melt$location1 <- rep("Cha", nrow(cha.par.melt))
cha.par.melt$location2 <- rep("Transplant", nrow(cha.par.melt))
cha.par.melt$halite <- rep("Air", nrow(cha.par.melt))
cha.par.melt$position <- rep("Air", nrow(cha.par.melt))

# optional: log PAR
# cha.par.melt$value <- log(cha.par.melt$value)

# merge and re-factor some levels
cha.melt <- dplyr::bind_rows(cha.melt, cha.par.melt)
str(cha.melt)
cha.melt$year <- factor(cha.melt$year)
cha.melt$location1 <- factor(cha.melt$location1)
cha.melt$location2 <- factor(cha.melt$location2)
cha.melt$halite <- factor(cha.melt$halite)
cha.melt$position <- factor(cha.melt$position)
cha.melt$variable <- factor(cha.melt$variable)
str(cha.melt)

fwrite(cha, file="3.data.processed/cha.processed.csv")
fwrite(cha.melt, file="3.data.processed/cha.processed.melt.csv")



######################
### PROCESS YUNGAY ###
######################
## IMPORT PAR ##
yun.par <- fread("Yun/YG_hobo_27Feb_2018-1April_2019.csv", data.table=F)

# generate yearday, and 24hr time
yun.par$date <- str_split_fixed(yun.par[,2], pattern=" ", n = 3)[,1]
yun.par$time12 <- str_split_fixed(yun.par[,2], pattern=" ", n = 2)[,2]
yun.par$ampm <- str_split_fixed(yun.par[,2], pattern=" ", n = 3)[,3]
head(yun.par)

# using timeDate package
yun.par$yearday <- dayOfYear(as.timeDate(yun.par$date))
yun.par$time <- format(strptime(yun.par$time12, "%I:%M:%S %p"), format="%H:%M:%S")
yun.par$dayhour <- (as.numeric(as.POSIXct(paste("2014-01-01", yun.par$time))) - as.numeric(as.POSIXct("2014-01-01 0:0:0")))/3600
head(yun.par)

yun.par <- yun.par[,c(7,9,3)]
colnames(yun.par) <- c("yearday", "dayhour", "PAR")
head(yun.par)
str(yun.par)

fwrite(yun.par, file="3.data.processed/yun.processed.PAR.csv")

## IMPORT TEMP AND RH ##
yun <- fread("Yun/transplant/combo.csv", data.table=F)
head(yun)

yun$date <- str_split_fixed(yun[,6], pattern=" ", n = 3)[,1]
yun$time <- str_split_fixed(yun[,6], pattern=" ", n = 2)[,2]
head(yun)

# using timeDate package
yun$yearday <- dayOfYear(as.timeDate(yun$date))
yun$dayhour <- (as.numeric(as.POSIXct(paste("2014-01-01", yun$time))) - as.numeric(as.POSIXct("2014-01-01 0:0:0")))/3600
head(yun)

yun <- yun[,c(1:5,11,12,7,8)]
colnames(yun) <- c("year", "location1", "location2", "halite", "position", "yearday", "dayhour", "RH", "Temp")
head(yun)
str(yun)


# CREATE yunMELT WITH PAR DATA 
# melt yun
yun.melt <- reshape2::melt(yun, measure.vars=c("Temp", "RH"), variable.name="variable", value.name="value")
head(yun.melt)

# melt yun.par
head(yun.par)
yun.par.melt <- reshape2::melt(yun.par, measure.vars="PAR", variable.name="variable", value.name="value")
head(yun.par.melt)

# adjust yun.par.melt
yun.par.melt$year <- rep(2020, nrow(yun.par.melt))
yun.par.melt$location1 <- rep("Yun", nrow(yun.par.melt))
yun.par.melt$location2 <- rep("Translplant", nrow(yun.par.melt))
yun.par.melt$halite <- rep("Air", nrow(yun.par.melt))
yun.par.melt$position <- rep("Air", nrow(yun.par.melt))

# optional: log PAR
# yun.par.melt$value <- log(yun.par.melt$value)

# merge and re-factor some levels
yun.melt <- dplyr::bind_rows(yun.melt, yun.par.melt)
str(yun.melt)
yun.melt$year <- factor(yun.melt$year)
yun.melt$location1 <- factor(yun.melt$location1)
yun.melt$location2 <- factor(yun.melt$location2)
yun.melt$halite <- factor(yun.melt$halite)
yun.melt$position <- factor(yun.melt$position)
yun.melt$variable <- factor(yun.melt$variable)

head(yun.melt)
str(yun.melt)

fwrite(yun, file="3.data.processed/yun.transplant.processed.csv")
fwrite(yun.melt, file="3.data.processed/yun.transplant.processed.melt.csv")



######################################
### MODEL PAR, TEMPERATURE, AND RH ###
######################################

### LOAD AND JOIN DATA ###
## LOAD ##
sg <- fread("3.data.processed/sg.processed.csv", data.table=F)
yun.trans <- fread("3.data.processed/yun.transplant.processed.csv", data.table=F)
cha <- fread("3.data.processed/cha.processed.csv", data.table=F)
alma <- fread("3.data.processed/alma.processed.csv", data.table=F)

sg.par <- fread("3.data.processed/sg.processed.PAR.csv", data.table=F)
yun.par.trans <- fread("3.data.processed/yun.processed.PAR.csv", data.table=F)
cha.par <- fread("3.data.processed/cha.processed.PAR.csv", data.table=F)
alma.par <- fread("3.data.processed/alma.processed.PAR.csv", data.table=F)


## JOIN AND ADJUST THE RH AND TEMP ##
all <- bind_rows(sg, yun.trans, cha, alma)

all$year <- factor(all$year)
all$location1 <- factor(all$location1)
all$location2 <- factor(all$location2)
all$halite <- factor(all$halite)
all$position <- factor(all$position)

head(subset(all, location2=="Native" & location1=="Yun" & position=="Air"))

## JOIN AND ADJUST THE PAR DATASET ##
# sg
sg.par$year <- rep(2020, nrow(sg.par))
sg.par$location1 <- rep("SG", nrow(sg.par))
sg.par$location2 <- rep("Native", nrow(sg.par))
sg.par$halite <- rep("Air", nrow(sg.par))
sg.par$position <- rep("Air", nrow(sg.par))

# Yungay transplant
yun.par.trans$year <- rep(2020, nrow(yun.par.trans))
yun.par.trans$location1 <- rep("Yun", nrow(yun.par.trans))
yun.par.trans$location2 <- rep("Transplant", nrow(yun.par.trans))
yun.par.trans$halite <- rep("Air", nrow(yun.par.trans))
yun.par.trans$position <- rep("Air", nrow(yun.par.trans))

# Chanaral
cha.par$year <- rep(2020, nrow(cha.par))
cha.par$location1 <- rep("Cha", nrow(cha.par))
cha.par$location2 <- rep("Transplant", nrow(cha.par))
cha.par$halite <- rep("Air", nrow(cha.par))
cha.par$position <- rep("Air", nrow(cha.par))

# ALMA
alma.par$year <- rep(2020, nrow(alma.par))
alma.par$location1 <- rep("Alma", nrow(alma.par))
alma.par$location2 <- rep("Transplant", nrow(alma.par))
alma.par$halite <- rep("Air", nrow(alma.par))
alma.par$position <- rep("Air", nrow(alma.par))


## COMBINE PAR ##
all.par <- bind_rows(sg.par, yun.par.trans, cha.par, alma.par)


## import light attenuation table
par.att <- fread("1.data.raw/field/sensors.csv", data.table=F)

## PREDICT PAR, TEMP AND RH using DPLYR
# exlude bad 2019 year from SG
all.par.sub <- subset(all.par, !(year==2019 & location1=="SG"))

# exclude first and last day (incomplete days)
all.par.sub <- all.par.sub %>% 
  group_by(location1, location2, halite, position) %>% 
  filter(!(row_number() %in% c(1, n())))

all.par.sub <- merge(all.par.sub, par.att, by=c("year", "location1", "location2", "halite", "position"))

all.par.sub$year <- factor(all.par.sub$year)
all.par.sub$location1 <- factor(all.par.sub$location1)
all.par.sub$location2 <- factor(all.par.sub$location2)
all.par.sub$halite <- factor(all.par.sub$halite)
all.par.sub$position <- factor(all.par.sub$position)


## PREDICT PAR
dayhours.pred <- data.frame(dayhour=seq(from=0, to=23.99, by=0.05))

data <- as_tibble(all.par.sub) %>%
  group_by(year, location1, location2, halite, position, transmission_factor, yearday) %>%
  nest()

models <- data %>%
  mutate(
    dayhours.new = map(data, ~ dayhours.pred$dayhour),
    model = map(data, ~ gam(PAR ~ s(dayhour, bs = "ad"), method=c("ML"), data = .x)),
    predicted.par = map(model, ~ predict.gam(.x, newdata = dayhours.pred))
  )

results.par <- data.frame(
  select(models, any_of(c("year", "location1", "location2", "halite", "position", "yearday", "transmission_factor", "dayhours.new", "predicted.par"))) %>% 
    unnest(cols = c(dayhours.new, predicted.par))
)

data <- NULL
models <- NULL


## PREDICT RH and TEMP
# for big data sites
# subset to big data locations only
all.sub.big <- subset(all, location1 %in% c("SG", "Yun", "Alma", "Cha"))

# exlude bad 2019 year from SG
all.sub.big <- subset(all.sub.big, !(year==2019 & location1=="SG"))
dim(all.sub.big)

# exclude first and last day
all.sub.big <- all.sub.big %>%
  group_by(location1, location2, halite, position) %>%
  filter(!(row_number() %in% c(1, n())))

# exclude incomplete days
all.sub.big <- all.sub.big %>% 
  group_by(location1, location2, halite, position, yearday) %>% 
  # filter(n()==48) # 48 is a perfect, full day
  filter(n()>47)

all.sub.big <- merge(all.sub.big, par.att, by=c("year", "location1", "location2", "halite", "position"))


# predict RH and TEMP
data <- as_tibble(all.sub.big) %>%
  group_by(year, location1, location2, halite, position, transmission_factor, yearday) %>%
  nest()

models <- data %>%
  mutate(
    dayhours.new = map(data, ~ dayhours.pred$dayhour),
    model.rh = map(data, ~ gam(RH ~ s(dayhour, bs = "ad"), method=c("ML"), data = .x)),
    # model.rh = map(data, ~ gam(RH ~ s(dayhour, bs = "cr"), data = .x)),
    # model.rh = map(data, ~ gam(RH ~ s(dayhour), data = .x)),
    predicted.rh = map(model.rh, ~ predict.gam(.x, newdata = dayhours.pred)),
    model.temp = map(data, ~ gam(Temp ~ s(dayhour, bs = "ad"), method=c("ML"), data = .x)),
    # model.temp = map(data, ~ gam(Temp ~ s(dayhour, bs = "cr"), data = .x)),
    # model.temp = map(data, ~ gam(Temp ~ s(dayhour), data = .x)),
    predicted.temp = map(model.temp, ~ predict.gam(.x, newdata = dayhours.pred))
  )

results.rh.temp.big <- data.frame(
  select(models, any_of(c("year", "location1", "location2", "halite", "position", "yearday", "transmission_factor", "dayhours.new", "predicted.rh", "predicted.temp"))) %>% 
    unnest(cols=c(dayhours.new, predicted.rh, predicted.temp))
)

data <- NULL
models <- NULL


# combine big and small datasets
results.rh.temp <- results.rh.temp.big

# merge and calculate at sensor PAR
combo.fin <- merge(results.rh.temp, results.par, by=c("year", "location1", "location2", "yearday", "dayhours.new"), all.x = T, all.y=F) %>% 
  select(all_of(c("year", "location1", "location2", "yearday", "dayhours.new", "halite.x", "position.x", "transmission_factor.x", "predicted.rh", "predicted.temp", "predicted.par")))
colnames(combo.fin) <- str_replace_all(colnames(combo.fin), ".x", "")

combo.fin$predicted.par.adj <- combo.fin$predicted.par / combo.fin$transmission_factor

head(subset(combo.fin, location1=="SG"))
head(subset(combo.fin, location1=="Yun" & location2=="Transplant" & position=="Air"))
head(subset(combo.fin, location1=="Cha"))
head(subset(combo.fin, location1=="Alma"))

fwrite(combo.fin, file="3.data.processed/combo.fin.csv")

combo.fin.rds <- combo.fin %>% 
  group_by(year, location1, location2, yearday, halite, position) %>% 
  nest()
saveRDS(combo.fin.rds, "4.application/combo.fin.rds")

# melt PAR, RH, TEMP
combo.fin.melt <- reshape2::melt(combo.fin, measure.vars=c("predicted.rh", "predicted.temp", "predicted.par.adj"), variable.name="variable", value.name="value")

fwrite(combo.fin.melt, file="3.data.processed/combo.fin.melt.csv")



###########################################
### GENERATE FIGURES S4, S5, AND FIG. 2 ###
###########################################

input <- data.frame(RH=75, RH.air=95, PAR=0.01, PAR.air=0.01, user.day=155, pos="Top", stringsAsFactors=F)

######################################
### LOAD DATA (modeled RH and PAR) ###
######################################
dat <- fread("3.data.processed/combo.fin.csv", data.table=F, stringsAsFactors=T)

dat$halite <- sub(pattern=paste0('\\b', "A", '\\b'), replacement=1,dat$halite)
dat$halite <- sub(pattern=paste0('\\b', "B", '\\b'), replacement=2,dat$halite)
dat$halite <- sub(pattern=paste0('\\b', "C", '\\b'), replacement=3,dat$halite)
dat$halite <- factor(dat$halite)

dat.sub <- subset(dat, !(year==2019 & location1=="SG"))

# add separate RH threshholds for halite and air
dat.sub$rh.thresh <- rep(input$RH, nrow(dat.sub))
dat.sub$rh.thresh[dat.sub$halite=="Air"] <- input$RH.air

# add capability for separate PAR threshholds for halite and air
dat.sub$par.thresh <- rep(input$PAR, nrow(dat.sub))
dat.sub$par.thresh[dat.sub$halite=="Air"] <- input$PAR.air


############################################
### COMPUTE CONTINUOUS (3 MINUTE) VALUES ###
############################################
# group and nest data
data.group <- dat.sub %>% group_by(year, location1, location2, halite, position, yearday) %>% nest() #%>% ungroup()


# respiration function
respiration.fun <- function(df){
  respiration.duration <- length(filter(df, predicted.rh >= rh.thresh)$dayhours.new)*3/60
  
  return(respiration.duration)
}

# photosynthetic function
wetlight.fun <- function(df){
  wetlight.duration <- length(filter(df, predicted.rh >= rh.thresh, predicted.par.adj >= par.thresh)$dayhours.new)*3/60
  
  return(wetlight.duration)
}

# # mol.co2 function
# mol.co2.fun <- function(df){
#   df.filter <- length(filter(df, predicted.rh >= rh.thresh, predicted.par.adj >= par.thresh)$dayhours.new) # length of photosynthesis in days (for each 3 minute interval)
# 
#   return(mol.co2)
# }

# calculate results
results <- data.group %>%
  mutate(wetlight.min.morning=map(data, ~ min(filter(., predicted.rh >= rh.thresh, predicted.par.adj >= par.thresh, dayhours.new < 14)$dayhours.new))) %>%
  mutate(wetlight.max.morning=map(data, ~ max(filter(., predicted.rh >= rh.thresh, predicted.par.adj >= par.thresh, dayhours.new < 14)$dayhours.new))) %>%
  mutate(wetlight.min.afternoon=map(data, ~ min(filter(., predicted.rh >= rh.thresh, predicted.par.adj >= par.thresh, dayhours.new > 14)$dayhours.new))) %>%
  mutate(wetlight.max.afternoon=map(data, ~ max(filter(., predicted.rh >= rh.thresh, predicted.par.adj >= par.thresh, dayhours.new > 14)$dayhours.new))) %>%
  mutate(dormancy=map(data, dormancy.fun)) %>%
  mutate(damage=map(data, damage.fun)) %>%
  mutate(respiration=map(data, respiration.fun)) %>%
  mutate(photosynthesis=map(data, wetlight.fun))

results <- unnest(results, cols = c(data, wetlight.min.morning, wetlight.max.morning, wetlight.min.afternoon, 
                                    wetlight.max.afternoon, dormancy, damage, respiration, photosynthesis)
)

## add moles CO2 column ##
nrow(results[results$predicted.rh >= results$rh.thresh,])
results$mol.co2 <- rep(0, nrow(results))
results[results$predicted.rh >= results$rh.thresh,]$mol.co2 <- ((results[results$predicted.rh >= results$rh.thresh,]$predicted.par*60*1140*(4*pi*0.5^2)*1)/(10^6*10^12*8))*0.05

results[results$predicted.rh < results$rh.thresh,]$mol.co2 <- 0
results[results$mol.co2 < 0,]$mol.co2 <- 0


# fwrite(results, file=paste0("3.data.processed/all_results", input$RH.air, ".csv"))
fwrite(results, file=paste0("3.data.processed/all_results_", input$RH.air, "_with_molCO2.csv"))



################################################################
### PLOT PHOTOSYNTHESIS AND RESPIRATION HOURS, ALFONSO STYLE ###
################################################################

### FIG. 2 ###

# OPTIONAL: read pre-computed results
results.daily.combo <- fread("3.data.processed/daily_totals_95_with_molCO2.csv")

results.daily.resp <- results.daily.combo %>% 
  select(year, location1, location2, yearday, position, respiration, respiration.se, max.temp, min.temp, mean.temp)
colnames(results.daily.resp) <- sub(pattern="respiration", replacement="hours", colnames(results.daily.resp), fixed=T)
results.daily.resp$variable <- rep("respiration", nrow(results.daily.resp))

results.daily.phot <- results.daily.combo %>% 
  select(year, location1, location2, yearday, position, photosynthesis, photosynthesis.se, max.temp, min.temp, mean.temp)
colnames(results.daily.phot) <- sub(pattern="photosynthesis", replacement="hours", colnames(results.daily.phot), fixed=T)
results.daily.phot$variable <- rep("photosynthesis", nrow(results.daily.phot))

results.daily.melt <- bind_rows(results.daily.resp, results.daily.phot)

results.daily.a.melt <- results.daily.melt

# OPTIONAL: invert respiration time
# results.daily.a.melt$hours[results.daily.a.melt$variable=="respiration"] <- -results.daily.a.melt$hours[results.daily.a.melt$variable=="respiration"]

## SUBSET to transplant halites
results.daily.sg.melt <- subset(results.daily.a.melt, location1=="SG") # all SG halites are "native" but should be included for transplant data, too
results.daily.a.melt <- subset(results.daily.a.melt, location2=="Transplant")
results.daily.a.melt <- rbind(results.daily.sg.melt, results.daily.a.melt)

# results.daily.a.melt <- subset(results.daily.a.melt, position=="Air" & !(location2=="Native"))
results.daily.a.melt <- subset(results.daily.a.melt, !(yearday=="71"))
results.daily.a.melt <- subset(results.daily.a.melt, !(yearday=="72"))

## adjust days axes to start at collection day
# start day is 85
# end day is 77
results.daily.a.melt$yearday <- results.daily.a.melt$yearday-84
results.daily.a.melt[results.daily.a.melt$yearday<0,]$yearday <- results.daily.a.melt[results.daily.a.melt$yearday<0,]$yearday+365

## rename labels
results.daily.a.melt$location1 <- str_replace_all(results.daily.a.melt$location1, "SG", "Salar Grande")
results.daily.a.melt$location1 <- str_replace_all(results.daily.a.melt$location1, "Alma", "ALMA")
results.daily.a.melt$location1 <- str_replace_all(results.daily.a.melt$location1, "Yun", "Yungay")
results.daily.a.melt$location1 <- str_replace_all(results.daily.a.melt$location1, "Cha", "Chañaral")

## reorder levels
results.daily.a.melt <- results.daily.a.melt %>%
  mutate(location1 = factor(location1, levels=c("Salar Grande", "ALMA", "Yungay", "Chañaral"))) %>% 
  mutate(position = factor(position, levels=c("Air", "Top", "Middle", "Bottom"))) 


## PLOT ##
p1.a <- NULL
p1.a <- ggplot(results.daily.a.melt, aes(x=yearday, y=hours, fill=variable)) +
  theme_bw() +
  theme(
    # plot.title=element_text(face="bold", size=20),
    axis.title = element_text(face="bold", size=16),
    axis.text.x = element_text(face="plain", size=14),
    axis.text.y = element_text(face="plain", size=11),
    strip.text = element_text(face="bold", size = 14),
    legend.title = element_text(face="bold"),
  ) +
  ylim(c(0,24)) +
  ylab("Hours") +
  xlab("Days") +
  scale_fill_manual(name="Type", labels=c("Wet+Light", "Wet"), values=c("goldenrod1", "dodgerblue")) + # transplants
  
  # manual geom_area
  geom_area(data = subset(results.daily.a.melt, variable=="respiration"), stat="identity", fill="dodgerblue", color="black", linetype=1, size=0.25) +
  geom_area(data = subset(results.daily.a.melt, variable=="photosynthesis"), stat="identity", fill="goldenrod1", color="black", linetype=1, size=0.25) +
  
  facet_grid(position~location1)
p1.a

## extract legend and combine
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

p1.a.legend <- ggplot(results.daily.a.melt, aes(x=yearday, y=hours, fill=variable)) +
  theme_bw() +
  theme(
    # plot.title=element_text(face="bold", size=20),
    axis.title = element_text(face="bold", size=16),
    axis.text.x = element_text(face="plain", size=14),
    axis.text.y = element_text(face="plain", size=11),
    strip.text = element_text(face="bold", size = 14),
    legend.title = element_text(face="bold", size=14),
    legend.text = element_text(face="bold", size=13)
  ) +
  ylim(c(0,24)) +
  ylab("Hours") +
  xlab("Days") +
  scale_fill_manual(name="Type", labels=c("Wet", "Wet+Light"), values=c("dodgerblue", "goldenrod1")) + # transplants
  
  facet_grid(position~location1)

p1.a.legend <- g_legend(p1.a.legend)
plot(p1.a.legend)

p1.a.combo <- NULL
p1.a.combo <- grid.arrange(p1.a, p1.a.legend, ncol=2, widths=c(9,1))
p1.a.combo


ggsave(p1.a.combo, file=paste0("4.figures/activities_transplant_", input$RH.air, ".png"), dpi=500, height=6, width=14)
ggsave(p1.a.combo, file=paste0("4.figures/activities_transplant_", input$RH.air, ".svg"), height=6, width=14)
ggsave(p1.a.combo, file=paste0("4.figures/activities_transplant_", input$RH.air, ".pdf"), height=6, width=14)
ggsave(p1.a.combo, file=paste0("4.figures/activities_transplant_", input$RH.air, ".tiff"), height=6, width=14)


### TEMPERATURE ###

# OPTIONAL: read pre-computed results
# results.daily.combo <- fread("3.data.processed/daily_totals_95.csv")
results.daily.combo <- fread("3.data.processed/daily_totals_95_with_molCO2.csv")

results.daily.temp.melt <- results.daily.combo %>% 
  select(year, location1, location2, yearday, position, max.temp, min.temp, mean.temp) %>% 
  melt(id=c("year", "location1", "location2", "yearday", "position"))

results.daily.b.melt <- results.daily.temp.melt

# OPTIONAL: invert respiration time
# results.daily.b.melt$hours[results.daily.b.melt$variable=="respiration"] <- -results.daily.b.melt$hours[results.daily.b.melt$variable=="respiration"]

## SUBSET to halites you want
# Native
# results.daily.b.melt <- subset(results.daily.b.melt, location1 %in% c("Yun", "SG", "Llamara") & location2=="Native")

# Transplants
results.daily.sg.melt <- subset(results.daily.b.melt, location1=="SG") # all SG halites are "native" but should be included for transplant data, too
results.daily.b.melt <- subset(results.daily.b.melt, location2=="Transplant")
results.daily.b.melt <- rbind(results.daily.sg.melt, results.daily.b.melt)

# Optional: Remove Air
# results.daily.b.melt <- subset(results.daily.b.melt, !(position=="Air"))

# results.daily.b.melt <- subset(results.daily.b.melt, position=="Air" & !(location2=="Native"))
results.daily.b.melt <- subset(results.daily.b.melt, !(yearday=="71"))
results.daily.b.melt <- subset(results.daily.b.melt, !(yearday=="72"))

## adjust days axes to start at collection day
# start day is 85
# end day is 77
results.daily.b.melt$yearday <- results.daily.b.melt$yearday-84
results.daily.b.melt[results.daily.b.melt$yearday<0,]$yearday <- results.daily.b.melt[results.daily.b.melt$yearday<0,]$yearday+365

## rename labels
results.daily.b.melt$location1 <- str_replace_all(results.daily.b.melt$location1, "SG", "Salar Grande")
results.daily.b.melt$location1 <- str_replace_all(results.daily.b.melt$location1, "Alma", "ALMA")
results.daily.b.melt$location1 <- str_replace_all(results.daily.b.melt$location1, "Yun", "Yungay")
results.daily.b.melt$location1 <- str_replace_all(results.daily.b.melt$location1, "Cha", "Chañaral")

## reorder levels
results.daily.b.melt <- results.daily.b.melt %>%
  mutate(variable = factor(variable, levels=c("max.temp", "mean.temp", "min.temp"))) %>%
  mutate(variable = fct_relevel(variable)) %>%
  mutate(location1 = factor(location1, levels=c("Salar Grande", "ALMA", "Yungay", "Chañaral"))) %>% 
  # mutate(position = factor(position, levels=c("Top", "Middle", "Bottom")))
  mutate(position = factor(position, levels=c("Air", "Top", "Middle", "Bottom")))


## PLOT ##
p1.b <- ggplot(results.daily.b.melt, aes(x=yearday, y=value, color=variable)) +
  theme_bw() +
  theme(
    # plot.title=element_text(face="bold", size=20),
    axis.title = element_text(face="bold", size=16),
    axis.text.x = element_text(face="plain", size=14),
    axis.text.y = element_text(face="plain", size=11),
    strip.text = element_text(face="bold", size = 14),
    legend.title = element_text(face="bold", size=14),
    legend.text = element_text(face="bold", size=13)
  ) +
  ylab("Degrees Celcius") +
  xlab("Days") +
  
  # indicates data
  geom_line(data = results.daily.b.melt, aes(y=value, color=variable)) +
  scale_color_manual(name="Temperature", labels=c("Maximum", "Mean", "Minimum"), values=c("red", "green", "blue")) + # transplants
  facet_grid(position~location1)
p1.b


ggsave(p1.b, file=paste0("4.figures/temperature_transplant_", input$RH.air, ".png"), dpi=500, height=6, width=10)
ggsave(p1.b, file=paste0("4.figures/temperature_transplant_", input$RH.air, ".tiff"), height=6, width=10)
ggsave(p1.b, file=paste0("4.figures/temperature_transplant_", input$RH.air, ".svg"), height=6, width=10)
ggsave(p1.b, file=paste0("4.figures/temperature_transplant_", input$RH.air, ".pdf"), height=6, width=10)



### RELATIVE HUMIDITY ###

# OPTIONAL: read pre-computed results
# results.daily.combo <- fread("3.data.processed/daily_totals_95.csv")
results.daily.combo <- fread("3.data.processed/daily_totals_95_with_molCO2.csv")

results.daily.temp.melt <- results.daily.combo %>% 
  select(year, location1, location2, yearday, position, max.rh, min.rh, mean.rh) %>% 
  melt(id=c("year", "location1", "location2", "yearday", "position"))

results.daily.c.melt <- results.daily.temp.melt

# OPTIONAL: invert respiration time
# results.daily.c.melt$hours[results.daily.c.melt$variable=="respiration"] <- -results.daily.c.melt$hours[results.daily.c.melt$variable=="respiration"]

## SUBSET to halites you want
# Native
# results.daily.c.melt <- subset(results.daily.c.melt, location1 %in% c("Yun", "SG", "Llamara") & location2=="Native")

# Transplants
results.daily.sg.melt <- subset(results.daily.c.melt, location1=="SG") # all SG halites are "native" but should be included for transplant data, too
results.daily.c.melt <- subset(results.daily.c.melt, location2=="Transplant")
results.daily.c.melt <- rbind(results.daily.sg.melt, results.daily.c.melt)

# Optional: Remove Air
# results.daily.c.melt <- subset(results.daily.c.melt, !(position=="Air"))

# results.daily.c.melt <- subset(results.daily.c.melt, position=="Air" & !(location2=="Native"))
results.daily.c.melt <- subset(results.daily.c.melt, !(yearday=="71"))
results.daily.c.melt <- subset(results.daily.c.melt, !(yearday=="72"))

## adjust days axes to start at collection day
# start day is 85
# end day is 77
results.daily.c.melt$yearday <- results.daily.c.melt$yearday-84
results.daily.c.melt[results.daily.c.melt$yearday<0,]$yearday <- results.daily.c.melt[results.daily.c.melt$yearday<0,]$yearday+365

## rename labels
results.daily.c.melt$location1 <- str_replace_all(results.daily.c.melt$location1, "SG", "Salar Grande")
results.daily.c.melt$location1 <- str_replace_all(results.daily.c.melt$location1, "Alma", "ALMA")
results.daily.c.melt$location1 <- str_replace_all(results.daily.c.melt$location1, "Yun", "Yungay")
results.daily.c.melt$location1 <- str_replace_all(results.daily.c.melt$location1, "Cha", "Chañaral")

## reorder levels
results.daily.c.melt <- results.daily.c.melt %>%
  mutate(variable = factor(variable, levels=c("max.rh", "mean.rh", "min.rh"))) %>%
  mutate(variable = fct_relevel(variable)) %>%
  mutate(location1 = factor(location1, levels=c("Salar Grande", "ALMA", "Yungay", "Chañaral"))) %>% 
  # mutate(position = factor(position, levels=c("Top", "Middle", "Bottom")))
  mutate(position = factor(position, levels=c("Air", "Top", "Middle", "Bottom")))


subset(results.daily.c.melt, location1=="Chañaral" & position=="Air" & variable=="max.rh")
subset(results.daily.c.melt, location1=="Chañaral" & position=="Air" & variable=="max.rh" & value > 75)
subset(results.daily.c.melt, location1=="Chañaral" & position=="Air" & variable=="max.rh" & yearday > 0 & yearday <=50 & value > 75)


## PLOT ##
p1.c <- ggplot(results.daily.c.melt, aes(x=yearday, y=value, color=variable)) +
  theme_bw() +
  theme(
    # plot.title=element_text(face="bold", size=20),
    axis.title = element_text(face="bold", size=16),
    axis.text.x = element_text(face="plain", size=14),
    axis.text.y = element_text(face="plain", size=11),
    strip.text = element_text(face="bold", size = 14),
    legend.title = element_text(face="bold", size=14),
    legend.text = element_text(face="bold", size=13)
  ) +
  ylab("Relative Humidity") +
  xlab("Days") +
  
  # indicates data
  geom_line(data = results.daily.c.melt, aes(y=value, color=variable)) +
  scale_color_manual(name="Temperature", labels=c("Maximum", "Mean", "Minimum"), values=c("red", "green", "blue")) + # transplants
  facet_grid(position~location1)
p1.c


ggsave(p1.c, file=paste0("4.figures/RH_transplant_", input$RH.air, ".png"), dpi=500, height=6, width=10)
ggsave(p1.c, file=paste0("4.figures/RH_transplant_", input$RH.air, ".tiff"), dpi=500, height=6, width=10)
ggsave(p1.c, file=paste0("4.figures/RH_transplant_", input$RH.air, ".svg"), height=6, width=10)
ggsave(p1.c, file=paste0("4.figures/RH_transplant_", input$RH.air, ".pdf"), height=6, width=10)


### PAR ###

# OPTIONAL: read pre-computed results
# results.daily.combo <- fread("3.data.processed/daily_totals_95.csv")
results.daily.combo <- fread("3.data.processed/daily_totals_95_with_molCO2.csv")

results.daily.temp.melt <- results.daily.combo %>% 
  select(year, location1, location2, yearday, position, max.par, mean.par, min.par) %>% 
  melt(id=c("year", "location1", "location2", "yearday", "position"))

results.daily.d.melt <- results.daily.temp.melt

# OPTIONAL: invert respiration time
# results.daily.d.melt$hours[results.daily.d.melt$variable=="respiration"] <- -results.daily.d.melt$hours[results.daily.d.melt$variable=="respiration"]

## SUBSET to halites you want
# Native
# results.daily.d.melt <- subset(results.daily.d.melt, location1 %in% c("Yun", "SG", "Llamara") & location2=="Native")

# Transplants
results.daily.sg.melt <- subset(results.daily.d.melt, location1=="SG") # all SG halites are "native" but should be included for transplant data, too
results.daily.d.melt <- subset(results.daily.d.melt, location2=="Transplant")
results.daily.d.melt <- rbind(results.daily.sg.melt, results.daily.d.melt)

# Optional: Remove Air
# results.daily.d.melt <- subset(results.daily.d.melt, !(position=="Air"))

# results.daily.d.melt <- subset(results.daily.d.melt, position=="Air" & !(location2=="Native"))
results.daily.d.melt <- subset(results.daily.d.melt, !(yearday=="71"))
results.daily.d.melt <- subset(results.daily.d.melt, !(yearday=="72"))

## adjust days axes to start at collection day
# start day is 85
# end day is 77
results.daily.d.melt$yearday <- results.daily.d.melt$yearday-84
results.daily.d.melt[results.daily.d.melt$yearday<0,]$yearday <- results.daily.d.melt[results.daily.d.melt$yearday<0,]$yearday+365

## rename labels
results.daily.d.melt$location1 <- str_replace_all(results.daily.d.melt$location1, "SG", "Salar Grande")
results.daily.d.melt$location1 <- str_replace_all(results.daily.d.melt$location1, "Alma", "ALMA")
results.daily.d.melt$location1 <- str_replace_all(results.daily.d.melt$location1, "Yun", "Yungay")
results.daily.d.melt$location1 <- str_replace_all(results.daily.d.melt$location1, "Cha", "Chañaral")

## reorder levels
results.daily.d.melt <- results.daily.d.melt %>%
  mutate(variable = factor(variable, levels=c("max.par", "mean.par", "min.par"))) %>%
  mutate(variable = fct_relevel(variable)) %>%
  mutate(location1 = factor(location1, levels=c("Salar Grande", "ALMA", "Yungay", "Chañaral"))) %>% 
  # mutate(position = factor(position, levels=c("Top", "Middle", "Bottom")))
  mutate(position = factor(position, levels=c("Air", "Top", "Middle", "Bottom")))


## PLOT ##
p1.d <- ggplot(results.daily.d.melt, aes(x=yearday, y=value, color=variable)) +
  theme_bw() +
  theme(
    # plot.title=element_text(face="bold", size=20),
    axis.title = element_text(face="bold", size=16),
    axis.text.x = element_text(face="plain", size=14),
    axis.text.y = element_text(face="plain", size=11),
    strip.text = element_text(face="bold", size = 14),
    legend.title = element_text(face="bold", size=14),
    legend.text = element_text(face="bold", size=13)
  ) +
  ylab(expression(paste("PAR (", mu, "mol/", m^{2}, "/sec"))) +
  xlab("Days") +
  
  # indicates data
  geom_line(data = results.daily.d.melt, aes(y=value, color=variable)) +
  scale_color_manual(name="Temperature", labels=c("Maximum", "Mean", "Minimum"), values=c("red", "green", "blue")) + # transplants
  facet_grid(position~location1, scales="free")
p1.d


ggsave(p1.d, file=paste0("4.figures/PAR_transplant_", input$RH.air, ".png"), dpi=500, height=6, width=10)
ggsave(p1.d, file=paste0("4.figures/PAR_transplant_", input$RH.air, ".tiff"), dpi=500, height=6, width=10)
ggsave(p1.d, file=paste0("4.figures/PAR_transplant_", input$RH.air, ".svg"), height=6, width=10)
ggsave(p1.d, file=paste0("4.figures/PAR_transplant_", input$RH.air, ".pdf"), height=6, width=10)



length(unique(
  subset(results.daily.combo, location1=="SG" & position=="Air" & max.rh>95)$yearday
)
)
unique(results.daily.combo$location1)



### COMBO PLOTS (FIG S4, FIG S5) ###

## NEED TO HAVE results.daily.A/B/C.melt calculated in the separate plots sections above

# make results.daily.combo.melt
results.daily.b.melt.temp <- results.daily.b.melt
results.daily.b.melt.temp$env <- rep("Temperature", nrow(results.daily.b.melt.temp))

results.daily.c.melt.temp <- results.daily.c.melt
results.daily.c.melt.temp <- subset(results.daily.c.melt.temp, value <= 100) # remove RH > 100% outliers
results.daily.c.melt.temp$env <- rep("RH", nrow(results.daily.c.melt.temp))

results.daily.d.melt.temp <- results.daily.d.melt
results.daily.d.melt.temp$env <- rep("PAR", nrow(results.daily.d.melt.temp))

results.daily.combo.melt <- rbind(results.daily.b.melt.temp, results.daily.c.melt.temp, results.daily.d.melt.temp)

# adjust variable names
results.daily.combo.melt$variable <- str_replace_all(results.daily.combo.melt$variable, "max.temp", "Maximum")
results.daily.combo.melt$variable <- str_replace_all(results.daily.combo.melt$variable, "max.rh", "Maximum")
results.daily.combo.melt$variable <- str_replace_all(results.daily.combo.melt$variable, "max.par", "Maximum")
results.daily.combo.melt$variable <- str_replace_all(results.daily.combo.melt$variable, "mean.temp", "Mean")
results.daily.combo.melt$variable <- str_replace_all(results.daily.combo.melt$variable, "mean.rh", "Mean")
results.daily.combo.melt$variable <- str_replace_all(results.daily.combo.melt$variable, "mean.par", "Mean")
results.daily.combo.melt$variable <- str_replace_all(results.daily.combo.melt$variable, "min.temp", "Minimum")
results.daily.combo.melt$variable <- str_replace_all(results.daily.combo.melt$variable, "min.rh", "Minimum")
results.daily.combo.melt$variable <- str_replace_all(results.daily.combo.melt$variable, "min.par", "Minimum")


### PLOT COMBO ENVIRONMENTS (FIG S4) ###

## PLOT AIR ONLY
p2 <- ggplot(subset(results.daily.combo.melt, position=="Air"), aes(x=yearday, y=value, color=variable)) +
  theme_bw() +
  theme(
    # plot.title=element_text(face="bold", size=20),
    axis.title = element_text(face="bold", size=16),
    axis.text.x = element_text(face="plain", size=14),
    axis.text.y = element_text(face="plain", size=12),
    strip.text = element_text(face="bold", size = 14),
    legend.title = element_text(face="bold", size=14),
    legend.text = element_text(face="bold", size=13)
  ) +
  ylab(expression(paste("Degrees (°C)", "            ", "Percent (%)", "            ", mu, "mol ", m^{-2}, sec^{-1}))) +
  xlab("Days") +
  
  # indicates data
  geom_line() +
  scale_color_manual(name="Daily Value", labels=c("Maximum", "Mean", "Minimum"), values=c("red", "green", "blue")) + # transplants
  
  facet_grid(env~location1, scales="free")
p2


ggsave(p2, file=paste0("4.figures/air_combo_transplant_", input$RH.air, ".png"), dpi=500, height=6, width=10)
ggsave(p2, file=paste0("4.figures/air_combo_transplant_", input$RH.air, ".tiff"), dpi=500, height=6, width=10)
ggsave(p2, file=paste0("4.figures/air_combo_transplant_", input$RH.air, ".svg"), height=6, width=10)
ggsave(p2, file=paste0("4.figures/air_combo_transplant_", input$RH.air, ".pdf"), height=6, width=10)



## PLOT HALITES ONLY (FIG S5) ##

# TEMPERATURE
p3.a <- ggplot(subset(results.daily.combo.melt, !(position=="Air") & env=="Temperature"), aes(x=yearday, y=value, color=variable)) +
  theme_bw() +
  theme(
    # plot.title=element_text(face="bold", size=20),
    axis.title = element_text(face="bold", size=16),
    axis.text.x = element_text(face="plain", size=14),
    axis.text.y = element_text(face="plain", size=11),
    strip.text = element_text(face="bold", size = 14),
    legend.title = element_text(face="bold", size=14),
    legend.text = element_text(face="bold", size=13)
  ) +
  ylab(expression("Degrees (°C)")) +
  xlab("Days") +
  
  # indicates data
  geom_line() +
  scale_color_manual(name="Daily Value", labels=c("Maximum", "Mean", "Minimum"), values=c("red", "green", "blue")) + # transplants
  
  facet_grid(env+position~location1, scales="free_x")
p3.a


ggsave(p3.a, file=paste0("4.figures/halites_combo_temp_transplant_", input$RH.air, ".png"), dpi=500, height=6, width=10)
ggsave(p3.a, file=paste0("4.figures/halites_combo_temp_transplant_", input$RH.air, ".tiff"), dpi=500, height=6, width=10)
ggsave(p3.a, file=paste0("4.figures/halites_combo_temp_transplant_", input$RH.air, ".svg"), height=6, width=10)
ggsave(p3.a, file=paste0("4.figures/halites_combo_temp_transplant_", input$RH.air, ".pdf"), height=6, width=10)


# RELATIVE HUMIDITY
p3.b <- ggplot(subset(results.daily.combo.melt, !(position=="Air") & env=="RH"), aes(x=yearday, y=value, color=variable)) +
  theme_bw() +
  theme(
    # plot.title=element_text(face="bold", size=20),
    axis.title = element_text(face="bold", size=16),
    axis.text.x = element_text(face="plain", size=14),
    axis.text.y = element_text(face="plain", size=11),
    strip.text = element_text(face="bold", size = 14),
    legend.title = element_text(face="bold", size=14),
    legend.text = element_text(face="bold", size=13)
  ) +
  ylab(expression("Percent (%)")) +
  xlab("Days") +
  
  # indicates data
  geom_line() +
  scale_color_manual(name="Daily Value", labels=c("Maximum", "Mean", "Minimum"), values=c("red", "green", "blue")) + # transplants
  
  facet_grid(env+position~location1, scales="free_x")
p3.b


ggsave(p3.b, file=paste0("4.figures/halites_combo_rh_transplant_", input$RH.air, ".png"), dpi=500, height=6, width=10)
ggsave(p3.b, file=paste0("4.figures/halites_combo_rh_transplant_", input$RH.air, ".tiff"), dpi=500, height=6, width=10)
ggsave(p3.b, file=paste0("4.figures/halites_combo_rh_transplant_", input$RH.air, ".svg"), height=6, width=10)
ggsave(p3.b, file=paste0("4.figures/halites_combo_rh_transplant_", input$RH.air, ".pdf"), height=6, width=10)


p3.combo <- ggarrange(p3.a, p3.b,
                      nrow=2,
                      labels=c("A", "B"),
                      common.legend = T,
                      font.label=list(size=22, color="black", face="bold", family=NULL)
)
p3.combo

ggsave(p3.combo, file=paste0("4.figures/halites_combo_transplant_", input$RH.air, ".png"), dpi=500, height=10, width=10)
ggsave(p3.combo, file=paste0("4.figures/halites_combo_transplant_", input$RH.air, ".tiff"), dpi=500, height=10, width=10)
ggsave(p3.combo, file=paste0("4.figures/halites_combo_transplant_", input$RH.air, ".svg"), height=10, width=10)
ggsave(p3.combo, file=paste0("4.figures/halites_combo_transplant_", input$RH.air, ".pdf"), height=10, width=10)






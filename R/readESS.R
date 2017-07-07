#'Reading and cleaning ESS Data
#'
#'
#'Takes an initial dataframe and cleans it
#'
#'@param e1, a raw data of ESS
#'
readESS <- function(ess){e1 <- dplyr::select(ess, name,
                    happy	,
                    stflife	,
                    health	,
                    stfjb	,
                    ppltrst	,
                    pplfair	,
                    pplhlp	,
                    trstprl	,
                    trstlgl	,
                    trstplc	,
                    trstplt	,
                    trstprt	,
                    trstep	,
                    trstun	,
                    trrtort	,
                    gincdif	,
                    freehms	,
                    scnsenv	,
                    euftf	,
                    imsmetn	,
                    imdfetn	,
                    impcntr	,
                    imbgeco	,
                    imueclt	,
                    imwbcnt	,
                    vote	,
                    wkvlorg	,
                    cntry	,
                    essround	,
                    dweight	,
                    pspwght	,
                    rlgdnm	,
                    rlgdgr	,
                    rlgatnd	,
                    pray	,
                    lrscale	,
                    rlgblg	,
                    ctzcntr	,
                    gndr	,
                    agea	,
                    marsts	,
                    edulvla	,
                    hinctnt,
                    eisced,
                    hinctnta ,
                    bctprd

)

e1$age<- as.numeric(e1$agea)

e1 <- to_label(e1)


# Recoding e1$name into e1$year
e1$year <- e1$name
e1$year[e1$name == "ESS1e06_5"] <- "2002"
e1$year[e1$name == "ESS2e03_5"] <- "2004"
e1$year[e1$name == "ESS3e03_6"] <- "2006"
e1$year[e1$name == "ESS7e02_1"] <- "2014"
e1$year[e1$name == "ESS4e04_4"] <- "2008"
e1$year[e1$name == "ESS5e03_3"] <- "2010"
e1$year[e1$name == "ESS6e02_3"] <- "2012"

## Cutting e1$age into e1$Age_group
e1$Age_group <- cut(e1$age, include.lowest=TRUE,  right=TRUE,
                    breaks=c(13, 29, 41, 53, 66, 123))
## Recoding e1$Age_group into e1$Age_group_rec
e1$Age_group_rec <- as.character(e1$Age_group)
e1$Age_group_rec[e1$Age_group == "(66,123]"] <- "66+"
## Reordering e1$Age_group_rec into e1$Age_group
e1$Age_group <- factor(e1$Age_group_rec, levels=c("[13,29]", "(29,41]", "(41,53]", "(53,66]", "66+"))

## Reordering e1$year
e1$year <- factor(e1$year, levels=c("2002", "2004", "2006", "2008", "2010", "2012", "2014"))


## Recoding e1$edulvla into e1$Education
e1$Education <- as.character(e1$edulvla)
e1$Education[e1$edulvla == "Not possible to harmonise into 5-level ISCED"] <- NA
e1$Education[e1$edulvla == "Less than lower secondary education (ISCED 0-1)"] <- "Low"
e1$Education[e1$edulvla == "Lower secondary education completed (ISCED 2)"] <- "Low"
e1$Education[e1$edulvla == "Upper secondary education completed (ISCED 3)"] <- "Mid"
e1$Education[e1$edulvla == "Post-secondary non-tertiary education completed (ISCED 4)"] <- "Mid"
e1$Education[e1$edulvla == "Tertiary education completed (ISCED 5-6)"] <- "High"
e1$Education[e1$edulvla == "Other"] <- NA
e1$Education[e1$edulvla == "Refusal"] <- NA
e1$Education[e1$edulvla == "Don't know"] <- NA
e1$Education[e1$edulvla == "No answer"] <- NA
## Reordering e1$Education into e1$Education_rec
e1$Education <- factor(e1$Education, levels=c("Low", "Mid", "High"))

## Recoding e1$gndr into e1$Gender
e1$Gender <- as.character(e1$gndr)
e1$Gender[e1$gndr == "No answer"] <- NA

## Recoding e1$rlgdgr into e1$rlgdgr_rec
e1$Religion_Imp <- as.character(e1$rlgdgr)
e1$Religion_Imp[e1$rlgdgr == "1"] <- "Not at all religious"
e1$Religion_Imp[e1$rlgdgr == "2"] <- "Not at all religious"
e1$Religion_Imp[e1$rlgdgr == "3"] <- "Not at all religious"
e1$Religion_Imp[e1$rlgdgr == "4"] <- "Not at all religious"
e1$Religion_Imp[e1$rlgdgr == "5"] <- "Mid"
e1$Religion_Imp[e1$rlgdgr == "6"] <- "Very religious"
e1$Religion_Imp[e1$rlgdgr == "7"] <- "Very religious"
e1$Religion_Imp[e1$rlgdgr == "8"] <- "Very religious"
e1$Religion_Imp[e1$rlgdgr == "9"] <- "Very religious"
e1$Religion_Imp[e1$rlgdgr == "Refusal"] <- NA
e1$Religion_Imp[e1$rlgdgr == "Don't know"] <- NA
e1$Religion_Imp[e1$rlgdgr == "No answer"] <- NA


## Recoding e1$lrscale into e1$Politics
e1$Politics <- as.character(e1$lrscale)
e1$Politics[e1$lrscale == "1"] <- "Left"
e1$Politics[e1$lrscale == "2"] <- "Left"
e1$Politics[e1$lrscale == "3"] <- "Left"
e1$Politics[e1$lrscale == "4"] <- "Left"
e1$Politics[e1$lrscale == "5"] <- "Center"
e1$Politics[e1$lrscale == "6"] <- "Right"
e1$Politics[e1$lrscale == "7"] <- "Right"
e1$Politics[e1$lrscale == "8"] <- "Right"
e1$Politics[e1$lrscale == "9"] <- "Right"
e1$Politics[e1$lrscale == "Refusal"] <- NA
e1$Politics[e1$lrscale == "Don't know"] <- NA
e1$Politics[e1$lrscale == "No answer"] <- NA

## Reordering e1$Politics
e1$Politics <- factor(e1$Politics, levels=c( "Center", "Left", "Right"))

## Recoding e1$lrscale into e1$Politics_Cont
e1$Politics_Cont <- as.character(e1$lrscale)
e1$Politics_Cont[e1$lrscale == "Left"] <- "0"
e1$Politics_Cont[e1$lrscale == "Right"] <- "10"
e1$Politics_Cont[e1$lrscale == "Refusal"] <- NA
e1$Politics_Cont[e1$lrscale == "Don't know"] <- NA
e1$Politics_Cont[e1$lrscale == "No answer"] <- NA
e1$Politics_Cont <- as.numeric(e1$Politics_Cont)

e1$cntry<-as.character(e1$cntry)
## Recoding e1$cntry into e1$West
e1$West <- as.character(e1$cntry)
e1$West[e1$cntry == "Austria"] <- "Not West"
e1$West[e1$cntry == "Belgium"] <- "West"
e1$West[e1$cntry == "Bulgaria"] <- "Not West"
e1$West[e1$cntry == "Switzerland"] <- "West"
e1$West[e1$cntry == "Cyprus"] <- "Not West"
e1$West[e1$cntry == "Czech Republic"] <- "Not West"
e1$West[e1$cntry == "Germany"] <- "West"
e1$West[e1$cntry == "Denmark"] <- "West"
e1$West[e1$cntry == "Estonia"] <- "Not West"
e1$West[e1$cntry == "Spain"] <- "West"
e1$West[e1$cntry == "Finland"] <- "West"
e1$West[e1$cntry == "France"] <- "West"
e1$West[e1$cntry == "United Kingdom"] <- "West"
e1$West[e1$cntry == "Greece"] <- "Not West"
e1$West[e1$cntry == "Croatia"] <- "Not West"
e1$West[e1$cntry == "Ireland"] <- "West"
e1$West[e1$cntry == "Israel"] <- "Not West"
e1$West[e1$cntry == "Iceland"] <- "West"
e1$West[e1$cntry == "Italy"] <- "West"
e1$West[e1$cntry == "Lithuania"] <- "Not West"
e1$West[e1$cntry == "Luxembourg"] <- "Not West"
e1$West[e1$cntry == "Netherlands"] <- "West"
e1$West[e1$cntry == "Norway"] <- "West"
e1$West[e1$cntry == "Poland"] <- "Not West"
e1$West[e1$cntry == "Portugal"] <- "West"
e1$West[e1$cntry == "Russia"] <- "Not West"
e1$West[e1$cntry == "Sweden"] <- "West"
e1$West[e1$cntry == "Slovenia"] <- "Not West"
e1$West[e1$cntry == "Slovakia"] <- "Not West"
e1$West[e1$cntry == "Turkey"] <- "Not West"
e1$West[e1$cntry == "Ukraine"] <- "Not West"


# Income variables is a bit messy to pool across years

e3 <- filter(e1, year == "2002" | year == "2004" |  year == "2006")

## Recoding e3$hinctnt into e3$income
e3$income <- as.character(e3$hinctnt)
e3$income[e3$hinctnt == "J"] <- "Low"
e3$income[e3$hinctnt == "R"] <- "Low"
e3$income[e3$hinctnt == "C"] <- "Low"
e3$income[e3$hinctnt == "M"] <- "Low"
e3$income[e3$hinctnt == "F"] <- "Mid"
e3$income[e3$hinctnt == "S"] <- "Mid"
e3$income[e3$hinctnt == "K"] <- "Mid"
e3$income[e3$hinctnt == "P"] <- "Mid"
e3$income[e3$hinctnt == "D"] <- "High"
e3$income[e3$hinctnt == "H"] <- "High"
e3$income[e3$hinctnt == "U"] <- "High"
e3$income[e3$hinctnt == "N"] <- "High"
e3$income[e3$hinctnt == "Refusal"] <- NA
e3$income[e3$hinctnt == "Don't know"] <- NA
e3$income[e3$hinctnt == "No answer"] <- NA

e4 <- filter(e1, year == "2008" | year == "2010" |  year == "2012"  |  year == "2014")

## Recoding e4$hinctnta into e4$income
e4$income <- as.character(e4$hinctnta)
e4$income[e4$hinctnta == "J - 1st decile"] <- "Low"
e4$income[e4$hinctnta == "R - 2nd decile"] <- "Low"
e4$income[e4$hinctnta == "C - 3rd decile"] <- "Low"
e4$income[e4$hinctnta == "M - 4th decile"] <- "Mid"
e4$income[e4$hinctnta == "F - 5th decile"] <- "Mid"
e4$income[e4$hinctnta == "S - 6th decile"] <- "Mid"
e4$income[e4$hinctnta == "K - 7th decile"] <- "Mid"
e4$income[e4$hinctnta == "P - 8th decile"] <- "High"
e4$income[e4$hinctnta == "D - 9th decile"] <- "High"
e4$income[e4$hinctnta == "H - 10th decile"] <- "High"
e4$income[e4$hinctnta == "Refusal"] <- NA
e4$income[e4$hinctnta == "Don't know"] <- NA
e4$income[e4$hinctnta == "No answer"] <- NA


x <- rbind(e3,e4)

# Make catorical variables FACTORS
## Reordering x$income
x$income <- factor(x$income, levels=c("Low", "Mid", "High"))
## Reordering x$Religion_Imp
x$Religion_Imp <- factor(x$Religion_Imp, levels=c("Not at all religious", "Mid", "Very religious"))## Reordering x$Gender
x$Gender <- factor(x$Gender, levels=c("Female", "Male"))
## Reordering x$cntry
x$cntry <- factor(x$cntry, levels=c("Belgium", "Switzerland", "Germany", "Denmark", "Spain", "Finland", "France", "United Kingdom", "Ireland", "Iceland", "Italy", "Netherlands", "Norway", "Portugal", "Sweden"))}


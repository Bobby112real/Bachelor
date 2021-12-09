#-----------------------------------Bachelorprojekt R-kode-----------------------------------#
#Lavet af Benjamin Kollerup og Benjamin Seddighi
#Sidst opdateret 9/12 kl. 15
#---------------Dependencies----------------
#Alle deaktiveret, men alle noedvendige at koere mindst en gang for at alt virker.
#install.packages("tidyverse")
#install.packages("Hmisc")
#install.packages("ggpubr")
#install.packages("grid")
#etc.
#Hent ADNIMERGE_0.0.1.tar.gz fra LONI
#https://ida.loni.usc.edu/pages/access/studyData.jsp?categoryId=16&subCategoryId=43
#Vaelg ADNIMERGE package for R
#Koer derefter:
#install.packages("../your path/ADNIMERGE_0.0.1.tar.gz", repos=NULL, type="source")
#---------------Libraries---------------
library(tidyverse)
library(ggpubr)
library(Gmisc, quietly = TRUE)
library(ADNIMERGE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)
library(grid)
#Tidyverse indeholder baade dplyr og ggplot2
#-------Udregning af atrofi vaegtet estimat------
atrofi_just <- function(){
  #Udregning af justeringsfaktor for atrofi:
  #Laver vaegtet gennemsnit af Frisoni2010's data!
  
  eJack_et_al2004 <- 1.4
  eKaye_et_al2005 <- 2.2
  eSchott_et_al2005 <- 0.9
  eBarnes_et_al2007 <- 0.3
  eHenneman_et_al2009 <- 2.2
  eMorra_et_al2009 <- 0.7
  
  
  #SE udregning:
  #SE(x)=SD/SQRT(n)
  Jack_et_al2004 <- (1.2/1.35)/sqrt(40)
  Kaye_et_al2005 <- 6/sqrt(88)
  Schott_et_al2005 <- 1/sqrt(19)
  Barnes_et_al2007 <- 0.9/sqrt(19)
  Henneman_et_al2009 <- 1.4/sqrt(34)
  Morra_et_al2009 <- (1.7-(-0.3))/(2*1.96)
  
  #Saetter vaegtning til at vaere w = 1/SE^2
  
  wJack_et_al2004 <- 1/Jack_et_al2004^2
  wKaye_et_al2005 <- 1/Kaye_et_al2005^2
  wSchott_et_al2005 <- 1/Schott_et_al2005^2
  wBarnes_et_al2007 <- 1/Barnes_et_al2007^2
  wHenneman_et_al2009 <- 1/Henneman_et_al2009^2
  wMorra_et_al2009 <- 1/Morra_et_al2009^2
  
  #Weighted estimate = (SUM(w*E))/SUMw
  wJack_et_al2004
  wKaye_et_al2005
  wSchott_et_al2005
  wBarnes_et_al2007
  wHenneman_et_al2009
  wMorra_et_al2009
  
  Atrofi <- round(-1* (((wJack_et_al2004*eJack_et_al2004)+
                          (wKaye_et_al2005*eKaye_et_al2005)+
                          (wSchott_et_al2005*eSchott_et_al2005)+
                          (wBarnes_et_al2007*eBarnes_et_al2007)+
                          (wHenneman_et_al2009*eHenneman_et_al2009)+
                          (wMorra_et_al2009*eMorra_et_al2009))/
                         (wJack_et_al2004+
                            wKaye_et_al2005+
                            wSchott_et_al2005+
                            wBarnes_et_al2007+
                            wHenneman_et_al2009+
                            wMorra_et_al2009))/100,digits = 4)
  
  return(Atrofi)
}
#---------------Variable----------------

#Foerst introduktion af variable til at justere med:
Atrofi_just <- atrofi_just()
Cutoff <- 1.1
Atrofi_maal <- 1 # Skal vaere 1, da vi har besluttet at koere med relativ change.
xlimitmin <- 0.8 
xlimitmax <- 2
ylimitmin <- -0.1
ylimitmax <- 0.1
#Atrofi_maal kan vaere en af foelgende:
# 0     - Hvis den absolutte differens oenskes
# 1     - Hvis den relative differens oenskes (X)
# 2     - Hvis en ratio oenskes
custompalette = c("#d7191c","#a2a62d","#008837","#0571b0","#06070E")
ekstratekst <- "Tilfoej tekst!"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
#---------------Import og opsaetning af dataframes---------------
Roster <- ADNIMERGE::roster
Roster <- transmute(Roster,
                    PTID,
                    RID)
#Det giver mest mening at fjerne duplicates fra vores Roster allerede i starten, saa den ikke giver anledning til at danne flere duplicates naar vi merger.
Roster <- distinct(Roster,PTID,.keep_all = TRUE)

#Import af UC-Berkeley saa vi faar SUVr-vaerdier
UcBerkeley <- ADNIMERGE::ucberkeleyav45
UcBerkeley_Roster <- left_join(UcBerkeley,Roster,by.x="RID",by.y="PTID")
UcBerkeley_Roster <- transmute(UcBerkeley_Roster,
                               PTID,
                               RID,
                               COMPSUVR = SUMMARYSUVR_WHOLECEREBNORM,
                               EXAMDATE_PET = EXAMDATE)

#Har valgt at forsoege at droppe UCSF, Da den kun koerer med ADNI1-data.

#Bruger i stedet University of California Davis!!
Ucdvol <- ADNIMERGE::ucd_wmh
Ucdvol <- transmute(Ucdvol,
                    RID,
                    EXAMDATE,
                    LEFTHIPPO=LEFT_HIPPO,
                    RIGHTHIPPO=RIGHT_HIPPO,
)
Ucdvol$TESTFORDUPES <- with(Ucdvol, paste0(RID, "-", EXAMDATE)) #Kombinerer RID og examdate for at se om der er duplicates
Ucdvol <- distinct(Ucdvol, TESTFORDUPES,.keep_all = TRUE) #Hvilket der ikke er.


#Merger med Roster, og laver baseline og tid2:
Ucdvol_Roster <- left_join(Ucdvol,Roster, by.x="RID",by.y="PTID")
#PTID kobles på og anvendes i stedet for RID, da nogle dataseat kun angiver patienternes PTID.

#1. MRI:
MRI_baseline <- transmute(Ucdvol_Roster,
                          RID,
                          PTID,
                          EXAMDATE_MRI_BASELINE = EXAMDATE,
                          LEFTHIPPO_BASELINE = LEFTHIPPO,
                          RIGHTHIPPO_BASELINE = RIGHTHIPPO,
)
#Merger med data om SUVr:
MRI_SUVR_baseline <- left_join(MRI_baseline,UcBerkeley_Roster,by.x="PTID",by.y="PTID",.keep_all = TRUE)
MRI_SUVR_baseline <- na.omit(MRI_SUVR_baseline)
MRI_SUVR_baseline <- distinct(MRI_SUVR_baseline,PTID,.keep_all = TRUE)

#2. MRI:
MRI_time <- transmute(Ucdvol_Roster,
                      RID,
                      PTID,
                      EXAMDATE_MRI_TIME = EXAMDATE,
                      LEFTHIPPO_TIME = LEFTHIPPO,
                      RIGHTHIPPO_TIME = RIGHTHIPPO,
)

#Merger med data om SUVr:
MRI_SUVR_time <- left_join(MRI_time,UcBerkeley_Roster,by.x="PTID",by.y="PTID",.keep_all = TRUE)
MRI_SUVR_time <- na.omit(MRI_SUVR_time)
MRI_SUVR_time <- arrange(MRI_SUVR_time,desc(EXAMDATE_MRI_TIME))
MRI_SUVR_time <- distinct(MRI_SUVR_time,PTID,.keep_all = TRUE)


#Merger de to MRI+SUVR skanninger til een fil.
MRI_SUVR <- left_join(MRI_SUVR_baseline,MRI_SUVR_time,by.x="PTID",by.y="PTID",.keep_all = TRUE)
MRI_SUVR <- distinct(MRI_SUVR,PTID,.keep_all = TRUE)
MRI_SUVR <- na.omit(MRI_SUVR)
MRI_SUVR <- transmute(MRI_SUVR,
                      RID,
                      PTID,
                      EXAMDATE_MRI_BASELINE,
                      EXAMDATE_MRI_TIME,
                      EXAMDATE_PET,
                      TIME_DIFF = as.numeric(difftime(EXAMDATE_MRI_TIME, EXAMDATE_MRI_BASELINE,units=c("days"))),
                      COMPSUVR,
                      LEFTHIPPO_BASELINE,
                      LEFTHIPPO_TIME,
                      RIGHTHIPPO_BASELINE,
                      RIGHTHIPPO_TIME,
)

#Fjerner folk med 0 dage mellem MRI baseline og MRI time:

MRI_SUVR$TIME_DIFF[MRI_SUVR$TIME_DIFF == "0"] <- NA
MRI_SUVR <- na.omit(MRI_SUVR) 


#---------Tilfoejelse af data om patienterne:---------

#Tilfoejer ADNI protokoldata
adnidata <- ADNIMERGE::adnimerge
adnidata <- transmute(adnidata,
                      RID = as.numeric(RID),
                      AGE,
                      MMSE,
                      GENDER = PTGENDER,
                      DIAGNOSIS = DX.bl,
                      EDUCATION = PTEDUCAT,
                      ADNIPROT = COLPROT)
adnidata <- na.omit(adnidata)
adnidata <- distinct(adnidata, RID,.keep_all = TRUE)

#Og merger ADNIdata ind:
MRI_SUVR<- left_join(MRI_SUVR,adnidata,by.x="RID",by.y="RID",.keep_all = TRUE)

#---------Deaktiveret! Tilfoejelse af psych og neuro data, til ekskludering-------

#------------------------------#
#Fjernede tidligere ADNI3 data, da datasaet om psykisk og neurologisk sygdom ikke inkluderede ADNI3.
#Blev loest ved at vi ikke laengere ekskluderede paa baggrund af dette.

#INGEN GRUND TIL AT FJERNE ADNI3 LAENGERE!
#Data om diagnose, inden ADNI3 fjernelse:
#table(MRI_SUVR$DIAGNOSIS)

#Fjerner folk udelukkende fra ADNI3:
#MRI_SUVR$ADNIPROT[MRI_SUVR$ADNIPROT == "ADNI3"] <- NA
#MRI_SUVR <- na.omit(MRI_SUVR) 

#Data om diagnose, efter ADNI3 fjernelse:
#table(MRI_SUVR$DIAGNOSIS)


#Goer nu det samme med medical info som for ADNIDATA:
Medhist <- ADNIMERGE::medhist
Medhist <- transmute(Medhist,
                     RID,
                     MHPSYCH,
                     MH2NEURL)
Medhist <- na.omit(Medhist)
Medhist <- distinct(Medhist, RID,.keep_all = TRUE)
#PROBLEM! Vi har kun adni 1, 2 og go i denne database, men en god slat er fra ADNI3!
#Tidligere loest problem ved at fjerne ADNI3.
#Nu loest ved ikke at kigge på psych og neuro!
#Merger medhist ind i vores data:
MRI_SUVR <- left_join(MRI_SUVR,Medhist,by.x="RID",by.y="RID",.keep_all = TRUE)

#-----ADNI har allerede ekskluderet signifikante psych og neuro patienter!----#
#Vi skaerer psykiatriske og neurologiske patienter fra.
#MRI_SUVR$MHPSYCH[!MRI_SUVR$MHPSYCH == "No"] <- NA
#MRI_SUVR$MH2NEURL[!MRI_SUVR$MH2NEURL == "No"] <- NA
#MRI_SUVR <- na.omit(MRI_SUVR) 
#Vi faar fjernet en betydelig maengde patienter.

#Data om diagnose, efter psykiatriske og neurologiske patienter fjernelse:
#table(MRI_SUVR$DIAGNOSIS)

#--------------Div. tilfoejelser til dataframes------------

#Tilfoejer parametre som vi har valgt i starten til MRI_SUVR:
invisible(add_column(MRI_SUVR, Atrofi_maal=Atrofi_maal))
invisible(add_column(MRI_SUVR, Atrofi_just=Atrofi_just))
#Invisible goer bare at Rstudio ikke forsoeger at outputte hele dataframen i console output.


#Laver lidt udregninger, og introducerer lefthippo, righthippo og totalhippo, som bliver til det korrekte maal, alt efter hvad atrofi-maalet er sat til!
MRI_SUVR <- transmute(MRI_SUVR,
                      RID,
                      PTID,
                      EXAMDATE_PET,
                      EXAMDATE_MRI_BASELINE,
                      EXAMDATE_MRI_TIME,
                      TIME_DIFF,
                      MRI_PET_TIMEDIFF = as.numeric(difftime(EXAMDATE_PET, EXAMDATE_MRI_BASELINE,units=c("days"))),
                      COMPSUVR,
                      LEFTHIPPO_BASELINE,
                      LEFTHIPPO_TIME,
                      RIGHTHIPPO_BASELINE,
                      RIGHTHIPPO_TIME,
                      DIFF_LEFTHIPPO = LEFTHIPPO_TIME - LEFTHIPPO_BASELINE,
                      DIFF_RIGHTHIPPO = RIGHTHIPPO_TIME - RIGHTHIPPO_BASELINE,
                      DIFF_TOTALHIPPO = (LEFTHIPPO_TIME + RIGHTHIPPO_TIME) - (LEFTHIPPO_BASELINE + RIGHTHIPPO_BASELINE),
                      RELATIVE_DIFF_LEFTHIPPO = (LEFTHIPPO_TIME - LEFTHIPPO_BASELINE) / LEFTHIPPO_BASELINE,
                      RELATIVE_DIFF_RIGHTHIPPO = (RIGHTHIPPO_TIME - RIGHTHIPPO_BASELINE) / RIGHTHIPPO_BASELINE, 
                      RELATIVE_DIFF_TOTALHIPPO = ((LEFTHIPPO_TIME + RIGHTHIPPO_TIME) - (LEFTHIPPO_BASELINE + RIGHTHIPPO_BASELINE)) / (LEFTHIPPO_BASELINE + RIGHTHIPPO_BASELINE),
                      RATIO_LEFTHIPPO = LEFTHIPPO_TIME / LEFTHIPPO_BASELINE,
                      RATIO_RIGHTHIPPO = RIGHTHIPPO_TIME / RIGHTHIPPO_BASELINE,
                      RATIO_TOTALHIPPO = (LEFTHIPPO_TIME + RIGHTHIPPO_TIME) / (LEFTHIPPO_BASELINE + RIGHTHIPPO_BASELINE),
                      DIAGNOSIS,
                      MMSE,
                      AGE,
                      GENDER,
                      EDUCATION,
                      ADNIPROT,
                      Atrofi_maal,
                      Atrofi_just,
                      LEFTHIPPO = DIFF_LEFTHIPPO*ifelse(Atrofi_maal==0, 1, 0) + RELATIVE_DIFF_LEFTHIPPO*ifelse(Atrofi_maal==1, 1, 0) + RATIO_LEFTHIPPO*ifelse(Atrofi_maal==2, 1, 0),
                      RIGHTHIPPO = DIFF_RIGHTHIPPO*ifelse(Atrofi_maal==0, 1, 0) + RELATIVE_DIFF_RIGHTHIPPO*ifelse(Atrofi_maal==1, 1, 0) + RATIO_RIGHTHIPPO*ifelse(Atrofi_maal==2, 1, 0),
                      TOTALHIPPO = DIFF_TOTALHIPPO*ifelse(Atrofi_maal==0, 1, 0) + RELATIVE_DIFF_TOTALHIPPO*ifelse(Atrofi_maal==1, 1, 0) + RATIO_TOTALHIPPO*ifelse(Atrofi_maal==2, 1, 0),
)


#Forenkler MRI_SUVR, og anvender kun det valgte maal for atrofien.
# 0     - Hvis den absolutte differens oenskes
# 1     - Hvis den relative differens oenskes
# 2     - Hvis en ratio oenskes
#Goer dette for at give os en variabel, der kan skifte mellem relativt og absolut maal


#Cutter alt unoedvendigt fra.
MRI_SUVR <- transmute(MRI_SUVR,
                      RID,
                      PTID,
                      TIME_DIFF,
                      MRI_PET_TIMEDIFF,
                      COMPSUVR,
                      LEFTHIPPO,
                      RIGHTHIPPO,
                      TOTALHIPPO,
                      DIAGNOSIS,
                      MMSE,
                      AGE,
                      GENDER,
                      EDUCATION,
                      ADNIPROT,
                      Atrofi_just,
)

#Kommer senere til at bruge justeret MRI_SUVR:
#Vi forsoeger at justere datasaettet, ved at fjerne folk med under 365 dage mellem maalinger.
MRI_SUVR_just <- MRI_SUVR
MRI_SUVR_just$TIME_DIFF[MRI_SUVR$TIME_DIFF <= 365] <- NA
MRI_SUVR_just <- na.omit(MRI_SUVR_just)

#Data om diagnose, inden justering:
table(MRI_SUVR$DIAGNOSIS)
#Data om diagnose, efter justering:
table(MRI_SUVR_just$DIAGNOSIS)

#mean og standard deviation for dem der fjernes ved justering:
mean(MRI_SUVR$TIME_DIFF[MRI_SUVR$TIME_DIFF <= 365])
sd(MRI_SUVR$TIME_DIFF[MRI_SUVR$TIME_DIFF <= 365])
#minimumsvaerdien og maximumsvaerdien foer justeringen:
min(MRI_SUVR$TIME_DIFF)
max(MRI_SUVR$TIME_DIFF)
#Muligvis problematisk fjernelse af data?

#---------------Opstilling af funktioner---------------

Timediff <- as.numeric(MRI_SUVR$TIME_DIFF)
CompSUVR <- as.data.frame(MRI_SUVR$COMPSUVR)

#Vaelger at lave vores regressioner som funktioner, saa de kan anvendes igen, uden at skulle skrives igen.

LeftRegression <- function(MRI_SUVR,ekstratekst,xlimval,ylimval){
  #Left side
  
  #Left hippocampus aendring i volume (absolut diff, relativ diff el. ratio)
  LeftHippoRegression <- transmute(MRI_SUVR,
                                   LEFTVOLCHANGE = na.omit((LEFTHIPPO/TIME_DIFF*365)-Atrofi_just),
                                   COMPSUVR,
                                   Atrofi_maal,
  )
  
  #Laver noget lidt kringlet her, for at faa en dataframe med regressionen, saa vi forhaabentligt kan putte det ind paa billederne!
  LeftHippoRegression_Formel <- ls.print(lsfit(LeftHippoRegression$COMPSUVR,LeftHippoRegression$LEFTVOLCHANGE))
  LeftHippoRegression_Formel <- as.data.frame(LeftHippoRegression_Formel)
  
  
  return(print(ggplot(LeftHippoRegression,aes(COMPSUVR,LEFTVOLCHANGE))
               +geom_point()
               +geom_smooth(method='lm',formula=y~x)
               +xlab("ß-Amyloid SUVr")
               +ylab("Left hippocampus relative volume change")
               +coord_cartesian(
                 xlim = xlimval,
                 ylim = ylimval)
               +ggtitle(ifelse(Atrofi_maal==0, ("Absolute change in left hippocampal volume per year, as a function of composite SUVr"), 
                               ifelse(Atrofi_maal==1, ("Relative change in left hippocampal volume per year, as a function of composite SUVr"), 
                                      ifelse(Atrofi_maal==2, ("Left hippocampal volume ratio per year, as a function of composite SUVr"), 
                                             "Fejl"))),ekstratekst)
               +annotation_custom(grobTree(textGrob(x=0.02,  y=0.9, hjust=0, paste("y=",round(LeftHippoRegression_Formel[2,7],digits=3),
                                                                                   "* x +",round(LeftHippoRegression_Formel[1,7],digits=3),
                                                                                   "  R^2=",LeftHippoRegression_Formel[1,2],
                                                                                   " T-value=",round(LeftHippoRegression_Formel[2,9],digits=3),
                                                                                   " P-value=",round(LeftHippoRegression_Formel[2,10],digits=4)),
                                                    gp=gpar(col="red", fontsize=16, fontface="italic"))))
  ))
}

RightRegression <- function(MRI_SUVR,ekstratekst,xlimval,ylimval){
  #Right side
  
  #Right hippocampus aendring i volume (absolut diff, relativ diff el. ratio)
  RightHippoRegression <- transmute(MRI_SUVR,
                                    RIGHTVOLCHANGE = na.omit((RIGHTHIPPO/TIME_DIFF*365)-Atrofi_just),
                                    COMPSUVR,
                                    Atrofi_maal,
  )
  
  #Laver noget lidt kringlet her, for at faa en dataframe med regressionen, saa vi forhaabentligt kan putte det ind paa billederne!
  RightHippoRegression_Formel <- ls.print(lsfit(RightHippoRegression$COMPSUVR,RightHippoRegression$RIGHTVOLCHANGE))
  RightHippoRegression_Formel <- as.data.frame(RightHippoRegression_Formel)
  
  
  return(print(ggplot(RightHippoRegression,aes(COMPSUVR,RIGHTVOLCHANGE))
               +geom_point()
               +geom_smooth(method='lm',formula=y~x)
               +xlab("ß-Amyloid SUVr")
               +ylab("Right hippocampus relative volume change")
               +coord_cartesian(
                 xlim = xlimval,
                 ylim = ylimval)
               +ggtitle(ifelse(Atrofi_maal==0, ("Absolute change in right hippocampal volume per year, as a function of composite SUVr"), 
                               ifelse(Atrofi_maal==1, ("Relative change in right hippocampal volume per year, as a function of composite SUVr"), 
                                      ifelse(Atrofi_maal==2, ("Right hippocampal volume ratio per year, as a function of composite SUVr"), 
                                             "Fejl"))),ekstratekst)
               +annotation_custom(grobTree(textGrob(x=0.02,  y=0.9, hjust=0, paste("y=",round(RightHippoRegression_Formel[2,7],digits=3),
                                                                                   "* x +",round(RightHippoRegression_Formel[1,7],digits=3),
                                                                                   "  R^2=",RightHippoRegression_Formel[1,2],
                                                                                   " T-value=",round(RightHippoRegression_Formel[2,9],digits=3),
                                                                                   " P-value=",round(RightHippoRegression_Formel[2,10],digits=4)),
                                                    gp=gpar(col="red", fontsize=16, fontface="italic"))))
  ))
}

TotalRegression <- function(MRI_SUVR,ekstratekst,xlimval,ylimval){   
  #Both sides
  
  #Total hippocampus aendring i volume (absolut diff, relativ diff el. ratio)
  TotalHippoRegression <- transmute(MRI_SUVR,
                                    TOTALVOLCHANGE = na.omit((TOTALHIPPO/TIME_DIFF*365)-Atrofi_just),
                                    COMPSUVR,
                                    Atrofi_maal
  )
  
  #Laver noget lidt kringlet her, for at faa en dataframe med regressionen, saa vi forhaabentligt kan putte det ind paa billederne!
  TotalHippoRegression_Formel <- ls.print(lsfit(TotalHippoRegression$COMPSUVR,TotalHippoRegression$TOTALVOLCHANGE))
  TotalHippoRegression_Formel <- as.data.frame(TotalHippoRegression_Formel)
  
  
  return(print(ggplot(TotalHippoRegression,aes(COMPSUVR,TOTALVOLCHANGE))
               +geom_point()
               +geom_smooth(method='lm',formula=y~x)
               +xlab("ß-Amyloid SUVr")
               +ylab("Relative change in total hippocampal volume per year")
               +coord_cartesian(
                 xlim = xlimval,
                 ylim = ylimval)
               +ggtitle(ifelse(Atrofi_maal==0, ("Absolute change in total hippocampal volume per year, as a function of composite SUVr"), 
                               ifelse(Atrofi_maal==1, ("Relative change in total hippocampal volume per year, as a function of composite SUVr"), 
                                      ifelse(Atrofi_maal==2, ("Total hippocampal volume ratio per year, as a function of composite SUVr"), 
                                             "Fejl"))),ekstratekst)
               +annotation_custom(grobTree(textGrob(x=0.02,  y=0.9, hjust=0, paste("y=",round(TotalHippoRegression_Formel[2,7],digits=3),
                                                                                   ifelse(TotalHippoRegression_Formel[1,7]<0,"* x ","* x +"),round(TotalHippoRegression_Formel[1,7],digits=3),
                                                                                   "  R^2=",TotalHippoRegression_Formel[1,2],
                                                                                   " T-value=",round(TotalHippoRegression_Formel[2,9],digits=3),
                                                                                   " P-value=",ifelse(TotalHippoRegression_Formel[2,10]<0.001,formatC(TotalHippoRegression_Formel[2,10], format = "f", digits=4),round(TotalHippoRegression_Formel[2,10],digits=4))),
                                                    gp=gpar(col="red", fontsize=16, fontface="italic")))
               )))
}

#---------------Stratificering----------------
#---------------Diagnose-stratificering-----------------#
#Diagnoser, i niveau af alvorlighedsgrad:
#CN   - Clinically normal
#SMC  - Significant Memory Concerns
#EMCI - Early Mild Cognitive Impairment
#LMCI - Late Mild Cognitive Impairment
#AD   - Alzheimer's Disease

MRI_SUVR_CN <- MRI_SUVR_just
MRI_SUVR_CN$DIAGNOSIS[!MRI_SUVR_CN$DIAGNOSIS == 'CN'] <- NA
MRI_SUVR_CN <- na.omit(MRI_SUVR_CN)

MRI_SUVR_SMC <- MRI_SUVR_just
MRI_SUVR_SMC$DIAGNOSIS[!MRI_SUVR_SMC$DIAGNOSIS == 'SMC'] <- NA
MRI_SUVR_SMC <- na.omit(MRI_SUVR_SMC)

MRI_SUVR_EMCI <- MRI_SUVR_just
MRI_SUVR_EMCI$DIAGNOSIS[!MRI_SUVR_EMCI$DIAGNOSIS == 'EMCI'] <- NA
MRI_SUVR_EMCI <- na.omit(MRI_SUVR_EMCI)

MRI_SUVR_LMCI <- MRI_SUVR_just
MRI_SUVR_LMCI$DIAGNOSIS[!MRI_SUVR_LMCI$DIAGNOSIS == 'LMCI'] <- NA
MRI_SUVR_LMCI <- na.omit(MRI_SUVR_LMCI)

MRI_SUVR_AD <- MRI_SUVR_just
MRI_SUVR_AD$DIAGNOSIS[!MRI_SUVR_AD$DIAGNOSIS == 'AD'] <- NA
MRI_SUVR_AD <- na.omit(MRI_SUVR_AD)
#---------------Koens-stratificering---------------#

MRI_SUVR_male <- MRI_SUVR_just
MRI_SUVR_male$GENDER[!MRI_SUVR_male$GENDER == 'Male'] <- NA
MRI_SUVR_male <- na.omit(MRI_SUVR_male)

MRI_SUVR_female <- MRI_SUVR_just
MRI_SUVR_female$GENDER[!MRI_SUVR_female$GENDER == 'Female'] <- NA
MRI_SUVR_female <- na.omit(MRI_SUVR_female)

#--------------cutoff-stratificering---------------#

MRI_SUVR_Overcut <- MRI_SUVR_just
MRI_SUVR_Overcut$COMPSUVR[!MRI_SUVR_Overcut$COMPSUVR >= Cutoff ] <- NA
MRI_SUVR_Overcut <- na.omit(MRI_SUVR_Overcut)

MRI_SUVR_Undercut <- MRI_SUVR_just
MRI_SUVR_Undercut$COMPSUVR[!MRI_SUVR_Undercut$COMPSUVR <= Cutoff ] <- NA
MRI_SUVR_Undercut <- na.omit(MRI_SUVR_Undercut)


#---------------Regression---------------
#Ujusteret:
ekstratekst = "Without adjusting for outliers with under 365 days of time between measurements"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
LeftRegression(MRI_SUVR, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR, ekstratekst, xlimval, ylimval)
Regression_ujust <- TotalRegression(MRI_SUVR, ekstratekst, xlimval, ylimval)

#Justeret:
ekstratekst = "adjusted for outliers with under 365 days of time between measurements"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
LeftRegression(MRI_SUVR_just, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR_just, ekstratekst, xlimval, ylimval)
TotalRegression(MRI_SUVR_just, ekstratekst, xlimval, ylimval)

#Vi forsoeger ogsaa at stratificere for koen, diagnose ved baseline, og cut-off:
#Jeg er godt klar over at der er lavet maaaaange grafer, men taenker det goer det lidt lettere i sidste ende at vide hvilke der er bedst at fokusere paa.

#CN:
ekstratekst = "Stratified for diagnosis (CN)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
LeftRegression(MRI_SUVR_CN, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR_CN, ekstratekst, xlimval, ylimval)
Regression_CN <- TotalRegression(MRI_SUVR_CN, ekstratekst, xlimval, ylimval)

#SMC:
ekstratekst = "Stratified for diagnosis (SMC)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
LeftRegression(MRI_SUVR_SMC, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR_SMC, ekstratekst, xlimval, ylimval)
Regression_SMC <- TotalRegression(MRI_SUVR_SMC, ekstratekst, xlimval, ylimval)

#EMCI:
ekstratekst = "Stratified for diagnosis (EMCI)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
LeftRegression(MRI_SUVR_EMCI, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR_EMCI, ekstratekst, xlimval, ylimval)
Regression_EMCI <- TotalRegression(MRI_SUVR_EMCI, ekstratekst, xlimval, ylimval)

#LMCI:
ekstratekst = "Stratified for diagnosis (LMCI)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
LeftRegression(MRI_SUVR_LMCI, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR_LMCI, ekstratekst, xlimval, ylimval)
Regression_LMCI <- TotalRegression(MRI_SUVR_LMCI, ekstratekst, xlimval, ylimval)

#AD:
ekstratekst = "Stratified for diagnosis (AD)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
LeftRegression(MRI_SUVR_AD, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR_AD, ekstratekst, xlimval, ylimval)
Regression_AD <- TotalRegression(MRI_SUVR_AD, ekstratekst, xlimval, ylimval)

#_________________________________

#Male:
ekstratekst = "Stratified for gender (Male)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
LeftRegression(MRI_SUVR_male, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR_male, ekstratekst, xlimval, ylimval)
Regression_male <- TotalRegression(MRI_SUVR_male, ekstratekst, xlimval, ylimval)

#Female:
ekstratekst = "Stratified for gender (Female)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
LeftRegression(MRI_SUVR_female, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR_female, ekstratekst, xlimval, ylimval)
Regression_female <- TotalRegression(MRI_SUVR_female, ekstratekst, xlimval, ylimval)

#_________________________________

#Over cut-off graensen:
ekstratekst = "Stratified for cut-off (over cut-off value)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)


LeftRegression(MRI_SUVR_Overcut, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR_Overcut, ekstratekst, xlimval, ylimval)
Regression_overcut <- TotalRegression(MRI_SUVR_Overcut, ekstratekst, xlimval, ylimval)

#Under cut-off graensen:
ekstratekst = "Stratified for cut-off (under cut-off value)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)


LeftRegression(MRI_SUVR_Undercut, ekstratekst, xlimval, ylimval)
RightRegression(MRI_SUVR_Undercut, ekstratekst, xlimval, ylimval)
Regression_undercut <- TotalRegression(MRI_SUVR_Undercut, ekstratekst, xlimval, ylimval)

#----------AlternativCutoffstratificering:---------------
#Definerer nogle color palettes:
custompalette <- c("#D55E00", "#0072B2", "#CC79A7", "#F0E442", "#52854C")

Overcut_formel <- transmute(MRI_SUVR_Overcut,
                            TOTALVOLCHANGE = na.omit((TOTALHIPPO/TIME_DIFF*365)-Atrofi_just),
                            COMPSUVR,
                            Atrofi_maal)
Overcut_formel <- ls.print(lsfit(Overcut_formel$COMPSUVR,Overcut_formel$TOTALVOLCHANGE))
Overcut_formel <- as.data.frame(Overcut_formel)


Undercut_formel <- transmute(MRI_SUVR_Undercut,
                             TOTALVOLCHANGE = na.omit((TOTALHIPPO/TIME_DIFF*365)-Atrofi_just),
                             COMPSUVR,
                             Atrofi_maal)
Undercut_formel <- ls.print(lsfit(Undercut_formel$COMPSUVR,Undercut_formel$TOTALVOLCHANGE))
Undercut_formel <- as.data.frame(Undercut_formel)

MRI_SUVR_cutoff <- MRI_SUVR_just
MRI_SUVR_cutoff$cutoff[MRI_SUVR_cutoff$COMPSUVR < Cutoff ] <- "Under cut-off"
MRI_SUVR_cutoff$cutoff[MRI_SUVR_cutoff$COMPSUVR >= Cutoff ] <- "Over cut-off"
MRI_SUVR_cutoff$cutoff <- as.factor(MRI_SUVR_cutoff$cutoff)

ekstratekst = "Stratified for cut-off"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)


MRI_SUVR_cutoff <- transmute(MRI_SUVR_cutoff,
                             TOTALVOLCHANGE = na.omit((TOTALHIPPO/TIME_DIFF*365)-Atrofi_just),
                             COMPSUVR,
                             Atrofi_maal,
                             cutoff)

print(ggplot(MRI_SUVR_cutoff,aes(COMPSUVR,TOTALVOLCHANGE,fill = cutoff, shape = cutoff, color = cutoff))
      +theme_bw() #Vi kan koere denne på alle de andre ogsaa!
      +geom_point(size = 1.5, )
      +scale_color_manual(values = custompalette)
      +theme(legend.position = "top")
      +geom_vline(xintercept=Cutoff)
      +geom_smooth(method='lm', formula = y~x)
      +geom_smooth(method='lm', formula = y~x, color="#00000050", se=FALSE) #Bruges til at goere regressionsstregen mere tydelig ift. punkter i baggrunden!
      +xlab("ß-Amyloid SUVr")
      +ylab("Relative change in total hippocampal volume per year")
      +scale_x_continuous(breaks = seq(xlimitmin, xlimitmax, by = 0.1))
      +coord_cartesian(
        xlim = xlimval,
        ylim = ylimval)
      +ggtitle(ifelse(Atrofi_maal==0, ("Absolute change in total hippocampal volume per year, as a function of composite SUVr"), 
                      ifelse(Atrofi_maal==1, ("Relative change in total hippocampal volume per year, as a function of composite SUVr"), 
                             ifelse(Atrofi_maal==2, ("Total hippocampal volume ratio per year, as a function of composite SUVr"), 
                                    "Fejl"))),ekstratekst)
      +annotation_custom(grobTree(textGrob(x=0.02,  y=0.95, hjust=0, paste("y=",round(Undercut_formel[2,7],digits=3),
                                                                           ifelse(Undercut_formel[1,7]<0,"* x ","* x +"),round(Undercut_formel[1,7],digits=3),
                                                                           "  R^2=",Undercut_formel[1,2],
                                                                           " T-value=",round(Undercut_formel[2,9],digits=3),
                                                                           " P-value=",ifelse(Undercut_formel[2,10]<0.001,formatC(Undercut_formel[2,10], format = "f", digits=4),round(Undercut_formel[2,10],digits=4))),
                                           gp=gpar(col="#0072B2", fontsize=16, fontface="italic")),
                                  textGrob(x=0.02,  y=0.90, hjust=0, paste("y=",round(Overcut_formel[2,7],digits=3),
                                                                           ifelse(Overcut_formel[1,7]<0,"* x ","* x +"),round(Overcut_formel[1,7],digits=3),
                                                                           "  R^2=",Overcut_formel[1,2],
                                                                           " T-value=",round(Overcut_formel[2,9],digits=3),
                                                                           " P-value=",ifelse(Overcut_formel[2,10]<0.001,formatC(Overcut_formel[2,10], format = "f", digits=4),round(Overcut_formel[2,10],digits=4))),
                                           gp=gpar(col="#D55E00", fontsize=16, fontface="italic"))))
)
#Width:800 Height:500
ggsave(filename = "Rplot strat for cutoff.png", device = "png", width =7.9 , height =5.7 , dpi=700)





#_________________________________


#---------------Boxplots---------------

#Fortsaetter udelukkende med MRI_SUVR_just:
MRI_SUVR_boxplot <- MRI_SUVR_just

#Stratificerer data:
MRI_SUVR_boxplot$COMPSUVR <- cut(MRI_SUVR_boxplot$COMPSUVR, br=c(-Inf,Cutoff,Inf), labels=c("under cut-off","over cut-off"))
table(MRI_SUVR_boxplot$COMPSUVR)
table(MRI_SUVR_boxplot$GENDER)
table(MRI_SUVR_boxplot$DIAGNOSIS)
#Tilfoejer en masse dummy-variable til at kunne stratificere paa cut-off og koen eller cut-off og diagnose, paa samme tid:
MRI_SUVR_boxplot <- mutate(MRI_SUVR_boxplot,
                           CUTOFF_DIAGNOSIS = paste(COMPSUVR,",", DIAGNOSIS),
                           CUTOFF_GENDER = paste(COMPSUVR,",", GENDER),
                           CN_CUTOFF = NA,
                           SMC_CUTOFF = NA,
                           EMCI_CUTOFF = NA,
                           LMCI_CUTOFF = NA,
                           AD_CUTOFF = NA,
                           MALE_CUTOFF = NA,
                           FEMALE_CUTOFF = NA,
)
#Og forsoeger nu at tilfoeje dem:
MRI_SUVR_boxplot$CN_CUTOFF[MRI_SUVR_boxplot$CUTOFF_DIAGNOSIS == "over cut-off , CN"] <- "over cut-off, CN"
MRI_SUVR_boxplot$CN_CUTOFF[MRI_SUVR_boxplot$CUTOFF_DIAGNOSIS == "under cut-off , CN"] <- "under cut-off, CN"
boxplot_CNCUTOFF <- transmute(MRI_SUVR_boxplot,COMPSUVR,LEFTHIPPO,RIGHTHIPPO,TOTALHIPPO,CN_CUTOFF)
boxplot_CNCUTOFF <- na.omit(boxplot_CNCUTOFF)

MRI_SUVR_boxplot$SMC_CUTOFF[MRI_SUVR_boxplot$CUTOFF_DIAGNOSIS == "over cut-off , SMC"] <- "over cut-off, SMC"
MRI_SUVR_boxplot$SMC_CUTOFF[MRI_SUVR_boxplot$CUTOFF_DIAGNOSIS == "under cut-off , SMC"] <- "under cut-off, SMC"
boxplot_SMCCUTOFF <- transmute(MRI_SUVR_boxplot,COMPSUVR,LEFTHIPPO,RIGHTHIPPO,TOTALHIPPO,SMC_CUTOFF)
boxplot_SMCCUTOFF <- na.omit(boxplot_SMCCUTOFF)

MRI_SUVR_boxplot$EMCI_CUTOFF[MRI_SUVR_boxplot$CUTOFF_DIAGNOSIS == "over cut-off , EMCI"] <- "over cut-off, EMCI"
MRI_SUVR_boxplot$EMCI_CUTOFF[MRI_SUVR_boxplot$CUTOFF_DIAGNOSIS == "under cut-off , EMCI"] <- "under cut-off, EMCI"
boxplot_EMCICUTOFF <- transmute(MRI_SUVR_boxplot,COMPSUVR,LEFTHIPPO,RIGHTHIPPO,TOTALHIPPO,EMCI_CUTOFF)
boxplot_EMCICUTOFF <- na.omit(boxplot_EMCICUTOFF)

MRI_SUVR_boxplot$LMCI_CUTOFF[MRI_SUVR_boxplot$CUTOFF_DIAGNOSIS == "over cut-off , LMCI"] <- "over cut-off, LMCI"
MRI_SUVR_boxplot$LMCI_CUTOFF[MRI_SUVR_boxplot$CUTOFF_DIAGNOSIS == "under cut-off , LMCI"] <- "under cut-off, LMCI"
boxplot_LMCICUTOFF <- transmute(MRI_SUVR_boxplot,COMPSUVR,LEFTHIPPO,RIGHTHIPPO,TOTALHIPPO,LMCI_CUTOFF)
boxplot_LMCICUTOFF <- na.omit(boxplot_LMCICUTOFF)

MRI_SUVR_boxplot$AD_CUTOFF[MRI_SUVR_boxplot$CUTOFF_DIAGNOSIS == "over cut-off , AD"] <- "over cut-off, AD"
MRI_SUVR_boxplot$AD_CUTOFF[MRI_SUVR_boxplot$CUTOFF_DIAGNOSIS == "under cut-off , AD"] <- "under cut-off, AD"
boxplot_ADCUTOFF <- transmute(MRI_SUVR_boxplot,COMPSUVR,LEFTHIPPO,RIGHTHIPPO,TOTALHIPPO,AD_CUTOFF)
boxplot_ADCUTOFF <- na.omit(boxplot_ADCUTOFF)

MRI_SUVR_boxplot$Male_CUTOFF[MRI_SUVR_boxplot$CUTOFF_GENDER == "over cut-off , Male"] <- "over cut-off, Male"
MRI_SUVR_boxplot$Male_CUTOFF[MRI_SUVR_boxplot$CUTOFF_GENDER == "under cut-off , Male"] <- "under cut-off, Male"
boxplot_MaleCUTOFF <- transmute(MRI_SUVR_boxplot,COMPSUVR,LEFTHIPPO,RIGHTHIPPO,TOTALHIPPO,Male_CUTOFF)
boxplot_MaleCUTOFF <- na.omit(boxplot_MaleCUTOFF)

MRI_SUVR_boxplot$Female_CUTOFF[MRI_SUVR_boxplot$CUTOFF_GENDER == "over cut-off , Female"] <- "over cut-off, Female"
MRI_SUVR_boxplot$Female_CUTOFF[MRI_SUVR_boxplot$CUTOFF_GENDER == "under cut-off , Female"] <- "under cut-off, Female"
boxplot_FemaleCUTOFF <- transmute(MRI_SUVR_boxplot,COMPSUVR,LEFTHIPPO,RIGHTHIPPO,TOTALHIPPO,Female_CUTOFF)
boxplot_FemaleCUTOFF <- na.omit(boxplot_FemaleCUTOFF)


#Opstiller endnu en gang en funktion:
boxplot_function <- function(boxplot,boxplotcolumn,boxplottekst){
  
  #SUVr Left:
  boxplot_left <- ggplot2::qplot(x=boxplotcolumn,
                                 y=boxplot$LEFTHIPPO,
                                 fill=I("lightblue"),
                                 geom="boxplot", 
                                 xlab="Composite SUVr", 
                                 ylab=ifelse(Atrofi_maal==0, ("Absolute diff. in left hippo volume per year"), 
                                             ifelse(Atrofi_maal==1, ("Relative diff. in left hippo volume per year"), 
                                                    ifelse(Atrofi_maal==2, ("Left hippo volume ratio per year "), 
                                                           "Fejl"))),
                                 main=paste(ifelse(Atrofi_maal==0, ("Absolute change in left hippocampal volume per year"), 
                                                   ifelse(Atrofi_maal==1, ("Relative change in left hippocampal volume per year"), 
                                                          ifelse(Atrofi_maal==2, ("Left hippocampal volume ratio per year"), 
                                                                 "Fejl"))),
                                            ",",boxplottekst),
  )
  
  #SUVr Right:
  boxplot_right <- ggplot2::qplot(x=boxplotcolumn,
                                  y=boxplot$RIGHTHIPPO,
                                  fill=I("lightblue"),
                                  geom="boxplot", 
                                  xlab="Composite SUVr", 
                                  ylab=ifelse(Atrofi_maal==0, ("Absolute diff. in right hippo volume per year"), 
                                              ifelse(Atrofi_maal==1, ("Relative diff. in right hippo volume per year"), 
                                                     ifelse(Atrofi_maal==2, ("Right hipp volume ratio per year "), 
                                                            "Fejl"))),
                                  main=paste(ifelse(Atrofi_maal==0, ("Absolute change in right hippocampal volume per year"), 
                                                    ifelse(Atrofi_maal==1, ("Relative change in right hippocampal volume per year"), 
                                                           ifelse(Atrofi_maal==2, ("Right hippocampal volume ratio per year"), 
                                                                  "Fejl"))),
                                             ",",boxplottekst),
  )
  
  #SUVr Total:
  boxplot_total <- ggplot2::qplot(x=boxplotcolumn,
                                  y=boxplot$TOTALHIPPO,
                                  fill=I("lightblue"),
                                  geom="boxplot", 
                                  xlab="Composite SUVr", 
                                  ylab=ifelse(Atrofi_maal==0, ("Absolute diff. in total hippo volume per year"), 
                                              ifelse(Atrofi_maal==1, ("Relative diff. in total hippo volume per year"), 
                                                     ifelse(Atrofi_maal==2, ("Total hippo volume ratio per year"), 
                                                            "Fejl"))),
                                  main=paste(ifelse(Atrofi_maal==0, ("Absolute change in total hippocampal volume per year"), 
                                                    ifelse(Atrofi_maal==1, ("Relative change in total hippocampal volume per year"), 
                                                           ifelse(Atrofi_maal==2, ("Total hippocampal volume ratio per year"), 
                                                                  "Fejl"))),
                                             ",",boxplottekst),
  )
  
  return(ggarrange(boxplot_left, boxplot_right, boxplot_total))
}

#Kan nu koere funktionen, og kun aendre lidt i dens parametre:
#CN:
boxplot = boxplot_CNCUTOFF
boxplotcolumn = boxplot_CNCUTOFF$CN_CUTOFF
boxplottekst = "stratified for SUVr cut-off and diagnosis (CN)"
boxplot_function(boxplot,boxplotcolumn,boxplottekst)

#SMC:
boxplot = boxplot_SMCCUTOFF
boxplotcolumn = boxplot_SMCCUTOFF$SMC_CUTOFF
boxplottekst = "stratified for SUVr cut-off and diagnosis (SMC)"
boxplot_function(boxplot,boxplotcolumn,boxplottekst)

#EMCI:
boxplot = boxplot_EMCICUTOFF
boxplotcolumn = boxplot_EMCICUTOFF$EMCI_CUTOFF
boxplottekst = "stratified for SUVr cut-off and diagnosis (EMCI)"
boxplot_function(boxplot,boxplotcolumn,boxplottekst)

#LMCI:
boxplot = boxplot_LMCICUTOFF
boxplotcolumn = boxplot_LMCICUTOFF$LMCI_CUTOFF
boxplottekst = "stratified for SUVr cut-off and diagnosis (LMCI)"
boxplot_function(boxplot,boxplotcolumn,boxplottekst)

#AD:
boxplot = boxplot_ADCUTOFF
boxplotcolumn = boxplot_ADCUTOFF$AD_CUTOFF
boxplottekst = "stratified for SUVr cut-off and diagnosis (AD)"
boxplot_function(boxplot,boxplotcolumn,boxplottekst)

#Male:
boxplot = boxplot_MaleCUTOFF
boxplotcolumn = boxplot_MaleCUTOFF$Male_CUTOFF
boxplottekst = "stratified for SUVr cut-off and gender (Male)"
boxplot_function(boxplot,boxplotcolumn,boxplottekst)

#Female:
boxplot = boxplot_FemaleCUTOFF
boxplotcolumn = boxplot_FemaleCUTOFF$Female_CUTOFF
boxplottekst = "stratified for SUVr cut-off and gender (Female)"
boxplot_function(boxplot,boxplotcolumn,boxplottekst)

#---------------MMSE-----

#Regression:
#Total hippocampus aendring i volume (absolut diff, relativ diff el. ratio)
MMSEHippoVol <- transmute(MRI_SUVR_just,
                          TOTALVOLCHANGE = na.omit((TOTALHIPPO/TIME_DIFF*365)-Atrofi_just),
                          COMPSUVR,
                          DIAGNOSIS,
                          DIAGNOSISCUT = DIAGNOSIS,
                          MMSE,
                          Atrofi_maal
)

#Laver noget lidt kringlet her, for at faa en dataframe med regressionen, saa vi forhaabentligt kan putte det ind paa billederne!
MMSEHippoRegression_Formel <- ls.print(lsfit(MMSEHippoVol$MMSE,MMSEHippoVol$TOTALVOLCHANGE))
MMSEHippoRegression_Formel <- as.data.frame(MMSEHippoRegression_Formel)
print(ggplot(MMSEHippoVol,aes(MMSE,TOTALVOLCHANGE))
      +geom_point()
      +geom_smooth(method='lm',formula=y~x+I(x^2))
      +xlab("Baseline Mini-Mental State Examination score")
      +ylab("Rel. change for total hippocampal volume /year")
      +ggtitle(ifelse(Atrofi_maal==0, ("Absolute change in total hippocampal volume per year, as a function of MMSE score at baseline"), 
                      ifelse(Atrofi_maal==1, ("Relative change in total hippocampal volume per year, as a function of MMSE score at baseline"), 
                             ifelse(Atrofi_maal==2, ("Total hippocampal volume ratio per year, as a function of MMSE score at baseline"), 
                                    "Fejl"))),
               #"", #Her kan staa brødtekst
      )
      +annotation_custom(grobTree(textGrob(x=0.02,  y=0.9, hjust=0, paste("y=",round(MMSEHippoRegression_Formel[2,7],digits=3),
                                                                          "* x +",round(MMSEHippoRegression_Formel[1,7],digits=3),
                                                                          "  R^2=",MMSEHippoRegression_Formel[1,2],
                                                                          " T-value=",round(MMSEHippoRegression_Formel[2,9],digits=3),
                                                                          " P-value=",round(MMSEHippoRegression_Formel[2,10],digits=4)),
                                           gp=gpar(col="red", fontsize=16, fontface="italic"))))
)
#Boxplot:
MMSEHippoCut <- cut(MMSEHippoVol$MMSE, breaks=c(0,20,22,24,26,28,100), labels=c("0-20 (n=11)","21-22 (n=33)","23-24 (n=48)","25-26 (n=114)","27-28 (n=220)","29-30 (n=499)"))
table(MMSEHippoCut)

MMSEHippoBox <- ggplot2::qplot(x=MMSEHippoCut,
                               y=MMSEHippoVol$TOTALVOLCHANGE,
                               fill=I("lightblue"),
                               geom="boxplot", 
                               aes(x=MMSEHippoCut,y=MMSEHippoVol$TOTALVOLCHANGE),
                               xlab="Baseline Mini-Mental State Examination Score", 
                               ylab=ifelse(Atrofi_maal==0, ("Absolute change in total hippo volume per year"), 
                                           ifelse(Atrofi_maal==1, ("Relative change in total hippocampus volume per year"), 
                                                  ifelse(Atrofi_maal==2, ("Total hippo volume ratio per year"), 
                                                         "Fejl"))),
                               main=paste(ifelse(Atrofi_maal==0, ("Absolute change in total hippocampal volume per year,"), 
                                                 ifelse(Atrofi_maal==1, ("Relative change in total hippocampal volume per year,"), 
                                                        ifelse(Atrofi_maal==2, ("Total hippocampal volume ratio per year,"), 
                                                               "Fejl"))),"stratified for baseline MMSE score"),
) + theme_bw()
ggarrange(MMSEHippoBox)
ggsave(filename = "Rplot strat for MMSE boxplot hippovol.png", device = "png", width =8 , height =5.7 , dpi=700)

#############---------------#############.


#Maaske interessant at se paa amyloid i stedet for volumenaendring:

MMSESUVRRegression_Formel <- ls.print(lsfit(MMSEHippoVol$MMSE,MMSEHippoVol$COMPSUVR))
MMSESUVRRegression_Formel <- as.data.frame(MMSESUVRRegression_Formel)

print(ggplot(MMSEHippoVol,aes(MMSE,COMPSUVR))
      +geom_point()
      +geom_smooth(method='lm',formula=y~x)
      +xlab("Baseline Mini-Mental State Examination score")
      +ylab("ß-Amyloid SUVr")
      +ggtitle(ifelse(Atrofi_maal==0, ("ß-Amyloid SUVr as a function of MMSE score at baseline"), 
                      ifelse(Atrofi_maal==1, ("ß-Amyloid SUVr as a function of MMSE score at baseline"), 
                             ifelse(Atrofi_maal==2, ("ß-Amyloid SUVr as a function of MMSE score at baseline"), 
                                    "Fejl"))),
               #"", #Her kan staa brødtekst
      )              
      +annotation_custom(grobTree(textGrob(x=0.02,  y=0.9, hjust=0, paste("y=",round(MMSESUVRRegression_Formel[2,7],digits=3),
                                                                          "* x +",round(MMSESUVRRegression_Formel[1,7],digits=3),
                                                                          "  R^2=",MMSESUVRRegression_Formel[1,2],
                                                                          " T-value=",round(MMSESUVRRegression_Formel[2,9],digits=3),
                                                                          " P-value=",round(MMSESUVRRegression_Formel[2,10],digits=4)),
                                           gp=gpar(col="red", fontsize=16, fontface="italic"))))
)
#Boxplot:
MMSESUVRCut <- cut(MMSEHippoVol$MMSE, breaks=c(0,20,22,24,26,28,100), labels=c("0-20 (n=11)","21-22 (n=33)","23-24 (n=48)","25-26 (n=114)","27-28 (n=220)","29-30 (n=499)"))
table(MMSESUVRCut)

MMSESUVRBox <- ggplot2::qplot(x=MMSESUVRCut,
                              y=MMSEHippoVol$COMPSUVR,
                              fill=I("lightblue"),
                              geom="boxplot", 
                              xlab="Baseline Mini-Mental State Examination Score", 
                              ylab=ifelse(Atrofi_maal==0, ("ß-Amyloid SUVr"), 
                                          ifelse(Atrofi_maal==1, ("ß-Amyloid SUVr"), 
                                                 ifelse(Atrofi_maal==2, ("ß-Amyloid SUVr"), 
                                                        "Fejl"))),
                              main=paste(ifelse(Atrofi_maal==0, ("ß-Amyloid SUVr"), 
                                                ifelse(Atrofi_maal==1, ("ß-Amyloid SUVr"), 
                                                       ifelse(Atrofi_maal==2, ("ß-Amyloid SUVr"), 
                                                              "Fejl"))),"stratified for baseline MMSE score"),
)
ggarrange(MMSESUVRBox)
ggsave(filename = "Rplot strat for MMSE boxplot SUVr.png", device = "png", width =7.8 , height =5.7 , dpi=700)
#---------------------#


MMSEHippoVol$DIAGNOSISCUT <- as.factor(MMSEHippoVol$DIAGNOSISCUT)

MMSEDIAGNOSISBox <- (ggplot(MMSEHippoVol,aes(x=DIAGNOSISCUT, y=MMSE))
                     +geom_boxplot(fill="lightblue")
                     +scale_y_continuous(breaks=seq(15,30,1))
                     +xlab("Diagnosis")
                     +ylab("Baseline Mini-Mental State Examination Score")
                     +ggtitle("Baseline Mini-Mental State Examination Score, stratified for diagnosis")
)

ggarrange(MMSEDIAGNOSISBox)




table(MRI_SUVR_just$DIAGNOSIS)
quantile(MRI_SUVR_CN$MMSE)
quantile(MRI_SUVR_SMC$MMSE)

mean(MRI_SUVR_CN$MMSE)
mean(MRI_SUVR_SMC$MMSE)
mean(MRI_SUVR_EMCI$MMSE)
mean(MRI_SUVR_LMCI$MMSE)
mean(MRI_SUVR_AD$MMSE)

sd(MRI_SUVR_CN$MMSE)
sd(MRI_SUVR_SMC$MMSE)
sd(MRI_SUVR_EMCI$MMSE)
sd(MRI_SUVR_LMCI$MMSE)
sd(MRI_SUVR_AD$MMSE)


#--------------Ekstra funktioner, tests mm-------------
ggarrange(
  Regression_male,
  Regression_female,
  Regression_undercut,
  Regression_overcut,
  Regression_CN,
  Regression_SMC,
  Regression_EMCI,
  Regression_LMCI,
  Regression_AD,
  MMSEHippoBox,
  nrow = 5,ncol = 2)

#Export som ca. 1600x2500 pixels
#Compact version: 1550x2000 pixels
#Ekstra
mean(MRI_SUVR_just$AGE)
sd(MRI_SUVR_just$AGE)
mean(MRI_SUVR$AGE)
sd(MRI_SUVR$AGE)
table(MRI_SUVR$DIAGNOSIS)
table(MRI_SUVR_just$DIAGNOSIS)
table(MRI_SUVR_just$GENDER)
table(MRI_SUVR$GENDER)

mean(MRI_SUVR_just$TIME_DIFF)
sd(MRI_SUVR_just$TIME_DIFF)
mean(MRI_SUVR_CN$TIME_DIFF)
mean(MRI_SUVR_SMC$TIME_DIFF)
mean(MRI_SUVR_EMCI$TIME_DIFF)
mean(MRI_SUVR_LMCI$TIME_DIFF)
mean(MRI_SUVR_AD$TIME_DIFF)

sd(MRI_SUVR_CN$TIME_DIFF)
sd(MRI_SUVR_SMC$TIME_DIFF)
sd(MRI_SUVR_EMCI$TIME_DIFF)
sd(MRI_SUVR_LMCI$TIME_DIFF)
sd(MRI_SUVR_AD$TIME_DIFF)

ggarrange(
  Regression_undercut,
  Regression_overcut,
  nrow = 1,ncol = 2)
#Skal eksporteres som 1550x400



#Antal der har en positiv aendring i volumen:
MRI_SUVR_just_2 <- transmute(MRI_SUVR_just,
                             TOTALVOLCHANGE = na.omit((TOTALHIPPO/TIME_DIFF*365)-Atrofi_just))
MRI_SUVR_just_2$TOTALVOLCHANGE[MRI_SUVR_just_2 <0 ] <- NA
MRI_SUVR_just_2 <- na.omit(MRI_SUVR_just_2)



#------------Aendring til andengradsfunktion-------
starttekst = ""
TotalQuadRegression <- function(MRI_SUVR,starttekst,ekstratekst,xlimval,ylimval){   
  #Both sides
  
  #Total hippocampus aendring i volume (absolut diff, relativ diff el. ratio)
  TotalHippoRegression <- transmute(MRI_SUVR,
                                    TOTALVOLCHANGE = na.omit((TOTALHIPPO/TIME_DIFF*365)-Atrofi_just),
                                    COMPSUVR,
                                    Atrofi_maal,
  )
  
  #Laver noget lidt kringlet her, for at faa en dataframe med regressionen, saa vi forhaabentligt kan putte det ind paa billederne!
  
  quadraticfunction = summary(lm(TOTALVOLCHANGE~COMPSUVR+I(COMPSUVR^2),data=TotalHippoRegression))
  quadfunct = data.frame(quadraticfunction[4])
  ftest = data.frame(quadraticfunction[10])
  pval = pf(ftest[1,1],ftest[2,1],ftest[3,1],lower.tail = FALSE)
  
  
  
  return(print(ggplot(TotalHippoRegression,aes(COMPSUVR,TOTALVOLCHANGE)) 
               +geom_point()
               +theme_bw()
               +geom_smooth(method='lm',formula=y~x+I(x^2),se=TRUE)
               +xlab("ß-Amyloid SUVr")
               +ylab("Relative change in total hippocampal volume per year")
               +coord_cartesian(
                 xlim = xlimval,
                 ylim = ylimval)
               +ggtitle(paste(starttekst,ifelse(Atrofi_maal==0, ("Absolute change in total hippocampal volume per year, as a function of composite SUVr"), 
                                                ifelse(Atrofi_maal==1, ("Relative change in total hippocampal volume per year, as a function of composite SUVr"), 
                                                       ifelse(Atrofi_maal==2, ("Total hippocampal volume ratio per year, as a function of composite SUVr"), 
                                                              "Fejl")))),ekstratekst)
               +annotation_custom(grobTree(textGrob(x=0.2,  y=0.9, hjust=0, paste("y=",round(quadfunct[3,1],digits=3),
                                                                                  ifelse(quadfunct[2,1]<0,"x^2"," x^2+"),round(quadfunct[2,1],digits=3),
                                                                                  ifelse(quadfunct[1,1]<0,"x ","x+"),round(quadfunct[1,1],digits=3)),
                                                    gp=gpar(col="red", fontsize=16, fontface="italic")),
                                           grobTree(textGrob(x=0.2,  y=0.1, hjust=0, paste("R^2=",round(quadraticfunction[[8]],digits=3),
                                                                                           ", F-value=",round(ftest[1,1],digits=3),
                                                                                           ", P-value=",ifelse(pval<0.001,formatC(pval, format = "f", digits = 2),round(pval,digits=4))),
                                                             gp=gpar(col="red", fontsize=16, fontface="italic"))))
               )))
}
#-----Koer funktioner igennem igen:------

#CN:
ekstratekst = "Stratified for diagnosis (CN)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
Regression_CN <- TotalQuadRegression(MRI_SUVR_CN,"A:", ekstratekst, xlimval, ylimval)

#SMC:
ekstratekst = "Stratified for diagnosis (SMC)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
Regression_SMC <- TotalQuadRegression(MRI_SUVR_SMC,"B:", ekstratekst, xlimval, ylimval)

#EMCI:
ekstratekst = "Stratified for diagnosis (EMCI)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
Regression_EMCI <- TotalQuadRegression(MRI_SUVR_EMCI,"C:", ekstratekst, xlimval, ylimval)

#LMCI:
ekstratekst = "Stratified for diagnosis (LMCI)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
Regression_LMCI <- TotalQuadRegression(MRI_SUVR_LMCI,"D:", ekstratekst, xlimval, ylimval)
summary(Regression_LMCI)

#AD:
ekstratekst = "Stratified for diagnosis (AD)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
Regression_AD <- TotalQuadRegression(MRI_SUVR_AD,"E:", ekstratekst, xlimval, ylimval)

#_________________________________

#Male:
ekstratekst = "Stratified for gender (Male)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
Regression_male <- TotalQuadRegression(MRI_SUVR_male,"", ekstratekst, xlimval, ylimval)

#Female:
ekstratekst = "Stratified for gender (Female)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)
Regression_female <- TotalQuadRegression(MRI_SUVR_female,"", ekstratekst, xlimval, ylimval)

#_________________________________

#Over cut-off graensen:
ekstratekst = "Stratified for cut-off (over cut-off value)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)


Regression_overcut <- TotalQuadRegression(MRI_SUVR_Overcut,starttekst, ekstratekst, xlimval, ylimval)

#Under cut-off graensen:
ekstratekst = "Stratified for cut-off (under cut-off value)"
xlimval <- c(xlimitmin,xlimitmax)
ylimval <- c(ylimitmin,ylimitmax)


Regression_undercut <- TotalQuadRegression(MRI_SUVR_Undercut,starttekst, ekstratekst, xlimval, ylimval)


ggarrange(
  Regression_male,
  Regression_female,
  nrow = 1,ncol = 2)
# Width= 1550, height = 400
ggsave(filename = "Rplot strat for gender.png", device = "png", width =16.5 , height =5 , dpi=700)
ggarrange(
  Regression_CN,
  Regression_SMC,
  Regression_EMCI,
  Regression_LMCI,
  Regression_AD,
  MMSEHippoBox,
  nrow = 3,ncol = 2)
# Width = 1550, height = 1200
ggsave(filename = "Rplot strat for diagnosis and boxplot.png", device = "png", width =16.5 , height =15 , dpi=700)
#Andengradsfunktioner paa een graf--------------
#With 5 stratifications needing functions:
CompleteDiagnostrat <- function(MRI_SUVR,ekstratekst,xlimval,ylimval){
  return(print(ggplot(MRI_SUVR,aes(COMPSUVR,TOTALVOLCHANGE,group = DIAGNOSIS, fill = DIAGNOSIS, shape = DIAGNOSIS, color = DIAGNOSIS))
               +theme_bw() #Vi kan koere denne på alle de andre ogsaa!
               +scale_color_manual(values = custompalette)
               +theme(legend.position = "top")
               +xlab("ß-Amyloid SUVr")
               +ylab("Relative change in total hippocampal volume per year")
               +coord_cartesian(
                 xlim = xlimval,
                 ylim = ylimval)
               +ggtitle(ifelse(Atrofi_maal==0, ("Absolute change in total hippocampal volume per year, as a function of composite SUVr"), 
                               ifelse(Atrofi_maal==1, ("Relative change in total hippocampal volume per year, as a function of composite SUVr"), 
                                      ifelse(Atrofi_maal==2, ("Total hippocampal volume ratio per year, as a function of composite SUVr"), 
                                             "Fejl"))),ekstratekst)
               +geom_smooth(method='lm', formula = y~x+I(x^2), aes(fill =))
               + scale_x_continuous(breaks = seq(0.8, 2, by = 0.1))
  ))
}


#With 3 stratifications needing functions:
Diagnostrat <- function(MRI_SUVR,ekstratekst,xlimval,ylimval, diagnostrat1, diagnostrat2, diagnostrat3){
  
  quadraticfunction1 = summary(lm(TOTALVOLCHANGE~COMPSUVR+I(COMPSUVR^2),data=diagnostrat1))
  quadfunct1 = data.frame(quadraticfunction1[4])
  ftest1 = data.frame(quadraticfunction1[10])
  pval1 = pf(ftest1[1,1],ftest1[2,1],ftest1[3,1],lower.tail = FALSE)
  
  
  quadraticfunction2 = summary(lm(TOTALVOLCHANGE~COMPSUVR+I(COMPSUVR^2),data=diagnostrat2))
  quadfunct2 = data.frame(quadraticfunction2[4])
  ftest2 = data.frame(quadraticfunction2[10])
  pval2 = pf(ftest2[1,1],ftest2[2,1],ftest2[3,1],lower.tail = FALSE)
  
  
  quadraticfunction3 = summary(lm(TOTALVOLCHANGE~COMPSUVR+I(COMPSUVR^2),data=diagnostrat3))
  quadfunct3 = data.frame(quadraticfunction3[4])
  ftest3 = data.frame(quadraticfunction3[10])
  pval3 = pf(ftest3[1,1],ftest3[2,1],ftest3[3,1],lower.tail = FALSE)
  
  return(print(ggplot(MRI_SUVR,aes(COMPSUVR,TOTALVOLCHANGE,group = DIAGNOSIS, fill = DIAGNOSIS, shape = DIAGNOSIS, color = DIAGNOSIS))
               +theme_bw() #Vi kan koere denne på alle de andre ogsaa!
               +scale_color_manual(values = custompalette)
               +theme(legend.position = "top")
               +xlab("ß-Amyloid SUVr")
               +ylab("Relative change in total hippocampal volume per year")
               +coord_cartesian(
                 xlim = xlimval,
                 ylim = ylimval)
               +ggtitle(ifelse(Atrofi_maal==0, ("Absolute change in total hippocampal volume per year, as a function of composite SUVr"), 
                               ifelse(Atrofi_maal==1, ("Relative change in total hippocampal volume per year, as a function of composite SUVr"), 
                                      ifelse(Atrofi_maal==2, ("Total hippocampal volume ratio per year, as a function of composite SUVr"), 
                                             "Fejl"))),ekstratekst)
               +geom_smooth(method='lm', formula = y~x+I(x^2), aes(fill =))
               + scale_x_continuous(breaks = seq(0.8, 2, by = 0.1))
               +annotation_custom(grobTree(textGrob(x=0.05,  y=0.1, hjust=0, paste("y=",round(quadfunct1[3,1],digits=3),
                                                                                   ifelse(quadfunct1[2,1]<0,"x^2"," x^2+"),round(quadfunct1[2,1],digits=3),
                                                                                   ifelse(quadfunct1[1,1]<0,"x ","x+"),round(quadfunct1[1,1],digits=3),
                                                                                   "  |   R^2=",round(quadraticfunction1[[8]],digits = 3),
                                                                                   ", F-value=",round(ftest1[1,1],digits = 3),
                                                                                   ", P-value=",ifelse(pval1<0.001,formatC(pval1, format = "f", digits = 2),round(pval1,digits=4))),
                                                    gp=gpar(col="#0571b0", fontsize=12, fontface="italic")),
                                           grobTree(textGrob(x=0.05,  y=0.24, hjust=0, paste("y=",round(quadfunct2[3,1],digits=3),
                                                                                             ifelse(quadfunct2[2,1]<0,"x^2"," x^2+"),round(quadfunct2[2,1],digits=3),
                                                                                             ifelse(quadfunct2[1,1]<0,"x ","x+"),round(quadfunct2[1,1],digits=3),
                                                                                             "  |   R^2=",round(quadraticfunction2[[8]],digits = 3),
                                                                                             ", F-value=",round(ftest2[1,1],digits = 3),
                                                                                             ", P-value=",ifelse(pval2<0.001,formatC(pval2, format = "f", digits = 2),round(pval2,digits=4))),
                                                             gp=gpar(col="#d7191c", fontsize=12, fontface="italic")),
                                                    grobTree(textGrob(x=0.05,  y=0.17, hjust=0, paste("y=",round(quadfunct3[3,1],digits=3),
                                                                                                      ifelse(quadfunct3[2,1]<0,"x^2"," x^2+"),round(quadfunct3[2,1],digits=3),
                                                                                                      ifelse(quadfunct3[1,1]<0,"x ","x+"),round(quadfunct3[1,1],digits=3),
                                                                                                      "  |   R^2=",round(quadraticfunction3[[8]],digits = 3),
                                                                                                      ", F-value=",round(ftest3[1,1],digits = 3),
                                                                                                      ", P-value=",ifelse(pval3<0.001,formatC(pval3, format = "f", digits = 2),round(pval3,digits=4))),
                                                                      gp=gpar(col="#008837", fontsize=12, fontface="italic"))
                                                    ))))
  ))
}
#With 2 stratifications needing functions (gender):
Genderstrat <- function(MRI_SUVR,ekstratekst,xlimval,ylimval, MRI_SUVR_male, MRI_SUVR_female){
  
  quadraticfunction1 = summary(lm(TOTALVOLCHANGE~COMPSUVR+I(COMPSUVR^2),data=MRI_SUVR_male))
  quadfunct1 = data.frame(quadraticfunction1[4])
  ftest1 = data.frame(quadraticfunction1[10])
  pval1 = pf(ftest1[1,1],ftest1[2,1],ftest1[3,1],lower.tail = FALSE)
  
  
  quadraticfunction2 = summary(lm(TOTALVOLCHANGE~COMPSUVR+I(COMPSUVR^2),data=MRI_SUVR_female))
  quadfunct2 = data.frame(quadraticfunction2[4])
  ftest2 = data.frame(quadraticfunction2[10])
  pval2 = pf(ftest2[1,1],ftest2[2,1],ftest2[3,1],lower.tail = FALSE)
  
  
  return(print(ggplot(MRI_SUVR,aes(COMPSUVR,TOTALVOLCHANGE,group = GENDER, fill = GENDER, shape = GENDER, color = GENDER))
               +theme_bw() #Vi kan koere denne på alle de andre ogsaa!
               +scale_color_manual(values = custompalette)
               +theme(legend.position = "top")
               +xlab("ß-Amyloid SUVr")
               +ylab("Relative change in total hippocampal volume per year")
               +coord_cartesian(
                 xlim = xlimval,
                 ylim = ylimval)
               +ggtitle(ifelse(Atrofi_maal==0, ("Absolute change in total hippocampal volume per year, as a function of composite SUVr"), 
                               ifelse(Atrofi_maal==1, ("Relative change in total hippocampal volume per year, as a function of composite SUVr"), 
                                      ifelse(Atrofi_maal==2, ("Total hippocampal volume ratio per year, as a function of composite SUVr"), 
                                             "Fejl"))),ekstratekst)
               +geom_smooth(method='lm', formula = y~x+I(x^2), aes(fill =))
               +annotation_custom(grobTree(textGrob(x=0.05,  y=0.9, hjust=0, paste("y=",round(quadfunct1[3,1],digits=3),
                                                                                   ifelse(quadfunct1[2,1]<0,"x^2"," x^2+"),round(quadfunct1[2,1],digits=3),
                                                                                   ifelse(quadfunct1[1,1]<0,"x ","x+"),round(quadfunct1[1,1],digits=3),
                                                                                   "  |   R^2=",round(quadraticfunction1[[8]],digits = 3),
                                                                                   ", F-value=",round(ftest1[1,1],digits = 3),
                                                                                   ", P-value=",ifelse(pval1<0.001,formatC(pval1, format = "f", digits = 2),round(pval1,digits=4))),
                                                    gp=gpar(col="#0571b0", fontsize=12, fontface="italic")), 
                                           grobTree(textGrob(x=0.05,  y=0.85, hjust=0, paste("y=",round(quadfunct2[3,1],digits=3),
                                                                                             ifelse(quadfunct2[2,1]<0,"x^2"," x^2+"),round(quadfunct2[2,1],digits=3),
                                                                                             ifelse(quadfunct2[1,1]<0,"x ","x+"),round(quadfunct2[1,1],digits=3),
                                                                                             "  |   R^2=",round(quadraticfunction2[[8]],digits = 3),
                                                                                             ", F-value=",round(ftest2[1,1],digits = 3),
                                                                                             ", P-value=",ifelse(pval2<0.001,formatC(pval2, format = "f", digits = 2),round(pval2,digits=4))),
                                                             gp=gpar(col="#d7191c", fontsize=12, fontface="italic"))
                                           )))
  ))
}




ekstratekst = "Stratified for diagnosis"

MRI_SUVR_diagnostrat <- transmute(MRI_SUVR_just,
                                  TOTALVOLCHANGE = na.omit((TOTALHIPPO/TIME_DIFF*365)-Atrofi_just),
                                  COMPSUVR,
                                  Atrofi_maal,
                                  DIAGNOSIS,
                                  GENDER)
custompalette = c("#d7191c","#a2a62d","#008837","#0571b0","#06070E")

CompleteDiagnostrat(MRI_SUVR_diagnostrat,ekstratekst, c(0.8,2), c(-0.075,0.025))
# Width: 750, height = 550
ggsave(filename = "Rplot strat for diagnosis.png", device = "png", width =7.8 , height =5.7 , dpi=700)

MRI_SUVR_diagnostrat_AD_MCI_CN <- MRI_SUVR_diagnostrat

MRI_SUVR_diagnostrat_AD_MCI_CN$DIAGNOSIS <- as.character(MRI_SUVR_diagnostrat_AD_MCI_CN$DIAGNOSIS)
MRI_SUVR_diagnostrat_AD_MCI_CN$DIAGNOSIS[MRI_SUVR_diagnostrat_AD_MCI_CN$DIAGNOSIS == "SMC"] <- NA
MRI_SUVR_diagnostrat_AD_MCI_CN$DIAGNOSIS[MRI_SUVR_diagnostrat_AD_MCI_CN$DIAGNOSIS == "EMCI"] <- "MCI"
MRI_SUVR_diagnostrat_AD_MCI_CN$DIAGNOSIS[MRI_SUVR_diagnostrat_AD_MCI_CN$DIAGNOSIS == "LMCI"] <- "MCI"
MRI_SUVR_diagnostrat_AD_MCI_CN <- na.omit(MRI_SUVR_diagnostrat_AD_MCI_CN)
#Stratifying til at kunne skrive funktioner:
diagnostrat1 <- MRI_SUVR_diagnostrat_AD_MCI_CN # MCI
diagnostrat1$DIAGNOSIS[diagnostrat1$DIAGNOSIS == "CN"] <- NA
diagnostrat1$DIAGNOSIS[diagnostrat1$DIAGNOSIS == "AD"] <- NA
diagnostrat1 <- na.omit(diagnostrat1)

diagnostrat2 <- MRI_SUVR_diagnostrat_AD_MCI_CN #AD
diagnostrat2$DIAGNOSIS[diagnostrat2$DIAGNOSIS == "MCI"] <- NA
diagnostrat2$DIAGNOSIS[diagnostrat2$DIAGNOSIS == "CN"] <- NA
diagnostrat2 <- na.omit(diagnostrat2)

diagnostrat3 <- MRI_SUVR_diagnostrat_AD_MCI_CN #CN
diagnostrat3$DIAGNOSIS[diagnostrat3$DIAGNOSIS == "AD"] <- NA
diagnostrat3$DIAGNOSIS[diagnostrat3$DIAGNOSIS == "MCI"] <- NA
diagnostrat3 <- na.omit(diagnostrat3)



custompalette = c("#d7191c","#008837","#0571b0")

Diagnostrat(MRI_SUVR_diagnostrat_AD_MCI_CN,ekstratekst, c(0.8,2), c(-0.075,0.025), diagnostrat1, diagnostrat2, diagnostrat3)
# Width: 750, height = 550
ggsave(filename = "Rplot strat for AD, MCI, CN.png", device = "png", width =7.8 , height =5.7 , dpi=700)

MRI_SUVR_diagnostrat_SMC_EMCI_LMCI <- MRI_SUVR_diagnostrat
MRI_SUVR_diagnostrat_SMC_EMCI_LMCI$DIAGNOSIS <- as.character(MRI_SUVR_diagnostrat_SMC_EMCI_LMCI$DIAGNOSIS)
MRI_SUVR_diagnostrat_SMC_EMCI_LMCI$DIAGNOSIS[MRI_SUVR_diagnostrat_SMC_EMCI_LMCI$DIAGNOSIS == "CN"] <- NA
MRI_SUVR_diagnostrat_SMC_EMCI_LMCI$DIAGNOSIS[MRI_SUVR_diagnostrat_SMC_EMCI_LMCI$DIAGNOSIS == "AD"] <- NA
MRI_SUVR_diagnostrat_SMC_EMCI_LMCI <- na.omit(MRI_SUVR_diagnostrat_SMC_EMCI_LMCI)
#Stratifying til at lave funktioner:
diagnostrat1 <- MRI_SUVR_diagnostrat_SMC_EMCI_LMCI #SMC
diagnostrat1$DIAGNOSIS[diagnostrat1$DIAGNOSIS == "EMCI"] <- NA
diagnostrat1$DIAGNOSIS[diagnostrat1$DIAGNOSIS == "LMCI"] <- NA
diagnostrat1 <- na.omit(diagnostrat1)

diagnostrat2 <- MRI_SUVR_diagnostrat_SMC_EMCI_LMCI #EMCI
diagnostrat2$DIAGNOSIS[diagnostrat2$DIAGNOSIS == "SMC"] <- NA
diagnostrat2$DIAGNOSIS[diagnostrat2$DIAGNOSIS == "LMCI"] <- NA
diagnostrat2 <- na.omit(diagnostrat2)

diagnostrat3 <- MRI_SUVR_diagnostrat_SMC_EMCI_LMCI #LMCI
diagnostrat3$DIAGNOSIS[diagnostrat3$DIAGNOSIS == "SMC"] <- NA
diagnostrat3$DIAGNOSIS[diagnostrat3$DIAGNOSIS == "EMCI"] <- NA
diagnostrat3 <- na.omit(diagnostrat3)



Diagnostrat(MRI_SUVR_diagnostrat_SMC_EMCI_LMCI,ekstratekst, c(0.8,2), c(-0.075,0.025), diagnostrat1, diagnostrat2, diagnostrat3)
# Width: 750, height = 550
ggsave(filename = "Rplot strat for EMCI, LMCI, SMC.png", device = "png", width =7.8 , height =5.7 , dpi=700)

#Gender stratifications:
ekstratekst = "Stratified for gender"
custompalette = c("#d7191c","#0571b0")
MRI_SUVR_male <- MRI_SUVR_diagnostrat
MRI_SUVR_male$GENDER[MRI_SUVR_male$GENDER == "Female"] <- NA
MRI_SUVR_male <- na.omit(MRI_SUVR_male)

MRI_SUVR_female <- MRI_SUVR_diagnostrat
MRI_SUVR_female$GENDER[MRI_SUVR_female$GENDER == "Male"] <- NA
MRI_SUVR_female <- na.omit(MRI_SUVR_female)
Genderstrat(MRI_SUVR_diagnostrat, ekstratekst, c(0.8,2), c(-0.075,0.025), MRI_SUVR_male, MRI_SUVR_female)
ggsave(filename = "Rplot strat for gender.png", device = "png", width =7.8 , height =5.7 , dpi=700)

#--------Eksklusionsdiagram--------
#------Eksklusionsdiagram opstilling af udregninger:-----#
#Total number of ADNI participants (4112)
Roster <- ADNIMERGE::roster
Roster <- distinct(Roster, RID, .keep_all = TRUE)
ADNIparticipants <- nrow(Roster)

#ADNI participants within our datasets
#UCBerkeley (1299)
UCBerk <- ADNIMERGE::ucberkeleyav45
UCBerk <- distinct(UCBerk, RID, .keep_all = TRUE)
UCBerk <- transmute(UCBerk, PETORIGPROT = ORIGPROT, EXAMDATE, RID, SUMMARYSUVR_WHOLECEREBNORM)
UCBerk <- na.omit(UCBerk) #Fjerner 0
UCBerkcount <- nrow(UCBerk)

#UCDavis (1701)
UCDav <- ADNIMERGE::ucd_wmh
UCDav <- transmute(UCDav, EXAMDATE, RID, MRICOLPROT = COLPROT, MRIORIGPROT = ORIGPROT, LEFT_HIPPO, RIGHT_HIPPO, TOTAL_HIPPO)
UCDav <- na.omit(UCDav) #Fjerner 2 (duplicates)
UCDavDistinct <- distinct(UCDav, RID, .keep_all = TRUE)
UCDavcount <- nrow(UCDavDistinct)

#Participants that are within both datasets (1186)
BerkDav <- left_join(UCBerk, UCDav, "RID")
BerkDav <- na.omit(BerkDav) #Fjerner 113 (duplicates)
BerkDavDistinct <- distinct(BerkDav, RID, .keep_all = TRUE)
BerkDavcount <- nrow(BerkDavDistinct)


#Participants with at least 1 Beta Amyloid PET and 2 MRI scans: (1011 ifoelge script)
#PET: (0), only data in UCBerkeley
UCBerkcount

#MRI: (1011)
UCDavMultiMRI <- merge(UCDav, UCBerk, by = "RID")
UCDavMultiMRI <- UCDavMultiMRI[duplicated(UCDavMultiMRI$RID),]
UCDavMultiMRIcount <- distinct(UCDavMultiMRI, RID, .keep_all = TRUE)
UCDavMultiMRIcount <- nrow(UCDavMultiMRIcount)

#Participants with at least 356 days between scans: (926)
BerkDav_baseline <- distinct(BerkDav, RID, .keep_all = TRUE)
BerkDav_baseline <- transmute(BerkDav_baseline, RID, BASELINE = EXAMDATE.y, MRI_COLPROT_BASELINE = MRICOLPROT, MRIORIGPROT, PETORIGPROT, PET_EXAMDATE = EXAMDATE.x)
BerkDav_baseline <- na.omit (BerkDav_baseline)
BerkDav_time <- transmute(BerkDav, RID, TIME = EXAMDATE.y, MRI_COLPROT_TIME = MRICOLPROT,)
BerkDav_time <- arrange(BerkDav_time,desc(TIME))
BerkDav_time <- distinct(BerkDav_time, RID, .keep_all = TRUE)
BerkDav_time <- na.omit(BerkDav_time)

BerkDav_timediff <- left_join(BerkDav_time, BerkDav_baseline, by = "RID")
BerkDav_timediff <- distinct(BerkDav_timediff)
BerkDav_timediff <- na.omit(BerkDav_timediff)
BerkDav_timediff <- mutate(BerkDav_timediff, TIMEDIFF = as.numeric(difftime(TIME, BASELINE,units=c("days"))))

BerkDav_timediff$TIMEDIFF[BerkDav_timediff$TIMEDIFF == "0"] <- NA
BerkDav_timediff <- na.omit(BerkDav_timediff)

Timediff365 <- BerkDav_timediff
Timediff365$TIMEDIFF[Timediff365$TIMEDIFF <= 365] <- NA
Timediff365 <- na.omit(Timediff365)
Timediff365count <- nrow(Timediff365)

#Final cohort info:
adnidata <- ADNIMERGE::adnimerge
adnidata <- transmute(adnidata,
                      RID = as.numeric(RID),
                      AGE,
                      MMSE,
                      GENDER = PTGENDER,
                      DIAGNOSIS = DX.bl,
                      EDUCATION = PTEDUCAT,
                      ADNIPROT = COLPROT)
adnidata <- na.omit(adnidata)
adnidata <- distinct(adnidata, RID, .keep_all = TRUE)
CohortInfo <- merge(Timediff365, adnidata, by = "RID")
meanage <- mean(CohortInfo$AGE)
sdage <- sd(CohortInfo$AGE)
diagnosis = table(CohortInfo$DIAGNOSIS)
meantimediff <- mean(CohortInfo$TIMEDIFF)
sdtimediff <- sd(CohortInfo$TIMEDIFF)
table(CohortInfo$GENDER)


#--------Eksklusionsdiagram opstilling af billedet:---------#

withinADNI <- boxGrob(glue("ADNI1, ADNI-GO, ADNI2 & ADNI3 participants",
                           "n = {pop}",
                           "UC Davis = {UCDavcount}",
                           "UC Berkeley = {UCBerkcount}",
                           pop = ADNIparticipants,
                           .sep = "\n"))

dataset <- boxGrob(glue("Within both UC Berkeley and UC Davis dataset",
                        "n = {pop}",
                        pop = BerkDavcount,
                        .sep = "\n"))
excludeddataset <- boxGrob(glue("Excluded (n = {tot})",
                                " - Not within UC Davis: {notUCDav}",
                                " - Not also within UC Berkeley: {notUCBerk}",
                                tot = (ADNIparticipants-BerkDavcount),
                                notUCDav = (ADNIparticipants - UCDavcount),
                                notUCBerk = ((ADNIparticipants-BerkDavcount) - notUCDav),
                                .sep = "\n"),
                           just = "left",
                           bjust = c(0,0))

scanexclusion <- boxGrob(glue("Minimum 2 MRI-scans and 1 Florbetapir PET-scan",
                              "n = {pop}",
                              pop = (UCDavMultiMRIcount),
                              .sep = "\n"))

excludedscan <- boxGrob(glue("Excluded (n = {tot}):",
                             " - Less than two MRI-scans: {noMRI}",
                             " - No florbetapir PET-scan: {noflorbetapir}",
                             tot = BerkDavcount - UCDavMultiMRIcount,
                             noflorbetapir = 0,
                             noMRI = BerkDavcount - UCDavMultiMRIcount,
                             .sep = "\n"),
                        just = "left", 
                        bjust = c(0,0))

withindaysexclusion <- boxGrob(glue("At least 365 days between oldest and newest MRI scan",
                                    "n = {pop}",
                                    pop = (Timediff365count),
                                    .sep = "\n"))

excludedwithindays <- boxGrob(glue("Excluded: (n = {tot})",
                                   " - Less than 365 days between oldest and newest MRI: {tot}",
                                   tot = (UCDavMultiMRIcount - Timediff365count),
                                   .sep = "\n"),
                              just = "left",
                              bjust = c(0,0))

cohort <- boxGrob(glue("Final study population: (n = {tot})",
                       "Mean age: {meanage}, 95%-CI: [{lowerage} ; {upperage}]",
                       "Mean time difference: {meantimedifference}, 95%-CI: [{lowertime} ; {uppertime}]",
                       tot = (Timediff365count),
                       lowerage = (round(meanage - 1.96 * sdage,digits=0)),
                       upperage = (round(meanage + 1.96 * sdage,digits=0)),
                       meantimedifference = (round(meantimediff,digits=0)),
                       lowertime = (round(meantimediff - 1.96 * sdtimediff,digits=0)),
                       uppertime = (round(meantimediff + 1.96 * sdtimediff,digits=0)),
                       .sep = "\n"))

png("Exclusion diagram.png", width = 900, height = 900, units="px")

grid.newpage()
vert <- spreadVertical(withinADNI = withinADNI,
                       dataset = dataset,
                       scanexclusion = scanexclusion,
                       withindaysexclusion = withindaysexclusion,
                       cohort = cohort)


excludeddataset <- moveBox(excludeddataset,
                           x = 0.6,
                           y = coords(vert$dataset)$top + distance(vert$withinADNI, vert$dataset, half = TRUE, center = FALSE))

excludedscan <- moveBox(excludedscan,
                        x = 0.6,
                        y = coords(vert$scanexclusion)$top + distance(vert$dataset, vert$scanexclusion, half = TRUE, center = FALSE))


excludedwithindays <- moveBox(excludedwithindays,
                              x = 0.6,
                              y = coords(vert$withindaysexclusion)$top + distance(vert$scanexclusion, vert$withindaysexclusion, half = TRUE, center = FALSE))


for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}
connectGrob(vert$withinADNI, excludeddataset, type = "L")
connectGrob(vert$dataset, excludedscan, type = "L")
connectGrob(vert$scanexclusion, excludedwithindays, type = "L")

# Print boxes
vert
excludeddataset
excludedscan
excludedwithindays
dev.off() #Stopper med at lave eksklusionsdiagram-billedet

#Z-test udregning, spredning af timediff:-------------
table(MRI_SUVR_just$DIAGNOSIS)
mean(MRI_SUVR_AD$TIME_DIFF)-1.96*sd(MRI_SUVR_AD$TIME_DIFF)
mean(MRI_SUVR_AD$TIME_DIFF)+1.96*sd(MRI_SUVR_AD$TIME_DIFF)
mean(MRI_SUVR_AD$TIME_DIFF)
mean(MRI_SUVR_CN$TIME_DIFF)
mean(MRI_SUVR_SMC$TIME_DIFF)
mean(MRI_SUVR_EMCI$TIME_DIFF)
mean(MRI_SUVR_LMCI$TIME_DIFF)

#SE = SD/sqrt(n)
sd(MRI_SUVR_AD$TIME_DIFF)/sqrt(99)
sd(MRI_SUVR_CN$TIME_DIFF)/sqrt(233)
sd(MRI_SUVR_SMC$TIME_DIFF)/sqrt(118)
sd(MRI_SUVR_EMCI$TIME_DIFF)/sqrt(291)
sd(MRI_SUVR_LMCI$TIME_DIFF)/sqrt(184)

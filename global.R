library(readxl)
library(dplyr)

setwd("C:/Users/USER/Desktop/2020-1/Statistics/sarcoma/Tumor grade LR")
a <- excel_sheets("sarcoma data sheet SMC 20200817.xlsx") %>% 
  lapply(function(x){read_excel("sarcoma data sheet SMC 20200817.xlsx",sheet=x, skip=2, na = c("UK"))})
b <- a[[1]] %>% 
  left_join(a[[2]], by = "환자번호") %>% left_join(a[[3]], by = "환자번호") %>% left_join(a[[4]], by = "환자번호") %>%
  left_join(a[[5]], by = "환자번호") %>% left_join(a[[6]], by = "환자번호") %>% left_join(a[[7]], by = "환자번호")
a.list<-read_excel("grade change 대상자 명단.xlsx", skip=1, na = c("UK"))
c<-b %>% filter(b$환자번호 %in% a.list$Number)

c$Age<-as.numeric(c[["수술날짜\r\n\r\ndd-mm-yyyy.x"]] - c[["생년월일\r\n\r\ndd-mm-yyyy"]])/365.25

out<-c %>% select(`환자번호`,Age,`성별\r\n\r\nM/F`)
names(out)[3] <- "Sex"; names(out)[1] <- "ID"
out$Sex <- as.factor(out$Sex)

out$Height<-as.numeric(c[["키\r\n(cm)"]])
out$Weight<-as.numeric(c[["몸무게\r\n(kg)"]])

out$FNCLCCgrade_primary<-as.factor(c[["FNCLCC grade\r\n\r\n1. total score 2-3\r\n2. total score 4-5\r\n3. total score 6,7,8\r\n4. UK"]])
out$FNCLCCgrade_firstLR<-as.factor(c[["FNCLCC grade"]])
out$Group<-as.factor(ifelse(as.integer(out$FNCLCCgrade_primary)>as.integer(out$FNCLCCgrade_firstLR),"Improving",
                            ifelse(as.integer(out$FNCLCCgrade_primary)==as.integer(out$FNCLCCgrade_firstLR),"Stable","Worsening")))

out$Day_LR<-as.numeric(as.Date(as.numeric(c[["Date of local recurrence"]]), origin = "1899-12-30")-as.Date(c[["수술날짜\r\n\r\ndd-mm-yyyy.x"]]))
out$Day_FU<-as.numeric(c[["마지막 f/u\r\n\r\ndd-mm-yyyy"]] - c[["수술날짜\r\n\r\ndd-mm-yyyy.x"]])

out$SecondLR<-as.factor(c[["재발#2\r\n\r\n0: 무\r\n1: 유.x"]])
out$Death<-as.factor(c[["사망여부\r\n\r\n0.Alive\r\n1.Dead\r\n2.Unknown.x"]])

out$TumorSize<-as.numeric(c[["종양 크기\r\n(Tumor size, mm)\r\n다발성인 경우 largest tumor size"]])

out$Histology_primary<-as.factor(c[["병리결과\r\n\r\n0. WD Liposarcoma\r\n1. DD Liposarcoma\r\n2. Pleomorphic Liposarcoma\r\n3. Leiomyosarcoma\r\n4. MPNST\r\n5. Solitary fibrous tumor\r\n6. PEComa\r\n7. liposarcoma NOS\r\n8. Myxoid liposarcoma\r\n9. others"]])
out$Histology_primary<-as.factor(ifelse(out$Histology_primary=="0","WDLPS",ifelse(out$Histology_primary=="1","DDLPS",
                         ifelse(out$Histology_primary=="2","PLS",ifelse(out$Histology_primary=="3","LMS",
                         ifelse(out$Histology_primary=="4","MPNST",ifelse(out$Histology_primary=="5","SFT",
                         ifelse(out$Histology_primary=="6","PEComa",ifelse(out$Histology_primary=="7","LPS_NOS",
                         ifelse(out$Histology_primary=="8","MLPS","Other"))))))))))

out$Histology_firstLR<-as.factor(c[["병리결과"]])
out$Histology_firstLR<-as.factor(ifelse(out$Histology_firstLR=="0","WDLPS",ifelse(out$Histology_firstLR=="1","DDLPS",
                          ifelse(out$Histology_firstLR=="2","PLS",ifelse(out$Histology_firstLR=="3","LMS",
                          ifelse(out$Histology_firstLR=="4","MPNST",ifelse(out$Histology_firstLR=="5","SFT",
                          ifelse(out$Histology_firstLR=="6","PEComa",ifelse(out$Histology_firstLR=="7","LPS_NOS",
                          ifelse(out$Histology_firstLR=="8","MLPS","Other"))))))))))

out$RTx<-as.factor(c[["수술 전후 RT 여부\r\n\r\n0.No\r\n1.Yes"]])
out$RTtiming<-as.factor(c[["RT timing\r\n\r\n0.None \r\n1.Preop only\r\n2. IORT only\r\n3.Preop + IORT\r\n4.Postop only\r\n5.Preop + postop boost\r\n6.IORT + postop"]])
out$Chemo<-as.factor(c[["Neoadjuvant chemo 여부\r\n\r\n0.No\r\n1.Yes"]])

#underlying diseases
out$ECOG<-as.factor(c[["ECOG\r\n\r\n0/1/2/3/4"]])
out$underlying_HTN<-as.factor(c[["HTN\r\n\r\n0. No\r\n1.yes"]])
out$underlying_DM<-as.factor(c[["DM\r\n\r\n0. No\r\n1.yes"]])
out$underlying_COPD<-as.factor(c[["COPD\r\n\r\n0. No\r\n1.yes"]])
out$underlying_CAD<-as.factor(c[["Coronary artery disease\r\n\r\n0. No\r\n1.yes"]])
out$underlying_CRD<-as.factor(c[["Chronic renal disease\r\n\r\n0. No\r\n1.yes"]])
out$underlying_others<-c[["Others\r\n\r\ncomment...16"]]

#complications
out$ClavienDindoYesNo<-as.factor(c[["Clavien-Dindo complication \r\n\r\n0. No\r\n1. Yes"]])
out$ClavienDindoGrade<-as.factor(c[["Clavien-Dindo grade \r\n\r\n2/3a/3b/4a/4b/5"]])
out$complication_AbdominalAbscess<-as.factor(c[["Abdominal abscess\r\n\r\n0. No\r\n1. Yes"]])
out$complication_BowelAnastomosisLeak<-as.factor(c[["Bowel anastomosis leak\r\n\r\n0. No\r\n1. Yes"]])
out$complication_BiliaryLeak<-as.factor(c[["Biliary leak\r\n\r\n0. No\r\n1. Yes"]])
out$complication_Bleeding<-as.factor(c[["Bleeding\r\n\r\n0. No\r\n1. Yes"]])
out$complication_Evisceration<-as.factor(c[["Evisceration\r\n\r\n0. No\r\n1. Yes"]])
out$complication_DVT<-as.factor(c[["DVT\r\n\r\n0. No\r\n1. Yes"]])
out$complication_LymphaticLeak<-as.factor(c[["Lymphatic leak\r\n\r\n0. No\r\n1. Yes"]])
out$complication_PancreaticLeak<-as.factor(c[["Pancreatic leak\r\n\r\n0. No\r\n1. Yes"]])
out$complication_Sepsis<-as.factor(c[["Sepsis\r\n\r\n0. No\r\n1. Yes"]])
out$complication_UrinaryLeak<-as.factor(c[["Urinary leak\r\n\r\n0. No\r\n1. Yes"]])
out$complication_Ileus<-as.factor(c[["Ileus\r\n\r\n0. No\r\n1. Yes"]])
out$complication_others<-c[["\r\nOthers\r\n\r\ncomment"]]

out$Necrosis_primary<-as.factor(ifelse(c[["Necrosis\r\n\r\n1. Absent\r\n2.<50% \r\n3.≥50%"]]=="1","Absent",
                               ifelse(c[["Necrosis\r\n\r\n1. Absent\r\n2.<50% \r\n3.≥50%"]]=="2","<50%","≥50%")))
out$Mitosis_primary<-as.factor(ifelse(c[["Mitotic index\r\n\r\n1.<9/10 HPF\r\n2. 10-19/10 HPF\r\n3. ≥20/10 HPF"]]=="1","<9/10 HPF",
                              ifelse(c[["Mitotic index\r\n\r\n1.<9/10 HPF\r\n2. 10-19/10 HPF\r\n3. ≥20/10 HPF"]]=="2","10-19/10 HPF","≥20/10 HPF")))


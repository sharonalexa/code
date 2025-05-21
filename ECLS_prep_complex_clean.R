setwd("~/Grad School/Research/My Publications/_Submitted/ECLS Resilience")

library(EdSurvey)
library(psych)
library(tidyverse)
library(haven)

############################################################################
#downloadECLS_K(years = 2011, root = "C:/", cache = TRUE) 
#^only need to run the first time on a new device

eclsk11 <- readECLS_K2011("C:/ECLS_K/2011")
View(showCodebook(eclsk11))

#showWeights(data = eclsk11, verbose = F) #way too long to be helpful
#getStratumVar(eclsk11, weightVar = "w1c0") #w1c0str <changed to base year
#getPSUVar(eclsk11, weightVar = "w1c0") #w1c0psu <changed to base year
#showPlausibleValues(eclsk11, verbose = FALSE) #none for this dataset


#subset data to only desired variables
myvars <- c("childid", "w1c0", "w1c0str", "w1c0psu", "w2sch0", "x1rscalk5", 
            "x1mscalk5", "x1tchper", "x1prnsoc", "x12par1ed_i", "x12par2ed_i", 
            "p2parct1", "p2parct2", "p1hscale", "p1readbk", "p1tellst", 
            "p1singso", "p1hlpart", "p1chores", "p1games", "p1nature", "p1build", 
            "p1sport", "p1numbrs", "p2homecm", "p1expect", "x1par1scr_i", 
            "x1par2scr_i", "x2povty", "x2fsstat2", "x1kage_r", "x_raceth_r", 
            "x_chsex_r", "x2disabl", "x2flch2_i", "x2rlch2_i", "x2krceth", 
            "a2notcap",  "a2gtthr", "a2chgap", "a2dfmth", "a2ltldo", "a2wsttm", 
            "a2habit", "a2fctor", "a2relfam", "a2incret", "a2avddis", "a2enjoy", 
            "a2mkdiff", "a2misbhv", "a2accptd", "a2cntnlr", "a2paprwr", 
            "a2psupp", "a2copstf", "a2recjob", "a2stndlo", "a2missio", "a2encour",
            "s1_id", "s2_id", "x1locale", "x2locale", "x12sesl", "p1anylng", #ECLS-K imputation variables (+sex,age,race/eth) 
            "x2rscalk5", "x2mscalk5", "x2tchper", "x2prnsoc", #longitudinal imputation variables
            "x4rscalk5", "x4mscalk5", "x4tchper", "x4prnsoc", 
            "x5rscalk5", "x6rscalk5", "x7rscalk5", "x8rscalk5", "x9rscalk5", 
            "x5mscalk5", "x6mscalk5", "x7mscalk5", "x8mscalk5", "x9mscalk5", 
        "x4par1ed_i", "x4par1scr_i", "x4povty_i", "x4fsstat2", 
        "x7par1ed_i", "x6par1scr_i", "x6povty_i", "x9fsstat2", 
        "x8par1ed_i",                "x7povty_i",              
        "x9par1ed_i", "x9par1scr_i", "x8povty_i",              
                                     "x9povty_i") 

res1 <- getData(data = eclsk11, varnames = myvars, addAttributes = TRUE, dropOmittedLevels = FALSE)
#^pulls everything, including replicate weights



##parent nativity, recode to binary
summary2(res1, variable = "p2parct1", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$parct1 <- ifelse(res1$p2parct1 == "1: USA (1)", "US-born", 
                      ifelse(res1$p2parct1 == "2: MEXICO (145)", "Foreign-born",
                             ifelse(res1$p2parct1 == "3: OTHER", "Foreign-born",
                                    NA)))
summary2(res1, variable = "parct1", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "p2parct2", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$parct2 <- ifelse(res1$p2parct2 == "1: USA (1)", "US-born", 
                      ifelse(res1$p2parct2 == "2: MEXICO (145)", "Foreign-born",
                             ifelse(res1$p2parct2 == "3: OTHER", "Foreign-born",
                                    NA)))
summary2(res1, variable = "parct2", weightVar = "w1c0", dropOmittedLevels = FALSE)
#combine parents
res1$parct <- as.factor(paste(res1$parct1, res1$parct2))
summary2(res1, variable = "parctb", weightVar = "w1c0", dropOmittedLevels = FALSE)
new.levels <- c("Foreign-Born Parent(s)", "Foreign-Born Parent(s)", #1,2
                "Mixed Parent Nativity", "Foreign-Born Parent(s)", #3,4
                NA, "U.S.-Born Parent(s)", "Mixed Parent Nativity", #5,6,7
                "U.S.-Born Parent(s)", "U.S.-Born Parent(s)") #8,9
res1$parctb <- factor(new.levels[res1$parct])


##teaching self-efficacy
summary2(res1, variable = "a2notcap", weightVar = "w1c0", dropOmittedLevels = FALSE) #reverse code
res1$TQ1_notcap <- ifelse(res1$a2notcap == "1: STRONGLY DISAGREE", 5, 
                       ifelse(res1$a2notcap == "2: DISAGREE", 4,
                           ifelse(res1$a2notcap == "3: NEITHER AGREE NOR DISAGREE", 3,
                               ifelse(res1$a2notcap == "4: AGREE", 2, 
                                   ifelse(res1$a2notcap == "5: STRONGLY AGREE", 1, 
                                          NA)))))
summary2(res1, variable = "TQ1_notcap", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2gtthr", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$TQ2_gtthr <- ifelse(res1$a2gtthr == "1: STRONGLY DISAGREE", 1, 
                      ifelse(res1$a2gtthr == "2: DISAGREE", 2,
                          ifelse(res1$a2gtthr == "3: NEITHER AGREE NOR DISAGREE", 3,
                              ifelse(res1$a2gtthr == "4: AGREE", 4, 
                                     ifelse(res1$a2gtthr == "5: STRONGLY AGREE", 5, 
                                            NA)))))
summary2(res1, variable = "TQ2_gtthr", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2chgap", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$TQ3_chgap <- ifelse(res1$a2chgap == "1: STRONGLY DISAGREE", 1, 
                ifelse(res1$a2chgap == "2: DISAGREE", 2,
                    ifelse(res1$a2chgap == "3: NEITHER AGREE NOR DISAGREE", 3,
                        ifelse(res1$a2chgap == "4: AGREE", 4, 
                            ifelse(res1$a2chgap == "5: STRONGLY AGREE", 5, 
                                                 NA)))))
summary2(res1, variable = "TQ3_chgap", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2dfmth", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$TQ4_dfmth <- ifelse(res1$a2dfmth == "1: STRONGLY DISAGREE", 1, 
                     ifelse(res1$a2dfmth == "2: DISAGREE", 2,
                            ifelse(res1$a2dfmth == "3: NEITHER AGREE NOR DISAGREE", 3,
                                   ifelse(res1$a2dfmth == "4: AGREE", 4, 
                                          ifelse(res1$a2dfmth == "5: STRONGLY AGREE", 5, 
                                                 NA)))))
summary2(res1, variable = "TQ4_dfmth", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2ltldo", weightVar = "w1c0", dropOmittedLevels = FALSE) #reverse code
res1$TQ5_ltldo <- ifelse(res1$a2ltldo == "1: STRONGLY DISAGREE", 5, 
                   ifelse(res1$a2ltldo == "2: DISAGREE", 4,
                          ifelse(res1$a2ltldo == "3: NEITHER AGREE NOR DISAGREE", 3,
                                 ifelse(res1$a2ltldo == "4: AGREE", 2, 
                                        ifelse(res1$a2ltldo == "5: STRONGLY AGREE", 1, 
                                               NA)))))
summary2(res1, variable = "TQ5_ltldo", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2wsttm", weightVar = "w1c0", dropOmittedLevels = FALSE) #reverse code
res1$TQ6_wsttm <- ifelse(res1$a2wsttm == "1: STRONGLY DISAGREE", 5, 
                      ifelse(res1$a2wsttm == "2: DISAGREE", 4,
                          ifelse(res1$a2wsttm == "3: NEITHER AGREE NOR DISAGREE", 3,
                               ifelse(res1$a2wsttm == "4: AGREE", 2, 
                                   ifelse(res1$a2wsttm == "5: STRONGLY AGREE", 1, 
                                                     NA)))))
summary2(res1, variable = "TQ6_wsttm", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2habit", weightVar = "w1c0", dropOmittedLevels = FALSE) #reverse code
res1$TQ7_habit <- ifelse(res1$a2habit == "1: STRONGLY DISAGREE", 5, 
                      ifelse(res1$a2habit == "2: DISAGREE", 4,
                          ifelse(res1$a2habit == "3: NEITHER AGREE NOR DISAGREE", 3,
                               ifelse(res1$a2habit == "4: AGREE", 2, 
                                    ifelse(res1$a2habit == "5: STRONGLY AGREE", 1, 
                                                 NA)))))
summary2(res1, variable = "TQ7_habit", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2fctor", weightVar = "w1c0", dropOmittedLevels = FALSE) #reverse code
res1$TQ8_fctor <- ifelse(res1$a2fctor == "1: STRONGLY DISAGREE", 5, 
                    ifelse(res1$a2fctor == "2: DISAGREE", 4,
                           ifelse(res1$a2fctor == "3: NEITHER AGREE NOR DISAGREE", 3,
                                  ifelse(res1$a2fctor == "4: AGREE", 2, 
                                         ifelse(res1$a2fctor == "5: STRONGLY AGREE", 1, 
                                                NA)))))
summary2(res1, variable = "TQ8_fctor", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2relfam", weightVar = "w1c0", dropOmittedLevels = FALSE) #reverse code
res1$TQ9_relfam <- ifelse(res1$a2relfam == "1: STRONGLY DISAGREE", 5, 
                    ifelse(res1$a2relfam == "2: DISAGREE", 4,
                           ifelse(res1$a2relfam == "3: NEITHER AGREE NOR DISAGREE", 3,
                                  ifelse(res1$a2relfam == "4: AGREE", 2, 
                                         ifelse(res1$a2relfam == "5: STRONGLY AGREE", 1, 
                                                NA)))))
summary2(res1, variable = "TQ9_relfam", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2incret", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$TQ10_incret <- ifelse(res1$a2incret == "1: STRONGLY DISAGREE", 1, 
                    ifelse(res1$a2incret == "2: DISAGREE", 2,
                           ifelse(res1$a2incret == "3: NEITHER AGREE NOR DISAGREE", 3,
                                  ifelse(res1$a2incret == "4: AGREE", 4, 
                                         ifelse(res1$a2incret == "5: STRONGLY AGREE", 5, 
                                                NA)))))
summary2(res1, variable = "TQ10_incret", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2avddis", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$TQ11_avddis <- ifelse(res1$a2avddis == "1: STRONGLY DISAGREE", 1, 
                    ifelse(res1$a2avddis == "2: DISAGREE", 2,
                           ifelse(res1$a2avddis == "3: NEITHER AGREE NOR DISAGREE", 3,
                                  ifelse(res1$a2avddis == "4: AGREE", 4, 
                                         ifelse(res1$a2avddis == "5: STRONGLY AGREE", 5, 
                                                NA)))))
summary2(res1, variable = "TQ11_avddis", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2enjoy", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$TQ12_enjoy <- ifelse(res1$a2enjoy == "1: STRONGLY DISAGREE", 1, 
                    ifelse(res1$a2enjoy == "2: DISAGREE", 2,
                           ifelse(res1$a2enjoy == "3: NEITHER AGREE NOR DISAGREE", 3,
                                  ifelse(res1$a2enjoy == "4: AGREE", 4, 
                                         ifelse(res1$a2enjoy == "5: STRONGLY AGREE", 5, 
                                                NA)))))
summary2(res1, variable = "TQ12_enjoy", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "a2mkdiff", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$TQ13_mkdiff <- ifelse(res1$a2mkdiff == "1: STRONGLY DISAGREE", 1, 
                    ifelse(res1$a2mkdiff == "2: DISAGREE", 2,
                           ifelse(res1$a2mkdiff == "3: NEITHER AGREE NOR DISAGREE", 3,
                                  ifelse(res1$a2mkdiff == "4: AGREE", 4, 
                                         ifelse(res1$a2mkdiff == "5: STRONGLY AGREE", 5, 
                                                NA)))))
summary2(res1, variable = "TQ13_mkdiff", weightVar = "w1c0", dropOmittedLevels = FALSE)

res1$tchscale <- rowMeans(res1[c("TQ1_notcap", 
                                 "TQ2_gtthr", 
                                 "TQ3_chgap", 
                                 "TQ4_dfmth", 
                                 "TQ5_ltldo", 
                                 "TQ6_wsttm", 
                                 "TQ7_habit", 
                                 "TQ8_fctor", 
                                 "TQ9_relfam", 
                                 "TQ10_incret", 
                                 "TQ11_avddis", 
                                 "TQ12_enjoy", 
                                 "TQ13_mkdiff")], na.rm = TRUE)
summary2(res1, variable = "tchscale", weightVar = "w1c0", dropOmittedLevels = FALSE)


##F K reading
summary2(res1, variable = "x1rscalk5", weightVar = "w1c0", dropOmittedLevels = FALSE)


##F K math
summary2(res1, variable = "x1mscalk5", weightVar = "w1c0", dropOmittedLevels = FALSE)


##F K SE skills
summary2(res1, variable = "x1prnsoc", weightVar = "w1c0", dropOmittedLevels = FALSE) #parent rating

summary2(res1, variable = "x1tchper", weightVar = "w1c0", dropOmittedLevels = FALSE)  #teacher rating


##parent edu
#get rid of extra NA group, make numeric
summary2(res1, variable = "x12par1ed_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$par1ed_num <- ifelse(res1$x12par1ed_i == -9, NA, as.numeric(res1$x12par1ed_i))
res1 <- rebindAttributes(data = res1, attributeData = eclsk11) #doesn't seem to do anything?
summary2(res1, variable = "par1ed_num", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "x12par2ed_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$par2ed_num <- ifelse(res1$x12par2ed_i == -1, NA, 
                             ifelse(res1$x12par2ed_i == -9, NA, 
                                    as.numeric(res1$x12par2ed_i)))
res1 <- rebindAttributes(data = res1, attributeData = eclsk11) #doesn't seem to do anything?
summary2(res1, variable = "par2ed_num", weightVar = "w1c0", dropOmittedLevels = FALSE)

res1$pared_avg <- rowMeans(res1[, c("par1ed_num", "par2ed_num")], na.rm = TRUE) #need to do in SPSS after imputation
summary2(res1, variable = "pared_avg", weightVar = "w1c0", dropOmittedLevels = FALSE)


##school poverty
#combine frpm, cap total at 100%
summary2(res1, variable = "x2flch2_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
summary2(res1, variable = "x2rlch2_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$frlch <- rowSums(res1[c("x2flch2_i", "x2rlch2_i")])
summary2(res1, variable = "frlch", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$frlch = pmin(res1$frlch, 100)


##SOCIOECONOMIC RISK review
summary2(res1, variable = c("par1ed_num", "par2ed_num", "frlch"), weightVar = "w1c0", dropOmittedLevels = FALSE) #only calculates stats for overlapping children




saveRDS(res1, file = "ECLSResilience.rds")
write_sav(res1, "./ECLSResilience.sav")





############################################################
############################################################
##################IMPUTATION##############################
############################################################
setwd("~/Grad School/Research/My Publications/_Submitted/ECLS Resilience")

library(EdSurvey)
library(psych)
library(haven)
res1 <- readRDS("ECLSResilience.rds")


##reading
summary2(res1, variable = "x1rscalk5", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$x1rscalk5_imp <- ifelse(is.na(res1$x1rscalk5), 
                             rowMeans(res1[, c("x2rscalk5", "x4rscalk5", "x5rscalk5", "x6rscalk5", "x7rscalk5", "x8rscalk5", "x9rscalk5")], na.rm = TRUE),
                             res1$x1rscalk5)
summary2(res1, variable = c("x1rscalk5_imp", "x2rscalk5", "x4rscalk5"), weightVar = "w1c0", dropOmittedLevels = FALSE)


##math
summary2(res1, variable = "x1mscalk5", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$x1mscalk5_imp <- ifelse(is.na(res1$x1mscalk5), 
                             rowMeans(res1[, c("x2mscalk5", "x4mscalk5", "x5mscalk5", "x6mscalk5", "x7mscalk5", "x8mscalk5", "x9mscalk5")], na.rm = TRUE),
                             res1$x1mscalk5)
summary2(res1, variable = c("x1mscalk5_imp", "x2mscalk5", "x4mscalk5", "x9mscalk5"), weightVar = "w1c0", dropOmittedLevels = FALSE)
nomath <- subset(res1, is.na(x1mscalk5_imp))
nomath <- rebindAttributes(data = nomath, attributeData = res1)
summary2(nomath, variable = c("x1mscalk5_imp", "x2mscalk5", "x4mscalk5"), weightVar = "w1c0", dropOmittedLevels = FALSE)
nomath$count <- as.factor(nomath$w1c0)
summary(nomath$count)
211-135 #76 missing kids w/ non-zero weights


##home SE
summary2(res1, variable = "x1prnsoc", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$x1prnsoc_imp <- ifelse(is.na(res1$x1prnsoc), 
                             rowMeans(res1[, c("x2prnsoc", "x4prnsoc")], na.rm = TRUE),
                             res1$x1prnsoc)
summary2(res1, variable = c("x1prnsoc_imp", "x2prnsoc", "x4prnsoc"), weightVar = "w1c0", dropOmittedLevels = FALSE)
nohomeSE <- subset(res1, is.na(x1prnsoc_imp))
nohomeSE$count <- as.factor(nohomeSE$w1c0)
summary(nohomeSE$count)
1498-322 #1176 missing kids w/ non-zero weights


##school SE
summary2(res1, variable = "x1tchper", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$x1tchper_imp <- ifelse(is.na(res1$x1tchper), 
                            rowMeans(res1[, c("x2tchper", "x4tchper")], na.rm = TRUE),
                            res1$x1tchper)
summary2(res1, variable = c("x1tchper_imp", "x2tchper", "x4tchper"), weightVar = "w1c0", dropOmittedLevels = FALSE)
noschoolSE <- subset(res1, is.na(x1tchper_imp))
noschoolSE$count <- as.factor(noschoolSE$w1c0)
summary(noschoolSE$count)
871-289 #582 missing kids w/ non-zero weights


##par1 edu
#clean G1
summary2(res1, variable = "x4par1ed_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$x4par1ed_num <- ifelse(res1$x4par1ed_i == "8: MASTER'S DEGREE (MA, MS)" | 
                            res1$x4par1ed_i == "9: DOCTORATE OR PROFESSIONAL DEGREE", 
                            8, as.numeric(res1$x4par1ed_i))
summary2(res1, variable = "x4par1ed_num", weightVar = "w1c0", dropOmittedLevels = FALSE)
#clean G3
summary2(res1, variable = "x7par1ed_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$x7par1ed_num <- ifelse(res1$x7par1ed_i == "8: MASTER'S DEGREE (MA, MS)" | 
                              res1$x7par1ed_i == "9: DOCTORATE OR PROFESSIONAL DEGREE", 
                            8, as.numeric(res1$x7par1ed_i))
summary2(res1, variable = "x7par1ed_num", weightVar = "w1c0", dropOmittedLevels = FALSE)
#clean G4
summary2(res1, variable = "x8par1ed_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$x8par1ed_num <- ifelse(res1$x8par1ed_i == "8: MASTER'S DEGREE (MA, MS)" | 
                              res1$x8par1ed_i == "9: DOCTORATE OR PROFESSIONAL DEGREE", 
                            8, as.numeric(res1$x8par1ed_i))
summary2(res1, variable = "x8par1ed_num", weightVar = "w1c0", dropOmittedLevels = FALSE)
#clean G5
summary2(res1, variable = "x9par1ed_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$x9par1ed_num <- ifelse(res1$x9par1ed_i == "8: MASTER'S DEGREE (MA, MS)" | 
                              res1$x9par1ed_i == "9: DOCTORATE OR PROFESSIONAL DEGREE", 
                            8, as.numeric(res1$x9par1ed_i))
summary2(res1, variable = "x9par1ed_num", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "par1ed_num", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$par1ed_imp <- ifelse(is.na(res1$par1ed_num), 
                          rowMeans(res1[, c("x4par1ed_num", "x7par1ed_num", "x8par1ed_num", "x9par1ed_num")], na.rm = TRUE), 
                          res1$par1ed_num)
summary2(res1, variable = c("par1ed_imp", "par1ed_num", "x4par1ed_num"), 
         weightVar = "w1c0", dropOmittedLevels = FALSE)


##par1 occu prestige
#check G1, G2, G5
summary2(res1, variable = c("x4par1scr_i", "x6par1scr_i", "x9par1scr_i"), 
         weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "x1par1scr_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$par1scr_imp <- ifelse(is.na(res1$x1par1scr_i), 
                           rowMeans(res1[, c("x4par1scr_i", "x6par1scr_i", "x9par1scr_i")], na.rm = TRUE), 
                           res1$x1par1scr_i)
summary2(res1, variable = c("par1scr_imp", "x1par1scr_i"), weightVar = "w1c0", dropOmittedLevels = FALSE)


##household poverty
#check G1, G2, G3, G5
summary2(res1, variable = "x4povty_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
summary2(res1, variable = "x6povty_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
summary2(res1, variable = "x7povty_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
summary2(res1, variable = "x8povty_i", weightVar = "w1c0", dropOmittedLevels = FALSE)
summary2(res1, variable = "x9povty_i", weightVar = "w1c0", dropOmittedLevels = FALSE)

summary2(res1, variable = "x2povty", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$povty <- as.factor(ifelse(res1$x2povty == "-9: NOT ASCERTAINED", NA, res1$x2povty))

res1$povty_imp <- as.factor(ifelse(is.na(res1$x2povty), res1$x4povty_i,
                           ifelse(is.na(res1$x2povty), res1$x6povty_i,
                                  ifelse(is.na(res1$x2povty), res1$x7povty_i,
                                         ifelse(is.na(res1$x2povty), res1$x8povty_i,
                                                ifelse(is.na(res1$x2povty), res1$x9povty_i,
                                                       res1$x2povty))))))
summary2(res1, variable = c("povty_imp", "povty"), weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$p1 <- as.numeric(res1$povty_imp)
res1$p2 <- as.numeric(res1$povty)
summary2(res1, variable = c("p1", "p2"), weightVar = "w1c0", dropOmittedLevels = FALSE)


##food insecurity
#check G1, G5
summary2(res1, variable = "x4fsstat", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$x4fsstat <- as.factor(ifelse(res1$x4fsstat2 == "-9: NOT ASCERTAINED", NA, res1$x4fsstat2))
summary2(res1, variable = "x9fsstat", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$x9fsstat <- as.factor(ifelse(res1$x9fsstat2 == "-9: NOT ASCERTAINED", NA, res1$x9fsstat2))

summary2(res1, variable = "fsstat", weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$fsstat <- as.factor(ifelse(res1$x2fsstat2 == "-9: NOT ASCERTAINED", NA, res1$x2fsstat2))

res1$fsstat_imp <- as.factor(ifelse(is.na(res1$fsstat), res1$x4fsstat,
                                   ifelse(is.na(res1$fsstat), res1$x9fsstat,
                                                               res1$fsstat)))
summary2(res1, variable = c("fsstat_imp", "fsstat"), weightVar = "w1c0", dropOmittedLevels = FALSE)
res1$f1 <- as.numeric(res1$fsstat_imp)
res1$f2 <- as.numeric(res1$fsstat)
summary2(res1, variable = c("f1", "f2"), weightVar = "w1c0", dropOmittedLevels = FALSE)


##school poverty
#check G2, G3, G4, G5 <apparently they switched to categorical
summary2(res1, variable = c("x6frmeal_i", "x7frmeal_i", "x8frmeal_i", "x9frmeal_i"), 
         weightVar = "w1c0", dropOmittedLevels = FALSE)

#standardize outcomes
read_avg <- mean(res1$x4rscalk5, na.rm = TRUE)
read_sd <- sd(res1$x4rscalk5, na.rm = TRUE)
res1$x4rscalk5_z <- (res1$x4rscalk5 - read_avg) / read_sd

math_avg <- mean(res1$x4mscalk5, na.rm = TRUE)
math_sd <- sd(res1$x4mscalk5, na.rm = TRUE)
res1$x4mscalk5_z <- (res1$x4mscalk5 - math_avg) / math_sd

tch_avg <- mean(res1$x4tchper, na.rm = TRUE)
tch_sd <- sd(res1$x4tchper, na.rm = TRUE)
res1$x4tchper_z <- (res1$x4tchper - tch_avg) / tch_sd

par_avg <- mean(res1$x4prnsoc, na.rm = TRUE)
par_sd <- sd(res1$x4prnsoc, na.rm = TRUE)
res1$x4prnsoc_z <- (res1$x4prnsoc - par_avg) / par_sd


saveRDS(res1, file = "ECLSResilience_imp1.rds")
write_sav(res1, "./ECLSResilience_imp1.sav")



###############################subsetting#################################
############################################################################
setwd("~/Grad School/Research/My Publications/_Submitted/ECLS Resilience")
res1 <- readRDS("res1.rds")
library(psych)


SErisk <- res1[which(res1$x1par1scr_i < 48 | res1$x1par2scr_i < 48 | 
                       res1$x4par1scr_i < 48 | res1$x4par2scr_i < 48 | 
                       res1$x2povty == 1 | res1$x2povty == 2 | 
                       res1$x4povty_i == 1 | res1$x4povty_i == 2 | 
                       res1$Kpar1 < 5 | res1$Kpar2 < 5 | 
                       res1$G1par2 < 5 | res1$G1par2 < 5 | 
                       as.numeric(res1$x2fsstat2) > 1 | as.numeric(res1$x4fsstat2) > 1 | 
                       res1$x4frmeal_i > 75),] #n=14803


###check subsetting
#Low edu, scores 5 or less
summary(res1$x12par1ed_i) #KG parent 1
levels(res1$x12par1ed_i)
summary(res1$x12par2ed_i) #KG parent 2
summary(res1$x4par1ed_i) #G1 parent 1
summary(res1$x4par2ed_i) #G2 parent 2

#Household food insecurity, low and very low or 3+
summary(res1$x2fsstat2)
summary(res1$x4fsstat2)

#High poverty school, 75%+ FRPM
summary(res1$x4fmeal_i) #SP G1 free meals
describe(res1$x4fmeal_i)
summary(res1$x4rmeal_i) #SP G1 reduced price meals
summary(res1$x4frmeal_i)
describe(res1$x4frmeal_i)

#xp variables review
levels(res1$x_chsex_r)
levels(res1$x_raceth_r)


saveRDS(SErisk, file = "SErisk.rds")
write.csv(SErisk, "SErisk.csv")


library(tidyverse)
library(psych)
library(apaTables)


# learning team roster ----------------------------------------------------

roster <- read_csv("~/Dropbox/Values, Race, & Psych Safety/Data/LT Assignments Fall 2023, Fall 2024, Fall 2025.csv") %>%
  mutate(cohort = paste0("fa", substr(year, 3, 4)),
         LT = paste0(cluster, learningteam)) %>%
  select(c(PID, cohort, LT))

roster$uniqueID <- paste0(roster$PID, "_", roster$cohort)

# demographics ------------------------------------------------------------

# import fall 2023 demographic information
fa23 <- read_csv("~/Dropbox/Values, Race, & Psych Safety/Data/Demos/Fall 2023 Demo.csv") %>%
  mutate(cohort = "fa23") %>%
  rename("sexual_orientation"="sexual orientation")

# import fall 2024 demographic information
fa24 <- read_csv("~/Dropbox/Values, Race, & Psych Safety/Data/Demos/Fall 2024 Demo.csv") %>%
  mutate(cohort = "fa24",
         race_ethnicity = coalesce(race_ethnicity_1, race_ethnicity_2, race_ethnicity_3, race_ethnicity_4, 
                                   race_ethnicity_5, race_ethnicity_6, race_ethnicity_7),
         sexual_orientation = coalesce(`sexual orientation_1`, `sexual orientation_4`, `sexual orientation_5`, 
                                       `sexual orientation_6`, `sexual orientation_7`, `sexual orientation_8`, 
                                       `sexual orientation_9`, `sexual orientation_10`, `sexual orientation_11`))

# import fall 2025 demographic information
fa25 <- read_csv("~/Dropbox/Values, Race, & Psych Safety/Data/Demos/Fall 2025 Demo.csv") %>%
  mutate(cohort = "fa25",
         race_ethnicity = coalesce(race_ethnicity_1, race_ethnicity_2, race_ethnicity_3, race_ethnicity_4, 
                                   race_ethnicity_5, race_ethnicity_6, race_ethnicity_7),
         sexual_orientation = coalesce(`sexual orientation_1`, `sexual orientation_4`, `sexual orientation_5`, 
                                       `sexual orientation_6`, `sexual orientation_7`, `sexual orientation_8`, 
                                       `sexual orientation_9`, `sexual orientation_10`, `sexual orientation_11`))

# define columns of interest; select
keep <- c("RecordedDate", "ResponseId", "PID", "Age", "Gender", "race_ethnicity", "sexual_orientation", 
          "borncountry", "homecountry", "industry", "industry_tenure", "cohort")

fa23 <- fa23 %>% select(all_of(keep))
fa24 <- fa24 %>% select(all_of(keep))
fa25 <- fa25 %>% select(all_of(keep))

# bind 2023-2025 data
demos <- rbind(fa23, fa24, fa25)
rm(keep, fa23, fa24, fa25)

# create unique ID column
demos$uniqueID <- paste0(demos$PID, "_", demos$cohort)

# create dummy demographic variables
demos$dum.race <- ifelse(demos$race_ethnicity != "White/European American", 1, 0)
demos$dum.gender <- ifelse(demos$Gender != "Man", 1, 0)
demos$dum.sexori <- ifelse(demos$sexual_orientation != "straight (heterosexual)", 1, 0)
demos$dum.intl <- ifelse(demos$homecountry != "United States", 1, 0)

# merge demographics and roster data
demos <- merge(demos, roster, by=c("uniqueID", "PID", "cohort"), all=T)

# create unique team column
demos$uniqueTeam <- paste0(demos$LT, "_", demos$cohort)

# check for duplicates; select most recent response
length(unique(demos$uniqueID))
dupli <- demos[duplicated(demos$uniqueID) | duplicated(demos$uniqueID, fromLast=T),]
demos$RecordedDate <- as.POSIXct(demos$RecordedDate, format = "%m/%d/%y %H:%M")
demos <- demos %>% group_by(uniqueID) %>%
  slice_max(RecordedDate, n=1, with_ties=F) %>%
  ungroup()
rm(dupli)

# calculate racial similarity
demos <- demos %>%
  group_by(LT, race_ethnicity) %>%
  mutate(same_race = n() - 1) %>%
  group_by(LT) %>%
  mutate(prop_same.race = same_race / (n() - 1)) %>% 
  ungroup()

# address missing data
demos$same_race[is.na(demos$race_ethnicity) | is.na(demos$LT)] <- NA
demos$prop_same.race[is.na(demos$race_ethnicity) | is.na(demos$LT)] <- NA

# drop redundant columns
demos <- demos %>% select(-c(PID, cohort, RecordedDate, ResponseId))

# team inventory ----------------------------------------------------------

# import fall 2023 team inventory data
fa23 <- read_csv("~/Dropbox/Values, Race, & Psych Safety/Data/Team Inventory/Fall 2023 Team Inventory.csv") %>%
  mutate(cohort = "fa23")

# import fall 2024 team inventory data
fa24 <- read_csv("~/Dropbox/Values, Race, & Psych Safety/Data/Team Inventory/Fall 2024 Team Inventory.csv") %>% 
  mutate(cohort = "fa24")

# import fall 2025 team inventory data
fa25 <- read_csv("~/Dropbox/Values, Race, & Psych Safety/Data/Team Inventory/Fall 2025 Team Inventory.csv") %>%
  mutate(cohort = "fa25")

# bind 2023-2025 data
inventory <- rbind(fa23, fa24, fa25)
rm(fa23, fa24, fa25)

# score psychological safety
inventory$PsychSafety <- ((8-inventory$PsychSafety_1) + inventory$PsychSafety_2 + (8-inventory$PsychSafety_3) + inventory$PsychSafety_4 + (8-inventory$PsychSafety_5) + inventory$PsychSafety_6 + inventory$PsychSafety_7)/7
PsychSafety <- data.frame((8-inventory$PsychSafety_1), inventory$PsychSafety_2, (8-inventory$PsychSafety_3), inventory$PsychSafety_4, (8-inventory$PsychSafety_5), inventory$PsychSafety_6, inventory$PsychSafety_7)
alpha(PsychSafety)$total$raw
rm(PsychSafety)

# score team efficacy
inventory$TeamEfficacy <- (inventory$TeamEfficacy_1 + inventory$TeamEfficacy_2 + inventory$TeamEfficacy_3)/3
TeamEfficacy <- data.frame(inventory$TeamEfficacy_1, inventory$TeamEfficacy_2, inventory$TeamEfficacy_3)
alpha(TeamEfficacy)$total$raw
rm(TeamEfficacy)

# score team efficacy
inventory$TeamPerformance <- ((8-inventory$TeamPerformance_1) + (8-inventory$TeamPerformance_2) + inventory$TeamPerformance_3 + (8-inventory$TeamPerformance_4) + (8-inventory$TeamPerformance_5))/5
TeamPerformance <- data.frame((8-inventory$TeamPerformance_1), (8-inventory$TeamPerformance_2), inventory$TeamPerformance_3, (8-inventory$TeamPerformance_4), (8-inventory$TeamPerformance_5))
alpha(TeamPerformance)$total$raw
rm(TeamPerformance)

# create unique ID column
inventory$uniqueID <- paste0(inventory$PID, "_", inventory$cohort)

# merge demographics and roster data
inventory <- merge(inventory, roster, by=c("uniqueID", "PID", "cohort"), all=T)

# create unique team column
inventory$uniqueTeam <- paste0(inventory$LT, "_", inventory$cohort)

# check for duplicates; select most recent response
length(unique(inventory$uniqueID))
dupli <- inventory[duplicated(inventory$uniqueID) | duplicated(inventory$uniqueID, fromLast=T),]
inventory$RecordedDate <- as.POSIXct(inventory$RecordedDate, format = "%m/%d/%y %H:%M")
inventory <- inventory %>% group_by(uniqueID) %>%
  slice_max(RecordedDate, n=1, with_ties=F) %>%
  ungroup()
rm(dupli)

# drop redundant columns
inventory <- inventory %>% select(-c(RecordedDate, ResponseId, PID, cohort))

# values ------------------------------------------------------------------

# import fall 2023 team inventory data
fa23 <- read_csv("~/Dropbox/Values, Race, & Psych Safety/Data/Values/Fall 2023 Values.csv") %>%
  mutate(cohort = "fa23")

# import fall 2024 team inventory data
fa24 <- read_csv("~/Dropbox/Values, Race, & Psych Safety/Data/Values/Fall 2024 Values.csv") %>%
  mutate(cohort = "fa24")

# import fall 2025 team inventory data
fa25 <- read_csv("~/Dropbox/Values, Race, & Psych Safety/Data/Values/Fall 2025 Values.csv") %>%
  mutate(cohort = "fa25")

# bind 2023-2025 data
values <- rbind(fa23, fa24, fa25)
rm(fa23, fa24, fa25)

# create unique ID column
values$uniqueID <- paste0(values$PID, "_", values$cohort)

# merge demographics and roster data
values <- merge(values, roster, by=c("uniqueID", "PID", "cohort"), all=T)

# create unique team column
values$uniqueTeam <- paste0(values$LT, "_", values$cohort)

# check for duplicates; select most recent response
length(unique(values$uniqueID))
dupli <- values[duplicated(values$uniqueID) | duplicated(values$uniqueID, fromLast=T),]
values$RecordedDate <- as.POSIXct(values$RecordedDate, format = "%m/%d/%y %H:%M")
values <- values %>% group_by(uniqueID) %>%
  slice_max(RecordedDate, n=1, with_ties=F) %>%
  ungroup()
rm(dupli)

# address test and missing data

## change NAs and test to NA
values$Value1[tolower(values$Value1) == "na" | tolower(values$Value1) == "test"] <- NA
values$Value2[tolower(values$Value2) == "na" | tolower(values$Value2) == "test"] <- NA
values$Value3[tolower(values$Value3) == "na" | tolower(values$Value3) == "test"] <- NA
values$Value4[tolower(values$Value4) == "na" | tolower(values$Value4) == "test"] <- NA
values$Value5[tolower(values$Value5) == "na" | tolower(values$Value5) == "test"] <- NA

## create string of values to lower; convert NAs to NA
values$values <- paste(values$Value1, values$Value2, values$Value3, values$Value4, values$Value5, sep=", ")
values$values <- tolower(values$values); values$values[values$values == "na, na, na, na, na"] <- NA

# import GloVe (https://nlp.stanford.edu/data/wordvecs/glove.2024.wikigiga.300d.zip)
glove <- read_table("~/Dropbox/Other/wiki_giga_2024_300_MFT20_vectors_seed_2024_alpha_0.75_eta_0.05_combined.txt", col_names=F)
glove <- as.data.frame(glove)
rownames(glove) <- glove$X1
glove <- glove[,2:ncol(glove)]
glove <- as.matrix(glove)

# create dyad data frame
dyads <- values %>%
  group_by(uniqueTeam) %>% # for each unique team...
  group_modify(~ {
    teams <- .x
    participants <- teams$uniqueID
    # ... create a matrix of participant-peer pairs ...
    expand.grid( 
      subj_id=participants,
      eval_id=participants,
      stringsAsFactors=F
    ) %>%
      filter(subj_id != eval_id) %>%
      mutate(subj.values.list = teams$values[match(subj_id, teams$uniqueID)],
             eval.values.list = teams$values[match(eval_id, teams$uniqueID)])
  }) %>%
  ungroup()

# ... calculate value similarity via pairwise mean cosine sim
dyads$valueSimil <- NA
for (i in 1:nrow(dyads)) {
  subj <- unlist(str_split(dyads$subj.values.list[i], ", "))
  eval <- unlist(str_split(dyads$eval.values.list[i], ", "))

  valueDf <- matrix(0, nrow = 5, ncol = 5)
  
  for (j in 1:5) {
    for (k in 1:5) {
      if (!subj[j] %in% rownames(glove) | !eval[k] %in% rownames(glove)) {
        valueDf[j, k] <- NA
      } else {
        valueDf[j, k] <- text2vec::sim2(
          x = glove[subj[j], , drop = FALSE],
          y = glove[eval[k], , drop = FALSE],
          method = "cosine", 
          norm = "l2"
        )
      }
    }
  }
  
  rowMeansValue <- rowMeans(valueDf, na.rm = TRUE)
  dyads$valueSimil[i] <- mean(rowMeansValue, na.rm = TRUE)
}
rm(valueDf, rowMeansValue, subj, eval, i, j, k)

scores <- dyads %>% group_by(subj_id, uniqueTeam) %>%
  summarise(valueSimil = mean(valueSimil, na.rm=T)) %>%
  rename("uniqueID"="subj_id")
values <- merge(values, scores, by=c("uniqueID", "uniqueTeam"))
rm(dyads, scores, glove)

# merge data --------------------------------------------------------------

df <- merge(demos, inventory, by=c("uniqueID", "uniqueTeam"), all=T)
df <- merge(df, values, by=c("uniqueID", "uniqueTeam"), all=T)

save.image("~/Dropbox/Values, Race, & Psych Safety/Data/R Environment 05.13.26.Rdata")

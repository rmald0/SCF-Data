
scf_data <- read.csv("/Users/rick/Downloads/SCFP2022.csv")

head(scf_data)
str(scf_data)

#Identify relevant factors

col_names <- names(scf_data)
cat(col_names, sep = "\n")


columns_to_keep <- c("AGE", "AGECL", "HHSEX", "EDCL", "FAMSTRUCT", 
                     "RACE", "RACECL", "LF", "ASSET", "FIN", 
                     "CHECKING", "SAVING", "CALL", "STOCKS", "HSTOCKS", 
                     "THRIFT", "IRAKH", "DEBT", "KNOWL", "FINLIT", 
                     "YESFINRISK", "NOFINRISK", "SAVRES1", "SAVRES2", 
                     "SAVRES3", "SAVRES4", "SAVRES5", "SAVRES6", 
                     "SAVRES7", "SAVRES8", "SAVED", "INCOME", 
                     "INCCAT", "WAGEINC", "NETWORTH", "NWCAT", "EDUC", "OCCAT2")

# 
new_scf_data <- scf_data[, columns_to_keep, drop = FALSE]


# Create dummy variables for each age group in the new_scf_data data frame

new_scf_data$AGECL_1 <- ifelse(new_scf_data$AGECL == 1, 1, 0)  # < 35
new_scf_data$AGECL_2 <- ifelse(new_scf_data$AGECL == 2, 1, 0)  # 35-44
new_scf_data$AGECL_3 <- ifelse(new_scf_data$AGECL == 3, 1, 0)  # 45-54
new_scf_data$AGECL_4 <- ifelse(new_scf_data$AGECL == 4, 1, 0)  # 55-64
new_scf_data$AGECL_5 <- ifelse(new_scf_data$AGECL == 5, 1, 0)  # 65-74
new_scf_data$AGECL_6 <- ifelse(new_scf_data$AGECL == 6, 1, 0)  # >= 75


new_scf_data$HHSEX <- ifelse(new_scf_data$HHSEX == 2, 0, 1)

# Rename all AGECL columns based on the corresponding age groups
names(new_scf_data)[names(new_scf_data) == "AGECL_1"] <- "AGE_less_than_35"   # 1. < 35
names(new_scf_data)[names(new_scf_data) == "AGECL_2"] <- "AGE_35_44"          # 2. 35-44
names(new_scf_data)[names(new_scf_data) == "AGECL_3"] <- "AGE_45_54"          # 3. 45-54
names(new_scf_data)[names(new_scf_data) == "AGECL_4"] <- "AGE_55_64"          # 4. 55-64
names(new_scf_data)[names(new_scf_data) == "AGECL_5"] <- "AGE_65_74"          # 5. 65-74
names(new_scf_data)[names(new_scf_data) == "AGECL_6"] <- "AGE_75_plus"        # 6. >= 75


# Create dummy variables for each education category in the EDCL column

new_scf_data$EDCL_no_HS <- ifelse(new_scf_data$EDCL == 1, 1, 0)  # 1. no high school diploma/GED
new_scf_data$EDCL_HS_GED <- ifelse(new_scf_data$EDCL == 2, 1, 0) # 2. high school diploma or GED
new_scf_data$EDCL_some_college <- ifelse(new_scf_data$EDCL == 3, 1, 0) # 3. some college
new_scf_data$EDCL_college_degree <- ifelse(new_scf_data$EDCL == 4, 1, 0) # 4. college degree


# Create dummy variables for key knowledge levels in the KNOWL column

new_scf_data$KNOWL_not_knowledgeable <- ifelse(new_scf_data$KNOWL == -1, 1, 0)  # -1: not at all knowledgeable
new_scf_data$KNOWL_no_knowledge <- ifelse(new_scf_data$KNOWL == 0, 1, 0)        # 0: minimal knowledge
new_scf_data$KNOWL_somewhat_knowledgeable <- ifelse(new_scf_data$KNOWL %in% 1:5, 1, 0) # 1-5: somewhat knowledgeable
new_scf_data$KNOWL_very_knowledgeable <- ifelse(new_scf_data$KNOWL %in% 6:10, 1, 0)   # 6-10: very knowledgeable

# Create dummy variables for the FINLIT column

new_scf_data$FINLIT_0 <- ifelse(new_scf_data$FINLIT == 0, 1, 0)  # 0 correct answers
new_scf_data$FINLIT_1 <- ifelse(new_scf_data$FINLIT == 1, 1, 0)  # 1 correct answer
new_scf_data$FINLIT_2 <- ifelse(new_scf_data$FINLIT == 2, 1, 0)  # 2 correct answers
new_scf_data$FINLIT_3 <- ifelse(new_scf_data$FINLIT == 3, 1, 0)  # 3 correct answers

# Create dummy variables for the RACE column

new_scf_data$RACE_white_non_hispanic <- ifelse(new_scf_data$RACE == 1, 1, 0)     # 1 = white non-Hispanic
new_scf_data$RACE_black_african_american <- ifelse(new_scf_data$RACE == 2, 1, 0) # 2 = black / African American
new_scf_data$RACE_hispanic <- ifelse(new_scf_data$RACE == 3, 1, 0)               # 3 = Hispanic
new_scf_data$RACE_other <- ifelse(new_scf_data$RACE == 5, 1, 0)                  # 5 = Other

# Create dummy variables for the OCCAT2 column

new_scf_data$OCCAT2_managerial_professional <- ifelse(new_scf_data$OCCAT2 == 1, 1, 0)  # 1 = managerial/professional
new_scf_data$OCCAT2_technical_sales_services <- ifelse(new_scf_data$OCCAT2 == 2, 1, 0) # 2 = technical/sales/services
new_scf_data$OCCAT2_other_manual_labor <- ifelse(new_scf_data$OCCAT2 == 3, 1, 0)       # 3 = other/manual labor jobs
new_scf_data$OCCAT2_not_working <- ifelse(new_scf_data$OCCAT2 == 4, 1, 0)              # 4 = not working




library(dplyr)
new_scf_data <- new_scf_data %>%
  select(-c(AGECL, KNOWL, FAMSTRUCT, RACE, RACECL, FINLIT, EDCL, EDUC, OCCAT2))

str(new_scf_data)



column_names_df <- data.frame(Column_Names = names(new_scf_data))
print(column_names_df)



#desired order of columns
desired_column_order <- c(
  "AGE",
  "AGE_less_than_35", 
  "AGE_35_44", 
  "AGE_45_54", 
  "AGE_55_64", 
  "AGE_65_74", 
  "AGE_75_plus",
  "HHSEX",
  "RACE_white_non_hispanic", 
  "RACE_black_african_american", 
  "RACE_hispanic", 
  "RACE_other",
  "EDCL_no_HS", 
  "EDCL_HS_GED", 
  "EDCL_some_college", 
  "EDCL_college_degree", 
  "LF", 
  "ASSET", 
  "FIN", 
  "CHECKING", 
  "SAVING", 
  "CALL", 
  "STOCKS", 
  "HSTOCKS", 
  "THRIFT", 
  "IRAKH", 
  "DEBT", 
  "SAVED", 
  "INCOME", 
  "NWCAT", 
  "NETWORTH", 
  "YESFINRISK", 
  "NOFINRISK", 
  "SAVRES1", 
  "SAVRES2", 
  "SAVRES3", 
  "SAVRES4", 
  "SAVRES5", 
  "SAVRES6", 
  "SAVRES7", 
  "SAVRES8", 
  "KNOWL_not_knowledgeable", 
  "KNOWL_no_knowledge", 
  "KNOWL_somewhat_knowledgeable", 
  "KNOWL_very_knowledgeable", 
  "FINLIT_0", 
  "FINLIT_1", 
  "FINLIT_2", 
  "FINLIT_3", 
  "OCCAT2_managerial_professional", 
  "OCCAT2_technical_sales_services", 
  "OCCAT2_other_manual_labor", 
  "OCCAT2_not_working"
)
new_scf_data <- new_scf_data[, desired_column_order]

write.csv(new_scf_data, file = "/Users/rick/Downloads/scfdata_cleaned.csv", row.names = FALSE)











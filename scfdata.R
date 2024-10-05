
scf_data <- read.csv("/Users/rick/Downloads/SCFP2022.csv")

head(scf_data)
str(scf_data)

#Identify relevant factors

col_names <- names(scf_data)
cat(col_names, sep = "\n")


# Define the columns to keep
columns_to_keep <- c("AGE", "AGECL", "HHSEX", "EDCL", "FAMSTRUCT", 
                     "HOUSECL", "RACE", "RACECL", "LF", "ASSET", "FIN", 
                     "CHECKING", "SAVING", "CALL", "STOCKS", "HSTOCKS", 
                     "THRIFT", "IRAKH", "DEBT", "KNOWL", "FINLIT", 
                     "YESFINRISK", "NOFINRISK", "SAVRES1", "SAVRES2", 
                     "SAVRES3", "SAVRES4", "SAVRES5", "SAVRES6", 
                     "SAVRES7", "SAVRES8", "SAVED", "INCOME", 
                     "INCCAT", "WAGEINC", "NETWORTH", "NWCAT")

# 
new_scf_data <- scf_data[, columns_to_keep, drop = FALSE]


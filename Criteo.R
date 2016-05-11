# purpose: analyze the Criteo Data

# save the current time
time_start <- proc.time()

# ----------------------------------------------------------------------------
# generatl parameters
# ----------------------------------------------------------------------------
setwd("C:/Users/lixun/Desktop/CriteoMLADS/CSV")
# report rows processed and timings
rxOptions(reportProgress = 2) 

# remove scientific notation when saving prediction data
options(scipen=999) 

# number of rows to import from 1training data, use -1 to read all
num_Rows_Train <-  5000 # 1000000 
# number of rows to import from test data, use -1 to read all
num_Rows_Test  <-  1000 # 199999 
# rows per read
# rows_per_read  <-  100000 

# frequency threshold for the categorical variables
t_percent <- 0.05

# variable lists
myvars_I <- c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", 
              "I9", "I10", "I11", "I12", "I13")

myvars_C <- c("factorC1", "factorC2", "factorC3", "factorC4", 
              "factorC5", "factorC6", "factorC7", "factorC8", "factorC9", 
              "factorC10", "factorC11", "factorC12", "factorC13", 
              "factorC14", "factorC15", "factorC16", "factorC17", 
              "factorC18", "factorC19", "factorC20", "factorC21", 
              "factorC22", "factorC23", "factorC24", "factorC25", 
              "factorC26")

myvars <- c(myvars_I, myvars_C)
myformula <- as.formula(paste(" ~ ", paste(myvars, collapse= "+")))

# add this to prediction ID
ref_value <- 59999999

# function and parameter used for all predictions
addRowNum <- function(dataList) {
  dataList$Id = seq(from = (refValue + .rxStartRow), length.out = .rxNumRows)
  dataList$Predicted = dataList$predicted_clicked
  return(dataList)
}

# ----------------------------------------------------------------------------
# convert data from TXT to XDF
# ----------------------------------------------------------------------------
# training data
trainCsv <- "train.txt"
trainXdf <- "train.xdf"
newVarInfo <- list(V1 = list(newName = "clicked"),
                   V2 = list(newName = "I1"),
                   V3 = list(newName = "I2"),
                   V4 = list(newName = "I3"),
                   V5 = list(newName = "I4"),
                   V6 = list(newName = "I5"),
                   V7 = list(newName = "I6"),
                   V8 = list(newName = "I7"),
                   V9 = list(newName = "I8"),
                   V10 = list(newName = "I9"),
                   V11 = list(newName = "I10"),
                   V12 = list(newName = "I11"),
                   V13 = list(newName = "I12"),
                   V14 = list(newName = "I13"),
                   V15 = list(newName = "C1"),
                   V16 = list(newName = "C2"),
                   V17 = list(newName = "C3"),
                   V18 = list(newName = "C4"),
                   V19 = list(newName = "C5"),
                   V20 = list(newName = "C6"),
                   V21 = list(newName = "C7"),
                   V22 = list(newName = "C8"),
                   V23 = list(newName = "C9"),
                   V24 = list(newName = "C10"),
                   V25 = list(newName = "C11"),
                   V26 = list(newName = "C12"),
                   V27 = list(newName = "C13"),
                   V28 = list(newName = "C14"),
                   V29 = list(newName = "C15"),
                   V30 = list(newName = "C16"),
                   V31 = list(newName = "C17"),
                   V32 = list(newName = "C18"),
                   V33 = list(newName = "C19"),
                   V34 = list(newName = "C20"),
                   V35 = list(newName = "C21"),
                   V36 = list(newName = "C22"),
                   V37 = list(newName = "C23"),
                   V38 = list(newName = "C24"),
                   V39 = list(newName = "C25"),
                   V40 = list(newName = "C26"))

rxImport(inData = trainCsv, outFile = trainXdf, numRows = num_Rows_Train,
         colInfo = newVarInfo, 
         transforms = list(TrainValidate = 
                             (ifelse(rbinom(.rxNumRows, size = 1, prob = 0.8), 
                                     "train", "validate"))), 
         overwrite = TRUE)
trainXdf <- RxXdfData(trainXdf)
rxGetInfo(trainXdf, getVarInfo = TRUE, numRows = 5)
# rxSummary(~ TrainValidate, data = trainXdf)

# save the current time
time_convert_train_data <- proc.time()

# test data
testCsv <- "test.txt"
testXdf <- "test.xdf"
newVarInfo <- list(V1 = list(newName = "I1"),
                   V2 = list(newName = "I2"),
                   V3 = list(newName = "I3"),
                   V4 = list(newName = "I4"),
                   V5 = list(newName = "I5"),
                   V6 = list(newName = "I6"),
                   V7 = list(newName = "I7"),
                   V8 = list(newName = "I8"),
                   V9 = list(newName = "I9"),
                   V10 = list(newName = "I10"),
                   V11 = list(newName = "I11"),
                   V12 = list(newName = "I12"),
                   V13 = list(newName = "I13"),
                   V14 = list(newName = "C1"),
                   V15 = list(newName = "C2"),
                   V16 = list(newName = "C3"),
                   V17 = list(newName = "C4"),
                   V18 = list(newName = "C5"),
                   V19 = list(newName = "C6"),
                   V20 = list(newName = "C7"),
                   V21 = list(newName = "C8"),
                   V22 = list(newName = "C9"),
                   V23 = list(newName = "C10"),
                   V24 = list(newName = "C11"),
                   V25 = list(newName = "C12"),
                   V26 = list(newName = "C13"),
                   V27 = list(newName = "C14"),
                   V28 = list(newName = "C15"),
                   V29 = list(newName = "C16"),
                   V30 = list(newName = "C17"),
                   V31 = list(newName = "C18"),
                   V32 = list(newName = "C19"),
                   V33 = list(newName = "C20"),
                   V34 = list(newName = "C21"),
                   V35 = list(newName = "C22"),
                   V36 = list(newName = "C23"),
                   V37 = list(newName = "C24"),
                   V38 = list(newName = "C25"),
                   V39 = list(newName = "C26"))

rxImport(inData = testCsv, outFile = testXdf, numRows = num_Rows_Test,
         colInfo = newVarInfo,  
         transforms = list(clicked = rep(as.integer(0),.rxNumRows),
                           TrainValidate = rep("test",.rxNumRows)), 
         overwrite = TRUE)

testXdf <- RxXdfData(testXdf)
rxGetInfo(testXdf, getVarInfo = TRUE, numRows = 5)
# rxSummary(~ TrainValidate, data = testXdf)

# save the current time
time_convert_test_data <- proc.time()

# ----------------------------------------------------------------------------
# append test data to training data 
# ----------------------------------------------------------------------------
rxImport(testXdf, outFile = trainXdf, 
         varsToKeep = c("clicked","I1", "I2", "I3", "I4", "I5", 
                        "I6", "I7", "I8", "I9", "I10", "I11", "I12", "I13",
                        "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", 
                        "C10", "C11", "C12", "C13", 
                        "C14", "C15", "C16", "C17", "C18", "C19", "C20", 
                        "C21", "C22", "C23", "C24", "C25", "C26",
                        "TrainValidate"),
         append = "rows", overwrite = TRUE)
rxGetInfo(trainXdf, getVarInfo = TRUE, numRows = 5)

# save the current time
time_append_data <- proc.time()

# ----------------------------------------------------------------------------
# create factor variables
# ----------------------------------------------------------------------------
# did not import character as factor, because doing so
# would lead to problems when appending test data to the 
# training data: a factor may have different levels between the two datasets.
rxFactors(inData = trainXdf, 
          outFile = trainXdf, 
          overwrite = TRUE,
          factorInfo = list(factorC1 = list(varName = "C1"),
                            factorC2 = list(varName = "C2"),
                            factorC3 = list(varName = "C3"),
                            factorC4 = list(varName = "C4"),
                            factorC5 = list(varName = "C5"),
                            factorC6 = list(varName = "C6"),
                            factorC7 = list(varName = "C7"),
                            factorC8 = list(varName = "C8"),
                            factorC9 = list(varName = "C9"),
                            factorC10 = list(varName = "C10"),
                            factorC11 = list(varName = "C11"),
                            factorC12 = list(varName = "C12"),
                            factorC13 = list(varName = "C13"),
                            factorC14 = list(varName = "C14"),
                            factorC15 = list(varName = "C15"),
                            factorC16 = list(varName = "C16"),
                            factorC17 = list(varName = "C17"),
                            factorC18 = list(varName = "C18"),
                            factorC19 = list(varName = "C19"),
                            factorC20 = list(varName = "C20"),
                            factorC21 = list(varName = "C21"),
                            factorC22 = list(varName = "C22"),
                            factorC23 = list(varName = "C23"),
                            factorC24 = list(varName = "C24"),
                            factorC25 = list(varName = "C25"),
                            factorC26 = list(varName = "C26")
          )
)

# save the current time
time_create_factors_C <- proc.time()

# ----------------------------------------------------------------------------
# summarize variables
# ----------------------------------------------------------------------------
rxs <- rxSummary(myformula, data = trainXdf)

# save the current time
time_get_summary <- proc.time()

# ----------------------------------------------------------------------------
# character variables: keep only the frequent values
# ----------------------------------------------------------------------------
# define thefrequency threshold
Cvar_Thr <- rxs$nobs.valid * t_percent

# identify and save frequent values
i <- 1
testEF_total <- 0

for (var in myvars_C){
  mydf <- as.data.frame(rxs$categorical[i])
  mydf_sub <- mydf[mydf$Counts>Cvar_Thr,]
  testEF <- ('eeeeeeee' %in% mydf[,var] | 'ffffffff' %in% mydf[,var])  
  
  write.csv(mydf, paste0("../mydf_", var, ".csv"))
  write.csv(mydf_sub, paste0("../mydf_sub_", var, ".csv"))
  write.csv(testEF, paste0("../testEF_", var, ".csv"))
  
  assign(paste0("myset_", var), c(as.character(mydf_sub[,var]),'ffffffff'))
  
  i <- i + 1
  testEF_total <- testEF_total + testEF
}

# ----------------------------------------------------------------------------
# get means for integers
# ----------------------------------------------------------------------------
for (var in myvars_I){
  mean_v <- as.integer(round(rxs$sDataFrame$Mean[rxs$sDataFrame$Name == var]))
  assign(paste0(var, "_mean"), mean_v)
  write.csv(mean_v, paste0("../",var, "_mean.csv"))
}

# ----------------------------------------------------------------------------
# check the minimum value for integers 
# ----------------------------------------------------------------------------
add_value <- abs(min(
  rxs$sDataFrame$Min[rxs$sDataFrame$Name %in% myvars_I])) + 1

write.csv(add_value, "../add_value.csv")

# ----------------------------------------------------------------------------
# feature engineering
# ----------------------------------------------------------------------------
allXdf <- "all.xdf"

rxDataStep(inData = trainXdf, 
           outFile = allXdf,
           transforms = list(
             # fill in missing values
             I1=(ifelse(is.na(I1), I1_Mean, I1)),
             I2=(ifelse(is.na(I2), I2_Mean, I2)),
             I3=(ifelse(is.na(I3), I3_Mean, I3)),
             I4=(ifelse(is.na(I4), I4_Mean, I4)),
             I5=(ifelse(is.na(I5), I5_Mean, I5)),
             I6=(ifelse(is.na(I6), I6_Mean, I6)),
             I7=(ifelse(is.na(I7), I7_Mean, I7)),
             I8=(ifelse(is.na(I8), I8_Mean, I8)),
             I9=(ifelse(is.na(I9), I9_Mean, I9)),
             I10=(ifelse(is.na(I10), I10_Mean, I10)),
             I11=(ifelse(is.na(I11), I11_Mean, I11)),
             I12=(ifelse(is.na(I12), I12_Mean, I12)),
             I13=(ifelse(is.na(I13), I13_Mean, I13)),
             
             # get floor to reduce unique values
             I1_new = as.integer(floor(log(I1+addvalue)^2)),
             I2_new = as.integer(floor(log(I2+addvalue)^2)),
             I3_new = as.integer(floor(log(I3+addvalue)^2)),
             I4_new = as.integer(floor(log(I4+addvalue)^2)),
             I5_new = as.integer(floor(log(I5+addvalue)^2)),
             I6_new = as.integer(floor(log(I6+addvalue)^2)),
             I7_new = as.integer(floor(log(I7+addvalue)^2)),
             I8_new = as.integer(floor(log(I8+addvalue)^2)),
             I9_new = as.integer(floor(log(I9+addvalue)^2)),
             I10_new = as.integer(floor(log(I10+addvalue)^2)),
             I11_new = as.integer(floor(log(I11+addvalue)^2)),
             I12_new = as.integer(floor(log(I12+addvalue)^2)),
             I13_new = as.integer(floor(log(I13+addvalue)^2)),
             
             # character variables - set missing value to "ffffffff"
             C1_new=(ifelse(is.na(C1), "ffffffff", C1)),
             C2_new=(ifelse(is.na(C2), "ffffffff", C2)),
             C3_new=(ifelse(is.na(C3), "ffffffff", C3)),
             C4_new=(ifelse(is.na(C4), "ffffffff", C4)),
             C5_new=(ifelse(is.na(C5), "ffffffff", C5)),
             C6_new=(ifelse(is.na(C6), "ffffffff", C6)),
             C7_new=(ifelse(is.na(C7), "ffffffff", C7)),
             C8_new=(ifelse(is.na(C8), "ffffffff", C8)),
             C9_new=(ifelse(is.na(C9), "ffffffff", C9)),
             C10_new=(ifelse(is.na(C10), "ffffffff", C10)),
             C11_new=(ifelse(is.na(C11), "ffffffff", C11)),
             C12_new=(ifelse(is.na(C12), "ffffffff", C12)),
             C13_new=(ifelse(is.na(C13), "ffffffff", C13)),
             C14_new=(ifelse(is.na(C14), "ffffffff", C14)),
             C15_new=(ifelse(is.na(C15), "ffffffff", C15)),
             C16_new=(ifelse(is.na(C16), "ffffffff", C16)),
             C17_new=(ifelse(is.na(C17), "ffffffff", C17)),
             C18_new=(ifelse(is.na(C18), "ffffffff", C18)),
             C19_new=(ifelse(is.na(C19), "ffffffff", C19)),
             C20_new=(ifelse(is.na(C20), "ffffffff", C20)),
             C21_new=(ifelse(is.na(C21), "ffffffff", C21)),
             C22_new=(ifelse(is.na(C22), "ffffffff", C22)),
             C23_new=(ifelse(is.na(C23), "ffffffff", C23)),
             C24_new=(ifelse(is.na(C24), "ffffffff", C24)),
             C25_new=(ifelse(is.na(C25), "ffffffff", C25)),
             C26_new=(ifelse(is.na(C26), "ffffffff", C26)),
             
             C1_new_norm = (ifelse(C1_new %in% mySetC1, C1_new, "eeeeeeee")),
             C2_new_norm = (ifelse(C2_new %in% mySetC2, C2_new, "eeeeeeee")),
             C3_new_norm = (ifelse(C3_new %in% mySetC3, C3_new, "eeeeeeee")),
             C4_new_norm = (ifelse(C4_new %in% mySetC4, C4_new, "eeeeeeee")),
             C5_new_norm = (ifelse(C5_new %in% mySetC5, C5_new, "eeeeeeee")),
             C6_new_norm = (ifelse(C6_new %in% mySetC6, C6_new, "eeeeeeee")),
             C7_new_norm = (ifelse(C7_new %in% mySetC7, C7_new, "eeeeeeee")),
             C8_new_norm = (ifelse(C8_new %in% mySetC8, C8_new, "eeeeeeee")),
             C9_new_norm = (ifelse(C9_new %in% mySetC9, C9_new, "eeeeeeee")),
             C10_new_norm=(ifelse(C10_new %in% mySetC10, C10_new, "eeeeeeee")),
             C11_new_norm=(ifelse(C11_new %in% mySetC11, C11_new, "eeeeeeee")),
             C12_new_norm=(ifelse(C12_new %in% mySetC12, C12_new, "eeeeeeee")),
             C13_new_norm=(ifelse(C13_new %in% mySetC13, C13_new, "eeeeeeee")),
             C14_new_norm=(ifelse(C14_new %in% mySetC14, C14_new, "eeeeeeee")),
             C15_new_norm=(ifelse(C15_new %in% mySetC15, C15_new, "eeeeeeee")),
             C16_new_norm=(ifelse(C16_new %in% mySetC16, C16_new, "eeeeeeee")),
             C17_new_norm=(ifelse(C17_new %in% mySetC17, C17_new, "eeeeeeee")),
             C18_new_norm=(ifelse(C18_new %in% mySetC18, C18_new, "eeeeeeee")),
             C19_new_norm=(ifelse(C19_new %in% mySetC19, C19_new, "eeeeeeee")),
             C20_new_norm=(ifelse(C20_new %in% mySetC20, C20_new, "eeeeeeee")),
             C21_new_norm=(ifelse(C21_new %in% mySetC21, C21_new, "eeeeeeee")),
             C22_new_norm=(ifelse(C22_new %in% mySetC22, C22_new, "eeeeeeee")),
             C23_new_norm=(ifelse(C23_new %in% mySetC23, C23_new, "eeeeeeee")),
             C24_new_norm=(ifelse(C24_new %in% mySetC24, C24_new, "eeeeeeee")),
             C25_new_norm=(ifelse(C25_new %in% mySetC25, C25_new, "eeeeeeee")),
             C26_new_norm=(ifelse(C26_new %in% mySetC26, C26_new, "eeeeeeee"))
           ),
           transformObjects = list(I1_Mean = I1_mean,
                                   I2_Mean = I2_mean,
                                   I3_Mean = I3_mean,
                                   I4_Mean = I4_mean,
                                   I5_Mean = I5_mean,
                                   I6_Mean = I6_mean,
                                   I7_Mean = I7_mean,
                                   I8_Mean = I8_mean,
                                   I9_Mean = I9_mean,
                                   I10_Mean = I10_mean,
                                   I11_Mean = I11_mean,
                                   I12_Mean = I12_mean,
                                   I13_Mean = I13_mean,
                                   
                                   addvalue = add_value,
                                   
                                   mySetC1 = myset_factorC1,
                                   mySetC2 = myset_factorC2,
                                   mySetC3 = myset_factorC3,
                                   mySetC4 = myset_factorC4,
                                   mySetC5 = myset_factorC5,
                                   mySetC6 = myset_factorC6,
                                   mySetC7 = myset_factorC7,
                                   mySetC8 = myset_factorC8,
                                   mySetC9 = myset_factorC9,
                                   mySetC10 = myset_factorC10,
                                   mySetC11 = myset_factorC11,
                                   mySetC12 = myset_factorC12,
                                   mySetC13 = myset_factorC13,
                                   mySetC14 = myset_factorC14,
                                   mySetC15 = myset_factorC15,
                                   mySetC16 = myset_factorC16,
                                   mySetC17 = myset_factorC17,
                                   mySetC18 = myset_factorC18,
                                   mySetC19 = myset_factorC19,
                                   mySetC20 = myset_factorC20,
                                   mySetC21 = myset_factorC21,
                                   mySetC22 = myset_factorC22,
                                   mySetC23 = myset_factorC23,
                                   mySetC24 = myset_factorC24,
                                   mySetC25 = myset_factorC25,
                                   mySetC26 = myset_factorC26
           ), overwrite = TRUE)

rxGetInfo(allXdf, numRows = 5)

# save the current time
time_feature_engineering <- proc.time()

# ----------------------------------------------------------------------------
# create factor variables
# ----------------------------------------------------------------------------
rxFactors(inData = allXdf, 
          outFile = allXdf, 
          overwrite = TRUE,
          factorInfo = list(factorI1 = list(varName = "I1_new"),
                            factorI2 = list(varName = "I2_new"),
                            factorI3 = list(varName = "I3_new"),
                            factorI4 = list(varName = "I4_new"),
                            factorI5 = list(varName = "I5_new"),
                            factorI6 = list(varName = "I6_new"),
                            factorI7 = list(varName = "I7_new"),
                            factorI8 = list(varName = "I8_new"),
                            factorI9 = list(varName = "I9_new"),
                            factorI10 = list(varName = "I10_new"),
                            factorI11 = list(varName = "I11_new"),
                            factorI12 = list(varName = "I12_new"),
                            factorI13 = list(varName = "I13_new"),
                            
                            factorC1 = list(varName = "C1_new_norm"),
                            factorC2 = list(varName = "C2_new_norm"),
                            factorC3 = list(varName = "C3_new_norm"),
                            factorC4 = list(varName = "C4_new_norm"),
                            factorC5 = list(varName = "C5_new_norm"),
                            factorC6 = list(varName = "C6_new_norm"),
                            factorC7 = list(varName = "C7_new_norm"),
                            factorC8 = list(varName = "C8_new_norm"),
                            factorC9 = list(varName = "C9_new_norm"),
                            factorC10 = list(varName = "C10_new_norm"),
                            factorC11 = list(varName = "C11_new_norm"),
                            factorC12 = list(varName = "C12_new_norm"),
                            factorC13 = list(varName = "C13_new_norm"),
                            factorC14 = list(varName = "C14_new_norm"),
                            factorC15 = list(varName = "C15_new_norm"),
                            factorC16 = list(varName = "C16_new_norm"),
                            factorC17 = list(varName = "C17_new_norm"),
                            factorC18 = list(varName = "C18_new_norm"),
                            factorC19 = list(varName = "C19_new_norm"),
                            factorC20 = list(varName = "C20_new_norm"),
                            factorC21 = list(varName = "C21_new_norm"),
                            factorC22 = list(varName = "C22_new_norm"),
                            factorC23 = list(varName = "C23_new_norm"),
                            factorC24 = list(varName = "C24_new_norm"),
                            factorC25 = list(varName = "C25_new_norm"),
                            factorC26 = list(varName = "C26_new_norm"),
                            
                            factorTrainValidate = list(varName = 
                                                         "TrainValidate")
          )
)

rxGetInfo(allXdf, getVarInfo = TRUE, numRows = 5)

# save the current time
time_create_factors_All <- proc.time()

# ----------------------------------------------------------------------------
# separate out the datasets
# ----------------------------------------------------------------------------
vars_to_keep <- c("clicked", "factorI1", "factorI2", "factorI3", 
                  "factorI4", "factorI5", "factorI6", "factorI7", 
                  "factorI8", "factorI9", "factorI10", "factorI11", 
                  "factorI12", "factorI13", "factorC1", "factorC2", 
                  "factorC3", "factorC4", "factorC5", "factorC6", "factorC7", 
                  "factorC8", "factorC9", "factorC10", "factorC11", 
                  "factorC12", "factorC13", "factorC14", 
                  "factorC15", "factorC16", "factorC17", "factorC18", 
                  "factorC19", "factorC20", "factorC21", 
                  "factorC22", "factorC23", "factorC24", 
                  "factorC25", "factorC26", "factorTrainValidate")

mySplit <- rxSplit(inData = allXdf, splitByFactor = "factorTrainValidate",
                   varsToKeep = vars_to_keep,
                   overwrite = TRUE)

names(mySplit)  

# the following is not reliable because the 1st one can be train or validate
# train_cleaned <- mySplit[[1]] 
# validate_cleaned <- mySplit[[2]]
# test_cleaned <- mySplit[[3]]

# use this approach instead
train_cleaned <- RxXdfData("all.factorTrainValidate.train.xdf")
validate_cleaned <- RxXdfData("all.factorTrainValidate.validate.xdf")
test_cleaned <- RxXdfData("all.factorTrainValidate.test.xdf")

# get count info
total_rows_train <- rxGetInfo(train_cleaned)$numRows
total_rows_validate <- rxGetInfo(validate_cleaned)$numRows
total_rows_test <- rxGetInfo(test_cleaned)$numRows

# save the current time
time_split_data <- proc.time()

# ----------------------------------------------------------------------------
# formula for model
# ----------------------------------------------------------------------------
# for full dataset
myformula_model <- formula(train_cleaned, 
                           depVars = 'clicked', 
                           varsToDrop=c("factorTrainValidate"))

# for testing data
# pred_vars <- c("factorI1", "factorC1")
# myformula_model <- as.formula(paste("clicked ~ ",
#                                    paste(pred_vars, collapse= "+")))

# ----------------------------------------------------------------------------
# decision tree
# ----------------------------------------------------------------------------
# train model
Tree <- rxDTree(myformula_model, 
                data = train_cleaned,
                cp = 0.001, reportProgress = 2)

# save the current time
time_train_model_tree <- proc.time()

# evaluate model
evaluateData <- train_cleaned 
evaluateData <- validate_cleaned 
mypred <- "validate_score_tree.xdf"
rxPredict(Tree, evaluateData, mypred, writeModelVars = TRUE,
          overwrite = TRUE, predVarNames = c("predicted_clicked"))
rxGetInfo(mypred, getVarInfo = TRUE, numRows = 5)
rxRocCurve(actualVarName = "clicked", 
           predVarNames = "predicted_clicked",
           data = mypred)
# save the plot
dev.copy(png,'../roc_plot_tree.png')
dev.off()      
             
roc.df <- rxRoc(actualVarName = "clicked", 
                predVarNames = "predicted_clicked",
                data = mypred)
head(roc.df)
rxAuc(roc.df)

# save the current time
time_prediction_validate_tree <- proc.time()

# score test data
evaluateData <- test_cleaned 
test_data_score <- "test_score_tree.xdf"
submission_data <- "submission_tree.xdf"
submission_data_csv <- "submission_tree.csv"
rxPredict(Tree, evaluateData, test_data_score , 
          overwrite = TRUE, predVarNames = c("predicted_clicked"))
rxGetInfo(test_data_score, getVarInfo = TRUE, numRows = 5)

rxDataStep(inData = test_data_score, 
           outFile = submission_data,
           transformFunc = addRowNum,
           transformObjects = list(refValue = ref_value),
           overwrite = TRUE)
rxGetInfo(submission_data, getVarInfo = TRUE, numRows = 5)

rxXdfToText(submission_data, outFile = submission_data_csv, 
            sep = ",", varsToDrop = c("predicted_clicked"),
            overwrite = TRUE)
# save the current time
time_prediction_test_tree <- proc.time()

# ----------------------------------------------------------------------------
# calculate time as save
# ----------------------------------------------------------------------------
t_total <- time_prediction_test_tree - time_start
t_convert_train_data <- (time_convert_train_data - time_start)[3]
t_convert_test_data  <- (time_convert_test_data  - time_convert_train_data)[3]
t_append_data <- (time_append_data - time_convert_test_data )[3]
t_create_factors_C <- (time_create_factors_C - time_append_data)[3]
t_get_summary <- (time_get_summary - time_create_factors_C)[3]
t_feature_engineering <- (time_feature_engineering - time_get_summary)[3]
t_create_factors_All <- (time_create_factors_All - time_feature_engineering)[3]
t_split_data <- (time_split_data - time_create_factors_All)[3]
t_train_model_tree <- (time_train_model_tree - time_split_data)[3]
t_prediction_validate_tree <- (time_prediction_validate_tree - time_train_model_tree)[3]
t_prediction_test_tree <- (time_prediction_test_tree - time_prediction_validate_tree)[3]

Item <- c("test_EF_fills", "num_Rows_read_Train", 
          "num_Rows_read_Test", "total_rows_train", 
          "total_rows_validate", "total_rows_test", 
          "time_user", "time_system", "time_elapsed", 
          "time_convert_train_data",
          "time_convert_test_data ",
          "time_append_data",
          "time_create_factors_C",
          "time_get_summary",
          "time_feature_engineering",
          "time_create_factors_All",
          "time_split_data",
          "time_train_model_tree",
          "time_prediction_validate_tree",
          "time_prediction_test_tree")

Number <- c(testEF_total, num_Rows_Train, 
            num_Rows_Test, total_rows_train,
            total_rows_validate, total_rows_test, 
            t_total[1], t_total[2], t_total[3],
            t_convert_train_data,
            t_convert_test_data ,
            t_append_data,
            t_create_factors_C,
            t_get_summary,
            t_feature_engineering,
            t_create_factors_All,
            t_split_data,
            t_train_model_tree,
            t_prediction_validate_tree,
            t_prediction_test_tree)

total_time_df <- as.data.frame(cbind(Item = Item, Number = Number))
write.csv(total_time_df, "../time_log.csv", row.names = FALSE)

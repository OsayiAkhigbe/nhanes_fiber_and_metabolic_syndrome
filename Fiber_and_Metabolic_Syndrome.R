# Fiber and Metabolic Syndrome (MetS) from NHANES 2017-2018
# 2026-11-14
# Osayi Akhigbe

# packages used ----
if(!require(pacman)) install.packages("pacman") # installs pacman if not yet installed
pacman::p_load(                                 # installs the following packages if not yet installed
  nhanesA,
  survey,
  mice,
  tidyverse,
  here
)

# What is this project
# Replication of: Association of dietary fiber intake with all-cause and cardiovascular mortality in U.S. adults with metabolic syndrome: NHANES 1999–2018
# https://www.frontiersin.org/journals/nutrition/articles/10.3389/fnut.2025.1659000/full

# Creating function to download and save nhanes data frames if not already downloaded.
nhanes_data_grab <- function(object, df) {
  if(file.exists(here(".data_sets/", paste0(object, ".rds")))) {  
    message("Loading ", df,"...")
    readRDS(here(".data_sets/", paste0(object, ".rds")))    # load if data exist. 
} else {
    message("Downloading ", df,"...")
    download <- nhanes(df)                              # Download data frame
    download |> saveRDS(here(".data_sets/", paste0(object, ".rds")))  # Save data frame to disk in hidden folder as a .rds file
    download
  }
}

# Data used ----

# The Data comes from The National Health and Nutrition Examination Survey (NHANES) 2017-2018 ################ Data from NHANES 1999-2016 will be included later
# Index: https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2017

# create hidden directory if it doesn't exist already
dir.create(here(".data_sets"), showWarnings = FALSE)

# *** Demographics: age, sex, race, income, sample weights ***
# age = RIDAGEYR: "Age in years of the participant at the time of screening. Individuals 80 and over are topcoded at 80 years of age."
# sex = RIAGENDR: Male or Female
# race = RIDRETH3: "This is the race-ethnicity variable included in the demographics file since the 2011-2012 survey cycle to accommodate the oversample of Asian Americans."
# poverty_income_ratio = "INDFMPIR - Ratio of family income to poverty" (Need to control for social economic factors)
# education = DMDEDUC2 (age 20+) "What is the highest grade or level of school {you have/SP has} completed or the highest degree {you have/s/he has} received?"
demo <- nhanes_data_grab("demo", "DEMO_J")

# Dietary Interview - Total Nutrient Intakes, Day 1
diet <- nhanes_data_grab("diet", "DR1TOT_J") # dietary_fiber (g/day) = DR1TFIBE

# Examination Data - Blood Pressure and Body Measures
bpx  <- nhanes_data_grab("bpx", "BPX_J")     # systolic_BP ((1st rdg) mm Hg) = BPXSY1; Diastolic ((1st rdg) mm Hg) = BPXDI1
bmx  <- nhanes_data_grab("bmx", "BMX_J")     # waist_circumference (cm) = BMXWAIST

# Laboratory Data - Plasma Fasting glucose, HDL and triglycerides
glu  <- nhanes_data_grab("glu", "GLU_J")     # glucose (mg/dL) = LBXGLU 
hdl  <- nhanes_data_grab("hdl", "HDL_J")     # hdl_cholesterol (mg/dL) = LBDHDD 
trig <- nhanes_data_grab("trig", "TRIGLY_J")  # triglycerides (mg/dL)= LBXTR (LDL not needed for MetS)

# Questionnaire Data - Blood Pressure & Cholesterol
bpq <- nhanes_data_grab("bpq", "BPQ_J")      # prescription_for_HBP (Yes / No) = BPQ050A




# ###################################*** 
# Mortality: all-cause, CVD-specific
############ Participants included in the Public-use mortality files are all adults  
# "Synthetic data were substituted for follow-up time or underlying cause of death for select records. Information"
# regarding vital status was not perturbed.

# Download Mortality data
mort_years <- c("2017_2018", "2015_2016")    # select mortality years to be downloaded

urls <- c()       # Prevents duplicates in the event of rerunning code

for (mort_year in mort_years) {
  urls <- c(urls, paste0("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_", mort_year, "_MORT_2019_PUBLIC.dat"))
}

print(urls) # These the file(s) will be downloaded from these/this URL(s)

for (url in urls) {
  if(!file.exists(here(".data_sets/", basename(url)))) {                # Only proceeds if file(s) is/are not already in the .data_sets/ directory.
    message("Downloading ", basename(url),"...")
    url |> download.file(destfile = here(".data_sets", basename(url)),  # Downloading all selected mortality years to .data_sets/ directory.
                         method = "wget")                               # Change method depending on you operating system (ex: method = "libcurl")
  }
} 

# Load Mortality data
# This code chunk was taken from the CDC National Death Index 2019 public-use Smaple R Software paogram
#-----------------------------------------------------------------------------------------------------------------
# "
# ** PUBLIC-USE LINKED MORTALITY FOLLOW-UP THROUGH DECEMBER 31, 2019 **
#
# The following R code can be used to read the fixed-width format ASCII public-use Linked
# Mortality Files (LMFs) from a stored location into a R data frame.  Basic frequencies
# are also produced. 

srvyin <- paste(".data_sets/NHANES_2017_2018_MORT_2019_PUBLIC.dat")   # full .DAT name here
srvyout <- "NHANES_2017_2018" # shorthand dataset name here

# Example syntax:
#srvyin <- paste("NHANES_1999_2000_MORT_2019_PUBLIC.dat")   
#srvyout <- "NHANES_1999_2000"      


# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin,
                col_types = "iiiiiiii",
                fwf_cols(seqn = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)

# NOTE:   SEQN is the unique ID for NHANES.
# "
#-----------------------------------------------------------------------------------------------------------------


############# use code book to explain what they mean
dsn |> head()

dsn |> visdat::vis_dat() # All data values are of class integer

# I'll be making my own functions on occasion to practice
missingness <- function(data, object) {      
  data |> count({{object}} |> is.na()) |> # counts how many values are missing in a given object
  mutate(`%` = n/nrow(data)*100)          # makes new columb showing what percent of data is missing/not missing
}

for (variable in dsn |> names()) {
  output <- dsn |> missingness(!!sym(variable))
  print(output)
}


dsn |> inspectdf::inspect_num()

dsn |> inspectdf::inspect_num() |> 
  inspectdf::show_plot()


# might need to rename seqn to SEQN at some point to match NHANES
# Data merging ----
# Merge all data sets by Sequence Number (SEQN)
nhanes_merged <- demo |>
  full_join(diet, by = "SEQN") |>
  full_join(bpx,  by = "SEQN") |>
  full_join(bmx,  by = "SEQN") |>
  full_join(glu,  by = "SEQN") |>
  full_join(hdl,  by = "SEQN") |>
  full_join(trig, by = "SEQN") |>
  full_join(bpq,  by = "SEQN")
# full_join(cvd,  by = "SEQN")
# full_join(all_cause, by = "SEQN")


# Clean up Environment
#rm(list = ls()[!ls() %in% "nhanes_merged"]) 
 
# Select and rename columns
nhanes_predictors <- nhanes_merged |> 
  select(
    SEQN = SEQN,
    age = RIDAGEYR,
    sex = RIAGENDR,
    race = RIDRETH3,
    poverty_income_ratio = INDFMPIR,
    education = DMDEDUC2,
    dietary_fiber = DR1TFIBE,
    systolic_BP = BPXSY1,
    diastolic_BP = BPXDI1,
    waist_circumference = BMXWAIST,
    glucose = LBXGLU,
    hdl_cholesterol = LBDHDD,
    triglycerides = LBXTR,
    prescription_for_HBP = BPQ050A
  )

nhanes_predictors |> head()


# Selection criteria ----
# based on the Adult Treatment Panel III (ATP III) of metabolic syndrome (MetS)
# Index: https://www.ahajournals.org/doi/10.1161/circulationaha.105.169404

# "Measure (any 3 of 5 constitute diagnosis of metabolic syndrome)"

mets_df <- nhanes_predictors |> 
  mutate(
         # "(1) Elevated waist circumference*†	
         # ≥102 cm (≥40 inches) in men
         # ≥88 cm (≥35 inches) in women"
         waist_check = #(age >= 18) |
                      (sex == "Male" & waist_circumference >= 102) |
                      (sex == "Female" & waist_circumference >= 88),
         
         # "(2) Elevated triglycerides	
         # ≥150 mg/dL (1.7 mmol/L)
         # or
         # On drug treatment for elevated triglycerides‡" ################# ?not recorded or can't find?
         trig_check = triglycerides >= 150,
         
         # "(3) Reduced HDL-C	
         # <40 mg/dL (1.03 mmol/L) in men
         # <50 mg/dL (1.3 mmol/L) in women
         # or
         # On drug treatment for reduced HDL-C‡" ################# ?not recorded or can't find?
         hdl_check = (sex == "Male" & hdl_cholesterol < 40) |
                    (sex == "Female" & hdl_cholesterol < 50),
         
         # "(4) Elevated blood pressure	
         # ≥130 mm Hg systolic blood pressure
         # or
         # ≥85 mm Hg diastolic blood pressure
         # or
         # On antihypertensive drug treatment in a patient with a history of hypertension"
         hbp_check = (systolic_BP >= 130) | 
                     (diastolic_BP >= 85) | 
                     (prescription_for_HBP == "Yes"),  # If systolic_BP and diastolic_BP are both below the thresh but prescription_for_HBP is NA then hbp_check is also NA
         
         # "(5) Elevated fasting glucose	
         # ≥100 mg/dL
         # or
         # On drug treatment for elevated glucose" ################# ?not recorded or can't find?
         glu_check = glucose >= 100, 
         
         # "Measure (any 3 of 5 constitute diagnosis of metabolic syndrome)"
         n_criteria = rowSums(
           cbind(waist_check, 
                 trig_check, 
                 hdl_check,
                 hbp_check, 
                 glu_check), 
           na.rm = TRUE)
         
  ) |> filter(n_criteria >= 3 & age >= 18)  # ATP III MetS definition applies to Adults only.


# Making sure the new n_criteria variable makes sense
janitor::tabyl(mets_df, n_criteria) 

mets_df |> count()

# full n_criteria subset accounted for
janitor::tabyl(mets_df, n_criteria) |> 
  select(n) |>  colSums() == mets_df |> count()


# Check all criteria in spread sheet
mets_df |> filter(sex == "Female") |> 
  select(sex, waist_circumference, waist_check) |> 
  arrange(waist_circumference) |> view()

#...

mets_df |> select(
  systolic_BP, diastolic_BP, prescription_for_HBP, hbp_check) |> 
  arrange(hbp_check) |> view()



# Meets MetS criteria
mets_df$waist_circumference |> hist()
summary(mets_df$waist_circumference)

# Hole data set
nhanes_predictors$waist_circumference |> hist()
summary(nhanes_predictors$waist_circumference)



# check classes
mets_df |> visdat::vis_dat()


# Missingness----
# *** logical objects ***
# I will be excluding Missing data for replication of the study
mets_df |> select(
  ends_with("_check")) |> 
  inspectdf::inspect_cat() |> 
  inspectdf::show_plot()

mets_df$glu_check |> janitor::tabyl() # glu_check has more missing then FALSE

mets_df$hbp_check |> janitor::tabyl() # hbp_check has more missing then FALSE

mets_df$trig_check |> janitor::tabyl() # trig_check has 25.23% missing data


# *** factor objects ***
mets_df |> select(
  !ends_with("_check")) |> 
  inspectdf::inspect_cat() |> 
  inspectdf::show_plot()

mets_df$education |> janitor::tabyl() # 0.83% of subset missing

mets_df$prescription_for_HBP |> janitor::tabyl() # 40.78% of subset missing


# *** numeric ***
# glucose and triglycerides have scaling that's off. Maybe some outliers?
mets_df |> select(
  !c(SEQN, n_criteria)) |> 
  inspectdf::inspect_num() |> 
  inspectdf::show_plot()

# table of missingness for numeric objects
mets_df |> 
  select(where(is.numeric) & !c(SEQN, n_criteria)) |> 
  summarise(across(everything(), ~ sum(is.na(.x)))) |> 
  pivot_longer(everything(),
               names_to = "numeric_objects",
               values_to = "n_missing") |> 
  mutate(percent_missing = 
           (n_missing / nrow(mets_df) * 100) |> 
           round(2)) |> 
  arrange(-n_missing)




# Remember that ages greater than or equal to 80 are set to 80
hist(mets_df$age)





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
data_grab <- function(object, df) {
  if(file.exists(here(".data_sets/", paste0(object, ".rds")))) {  
    readRDS(here(".data_sets/", paste0(object, ".rds")))  # load if data exist. 
  } else {
    message("Downloading ", df,"...")
    download <- nhanes(df)                      # Download data frame
    download |> saveRDS(here(".data_sets/", paste0(object, ".rds")))  # Save data frame to disk in hidden folder as a .rds file
    download
  }
}

# if data frames exist in directory .data_sets skip download
# create hidden directory if it doesn't exist already
dir.create(here(".data_sets"), showWarnings = FALSE)

# Data used ----

# The Data comes from The National Health and Nutrition Examination Survey (NHANES) 2017-2018 ################ Data from NHANES 1999-2016 will be included later
# Index: https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2017

########################### if data frames exist in directory .data_sets skip download
# create hidden directory if it doesn't exist already
dir.create(here(".data_sets"), showWarnings = FALSE)

# *** Demographics: age, sex, race, income, sample weights ***
# age = RIDAGEYR: (Age in years of the participant at the time of screening. Individuals 80 and over are topcoded at 80 years of age.)
# sex = RIAGENDR: (male or female)
# race = RIDRETH3: This is the race-ethnicity variable included in the demographics file since the 2011-2012 survey cycle to accommodate the oversample of Asian Americans.
# poverty_income_ratio = INDFMPIR - Ratio of family income to poverty (Need to control for social economic factors)
# education = DMDEDUC2 (age 20+) (What is the highest grade or level of school {you have/SP has} completed or the highest degree {you have/s/he has} received?)

demo <- data_grab("demo", "DEMO_J")

# Dietary Interview - Total Nutrient Intakes, Day 1
diet <- data_grab("diet", "DR1TOT_J") # dietary_fiber (g/day) = DR1TFIBE

# Examination Data - Blood Pressure and Body Measures
bpx  <- data_grab("bpx", "BPX_J")     # systolic_BP ((1st rdg) mm Hg) = BPXSY1; Diastolic ((1st rdg) mm Hg) = BPXDI1
bmx  <- data_grab("bmx", "BMX_J")     # waist_circumference (cm) = BMXWAIST

# Laboratory Data - Plasma Fasting glucose, HDL and triglycerides
glu  <- data_grab("glu", "GLU_J")     # glucose (mg/dL) = LBXGLU 
hdl  <- data_grab("hdl", "HDL_J")     # hdl_cholesterol (mg/dL) = LBDHDD 
trig <- data_grab("trig", "TRIGLY_J")  # triglycerides (mg/dL)= LBXTR (LDL not needed for MetS)

# Questionnaire Data - Blood Pressure & Cholesterol
bpq <- data_grab("bpq", "BPQ_J")      # prescription_for_HBP (Yes / No) = BPQ050A

if(file.exists(here(".data_sets/demo.rds"))){  
  demo <- readRDS(here(".data_sets/demo.rds"))  # if data exist, load and pass into object.
} else {
  demo <- nhanes("DEMO_J")                      # Download data and pass into object
  demo |> saveRDS(here(".data_sets/demo.rds"))  # Save object to disk in hidden folder
}                      

# Dietary Interview - Total Nutrient Intakes, Day 1
diet <- nhanes("DR1TOT_J") # dietary_fiber (g/day) = DR1TFIBE

# Examination Data - Blood Pressure and Body Measures
bpx  <- nhanes("BPX_J")     # systolic_BP ((1st rdg) mm Hg) = BPXSY1; Diastolic ((1st rdg) mm Hg) = BPXDI1
bmx  <- nhanes("BMX_J")     # waist_circumference (cm) = BMXWAIST

# Laboratory Data - Plasma Fasting glucose, HDL and triglycerides
glu  <- nhanes("GLU_J")     # glucose (mg/dL) = LBXGLU 
hdl  <- nhanes("HDL_J")     # hdl_cholesterol (mg/dL) = LBDHDD 
trig <- nhanes("TRIGLY_J")  # triglycerides (mg/dL)= LBXTR (LDL not needed for MetS)

# Questionnaire Data - Blood Pressure & Cholesterol
bpq <- nhanes("BPQ_J")      # prescription_for_HBP (Yes / No) = BPQ050A



# *** 
# Mortality: all-cause, CVD-specific

# rm(list = setdiff(ls(), c("demo", "diet", "bpx", "bmx", "glu", "hdl", "trig", "bpq")))


# Data merging ----
# Merge all data sets by Sequence Number (SEQN)
nhanesMerged <- demo |>
  full_join(diet, by = "SEQN") |>
  full_join(bpx,  by = "SEQN") |>
  full_join(bmx,  by = "SEQN") |>
  full_join(glu,  by = "SEQN") |>
  full_join(hdl,  by = "SEQN") |>
  full_join(trig, by = "SEQN") |>
  full_join(bpq,  by = "SEQN")

# Select and rename columns
nhanes_simple <- nhanesMerged |> 
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
# rm(list = ls()[!ls() %in% "nhanes_simple"]) # Clean up Enviornment

nhanes_simple |> head()

# Selection criteria ----
# based on the Adult Treatment Panel III (ATP III) of metabolic syndrome (MetS)
# Index: https://www.ahajournals.org/doi/10.1161/circulationaha.105.169404

# Measure (any 3 of 5 constitute diagnosis of metabolic syndrome)

mets_df <- nhanes_simple |> 
  mutate(
         # (1) Elevated waist circumference*†	
         # ≥102 cm (≥40 inches) in men
         # ≥88 cm (≥35 inches) in women
         waist_check = #(age >= 18) |
                      (sex == "Male" & waist_circumference >= 102) |
                      (sex == "Female" & waist_circumference >= 88),
         
         # (2) Elevated triglycerides	
         # ≥150 mg/dL (1.7 mmol/L)
         # or
         # On drug treatment for elevated triglycerides‡ ################# ?not recorded or can't find?
         trig_check = triglycerides >= 150,
         
         # (3) Reduced HDL-C	
         # <40 mg/dL (1.03 mmol/L) in men
         # <50 mg/dL (1.3 mmol/L) in women
         # or
         # On drug treatment for reduced HDL-C‡ ################# ?not recorded or can't find?
         hdl_check = (sex == "Male" & hdl_cholesterol < 40) |
                    (sex == "Female" & hdl_cholesterol < 50),
         
         # (4) Elevated blood pressure	
         # ≥130 mm Hg systolic blood pressure
         # or
         # ≥85 mm Hg diastolic blood pressure
         # or
         # On antihypertensive drug treatment in a patient with a history of hypertension
         hbp_check = (systolic_BP >= 130) | 
                     (diastolic_BP >= 85) | 
                     (prescription_for_HBP == "Yes"),  # If systolic_BP and diastolic_BP are both below the thresh but prescription_for_HBP is NA then hbp_check is also NA
         
         # (5) Elevated fasting glucose	
         # ≥100 mg/dL
         # or
         # On drug treatment for elevated glucose ################# ?not recorded or can't find?
         glu_check = glucose >= 100, 
         
         # Measure (any 3 of 5 constitute diagnosis of metabolic syndrome)
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
nhanes_simple$waist_circumference |> hist()
summary(nhanes_simple$waist_circumference)



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



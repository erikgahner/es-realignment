library("tidyverse")
library("haven")

# Download data from: https://www.britishelectionstudy.com/data-objects/panel-study-data/
bes_raw <- read_sav("BES2019_W23_Panel_v23.0.sav")

# Download data from: https://electionstudies.org/data-center/2016-time-series-study/
anes_raw <- read_dta("anes_timeseries_2016.dta")

# Download data from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GDF6Z0
cces_raw <- read_dta("CCES16_Common_OUTPUT_Feb2018_VV.dta")

# Download data from: https://search.gesis.org/research_data/ZA6804
gles_raw <- read_dta("ZA6804_en_v7-0-0.dta")

bes <- bes_raw |> 
  # Limit to England and Wales (from N=97,158 to N=84,118)
  filter(countryW1 %in% c(1, 3) | countryW2 %in% c(1, 3) | countryW3 %in% c(1, 3) | countryW5 %in% c(1, 3) | countryW6 %in% c(1, 3) | countryW7 %in% c(1, 3) | countryW8 %in% c(1, 3) | countryW9 %in% c(1, 3) | countryW10 %in% c(1, 3) | countryW11 %in% c(1, 3) | countryW12 %in% c(1, 3) | countryW13 %in% c(1, 3) | countryW14 %in% c(1, 3) | countryW15 %in% c(1, 3) | countryW16 %in% c(1, 3) | countryW17 %in% c(1, 3) | countryW19 %in% c(1, 3)) |> 
  mutate(
    # Code left2010 variable
    left2010 = case_when(
      p_past_vote_2010 %in% c(1, 6, 8) ~ 0,
      p_past_vote_2010 %in% c(2, 3, 5, 7) ~ 1
    ),
    left2015 = case_when(
      p_past_vote_2015 %in% c(1, 6, 8) ~ 0,
      p_past_vote_2015 %in% c(2, 3, 5, 7) ~ 1
    ),
    left2019 = case_when(
      p_past_vote_2019 %in% c(1, 6) ~ 0,
      p_past_vote_2019 %in% c(2, 3, 5, 7) ~ 1
    ),
    
    male = ifelse(gender == 1, 1, 0),
    age = ifelse(ageW19 < 100, ageW19, NA),
    education = ifelse(p_education_ageW19 < 7, p_education_ageW19, NA),
    unemployed = ifelse(p_work_statW19 == 6, 1, 0),
    ideology = ifelse(leftRightW1 != 9999, leftRightW1, NA),
    
    imm_issue = ifelse(mii_catW1 == 12, 1, 0),
    eco_issue = ifelse(mii_catW1 %in% 26:34, 1, 0),
    
    iv_immigration = ifelse(immigrationLevelW4 == 9999, NA, 5 - immigrationLevelW4)/4,
    iv_economy = ifelse(cutsTooFarNationalW1 != 9999, cutsTooFarNationalW1 - 1, NA)/4,
    
    # Code 2010 vote
    vote2010 = case_when(
      p_past_vote_2010 == 1 ~ "Conservative",
      p_past_vote_2010 == 2 ~ "Labour",
      p_past_vote_2010 == 3 ~ "Liberal Democrat",
      p_past_vote_2010 == 4 ~ "Scottish National Party (SNP)",
      p_past_vote_2010 == 5 ~ "Plaid Cymru",
      p_past_vote_2010 == 6 ~ "United Kingdom Independence Party (UKIP)",
      p_past_vote_2010 == 7 ~ "Green Party",
      p_past_vote_2010 == 8 ~ "British National Party (BNP)"
    ),
    
    # Code 2015 vote
    vote2015 = case_when(
      p_past_vote_2015 == 1 ~ "Conservative",
      p_past_vote_2015 == 2 ~ "Labour",
      p_past_vote_2015 == 3 ~ "Liberal Democrat",
      p_past_vote_2015 == 4 ~ "Scottish National Party (SNP)",
      p_past_vote_2015 == 5 ~ "Plaid Cymru",
      p_past_vote_2015 == 6 ~ "United Kingdom Independence Party (UKIP)",
      p_past_vote_2015 == 7 ~ "Green Party",
      p_past_vote_2015 == 8 ~ "British National Party (BNP)"
    ),
    
    # Code 2019 vote
    vote2019 = case_when(
      p_past_vote_2019 == 1 ~ "Conservative",
      p_past_vote_2019 == 2 ~ "Labour",
      p_past_vote_2019 == 3 ~ "Liberal Democrat",
      p_past_vote_2019 == 4 ~ "Scottish National Party (SNP)",
      p_past_vote_2019 == 5 ~ "Plaid Cymru",
      p_past_vote_2019 == 6 ~ "Brexit Party",
      p_past_vote_2019 == 7 ~ "Green"
    )
    
  ) |> 
  # Select variables
  select(
    left2010, left2019, left2015, vote2010, vote2015, vote2019, male, age, education, unemployed, ideology, iv_immigration, iv_economy, imm_issue, eco_issue
    ) |> 
  mutate(right2010 = 1 - left2010,
         right2015 = 1 - left2015,
         right2019 = 1 - left2019) |> 
  drop_na(iv_immigration)

anes <- anes_raw |> 
  mutate(
    male = ifelse(V161342 == 1, 1, 0),
    age = ifelse(V161267 < 0, NA, V161267),
    education = ifelse(V161270 > 0 & V161270 < 17, V161270, NA),
    unemployed = ifelse(V161277 == 4, 1, 0),
    ideology = ifelse(V162289 >= 0, V162289, NA),
    
    iv_economy = case_when(
      V161205 == 1 ~ 2,
      V161205 == 3 ~ 1,
      V161205 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    left2012 = case_when(
      V161006 == 1 ~ 1,
      V161006 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    imm_issue = case_when(
      V162122a_1 %in% c(41) ~ 1,
      TRUE ~ 0
    ),
    eco_issue = case_when(
      V162122a_1 %in% c(27, 50, 52) ~ 1,
      TRUE ~ 0
    ),
    left2016 = case_when(
      V162034a == 1 ~ 1,
      V162034a == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    imm_decrease = ifelse(V162157 < 0, NA, V162157)
  ) |> 
  mutate(
    right2012 = 1 - left2012,
    right2016 = 1 - left2016,
    iv_immigration = (imm_decrease-1) / 4,
    iv_economy = iv_economy / 2
  ) |> 
  select(male, age, education, unemployed, ideology, left2012, left2016, eco_issue, imm_issue, iv_immigration, iv_economy) |> 
  mutate(
    right2012 = 1 - left2012,
    right2016 = 1 - left2016
  ) |> 
  drop_na(iv_immigration)

cces <- cces_raw |> 
  mutate(
    male = ifelse(gender == 1, 1, 0),
    age = 2016 - birthyr,
    education = educ,
    unemployed = ifelse(employ == 4, 1, 0),
    ideology = ifelse(CC16_340a == 8, NA, CC16_340a),
    
    iv_economy = (5 - CC16_426_1)/4,
    imm_issue = case_when(
      CC16_301d %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    eco_issue = case_when(
      CC16_301i %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    iv_immigration = ((CC16_331_1 - 1) + ifelse(CC16_331_2 == 2, 0, 1) + (CC16_331_3 - 1) + ifelse(CC16_331_4 == 2, 0, 1) + ifelse(CC16_331_5 == 2, 0, 1) + (CC16_331_6 - 1) + ifelse(CC16_331_7 == 2, 0, 1) + ifelse(CC16_331_8 == 2, 0, 1) )/8,
    left2012 = case_when(CC16_326 == 1 ~ 1,
                         CC16_326 == 2 ~ 0,
                         TRUE ~ NA_real_),
    left2016 = case_when(CC16_410a == 1 ~ 0,
                         CC16_410a == 2 ~ 1,
                         TRUE ~ NA_real_)) |> 
  select(male, age, education, unemployed, ideology, left2012, left2016, eco_issue, imm_issue, iv_immigration, iv_economy, CC16_301d, CC16_301i, 
         CC16_331_1, CC16_331_2, CC16_331_3, CC16_331_4, CC16_331_5, CC16_331_6, CC16_331_7, CC16_331_8) |> 
  mutate(
    right2012 = 1 - left2012,
    right2016 = 1 - left2016
  ) |> 
  drop_na(iv_immigration)

gles <- gles_raw |> 
  mutate(iv_immigration = ifelse(kp1_2880b < 0, NA, kp1_2880b - 1) / 4,
         iv_economy = ifelse(kp1_1090 < 0, NA, kp1_1090 - 1) / 6,
         right01 = case_when(
           kp1_190aa == 1 ~ 1, # CDU/CSU
           kp1_190aa == 4 ~ 0, # SPD
           kp1_190aa == 5 ~ 1, # FDP
           kp1_190aa == 6 ~ 0, # GRUENE
           kp1_190aa == 7 ~ 0, # DIE LINKE
           kp1_190aa == 322 ~ 1, # AfD
           TRUE ~ NA_real_
         ),
         right02 = case_when(
           kp9_190aa == 1 ~ 1, # CDU/CSU
           kp9_190aa == 4 ~ 0, # SPD
           kp9_190aa == 5 ~ 1, # FDP
           kp9_190aa == 6 ~ 0, # GRUENE
           kp9_190aa == 7 ~ 0, # DIE LINKE
           kp9_190aa == 322 ~ 1, # AfD
           TRUE ~ NA_real_
         ),
         male = ifelse(kpx_2280 == 1, 1, 0),
         age = 2016 - kpx_2290,
         education = ifelse(!kp1_2320 %in% c(-95, 9), as.factor(kp1_2320), NA),
         unemployed = ifelse(kp1_2340 == 7, 1, 0),
         ideology = ifelse(kp1_1500 > 0, kp1_1500, NA),
  ) |> 
  mutate(problem = str_to_lower(as.character(labelled::to_factor(kp1_840_c1)))) |> 
  mutate(eco_issue = ifelse(str_detect(problem, "econ|tax|financ|fiscal|deficit|budget|wage|jobs"), 1, 0),
         imm_issue = ifelse(str_detect(problem, "imm"), 1, 0)) |> 
  mutate(left01 = ifelse(right01 == 1, 0, 1)) |> 
  mutate(left02 = ifelse(right02 == 1, 0, 1)) |> 
  drop_na(iv_immigration, right01, right02) |> 
  select(male, age, education, unemployed, ideology, left01, left02, right01, right02, eco_issue, imm_issue, iv_immigration, iv_economy)

anes |> write_csv("data_anes.csv")
bes |> write_csv("data_bes.csv")
cces |> write_csv("data_cces.csv")
gles |> write_csv("data_gles.csv")

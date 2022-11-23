library("tidyverse")
library("broom")
library("nnet")
library("patchwork")
library("stargazer")
library("ggalluvial")
library("gridExtra")
library("gt")
library("gtsummary")
library("flextable")
library("scales")
library("rstatix")

anes <- read_csv("data_anes.csv")
bes <- read_csv("data_bes.csv")
cces <- read_csv("data_cces.csv")
gles <- read_csv("data_gles.csv")

fig_dist_bes <- bes |> 
  ggplot(aes(iv_immigration)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#AAAAAA", colour = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1L)) +
  labs(title = "Britain (BES)",
       x = NULL,
       y = NULL) +
  coord_flip() +
  scale_x_continuous(breaks = seq(0, 1, 0.25), labels = c("Increase\na lot", 
                                                          "Increase\na little",
                                                          "Same as\nnow",
                                                          "Decrease\na little",
                                                          "Decrease\na lot")) 

fig_dist_anes <- anes |> 
  ggplot(aes(iv_immigration)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#AAAAAA", colour = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1L)) +
  labs(title = "United States (ANES)",
       x = NULL,
       y = NULL) +
  coord_flip() +
  scale_x_continuous(breaks = seq(0, 1, 0.25), labels = c("Increase\na lot", 
                                                          "Increase\na little",
                                                          "Same as\nnow",
                                                          "Decrease\na little",
                                                          "Decrease\na lot")) 


fig_dist_gles <- gles |> 
  ggplot(aes(iv_immigration)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#AAAAAA", colour = "black") +
  scale_y_continuous(labels = percent) +
  labs(title = "Germany (GLES)",
       x = NULL,
       y = NULL) +
  coord_flip() +
  scale_x_continuous(breaks = seq(0, 1, 0.25), labels = c("Strongly\ndisagree", "Disagree", "Neither or", "Agree", "Strongly\nagree")) 

pdf("figure_distribution.pdf", width = 8, height = 3)
grid.arrange(fig_dist_bes, fig_dist_anes, fig_dist_gles, ncol = 3)
dev.off()

cces |> 
  select(starts_with("CC16_331_")) |> 
  pivot_longer(starts_with("CC16_331_")) |> 
  mutate(value = ifelse(value == 2, 0, 1)) |> 
  group_by(name) |> 
  summarise(value = mean(value), .groups = "drop") |> 
  mutate(question = case_when(
    name == "CC16_331_1" ~ "1. Grant legal status to all illegal immigrants who 
    have held jobs and paid taxes for at least 3 years, 
    and not been convicted of any felony crimes",
    name == "CC16_331_2" ~ "2. Increase the number of border 
    patrols on the U.S.-Mexican border",
    name == "CC16_331_3" ~ "3. Grant legal status to people who were brought
    to the US illegally as children, 
    but who have graduated from a U.S. high school",
    name == "CC16_331_4" ~ "4. Fine U.S. businesses that hire illegal immigrants",
    name == "CC16_331_5" ~ "5. Admit no refugees from Syria",
    name == "CC16_331_6" ~ "6. Increase the number of visas for 
    overseas workers to work in the U.S.",
    name == "CC16_331_7" ~ "7. Identify and deport illegal immigrants",
    name == "CC16_331_8" ~ "8. Ban Muslims from immigrating to the U.S.",
  )) |> 
  ggplot(aes(question, value)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL,
       y = "Number of respondents saying yes (%)") +
  scale_y_continuous(labels = percent_format())

ggsave("figure_cces_distribution_immigration.png", width = 7, height = 4)

NROW(bes[!is.na(bes$iv_immigration),])
NROW(anes[!is.na(anes$iv_immigration),])
NROW(cces[!is.na(cces$iv_immigration),])
NROW(gles[!is.na(gles$iv_immigration),])

realignment_cor_variables <- c("imm_issue", "eco_issue", "iv_immigration", "iv_economy")

cor_bes <- map_df(realignment_cor_variables, ~ bes |> cor_test(all_of(.x))) |> 
  filter((var1 == "imm_issue" & var2 == "iv_immigration") | (var1 == "eco_issue" & var2 == "iv_economy")) |> 
  mutate(dataset = "Britain (BES)")
cor_anes <- map_df(realignment_cor_variables, ~ anes |> cor_test(all_of(.x))) |> 
  filter((var1 == "imm_issue" & var2 == "iv_immigration") | (var1 == "eco_issue" & var2 == "iv_economy")) |> 
  mutate(dataset = "United States (ANES)")
cor_cces <- map_df(realignment_cor_variables, ~ cces |> cor_test(all_of(.x))) |> 
  filter((var1 == "imm_issue" & var2 == "iv_immigration") | (var1 == "eco_issue" & var2 == "iv_economy")) |> 
  mutate(dataset = "United States (CCES)")
cor_gles <- map_df(realignment_cor_variables, ~ gles |> cor_test(all_of(.x))) |> 
  filter((var1 == "imm_issue" & var2 == "iv_immigration") | (var1 == "eco_issue" & var2 == "iv_economy")) |> 
  mutate(dataset = "Germany (GLES)")

cor_df <- bind_rows(cor_bes, cor_anes, cor_cces, cor_gles)

cor_df |> 
  mutate(IV = ifelse(var1 == "imm_issue", "Immigration issue", "Economic issue"),
         DV = case_when(
           var2 == "iv_immigration" ~ "Immigration\ncorrelation",
           var2 == "iv_economy" ~ "Economy\ncorrelation"
         )) |> 
  ggplot(aes(x = DV, y = cor, ymin = cor - (conf.high - cor), ymax = conf.high, colour = DV)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3)  +
  scale_colour_manual(values = c("#398ECF", "#EB4025")) +
  geom_point(size = 2.5, shape=21, colour = "white", stroke = .5) +
  geom_errorbar(width = 0, size = 1) +
  facet_wrap(~ dataset) +
  coord_flip() +
  labs(x = NULL,
       y = "Correlation") +
  theme(legend.position = "none")

ggsave("figure_correlations.pdf", width=8, height = 3)

m1L <- bes |> filter(left2010 == 1) |> lm(formula = right2019 ~ iv_immigration) 
m2R <- bes |> filter(right2010 == 1) |> lm(formula = left2019 ~ iv_immigration) 
m3L <- anes |> filter(left2012 == 1) |> lm(formula = right2016 ~ iv_immigration) 
m4R <- anes |> filter(right2012 == 1) |> lm(formula = left2016 ~ iv_immigration) 
m5L <- cces |> filter(left2012 == 1) |> lm(formula = right2016 ~ iv_immigration) 
m6R <- cces |> filter(right2012 == 1) |> lm(formula = left2016 ~ iv_immigration) 
m7L <- cces |> filter(left2012 == 1) |> lm(formula = right2016 ~ iv_immigration) 
m8R <- cces |> filter(right2012 == 1) |> lm(formula = left2016 ~ iv_immigration) 

stargazer(m1L, m2R, m3L, m4R, m5L, m6R, m7L, m8R,
          keep.stat = c("n", "rsq"),
          digits = 2,
          type = "text")

mlogit_formula <- as.formula("dv ~ iv_immigration + iv_economy + male + age + education + unemployed + ideology")

bes_mlogit_data <- bes |> 
  mutate(dv = case_when(
    right2019 == 1 &  left2010 == 1 ~ "Move right",
    right2019 == 1 &  right2010 == 1 ~ "Same",
    left2019 == 1 &  right2010 == 1 ~ "Move left",
    left2019 == 1 &  left2010 == 1 ~ "Same",
    TRUE ~ NA_character_
  )) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = mlogit_formula, 
              na.action=na.omit)

table(bes_mlogit_data$dv)

bes_mlogit <- multinom(mlogit_formula, data = bes_mlogit_data)

anes_mlogit_data <- anes |> 
  mutate(dv = case_when(
    right2016 == 1 &  left2012 == 1 ~ "Move right",
    right2016 == 1 &  right2012 == 1 ~ "Same",
    left2016 == 1 &  right2012 == 1 ~ "Move left",
    left2016 == 1 &  left2012 == 1 ~ "Same",
    TRUE ~ NA_character_
  )) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = mlogit_formula, 
              na.action=na.omit)

table(anes_mlogit_data$dv)

anes_mlogit <- multinom(mlogit_formula, data = anes_mlogit_data) 

cces_mlogit_data <- cces |> 
  mutate(dv = case_when(
    right2016 == 1 &  left2012 == 1 ~ "Move right",
    right2016 == 1 &  right2012 == 1 ~ "Same",
    left2016 == 1 &  right2012 == 1 ~ "Move left",
    left2016 == 1 &  left2012 == 1 ~ "Same",
    TRUE ~ NA_character_
  )) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = mlogit_formula, 
              na.action=na.omit)

table(cces_mlogit_data$dv)

cces_mlogit <- multinom(mlogit_formula, data = cces_mlogit_data) 

gles_mlogit_data <- gles |> 
  mutate(dv = case_when(
    right02 == 1 &  left01 == 1 ~ "Move right",
    right02 == 1 &  right01 == 1 ~ "Same",
    left02 == 1 &  right01 == 1 ~ "Move left",
    left02 == 1 &  left01 == 1 ~ "Same",
    TRUE ~ NA_character_
  )) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = mlogit_formula, 
              na.action=na.omit)

table(gles_mlogit_data$dv)

gles_mlogit <- multinom(mlogit_formula, data = gles_mlogit_data)

stargazer(bes_mlogit, anes_mlogit, cces_mlogit, gles_mlogit, type = "text", 
          covariate.labels = c("Immigration attitude", "Economic attitude", "Male", "Age", "Education", "Unemployed", "Ideology"),
          digits = 2,
          add.lines = list(c("N", NROW(bes_mlogit_data), " ", NROW(anes_mlogit_data), "", NROW(cces_mlogit_data), "", NROW(gles_mlogit_data))),
          out = "table_mlogit_attitude.htm")

bes_mlogit_data_2015 <- bes |> 
  mutate(dv = case_when(
    right2019 == 1 &  left2015 == 1 ~ "Move right",
    right2019 == 1 &  right2015 == 1 ~ "Same",
    left2019 == 1 &  right2015 == 1 ~ "Move left",
    left2019 == 1 &  left2015 == 1 ~ "Same",
    TRUE ~ NA_character_
  )) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = mlogit_formula, 
              na.action = na.omit)

bes_mlogit_2015 <- multinom(mlogit_formula, data = bes_mlogit_data_2015)

stargazer(bes_mlogit_2015, type = "text", 
          covariate.labels = c("Immigration attitude", "Economic attitude", "Male", "Age", "Education", "Unemployed", "Ideology"),
          digits = 2,
          add.lines = list(c("N", NROW(bes_mlogit_data_2015))),
          out = "table_mlogit_attitude_2015.htm")


bes |> 
  select(left2010, left2015, left2019, everything()) |> 
  select(-starts_with("vote"), -starts_with("right")) |> 
  tbl_summary(type = everything() ~ "continuous", 
              statistic = list(all_continuous() ~ "{mean} ({sd}) [{min}, {max}]"),
              label = list(left2010 = "Left, 2010", left2015 = "Left, 2015", left2019 = "Left, 2019",
                           male = "Male", age = "Age", education = "Education", unemployed = "Unemployed", ideology = "Ideology",
                           iv_immigration = "Immigration attitude", iv_economy = "Economic attitude",
                           imm_issue = "Immigration issue", eco_issue = "Economic issue"),
              digits = all_continuous() ~ 2,
              missing = "no") |> 
  as_flex_table() |> 
  save_as_docx(path = "table_descriptive-bes.docx")


anes |> 
  select(-starts_with("right")) |> 
  select(left2012, left2016, male, age, education, unemployed, ideology, iv_immigration, iv_economy, imm_issue, eco_issue) |> 
  tbl_summary(type = everything() ~ "continuous", 
              statistic = list(all_continuous() ~ "{mean} ({sd}) [{min}, {max}]"),
              label = list(left2012 = "Left, 2012", left2016 = "Left, 2016",
                           male = "Male", age = "Age", education = "Education", unemployed = "Unemployed", ideology = "Ideology",
                           iv_immigration = "Immigration attitude", iv_economy = "Economic attitude",
                           imm_issue = "Immigration issue", eco_issue = "Economic issue"),
              digits = all_continuous() ~ 2,
              missing = "no") |> 
  as_flex_table() |> 
  save_as_docx(path = "table_descriptive-anes.docx")

cces |> 
  select(-starts_with("right")) |> 
  select(left2012, left2016, male, age, education, unemployed, ideology, iv_immigration, iv_economy, imm_issue, eco_issue) |> 
  tbl_summary(type = everything() ~ "continuous", 
              statistic = list(all_continuous() ~ "{mean} ({sd}) [{min}, {max}]"),
              label = list(left2012 = "Left, 2012", left2016 = "Left, 2016",
                           male = "Male", age = "Age", education = "Education", unemployed = "Unemployed", ideology = "Ideology",
                           iv_immigration = "Immigration attitude", iv_economy = "Economic attitude",
                           imm_issue = "Immigration issue", eco_issue = "Economic issue"),
              digits = all_continuous() ~ 2,
              missing = "no") |> 
  as_flex_table() |> 
  save_as_docx(path = "table_descriptive-cces.docx")

gles |> 
  select(-starts_with("right")) |> 
  select(left01, left02, male, age, education, unemployed, ideology, iv_immigration, iv_economy, imm_issue, eco_issue) |> 
  tbl_summary(type = everything() ~ "continuous", 
              statistic = list(all_continuous() ~ "{mean} ({sd}) [{min}, {max}]"),
              label = list(left01 = "Left, 2016", left02 = "Left, 2018",
                           male = "Male", age = "Age", education = "Education", unemployed = "Unemployed", ideology = "Ideology",
                           iv_immigration = "Immigration attitude", iv_economy = "Economic attitude",
                           imm_issue = "Immigration issue", eco_issue = "Economic issue"),
              digits = all_continuous() ~ 2,
              missing = "no") |> 
  as_flex_table() |> 
  save_as_docx(path = "table_descriptive-gles.docx")

m1bi <- bes |> lm(formula = iv_immigration ~ imm_issue + eco_issue + male + age + education + unemployed + ideology) 
m2be <- bes |> lm(formula = iv_economy ~ imm_issue + eco_issue + male + age + education + unemployed + ideology) 
m3ai <- anes |> lm(formula = iv_immigration ~ imm_issue + eco_issue + male + age + education + unemployed + ideology) 
m4ae <- anes |> lm(formula = iv_economy ~ imm_issue + eco_issue + male + age + education + unemployed + ideology) 
m5ci <- cces |> lm(formula = iv_immigration ~ imm_issue + eco_issue + male + age + education + unemployed + ideology) 
m6ce <- cces |> lm(formula = iv_economy ~ imm_issue + eco_issue + male + age + education + unemployed + ideology) 
m7gi <- gles |> lm(formula = iv_immigration ~ imm_issue + eco_issue + male + age + education + unemployed + ideology) 
m8ge <- gles |> lm(formula = iv_economy ~ imm_issue + eco_issue + male + age + education + unemployed + ideology) 

stargazer(m1bi, m2be, m3ai, m4ae, m5ci, m6ce, m7gi, m8ge,
          keep.stat = c("n", "rsq"),
          covariate.labels = c("Immigration issue", "Economic issue", "Male", "Age", "Education", "Unemployed", "Ideology"),
          out = "table_reg_issue_salience.htm",
          digits = 2, type = "text")


cces_itemreg_1 <- cces |> 
  mutate(CC16_331_1_rc = (CC16_331_1 - 1)) |> 
  mutate(dv = case_when(right2016 == 1 &  left2012 == 1 ~ "Move right", right2016 == 1 &  right2012 == 1 ~ "Same", left2016 == 1 &  right2012 == 1 ~ "Move left", left2016 == 1 &  left2012 == 1 ~ "Same", TRUE ~ NA_character_)) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = as.formula("dv ~ CC16_331_1_rc + male + age + education + unemployed + ideology"), na.action=na.omit) |> 
  multinom(as.formula("dv ~ CC16_331_1_rc + male + age + education + unemployed + ideology"), data = _) |> 
  tidy() |> 
  select(y.level, term, estimate, std.error) |> 
  filter(term == "CC16_331_1_rc") 

cces_itemreg_2 <- cces |> 
  mutate(CC16_331_2_rc = ifelse(CC16_331_2 == 2, 0, 1)) |> 
  mutate(dv = case_when(right2016 == 1 &  left2012 == 1 ~ "Move right", right2016 == 1 &  right2012 == 1 ~ "Same", left2016 == 1 &  right2012 == 1 ~ "Move left", left2016 == 1 &  left2012 == 1 ~ "Same", TRUE ~ NA_character_)) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = as.formula("dv ~ CC16_331_2_rc + male + age + education + unemployed + ideology"), na.action=na.omit) |> 
  multinom(as.formula("dv ~ CC16_331_2_rc + male + age + education + unemployed + ideology"), data = _) |> 
  tidy() |> 
  select(y.level, term, estimate, std.error) |> 
  filter(term == "CC16_331_2_rc") 

cces_itemreg_3 <- cces |> 
  mutate(CC16_331_3_rc = CC16_331_3 - 1) |> 
  mutate(dv = case_when(right2016 == 1 &  left2012 == 1 ~ "Move right", right2016 == 1 &  right2012 == 1 ~ "Same", left2016 == 1 &  right2012 == 1 ~ "Move left", left2016 == 1 &  left2012 == 1 ~ "Same", TRUE ~ NA_character_)) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = as.formula("dv ~ CC16_331_3_rc + male + age + education + unemployed + ideology"), na.action=na.omit) |> 
  multinom(as.formula("dv ~ CC16_331_3_rc + male + age + education + unemployed + ideology"), data = _) |> 
  tidy() |> 
  select(y.level, term, estimate, std.error) |> 
  filter(term == "CC16_331_3_rc") 

cces_itemreg_4 <- cces |> 
  mutate(CC16_331_4_rc = ifelse(CC16_331_4 == 2, 0, 1)) |> 
  mutate(dv = case_when(right2016 == 1 &  left2012 == 1 ~ "Move right", right2016 == 1 &  right2012 == 1 ~ "Same", left2016 == 1 &  right2012 == 1 ~ "Move left", left2016 == 1 &  left2012 == 1 ~ "Same", TRUE ~ NA_character_)) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = as.formula("dv ~ CC16_331_4_rc + male + age + education + unemployed + ideology"), na.action=na.omit) |> 
  multinom(as.formula("dv ~ CC16_331_4_rc + male + age + education + unemployed + ideology"), data = _) |> 
  tidy() |> 
  select(y.level, term, estimate, std.error) |> 
  filter(term == "CC16_331_4_rc") 

cces_itemreg_5 <- cces |> 
  mutate(CC16_331_5_rc = ifelse(CC16_331_5 == 2, 0, 1)) |> 
  mutate(dv = case_when(right2016 == 1 &  left2012 == 1 ~ "Move right", right2016 == 1 &  right2012 == 1 ~ "Same", left2016 == 1 &  right2012 == 1 ~ "Move left", left2016 == 1 &  left2012 == 1 ~ "Same", TRUE ~ NA_character_)) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = as.formula("dv ~ CC16_331_5_rc + male + age + education + unemployed + ideology"), na.action=na.omit) |> 
  multinom(as.formula("dv ~ CC16_331_5_rc + male + age + education + unemployed + ideology"), data = _) |> 
  tidy() |> 
  select(y.level, term, estimate, std.error) |> 
  filter(term == "CC16_331_5_rc") 

cces_itemreg_6 <- cces |> 
  mutate(CC16_331_6_rc = CC16_331_6 - 1) |> 
  mutate(dv = case_when(right2016 == 1 &  left2012 == 1 ~ "Move right", right2016 == 1 &  right2012 == 1 ~ "Same", left2016 == 1 &  right2012 == 1 ~ "Move left", left2016 == 1 &  left2012 == 1 ~ "Same", TRUE ~ NA_character_)) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = as.formula("dv ~ CC16_331_6_rc + male + age + education + unemployed + ideology"), na.action=na.omit) |> 
  multinom(as.formula("dv ~ CC16_331_6_rc + male + age + education + unemployed + ideology"), data = _) |> 
  tidy() |> 
  select(y.level, term, estimate, std.error) |> 
  filter(term == "CC16_331_6_rc") 

cces_itemreg_7 <- cces |> 
  mutate(CC16_331_7_rc = ifelse(CC16_331_7 == 2, 0, 1)) |> 
  mutate(dv = case_when(right2016 == 1 &  left2012 == 1 ~ "Move right", right2016 == 1 &  right2012 == 1 ~ "Same", left2016 == 1 &  right2012 == 1 ~ "Move left", left2016 == 1 &  left2012 == 1 ~ "Same", TRUE ~ NA_character_)) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = as.formula("dv ~ CC16_331_7_rc + male + age + education + unemployed + ideology"), na.action=na.omit) |> 
  multinom(as.formula("dv ~ CC16_331_7_rc + male + age + education + unemployed + ideology"), data = _) |> 
  tidy() |> 
  select(y.level, term, estimate, std.error) |> 
  filter(term == "CC16_331_7_rc") 

cces_itemreg_8 <- cces |> 
  mutate(CC16_331_8_rc = ifelse(CC16_331_8 == 2, 0, 1)) |> 
  mutate(dv = case_when(right2016 == 1 &  left2012 == 1 ~ "Move right", right2016 == 1 &  right2012 == 1 ~ "Same", left2016 == 1 &  right2012 == 1 ~ "Move left", left2016 == 1 &  left2012 == 1 ~ "Same", TRUE ~ NA_character_)) |> 
  mutate(dv = fct_relevel(dv, c("Same", "Move left", "Move right"))) |> 
  model.frame(formula = as.formula("dv ~ CC16_331_8_rc + male + age + education + unemployed + ideology"), na.action=na.omit) |> 
  multinom(as.formula("dv ~ CC16_331_8_rc + male + age + education + unemployed + ideology"), data = _) |> 
  tidy() |> 
  select(y.level, term, estimate, std.error) |> 
  filter(term == "CC16_331_8_rc") 

cces_itemreg_all <- bind_rows(cces_itemreg_1, cces_itemreg_2, cces_itemreg_3, cces_itemreg_4,
                              cces_itemreg_5, cces_itemreg_6, cces_itemreg_7, cces_itemreg_8)

cces_itemreg_all |> 
  mutate(term = case_when(
    term == "CC16_331_1_rc" ~ "1. Grant legal status to all illegal immigrants who 
    have held jobs and paid taxes for at least 3 years, 
    and not been convicted of any felony crimes",
    term == "CC16_331_2_rc" ~ "2. Increase the number of border 
    patrols on the U.S.-Mexican border",
    term == "CC16_331_3_rc" ~ "3. Grant legal status to people who were brought
    to the US illegally as children, 
    but who have graduated from a U.S. high school",
    term == "CC16_331_4_rc" ~ "4. Fine U.S. businesses that hire illegal immigrants",
    term == "CC16_331_5_rc" ~ "5. Admit no refugees from Syria",
    term == "CC16_331_6_rc" ~ "6. Increase the number of visas for 
    overseas workers to work in the U.S.",
    term == "CC16_331_7_rc" ~ "7. Identify and deport illegal immigrants",
    term == "CC16_331_8_rc" ~ "8. Ban Muslims from immigrating to the U.S.",
  )) |> 
  ggplot(aes(y.level, estimate, ymin = estimate - std.error * 1.96, ymax = estimate + std.error * 1.96)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0) +
  labs(y = "Coefficient (w. 95% confidence intervals)",
       x = NULL) +
  facet_wrap(~ term, ncol = 2)

ggsave("figure_cces_items.png", width = 7, height = 8, bg = "white")


sink("sessionInfo.txt") 
cat("\nThe results are produced with 02-analysis.R with this session in R:\n\n")
sessionInfo()
sink() 

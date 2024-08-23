#ReadMe----
#Purpose: Cleaning Teacher Production Statewide by Organization and Certification Program
#Author: Andrew Avitabile
#Source: Data come from Texas Public Education Information Resource (TPEIR): https://www.texaseducationinfo.org/PickList.aspx?Page=Teacher+Certification&ReportName=Tpeir_ed_cert_prog_org&PickList=School+Year&SubList=Certification+Program&Title=Teacher+Production+Statewide+by+Organization+and+Certification+Program&Graph=N&from=Home%2fTopic%2fTeacher+Certification

#General----
#Load packages
pacman::p_load(tidyverse, here, showtext, readxl, fixest, broom, glue)

#Clear all
rm(list=ls())

#Add fonts
font_add(family = "LMRoman", regular = here("lmroman10-regular.otf"))
showtext_auto()

#Load data----
raw <- read_excel(here("data", "raw", "CrystalReportViewer1.xls"))

#Clean data----
# Some cleaning to get raw Excel file tidyclean <- raw %>%
clean <- raw %>%
  # Rename columns 
  rename(epp = 1, cert_area = `...2`, cert_person = `...3`, cert = `...4`, init_cert = `...5`) %>%
  # Fill in EPP variable
  fill(epp, .direction = "up") %>%
  # Generate a program type variable
  mutate(cert_type = ifelse(epp == "Certification Program:", cert_area, NA)) %>%
  fill(cert_type, .direction = "down") %>%
  # Generate school year variable
  mutate(school_year = as.numeric(str_extract(ifelse(epp == "School Year:", cert_area, NA), "^.{4}"))) %>%
  fill(school_year, .direction = "down") %>%
  filter(!is.na(cert_area), !is.na(cert_person)) %>%
  # Remove rows separating data between years
  filter(!is.na(cert_area), cert_area != "texaseducationinfo.org") %>%
  # Make columns with counts of teachers numeric
  mutate(across(.cols = c("cert_person", "cert", "init_cert"), as.numeric)) %>%
  relocate(school_year, epp) %>%
  # Flag BEP programs
  mutate(BEP = ifelse(epp %in% c("University of North Texas", "University of North Texas - Dallas", "Texas Womans University", "University of Texas - Dallas", "University of Texas - Arlington", "Texas A&M University - Commerce", "Texas Tech University"), "BEP", "Non-BEP")) %>%
  # Flag eligibility
  mutate(post = case_when(BEP == 1 & school_year >= 2016 ~ 1,
                          epp == "University of North Texas - Dallas" ~ 1,
                          TRUE ~ 0)) %>%
  # Make a short version of cert area
  mutate(cert_area_short = case_when(cert_area == "Bilingual Education" ~ "bl",
                                     cert_area == "English Lang Arts and Reading" ~ "ela",
                                     cert_area == "Generalist" ~ "g",
                                     cert_area == "Mathematics" ~ "m",
                                     cert_area == "Science" ~ "sci",
                                     cert_area == "Social Studies" ~ "ss",
                                     cert_area == "Career and Technical Education" ~ "cte",
                                     cert_area == "Fine Arts" ~ "fa",
                                     cert_area == "Computer Science" ~ "cs",
                                     cert_area == "Health & Physical Education" ~ "hel",
                                     cert_area == "Languages Other than English" ~ "loe",
                                     cert_area == "Special Education" ~ "sped",
                                     cert_area == "Total" ~ "tot"
                                     )) %>%
  # Make a short version of program type
  mutate(cert_type_short = case_when(cert_type == "Alternative Program" ~ "alt",
                                     cert_type == "Post-Baccalaureate" ~ "post_bac",
                                     cert_type == "Standard Program" ~ "trad"
  ))

# Get a data set of just bilingual education certifications
bilingual <- clean %>% 
  filter(cert_area == "Bilingual Education") %>%
  arrange(desc(school_year), desc(cert))
  
#Program-level dataset
prog_level_data <- pivot_wider(clean, 
                               id_cols = c(epp, school_year, BEP, post), 
                               values_from = cert, 
                               names_from = c("cert_type_short", "cert_area_short"),
                               names_glue = "{cert_type_short}_{cert_area_short}") %>%
  # Make all missing numeric variables 0
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x))) %>%
  # Make calculate total certifications by program type
  rowwise() %>%
  mutate(alt = sum(c_across(contains("alt"))),
         post_bac = sum(c_across(contains("post_bac"))),
         trad = sum(c_across(contains("trad")))) %>%
  # Calculate number of bilingual certs across all program types
  mutate(bl = sum(c_across(contains("_bl")))) %>%
  ungroup %>%
  # Calculate the proportion of teachers at traditional EEPs that get their certification in Bilingual Education
  mutate(trad_bl_prop = trad_bl/trad) %>%
  # Rearrange dataset
  relocate(epp, school_year, BEP, post, alt, post_bac, trad, contains("bl"))

# Save data to .csv
write.csv(prog_level_data, here("data", "cleaned", "tx_teacher_certs_by_program.csv"))

#Plot data----

# Number of traditional Bilingual Educator certificates by BEP participation and year
prog_level_data %>%
  filter(bl>0) %>%
  group_by(school_year, BEP) %>%
  summarise(trad_bl = sum(trad_bl, na.rm = T)) %>%
  ggplot(aes(x=school_year, y=trad_bl, color=BEP)) +
  geom_line() +
  theme_minimal()  +
  # scale_y_continuous(limits = c(0, 100), expand = c(0, 10)) +
  ylab("# of certifications") +
  xlab("Year")

# Number of traditional Bilingual Educator certificates by year
prog_level_data %>%
  group_by(school_year) %>%
  summarise(trad_bl = sum(trad_bl)) %>%
  ggplot(aes(x=school_year, y=trad_bl)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 10)) +
  ylab("# of certifications") +
  xlab("Year")

#Event study----
# Run the event study
model <- feols(fml = bl ~ post*relevel(as.factor(school_year), 5) | as.factor(epp),
               data = filter(prog_level_data, trad_bl <= 200))

summary(model)

# Tidy the results - keeping just the interaction terms
results <- tidy(model) %>%
  filter(grepl("post:relevel\\(as.factor\\(school_year\\), 5", term)) %>%
  mutate(
    year = as.numeric(gsub("post:relevel\\(as.factor\\(school_year\\), 5\\)", "", term)),
    lower_ci = estimate - 1.96 * std.error,
    upper_ci = estimate + 1.96 * std.error
  )

# Create the plot
ggplot(results, aes(x = year, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "black") +
  labs(
    x = "Years Relative to Event",
    y = "Coefficient Estimate",
    title = "Event Study: Effect on Certification",
    caption = "95% Confidence Intervals shown"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = c(2012:2021), minor_breaks = c())
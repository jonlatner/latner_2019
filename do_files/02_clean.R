# Top commands ----
# Create empty R application (no figures, data frames, packages, etc.)
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()

rm(list=ls(all=TRUE))

# FOLDERS
setwd("/Users/jonathanlatner/GitHub/latner_2019/")

data_files = "data_files/"

# LIBRARY
library(tidyverse)

options(scipen = 999) # disable scientific notation

# LOAD DATA ----

df_psid <- readRDS(file = paste0(data_files,"psid.rds"))

# CLEAN ----

# KEEP IF HEAD OR SPOUSE
df_psid <- df_psid %>% 
        mutate(head = ifelse((relation.head == 1 | relation.head == 10) & sequence == 1, 1, 
                             ifelse((relation.head == 2 | relation.head == 20 | relation.head == 22) & sequence <= 20, 2, 0))) %>% 
        select(-relation.head, -sequence, -pernum) %>% # drop head and sequence
        filter(head == 1 | head == 2) %>% 
        select(pid, year, head, household, everything()) %>% 
        arrange(pid, year) %>% 
        rename(family = ID1968)

# PSID oversamples

df_psid <- df_psid %>% 
        mutate(seo = ifelse(family>5000 & family<7000, 1, 0), # SEO (included)
               src = ifelse(family <= 3000, 1, 0), # SRC (included)
               latino = ifelse(family>7000 & family<9500, 1, 0), # latino (excluded)
               immigrant = ifelse(family>3000 & family<5000, 1, 0)) # immigrant sample (excluded)

# TOP CODE WAGES
df_psid <- df_psid %>% 
        mutate(tot_fam_income = ifelse((tot_fam_income > 99997 & year <= 1979), no = tot_fam_income, yes = NA))  %>% 
        mutate(wages_hd = ifelse(((
        year <= 1969 | # categorized prior to 1970
                wages_hd > 99997 & year <= 1982) | # top code
                (wages_hd > 999997 & (year >= 1983 & year<=1999))), # top code
        no = wages_hd, yes = NA))

df_psid <- df_psid %>% 
        mutate(tot_fam_income_adj_fz = tot_fam_income/sqrt(fam_size))

# EMPLOYMENT STATUS (EMPLOYED, UNEMPLOYED, STUDENT, RETIRED)
# FAMILY LEVEL (HEAD) < 1997
# INDIVIDUAL LEVEL >= 1997
df_psid <- df_psid %>% 
        mutate(emp = ifelse(((emp_hd == 1 | emp_hd == 2) & year <= 1975) | ((emp_hd == 1 | emp_hd == 2 | emp_hd == 3) & (year >= 1976 & year < 1997)) | ((emp_ind == 1 | emp_ind == 2 | emp_ind == 3) & year >= 1997) , yes = 1, no = 0),
               unemp   = ifelse((emp_hd == 2 & year <= 1975) | ((emp_hd == 2 | emp_hd == 3) & (year >= 1976 & year < 1997)) | ((emp_ind == 2 | emp_ind == 3) & year >= 1997) , yes = 1, no = 0),
               student = ifelse(((emp_hd == 5 & year <= 1975) | (emp_hd == 7 & (year >= 1976 & year < 1997)) | (emp_ind == 7 & year >= 1997)), yes = 1, no = 0),
               retired = ifelse((emp_hd == 3 & year <= 1975) | ((emp_hd == 4 | emp_hd == 5) & (year >= 1976 & year < 1997)) | ((emp_ind == 4 | emp_ind == 5) & year >= 1997), yes = 1, no = 0)) %>% 
        select(-emp_hd, -emp_wf, -emp_ind)

# AGE-COHORT
# Age is sometimes not consequential
# Several options: can make age consequential from first observation or calculate age from birth year (min, mamode) so that age is sequential
# I decide to leave it alone        
# Generate age categories

df_psid <- df_psid %>% mutate(age_cat = ifelse(age >= 25 & age < 35, 1, 
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45, 3, 0))))

df_psid <- df_psid %>% 
        mutate(cohort = year - age,
               cohgr = ifelse(cohort >= 1910 & cohort < 1920, 1,
                              ifelse(cohort >= 1920 & cohort < 1930, 2,
                                     ifelse(cohort >= 1930 & cohort < 1940, 3,
                                            ifelse(cohort >= 1940 & cohort < 1950, 4,
                                                   ifelse(cohort >= 1950 & cohort < 1960, 5,
                                                          ifelse(cohort >= 1960 & cohort < 1970, 6,
                                                                 ifelse(cohort >= 1970 & cohort < 1980, 7, 0)))))))) %>%
        ungroup() %>% 
        arrange(pid, year)

# UNEMPLOYMENT
df_psid <- df_psid %>% 
        mutate(unemp_hours_hd = ifelse(unemp_hd_hrs >= 40, yes = 1, no = 0),
               unemp_weeks_hd = ifelse(unemp_hd_wks >= 1 & unemp_hd_wks < 90, yes = 1, no = 0),
               unemp_lastyr_hd = ifelse(unemp_hours_hd == 1 & year <= 1993, yes = 1,
                                        ifelse(unemp_weeks_hd == 1 & year >= 1994, yes = 1, no = 0)))

# group_by(df_psid, year) %>% summarize(mean = mean(unemp_lastyr_hd)) %>% print(n=55)

df_psid <- df_psid %>% 
        select(-matches("unemp_hd"), -matches("unemp_wf"), -matches("unemp_hours"), -matches("unemp_weeks"))


# SELF EMPLOYMENT
df_psid <- df_psid %>% 
        mutate(selfemp_lastyr_hd = ifelse(self_emp_hd == 2 | self_emp_hd == 3, yes = 1, no = 0)) %>%
        select(-matches("self_emp_hd"), -matches("self_emp_wf"))


# EDUCATION  
df_psid <- df_psid %>% 
        mutate(edu_cat_ind = ifelse(edu_ind_yrs >= 0 & edu_ind_yrs < 12, 1, 
                                    ifelse(edu_ind_yrs == 12, 2, 
                                           ifelse(edu_ind_yrs >= 13 & edu_ind_yrs <= 17, 3, NA))),
               edu_cat_pre_1975 = ifelse(edu_hd_cat >= 0 & edu_hd_cat < 4, 1,
                                         ifelse(edu_hd_cat == 4 | edu_hd_cat == 5, 2, 
                                                ifelse(edu_hd_cat >= 6 & edu_hd_cat < 9, 3, NA))),
               education = edu_cat_ind,
               education = ifelse(year < 1975, edu_cat_pre_1975, education)) %>%
        select(-matches("edu_")) %>%
        filter(!is.na(education)) %>%
        mutate(education = as.factor(education))

# edu <- df_psid %>%
#         group_by(year) %>%
#         mutate(total = n()) %>%
#         group_by(year, education) %>%
#         mutate(n = n(), rel.freq = n / total) %>%
#         summarise(pct = mean(rel.freq)) %>%
#         ungroup() %>%
#         arrange(education, year)
# ggplot(data = edu, aes(x = year, y = pct, group = education, color = education)) +
#         geom_line(size = 2)

# edu_wgt_fam <- df_psid %>%
#         group_by(year) %>%
#         mutate(total = sum(weight_fam)) %>%
#         group_by(year, education) %>%
#         mutate(n = sum(weight_fam), rel.freq = n / total) %>%
#         summarise(pct = mean(rel.freq)) %>%
#         ungroup() %>%
#         arrange(education, year)
# ggplot(data = edu_wgt_fam, aes(x = year, y = pct, group = education, color = education)) +
#         geom_line(size = 2)

# MALE
df_psid <- df_psid %>% 
        mutate(male = ifelse(gender == 1, 1, 0)) %>% 
        select(-gender, -sex_hd)

# RACE
df_psid <- df_psid %>% 
        mutate(white = ifelse(race_hd == 1, 1, 0)) %>%
        select(-race_hd)

# INFLATION ADJUST
cpi_u_rs = c(4.929577465, 4.722638681, 4.585152838, 4.315068493, 3.922789539, 3.624856157, 3.427638738, 3.224155578, 3.017241379, 2.753496503, 2.478363493, 2.262931034, 2.134146341, 2.046783626, 1.966292135, 1.901025951, 1.867219917, 1.806192661, 1.742256637, 1.670201485, 1.590909091, 1.535836177, 1.4978602, 1.461716937, 1.431167651, 1.397515528, 1.36127917, 1.33248731, 1.28729056, 1.211538462, 1.166234728, 1.098709452, 1.034482759, 1, 0.953269614, 0.92040225)
year = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013)
inflation <- data.frame(cpi_u_rs, year)
df_psid <- merge(x = df_psid, y = inflation, by = "year", all = TRUE)
df_psid <- df_psid %>% 
        mutate(wages_hd_adj = wages_hd * cpi_u_rs)
df_psid <- df_psid %>% 
        arrange(pid, year)

# Save ----

saveRDS(df_psid, file = paste0(data_files,"psid_clean.rds"))


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

data = "data_files/"

# LIBRARY
library(dplyr)
library(broom)
library(beepr)

# LOAD DATA ----

x <- readRDS(file = paste0(data,"psid_clean.rds"))

# Study period ----

test <- c(seq(1970, 1987, 1))
test2 <- c(seq(1989, 2003, 2))
year <- as.vector(append(test, test2))

for(c in year) {
        z <- filter(x, year >= c & year <= c + 10)
        
        #keep if household head
        z <- filter(z, head == 1) %>% select(-head)
        
        #keep if employed or looking for work
        z <- filter(z, emp == 1) %>% select(-emp)
        
        z <- filter(z, age <= 64)
        
        #keep if in the sample each year in the study period
        z <- group_by(z, pid) %>%
                mutate(test = first(age)) %>%
                ungroup() %>%
                filter(test >= 25 & test <= 54) %>%
                select(-test)
        
        #keep if positive earnings
        z <- filter(z, tot_fam_income_adj_fz >= 100)
        
        #transform wages into log form (wages are total family income adjusted for family size)
        z <- mutate(z, wages_hd = tot_fam_income_adj_fz)
        z <- mutate(z, wages_hd_unadj = wages_hd)
        z <- mutate(z, wages_hd = log(wages_hd))
        
        #keep if in the sample each year in the study period
        z <- group_by(z, pid) %>%
                mutate(count = row_number(), count = last(count), study_period = min(year)) %>%
                ungroup() %>%
                mutate(max = max(count)) %>%
                filter(max == count) %>%
                select(-count)
        z <- arrange(z, pid, year)
        assign(paste0("yr_", c), z)
}
rm(x, z)        

#Append data sets ----

z = data.frame()
for(c in year) {
        y <- get(paste0("yr_", c))
        z <- rbind(z,y)
        rm(y)
}

for(c in year) {
        rm(list=paste0("yr_", c))
}

# DEMOGRAPHICS (education, race, age, cohort weight) ----

z <- arrange(z, study_period, pid, year)

#replace education with education in first year of the study period
#replace race with race in first year of the study period
#replace age_cat with age_cat in first year of the study period
##this variable is a bit messed up because age is not always sequential (pid == 840001)
##group_by(z, age_cat) %>% summarize(min = min(age), max = max(age))
#replace cohort group with cohort group in first year of the study period
#replace weight group with weight in first year of the study period
z <- group_by(z, study_period, pid) %>%
        mutate(education = first(education),
               white = first(white),
               age = first(age),
               age_cat = first(age_cat),
               cohgr = first(cohgr),
               weight_fam = first(weight_fam)) %>%
        ungroup()

#FAMILY ----
#single/married
z <- mutate(z, married =(ifelse(marital >= 2, yes = 0, no = 1)))

#always married == 3
#sometimes married == 2
#never married (i.e. always single) == 1
#always married
z <- group_by(z, study_period, pid) %>%
        mutate(test = sum(married), marr_chng = ifelse(test == max, yes = 3,
                                                       ifelse(test == 0, yes = 1, no = 2))) %>%
        ungroup()

#kids/no kids
z <- mutate(z, kids =(ifelse(kids>= 1, yes = 1, no = 0)))

#always kids == 3
#sometimes kids == 2
#never kids == 1
z <- group_by(z, study_period, pid) %>%
        mutate(test = sum(kids), kids_chng = ifelse(test == max, yes = 3, 
                                                    ifelse(test == 0, yes = 2, no = 1))) %>%
        ungroup()

# drops non-relevant family variables
z <- select(z, -kids, -married, -marital)

#WORK ----

#ever employed
z <- group_by(z, study_period, pid) %>%
        mutate(test = sum(hours_wf), emp_wf = ifelse(test == 0, yes = 0, no = 1)) %>%
        ungroup()

#ever unemployed
z <- group_by(z, study_period, pid) %>%
        mutate(test = sum(unemp_lastyr_hd), unemp_hd = ifelse(test == 0, yes = 0, no = 1)) %>%
        ungroup()

# group_by(z, year) %>% summarize(unemp_lastyr_hd=mean(unemp_lastyr_hd),unemp_hd=mean(unemp_hd)) %>% print(n=55)
# View(select(z, pid, study_period, year, unemp_hd, unemp_lastyr_hd) %>% arrange(pid, study_period, year))

#ever self employed
z <- group_by(z, study_period, pid) %>%
        mutate(test = sum(selfemp_lastyr_hd), self_hd = ifelse(test == 0, yes = 0, no = 1)) %>%
        ungroup()

# drops non-relevant work variables
z <- select(z, -selfemp_lastyr_hd, -unemp_lastyr_hd, -student, -retired, -unemp, -weeks_hd, -weeks_wf, -hours_hd, -hours_wf, -cpi_u_rs)

#VOLATILITY ----

#AGE-EARNINGS PROFILE
regression <- group_by(z, study_period) %>%
        do(augment(lm(wages_hd ~ factor(year), data=.), data = .))
z$uhat <- as.numeric(regression$.resid)

#YEAR-ADJUSTED TREND LINE
regression <- group_by(z, study_period, pid) %>%
        do(augment(lm(uhat ~ year, data = .), data = .))
z$yhat <- as.numeric(regression$.fitted)

#YEAR^2-ADJUSTED TREND LINE
regression <- group_by(z, study_period, pid) %>%
        do(augment(lm(uhat ~ poly(year, degree = 2, raw = TRUE), data=.), data = .))
z$yhat_2 <- as.numeric(regression$.fitted)

#MEAN
z <- group_by(z, study_period, pid) %>%
        mutate(mline = mean(uhat)) %>%
        ungroup()

#VOLATILITY
z <- group_by(z, study_period, pid) %>%
        mutate(std = sd(uhat), 
               volatility = sd(uhat - yhat), 
               volatility_2 = sd(uhat - yhat_2)) %>%
        ungroup()

#MOBILITY ----

#YEAR-ADJUSTED TREND LINE
z <- group_by(z, study_period, pid) %>%
        mutate(slope = last(yhat) - first(yhat)) %>%
        ungroup()

#YEAR^2-ADJUSTED TREND LINE
z <- group_by(z, study_period, pid) %>%
        mutate(slope_2 = last(yhat_2) - first(yhat_2)) %>%
        ungroup()

#INCOME AT START
z <- group_by(z, study_period, pid) %>%
        mutate(income_beg = (nth(uhat, 1) + nth(uhat, 2))/2,
               income_beg_unadj = (nth(wages_hd, 1) + nth(wages_hd, 2))/2) %>%
        ungroup()

#INCOME AT END
z <- group_by(z, study_period, pid) %>%
        mutate(income_end_1 = (last(uhat)), income_end_2 = lag(uhat, n = 1), income_end_2 = last(income_end_2), income_end = (income_end_1 + income_end_2)/2) %>%
        select(-income_end_1, -income_end_2) %>%
        ungroup()

#INCOME MOBILITY (UNADJUSTED)
z <- mutate(z, income_diff = (income_end - income_beg)*100)
z <- mutate(z, quartile_beg = ntile(income_beg, 4))
z <- mutate(z, quartile_end = ntile(income_end, 4))

vars <- c("slope", "slope_2", "std", "volatility", "volatility_2", "income_beg", "income_end")
for(i in vars){
        z[[i]] <- z[[i]]*100
}

# z <- select(z, -max, -yhat, -yhat_2, -mline)

#Make cross-sectional for cross-sectional analysis ----

z <- group_by(z, study_period, pid) %>%
        filter(row_number()==1) %>%
        ungroup()

#Save ----

saveRDS(z, file = paste0(data, "psid_clean_sample.rds"))
beep()


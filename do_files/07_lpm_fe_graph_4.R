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
tables = "tables/"
graphs = "graphs/"

# library
library(tidyverse)
library(beepr)
library(dummies) #dummy variables
library(robumeta) #group.center
library(stringr)
library(grid)
library(stargazer)

# no scientific notation
options(scipen=999)

# MANUAL FUNCTION
insertrow <- function(existingDF, newrow, r) {
        existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
        existingDF[r,] <- newrow
        existingDF
}

# load data ----

x <- readRDS(file = paste0(data,"psid_clean_sample.rds"))
x <- select(x, pid, year, volatility, slope, quartile_beg,
            education, male, white, age, marr_chng, kids_chng, unemp_hd, self_hd) # control variables


# clean data ----

x <- mutate(x, 
            ln_volatility = log(volatility),
            slp = ifelse(slope < -5, yes = 1, 
                         ifelse(slope > 5, yes = 3, no = 2)),
            t = ifelse(year < 1975, yes = 1,
                       ifelse(year >= 1975 & year < 1980, yes = 2,
                              ifelse(year >= 1980 & year < 1985, yes = 3,
                                     ifelse(year >= 1985 & year < 1990, yes = 4,
                                            ifelse(year >= 1990 & year < 1997, yes = 5,
                                                   ifelse(year >= 1997, yes = 6, no = 0))))))) %>%
        select(-slope)
with(x, table(year, t))

x <- rename(x, dv = volatility, inc = quartile_beg, edu = education, mar = marr_chng, kds = kids_chng, unmp = unemp_hd, slf = self_hd, ml = male, wh = white) %>%
        arrange(pid,t,year)

#recode dummy variables from (0/1) to (1/2)
dummies =          c("slf", "unmp", "ml", "wh")
for (i in dummies) {
        x[[i]] <- ifelse(x[[i]] == 1, yes = 2, no = 1)
}
rm(dummies)

# recode male and white to remain unchanged from first observation
x <- group_by(x, pid) %>%
        mutate(ml = first(ml), wh = first(wh)) %>%
        ungroup()

# recode education to remain unchanged within study period groupings
# x <- group_by(x, pid, t) %>%
#         mutate(edu = last(edu)) %>%
#         ungroup()

x <- mutate(x, older = ifelse(age > 49, yes = 2, no = 1))

x <- group_by(x, t) %>%
        mutate(pct80 = quantile(dv, probs = 0.90),
               high = as.integer(ifelse(dv > pct80, yes = 1, no = 0))) %>%
        ungroup()

# create a data frame for each set of categorical variables
vars =     c("slp", "inc", "wh", "ml", "edu", "older", "unmp", "slf", "mar", "kds")
z <- data.frame(matrix(NA, nrow = nrow(x), ncol = 0))
for (i in vars) {
        idx <- sort(unique(x[[i]]))
        dummy <- data.frame(matrix(NA, nrow = nrow(x), ncol = length(idx)))
        for (j in 1:length(idx)) { 
                dummy[j] <- as.integer(x[[i]] == idx[j])
        }
        for(n in 1:length(idx)){
                names(dummy)[names(dummy)==paste0("X",n)] <- paste(i, n, sep = "_")
        }
        z <- cbind(z, dummy)
        rm(dummy, i, idx, j, n)
}

# add in the data frame for each set of categorical variables
x <- cbind(x, z)
rm(z)

square <- select_(x, .dots = vars) %>%
        group_by_(.dots = vars) %>%
        filter(row_number() == 1) %>%
        arrange_(.dots = vars)
rm(square)

# tranform time into categorical dummy variables
time <- dummy(x = x$t, sep = "_")
time <- time[,-1]
x <- cbind(x, time)

# create interaction variables between time and categorical variables
vars <- c("slp_1", "slp_3", "inc_1", "inc_3", "inc_4", "wh_2", "ml_2", "edu_1", "edu_3", "older_2", "unmp_1", "slf_1", "mar_1", "mar_3", "kds_1", "kds_3")
time <- c("t_2", "t_3", "t_4", "t_5", "t_6")
for(v in vars){
        for(r in time){
                x$test <- x[[r]]*x[[v]]
                names(x)[names(x) == 'test'] <- paste("int",v,r,sep = "_")
        }
}
rm(v,r,vars,time)

# center variables
vars = c("t_2", "t_3", "t_4", "t_5", "t_6",
         "slp_1", "int_slp_1_t_2", "int_slp_1_t_3", "int_slp_1_t_4", "int_slp_1_t_5", "int_slp_1_t_6",
         "slp_3", "int_slp_3_t_2", "int_slp_3_t_3", "int_slp_3_t_4", "int_slp_3_t_5", "int_slp_3_t_6",
         "inc_1", "int_inc_1_t_2", "int_inc_1_t_3", "int_inc_1_t_4", "int_inc_1_t_5", "int_inc_1_t_6",
         "inc_3", "int_inc_3_t_2", "int_inc_3_t_3", "int_inc_3_t_4", "int_inc_3_t_5", "int_inc_3_t_6",
         "inc_4", "int_inc_4_t_2", "int_inc_4_t_3", "int_inc_4_t_4", "int_inc_4_t_5", "int_inc_4_t_6",
         "wh_2", "int_wh_2_t_2", "int_wh_2_t_3", "int_wh_2_t_4", "int_wh_2_t_5", "int_wh_2_t_6",
         "ml_2", "int_ml_2_t_2", "int_ml_2_t_3", "int_ml_2_t_4", "int_ml_2_t_5", "int_ml_2_t_6",
         "edu_1", "int_edu_1_t_2", "int_edu_1_t_3", "int_edu_1_t_4", "int_edu_1_t_5", "int_edu_1_t_6",
         "edu_3", "int_edu_3_t_2", "int_edu_3_t_3", "int_edu_3_t_4", "int_edu_3_t_5", "int_edu_3_t_6",
         "older_2", "int_older_2_t_2", "int_older_2_t_3", "int_older_2_t_4", "int_older_2_t_5", "int_older_2_t_6",
         "unmp_1", "int_unmp_1_t_2", "int_unmp_1_t_3", "int_unmp_1_t_4", "int_unmp_1_t_5", "int_unmp_1_t_6",
         "slf_1", "int_slf_1_t_2", "int_slf_1_t_3", "int_slf_1_t_4", "int_slf_1_t_5", "int_slf_1_t_6",
         "mar_1", "int_mar_1_t_2", "int_mar_1_t_3", "int_mar_1_t_4", "int_mar_1_t_5", "int_mar_1_t_6",
         "mar_3", "int_mar_3_t_2", "int_mar_3_t_3", "int_mar_3_t_4", "int_mar_3_t_5", "int_mar_3_t_6",
         "kds_1", "int_kds_1_t_2", "int_kds_1_t_3", "int_kds_1_t_4", "int_kds_1_t_5", "int_kds_1_t_6",
         "kds_3", "int_kds_3_t_2", "int_kds_3_t_3", "int_kds_3_t_4", "int_kds_3_t_5", "int_kds_3_t_6")

for (i in vars) {
        x$test <- group.center(x[[i]], x$pid)
        x$test <- as.numeric(x$test)
        names(x)[names(x) == "test"] <- paste0("c_",i)
}
rm(vars,i)

# lpm regression model ------------------------------------------------

model_lm <- lm(formula = high ~ 
                       c_slp_1 + c_int_slp_1_t_2 + c_int_slp_1_t_3 + c_int_slp_1_t_4 + c_int_slp_1_t_5 + c_int_slp_1_t_6
               + c_slp_3 + c_int_slp_3_t_2 + c_int_slp_3_t_3 + c_int_slp_3_t_4 + c_int_slp_3_t_5 + c_int_slp_3_t_6
               + c_inc_1 + c_int_inc_1_t_2 + c_int_inc_1_t_3 + c_int_inc_1_t_4 + c_int_inc_1_t_5 + c_int_inc_1_t_6
               + c_inc_3 + c_int_inc_3_t_2 + c_int_inc_3_t_3 + c_int_inc_3_t_4 + c_int_inc_3_t_5 + c_int_inc_3_t_6
               + c_inc_4 + c_int_inc_4_t_2 + c_int_inc_4_t_3 + c_int_inc_4_t_4 + c_int_inc_4_t_5 + c_int_inc_4_t_6
               + c_wh_2 + c_int_wh_2_t_2 + c_int_wh_2_t_3 + c_int_wh_2_t_4 + c_int_wh_2_t_5 + c_int_wh_2_t_6
               + c_ml_2 + c_int_ml_2_t_2 + c_int_ml_2_t_3 + c_int_ml_2_t_4 + c_int_ml_2_t_5 + c_int_ml_2_t_6
               + c_older_2 + c_int_older_2_t_2 + c_int_older_2_t_3 + c_int_older_2_t_4 + c_int_older_2_t_5 + c_int_older_2_t_6
               + c_edu_1 + c_int_edu_1_t_2 + c_int_edu_1_t_3 + c_int_edu_1_t_4 + c_int_edu_1_t_5 + c_int_edu_1_t_6
               + c_edu_3 + c_int_edu_3_t_2 + c_int_edu_3_t_3 + c_int_edu_3_t_4 + c_int_edu_3_t_5 + c_int_edu_3_t_6
               + c_unmp_1 + c_int_unmp_1_t_2 + c_int_unmp_1_t_3 + c_int_unmp_1_t_4 + c_int_unmp_1_t_5 + c_int_unmp_1_t_6
               + c_slf_1 + c_int_slf_1_t_2 + c_int_slf_1_t_3 + c_int_slf_1_t_4 + c_int_slf_1_t_5 + c_int_slf_1_t_6
               + c_mar_1 + c_int_mar_1_t_2 + c_int_mar_1_t_3 + c_int_mar_1_t_4 + c_int_mar_1_t_5 + c_int_mar_1_t_6
               + c_mar_3 + c_int_mar_3_t_2 + c_int_mar_3_t_3 + c_int_mar_3_t_4 + c_int_mar_3_t_5 + c_int_mar_3_t_6
               + c_kds_1 + c_int_kds_1_t_2 + c_int_kds_1_t_3 + c_int_kds_1_t_4 + c_int_kds_1_t_5 + c_int_kds_1_t_6
               + c_kds_3 + c_int_kds_3_t_2 + c_int_kds_3_t_3 + c_int_kds_3_t_4 + c_int_kds_3_t_5 + c_int_kds_3_t_6
               + c_t_2 + c_t_3 + c_t_4 + c_t_5 + c_t_6, 
               data = x) # OLS reg
summary(model_lm)

# table of results ----

# VARIABLE LABLES
slp_dn_1 <- paste0("\\emph{Income mobility:} & \\\\
                   Downward mobility ($\\Delta \\hat{y}_{pi} < -5$)")
slp_dn_2 <- paste0("Downward mobility x 1975 $-$ 1979")
slp_dn_3 <- paste0("Downward mobility x 1980 $-$ 1984")
slp_dn_4 <- paste0("Downward mobility x 1985 $-$ 1989")
slp_dn_5 <- paste0("Downward mobility x 1990 $-$ 1996")
slp_dn_6 <- paste0("Downward mobility x 1997 $-$ 2003")

slp_up_1 <- paste0("Upward mobility ($\\Delta \\hat{y}_{pi} > 5)$")
slp_up_2 <- paste0("Upward mobility x 1975 $-$ 1979")
slp_up_3 <- paste0("Upward mobility x 1980 $-$ 1984")
slp_up_4 <- paste0("Upward mobility x 1985 $-$ 1989")
slp_up_5 <- paste0("Upward mobility x 1990 $-$ 1996")
slp_up_6 <- paste0("Upward mobility x 1997 $-$ 2003")

inc_1_1 <- paste0("& \\\\ 
                  \\emph{Income quartile at start:} & \\\\
                  Income quartile 1")
inc_1_2 <- paste0("Income quartile 1 x 1975 $-$ 1979")
inc_1_3 <- paste0("Income quartile 1 x 1980 $-$ 1984")
inc_1_4 <- paste0("Income quartile 1 x 1985 $-$ 1989")
inc_1_5 <- paste0("Income quartile 1 x 1990 $-$ 1996")
inc_1_6 <- paste0("Income quartile 1 x 1997 $-$ 2003")

inc_3_1 <- paste0("Income quartile 3")
inc_3_2 <- paste0("Income quartile 3 x 1975 $-$ 1979")
inc_3_3 <- paste0("Income quartile 3 x 1980 $-$ 1984")
inc_3_4 <- paste0("Income quartile 3 x 1985 $-$ 1989")
inc_3_5 <- paste0("Income quartile 3 x 1990 $-$ 1996")
inc_3_6 <- paste0("Income quartile 3 x 1997 $-$ 2003")

inc_4_1 <- paste0("Income quartile 4")
inc_4_2 <- paste0("Income quartile 4 x 1975 $-$ 1979")
inc_4_3 <- paste0("Income quartile 4 x 1980 $-$ 1984")
inc_4_4 <- paste0("Income quartile 4 x 1985 $-$ 1989")
inc_4_5 <- paste0("Income quartile 4 x 1990 $-$ 1996")
inc_4_6 <- paste0("Income quartile 4 x 1997 $-$ 2003")

wh_1 <- paste0("& \\\\ 
               \\emph{Demographic characteristics:} & \\\\
               White")
wh_2 <- paste0("White x 1975 $-$ 1979")
wh_3 <- paste0("White x 1980 $-$ 1984")
wh_4 <- paste0("White x 1985 $-$ 1989")
wh_5 <- paste0("White x 1990 $-$ 1996")
wh_6 <- paste0("White x 1997 $-$ 2003")

ml_1 <- paste0("Male")
ml_2 <- paste0("Male x 1975 $-$ 1979")
ml_3 <- paste0("Male x 1980 $-$ 1984")
ml_4 <- paste0("Male x 1985 $-$ 1989")
ml_5 <- paste0("Male x 1990 $-$ 1996")
ml_6 <- paste0("Male x 1997 $-$ 2003")

age_1 <- paste0("Older (Age $>$ 49)")
age_2 <- paste0("Older x 1975 $-$ 1979")
age_3 <- paste0("Older x 1980 $-$ 1984")
age_4 <- paste0("Older x 1985 $-$ 1989")
age_5 <- paste0("Older x 1990 $-$ 1996")
age_6 <- paste0("Older x 1997 $-$ 2003")

edu_1_1 <- paste0("\\emph{Education:} & \\\\
                  Less than HS")
edu_1_2 <- paste0("Less than HS x 1975 $-$ 1979")
edu_1_3 <- paste0("Less than HS x 1980 $-$ 1984")
edu_1_4 <- paste0("Less than HS x 1985 $-$ 1989")
edu_1_5 <- paste0("Less than HS x 1990 $-$ 1996")
edu_1_6 <- paste0("Less than HS x 1997 $-$ 2003")

edu_3_1 <- paste0("More than HS")
edu_3_2 <- paste0("More than HS x 1975 $-$ 1979")
edu_3_3 <- paste0("More than HS x 1980 $-$ 1984")
edu_3_4 <- paste0("More than HS x 1985 $-$ 1989")
edu_3_5 <- paste0("More than HS x 1990 $-$ 1996")
edu_3_6 <- paste0("More than HS x 1997 $-$ 2003")

unmp_1 <- paste0("& \\\\ 
                 \\emph{Employment characteristics:} & \\\\
                 Never unemployed")
unmp_2 <- paste0("Never unemployed x 1975 $-$ 1979")
unmp_3 <- paste0("Never unemployed x 1980 $-$ 1984")
unmp_4 <- paste0("Never unemployed x 1985 $-$ 1989")
unmp_5 <- paste0("Never unemployed x 1990 $-$ 1996")
unmp_6 <- paste0("Never unemployed x 1997 $-$ 2003")

slf_1 <- paste0("Never self-unemployed")
slf_2 <- paste0("Never self-unemployed x 1975 $-$ 1979")
slf_3 <- paste0("Never self-unemployed x 1980 $-$ 1984")
slf_4 <- paste0("Never self-unemployed x 1985 $-$ 1989")
slf_5 <- paste0("Never self-unemployed x 1990 $-$ 1996")
slf_6 <- paste0("Never self-unemployed x 1997 $-$ 2003")

mar_1_1 <- paste0("& \\\\ 
                  \\emph{Family characteristics:} & \\\\
                  Always single")
mar_1_2 <- paste0("Always single x 1975 $-$ 1979")
mar_1_3 <- paste0("Always single x 1980 $-$ 1984")
mar_1_4 <- paste0("Always single x 1985 $-$ 1989")
mar_1_5 <- paste0("Always single x 1990 $-$ 1996")
mar_1_6 <- paste0("Always single x 1997 $-$ 2003")

mar_3_1 <- paste0("Always married")
mar_3_2 <- paste0("Always married x 1975 $-$ 1979")
mar_3_3 <- paste0("Always married x 1980 $-$ 1984")
mar_3_4 <- paste0("Always married x 1985 $-$ 1989")
mar_3_5 <- paste0("Always married x 1990 $-$ 1996")
mar_3_6 <- paste0("Always married x 1997 $-$ 2003")

kds_1_1 <- paste0("Never kids")
kds_1_2 <- paste0("Never kids x 1975 $-$ 1979")
kds_1_3 <- paste0("Never kids x 1980 $-$ 1984")
kds_1_4 <- paste0("Never kids x 1985 $-$ 1989")
kds_1_5 <- paste0("Never kids x 1990 $-$ 1996")
kds_1_6 <- paste0("Never kids x 1997 $-$ 2003")

kds_3_1 <- paste0("Always kids")
kds_3_2 <- paste0("Always kids x 1975 $-$ 1979")
kds_3_3 <- paste0("Always kids x 1980 $-$ 1984")
kds_3_4 <- paste0("Always kids x 1985 $-$ 1989")
kds_3_5 <- paste0("Always kids x 1990 $-$ 1996")
kds_3_6 <- paste0("Always kids x 1997 $-$ 2003")

study_period_2 <- paste0("& \\\\ 
                         \\emph{Study period beginning:} & \\\\
                         1975 $-$ 1979")
study_period_3 <- paste0("1980 $-$ 1984")
study_period_4 <- paste0("1985 $-$ 1989")
study_period_5 <- paste0("1990 $-$ 1996")
study_period_6 <- paste0("1997 $-$ 2003")

intercept <- paste0("& \\\\ 
                    Constant")

t <- stargazer(model_lm, 
               out = paste0(tables,"lpm_fe_hh_period_part_1_90.tex"), 
               model.names = FALSE,
               model.numbers = FALSE,
               dep.var.caption = "",
               dep.var.labels.include = FALSE,
               keep = c("c_slp_1","c_int_slp_1_t_2","c_int_slp_1_t_3","c_int_slp_1_t_4","c_int_slp_1_t_5","c_int_slp_1_t_6","c_slp_3","c_int_slp_3_t_2","c_int_slp_3_t_3","c_int_slp_3_t_4","c_int_slp_3_t_5","c_int_slp_3_t_6","c_inc_1","c_int_inc_1_t_2","c_int_inc_1_t_3","c_int_inc_1_t_4","c_int_inc_1_t_5","c_int_inc_1_t_6","c_inc_3","c_int_inc_3_t_2","c_int_inc_3_t_3","c_int_inc_3_t_4","c_int_inc_3_t_5","c_int_inc_3_t_6","c_inc_4","c_int_inc_4_t_2","c_int_inc_4_t_3","c_int_inc_4_t_4","c_int_inc_4_t_5","c_int_inc_4_t_6","c_wh_2","c_int_wh_2_t_2","c_int_wh_2_t_3","c_int_wh_2_t_4","c_int_wh_2_t_5","c_int_wh_2_t_6","c_ml_2","c_int_ml_2_t_2","c_int_ml_2_t_3","c_int_ml_2_t_4","c_int_ml_2_t_5","c_int_ml_2_t_6","c_older_2","c_int_older_2_t_2","c_int_older_2_t_3","c_int_older_2_t_4","c_int_older_2_t_5","c_int_older_2_t_6"),
               covariate.labels = c(slp_dn_1,slp_dn_2,slp_dn_3,slp_dn_4,slp_dn_5,slp_dn_6,
                                    slp_up_1,slp_up_2,slp_up_3,slp_up_4,slp_up_5,slp_up_6,
                                    inc_1_1,inc_1_2,inc_1_3,inc_1_4,inc_1_5,inc_1_6,
                                    inc_3_1,inc_3_2,inc_3_3,inc_3_4,inc_3_5,inc_3_6,
                                    inc_4_1,inc_4_2,inc_4_3,inc_4_4,inc_4_5,inc_4_6,
                                    wh_1,wh_2,wh_3,wh_4,wh_5,wh_6,
                                    ml_1,ml_2,ml_3,ml_4,ml_5,ml_6,
                                    age_1,age_2,age_3,age_4,age_5,age_6
               ),
               omit.table.layout = "s",
               notes.align = "l",
               notes = c("\\emph{continued...}"),
               notes.append = FALSE,
               header = FALSE,
               float = FALSE,
               align = TRUE, digits = 3, digits.extra = 0,
               star.cutoffs = NA,
               single.row = TRUE
)

Tables <- as.data.frame(t)
Tables$t <- as.character(Tables$t)
#Categories
header <- "\\hline \\\\ [-1.8ex] \\multicolumn{1}{l}{Variables} & \\multicolumn{1}{c}{$\\beta$} \\\\"

# Find where you want to put in the user specific rows.  In our case, this is right after the last fixed effect.
r <- 4
Tables <- insertrow(Tables, header, r)

write.table(Tables,file=paste0(tables,"lpm_fe_hh_period_part_1_90.tex"),
            row.names= FALSE, 
            na="", 
            quote = FALSE, 
            col.names = FALSE)


t <- stargazer(model_lm, 
               out = paste0(tables,"lpm_fe_hh_period_part_2_90.tex"), 
               model.names = FALSE,
               model.numbers = FALSE,
               dep.var.caption = "",
               dep.var.labels.include = FALSE,
               keep = c("c_edu_1","c_int_edu_1_t_2","c_int_edu_1_t_3","c_int_edu_1_t_4","c_int_edu_1_t_5","c_int_edu_1_t_6","c_edu_3","c_int_edu_3_t_2","c_int_edu_3_t_3","c_int_edu_3_t_4","c_int_edu_3_t_5","c_int_edu_3_t_6","c_unmp_1","c_int_unmp_1_t_2","c_int_unmp_1_t_3","c_int_unmp_1_t_4","c_int_unmp_1_t_5","c_int_unmp_1_t_6","c_slf_1","c_int_slf_1_t_2","c_int_slf_1_t_3","c_int_slf_1_t_4","c_int_slf_1_t_5","c_int_slf_1_t_6","c_mar_1","c_int_mar_1_t_2","c_int_mar_1_t_3","c_int_mar_1_t_4","c_int_mar_1_t_5","c_int_mar_1_t_6","c_mar_3","c_int_mar_3_t_2","c_int_mar_3_t_3","c_int_mar_3_t_4","c_int_mar_3_t_5","c_int_mar_3_t_6","c_kds_1","c_int_kds_1_t_2","c_int_kds_1_t_3","c_int_kds_1_t_4","c_int_kds_1_t_5","c_int_kds_1_t_6","c_kds_3","c_int_kds_3_t_2","c_int_kds_3_t_3","c_int_kds_3_t_4","c_int_kds_3_t_5","c_int_kds_3_t_6","c_t_2","c_t_3","c_t_4","c_t_5","c_t_6","Constant"),
               covariate.labels = c(edu_1_1,edu_1_2,edu_1_3,edu_1_4,edu_1_5,edu_1_6,
                                    edu_3_1,edu_3_2,edu_3_3,edu_3_4,edu_3_5,edu_3_6,
                                    unmp_1,unmp_2,unmp_3,unmp_4,unmp_5,unmp_6,
                                    slf_1,slf_2,slf_3,slf_4,slf_5,slf_6,
                                    mar_1_1,mar_1_2,mar_1_3,mar_1_4,mar_1_5,mar_1_6,
                                    mar_3_1,mar_3_2,mar_3_3,mar_3_4,mar_3_5,mar_3_6,
                                    kds_1_1,kds_1_2,kds_1_3,kds_1_4,kds_1_5,kds_1_6,
                                    kds_3_1,kds_3_2,kds_3_3,kds_3_4,kds_3_5,kds_3_6,
                                    study_period_2,study_period_3,study_period_4,study_period_5,study_period_6,
                                    intercept
               ),
               notes.align = "l",
               notes = c("Standard errors in parenthesis."),
               notes.append = FALSE,
               header = FALSE,
               float = FALSE,
               align = TRUE, digits = 3, digits.extra = 0,
               keep.stat = c("n", "rsq"),
               star.cutoffs = NA,
               single.row = TRUE
)

Tables <- as.data.frame(t)
Tables$t <- as.character(Tables$t)
#Categories
header <- "\\hline \\\\ [-1.8ex] \\multicolumn{1}{l}{Variables \\emph{(continued)}} & \\multicolumn{1}{c}{$\\beta$} \\\\"

# Find where you want to put in the user specific rows.  In our case, this is right after the last fixed effect.
r <- 4
Tables <- insertrow(Tables, header, r)

write.table(Tables,file=paste0(tables,"lpm_fe_hh_period_part_2_90.tex"),
            row.names= FALSE, 
            na="", 
            quote = FALSE, 
            col.names = FALSE)

rm(slp_dn_1,slp_dn_2,slp_dn_3,slp_dn_4,slp_dn_5,slp_dn_6,slp_up_1,slp_up_2,slp_up_3,slp_up_4,slp_up_5,slp_up_6,inc_1_1,inc_1_2,inc_1_3,inc_1_4,inc_1_5,inc_1_6,inc_3_1,inc_3_2,inc_3_3,inc_3_4,inc_3_5,inc_3_6,inc_4_1,inc_4_2,inc_4_3,inc_4_4,inc_4_5,inc_4_6,wh_1,wh_2,wh_3,wh_4,wh_5,wh_6,ml_1,ml_2,ml_3,ml_4,ml_5,ml_6,age_1,age_2,age_3,age_4,age_5,age_6,edu_1_1,edu_1_2,edu_1_3,edu_1_4,edu_1_5,edu_1_6,edu_3_1,edu_3_2,edu_3_3,edu_3_4,edu_3_5,edu_3_6,unmp_1,unmp_2,unmp_3,unmp_4,unmp_5,unmp_6,slf_1,slf_2,slf_3,slf_4,slf_5,slf_6,mar_1_1,mar_1_2,mar_1_3,mar_1_4,mar_1_5,mar_1_6,mar_3_1,mar_3_2,mar_3_3,mar_3_4,mar_3_5,mar_3_6,kds_1_1,kds_1_2,kds_1_3,kds_1_4,kds_1_5,kds_1_6,kds_3_1,kds_3_2,kds_3_3,kds_3_4,kds_3_5,kds_3_6,study_period_2,study_period_3,study_period_4,study_period_5,study_period_6,intercept,r,t,Tables,header)

# prediction ----
# downward mobility ----
phat_slp_1_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=.95,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# downward mobility - t_2
phat_slp_1_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=.95,c_int_slp_1_t_2=.95,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# downward mobility - t_3
phat_slp_1_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=.95,c_int_slp_1_t_2=0,c_int_slp_1_t_3=.95,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# downward mobility - t_4
phat_slp_1_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=.95,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=.95,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# downward mobility - t_5
phat_slp_1_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=.95,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=.95,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# downward mobility - t_6
phat_slp_1_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=.95,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=.95,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# upward mobility ----
phat_slp_3_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=.95,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# upward mobility - t_2
phat_slp_3_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=.95,c_int_slp_3_t_2=.95,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))
# upward mobility - t_3
phat_slp_3_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=.95,c_int_slp_3_t_2=0,c_int_slp_3_t_3=.95,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))
# upward mobility - t_4
phat_slp_3_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=.95,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=.95,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# upward mobility - t_5
phat_slp_3_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=.95,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=.95,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# upward mobility - t_6
phat_slp_3_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=.95,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=.95,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 1 ----
phat_inc_1_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=.95,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 1 - t_2
phat_inc_1_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=.95,c_int_inc_1_t_2=.95,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 1 - t_3
phat_inc_1_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=.95,c_int_inc_1_t_2=0,c_int_inc_1_t_3=.95,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 1 - t_4
phat_inc_1_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=.95,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=.95,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 1 - t_5
phat_inc_1_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=.95,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=.95,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 1 - t_6
phat_inc_1_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=.95,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=.95,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))
# income quartile 3 ----
phat_inc_3_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=.95,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 3 - t_2
phat_inc_3_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=.95,c_int_inc_3_t_2=.95,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 3 - t_3
phat_inc_3_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=.95,c_int_inc_3_t_2=0,c_int_inc_3_t_3=.95,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 3 - t_4
phat_inc_3_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=.95,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=.95,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 3 - t_5
phat_inc_3_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=.95,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=.95,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))
# income quartile 3 - t_6
phat_inc_3_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=.95,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=.95,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 4 ----
phat_inc_4_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=.95,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 4 - t_2
phat_inc_4_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=.95,c_int_inc_4_t_2=.95,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 4 - t_3
phat_inc_4_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=.95,c_int_inc_4_t_2=0,c_int_inc_4_t_3=.95,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 4 - t_4
phat_inc_4_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=.95,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=.95,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 4 - t_5
phat_inc_4_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=.95,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=.95,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# income quartile 4 - t_6
phat_inc_4_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=.95,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=.95,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# white ----
phat_wh_2_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=.95,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# white - t_2
phat_wh_2_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=.95,c_int_wh_2_t_2=.95,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# white - t_3
phat_wh_2_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=.95,c_int_wh_2_t_2=0,c_int_wh_2_t_3=.95,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# white - t_4
phat_wh_2_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=.95,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=.95,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# white - t_5
phat_wh_2_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=.95,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=.95,c_int_wh_2_t_6=0,
                                    c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# white - t_6
phat_wh_2_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=.95,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=.95,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# male ----
phat_ml_2_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=.95,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# male - t_2
phat_ml_2_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=.95,c_int_ml_2_t_2=.95,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# male - t_3
phat_ml_2_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=.95,c_int_ml_2_t_2=0,c_int_ml_2_t_3=.95,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# male - t_4
phat_ml_2_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=.95,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=.95,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# male - t_5
phat_ml_2_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=.95,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=.95,c_int_ml_2_t_6=0,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# ml_2_t_6
phat_ml_2_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                            newdata = data.frame(
                                    c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                    c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                    c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                    c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                    c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                    c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                    c_ml_2=.95,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=.95,
                                    c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                    c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                    c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                    c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                    c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                    c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                    c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                    c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                    c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                    c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                            ))

# older ----
phat_older_2_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                               newdata = data.frame(
                                       c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                       c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                       c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                       c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                       c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                       c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                       c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                       c_older_2=.95,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                       c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                       c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                       c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                       c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                       c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                       c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                       c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                       c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                       c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                               ))

# older_2_t_2
phat_older_2_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                               newdata = data.frame(
                                       c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                       c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                       c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                       c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                       c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                       c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                       c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                       c_older_2=.95,c_int_older_2_t_2=.95,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                       c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                       c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                       c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                       c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                       c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                       c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                       c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                       c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                       c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                               ))

# older_2_t_3
phat_older_2_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                               newdata = data.frame(
                                       c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                       c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                       c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                       c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                       c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                       c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                       c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                       c_older_2=.95,c_int_older_2_t_2=0,c_int_older_2_t_3=.95,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                       c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                       c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                       c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                       c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                       c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                       c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                       c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                       c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                       c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                               ))

# older_2_t_4
phat_older_2_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                               newdata = data.frame(
                                       c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                       c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                       c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                       c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                       c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                       c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                       c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                       c_older_2=.95,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=.95,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                       c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                       c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                       c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                       c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                       c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                       c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                       c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                       c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                       c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                               ))

# older_2_t_5
phat_older_2_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                               newdata = data.frame(
                                       c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                       c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                       c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                       c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                       c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                       c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                       c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                       c_older_2=.95,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=.95,c_int_older_2_t_6=0,
                                       c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                       c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                       c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                       c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                       c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                       c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                       c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                       c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                       c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                               ))

# older_2_t_6
phat_older_2_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                               newdata = data.frame(
                                       c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                       c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                       c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                       c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                       c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                       c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                       c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                       c_older_2=.95,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=.95,
                                       c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                       c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                       c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                       c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                       c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                       c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                       c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                       c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                       c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                               ))

# education (LHS) ----
phat_edu_1_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=.95,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# edu_1_t_2
phat_edu_1_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=.95,c_int_edu_1_t_2=.95,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# edu_1_t_3
phat_edu_1_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=.95,c_int_edu_1_t_2=0,c_int_edu_1_t_3=.95,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# edu_1_t_4
phat_edu_1_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=.95,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# edu_1_t_5
phat_edu_1_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=.95,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=.95,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# edu_1_t_6
phat_edu_1_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=.95,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=.95,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# education (MHS) ----
phat_edu_3_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=.95,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# edu_3_t_2
phat_edu_3_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=.95,c_int_edu_3_t_2=.95,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# edu_3_t_3
phat_edu_3_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=.95,c_int_edu_3_t_2=0,c_int_edu_3_t_3=.95,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# edu_3_t_4
phat_edu_3_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=.95,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=.95,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# edu_3_t_5
phat_edu_3_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=.95,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=.95,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# edu_3_t_6
phat_edu_3_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=.95,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=.95,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# never unemployed ----
phat_unmp_1_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

# unmp_1_t_2
phat_unmp_1_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=.95,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

# unmp_1_t_3
phat_unmp_1_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=.95,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

# unmp_1_t_4
phat_unmp_1_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=.95,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

# unmp_1_t_5
phat_unmp_1_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=.95,c_int_unmp_1_t_6=0,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

# unmp_1_t_6
phat_unmp_1_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=.95,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

# never self-employed ----
phat_slf_1_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=.95,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# slf_1_t_2
phat_slf_1_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=.95,c_int_slf_1_t_2=.95,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# slf_1_t_3
phat_slf_1_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=.95,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=.95,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# slf_1_t_4
phat_slf_1_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=.95,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=.95,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# slf_1_t_5
phat_slf_1_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=.95,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=.95,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# slf_1_t_6
phat_slf_1_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=.95,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=.95,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# always single ----
phat_mar_1_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=.95,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# mar_1_t_2
phat_mar_1_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=.95,c_int_mar_1_t_2=.95,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# mar_1_t_3
phat_mar_1_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=.95,c_int_mar_1_t_2=0,c_int_mar_1_t_3=.95,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# mar_1_t_4
phat_mar_1_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=.95,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=.95,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# mar_1_t_5
phat_mar_1_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=.95,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=.95,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# mar_1_t_6
phat_mar_1_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=.95,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=.95,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# always married ----
phat_mar_3_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=.95,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# mar_3_t_2
phat_mar_3_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=.95,c_int_mar_3_t_2=.95,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# mar_3_t_3
phat_mar_3_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=.95,c_int_mar_3_t_2=0,c_int_mar_3_t_3=.95,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# mar_3_t_4
phat_mar_3_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=.95,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=.95,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# mar_3_t_5
phat_mar_3_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=.95,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=.95,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# mar_3_t_6
phat_mar_3_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=.95,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=.95,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# never children ----
phat_kds_1_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=.95,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# kds_1_t_2
phat_kds_1_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=.95,c_int_kds_1_t_2=.95,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# kds_1_t_3
phat_kds_1_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=.95,c_int_kds_1_t_2=0,c_int_kds_1_t_3=.95,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# kds_1_t_4
phat_kds_1_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=.95,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=.95,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# kds_1_t_5
phat_kds_1_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=.95,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=.95,c_int_kds_1_t_6=0,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# kds_1_t_6
phat_kds_1_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=.95,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=.95,
                                     c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# always children ----
phat_kds_3_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=.95,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# kds_3_t_2
phat_kds_3_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=.95,c_int_kds_3_t_2=.95,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# kds_3_t_3
phat_kds_3_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=.95,c_int_kds_3_t_2=0,c_int_kds_3_t_3=.95,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# kds_3_t_4
phat_kds_3_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=.95,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=.95,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# kds_3_t_5
phat_kds_3_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=.95,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=.95,c_int_kds_3_t_6=0,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))

# kds_3_t_6
phat_kds_3_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                             newdata = data.frame(
                                     c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                     c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                     c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                     c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                     c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                     c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                     c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                     c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                     c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                     c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                     c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                     c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                     c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                     c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                     c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                     c_kds_3=.95,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=.95,
                                     c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                             ))


# combine predicted fit and se.fit into a single data frame ------------------------------------------------

df_phat <- data.frame(rbind(
        phat_slp_1_t_1,phat_slp_1_t_2,phat_slp_1_t_3,phat_slp_1_t_4,phat_slp_1_t_5,phat_slp_1_t_6,
        phat_slp_3_t_1,phat_slp_3_t_2,phat_slp_3_t_3,phat_slp_3_t_4,phat_slp_3_t_5,phat_slp_3_t_6,
        phat_inc_1_t_1,phat_inc_1_t_2,phat_inc_1_t_3,phat_inc_1_t_4,phat_inc_1_t_5,phat_inc_1_t_6,
        phat_inc_3_t_1,phat_inc_3_t_2,phat_inc_3_t_3,phat_inc_3_t_4,phat_inc_3_t_5,phat_inc_3_t_6,
        phat_inc_4_t_1,phat_inc_4_t_2,phat_inc_4_t_3,phat_inc_4_t_4,phat_inc_4_t_5,phat_inc_4_t_6,
        phat_wh_2_t_1,phat_wh_2_t_2,phat_wh_2_t_3,phat_wh_2_t_4,phat_wh_2_t_5,phat_wh_2_t_6,
        phat_ml_2_t_1,phat_ml_2_t_2,phat_ml_2_t_3,phat_ml_2_t_4,phat_ml_2_t_5,phat_ml_2_t_6,
        phat_edu_1_t_1,phat_edu_1_t_2,phat_edu_1_t_3,phat_edu_1_t_4,phat_edu_1_t_5,phat_edu_1_t_6,
        phat_edu_3_t_1,phat_edu_3_t_2,phat_edu_3_t_3,phat_edu_3_t_4,phat_edu_3_t_5,phat_edu_3_t_6,
        phat_older_2_t_1,phat_older_2_t_2,phat_older_2_t_3,phat_older_2_t_4,phat_older_2_t_5,phat_older_2_t_6,
        phat_unmp_1_t_1,phat_unmp_1_t_2,phat_unmp_1_t_3,phat_unmp_1_t_4,phat_unmp_1_t_5,phat_unmp_1_t_6,
        phat_slf_1_t_1,phat_slf_1_t_2,phat_slf_1_t_3,phat_slf_1_t_4,phat_slf_1_t_5,phat_slf_1_t_6,
        phat_mar_1_t_1,phat_mar_1_t_2,phat_mar_1_t_3,phat_mar_1_t_4,phat_mar_1_t_5,phat_mar_1_t_6,
        phat_mar_3_t_1,phat_mar_3_t_2,phat_mar_3_t_3,phat_mar_3_t_4,phat_mar_3_t_5,phat_mar_3_t_6,
        phat_kds_1_t_1,phat_kds_1_t_2,phat_kds_1_t_3,phat_kds_1_t_4,phat_kds_1_t_5,phat_kds_1_t_6,
        phat_kds_3_t_1,phat_kds_3_t_2,phat_kds_3_t_3,phat_kds_3_t_4,phat_kds_3_t_5,phat_kds_3_t_6))

df_phat$names <- rownames(df_phat)
rownames(df_phat) <- seq.int(nrow(df_phat))
df_phat$test <- str_split_fixed(df_phat$names, "_", 2)[,2]
df_phat$variable <- str_split_fixed(df_phat$test, "_t_", 2)[,1]
df_phat$time <- str_split_fixed(df_phat$test, "_t_", 2)[,2]
df_phat$category <- str_split_fixed(df_phat$variable, "_", 2)[,1]
df_phat <- select(df_phat, fit, se.fit, category, variable, time)
df_phat$fit <- as.numeric(df_phat$fit)
df_phat$se.fit <- as.numeric(df_phat$se.fit)

df_phat <- mutate(df_phat,
                  lb = fit - qnorm(.975)*se.fit,
                  ub = fit + qnorm(.975)*se.fit)

df_phat$category <- factor(df_phat$category,
                           levels = c("slp", "inc", "wh", "ml", "edu", "older", "unmp", "slf", "mar", "kds"),
                           labels = c("Mobility", "Income", "White", "Male", "Education", "Older (Age>49)", "Unemp", "Self-emp", "Always Married", "Kids"))

df_phat$variable <- factor(df_phat$variable, 
                           levels = c("slp_1", "slp_3", "inc_1", "inc_3", "inc_4", "wh_2", "ml_2", "edu_1", "edu_3", "older_2", "unmp_1", "slf_1", "mar_1", "mar_3", "kds_1", "kds_3"),
                           labels = c("Mobility Dn", "Mobility Up", "Inc. Quartile 1", "Inc. Quartile 3", "Inc. Quartile 4", "White", "Male", "<HS", ">HS", "Older (Age>49)", "Never unemployed", "Never Self-employed", "Always Single", "Always Married", "Never Kids", "Always Kids"))

df_phat <- arrange(df_phat, desc(category), desc(variable), desc(time))
df_phat$number <- as.factor(seq.int(nrow(df_phat)))

y <- df_phat

# prepare one single big bar graph ------------------------------------------------
# http://stackoverflow.com/questions/23863345/set-ggplot-plots-to-have-same-x-axis-width-and-same-space-between-dot-plot-rows
# http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot
# http://stackoverflow.com/questions/31572239/set-space-in-facet-wrap-like-in-facet-grid

#mobility characteristics
mob <- filter(y, category == "Mobility") %>% arrange(variable, desc(time))
mob$category <- factor(mob$category)
mob$variable <- factor(mob$variable)
mob$number <- factor(mob$number)

#income characteristics
inc <- filter(y, category == "Income") %>% arrange(variable, desc(time))
inc$category <- factor(inc$category)
inc$variable <- factor(inc$variable)
inc$number <- factor(inc$number)

#employment characteristics
emp <- filter(y, category == "Unemp" | category == "Self-emp") %>% arrange(variable, desc(time))
emp$category <- factor(emp$category)
emp$variable <- factor(emp$variable)
emp$number <- factor(emp$number)

#personal characteristics
demographic <- filter(y, category == "White" | category == "Male" | category == "Older (Age>49)" | category == "Education") %>% arrange(variable, desc(time))
demographic$category <- factor(demographic$category)
demographic$variable <- factor(demographic$variable)
demographic$number <- factor(demographic$number)

#family characteristics
family <- filter(y, category == "Always Married" | category == "Kids") %>% arrange(variable, desc(time))
family$category <- factor(family$category)
family$variable <- factor(family$variable)
family$number <- factor(family$number)

#combine characteristics
mob$Plot <- "Mobility characteristics"
inc$Plot <- "Income characteristics"
emp$Plot <- "Employment characteristics"
demographic$Plot <- "Demographic characteristics"
family$Plot <- "Family characteristics"
dataset <- rbind(mob, inc, emp, demographic, family)
rm(mob, inc, emp, demographic, family)

dataset$Plot <- factor(dataset$Plot, 
                       levels = c("Mobility characteristics", "Income characteristics", "Employment characteristics", "Demographic characteristics", "Family characteristics"))
dataset$variable <- factor(dataset$variable, 
                           levels = c("Mobility Up", 
                                      "Mobility Dn", 
                                      "Inc. Quartile 4", 
                                      "Inc. Quartile 3", 
                                      "Inc. Quartile 1", 
                                      "White", 
                                      "Male", 
                                      ">HS", 
                                      "<HS", 
                                      "Older (Age>49)", 
                                      "Never Self-employed", 
                                      "Never unemployed", 
                                      "Always Kids",
                                      "Never Kids", 
                                      "Always Married", 
                                      "Always Single"))

lvls <- levels(dataset$variable)
mob_dn.level <- c("Mobility Dn") # you want to draw line here
mob_up.level <- c("Mobility Up") # you want to draw line here
inc1.level <- c("Inc. Quartile 1") # you want to draw line here
inc3.level <- c("Inc. Quartile 3") # you want to draw line here
inc4.level <- c("Inc. Quartile 4") # you want to draw line here
wh.level <- c("White") # you want to draw line here
ml.level <- c("Male") # you want to draw line here
lhs.level <- c("<HS") # you want to draw line here
mhs.level <- c(">HS") # you want to draw line here
old.level <- c("Older (Age>49)") # you want to draw line here
unmp.level <- c("Never unemployed") # you want to draw line here
slf.level <- c("Never Self-employed") # you want to draw line here
sng.level <- c("Always Single") # you want to draw line here
mar.level <- c("Always Married") # you want to draw line here
nkds.level <- c("Never Kids") # you want to draw line here
akds.level <- c("Always Kids") # you want to draw line here

# create one single big bar graph ------------------------------------------------
q <- ggplot(dataset, aes(y=fit, x=variable, shape=time)) +
        coord_flip() +
        geom_hline(aes(yintercept=0)) +
        geom_point(position=position_dodge(width=-1),aes(shape=time), size = 2) +
        ylab(expression(paste("Pr(",upsilon['pi'], " > 90th percentile)"))) +
        xlab(NULL) +
        geom_errorbar(aes(ymin=lb, ymax=ub), position=position_dodge(width=-1), width=0, size=0.5) +
        facet_wrap(~Plot, ncol = 1, scales = "free_y") +
        geom_vline(xintercept = which(lvls == mob_dn.level) - 0.5, col='gray') +
        geom_vline(xintercept = which(lvls == mob_up.level) - 0.5, col='gray') +
        geom_vline(xintercept = which(lvls == inc1.level) - 0.5, col='gray') +
        geom_vline(xintercept = which(lvls == inc3.level) - 0.5, col='gray') +
        geom_vline(xintercept = which(lvls == inc4.level) - 0.5, col='gray') +
        geom_vline(xintercept = which(lvls == wh.level) - 0.5, col='gray') +
        scale_y_continuous(breaks = seq(-.1, .4, by = .1), lim = c(-.1,.4)) +
        scale_shape_manual(values = c(0, 15, 1, 16, 2, 17),
                           labels = c(
                                   "1970 - 1974",
                                   "1975 - 1979", 
                                   "1980 - 1984", 
                                   "1985 - 1989", 
                                   "1990 - 1995", 
                                   "1997 - 2003"
                           )) +
        theme_bw() +
        guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
        theme(axis.line.x = element_line(),
              axis.line.y = element_line(),
              panel.grid.major.y = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key = element_blank()
        )

gt = ggplotGrob(q)

# locate the panels in the gtable layout
panels <- gt$layout$t[grepl("panel", gt$layout$name)]
panels

gt$heights[panels[1]] = unit(1, "null")
gt$heights[panels[2]] = unit(2, "null")
gt$heights[panels[3]] = unit(1, "null")
gt$heights[panels[4]] = unit(3, "null")
gt$heights[panels[5]] = unit(3, "null")
gt$heights[panels]

## Draw gt
grid.newpage()
grid.draw(gt)

ggsave(filename = paste0(graphs,"lpm_fe_pr_high_vol_rel_90.png"), plot = gt, height = 8, width = 6.5, units = "in", scale = 1)

rm(lvls,mob_dn.level,mob_up.level,inc1.level,inc3.level,inc4.level,wh.level,ml.level,lhs.level,mhs.level,old.level,unmp.level,slf.level,sng.level,mar.level,nkds.level,akds.level,panels,q,gt,dataset)

rm(df_phat, phat_slp_1_t_1,phat_slp_1_t_2,phat_slp_1_t_3,phat_slp_1_t_4,phat_slp_1_t_5,phat_slp_1_t_6,phat_slp_3_t_1,phat_slp_3_t_2,phat_slp_3_t_3,phat_slp_3_t_4,phat_slp_3_t_5,phat_slp_3_t_6,phat_inc_1_t_1,phat_inc_1_t_2,phat_inc_1_t_3,phat_inc_1_t_4,phat_inc_1_t_5,phat_inc_1_t_6,phat_inc_3_t_1,phat_inc_3_t_2,phat_inc_3_t_3,phat_inc_3_t_4,phat_inc_3_t_5,phat_inc_3_t_6,phat_inc_4_t_1,phat_inc_4_t_2,phat_inc_4_t_3,phat_inc_4_t_4,phat_inc_4_t_5,phat_inc_4_t_6,phat_wh_2_t_1,phat_wh_2_t_2,phat_wh_2_t_3,phat_wh_2_t_4,phat_wh_2_t_5,phat_wh_2_t_6,phat_ml_2_t_1,phat_ml_2_t_2,phat_ml_2_t_3,phat_ml_2_t_4,phat_ml_2_t_5,phat_ml_2_t_6,phat_edu_1_t_1,phat_edu_1_t_2,phat_edu_1_t_3,phat_edu_1_t_4,phat_edu_1_t_5,phat_edu_1_t_6,phat_edu_3_t_1,phat_edu_3_t_2,phat_edu_3_t_3,phat_edu_3_t_4,phat_edu_3_t_5,phat_edu_3_t_6,phat_older_2_t_1,phat_older_2_t_2,phat_older_2_t_3,phat_older_2_t_4,phat_older_2_t_5,phat_older_2_t_6,phat_unmp_1_t_1,phat_unmp_1_t_2,phat_unmp_1_t_3,phat_unmp_1_t_4,phat_unmp_1_t_5,phat_unmp_1_t_6,phat_slf_1_t_1,phat_slf_1_t_2,phat_slf_1_t_3,phat_slf_1_t_4,phat_slf_1_t_5,phat_slf_1_t_6,phat_mar_1_t_1,phat_mar_1_t_2,phat_mar_1_t_3,phat_mar_1_t_4,phat_mar_1_t_5,phat_mar_1_t_6,phat_mar_3_t_1,phat_mar_3_t_2,phat_mar_3_t_3,phat_mar_3_t_4,phat_mar_3_t_5,phat_mar_3_t_6,phat_kds_1_t_1,phat_kds_1_t_2,phat_kds_1_t_3,phat_kds_1_t_4,phat_kds_1_t_5,phat_kds_1_t_6,phat_kds_3_t_1,phat_kds_3_t_2,phat_kds_3_t_3,phat_kds_3_t_4,phat_kds_3_t_5,phat_kds_3_t_6)

# prediction groups key -----
# grp,            inc,  mob,  unmp, slf,  age,  edu,  sex,  race, mar,    kds
# phat_secure,    hi,   -,    no,   -,    -,    hi,   -,    -,    alw mar,-
# phat_insecure,  lo,   -,    ys,   -,    -,    lo,   -,    -,    alw sin,-

# predict secure HH -----
phat_secure_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=.95,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=.95,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=.95,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

phat_secure_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=.95,c_int_inc_4_t_2=.95,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=.95,c_int_edu_3_t_2=.95,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=.95,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=.95,c_int_mar_3_t_2=.95,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

phat_secure_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=.95,c_int_inc_4_t_2=0,c_int_inc_4_t_3=.95,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=.95,c_int_edu_3_t_2=0,c_int_edu_3_t_3=.95,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=.95,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=.95,c_int_mar_3_t_2=0,c_int_mar_3_t_3=.95,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

phat_secure_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=.95,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=.95,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=.95,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=.95,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=.95,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=.95,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=.95,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

phat_secure_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=.95,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=.95,c_int_inc_4_t_6=0,
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=.95,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=.95,c_int_edu_3_t_6=0,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=.95,c_int_unmp_1_t_6=0,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=.95,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=.95,c_int_mar_3_t_6=0,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

phat_secure_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                              newdata = data.frame(
                                      c_inc_1=0,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                      c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                      c_inc_4=.95,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=.95,
                                      c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                      c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                      c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                      c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                      c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                      c_edu_1=0,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                      c_edu_3=.95,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=.95,
                                      c_unmp_1=.95,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=.95,
                                      c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                      c_mar_1=0,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                      c_mar_3=.95,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=.95,
                                      c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                      c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                      c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                              ))

# predict insecure HH -----
phat_insecure_t_1 <- predict.lm(object = model_lm, se.fit = TRUE, 
                                newdata = data.frame(
                                        c_inc_1=.95,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                        c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                        c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                        c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                        c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                        c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                        c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                        c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                        c_edu_1=.95,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                        c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                        c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                        c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                        c_mar_1=.95,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                        c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                        c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                        c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                        c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                                ))

phat_insecure_t_2 <- predict.lm(object = model_lm, se.fit = TRUE, 
                                newdata = data.frame(
                                        c_inc_1=.95,c_int_inc_1_t_2=.95,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                        c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                        c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                        c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                        c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                        c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                        c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                        c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                        c_edu_1=.95,c_int_edu_1_t_2=.95,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                        c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                        c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                        c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                        c_mar_1=.95,c_int_mar_1_t_2=.95,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                        c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                        c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                        c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                        c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                                ))

phat_insecure_t_3 <- predict.lm(object = model_lm, se.fit = TRUE, 
                                newdata = data.frame(
                                        c_inc_1=.95,c_int_inc_1_t_2=0,c_int_inc_1_t_3=.95,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                        c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                        c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                        c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                        c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                        c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                        c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                        c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                        c_edu_1=.95,c_int_edu_1_t_2=0,c_int_edu_1_t_3=.95,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                        c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                        c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                        c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                        c_mar_1=.95,c_int_mar_1_t_2=0,c_int_mar_1_t_3=.95,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                        c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                        c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                        c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                        c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                                ))

phat_insecure_t_4 <- predict.lm(object = model_lm, se.fit = TRUE, 
                                newdata = data.frame(
                                        c_inc_1=.95,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=.95,c_int_inc_1_t_5=0,c_int_inc_1_t_6=0,
                                        c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                        c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                        c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                        c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                        c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                        c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                        c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                        c_edu_1=.95,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=.95,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                        c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                        c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                        c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                        c_mar_1=.95,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=.95,c_int_mar_1_t_5=0,c_int_mar_1_t_6=0,
                                        c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                        c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                        c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                        c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                                ))

phat_insecure_t_5 <- predict.lm(object = model_lm, se.fit = TRUE, 
                                newdata = data.frame(
                                        c_inc_1=.95,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=.95,c_int_inc_1_t_6=0,
                                        c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                        c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                        c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                        c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                        c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                        c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                        c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                        c_edu_1=.95,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=0,
                                        c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                        c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                        c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                        c_mar_1=.95,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=.95,c_int_mar_1_t_6=0,
                                        c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                        c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                        c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                        c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                                ))

phat_insecure_t_6 <- predict.lm(object = model_lm, se.fit = TRUE, 
                                newdata = data.frame(
                                        c_inc_1=.95,c_int_inc_1_t_2=0,c_int_inc_1_t_3=0,c_int_inc_1_t_4=0,c_int_inc_1_t_5=0,c_int_inc_1_t_6=.95,
                                        c_inc_3=0,c_int_inc_3_t_2=0,c_int_inc_3_t_3=0,c_int_inc_3_t_4=0,c_int_inc_3_t_5=0,c_int_inc_3_t_6=0,
                                        c_inc_4=0,c_int_inc_4_t_2=0,c_int_inc_4_t_3=0,c_int_inc_4_t_4=0,c_int_inc_4_t_5=0,c_int_inc_4_t_6=0,
                                        c_slp_1=0,c_int_slp_1_t_2=0,c_int_slp_1_t_3=0,c_int_slp_1_t_4=0,c_int_slp_1_t_5=0,c_int_slp_1_t_6=0,
                                        c_slp_3=0,c_int_slp_3_t_2=0,c_int_slp_3_t_3=0,c_int_slp_3_t_4=0,c_int_slp_3_t_5=0,c_int_slp_3_t_6=0,
                                        c_wh_2=0,c_int_wh_2_t_2=0,c_int_wh_2_t_3=0,c_int_wh_2_t_4=0,c_int_wh_2_t_5=0,c_int_wh_2_t_6=0,
                                        c_ml_2=0,c_int_ml_2_t_2=0,c_int_ml_2_t_3=0,c_int_ml_2_t_4=0,c_int_ml_2_t_5=0,c_int_ml_2_t_6=0,
                                        c_older_2=0,c_int_older_2_t_2=0,c_int_older_2_t_3=0,c_int_older_2_t_4=0,c_int_older_2_t_5=0,c_int_older_2_t_6=0,
                                        c_edu_1=.95,c_int_edu_1_t_2=0,c_int_edu_1_t_3=0,c_int_edu_1_t_4=0,c_int_edu_1_t_5=0,c_int_edu_1_t_6=.95,
                                        c_edu_3=0,c_int_edu_3_t_2=0,c_int_edu_3_t_3=0,c_int_edu_3_t_4=0,c_int_edu_3_t_5=0,c_int_edu_3_t_6=0,
                                        c_unmp_1=0,c_int_unmp_1_t_2=0,c_int_unmp_1_t_3=0,c_int_unmp_1_t_4=0,c_int_unmp_1_t_5=0,c_int_unmp_1_t_6=0,
                                        c_slf_1=0,c_int_slf_1_t_2=0,c_int_slf_1_t_3=0,c_int_slf_1_t_4=0,c_int_slf_1_t_5=0,c_int_slf_1_t_6=0,
                                        c_mar_1=.95,c_int_mar_1_t_2=0,c_int_mar_1_t_3=0,c_int_mar_1_t_4=0,c_int_mar_1_t_5=0,c_int_mar_1_t_6=.95,
                                        c_mar_3=0,c_int_mar_3_t_2=0,c_int_mar_3_t_3=0,c_int_mar_3_t_4=0,c_int_mar_3_t_5=0,c_int_mar_3_t_6=0,
                                        c_kds_1=0,c_int_kds_1_t_2=0,c_int_kds_1_t_3=0,c_int_kds_1_t_4=0,c_int_kds_1_t_5=0,c_int_kds_1_t_6=0,
                                        c_kds_3=0,c_int_kds_3_t_2=0,c_int_kds_3_t_3=0,c_int_kds_3_t_4=0,c_int_kds_3_t_5=0,c_int_kds_3_t_6=0,
                                        c_t_2=0,c_t_3=0,c_t_4=0,c_t_5=0,c_t_6=0
                                ))

# compare results -----

df_phat_fit <- rbind(phat_secure_t_1$fit,phat_secure_t_2$fit,phat_secure_t_3$fit,phat_secure_t_4$fit,phat_secure_t_5$fit,phat_secure_t_6$fit,phat_insecure_t_1$fit,phat_insecure_t_2$fit,phat_insecure_t_3$fit,phat_insecure_t_4$fit,phat_insecure_t_5$fit,phat_insecure_t_6$fit)
df_phat_sefit <- rbind(phat_secure_t_1$se.fit,phat_secure_t_2$se.fit,phat_secure_t_3$se.fit,phat_secure_t_4$se.fit,phat_secure_t_5$se.fit,phat_secure_t_6$se.fit,phat_insecure_t_1$se.fit,phat_insecure_t_2$se.fit,phat_insecure_t_3$se.fit,phat_insecure_t_4$se.fit,phat_insecure_t_5$se.fit,phat_insecure_t_6$se.fit)
df_phat <- data.frame(cbind(df_phat_fit, df_phat_sefit))
df_phat <- rename(df_phat, fit="X1", se.fit="V2")

df_phat$names <- c("phat_secure_t_1","phat_secure_t_2","phat_secure_t_3","phat_secure_t_4","phat_secure_t_5","phat_secure_t_6","phat_insecure_t_1","phat_insecure_t_2","phat_insecure_t_3","phat_insecure_t_4","phat_insecure_t_5","phat_insecure_t_6")
rownames(df_phat) <- seq.int(nrow(df_phat))
df_phat$test <- str_split_fixed(df_phat$names, "_", 2)[,2]
df_phat$variable <- str_split_fixed(df_phat$test, "_t_", 2)[,1]
df_phat$time <- str_split_fixed(df_phat$test, "_t_", 2)[,2]
df_phat <- select(df_phat, fit, se.fit, variable, time)
df_phat$fit <- as.numeric(df_phat$fit)
df_phat$se.fit <- as.numeric(df_phat$se.fit)

df_phat <- mutate(df_phat,
                  lb = fit - qnorm(.975)*se.fit,
                  ub = fit + qnorm(.975)*se.fit)
df_phat$time <- factor(df_phat$time, levels = c("6", "5", "4", "3", "2", "1"))
df_phat$variable <- factor(df_phat$variable, 
                           levels = c("secure", "insecure"),
                           labels = c("Secure HH", "Insecure HH"))

# prepare bar graph to comapre groups ----

ggplot(df_phat, aes(y=fit, x=time, group=variable)) +
        geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
        coord_flip() +
        geom_hline(aes(yintercept=0)) +
        ylab(expression(paste("Pr(",upsilon['pi'], " > 90th percentile)"))) +
        xlab(NULL) +
        geom_errorbar(aes(ymin=lb, ymax=ub), position=position_dodge(width=.95), width=0, size=.8) +
        scale_fill_manual(values = c("Secure HH" = "gray 50", "Insecure HH" = "gray 20")) +
        scale_y_continuous(breaks = seq(-.1, .4, by = .1), lim = c(-.12,.4)) +
        scale_x_discrete(breaks=c("1","2","3","4","5","6"),
                         labels = c(
                                 "1970 - 1974",
                                 "1975 - 1979", 
                                 "1980 - 1984", 
                                 "1985 - 1989", 
                                 "1990 - 1995", 
                                 "1997 - 2003"
                         )) +
        theme_bw() +
        theme(axis.line.x = element_line(),
              axis.line.y = element_line(),
              panel.grid.major.y = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key = element_blank()
        )

ggsave(filename = paste0(graphs,"lpm_fe_pr_high_vol_groups_bar_90.png"), plot = last_plot(), height = 4.75, width = 8, units = "in", scale = 1)

ggsave(filename = paste0(graphs,"lpm_fe_pr_high_vol_groups_bar_90_small.png"), plot = last_plot(), height = 6, width = 4.75, units = "in", scale = 1)

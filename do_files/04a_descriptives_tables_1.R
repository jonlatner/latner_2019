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

# LIBRARY
library(tidyverse)
library(beepr)
library(stargazer)

# NO SCIENTIFIC NOTATION
options(scipen=999)

# MANUAL FUNCTION
insertrow <- function(existingDF, newrow, r) {
        existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
        existingDF[r,] <- newrow
        existingDF
}

# LOAD DATA ----

x <- readRDS(file = paste0(data, "psid_clean_sample.rds"))
x <- select(x, pid, volatility, slope, year, quartile_beg, income_beg, income_beg_unadj,
            education, age, marr_chng, kids_chng, self_hd, unemp_hd, male, white) # control variables

# CLEAN DATA ----

x <- arrange(x, pid, year) %>%
        mutate(vol_a = log(volatility),
               up = ifelse(slope > 5, yes = slope, no = NA),
               dn = ifelse(slope < -5, yes = -1*slope, no = NA),
               t = ifelse(year < 1975, yes = 1,
                          ifelse(year >= 1975 & year < 1980, yes = 2,
                                 ifelse(year >= 1980 & year < 1985, yes = 3,
                                        ifelse(year >= 1985 & year < 1990, yes = 4,
                                               ifelse(year >= 1990 & year < 1997, yes = 5,
                                                      ifelse(year >= 1997, yes = 6, no = 0)))))))

x <- rename(x, slp = slope, qinc = quartile_beg, inc_u = income_beg_unadj, inc_a = income_beg, vol_u = volatility, edu = education, mar = marr_chng, kds = kids_chng, unmp = unemp_hd, slf = self_hd, ml = male, wh = white)

x <- mutate(x, dn = dn/100)
x <- mutate(x, up = up/100)
x <- mutate(x, slp = slp/100)
x <- mutate(x, inc_u = exp(inc_u))

x <- mutate(x, older = ifelse(age > 49, yes = 1, no = 0))

x <- group_by(x, year) %>%
        mutate(pct80 = quantile(vol_u, probs = 0.90),
               high = as.integer(ifelse(vol_u > pct80, yes = 1, no = 0))) %>%
        ungroup()

group_by(x, t, qinc) %>% summarize(y = exp(min(inc_u))) %>% filter(qinc==4)
group_by(x, t, high) %>% summarize(y = min(vol_u)) %>% filter(high==1)

#create a data frame for each set of categorical variables
z <- data.frame(matrix(NA, nrow = nrow(x), ncol = 0))
vars =  c("edu", "older", "mar", "kds", "t")
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
        rm(dummy, i, j, idx, n)
}

#add in the dependent variable
y <- cbind(x, z)
y$vol_a <- log(x$vol_u)
y <- group_by(y, t) %>% 
        mutate(dv = var(vol_a)) %>%
        ungroup()

# Average number of study windows
x <- group_by(x, pid) %>% mutate(count = n()) %>% ungroup()
windows <- round(mean(x$count), 2)

# Average number of study windows
x <- group_by(x, t, pid) %>% mutate(count_t = n()) %>% ungroup()
with(x, table(t, count_t))

# Unique observations
x <- group_by(x, pid) %>% mutate(unique = ifelse(row_number()==1, yes = 1, no = 0)) %>% ungroup()
unique <- format(sum(x$unique), big.mark = ",")
unique

# Total observations
obs <- format(nrow(x), big.mark = ",")

# Create data frame
rm(x, z)

# TABLES ----
# Note: stargazer v. 5.1 does not play nicely with dplyr's tbl_df class. As a temporary work-around I pipe the dataset to data.frame.
# order variables in data frame

l <- data.frame(select(y, inc_u, inc_a, slp, up, dn, vol_u, vol_a, high, ml, wh, matches("edu_"), older, matches("mar_"), matches("kds_"), unmp, slf))

# VARIABLE LABLES
inc_u <- paste0("\\multicolumn{7}{l}{\\emph{Income characteristics (household):}} \\\\ 
                \\hspace{10mm} $^{1}$Income at start ($y_{pi}$)")
inc_a <- paste0("\\hspace{10mm} $^{2}$Log income at start ($y_{pi}$)")

slp <- paste0("\\multicolumn{7}{l}{\\phantom{empty}} \\\\ 
              \\hspace{10mm} $^{3}$Change in income ($\\Delta \\hat{y}_{pi}$)")
up <- paste0("\\hspace{10mm} $\\Delta \\hat{y}_{pi} >$ 5\\%")
dn <- paste0("\\hspace{10mm} $\\Delta \\hat{y}_{pi} <$ -5\\%")

vol_u <- paste0("\\multicolumn{7}{l}{\\phantom{empty}} \\\\ 
                \\hspace{10mm} $^{4}$Income volatility ($\\upsilon_{pi}$)")
vol_a <- paste0("\\hspace{10mm} Income volatility (Log $\\upsilon_{pi}$)")
high <- paste0("\\hspace{10mm} High income volatility ($\\upsilon_{pi} > 90^{th}$ pct)")


ml <- paste0("\\multicolumn{7}{l}{\\phantom{empty}} \\\\ 
             \\multicolumn{7}{l}{\\emph{Demographic characteristics (head):}} \\\\ 
             \\hspace{10mm} Male")

wh <- paste0("\\multicolumn{7}{l}{\\phantom{empty}} \\\\ 
             \\hspace{10mm} White")

older <- paste0("\\multicolumn{7}{l}{\\phantom{empty}} \\\\ 
              \\hspace{10mm} Older (Age $>$49)")

edu_1 <- paste0("\\multicolumn{7}{l}{\\phantom{empty}} \\\\ 
                \\hspace{10mm} Less than HS")
edu_2 <- paste0("\\hspace{10mm} HS")
edu_3 <- paste0("\\hspace{10mm} More than HS")

mar_1 <- paste0("\\multicolumn{7}{l}{\\phantom{empty}} \\\\ 
                \\multicolumn{7}{l}{\\emph{Family characteristics (In a study period):}} \\\\
                \\hspace{10mm} Always single")
mar_2 <- paste0("\\hspace{10mm} Marital change")
mar_3 <- paste0("\\hspace{10mm} Always married")

kds_1 <- paste0("\\multicolumn{7}{l}{\\phantom{empty}} \\\\ 
                \\hspace{10mm} Never kids")
kds_2 <- paste0("\\hspace{10mm} Sometimes kids")
kds_3 <- paste0("\\hspace{10mm} Always kids")

unmp <- paste0("\\multicolumn{7}{l}{\\phantom{empty}} \\\\ 
               \\multicolumn{7}{l}{\\emph{Employment characteristics (In a study period):}} \\\\
               \\hspace{10mm} Ever unemployed")
slf <- paste0("\\multicolumn{7}{l}{\\phantom{empty}} \\\\ 
              \\hspace{10mm} Ever self employed")


# LARGE TABLE ----

t <- stargazer(l, 
               out = paste0(tables,"descriptives_large_hh.tex"), 
               column.sep.width = "0pt",
               header = FALSE,
               float = FALSE,
               align = TRUE, digits = 3, digits.extra = 0, 
               notes.align = "l",
               notes.append = FALSE,
               model.numbers = TRUE,
               median = TRUE, 
               omit.summary.stat = c("min", "max"), 
               dep.var.labels.include = FALSE,
               dep.var.caption = "",
               initial.zero = FALSE,
               covariate.labels = c(inc_u,
                                    inc_a,
                                    slp,
                                    up,
                                    dn,
                                    vol_u,
                                    vol_a,
                                    high,
                                    ml,
                                    wh,
                                    edu_1,
                                    edu_2,
                                    edu_3,
                                    older,
                                    mar_1,
                                    mar_2,
                                    mar_3,
                                    kds_1,
                                    kds_2,
                                    kds_3,
                                    unmp,
                                    slf
               ),
               star.cutoffs = NA,
               single.row = TRUE
)

Tables <- as.data.frame(t)
Tables$t <- as.character(Tables$t)

hline <- "\\hline \\\\[-1.8ex]"
totalobs <- paste0("\\hspace{3mm}Total N & \\multicolumn{1}{l}{", obs,"} & & & & \\\\")
uniqueobs <- paste0("\\hspace{3mm}Unique N & \\multicolumn{1}{l}{", unique,"} & & & & \\\\")
windowsobs <- paste0("\\hspace{3mm}Avg. study periods per unique N & \\multicolumn{1}{l}{", windows,"} & & & & \\\\")
notes <- paste0("
\\multicolumn{7}{l}{$^1$ The average of the first two-observations in a study period.  Income is family size adjusted.} \\\\
\\multicolumn{7}{l}{$^2$ The residual of log income after taking out year fixed effects in a given study period for the first year of a given study period.} \\\\
\\multicolumn{7}{l}{$^3$ Where $\\Delta \\hat{y}_{pi} = \\hat{y}_{pi,t=N} - \\hat{y}_{pi,t=1}$ if $\\hat{y}_{pit} = \\beta_{0i} + \\beta_{1i} T$} \\\\
\\multicolumn{7}{l}{$^4$ Where $\\upsilon_{pi} = \\text{\\emph{Standard deviation }} (\\mu_{pit})$ if $\\text{log } y_{pit} = \\beta_{0pi} + \\beta_{1pi} T + \\mu_{pit}$}")

# Find where you want to put in the user specific rows.  In our case, this is right after the last fixed effect.
r <- 44

Tables <- insertrow(Tables, totalobs, r)
Tables <- insertrow(Tables, uniqueobs, r+1)
Tables <- insertrow(Tables, windowsobs, r+2)
Tables <- insertrow(Tables, hline, r+3)
Tables <- insertrow(Tables, notes, r+4)

write.table(Tables,file=paste0(tables,"descriptives_large_hh.tex"),
            row.names= FALSE, 
            na="", 
            quote = FALSE, 
            col.names = FALSE)

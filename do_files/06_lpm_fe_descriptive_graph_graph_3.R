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

# no scientific notation
options(scipen=999)

# load data ----

x <- readRDS(file = paste0(data,"psid_clean_sample.rds"))
x <- select(x, pid, year, volatility, quartile_beg, education, marr_chng, kids_chng, unemp_hd, self_hd) # control variables

# clean data ----

x <- mutate(x, 
            t = ifelse(year < 1975, yes = 1,
                       ifelse(year >= 1975 & year < 1980, yes = 2,
                              ifelse(year >= 1980 & year < 1985, yes = 3,
                                     ifelse(year >= 1985 & year < 1990, yes = 4,
                                            ifelse(year >= 1990 & year < 1997, yes = 5,
                                                   ifelse(year >= 1997, yes = 6, no = 0)))))))

x <- rename(x, dv = volatility, inc = quartile_beg, edu = education, mar = marr_chng, kds = kids_chng, unmp = unemp_hd) %>%
        arrange(pid,t,year)

x <- group_by(x, t) %>%
        mutate(pct80 = quantile(dv, probs = 0.90),
               high = as.integer(ifelse(dv > pct80, yes = 1, no = 0))) %>%
        ungroup()

x <- mutate(x, 
            sec = ifelse(inc == 4 & edu == 3 & mar == 3 & unmp == 0, yes = 1,
                         ifelse(inc == 1 & edu == 1 & mar == 1 & unmp == 1, yes = 0, no = NA))) %>%
        filter(!is.na(sec))

x$sec <- factor(x$sec, levels = c("1","0"), labels = c("Secure HH", "Insecure HH"))
x$time <- factor(x$t, levels = c("6", "5", "4", "3", "2", "1"))

table <- x %>%
        group_by(time, sec) %>%
        summarize(
                N = n(),
                mean = mean(high),
                sd   = sd(high),
                se   = sd / sqrt(N),
                lb = mean - 2*se,
                ub = mean + 2*se,
                  ) %>%
        ungroup()
                  
# GRAPH ----

ggplot(table, aes(y=mean, x=time, group=sec)) +
        geom_bar(aes(fill = sec), position = "dodge", stat="identity") +
        coord_flip() +
        geom_hline(aes(yintercept=0)) +
        ylab(expression(paste(upsilon['pi'], " > 90th percentile"))) +
        xlab(NULL) +
        geom_errorbar(aes(ymin=lb, ymax=ub), position=position_dodge(width=.95), width=0, size=.8) +
        scale_fill_manual(values = c("Secure HH" = "gray 50", "Insecure HH" = "gray 20")) +
        # scale_y_continuous(breaks = seq(-.1, .4, by = .1), lim = c(-.12,.4)) +
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

ggsave(filename = paste0(graphs,"high_vol_groups_bar_90_descriptive.png"), plot = last_plot(), height = 4.75, width = 8, units = "in", scale = 1)

ggsave(filename = paste0(graphs,"high_vol_groups_bar_90_descriptive_small.png"), plot = last_plot(), height = 6, width = 4.75, units = "in", scale = 1)

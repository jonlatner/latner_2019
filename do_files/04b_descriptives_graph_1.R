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
library(stargazer)
library(Hmisc)

# NO SCIENTIFIC NOTATION
options(scipen=999)

# LOAD DATA ----

x <- select(readRDS(file = paste0(data,"psid_clean_sample.rds")), year, std, volatility, matches("weight"))

# CLEAN DATA ----

z <- group_by(x, year) %>%
        summarise(pct10 = quantile(volatility, probs = 0.10), 
                  pct90 = quantile(volatility, probs = 0.90),
                  pct50 = quantile(volatility, probs = 0.50),
                  var_vol = var((volatility)), 
                  sd_vol = sd((volatility)), 
                  var_sd = var(log(std)), 
                  mean_vol = mean(volatility),
                  mean_sd = mean(std)) %>%
        mutate(ratio_90_10 = pct90/pct10, ratio_90_50 = pct90/pct50, ratio_50_10 = pct50/pct10) %>%
        ungroup()

index <- arrange(z, year) %>%
        mutate_each(funs(./first(.)))
index$year <- z$year
long <- pivot_longer(index, !year, names_to = "variable")

level <- pivot_longer(z, !year, names_to = "variable")

# GRAPH ----

y <- filter(long, variable == "mean_vol" | variable == "sd_vol")
y$variable <- y$variable[, drop = TRUE]
y$variable <- relevel(as.factor(y$variable), ref = "sd_vol")

ggplot(data = y, aes(x = year, y = value, color = variable, linetype = variable)) +
        geom_line(size = 2) +
        ylab("Index") +
        xlab("Study period") + 
        scale_color_manual(values = c("black",
                                      "gray"),
                           labels = c(expression(paste("Inequality - SD(",upsilon['pi'],")")),
                                      expression(paste("Volatility (",upsilon['p'],")")))) +
        scale_linetype_manual(values = c("solid",
                                         "solid"),
                              labels = c(expression(paste("Inequality - SD(",upsilon['pi'],")")),
                                         expression(paste("Volatility (",upsilon['p'],")")))) +
        scale_y_continuous(breaks = seq(1, 2, by = .2), limits = c(.9, 2)) +
        scale_x_continuous(breaks = seq(1970, 2005, by = 5), limits = c(1970, 2005)) + 
        guides(col = guide_legend(nrow = 1)) +
        theme_bw() +
        theme(legend.key.width = unit(2,"cm"),
              panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size = .5),
              axis.line.x = element_line(color = "black", size = .5),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text.align = 0,
              legend.key = element_blank()
        )

ggsave(filename = paste0(graphs,"volatility_and_inequality_trends_hh.png"), plot = last_plot(), height = 4.75, width = 8, units = "in", scale = 1)

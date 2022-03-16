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
library(dummies)
library(stargazer)
library(plm)
library(robumeta) # group.center

# NO SCIENTIFIC NOTATION
options(scipen=999)

# MANUAL FUNCTION
insertrow <- function(existingDF, newrow, r) {
        existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
        existingDF[r,] <- newrow
        existingDF
}

# LOAD DATA ----

x <- readRDS(file = paste0(data,"psid_clean_sample.rds"))
x <- select(x, pid, year, volatility, age, slope, income_beg, income_beg_unadj)

# CLEAN DATA ----

x <- arrange(x, pid, year) %>%
        rename(inc = income_beg) %>%
        mutate(ln_volatility = log(volatility),
               up = ifelse(slope > 5, yes = slope, no = 0),
               dn = ifelse(slope < -5, yes = -1*slope, no = 0),
               t = ifelse(year < 1975, yes = 1,
                          ifelse(year >= 1975 & year < 1980, yes = 2,
                                 ifelse(year >= 1980 & year < 1985, yes = 3,
                                        ifelse(year >= 1985 & year < 1990, yes = 4,
                                               ifelse(year >= 1990 & year < 1997, yes = 5,
                                                      ifelse(year >= 1997, yes = 6, no = 0)))))))
        
x <- mutate(x, dn = dn/100)
x <- mutate(x, up = up/100)
x <- mutate(x, inc = inc/100)

#tranform time into categorical dummy variables
time <- dummy(x = x$t, sep = "_")
time <- time[,-1]
x <- cbind(x, time)

x <- mutate(x, older = ifelse(age > 49, yes = 1, no = 0))

# CENTER VARIABLES
vars = c("ln_volatility", "dn", "up", "inc", "older", "age")
for (i in seq(2,6,1)) {
        vars <- append(vars, paste0("t_", i))
}
for (i in vars) {
        x$test <- group.center(x[[i]], x$pid)
        x$test <- as.numeric(x$test)
        names(x)[names(x) == "test"] <- paste0("c",i)
}
rm(vars)

ctime <- as.matrix(select(x, matches("ct_")))
x <- arrange(x, pid, year)

# FE REGRESSION - OLS on group mean center variables ----

# regression_plm_fe <- plm(formula = ln_volatility ~ dn + up + inc + older + factor(t), data = x, index = c("pid", "year"), model = "within")
# summary(regression_plm_fe)

iv = "cdn + cup + cinc + cinc + colder + ct_2 + ct_3 + ct_4 + ct_5 + ct_6"

y <- select(x, cln_volatility, cdn, cup, cinc, cdn, colder, matches("ct_"))
summary(y)

# VARIANCE FUNCTION REGRESSION
beta <- lm(formula = paste0("cln_volatility ~ ", iv), data = x)
x$yhat <- predict(beta) # OLS resids
x$R <- residuals(beta) # OLS resids
x$R2 <- x$R^2 # Squared resids for glm fit
lambda <- glm(formula = paste0("R2 ~ ", iv), data = x, family = Gamma(link = log), control = list(maxit = 50)) # Gamma reg on log(r2)
x$S2 <- exp(predict(lambda)) # Fitted variances, exp(Xb)
x <- mutate(x, LOGLIK = (-.5*(log(S2) + (R2/S2)))) # Evaluating log likelihood
x <- mutate(x, LL0 = sum(LOGLIK)) # Summing log likelihood

stargazer(beta, lambda, type = "text")

# UPDATING BETA AND LAMBDA COEFFICIENTS
x <- mutate(x, DLL = 1)
while(x$DLL > .00001) {
        x <- select(x, -R, -R2)
        beta_ols_fe <- lm(formula = paste0("cln_volatility ~ ", iv), data = x, weights = 1/S2) # WLS with variances as weights
        x$R <- residuals(beta_ols_fe) # WLS resids
        x$R2 <- x$R^2 # Squared resids for glm fit
        lambda_ols_fe <- glm(formula = paste0("R2 ~ ", iv), data = x, family = Gamma(link = log)) # Gamma reg on log(r2)
        x$S2 <- exp(predict(lambda_ols_fe)) # Fitted variances, exp(Xb)
        x <- mutate(x, LOGLIK = (-.5*(log(S2) + (R2/S2)))) # Evaluating log likelihood
        x <- mutate(x, LLN = sum(LOGLIK)) # Summing log likelihood
        summary(x$LL0)
        x <- mutate(x, DLL = LLN - LL0, LL0 = LLN) # Assess convergence
        x <- select(x, -LLN)
}
x <- select(x, -R, -R2, -S2, -LOGLIK, -LL0, -DLL)

stargazer(beta_ols_fe, lambda_ols_fe, type = "text")

# TABLES ----

# VARIABLE LABLES
dn <- paste0("Downward mobility ($\\Delta \\hat{y}_{pi} < -5$)")
up <- paste0("& & \\\\ Upward mobility ($\\Delta \\hat{y}_{pi} > 5)$")

inc <- paste0("& & \\\\ Income at start")

older <- paste0("& & \\\\ Older (Age $>$ 49)")

study_period_2 <- paste0("& & \\\\ 
                         \\emph{Study period beginning:} & & \\\\
                         \\hspace{10mm} 1975 $-$ 1979")
study_period_3 <- paste0("\\hspace{10mm} 1980 $-$ 1984")
study_period_4 <- paste0("\\hspace{10mm} 1985 $-$ 1989")
study_period_5 <- paste0("\\hspace{10mm} 1990 $-$ 1996")
study_period_6 <- paste0("\\hspace{10mm} 1997 $-$ 2003")

intercept <- paste0("& & \\\\ 
                    Constant")


t <- stargazer(beta_ols_fe, lambda_ols_fe, 
               out = paste0(tables,"vfr_fe_hh_period.tex"), 
               model.names = FALSE,
               model.numbers = FALSE,
               dep.var.caption = "",
               dep.var.labels.include = FALSE,
               covariate.labels = c(dn,
                                    up,
                                    inc,
                                    older,
                                    study_period_2,
                                    study_period_3,
                                    study_period_4,
                                    study_period_5,
                                    study_period_6,
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
header <- "\\hline \\\\ [-1.8ex] & \\multicolumn{1}{c}{Average ($\\beta$)} & \\multicolumn{1}{c}{Distribution ($\\lambda$)} \\\\"

# Find where you want to put in the user specific rows.  In our case, this is right after the last fixed effect.
r <- 4
Tables <- insertrow(Tables, header, r)

write.table(Tables,file=paste0(tables,"vfr_fe_hh_period.tex"),
            row.names= FALSE, 
            na="", 
            quote = FALSE, 
            col.names = FALSE)


# GRAPHS (MOBILITY) ----

# get the coefficient for each variable
me_up <- expand.grid(direction = c(seq(0, .50, .10)))
me_dn <- expand.grid(direction = c(seq(0, .50, .10)))

me_up$b_dir <- coef(summary(beta_ols_fe))[3]
me_up$l_dir <- coef(summary(lambda_ols_fe))[3]
me_up$b_cons <- coef(summary(beta_ols_fe))[1]
me_up$l_cons <- coef(summary(lambda_ols_fe))[1]

me_up <- mutate(me_up, 
                beta = (direction*b_dir),
                lambda = (direction*l_dir)) %>%
        select(direction, beta, lambda)

me_dn$b_dir <- coef(summary(beta_ols_fe))[2]
me_dn$l_dir <- coef(summary(lambda_ols_fe))[2]
me_dn$b_cons <- coef(summary(beta_ols_fe))[1]
me_dn$l_cons <- coef(summary(lambda_ols_fe))[1]
me_dn <- mutate(me_dn, 
                beta = (direction*b_dir),
                lambda = (direction*l_dir)) %>%
        select(direction, beta, lambda)
me_dn$direction <- me_dn$direction*-1

me <- rbind(me_up, me_dn)
me <- arrange(me, direction)
me <- unique(me)
rm(me_up, me_dn)

# RESHAPE
level <- pivot_longer(me, !"direction", names_to = "variable")
level$variable <- factor(level$variable, labels = c("beta", "lambda"))

# GRAPH MARGINAL EFFECTS
ggplot(data = subset(level, variable == "beta"), aes(x = direction, y = value, color = variable)) +
        geom_line(size = 2) +
        geom_hline(aes(yintercept=0), size = 2) +
        scale_color_manual(values = c("black")) +
        scale_y_continuous(breaks = seq(-.2, .2, by = .05), limits = c(-.2, .2)) +
        ylab(expression(paste("Income volatility (Log  ",hat(upsilon)['pi'],")"))) +
        xlab(expression(paste("Income mobility (",hat(y)['pi'],")"))) +
        facet_wrap(~ variable, ncol = 1, labeller = label_parsed) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color = "black", size = .5),
              axis.line.x = element_line(color = "black", size = .5),
              legend.position = "none"
        )

ggsave(filename = paste0(graphs,"margins_betas_fe.png"), plot = last_plot(), height = 6, width = 4.75, units = "in", scale = 1)


# GRAPH MARGINAL EFFECTS
ggplot(data = subset(level, variable == "lambda"), aes(x = direction, y = value, color = variable)) +
        geom_line(size = 2) +
        geom_hline(aes(yintercept=0), size = 2) +
        scale_color_manual(values = c("black")) +
        scale_y_continuous(breaks = seq(-.2, .2, by = .05), limits = c(-.2, .2)) +
        ylab(expression(paste("Income volatility (Log  ",hat(upsilon)['pi'],")"))) +
        xlab(expression(paste("Income mobility (",hat(y)['pi'],")"))) +
        facet_wrap(~ variable, ncol = 1, labeller = label_parsed) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color = "black", size = .5),
              axis.line.x = element_line(color = "black", size = .5),
              legend.position = "none"
        )

ggsave(filename = paste0(graphs,"margins_lambdas_fe.png"), plot = last_plot(), height = 6, width = 4.75, units = "in", scale = 1)


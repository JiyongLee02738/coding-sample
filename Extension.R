# Loading libraries ----
library(readr)
library(tidyverse)
library(haven)
library(gt)
library(tibble)
library(webshot2)
library(janitor)
library(ggpubr)
library(rdd)
library(kableExtra)
library(modelsummary)

# Read dataset ----
recipients <- read_csv("data/dime_recipients_1979_2020.csv")

# Cleaning ----
df_recipients_all <- recipients |> 
  filter(grepl('fd', election)) |>  # Filtered for federal election
  filter(recipient.type == "cand") |> # Candidate 
  filter(seat != "federal:senate") |> # House primary 
  select(-starts_with("nimsp")) |> # Delete 
  select(-c(party.orig, # Delete variables not used in the analysis 
            before.switch.ICPSR,
            after.switch.ICPSR,
            NID,
            FEC.ID,
            Cand.ID,
            ICPSR2,
            comtype,
            igcat,
            ind.exp.oppose,
            ind.exp.support,
            total.contribs.from.candidate, 
            total.party.contribs,
            total.pac.contribs,
            total.unitemized, 
            total.indiv.contribs,
            total.disbursements,
            total.receipts,
            num.givers.total,
            num.givers,
            seat,
            fecyear,
            recipient.cfscore.dyn,
            contributor.cfscore,
            #s.elec.stat,
            #r.elec.stat,
            bonica.cid,
            dwdime,
            dwnom1,
            recipient.type))

df1 <- df_recipients_all |> # Delete obs with NAs for primary vote share 
  filter(is.na(prim.vote.pct) == FALSE,
         prim.vote.pct != 1)
        
df1_winner <- df1 |> # Primary election winners 
  filter(pwinner == "W")

df1_lost <- df1 |> # Primary election losers 
  filter(pwinner == "L")

# Merging ----
test_1 <- df1_winner |> 
  # For each obs to have two candidates in the same election
  # Match by district, year, party
  full_join(df1_lost, by = c("distcyc", "party")) |> 
  filter(!(is.na(name.y) == TRUE),
         !is.na(prim.vote.pct.x)==TRUE,
         !is.na(prim.vote.pct.y)==TRUE) # pwinner.x is all W and pwinner.y is all L

duplicates_1 <- test_1 |> 
  group_by(name.x, distcyc) |> # For each district and primary winner (W)
  filter(n() > 1) |> # Identify election w/ more than 2 candidates
  ungroup() |>  
  # Vote share of W - vote share of L
  mutate(margin_diff = prim.vote.pct.x - prim.vote.pct.y) |> 
  group_by(name.x, distcyc) |> 
  slice_min(margin_diff, n=1) |> # Identify top 2 candidates -> smallest margin_diff
  select(-margin_diff) # To match # columns with test_1

no_duplicates <- setdiff(test_1, duplicates_1) 

df_final <- rbind(no_duplicates, duplicates_1) |> 
  mutate(ideo_diff = abs(recipient.cfscore.x - recipient.cfscore.y)) |> 
  filter(ideo_diff > median(ideo_diff)) |> # Check dist of ideo_diff
  # Keep elections between extreme and moderate candidates 
  # Extreme candidate, absolute value of ideology score is higher 
  mutate(prim.vote.share.y = prim.vote.pct.y / 100,
         prim.vote.share.x = prim.vote.pct.x / 100) |> 
  mutate(extreme.x = ifelse(abs(recipient.cfscore.x) > abs(recipient.cfscore.y),
                          1,0), # CHECK
         treat = ifelse(pwinner.x == "W" & extreme.x == 1, 1, 0)) |> 
  mutate(rv = ifelse(extreme.x == 1, 
                     (prim.vote.share.x / (prim.vote.share.x + prim.vote.share.y)) - 0.5,
                     # Winning margin of extreme candidate 
                     (prim.vote.share.y / (prim.vote.share.x + prim.vote.share.y)) - 0.5), 
         rv2 = rv^2,
         rv3 = rv^3,
         rv4 = rv^4,
         rv_treat = ifelse(treat == 1, rv, 0),
         margin = abs(rv), # Absolute value of rv
         # Convert percentage to share
         gen.voteshare.x = gen.vote.pct.x / 100, 
         gen.voteshare.y = gen.vote.pct.y / 100,
         dv = ifelse(is.na(gen.voteshare.x) == FALSE, 
                     gen.voteshare.x, # Put winner's vote share 
                     gen.voteshare.y),
         # dv_win is True if thee candidate is general election winner 
         dv_win = ifelse(gwinner.x == "W", 1, 0)) |> 
  # Treatment status is determined by whether an extremist won (rv>0)
  mutate(rv = ifelse(treat == 1 & rv < 0, NA, rv), 
         rv = ifelse(treat == 0 & rv > 0, NA, rv)) |> 
  filter(is.na(rv)==FALSE) 

# Summary Stats (Figure 4) ----
treatment_summary_2_1 <- df_final |> 
  summarize(treatment_mean = mean(treat), 
            treatment_observations = n(),
            treatment_std_dev = sd(treat))

summary_stats_2_1 <- df_final |> 
  group_by(treat) |> 
  summarize(vote_share = mean(dv, na.rm=TRUE), # Vote share 
            victory = mean(dv_win, na.rm = TRUE), # Victory 
            ideological_score = mean(recipient.cfscore.x, na.rm=TRUE) # Share from party  
  ) |> 
  select(-treat)

summary_stats_2_1 <- as.data.frame(summary_stats_2_1) |> 
  rename("Vote share" = vote_share,
         "Victory" = victory,
         "Ideological Score" = ideological_score) 

vec_sd_2 <- c(sd(df_final$dv, na.rm=TRUE), # gen.voteshare.x
            sd(df_final$dv_win, na.rm=TRUE),
            sd(df_final$recipient.cfscore.x, na.rm=TRUE))

summary_stats_2_2 <- as.data.frame(t(summary_stats_2_1)) |> # Transposed
  rename("Moderate in General" = V1,
         "Extremist in General" = V2) |> 
  tibble::rownames_to_column(var = " ")  |> 
  mutate(SD = vec_sd_2, # Standard deviation
         N = c(sum(!is.na(df_final$dv)), # Number of obs 
               sum(!is.na(df_final$dv_win)),
               sum(!is.na(df_final$recipient.cfscore.x)))) 

summary_stats_2_3 <- summary_stats_2_2 |> 
  gt() |> 
  fmt_number(rows = c(1:3), # 2 decimal places 
             decimals = 2) |> 
  fmt_number(columns = c(5),
             decimals = 0) # Integers 
#gtsave(summary_stats_2_3, "template/images/summary_stats_extension_v1.png")

# Figure 2 ----
# ideo_diff here is absdist in replication and margin is margin 

df_final_v1 <- df_final |>
  filter(margin <= 0.2, ideo_diff > median(ideo_diff)) |> # From -0.2 to 0.2
  mutate(rv_int = cut_interval(rv, # Created intervals of length 0.02
                               length = 0.02,
                               right = F)) 

df_final_v2 <- df_final_v1 |> 
  group_by(rv_int) |>
  # Mean of winner's general election vote share for each interval 
  summarize(mean_vote_share_bin = mean(dv, na.rm = TRUE)) 

df_final_v2 <- df_final_v1 |> 
  count(rv_int) |> # Count # obs in each interval 
  right_join(df_final_v2, by = "rv_int") |> # Merged with mean vote share
  mutate(rv_int = c(-10:9)/50) # -0.2 to 0.18

# Plot showing a change in vote share around margin of zero 
ggplot(df_final_v1, aes(x = as.numeric(rv_int), y = mean_vote_share_bin)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = df_final_v2, size = 2) + # Each point race, not candidates?
  geom_smooth(method = "lm", 
              aes(x = rv, y = dv, group = treat), 
              formula = y ~ x, se = FALSE) +
  geom_point(data = df_final_v1, aes(x = rv, y = dv), alpha = 0.4, size = 1) +
  labs(x = "Extreme Candidate Primary Election Winning Margin",
       y = "General Election Vote Share",
       title = "") +
  theme_bw() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  annotate("text", x = 0.17, y = 0.15, label = "N=652")  # N=652

df_final_yr <- df_final |> 
  filter(cycle.x >= 1980, 
         cycle.x <= 2010)

# Regression - years 1980 to 2010 (Figure 6) ----
# General election vote share (dv) 
# Bandwidth=5, local linear 
m1 <- lm(dv ~ treat + rv + rv_treat, data = df_final_yr |> 
           filter(ideo_diff > median(ideo_diff),
                  margin < 0.05)) # Winning margin 

# Cubic
m2 <- lm(dv ~ treat + rv + rv2 + rv3, data = df_final_yr |> 
             filter(ideo_diff > median(ideo_diff))) 
# IK bandwidth 
m3 <- RDestimate(dv ~ rv, data = df_final_yr |> 
                     filter(ideo_diff > median(ideo_diff)) )

est <- format(round(m3$est[1], 2), nsmall = 2)
se <- format(round(m3$se[1], 2), nsmall = 2)
obs <- format(round(m3$obs[1], 2), nsmall = 0)

# General election victory (dv_win)
# Bandwidth=5, local linear 
m4 <- lm(dv_win ~ treat + rv + rv_treat, data = df_final_yr |> 
           filter(ideo_diff > median(ideo_diff),
                  margin < 0.05)) # winning margin 

# Cubic
m5 <- lm(dv_win ~ treat + rv + rv2 + rv3, data = df_final_yr |> 
           filter(ideo_diff > median(ideo_diff))) 
# IK
m6 <- RDestimate(dv_win ~ rv, data = df_final_yr |> 
                     filter(ideo_diff > median(ideo_diff))) 

est1 <- format(round(m6$est[1], 2), nsmall = 2) # 2 decimal places
se1 <- format(round(m6$se[1], 2), nsmall = 2)
obs1 <- format(round(m6$obs[1], 2), nsmall = 0)

# Create a regression table 
summary_rows <- tribble(~ term, ~ m1, ~ m3, ~m2, 
                        ~ m4, ~ m6, ~ m5,
                        "RDD bandwidth", # Add bandwidth row 
                        "5", "1.99", "-", 
                        "5", "2.81", "-",
                        "Specification", # Add specification row 
                        "Local linear","IK", "Cubic", 
                        "Local linear", "IK", "Cubic")

# Change column names 
list_m <- list("General Election Vote Share" = m1,
               "General Election Vote Share" = m2,
               "General Election Victory" = m4, 
               "General Election Victory" = m5)
col3 <- c(est, se, obs)
col6 <- c(est1, se1, obs1)
cols <- data.frame(col3, col6) 
names(cols) <- c("General Election Vote Share  ",
                 "General Election Victory")

attr(cols, "position") <- c(3, 6)

modelsummary(list_m, # Create a modelsummary table 
             gof_map = c("nobs"),
             coef_map = c("treat" = "Extremist win"),
             coef_omit = "rv|rv_treat|Intercept",
             add_rows = summary_rows,
             add_columns = cols,
             fmt = "%.2f") |> # 2 significant figures 
  footnote(general = "Standard errors are in parentheses. 
                      Columns 2 and 5 are estimates using the IK bandwidth from rdd package in R.") 

# Regression - all Years (Figure 7) ----
# Main regression 

# General election vote share (dv)
# Bandwidth=5, local linear 
m1 <- lm(dv ~ treat + rv + rv_treat, data = df_final |> 
           filter(ideo_diff > median(ideo_diff),
                  margin < 0.05)) # Winning margin 

# Cubic
m2 <- lm(dv ~ treat + rv + rv2 + rv3, data = df_final  |> 
           filter(ideo_diff > median(ideo_diff)))

# Bandwidth IK
m3 <- RDestimate(dv ~ rv, data = df_final  |> 
                   filter(ideo_diff > median(ideo_diff))) 

est <- format(round(m3$est[1], 2), nsmall = 2) # 2 decimal places
se <- format(round(m3$se[1], 2), nsmall = 2)
obs <- format(round(m3$obs[1], 2), nsmall = 0)

# General election victory (dv_win)
# Bandwidth=5, local linear 
m4 <- lm(dv_win ~ treat + rv + rv_treat, data = df_final |> 
           filter(ideo_diff > median(ideo_diff),
                  margin < 0.05)) # winning margin 

# Cubic
m5 <- lm(dv_win ~ treat + rv + rv2 + rv3, data = df_final |>
           filter(ideo_diff > median(ideo_diff)))

# Bandwidth=9.68, IK
m6 <- RDestimate(dv_win ~ rv, data = df_final |>
                   filter(ideo_diff > median(ideo_diff)))

est1 <- format(round(m6$est[1], 2), nsmall = 2) # 2 decimal places
se1 <- format(round(m6$se[1], 2), nsmall = 2)
obs1 <- format(round(m6$obs[1], 2), nsmall = 0)

# Create a regression table 
summary_rows <- tribble(~ term, ~ m1, ~ m3, ~m2, 
                        ~ m4, ~ m6, ~ m5,
                        "RDD bandwidth", # Add bandwidth row 
                        "5", "1.81", "-", 
                        "5", "2.28", "-",
                        "Specification", # Add specification row 
                        "Local linear","IK", "Cubic", 
                        "Local linear", "IK", "Cubic")

# Change column names 
list_m <- list("General Election Vote Share" = m1, 
               "General Election Vote Share" = m2,
               "General Election Victory" = m4, 
               "General Election Victory" = m5)
col3 <- c(est, se, obs)
col6 <- c(est1, se1, obs1)
cols <- data.frame(col3, col6) 
names(cols) <- c("General Election Vote Share  ",
                 "General Election Victory")

attr(cols, "position") <- c(3, 6)

modelsummary(list_m, # Create a modelsummary table 
             gof_map = c("nobs"),
             coef_map = c("treat" = "Extremist win"),
             coef_omit = "rv|rv_treat|Intercept",
             add_rows = summary_rows,
             add_columns = cols,
             fmt = "%.2f") |> # 2 significant figures 
  footnote(general = "Standard errors are in parentheses. 
                      Columns 2 and 5 are estimates using the IK bandwidth from rdd package in R.") 


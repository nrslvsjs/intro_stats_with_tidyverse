basketball_data <- read.csv("Stat4Econ/data/Basketball.csv")
summary(basketball_data)
library(tibble)
library(dplyr)
library(readr)
library(ggplot2)
library(knitr)
df_survey <- read_csv("Stat4Econ/data/classsurvey.csv")
glimpse(df_survey)
df_survey[["gender"]] <- as.factor(df_survey[["gender"]])
levels(df_survey$gender)
factor_col_names <- c("gender", "major", "commute", "games.any", "econ")
df_survey[factor_col_names] <- lapply(df_survey[factor_col_names],
                                      as.factor)
str(df_survey)

df_survey %>%
  group_by(gender) %>%
  summarise(frequency.count = n()) %>%
  mutate(proportions = frequency.count / sum(frequency.count))

options(repr.plot.width = 2, repr.plot.height = 2)
bar.plot <- ggplot(df_survey) +
  geom_bar(aes(x = gender)) +
  theme_bw()
print(bar.plot)

categorical.nominal.list <- c("gender",
                              "major",
                              "games.any",
                              "commute",
                              "econ")
dplyr.freq.table <- function(df, cate.var.str){
  print(sprintf("From Dataset: %s, Freq. Table for Variable: %s",
                deparse(substitute(df)), cate.var.str))
  freq.table <- df %>%
    group_by(!!sym(cate.var.str)) %>%
    summarise(frequency.count = n()) %>%
    mutate(proportions = frequency.count / sum(frequency.count))
  return(freq.table)
}

dplyr.freq.table(df = df_survey, cate.var.str = "gender")

for (ctr in seq_along(categorical.nominal.list)) {
  freq.table <- dplyr.freq.table(df = df_survey,
                                 cate.var.str = categorical.nominal.list[ctr])
  print(freq.table)
}

lapply(categorical.nominal.list,
       dplyr.freq.table,
       df = df_survey)

##Compare


ae <- read_sav("F:/¡¡¡‘åŠw/Ž ‰êˆã‘å/¡¡u‹`ŠÖ˜A/‹ž“s•{‘å/u‹`Ž‘—¿/2023_R/##‘æ3‰ñ/Compare/ae.sav")

ae2 <- read_sav("F:/¡¡¡‘åŠw/Ž ‰êˆã‘å/¡¡u‹`ŠÖ˜A/‹ž“s•{‘å/u‹`Ž‘—¿/2023_R/##‘æ3‰ñ/Compare/ae2.sav")

#Github‚©‚ç
ae <- read.csv("https://raw.githubusercontent.com/harabou/Biostat_Kyoto_pref/main/data/%2303/compare/ae.csv")
ae2 <- read.csv("https://raw.githubusercontent.com/harabou/Biostat_Kyoto_pref/main/data/%2303/compare/ae2.csv")


install.packages("arsenal")
library(arsenal)


ae %>%   comparedf(ae2) %>%   summary()

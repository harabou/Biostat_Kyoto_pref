##Compare


ae <- read_sav("F:/‘‘‘εw/ κγε/‘‘u`ΦA/s{ε/u`Ώ/2023_R/##ζ3ρ/Compare/ae.sav")

ae2 <- read_sav("F:/‘‘‘εw/ κγε/‘‘u`ΦA/s{ε/u`Ώ/2023_R/##ζ3ρ/Compare/ae2.sav")

#Github©η
ae <- read.csv("https://raw.githubusercontent.com/harabou/Biostat_Kyoto_pref/main/data/%2303/compare/ae.csv")
ae2 <- read.csv("https://raw.githubusercontent.com/harabou/Biostat_Kyoto_pref/main/data/%2303/compare/ae2.csv")


install.packages("arsenal")
library(arsenal)


ae %>%   comparedf(ae2) %>%   summary()

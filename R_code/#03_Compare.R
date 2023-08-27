##Compare


ae <- read_sav("F:/¡¡¡‘åŠw/Ž ‰êˆã‘å/¡¡u‹`ŠÖ˜A/‹ž“s•{‘å/u‹`Ž‘—¿/2023_R/##‘æ3‰ñ/Compare/ae.sav")

ae2 <- read_sav("F:/¡¡¡‘åŠw/Ž ‰êˆã‘å/¡¡u‹`ŠÖ˜A/‹ž“s•{‘å/u‹`Ž‘—¿/2023_R/##‘æ3‰ñ/Compare/ae2.sav")

install.packages("arsenal")
library(arsenal)


ae %>%   comparedf(ae2) %>%   summary()

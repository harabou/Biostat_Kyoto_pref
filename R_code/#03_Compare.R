##Compare


ae <- read_sav("F:/��������w/����������/�����u�`�֘A/���s�{��/�u�`����/2023_R/##��3��/Compare/ae.sav")

ae2 <- read_sav("F:/��������w/����������/�����u�`�֘A/���s�{��/�u�`����/2023_R/##��3��/Compare/ae2.sav")

install.packages("arsenal")
library(arsenal)


ae %>%   comparedf(ae2) %>%   summary()
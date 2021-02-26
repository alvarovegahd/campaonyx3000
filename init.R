library(tidyverse)

data = read_delim("data/Onys_lml.csv",delim = ";")

data %>%
  ggplot(aes(Despl_total, Cerca_total))+
  geom_point()


data %>%
  group_by(Tratamiento)%>%
  count()
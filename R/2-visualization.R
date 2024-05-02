library(tidyverse)
library(ggplot2)
library(dplyr)
data=read.csv("../data/452000.csv")

# 2.1 užduotis

png(file="../img/1_histogram.png",
    width=600, height=350)
h1=hist(data$avgWage, breaks = 200, main = paste("Vidutinio atlyginimo histograma"),
        xlab = "Atlyginimas", ylab ="Kiekis", col = 'blue')
dev.off()

# 2.2 užduotis

top5=data%>%
  group_by(name)%>%
  summarise(top=max(avgWage))%>%
  arrange(desc(top))%>%
  head(5)
h2=data%>%
  filter(name%in% top5$name)%>%
            mutate(Months=ym(month))%>%
            ggplot(aes(x=Months, y=avgWage, color=name))+geom_line()+labs(title = "5 įmonės, su didžiausiais atlyginimais", x="Mėnesiai", y="Vidutinis atlyginiams")
ggsave("../img/2_dynamic_chart.png",h2, width = 2400, height = 1200, units = ("px"))

# 2.3 Užduotis

insured = data %>%
  filter(name %in% top5$name) %>%
  group_by(name) %>%
  summarise(MostInsured=max(numInsured))%>%
  arrange(desc(MostInsured))

insured$name=factor(insured$name, levels = insured$name[order(insured$MostInsured, decreasing = T)])

h3=insured%>%
  ggplot(aes(x=name, y= MostInsured, fill=name))+geom_col()+labs(title = "Apdraustųjų skaičius", x="Įmonių pavadinimai", y="Apdraustųjų skaičius", fill = "Pavadinimai")+scale_x_discrete(guide = guide_axis(n.dodge = 2))
ggsave("../img/3_diagram.png", h3, width = 2400, height = 1200, units = ("px"))



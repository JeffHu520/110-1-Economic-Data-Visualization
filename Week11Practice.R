library(readr)
library(tidyverse)
library(dplyr)

df <- read_csv("C:/Users/jeff/Desktop/GERD.csv")

df[order(df$value,decreasing = T),] -> df_order
df_order[1:10,] -> df1

df1 %>%
  arrange(desc(df1$value)) %>%
  pull(country) -> levels

df1$country <- factor(df1$country,levels=as.character(levels))

ggplot(
  data = df1
)+
  geom_col(
    mapping = aes(
      x = country,
      y = value
    ),
    width = 0.5
  )+
  geom_line(
    mapping = aes(
      x = country,
      y = percent*100000,
      group = 1
    )
  )+
  geom_point(
    mapping = aes(
      x = country,
      y = percent*100000,
      group = 1
    )
  )+
  geom_text(
    mapping = aes(
      x = country,
      y = (percent+0.1)*100000,
      label = round(percent, digits = 2),
      group = 1
    ),
    hjust = -0.25
  )+
  scale_y_continuous(
    name = "R&D支出(美元)",
    breaks = c(100000,200000,300000,400000,500000),
    label = c("100億","200億","300億","400億","500億"),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = 
      sec_axis(~.*1/100000 ,name = "R&D支出佔GDP百分比(%)")
  )+
  scale_x_discrete(
    name = "國家"
  )+
  labs(
    title = "2015年研發支出前10高的國家",
    caption = "資料來源:OECD Statistics"
  )+
  theme(
    axis.text.x = element_text(vjust = 0.65, angle = 35)
  )


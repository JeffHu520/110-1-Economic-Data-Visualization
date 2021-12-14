library(readr)

df <- read_csv("https://raw.githubusercontent.com/JeffHu520/110-1-Economic-Data-Visualization/main/GERD.csv",
               show_col_types = FALSE
)
df$total_pc <- df$total/df$population
df$government_pc <- df$government/df$population
df$enterprise_pc <- df$enterprise/df$population
df$highedu_pc <- df$highedu/df$population

df[order(df$total_pc,decreasing = T),] -> df_order
df_order[1:10,] -> df1

df1 %>%
  arrange(desc(df1$total_pc)) %>%
  pull(country) -> levels

df1$country <- factor(df1$country,levels=as.character(levels))
ggplot(
  data = df1
)+
  geom_col(
    mapping = aes(
      x = country,
      y = total_pc*1000000,
      fill = "其他"
    ),
    width = 0.7
  )+
  geom_col(
    mapping = aes(
      x = country,
      y = (enterprise_pc+highedu_pc+government_pc)*1000000,
      fill = "企業"
    ),
    width = 0.7
  )+
  geom_col(
    mapping = aes(
      x = country,
      y = (highedu_pc+government_pc)*1000000,
      fill = "高等教育機構"
    ),
    width = 0.7
  )+
  geom_col(
    mapping = aes(
      x = country,
      y = government_pc*1000000,
      fill = "政府"
    ),
    width = 0.7
  )+
  scale_y_continuous(
    name = "平均每人研發支出(美金)",
    breaks = c(500,1000,1500,2000,2500),
    label = c("500","1000","1500","2000","2500")
    ,expand = expansion(mult = c(0,0.01))
  )+
  scale_x_discrete(
    name = "國家",
    breaks = df1$country,
    label = c("新加坡","瑞士","瑞典","美國","奧地利","以色列","南韓","丹麥","台灣","德國")
  )+
  theme(
    panel.background= element_rect(fill = "#DDF3F5"),
    panel.grid.major.y = element_line(colour = "#EAAC7F"),
    plot.caption = element_text(hjust = 1.35),
    axis.title.x = element_text(vjust = -4)
  )+
  scale_fill_manual(
    name = "資金來源",
    values = c("其他"="#F0EBCC",
               "企業"="#74DBEF",
               "高等教育機構"="#0074E4",
               "政府"="#264E86"
    )
  )+
  labs(
    title = "2015年平均每人研發支出前10高的國家",
    caption = "資料來源:OECD Statistics"
  )
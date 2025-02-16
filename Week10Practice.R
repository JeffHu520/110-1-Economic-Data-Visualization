df = list()
df$LuRouFan = data.frame(
    x = c(1 ,1),
    y = c(39 ,117),
    fill = factor(
      c("滷肉飯" ,"魯肉飯" ) ,levels = c("魯肉飯" ,"滷肉飯" )
    )
  )

df$FriedChicken = data.frame(
    x = c(2 ,2),
    y = c(68 ,95),
    fill = factor(
      c("鹹酥雞","鹽酥雞" ) ,levels = c("鹽酥雞","鹹酥雞" )
    )
  )

df$ChickenRice = data.frame(
    x = c(3 ,3),
    y = c(174 ,269),
    fill = factor(
      c("火雞肉飯","雞肉飯") ,levels = c("雞肉飯" ,"火雞肉飯" )
    )
  )
df$GuaBao = data.frame(
    x = c(4 ,4),
    y = c(4 ,23),
    fill = factor(
      c("割包" ,"刈包" ) ,levels = c("刈包" ,"割包" )
    )
  )
df$PotStickers = data.frame(
    x = c(5 ,5),
    y = c(16 ,122),
    fill = factor(
      c("煎餃","鍋貼") ,levels = c("鍋貼" ,"煎餃" )
    )
  )

dx = -0.3 #input$dx
dy = -20 #input$dy
h = 0.2 #input$h

ggplot()+
  geom_col(
    data = df$LuRouFan,
    mapping = aes(
      x = x,
      y = y,
      fill = fill
    ),
    width = 0.5
  )+
  geom_col(
    data = df$FriedChicken,
    mapping = aes(
      x = x,
      y = y,
      fill = fill
    ),
    width = 0.5
  )+
  geom_col(
    data = df$ChickenRice,
    mapping = aes(
      x = x,
      y = y,
      fill = fill
    ),
    width = 0.5
  )+
  geom_col(
    data = df$GuaBao,
    mapping = aes(
      x = x,
      y = y,
      fill = fill
    ),
    width = 0.5
  )+
  geom_col(
    data = df$PotStickers,
    mapping = aes(
      x = x,
      y = y,
      fill = fill
    ),
    width = 0.5
  )+
  geom_text(
    data = df$LuRouFan,
    mapping = aes(
      x = x,
      y = c(dy ,sum(y)+dy),
      label = fill
    ),
    color = "#010d85",
    position = position_dodge(width = 1),
    size = 15 #input$size
  )+
  geom_text(
    data = df$FriedChicken,
    mapping = aes(
      x = x,
      y = c(dy ,sum(y)+dy),
      label = fill
    ),
    color = "#010d85",
    position = position_dodge(width = 1),
    size = 15
  )+
  geom_text(
    data = df$ChickenRice,
    mapping = aes(
      x = x,
      y = c(dy ,sum(y)+dy),
      label = fill
    ),
    color = "#010d85",
    position = position_dodge(width = 1),
    size = 15
  )+
  geom_text(
    data = df$GuaBao,
    mapping = aes(
      x = x,
      y = c(dy ,sum(y)),
      label = fill
    ),
    color = "#010d85",
    position = position_dodge(width = 1),
    size = 15 
  )+
  geom_text(
    data = df$PotStickers,
    mapping = aes(
      x = x,
      y = c(dy ,sum(y)+dy),
      label = fill
    ),
    color = "#010d85",
    position = position_dodge(width = 1),
    size = 15
  )+
  geom_text(
    data = df$LuRouFan,
    mapping = aes(
      x = x+dx,
      y = c(dy ,sum(y)+dy),
      label = y
    ),
    color = "black",
    size = 10
  )+
  geom_text(
    data = df$FriedChicken,
    mapping = aes(
      x = x+dx,
      y = c(dy ,sum(y)+dy),
      label = y
    ),
    color = "black",
    size = 10
  )+
  geom_text(
    data = df$ChickenRice,
    mapping = aes(
      x = x+dx,
      y = c(dy ,sum(y)+dy),
      label = y
    ),
    color = "black",
    size = 10
  )+
  geom_text(
    data = df$GuaBao,
    mapping = aes(
      x = x+dx,
      y = c(dy ,sum(y)),
      label = y
    ),
    color = "black",
    size = 10
  )+
  geom_text(
    data = df$PotStickers,
    mapping = aes(
      x = x+dx,
      y = c(dy ,sum(y)+dy),
      label = y
    ),
    color = "black",
    size = 10
  )+
  coord_flip()+
  theme_void()+
  labs(
    title = "今晚...你吃的是...?",
    subtitle = "常見相似小吃家數比較",
    caption = "經濟部商業司 商工登記公示資料"
  )+
  xlim(1-1, 5+1)+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 50,hjust = 0.5),
    plot.subtitle = element_text(size = 40,hjust = 0.5),
    plot.caption = element_text(size = 25)
  )

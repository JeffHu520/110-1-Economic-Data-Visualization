dataSet1 <- 
  data.frame(
    x=1979:2018
  )
set.seed(2038)
dataSet1$y <- sample(10:40, length(dataSet1$x), T)
ggplot1 <- list()
ticks <- list()
ticks$major <- seq(1980, 2015, by=5)
ticks$minor <- c(1979, 2018)

majorLength = 3 #input$length
minor_majorRatio = 0.7 #input$ratio


ggplot()+
  geom_step(
    data=dataSet1,
    mapping=
      aes(
        x=x,
        y=y
      )
  ) +
  scale_x_continuous(
    breaks={
      breaks = 
        c(1979,seq(1985, 2015, by=5),2018)
      breaks
    },
    labels={
      labels = c(
        "1979", "85", "90", "95", "2000", "05", "10", "15", "18"
      )
      labels}
  ) +
  theme(
    axis.ticks.length.x = unit(0,"mm")
  ) +
  geom_rug(
    mapping=aes(
      x=ticks$major
    ),
    outside=TRUE, # draw rug outside the plot panel
    size=0.5, #input$majorsize
    length=grid::unit(
      majorLength, 
      "mm"
    )
  ) +
  geom_rug(
    mapping=aes(
      x=ticks$minor
    ),
    outside = TRUE,
    size=0.5, #input$minorsize
    length=grid::unit(
      minor_majorRatio*majorLength,
      "mm"
    )
  )+
  coord_cartesian(clip="off")+
  theme(
    axis.text.x = element_text(
      margin = margin(
        12 #input$margin
      ),
      size=16 #input$textSize
    ))

# the same as
ggplot()+
  geom_step(
    data=dataSet1,
    mapping=
      aes(
        x=x,
        y=y
      )
  ) +
  {
    list(
      scale_x_continuous(
        breaks = {
          breaks <-
            c(1979, seq(1985, 2015, by = 5), 2018)
          breaks
        },
        labels = {
          labels <- c(
            "1979", "85", "90", "95", "2000", "05", "10", "15", "18"
          )
          labels
        }
      ),
      theme(
        axis.ticks.length.x = unit(0, "mm")
      ),
      geom_rug(
        mapping = aes(
          x = ticks$major
        ),
        outside = TRUE, # draw rug outside the plot panel
        size = 0.5, # input$majorsize
        length = grid::unit(
          majorLength,
          "mm"
        )
      ),
      geom_rug(
        mapping = aes(
          x = ticks$minor
        ),
        outside = TRUE,
        size = 0.5, # input$minorsize
        length = grid::unit(
          minor_majorRatio * majorLength,
          "mm"
        )
      ),
      coord_cartesian(clip = "off"),
      theme(
        axis.text.x = element_text(
          margin = margin(
            12 # input$margin
          ),
          size = 16 # input$textSize
        )
      )
    )
  }


ggplot1$plot1


ggplot1$plot1  -> # allow drawing outside the plot panel 
  ggplot1$plot2

ggplot1$plot2

ggplot1$plot2 

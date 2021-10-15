library(ggplot2)
library(plotly)
library(dplyr)
library(crosstalk)

# generally speaking, use a "unique" key for filter, 
# especially when you have multiple filters!
 #data structure


tx <- highlight_key(tab3)

gg <- ggplot(tx) + geom_line(aes(date, median, group = city))
filter <- bscols(
  filter_select("id", "Select a city", tx, ~city),
  ggplotly(gg, dynamicTicks = TRUE),
  widths = c(12, 12)
)

tx2 <- highlight_key(txhousing, ~city, "Select a city")
gg <- ggplot(tx2) + geom_line(aes(date, median, group = city))
select <- highlight(
  ggplotly(gg, tooltip = "city"), 
  selectize = TRUE, persistent = TRUE
)

bscols(filter, select)


# £££example 2
cities <- c("Galveston", "Midland", "Odessa", "South Padre Island")
txsmall <- txhousing %>%
  select(city, year, month, median) %>%
  filter(city %in% cities)

txsmall %>%
  highlight_key(~year) %>% {
    ggplot(., aes(month, median, group = year)) + geom_line() +
      facet_wrap(~city, ncol = 2)
  } %>%
  ggplotly(tooltip = "year")

txsmall %>%
  group_by(city) %>%
  do(
    p = highlight_key(., ~year, group = "txhousing-trellis") %>%
      plot_ly(showlegend = FALSE) %>%
      group_by(year) %>%
      add_lines(
        x = ~month, y = ~median, text = ~year,
        hoverinfo = "text"
      ) %>%
      add_annotations(
        text = ~unique(city),
        x = 0.5, y = 1,
        xref = "paper", yref = "paper",
        xanchor = "center", yanchor = "bottom",
        showarrow = FALSE
      )
  ) %>%
  subplot(
    nrows = 2, margin = 0.05,
    shareY = TRUE, shareX = TRUE, titleY = FALSE
  )


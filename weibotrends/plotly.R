library(tidyverse)
library(openxlsx)
library(stringr)
library(plotly)

grps <- read.xlsx("~/Desktop/hs.xlsx") %>%
  pivot_longer(cols = everything()) %>%
  na.omit() %>%
  mutate(topic = gsub("#(.*)#.*", "\\1", value)) %>%
  distinct() %>%
  dplyr::select(tag = name, topic)

dm <- c("龚俊", "张哲瀚")
wz <- c("温客行", "周子舒")
misc <- c("顾湘", "曹蔚宁", "叶白衣", "成岭", "蝎王", "赵敬",
          "周也", "马闻远", "魏哲鸣", "陈紫函", "李岱昆", "王若麟", 
          "山河令", "浪浪钉", "台词", "其他", "其他人")
anti <- setdiff(unique(grps$tag), c(dm, wz, misc))

hs_dm <- grps %>%
  filter(tag %in% c(dm, wz, misc)) %>%
  pull(topic) %>%
  unique()

# write.xlsx(data.frame(topic = hs_dm), file = "~/Desktop/hs_dm.xlsx")
############
extract_value <- function(s){
  x <- str_match_all(s, pattern = "\"(.*)\"")[[1]][, 2]
  l <- length(x)/3
  d <- matrix(x, ncol = 3) %>%
    as.data.frame() %>%
    purrr::set_names(c("datetime", "rank", "popularity"))
}
hs0 <- read.xlsx("~/Desktop/hs_dm.xlsx")

colors_tag <- c("龚俊" = "#f3ba91", #"#FBB4AE",
                "张哲瀚" = "#439490", #"#B3CDE3",
                "温客行" = "#E41A1C",
                "周子舒" = "#377EB8"
)
levels_tag <- names(colors_tag)
dat <- lapply(1:nrow(hs0), function(i){
  d <- extract_value(hs0[["data"]][i]) %>%
    cbind(topic = hs0[["topic"]][i], .)
  }) %>%
  do.call(rbind, .) %>%
  left_join(grps, by = "topic") %>%
  mutate(tag = factor(tag, levels = levels_tag),
         datetime = as.POSIXct(datetime),
         rank = as.integer(rank),
         popularity = as.integer(popularity))


# dat_plt <- dat %>%
#   filter(tag %in% c(dm, wz)) %>%
#   na.omit() %>%
#   mutate(host = "A") %>%
#   arrange(tag, topic, datetime) %>%
#   group_by(tag, topic) %>%
#   mutate(i = cur_group_id()) %>%
#   ungroup() %>%
#   group_by(tag) %>%
#   mutate(showlegend = ifelse(i == min(i), TRUE, NA)) %>%
#   ungroup()
# 
# traces <- dat_plt %>%
#   filter(is.na(showlegend)) %>%
#   pull(i) %>%
#   unique()
#   
# dat_plt %>%
#   plot_ly() %>%
#   add_lines(x = ~ datetime, y = ~ rank, split = ~ topic, 
#             name = ~tag, color = ~ tag, colors = colors_tag, 
#             text = ~paste("<b>", topic, "</b><br>", "主持人:", host, "<br>"),
#             hovertemplate = paste(
#               "%{text}",
#               "排名: %{y}<br>",
#               "时间: %{x}",
#               "<extra></extra>")) %>%
#   style(showlegend = FALSE, traces = traces) %>%
#   layout(
#     xaxis = list(
#       title = "时间",
#       # hovermode = "compare",
#       tickformatstops = list(
#         list(
#           dtickrange = list(NULL, 86400000),
#           value = "%H:%M:%S<br>%Y-%m-%d<br>%a"
#         ),
#         list(
#           dtickrange = list(86400000, 604800000),
#           value = "%Y-%m-%d<br>%a"
#         ),
#         list(
#           dtickrange = list(604800000, "M12"), 
#           value = "%Y-%m"
#         ), 
#         list(
#           dtickrange = list("M12", NULL), 
#           value = "%Y"
#         )
#       ),
#       rangeslider = list(type = "date"),
#       rangeselector = list(
#         buttons = list(
#           list(step = "all", label = "全时段")
#         )
#       )
#     ),
#     yaxis = list(
#       title = "排名",
#       range = c(50, 0)
#     )
#   )
##########
dat_plt <- dat %>%
  filter(tag %in% c(dm, wz)) %>%
  na.omit() %>%
  mutate(host = "A",
         hover_text = paste0("<b>", topic, "</b><br>",
                             "主持人: ", host, "<br>",
                             "排名: ", rank, "<br>",
                             "时间: ", datetime)) 

p <- dat_plt %>%
  ggplot(aes(x = datetime, y = rank, group = topic, color = tag, 
             hover_text = hover_text)) +
  # geom_point(size = 1) + 
  geom_line(alpha = 0.75) +
  scale_y_reverse(limits = c(50, 0),breaks=seq(1,50,3), expand = c(0,0)) +
  scale_color_manual(values = colors_tag) +
  labs(x = "时间", y = "排名") +
  theme_classic() +
  theme(panel.grid.major.y = element_line()#,
  )
pl <- ggplotly(p, dynamicTicks = "x", tooltip = "hover_text") %>%
  layout(
    xaxis = list(
      # hovermode = "compare",
      tickformatstops = list(
        list(
          dtickrange = list(NULL, 86400000),
          value = "%H:%M:%S<br>%Y-%m-%d<br>%a"
        ),
        list(
          dtickrange = list(86400000, 604800000),
          value = "%Y-%m-%d<br>%a"
        ),
        list(
          dtickrange = list(604800000, "M12"), 
          value = "%Y-%m"
        ), 
        list(
          dtickrange = list("M12", NULL), 
          value = "%Y"
        )
      ),
      rangeslider = list(type = "date"),
      rangeselector = list(
        buttons = list(
          list(step = "all", label = "全时段")
        )
      )
    )#,
    # yaxis = list(
    #   # title = "排名",
    #   range = c(50, 0)
    # )
  )

plb <- plotly_build(pl) 
for(i in 1:length(plb$x$data)){
  plb$x$data[[i]]$text <- gsub("hover_text: ", "", plb$x$data[[i]]$text)
}
plb




%>%
  layout(
    xaxis = list(
      title = "时间",
      # hovermode = "compare",
      tickformatstops = list(
        list(
          dtickrange = list(NULL, 86400000),
          value = "%H:%M:%S<br>%Y-%m-%d<br>%a"
        ),
        list(
          dtickrange = list(86400000, 604800000),
          value = "%Y-%m-%d<br>%a"
        ),
        list(
          dtickrange = list(604800000, "M12"), 
          value = "%Y-%m"
        ), 
        list(
          dtickrange = list("M12", NULL), 
          value = "%Y"
        )
      ),
      rangeslider = list(type = "date"),
      rangeselector = list(
        buttons = list(
          list(step = "all", label = "全时段")
        )
      )
    ),
    yaxis = list(
      title = "排名",
      range = c(50, 0)
    )
  )






  
    {
      if (!is.null(TOPIC$full_community)) {
        add_lines(., y = ~all, name = TOPIC$full_community, color = I(ADMINLTE_COLORS$purple))
      } else .
    }%>%
    config(displayModeBar = FALSE) %>%
    layout(
      xaxis = list(
        range = c(now(tz_global()) - days(7), now(tz_global())),
        rangeselector = list(
          buttons = list(
            list(
              count = 1,
              label = "Today",
              step = "day",
              stepmode = "todate"),
            list(
              count = 1,
              label = "Yesterday",
              step = "day",
              stepmode = "backward"),
            list(
              count = 7,
              label = "Week",
              step = "day",
              stepmode = "backward"),
            list(step = "all", label = "All"))),
        rangeslider = list(type = "date")),
      yaxis = list(title = "Tweets"),
      legend = list(orientation = 'h', x = 0.05, y = 0.9),
      hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
    ) %>%
    config(collaborate = FALSE, cloud = FALSE, mathjax = NULL)
})








dat_plt <- dat %>%
  filter(tag %in% c(dm, wz)) %>%
  na.omit() %>%
  mutate(host = "A") %>%
  arrange(tag, topic, datetime) %>%
  group_by(tag, topic) %>%
  mutate(i = cur_group_id()) %>%
  ungroup() %>%
  group_by(tag) %>%
  mutate(showlegend = ifelse(i == min(i), TRUE, NA)) %>%
  ungroup()

traces <- dat_plt %>%
  filter(is.na(showlegend)) %>%
  pull(i) %>%
  unique()

dat_plt %>%
  filter(tag == "龚俊") %>%
  plot_ly() %>%
  add_lines(x = ~ datetime, y = ~ rank, split = ~ topic, 
            name = ~tag, color = ~ tag, colors = colors_tag, 
            text = ~paste("<b>", topic, "</b><br>", "主持人:", host, "<br>"),
            hovertemplate = paste(
              "%{text}",
              "排名: %{y}<br>",
              "时间: %{x}",
              "<extra></extra>"))

  add_lines(x = ~ datetime, y = ~ rank, split = ~ topic, 
            name = ~tag, color = ~ tag, colors = colors_tag, 
            text = ~paste("<b>", topic, "</b><br>", "主持人:", host, "<br>"),
            hovertemplate = paste(
              "%{text}",
              "排名: %{y}<br>",
              "时间: %{x}",
              "<extra></extra>")) %>%
  style(showlegend = FALSE, traces = traces) %>%
  layout(
    xaxis = list(
      title = "时间",
      # hovermode = "compare",
      tickformatstops = list(
        list(
          dtickrange = list(NULL, 86400000),
          value = "%H:%M:%S<br>%Y-%m-%d<br>%a"
        ),
        list(
          dtickrange = list(86400000, 604800000),
          value = "%Y-%m-%d<br>%a"
        ),
        list(
          dtickrange = list(604800000, "M12"), 
          value = "%Y-%m"
        ), 
        list(
          dtickrange = list("M12", NULL), 
          value = "%Y"
        )
      ),
      rangeslider = list(type = "date"),
      rangeselector = list(
        buttons = list(
          list(step = "all", label = "全时段")
        )
      )
    ),
    yaxis = list(
      title = "排名",
      range = c(50, 0)
    )
  )

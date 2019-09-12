library(shiny)

library(dplyr)
library(reshape2)

library(ggplot2)
library(ggthemes)
library(DT)

# prepare data
data.day <- read.csv('data/data_day.csv', encoding='UTF-8')
data.day$date <- as.Date(data.day$date)
data.day$year <- format.Date(data.day$date, '%Y')
data.day$month <- format.Date(data.day$date, '%m')
data.month <- data.day %>% 
                group_by(year, month, country) %>% 
                summarise(sum=sum(value), mean=round(mean(value),2), median=median(value))
data.month$ym <- paste(data.month$year, '-', data.month$month, sep='')
data.month <- ungroup(data.month)

# @param:
#   plot1: 国家曲线图
#   plot2: 月份数据对比图
#   table1: 月份数据对比列表
shinyServer(function(input, output) {
  # -- 国家曲线图 --
  output$plot1 <- renderPlot({
    start_dt <- input$dateRange[1]
    end_dt <- input$dateRange[2]
    if (end_dt > Sys.Date()) {
      end_dt <- Sys.Date()
    }
    country_limit <- input$countries
    
    if (input$dateType == 'day') {
      df <- data.day
      df <- df %>% 
        filter(country %in% country_limit, date >= start_dt & date < end_dt) %>% 
        select(1:3)
      df$country <- countrymap$cname[match(df$country, countrymap$ename)]
      ymax <- max(df$value)
      names(df) <- c('日期', '国家', '用户')
      g <- ggplot(data=df, aes(x=日期, y=用户, group=国家, color=国家)) + 
        geom_line(size=1.1) + 
        theme_bw() + 
        labs(x='') +
        ylim(0, ymax*1.1) + 
        theme(legend.position = 'bottom')
    } else if (input$dateType == 'month') {
      df <- data.month
      start_dt <- format.Date(start_dt, '%Y-%m')
      end_dt <- format.Date(end_dt, '%Y-%m')
      
      df <- df %>% 
        filter(country %in% country_limit, ym >= start_dt,  ym < end_dt) %>% 
        select(c('ym', 'country', 'sum'))
      ymax <- max(df$sum)
      
      df$country <- countrymap$cname[match(df$country, countrymap$ename)]
      names(df) <- c('日期', '国家', '累计用户')
      g <- ggplot(data=df, aes(x=日期, y=累计用户, group=国家, color=国家)) + 
        geom_line(size=1.1) + 
        geom_point(size=2) +
        theme_bw() + 
        labs(x='') +
        ylim(0, ymax * 1.1) +
        theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
    }
    g
  }) # - output$plot1
  
   # -- 月份数据对比-柱形图 --
  output$plot2 <- renderPlot({
    stat_func <- input$statFunc # 统计量参数
    date1 <- paste(input$year, '-', input$month, sep='')  # 选择月份
    date2 <- paste(input$year2, '-', input$month2, sep='')  # 对比月份
    dt <- data.month %>%
      filter(ym %in% c(date1, date2)) %>% 
      arrange(desc(sum), desc(ym))
    dt <- dt %>% select(c('ym', 'country', stat_func))
    dt$country <- countrymap$cname[match(dt$country, countrymap$ename)]
    names(dt) <- c('月份', '国家', 'value')
    g <- ggplot(data=dt, aes(x=国家, y=value, fill=月份)) +
          geom_col(position='dodge') +
          theme_bw() +
          geom_text(aes(label=value), vjust=-0.5, position=position_dodge(0.9)) +
          labs(x='', y=stat_vlist[stat_func]) +
          theme(legend.position = 'top')
    g
  }) # - output$plot2
  
  # -- 月份数据对比-表格 --
  output$table1 <- DT::renderDataTable(DT::datatable(
    rownames = FALSE,
    options = list(
      ordering=FALSE,
      paging=FALSE,
      searching=FALSE),
    {
      date1 <- paste(input$year, '-', input$month, sep='')
      date2 <- paste(input$year2, '-', input$month2, sep='')
      dt <- data.month %>%
        filter(ym %in% c(date1, date2)) %>% 
        arrange(desc(sum), desc(ym))
      dt$country <- countrymap$cname[match(dt$country, countrymap$ename)]
      dt <- dt %>% select(c('ym', 'country', 'sum', 'mean', 'median'))
      names(dt) <- c('月份', '国家', '求和', '均值', '中位数')
      dt
    }
  )) # - output$table1
}) # - shiny-server
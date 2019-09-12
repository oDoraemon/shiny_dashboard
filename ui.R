library(shiny)
library(shinydashboard)
library(DT)

# 参数初始化部分
date_end <- Sys.Date()

# ---- 页面布局 ---------
header <- dashboardHeader(title='Chenyongle demo Dashboard')
sidebar <- dashboardSidebar(disable=TRUE)

body <- dashboardBody(
  
  # 外部js/css资源
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  # 参数+用户曲线
  fluidRow(
     box(width=3,
         title='参数选择',
         status='danger',
         solidHeader = TRUE,
         radioButtons('dateType', label='时间粒度',
                      choices=dateType, selected='day'),
         dateRangeInput('dateRange', label='时间区间',
                        start='2017-01-1',
                        end=date_end),
         checkboxGroupInput('countries', label='国家',
                            choices=country_list, selected='JP'),
         radioButtons('forecastType', label='预测模型',
                      choices=model_list, selected=0)
    ),
     box(title = '国家用户曲线',
         width=9,
         solidHeader = TRUE,
         plotOutput('plot1'))
  ),
  # 月份对比数据
  fluidRow(
    box(
      width=4,
      collapse=TRUE,
      column(6, 
             selectInput('year',
                         label = '年份:',
                         year_list,
                         selected = format.Date(Sys.Date(), '%Y')
             )),
      column(6, 
             selectInput('month',
                         label = '月份:',
                         month_list,
                         selected = format.Date(Sys.Date(), '%m')
                         )),
      DT::dataTableOutput('table1')
    ),
    box(
      width = 6,
      column(2,
        selectInput('statFunc',
                     label = '统计量',
                     stat_list,
                     selected = 'sum')
      ),
      column(12, plotOutput('plot2'))
    ),
    box(
      width = 2,
      title = '对比月份',
      solidHeader = TRUE,
      status='info',
      column(12,
             selectInput('year2',
                         label = '年份:',
                         year_list,
                         selected = format.Date(Sys.Date(), '%Y')
             )
      ),
      column(12,
             selectInput('month2',
                         label = '月份:',
                         month_list,
                         selected = format.Date(Sys.Date(), '%m')
             )
      )
    )
  )
)

shinyUI(
  dashboardPage(header, sidebar, body, skin='red')
)
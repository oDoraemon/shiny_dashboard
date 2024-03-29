# R Shiny Dashboard Demo

## 简介  
R-shiny是R语言下的web框架，可以快速实现业务数据分析应用。  
框架提供了良好的数据可视化组件，能轻易实现出色的页面交互，达到类似Tableau、FineBI等数据分析套件的效果。  
这里是一个简单的R-shiny应用，由用户业务曲线+业务数据月份对比两部分组成，展示了简单的数据分析与可视化。  
国家用户数据是随机生成的fake数据，与现实业务无关。  
![app capture](https://github.com/oDoraemon/shiny_dashboard/blob/master/shiny-dashboard-snapshot.JPG?raw=true)  


## 文件结构  
* data/: 数据文件夹。后期可能会用sqlite替代  
* www/: 自定义js/css文件。必须放在此文件夹下才能生效。 
* global.R: server/ui端共用的数据配置  
* server.R: 后端逻辑实现  
* ui.R: 前端页面布局  

## $todo:  
* 实现时间序列预测模型及效果展示  
* 月份数据对比-对比月份选择的事件绑定
* 国家曲线颜色固定    
* 优化配置数据与结构。引入sqlite做数据存储  
* 优化代码结构, 抽象处理逻辑  

# A R Interface to [ECharts 3.0](https://github.com/ecomfe/echarts)


**This package is <font color=red>under development</font>, more features will be added.**

**这个包目前在<font color=red>开发中</font>。**


## Installation

```r
devtools::install_github('ChanningWong/REcharts3')

```

## Examples

```r
library(REcharts3)
```


### Bar Plot

```r
dat1 = aggregate(weight ~ feed, data = chickwts, mean)
bar(dat1, feed, weight, label = round(weight, 0), title = 'test')

dat2 = aggregate(breaks ~ wool + tension, data = warpbreaks, mean)
bar(dat2, wool, breaks, tension, label = round(breaks*10, 0), title = 'test')

his(esoph, agegp, ncontrols, alcgp, facets = tobgp, title = 'Data Set Esoph')

```

![Bar Plot 1](screenshots/barplot1.png)

![Bar Plot 2](screenshots/barplot2.png)

![Bar Plot 3](screenshots/barplot3.png)


### Line Plot

```r
line(airquality[1:20, ], paste0(Month, '-', Day), Temp, title = 'airquality')
```

![Line Plot 1](screenshots/lineplot1.png)



### Pie Plot

```r
dat1 = aggregate(weight ~ feed, data = chickwts, mean)
pie(dat1, feed, weight, label = round(weight*10, 0), title = 'Pie Plot')
donut(dat1, feed, weight, title = 'Pie Plot')
```

![Pie Plot 1](screenshots/pieplot1.png)
![Pie Plot 2](screenshots/pieplot2.png)



### Scatter Plot

```r
warpbreaks$x1 = runif(nrow(warpbreaks))
warpbreaks$y1 = runif(nrow(warpbreaks))
scatter(warpbreaks, x1, y1, wool, paste0('tension: ', tension), title = 'Scatter Plot')
```

![Scatter Plot 1](screenshots/scatterplot1.png)



### Mixed Plot

```r
library(tidyr)
y = runif(10, min = 1, max = 10)
dat = data.frame(x = LETTERS[1:10])
dat$proportion = y / sum(y)
dat$CumulativeProportion = cumsum(dat$proportion)
dat2 = gather(dat, key, value, -x)

p = his(dat2, x, value, key, label = percent(value, 0), 
        title = 'test', label.show = T, label.position = 'top', yAxis.max = 1)
p2 = p %>% addSecAxis(series = 'CumulativeProportion', type = 'line', yAxis.max = 1)
p2
```

![Pie Plot 1](screenshots/mixedplot1.png)



### Map Plot
```r
dat1 = data.frame(
  n = 1:10,
  type = rep(c('day1', 'day2'), each = 5),
  rbind(c(120.210813,27.321733), c(120.220813,27.321733), c(120.230813,27.351733),
        c(120.220813,27.351733), c(120.230813,27.311733), c(120.215,27.322),
        c(120.220,27.323), c(120.230,27.350), c(120.220,27.352), c(120.230,27.312)))
names(dat1) = c('n', 'type', 'lng', 'lat')

mapLines(dat1, 'lng', 'lat', 'type', title = '行程',
        center = c(dat1$lng[1], dat1$lat[1]), color = c('red', 'blue'),
         effect = list(show = T, constantSpeed = 100, symbol = 'circle', trailLength = 0.2, symbolSize = 5))
```

![Map Plot 1](screenshots/mapplot1.png)





## Shiny APP

```r

library(REcharts3)
library(shiny)

shinyApp(

  ui = fluidPage(
    tags$script(src = 'http://echarts.baidu.com/dist/echarts.min.js'),
    h2('REcharts3'),
    uiOutput('plot1'),
    uiOutput('plot2')
  ),
  
  server = function(input, output) {
    
    output$plot1 <- renderREcharts3({
      dat = aggregate(weight ~ feed, data = chickwts, mean)
      pie(dat, feed, weight, title = 'title', height = 400)
    })
    
    output$plot2 <- renderREcharts3({
      dat1 = aggregate(weight ~ feed, data = chickwts, mean)
      dat3 = data.frame(let = letters[1:5], id = 1:5, stringsAsFactors = F)
      
      p = donut(dat1, feed, weight, title = 'test')
      p0 = pie(dat3, let, id, title = 'test', chart.radius = '30%')
      p@option$series[[2]] = p0@option$series[[1]]
      p@option$legend$data = c(p0@option$legend$data, p@option$legend$data)
      p
    })
    
  }
)

```

![shiny 1](screenshots/shiny1.png)







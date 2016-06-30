# A R Interface to [ECharts 3.0](https://github.com/ecomfe/echarts)


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

![Bar Plot 1](screenshots/barplot1.PNG)

![Bar Plot 2](screenshots/barplot2.PNG)

![Bar Plot 3](screenshots/barplot3.PNG)


### Line Plot

```r
line(airquality[1:20, ], paste0(Month, '-', Day), Temp, title = 'airquality')
```

![Line Plot 1](screenshots/lineplot1.PNG)



### Pie Plot

```r
dat1 = aggregate(weight ~ feed, data = chickwts, mean)
pie(dat1, feed, weight, label = round(weight*10, 0), title = 'title')
donut(dat1, feed, weight, title = 'title')
```

![Pie Plot 1](screenshots/pieplot1.PNG)
![Pie Plot 2](screenshots/pieplot2.PNG)




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

![Pie Plot 1](screenshots/mixedplot1.PNG)









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

![shiny 1](screenshots/shiny1.PNG)







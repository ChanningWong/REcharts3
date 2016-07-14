
.round2 <- function(x, n = 1, numeric = T) {
  posneg <- sign(x)
  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  if(numeric) z*posneg else format(z*posneg,  nsmall = n)
}


percent <- function(x, n = 1){
  m <- paste('%.', n, 'f', sep = '')
  paste(sprintf(m, .round2(100 * x, n, numeric = T)), '%', sep='')
}



rmNULL <- function(x) {
  is.nullElement <- function(x) is.null(x) | all(sapply(x, is.null))
  x = Filter(Negate(is.nullElement), x)
  lapply(x, function(x) if (is.list(x)) rmNULL(x) else x)
}



.rev = function(x) x[length(x):1]

.plotColor = c('#c23531','#2f4554', '#61a0a8', '#d48265', '#91c7ae','#749f83',  '#ca8622', '#bda29a','#6e7074', '#546570', '#c4ccd3')

.guessLabelLength = function(){}

.selectElement = function(x, path){
  if(is.integer(path) | is.numeric(path)){
    f = function(x, i) if(i <= length(x)) `[[`(x, i)
  } else if(is.character(path)){
    f = function(x, i) if(i %in% names(x)) `[[`(x, i)
  } else f = function(x, i) NULL
  Reduce(f, c(list(x), as.list(path)))
}


`.selectElement<-` = function(x, path, value){
  e = .selectElement(x, path)
  if(!is.null(e)){
    s = if(is.character(path)) '\'' else ''
    expr = paste0('x', paste0('[[', s, path, s, ']]', collapse = ''), ' = value')
    eval(parse(text = expr))
    x
  } else stop('path is not valided')
}




# NA?
.pickLevels = function(x, droplevels = T){
  if('factor' %in% class(x)){
    if(droplevels) out = levels(droplevels(x)) else out = levels(x)
  } else {
    out = unique(x)
  } 
  out
}






addSecAxis = function(p, series, type, yAxis.max = NULL){
  zt.series = sapply(p@option$series, `[[`, 'name')
  zt.i = which(zt.series == series)
  p@option$series[[zt.i]]$type = type
  p@option$series[[zt.i]]$yAxisIndex = p@option$series[[zt.i]]$yAxisIndex + 1
  p@option$yAxis[[2]] = p@option$yAxis[[1]]
  if(!is.null(yAxis.max)) p@option$yAxis[[2]]$max = yAxis.max
  p
}

`+.REcharts3` = function(p, fun, ...){
  fun(p, ...)
}

# id = NULL; width = NULL; height = NULL
.makeDom = function(p, id = NULL, width = NULL, height = NULL){
  if(is.null(id)) id = p@id
  if(is.null(width) & p@plotOption$width > 0) width = p@plotOption$width
  if(is.null(height) & p@plotOption$height > 0) height = p@plotOption$height
  json = RJSONIO::toJSON(p@option, pretty = T)
  
  option = gsub('"formatFunction_label"', p@formatFunction_label, json)
  option = gsub('"formatFunction_tooltip"', p@formatFunction_tooltip, option)
  option = gsub('"formatFunction_symbolSize"', p@formatFunction_symbolSize, option)
  
  var = paste0('echart_', id)
  size = if(!is.null(height)) paste0('height:', height, 'px;') else paste0('height:100%;')
  size = if(!is.null(width)) paste0(size, 'width:', width, 'px;') else paste0(size, 'width:100%;')
  
  paste0('<div class="container-fluid">
         <div id="', id, '" style="', size, ';border:1px solid #ccc;padding:10px;"></div>
         <script type="text/javascript" src="echarts.min.js"></script>
         <script type="text/javascript" src="bmap.min.js"></script>
         <script type="text/javascript" src="http://api.map.baidu.com/api?v=2.0&ak=ZUONbpqGBsYGXNIYHicvbAbM"></script>
         <script type="text/javascript">var ', var, ' = echarts.init(document.getElementById(\'', id, '\'));
         ', var, '.setOption(', option, ');
          window.onresize = function () { ', var, '.resize(); }
         </script>'
  )
  
}
# <script src="http://echarts.baidu.com/dist/echarts.min.js"></script>


.makeHtml = function(dom){
  html = sprintf(
    '<!DOCTYPE html>
      <head>
        <meta charset="utf-8">
        <title>ECharts</title>
      </head>
    <body>%s</body>', dom)
  if(.Platform$OS.type == 'windows') html = iconv(html, 'gbk', 'utf8')
  html
}




# wd = getwd()
# setwd('E:/R/Function/REcharts3')  
# setwd(wd)
plot.REcharts3 = function(p, width = NULL, height = NULL, 
                          id = NULL, viewer = F, encoding = 'GBK', type = 'map'){
  
  plotDir = tempdir()
  if (!file.exists(plotDir)) dir.create(plotDir, recursive = TRUE)
  fileDir = paste0(plotDir, '/tmp.html')
  url = sprintf("http://localhost:%s/session/%s", tools:::httpdPort(), basename(fileDir))
  
  html = .makeDom(p, id = id, width = width, height = height)
  con = file(fileDir, 'w', encoding = encoding)
  writeLines(html, con, useBytes = F)
  close(con)
  
  file.copy(system.file('js/echarts.min.js', package = 'REcharts3'), plotDir, recursive = TRUE)
  if(type == 'map') file.copy(system.file('js/bmap.min.js', package = 'REcharts3'), plotDir, recursive = TRUE)
  
  if(!is.null(getOption('viewer')) & viewer) rstudio::viewer(url) else browseURL(url)

}


print.REcharts3 = plot.REcharts3



# shinyOuput.REcharts3 = function(p, width = 800, height = 400, id = NULL){
#   
#   if(is.null(id)) id = p@id
#   json = RJSONIO::toJSON(p@option, pretty = T)
#   option = gsub('"formatFunction_label"', p@formatFunction_label, json)
#   option = gsub('"formatFunction_tooltip"', p@formatFunction_tooltip, option)
#   
#   var = paste0('echart_', id)
#   dom = sprintf('
#                 <div class="container-fluid">
#                 <div id="%s" style="height:%spx;width:%spx;border:0px;padding:10px;"></div>
#                 <script type="text/javascript">var %s = echarts.init(document.getElementById(\'%s\'));
#                 %s.setOption(%s);</script>
#                 </div>', id, height, width, var, id, var, option) # border:1px solid #ccc
#   htmltools::HTML(dom)
# }


renderREcharts3 <- function(expr, env = parent.frame(), quoted = FALSE) 
{
  
  func <- shiny::exprToFunction(expr, env, quoted)
  function(){
    p <- func()
    
    id = p@id
    width = ifelse(p@plotOption$width > 0, p@plotOption$width, 600) 
    height = ifelse(p@plotOption$height > 0, p@plotOption$height, 350)
    
    json = RJSONIO::toJSON(p@option, pretty = T)
    
    option = gsub('"formatFunction_label"', p@formatFunction_label, json)
    option = gsub('"formatFunction_tooltip"', p@formatFunction_tooltip, option)
    option = gsub('"formatFunction_symbolSize"', p@formatFunction_symbolSize, option)
    
    var = paste0('echart_', id)
    size = if(!is.null(height)) paste0('height:', height, 'px;') else paste0('height:100%;')
    size = if(!is.null(width)) paste0(size, 'width:', width, 'px;') else paste0(size, 'width:100%;')
    
#     paste0('<div class="container-fluid">
#          <div id="', id, '" style="', size, ';border:1px solid #ccc;padding:10px;"></div>
#          <script src="echarts.min.js"></script>
#          <script type="text/javascript">var ', var, ' = echarts.init(document.getElementById(\'', id, '\'));
#          ', var, '.setOption(', option, ');
#           window.onresize = function () { ', var, '.resize(); }
#          </script>'
#     )
    
    
    
    var = paste0('echart_', id)
    dom = sprintf('
                <div class="container-fluid">
                <div id="%s" style="height:%spx;width:%spx;border:0px;padding:10px;"></div>
                <script type="text/javascript">var %s = echarts.init(document.getElementById(\'%s\'));
                %s.setOption(%s);</script>
                </div>', id, height, width, var, id, var, option) # border:1px solid #ccc
    # htmltools::HTML(dom)
    htmltools::HTML(dom)
  }
}

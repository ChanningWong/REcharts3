
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



`+.REcharts3` = function(p, fun, ...){
  fun(p, ...)
}




.makeDom = function(p, id = NULL, width = NULL, height = NULL){
  
  if(is.null(id)) id = p@id
  if(is.null(width) & p@plotOption$width > 0) width = p@plotOption$width
  if(is.null(height) & p@plotOption$height > 0) height = p@plotOption$height
  json = RJSONIO::toJSON(p@option, pretty = T)
  
  option = gsub('"formatFunction_label"', p@formatFunction_label, json)
  option = gsub('"formatFunction_tooltip"', p@formatFunction_tooltip, option)
  option = gsub('"formatFunction_symbolSize"', p@formatFunction_symbolSize, option)
  
  domName = paste0('echart_', id)
  size = if(!is.null(height)) paste0('height:', height, 'px;') else paste0('height:100%;')
  size = if(!is.null(width)) paste0(size, 'width:', width, 'px;') else paste0(size, 'width:100%;')
  
  paste0('
         <div id="', domName, '" style="', size, ';border:0px solid #ccc;padding:10px;"></div>
         <script type="text/javascript">var ', id, ' = echarts.init(document.getElementById(\'', domName, '\'));
         ', id, '.setOption(', option, ');
         window.onresize = function () { ', id, '.resize(); }
         </script>'
         )
}



.makeHtml = function(dom, plotMap = F){
  
  if(plotMap){
    mapjs = '<script src="bmap.min.js"></script>
        <script type="text/javascript" src="http://api.map.baidu.com/api?v=2.0&ak=ZUONbpqGBsYGXNIYHicvbAbM"></script>
    '
  } else mapjs = ''
  # <script src="http://echarts.baidu.com/dist/echarts.min.js"></script>
  html = paste0(
'<!DOCTYPE html>
  <html  style = "height:100%; width:100%;">
    <head>
      <meta charset="utf-8">
        <script src="echarts.min.js"></script>', 
        mapjs, '
      <title>ECharts</title>
    </head>
    <body style = "height:100%; width:100%;">
      ', dom, '
    </body>
 </html>')
  
  html
}



plot.REcharts3 = print.REcharts3 = function(
  p, width = NULL, height = NULL, id = NULL, viewer = F, encoding = 'UTF-8'
){
  
  if(is.null(id)) id = p@id
  if(is.null(width) & p@plotOption$width > 0) width = p@plotOption$width
  if(is.null(height) & p@plotOption$height > 0) height = p@plotOption$height
  html = .makeHtml(.makeDom(p, id = id, width = width, height = height), 
                   p@type %in% c('mapLines', 'mapHeatmap'))
  
  plotDir = tempdir()
  if (!file.exists(plotDir)) dir.create(plotDir, recursive = TRUE)
  fileDir = paste0(plotDir, '/', id, '.html')
  con = file(fileDir, 'w', encoding = encoding)
  writeLines(html, con, useBytes = F)
  close(con)
  
  file.copy(system.file('js/echarts.min.js', package = 'REcharts3'), plotDir, recursive = TRUE)
  if(p@type %in% c('mapLines', 'mapHeatmap')) file.copy(system.file('js/bmap.min.js', package = 'REcharts3'), plotDir, recursive = TRUE)
  
  url = sprintf("http://localhost:%s/session/%s", tools:::httpdPort(), basename(fileDir))
  if(!is.null(getOption('viewer')) & viewer) rstudio::viewer(url) else browseURL(url)
  
}



plotMultipleREcharts3 = function(
  p, width = NULL, height = NULL, id = NULL, viewer = F, encoding = 'UTF-8'
){
  
  if(is.null(id)) id = p@id
  if(is.null(width) & p@plotOption$width > 0) width = p@plotOption$width
  if(is.null(height) & p@plotOption$height > 0) height = p@plotOption$height
  html = .makeHtml(.makeDom(p, id = id, width = width, height = height), p@type == 'mapLines')
  
  plotDir = tempdir()
  if (!file.exists(plotDir)) dir.create(plotDir, recursive = TRUE)
  fileDir = paste0(plotDir, '/', id, '.html')
  con = file(fileDir, 'w', encoding = encoding)
  writeLines(html, con, useBytes = F)
  close(con)
  
  file.copy(system.file('js/echarts.min.js', package = 'REcharts3'), plotDir, recursive = TRUE)
  if(p@type %in% c('mapLines', 'mapHeatmap')) file.copy(system.file('js/bmap.min.js', package = 'REcharts3'), plotDir, recursive = TRUE)
  
  url = sprintf("http://localhost:%s/session/%s", tools:::httpdPort(), basename(fileDir))
  if(!is.null(getOption('viewer')) & viewer) rstudio::viewer(url) else browseURL(url)
  
}



renderREcharts3 <- function(expr, env = parent.frame(), quoted = FALSE){
  
  func <- shiny::exprToFunction(expr, env, quoted)
  function(){
    p = func()
    id = p@id
    if(p@plotOption$width > 0) width = p@plotOption$width else width = 600
    if(p@plotOption$height > 0) height = p@plotOption$height else height = 350
    dom = .makeDom(p, id = id, width = width, height = height)
    htmltools::HTML(dom)
  }
}



RECharts3Output = function (outputId, inline = FALSE, container = if(inline) span else div,
                           ...){
  suppressMessages(singleton(
    addResourcePath('js', system.file('js', package = 'REcharts3'))
  ))
  div(id = outputId, class = 'shiny-html-output',
      tagList(
        singleton(tags$head(tags$script(src = 'js/echarts.min.js', type = "text/javascript")))
      )
  )
}



incluedRECharts3 = function(local = F){
  js = ifelse(local, 
              system.file('js/echarts.min.js', package = 'REcharts3'),
              "http://echarts.baidu.com/dist/echarts.min.js" )
  htmltools::tagList(
    htmltools::tag("script",list(
      type='text/javascript', src = js
    ))
  )
}



RECharts3Knit = function(p, id = NULL, width = 800, height = 400){
  div = .makeDom(p, id = id, width = width, height = height)
  htmltools::HTML(div)
}






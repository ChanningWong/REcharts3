

setLayer = function(dat, x, y, z = NULL, label = NULL, legend = NULL,
                    facets = NULL, ncol = NULL, nrow = NULL, facets.fontSize = 14, facets.top = 6,
                    type = 'bar', stack = F, color = .plotColor,
                    title = NULL, title.fontSize = 18, title.top = 0, title.left = 'left',
                    label.show = F, label.position = 'inside', 
                    tooltip.show = T, 
                    grid.left = NULL, grid.top = NULL, grid.right = NULL, grid.bottom = NULL, grid.margin.x = 5, grid.margin.y = 5, 
                    legend.show = T, legend.left = 'center', legend.top = '6%', legend.orient = c('horizontal', 'vertical'),
                    legend.right = NULL, legend.bottom = NULL, legend.width = NULL, legend.height = NULL,
                    yAxis.max = 'auto',
                    xAxis.inverse = F, axisLabel.interval.x = NULL, axisLabel.interval.y = NULL,
                    width = NULL, height = NULL,
                    ...){
  
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  
  d = eval(expr, parent.frame())
  if(is.numeric(d$z)) d$z = as.character(d$z)
  if(is.numeric(d$facets)) d$facets = as.character(d$facets)
  
  if(is.null(grid.top)){ 
    if(is.null(d$facets)) grid.top = 10 else grid.top = 16
  }
  if(is.null(d$label)) label.show = F
  
  
  xLevels = .rev(.pickLevels(d$x))
  if(is.null(d$z)){
    if(is.null(legend)) legendName = list(as.character(parList$x)) else legendName = legend
    color = color[1]
  } else {
    legendName = .pickLevels(d$z)
    color = rep(color, ceiling(length(legendName)/length(color)))[1:length(legendName)]
  }
  if(is.null(d$facets)){
    facetsName = NULL
  } else {
    facetsName = .pickLevels(d$facets)
  }
  
  
  
  optionList = list()
  
  # seriesSet
  if(is.null(d$facets)){

    optionList$series = .setSeries(d, xLevels = xLevels, 
                                   type = type, name = legendName, stack = stack, color = color,
                                   label.show = label.show, label.position = label.position)
  } else {
    f0 = split(d, d$facets)
    series0 = lapply(f0, .setSeries, xLevels = xLevels, 
                     type = type, name = legendName, stack = stack, color = color,
                     label.show = label.show, label.position = label.position)
    names(series0) = NULL
    for(i in 1:length(series0)){ # i = 1
      if(stack){
        series0[[i]] = lapply(series0[[i]], `[[<-`, 'stack', facetsName[i])
      }
      series0[[i]] = lapply(series0[[i]], `[[<-`, 'xAxisIndex', i - 1)
      series0[[i]] = lapply(series0[[i]], `[[<-`, 'yAxisIndex', i - 1)
    }
    optionList$series = do.call(c, series0)
  }
  
  
  # legendSet
  optionList$legend = .legendSet(data = legendName,
                                 legend.show = legend.show,
                                 legend.left = legend.left, legend.top = legend.top,
                                 legend.right = legend.right, legend.bottom = legend.bottom,
                                 legend.width = legend.width, legend.height = legend.height,
                                 legend.orient = legend.orient[1])
  
  
  # gridSet
  if(is.null(d$facets) & is.null(grid.left) & is.null(grid.top) & is.null(grid.right) & is.null(grid.bottom)){
    gridSet = NULL
  } else if(!is.null(d$facets)){ 
    gridSet = .gridSet_facets(length(facetsName), ncol = ncol, nrow = nrow, 
                              grid.left = grid.left, grid.top = grid.top, 
                              grid.right = grid.right, grid.bottom = grid.bottom,
                              grid.margin.x = grid.margin.x, grid.margin.y = grid.margin.y)
  } else {
    gridSet = .gridSet(grid.left = grid.left, grid.top = grid.top, 
                       grid.right = grid.right, grid.bottom = grid.bottom)
  }
  optionList$grid = gridSet
  
  # titleSet
  if(!is.null(title)){
    optionList$title = list(list(text = title, fontSize = title.fontSize, 
                                 top = title.top, left = title.left))
  }
  if(!is.null(d$facets)){
    g = attr(gridSet, 'grid')
    addTitle = mapply(function(ir, it, x){ # ir = i.grid[1 ,1]; it = i.grid[1 ,2]
      o = list(left = ir, top = it, text = x, fontSize = facets.fontSize)
      o[1:2] = lapply(o[1:2], paste0, '%')
      o
    }, g[ ,1], g[ ,2] - facets.top, facetsName, SIMPLIFY = F, USE.NAMES = F)
    optionList$title = c(optionList$title, addTitle)
  }
  
  optionList$tooltip = list(show = tooltip.show, formatter = 'formatFunction_tooltip')
  
  # Axis
  optionList$xAxis =  list()
  optionList$yAxis =  list()
  for(i in 1:length(facetsName)){
    if(i < 1) next
    optionList$xAxis[[i]] = list(gridIndex = i - 1, data = xLevels, 
                                 axisLabel = list(interval = axisLabel.interval.x),
                                 inverse = xAxis.inverse)
    optionList$yAxis[[i]] = list(gridIndex = i - 1, 
                                 axisLabel = list(interval = axisLabel.interval.y),
                                 max = yAxis.max)
  }
  names(optionList$xAxis) = NULL
  names(optionList$yAxis) = NULL
  
  
  p = new("REcharts3")
  p@id = paste('ID', format(Sys.time(), "%Y%m%d%H%M%S"), substring(runif(1), 3, 5), sep = '_')
  p@id = gsub('\\..*', '', p@id)
  p@option = optionList
  p@width = ifelse(!is.null(width), width, 0) 
  p@height = ifelse(!is.null(height), height, 0) 
  p@formatFunction_label = 'function(params){return params.data.label}'
  p@formatFunction_tooltip = 'function(params){return params.name + \':<br>\' + params.seriesName + \':\' + params.data.label}'
  p
}
# p = setLayer(dat1, feed, weight, label = round(weight, 1))
# p = setLayer(dat2, wool, breaks, tension, label = breaks*10)





his = function(...){
  expr = match.call()
  parList = as.list(expr[-1])
  expr[[1]] = as.name('setLayer')
  expr[['xAxis.inverse']] = T
  p = eval(expr, parent.frame())
  for(i in 1:length(p@option$series)) p@option$series[[i]]$type = 'bar'
  p
} 




bar = function(dat, x, y, z = NULL, label = NULL, facets = NULL, stack = F,
               title = NULL, barGap = 10, axisLabel.interval.y = 0,
               label.position = 'inside', 
               legend.show = T, legend.left = 'center', legend.top = '6%', legend.orient = c('horizontal', 'vertical'),
               legend.right = NULL, legend.bottom = NULL, legend.width = NULL, legend.height = NULL,
               tooltip.show = T, ...){
  expr = match.call()
  parList = as.list(expr[-1])
  expr[[1]] = as.name('setLayer')
  p = eval(expr, parent.frame())
  
  for(i in 1:length(p@option$series)){ 
    p@option$series[[i]]$type = 'bar'
    p@option$series[[i]]$barGap = paste0(barGap, '%')
  }
  
  coord_rotate(p)
} 





line = function(dat, x, y, z = NULL, label = NULL, facets = NULL, stack = F,
                title = NULL,
                label.position = 'top', 
                tooltip.show = T, ...){
  expr = match.call()
  parList = as.list(expr[-1])
  expr[[1]] = as.name('setLayer')
  p = eval(expr, parent.frame())
  
  for(i in 1:length(p@option$series)) p@option$series[[i]]$type = 'line'
  
  p
} 





setLayer.pie = function(dat, x, y, label = NULL, facets = NULL, 
                        type = 'pie', 
                        title = NULL, title.fontSize = 18, title.top = 0, title.left = 'left',
                        label.position = 'outside', 
                        chart.radius = '50%', chart.position = c('50%', '60%'),
                        tooltip.show = T, 
                        legend.show = T, legend.left = 'center', legend.top = '7%', legend.orient = c('horizontal', 'vertical'),
                        legend.right = NULL, legend.bottom = NULL, legend.width = NULL, legend.height = NULL,
                        width = NULL, height = NULL,
                        ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  d = eval(expr, parent.frame())
  
  if(!is.null(parList$label)) label.show = T else label.show = F
  xLevels = .pickLevels(d$x)
  
  legendName = list(as.character(parList$x))
  series = list(.setSeriesData.pie(d, 
                                   type = type, name = legendName, label.show = label.show, label.position = label.position,
                                   chart.radius = chart.radius, chart.position = chart.position))
  
  optionList = list(
    tooltip = list(show = tooltip.show, 
                   formatter = '{a} <br/>{b} : {c} ({d}%)'),
    series = series
  )
  optionList$title = list(list(text = title, fontSize = title.fontSize, 
                               top = title.top, left = title.left))
  optionList$legend = .legendSet(data = xLevels,
                                 legend.show = legend.show,
                                 legend.left = legend.left, legend.top = legend.top,
                                 legend.right = legend.right, legend.bottom = legend.bottom,
                                 legend.width = legend.width, legend.height = legend.height,
                                 legend.orient = legend.orient[1])
  
  p = new("REcharts3")
  p@id = paste('ID', format(Sys.time(), "%Y%m%d%H%M%S"), substring(runif(1), 3, 5), sep = '_')
  p@id = gsub('\\..*', '', p@id)
  p@width = ifelse(!is.null(width), width, 0) 
  p@height = ifelse(!is.null(height), height, 0) 
  p@option = optionList
  p@formatFunction_label = 'function(params){return params.data.label}'
  p@formatFunction_tooltip = '' # function(params){return params.name + \':<br>\' + params.seriesName + \':\' + params.data.label}'
  p
}





pie = function(dat, x, y, label = NULL, facets = NULL, stack = F,
               title = NULL,
               label.position = 'outside', 
               chart.radius = '70%', chart.position = c('50%', '55%'),
               tooltip.show = T, ...){
  
  expr = match.call()
  parList = as.list(expr[-1])
  expr[[1]] = as.name('setLayer.pie')
  p = eval(expr, parent.frame())
  
  p
} 


donut = function(dat, x, y, label = NULL, facets = NULL, stack = F,
                 title = NULL,
                 label.position = 'outside', 
                 chart.radius = c('40%', '60%'), chart.position = c('50%', '55%'),
                 tooltip.show = T, ...){
  
  expr = match.call()
  parList = as.list(expr[-1])
  expr[[1]] = as.name('setLayer.pie')
  
  p = eval(expr, parent.frame())
  
  p@option$series[[1]]$radius = chart.radius
  p
}










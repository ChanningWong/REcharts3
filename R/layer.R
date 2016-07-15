# type = 'heatmap'
# ncol = NULL; nrow = NULL; facets.fontSize = 14; facets.top = 6;
# label = NULL; label.show = F; label.position = 'inside'; 
# opacity = 0.7; symbolSize = 'formatFunction_symbolSize'
# tooltip.show = T; 
# type = 'bar'; stack = F; color = .plotColor;
# title = NULL; title.fontSize = 18; title.top = 0; title.left = 'left';
# subtext = NULL; subTitle.fontSize = 14;subTitle.color = '#888',
# legend = NULL; legend.show = T; legend.orient = c('horizontal', 'vertical');
# legend.left = 'center'; legend.top = '6%'; 
# legend.right = NULL; legend.bottom = NULL; legend.width = NULL; legend.height = NULL;
# grid.left = NULL; grid.top = NULL; grid.right = NULL; grid.bottom = NULL; grid.margin.x = 5; grid.margin.y = 5; 
# yAxis.max = NULL;
# xAxis.inverse = F; axisLabel.interval.x = NULL; axisLabel.interval.y = NULL;
# toolbox.show = F; dataZoom.show = T; dataView.show = T; dataView.readOnly = T; restore.show = T; saveAsImage.show = T;
# width = NULL; height = NULL;
# chart.radius = '70%'; chart.position = c('50%', '55%')
# draggable = T;repulsion = 200;gravity = 0.1;edgeLength = 50;layoutAnimation = F;focusNodeAdjacency = F

setLayer = function(dataList, type = 'bar', 
                    ..., 
                    stack = F, color = .plotColor, opacity = 1, symbolSize = 10,
                    ncol = NULL, nrow = NULL, facets.fontSize = 14, facets.top = 6,
                    label = NULL, label.show = F, label.position = 'inside', 
                    tooltip.show = T, 
                    title = NULL, title.fontSize = 18, title.top = 0, title.left = 'left',
                    subTitle = NULL, subTitle.fontSize = 14, subTitle.color = '#888',
                    legend = NULL, legend.show = T, legend.orient = c('horizontal', 'vertical'),legend.left = 'center', legend.top = '5.5%', 
                    legend.right = NULL, legend.bottom = NULL, legend.width = NULL, legend.height = NULL,
                    grid.left = NULL, grid.top = NULL, grid.right = NULL, grid.bottom = NULL, grid.margin.x = 5, grid.margin.y = 5, 
                    xAxis.min = NULL, xAxis.max = NULL, xAxis.inverse = F,
                    yAxis.min = NULL, yAxis.max = NULL, yAxis.inverse = F,
                    axisLabel.interval.x = NULL, axisLabel.interval.y = NULL,
                    toolbox.show = F, dataZoom.show = T, dataView.show = T, dataView.readOnly = T, restore.show = T, saveAsImage.show = T,
                    width = NULL, height = NULL){
  
  
  optionList = list()
  
  # seriesSet
  optionList$series = .setSeries(dataList, 
                                 type = type, color = color, stack = stack,
                                 label.show = label.show, label.position = label.position, 
                                 opacity = opacity, symbolSize = symbolSize,
                                 ...)
  
  # legendSet
  if(type != 'heatmap'){
    legendData = if(type != 'pie') dataList@seriesName else dataList@xLevelsName
    optionList$legend = .legendSet(data = legendData,
                                   legend.show = legend.show,
                                   legend.left = legend.left, legend.top = legend.top,
                                   legend.right = legend.right, legend.bottom = legend.bottom,
                                   legend.width = legend.width, legend.height = legend.height,
                                   legend.orient = legend.orient[1])
  }
  
  # gridSet
  if(is.null(grid.top)){ 
    if(!'facets' %in% dataList@var) grid.top = 10 else grid.top = 16
  }
  
  if(!'facets' %in% dataList@var & is.null(grid.left) & is.null(grid.top) & is.null(grid.right) & is.null(grid.bottom)){
    gridSet = NULL
  } else if('facets' %in% dataList@var){ 
    gridSet = .gridSet_facets(length(dataList@facetsName), ncol = ncol, nrow = nrow, 
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
                                 top = title.top, left = title.left,
                                 subtext = subTitle, 
                                 subtextStyle = list(fontSize = subTitle.fontSize, color = subTitle.color)
                                 ))
  }
  if('facets' %in% dataList@var){
    g = attr(gridSet, 'grid')
    addTitle = mapply(function(ir, it, x){ # ir = i.grid[1 ,1]; it = i.grid[1 ,2]
      o = list(left = ir, top = it, text = x, fontSize = facets.fontSize)
      o[1:2] = lapply(o[1:2], paste0, '%')
      o
    }, g[ ,1], g[ ,2] - facets.top, as.list(dataList@facetsName), SIMPLIFY = F, USE.NAMES = F)
    optionList$title = c(optionList$title, addTitle)
  }
  

  
  # Axis
  if(type %in% c('bar', 'his', 'line', 'scatter', 'heatmap')){
    optionList$xAxis =  list()
    optionList$yAxis =  list()
    for(i in 1:length(dataList@ facetsName)){
      if(i < 1) next
      optionList$xAxis[[i]] = list(gridIndex = i - 1, 
                                   min =xAxis.min, max = xAxis.max, inverse = xAxis.inverse,
                                   axisLabel = list(interval = axisLabel.interval.x),
                                   inverse = xAxis.inverse)
      if(type %in% c('line', 'bar', 'his', 'heatmap')) optionList$xAxis[[i]]$data = dataList@xLevelsName
      optionList$yAxis[[i]] = list(gridIndex = i - 1, 
                                   min = yAxis.min, max = yAxis.max, inverse = yAxis.inverse,
                                   axisLabel = list(interval = axisLabel.interval.y),
                                   max = yAxis.max)
      if(type %in% c('heatmap')) optionList$yAxis[[i]]$data = dataList@yLevelsName
    }
    names(optionList$xAxis) = NULL
    names(optionList$yAxis) = NULL
  }
  
  
  optionList$tooltip = list(show = tooltip.show, formatter = 'formatFunction_tooltip')
  optionList$toolbox = .toolboxSet(toolbox.show = toolbox.show, 
                                   dataZoom.show = dataZoom.show, 
                                   dataView.show = dataView.show, dataView.readOnly = dataView.readOnly,
                                   restore.show = restore.show, 
                                   saveAsImage.show = saveAsImage.show)
  
  
  
  p = new("REcharts3")
  p@id = paste('ID', format(Sys.time(), "%y%m%d%H%M%S"), substring(runif(1), 3, 5), type, sep = '_')
  p@id = gsub('\\..*', '', p@id)
  p@type = type
  p@option = rmNULL(optionList)
  p@xLevelsName = dataList@xLevelsName
  p@yLevelsName = dataList@yLevelsName
  p@seriesName = dataList@seriesName
  p@facetsName = dataList@facetsName
  p@plotOption = list(width = ifelse(!is.null(width), width, 0),
                      height = ifelse(!is.null(height), height, 0))
  if(type %in% c('line', 'bar', 'his', 'graph', 'heatmap')){
    p@formatFunction_label = 'function(params){return params.data.label}'
    p@formatFunction_tooltip = 'function(params){return params.name + \':<br>\' + params.seriesName + \' : \' + params.data.label}'
  } else if(type == 'pie'){
    p@formatFunction_label = '\\"{b}: {c} ({d}%)\\"'
    p@formatFunction_tooltip = '\\"{a} <br/>{b}: {c} ({d}%)\\"'
  } else if(type %in% c('scatter', 'lines')){
    p@formatFunction_label = 'function(params){return params.data.label}'
    p@formatFunction_tooltip = 'function(params){return params.seriesName + \' : \' + params.data.label}'
    p@formatFunction_symbolSize = 'function (data){ return data[2]; }'
  }
  
  p
}
# p = setLayer(dat1, feed, weight, label = round(weight, 1))
# p = bar(dat2, wool, breaks, tension, label = breaks*10)




bar = function(dat, x, y, z = NULL, facets = NULL, label = NULL, 
               label.show = F, barGap = '10%', legend.left = 'center', ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'bar')
  
  if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'bar', xAxis.inverse = T,
               label.show = label.show, barGap = barGap, legend.left = legend.left, ...)

  coord_rotate(p)
}




his = function(dat, x, y, z = NULL, facets = NULL, label = NULL, 
               label.show = F, barGap = '10%', legend.left = 'center', ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'bar')
  
  if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'bar', label.show = label.show, barGap = barGap, legend.left = legend.left, ...)
  p
}




line = function(dat, x, y, z = NULL, facets = NULL, label = NULL, 
                label.show = F, legend.left = 'center', ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'line')
  
  if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'line', label.show = label.show, legend.left = legend.left, ...)
  p
}




scatter = function(dat, x, y, z = NULL, facets = NULL, label = NULL, size = NULL,
                   label.show = F, legend.left = 'center', opacity = 0.7, ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  expr[['type']] = 'scatter'
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'scatter')
  
  if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'scatter', 
               label.show = label.show, legend.left = legend.left, 
               opacity = opacity, ...)
  p
}




pie = function(dat, x, y, facets = NULL, label = NULL, 
               label.show = T, label.position = 'outside', 
               chart.radius = '70%', chart.position = c('50%', '55%'), 
               ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'pie')
  
  # if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'pie', 
               label.show = label.show,
               label.position = label.position,
               radius = chart.radius,
               center = chart.position,
               ...)
  p
}




donut = function(dat, x, y, facets = NULL, label = NULL, 
                 label.show = T, label.position = 'outside', 
                 chart.radius = c('40%', '60%'), chart.position = c('50%', '55%'),
                 ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'pie')
  
  # if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'pie', 
               label.show = label.show,
               label.position = label.position,
               radius = chart.radius,
               center = chart.position,
               ...)
  p
}




force = function(dat, x, y, z = NULL, facets = NULL, label = NULL, 
                 draggable = T, repulsion = 200, gravity = 0.1, edgeLength = 50, layoutAnimation = T,
                 focusNodeAdjacency = F,
                ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  parList[['type']] = 'graph'
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'graph')
  
  # if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'graph', layout = 'force', 
               draggable = draggable, focusNodeAdjacency = focusNodeAdjacency,
           force = list(repulsion = repulsion, gravity = gravity, 
                        edgeLength = edgeLength, layoutAnimation = layoutAnimation), 
           ...)
  p
}




mapLines = function(dat, x, y, z = NULL, label = NULL, 
                    center = NULL, zoom = 14, line.width = 0.1,
                    label.show = F, legend.left = 'center', ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'lines')
  
  if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'lines', label.show = label.show, legend.left = legend.left, 
               ..., 
               coordinateSystem = 'bmap', polyline = T, lineStyle = list(width = line.width))
  p@option$bmap = .setBmap(center, zoom)
  p
}



markScatter = function(p, dat, x, y, z, color = .plotColor[1]){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  if(is.null(dat$z)) dat$z = NA
  
  toList_markScatter = function(d)(
    mapply(function(x, y, z){
      list(name = z, value = c(x, y))
    }, 
    d$x, d$y, d$z, 
    SIMPLIFY = F, USE.NAMES = F)
  )
  
  m = length(p@option$series) + 1
  p@option$series[[m]] = list(
    type = 'scatter',
    coordinateSystem = 'bmap',
    data = toList_markScatter(dat),
    label = list(
      normal = list(show = T, position = 'inside', formatter = '{b}')
    ),
    itemStyle = list(normal = list(color = color, size = 10))
  )
  p
}



markPoint = function(p, dat, x, y, z, color = .plotColor[1], seriesIndex = 1){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  if(is.null(dat$z)) dat$z = NA
  
  toList_markPoint = function(d)(
    mapply(function(x, y, z){
      list(name = z, coord = c(x, y))
    }, 
    d$x, d$y, d$z, 
    SIMPLIFY = F, USE.NAMES = F)
  )
  
  p@option$series[[seriesIndex]]$markPoint = list(
    data = toList_markPoint(dat),
    label = list(
      normal = list(show = T, position = 'inside', formatter = '{b}')
    ),
    itemStyle = list(normal = list(color = color))
  )
  p
}


markAxisLine = function(p, dat, x, y, type = 'xAxis', 
                        color = .plotColor[1], seriesIndex = 1){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  if(is.null(dat$z)) dat$z = NA
  
  if(type == 'xAxis'){
    ds = mapply(function(x, y){ 
      list(xAxis = x, label = y)
    }, dat$x, dat$y, SIMPLIFY = F, USE.NAMES = F)
  } else {
    ds = mapply(function(x, y){ 
      list(yAxis = x, label = y)
    }, dat$x, dat$y, SIMPLIFY = F, USE.NAMES = F)
  }
  
  markLine = list(data = ds, 
                  lineStyle = list(normal = list(color = color)),
                  label = list(normal = list(formatter = 'formatFunction_label')))
  
  p@option$series[[seriesIndex]]$markLine = markLine
  p
}


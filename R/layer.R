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
# xAxis.inverse = F; xAxis.axisLabel.interval = NULL; yAxis.axisLabel.interval = NULL;
# toolbox.show = F; dataZoom.show = T; dataView.show = T; dataView.readOnly = T; restore.show = T; saveAsImage.show = T;
# width = NULL; height = NULL;
# chart.radius = '70%'; chart.position = c('50%', '55%')
# draggable = T;repulsion = 200;gravity = 0.1;edgeLength = 50;layoutAnimation = F;focusNodeAdjacency = F
# xAxis.name = NULL; xAxis.nameRotate = NULL, xAxis.nameLocation = 'middle'; xAxis.nameGap = 30;
# yAxis.name = NULL; yAxis.nameRotate = NULL, yAxis.nameLocation = 'middle'; yAxis.nameGap = 30;


setLayer = function(dataList, type = 'bar', 
                    ..., 
                    stack = F, color = .plotColor, opacity = 1, symbolSize = 10,
                    ncol = NULL, nrow = NULL, facets.fontSize = 14, facets.top = 6,
                    label = NULL, label.show = NULL, label.position = 'inside', 
                    label.fontColor = NULL, 
                    label.fontStyle = 'normal',
                    label.fontWeight = 'normal',
                    label.fontFamily = 'sans-serif',
                    label.fontSize = 12,
                    
                    tooltip.show = T, 
                    title = NULL, title.fontSize = 18, title.top = 0, title.left = 'left',
                    subTitle = NULL, subTitle.fontSize = 14, subTitle.color = '#888',
                    legend = NULL, legend.show = T, legend.orient = c('horizontal', 'vertical'),legend.left = 'center', legend.top = '5.5%', 
                    legend.right = NULL, legend.bottom = NULL, legend.width = NULL, legend.height = NULL,
                    grid.left = NULL, grid.top = NULL, grid.right = NULL, grid.bottom = NULL, grid.margin.x = 5, grid.margin.y = 5, 
                    
                    xAxis.min = NULL, xAxis.max = NULL, xAxis.inverse = F,
                    xAxis.name = NULL, xAxis.nameRotate = NULL, xAxis.nameLocation = 'middle', xAxis.nameGap = 30,
                    
                    xAxis.axisLine.show = T, 
                    xAxis.axisLine.onZero = T, 
                    xAxis.axisLine.lineStyle = list(color = '#666', width =  1, type = 'solid', shadowBlur = NULL, shadowColor = NULL, shadowOffsetX = 0, shadowOffsetY = 0, opacity = NULL),
                    
                    xAxis.axisTick.show = T, 
                    xAxis.axisTick.alignWithLabel = F,
                    xAxis.axisTick.interval = 'auto',
                    xAxis.axisTick.inside = F,
                    xAxis.axisTick.length = 5,
                    xAxis.axisTick.lineStyle = list(color = '#666', width =  1, type = 'solid', shadowBlur = NULL, shadowColor = NULL, shadowOffsetX = 0, shadowOffsetY = 0, opacity = NULL),
                    
                    xAxis.axisLabel.show = T, 
                    xAxis.axisLabel.interval = 'auto',
                    xAxis.axisLabel.inside = F,
                    xAxis.axisLabel.rotate = 0,
                    xAxis.axisLabel.margin = 8,
                    xAxis.axisLabel.formatter = NULL,
                    xAxis.axisLabel.textStyle = list(color = NULL, fontStyle = 'normal', fontWeight = 'normal', fontFamily = 'sans-serif', fontSize = 12),
                    
                    xAxis.splitLine.show = T,
                    xAxis.splitLine.interval = 'auto',
                    xAxis.splitLine.lineStyle = list(color = '#EEE9E9', width =  1, type = 'solid', shadowBlur = NULL, shadowColor = NULL, shadowOffsetX = 0, shadowOffsetY = 0, opacity = NULL),
                    
                    yAxis.min = NULL, yAxis.max = NULL, yAxis.inverse = F,
                    yAxis.name = NULL, yAxis.nameRotate = NULL, yAxis.nameLocation = 'middle', yAxis.nameGap = 30,
                    
                    
                    yAxis.axisLine.show = T, 
                    yAxis.axisLine.onZero = T, 
                    yAxis.axisLine.lineStyle = list(color = '#666', width =  1, type = 'solid', shadowBlur = NULL, shadowColor = NULL, shadowOffsetX = 0, shadowOffsetY = 0, opacity = NULL),
                    
                    yAxis.axisTick.show = T, 
                    yAxis.axisTick.alignWithLabel = F,
                    yAxis.axisTick.interval = 'auto',
                    yAxis.axisTick.inside = F,
                    yAxis.axisTick.length = 5,
                    yAxis.axisTick.lineStyle = list(color = '#666', width =  1, type = 'solid', shadowBlur = NULL, shadowColor = NULL, shadowOffsetX = 0, shadowOffsetY = 0, opacity = NULL),
                    
                    yAxis.axisLabel.show = T, 
                    yAxis.axisLabel.interval = 'auto',
                    yAxis.axisLabel.inside = F,
                    yAxis.axisLabel.rotate = 0,
                    yAxis.axisLabel.margin = 8,
                    yAxis.axisLabel.formatter = NULL,
                    yAxis.axisLabel.textStyle = list(color = '#666', fontStyle = 'normal', fontWeight = 'normal', fontFamily = 'sans-serif', fontSize = 12),
                    
                    yAxis.splitLine.show = T,
                    yAxis.splitLine.interval = 'auto',
                    yAxis.splitLine.lineStyle = list(color = '#EEE9E9', width =  1, type = 'solid', shadowBlur = NULL, shadowColor = NULL, shadowOffsetX = 0, shadowOffsetY = 0, opacity = NULL),
                    
                    
                    toolbox.show = F, dataZoom.show = T, dataView.show = T, dataView.readOnly = T, restore.show = T, saveAsImage.show = T,
                    width = NULL, height = NULL){
  
  # 
  if(is.null(grid.top)){ 
    if(!'facets' %in% dataList@var) grid.top = 10 else grid.top = 16
  }
  if(is.null(label.show)) if('label' %in% dataList@var) label.show = T else label.show = F
  
  
  optionList = list()
  
  # seriesSet
  optionList$series = .setSeries(dataList, 
                                 type = type, color = color, stack = stack,
                                 label.show = label.show, 
                                 label.position = label.position, 
                                 label.fontColor = label.fontColor, 
                                 label.fontStyle = label.fontStyle,
                                 label.fontWeight = label.fontWeight,
                                 label.fontFamily = label.fontFamily,
                                 label.fontSize = label.fontSize,
                                 opacity = opacity, symbolSize = symbolSize,
                                 ...)

  
  if('facets' %in% dataList@var & type %in% c('pie')){
    centerSet = .pieCenterSet(length(optionList$series), ncol = ncol, nrow = nrow, 
                              grid.left = grid.left, grid.top = grid.top, 
                              grid.right = grid.right, grid.bottom = grid.bottom,
                              grid.margin.x = grid.margin.x, grid.margin.y = grid.margin.y)

    for(i in 1:length(optionList$series)){ 
      optionList$series[[i]]$center = paste0(centerSet[i, ], '%')
    }
  }
  
  # legendSet
  if(!type %in% c('heatmap', 'mapHeatmap')){
    legendData = if(type != 'pie') dataList@seriesName else dataList@xLevelsName
    optionList$legend = .legendSet(data = legendData,
                                   legend.show = legend.show,
                                   legend.left = legend.left, legend.top = legend.top,
                                   legend.right = legend.right, legend.bottom = legend.bottom,
                                   legend.width = legend.width, legend.height = legend.height,
                                   legend.orient = legend.orient[1])
  }
  
  # gridSet
  if(!type %in% c('mapHeatmap', 'mapScatter')){
    
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
  }
  
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
      o = list(left = ir, top = it, text = x, textStyle = list(fontSize = facets.fontSize))
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
                                   name = xAxis.name,
                                   nameRotate = xAxis.nameRotate,
                                   nameLocation = xAxis.nameLocation,
                                   nameGap = xAxis.nameGap,
                                   min = xAxis.min, 
                                   max = xAxis.max, 
                                   inverse = xAxis.inverse,
                                   axisLine = list(show = xAxis.axisLine.show, 
                                                   onZero = xAxis.axisLine.onZero, 
                                                   lineStyle = xAxis.axisLine.lineStyle),
                                   axisTick = list(show = xAxis.axisTick.show, 
                                                   alignWithLabel = xAxis.axisTick.alignWithLabel,
                                                   interval = xAxis.axisTick.interval,
                                                   inside = xAxis.axisTick.inside,
                                                   length = xAxis.axisTick.length,
                                                   lineStyle = xAxis.axisTick.lineStyle),
                                   axisLabel = list(show = xAxis.axisLabel.show, 
                                                    interval = xAxis.axisLabel.interval,
                                                    inside = xAxis.axisLabel.inside,
                                                    rotate = xAxis.axisLabel.rotate,
                                                    margin = xAxis.axisLabel.margin,
                                                    formatter = xAxis.axisLabel.formatter,
                                                    textStyle = xAxis.axisLabel.textStyle),
                                   splitLine = list(show = xAxis.splitLine.show,
                                                    interval = xAxis.splitLine.interval,
                                                    lineStyle = xAxis.splitLine.lineStyle))
      if(type %in% c('line', 'bar', 'his', 'heatmap')) optionList$xAxis[[i]]$data = dataList@xLevelsName
      optionList$yAxis[[i]] = list(gridIndex = i - 1, 
                                   name = yAxis.name,
                                   nameRotate = yAxis.nameRotate,
                                   nameLocation = yAxis.nameLocation,
                                   nameGap = yAxis.nameGap,
                                   min = yAxis.min,
                                   max = yAxis.max, 
                                   inverse = yAxis.inverse,
                                   axisLine = list(show = yAxis.axisLine.show, 
                                                   onZero = yAxis.axisLine.onZero, 
                                                   lineStyle = yAxis.axisLine.lineStyle),
                                   axisTick = list(show = yAxis.axisTick.show, 
                                                   alignWithLabel = yAxis.axisTick.alignWithLabel,
                                                   interval = yAxis.axisTick.interval,
                                                   inside = yAxis.axisTick.inside,
                                                   length = yAxis.axisTick.length,
                                                   lineStyle = yAxis.axisTick.lineStyle),
                                   axisLabel = list(show = yAxis.axisLabel.show, 
                                                    interval = yAxis.axisLabel.interval,
                                                    inside = yAxis.axisLabel.inside,
                                                    rotate = yAxis.axisLabel.rotate,
                                                    margin = yAxis.axisLabel.margin,
                                                    formatter = yAxis.axisLabel.formatter,
                                                    textStyle = yAxis.axisLabel.textStyle),
                                   splitLine = list(show = yAxis.splitLine.show,
                                                    interval = yAxis.splitLine.interval,
                                                    lineStyle = yAxis.splitLine.lineStyle))
      if(type %in% c('heatmap')) optionList$yAxis[[i]]$data = dataList@yLevelsName
    }
    names(optionList$xAxis) = NULL
    names(optionList$yAxis) = NULL
  }
  
  
  if(!type %in% c('mapHeatmap', 'mapScatter')){
    optionList$tooltip = list(show = tooltip.show, formatter = 'formatFunction_tooltip')
    optionList$toolbox = .toolboxSet(toolbox.show = toolbox.show, 
                                     dataZoom.show = dataZoom.show, 
                                     dataView.show = dataView.show, dataView.readOnly = dataView.readOnly,
                                     restore.show = restore.show, 
                                     saveAsImage.show = saveAsImage.show)
  }
  
  
  p = new("REcharts3")
  p@id = paste(type, format(Sys.time(), "%y%m%d%H%M%S"), substring(runif(1), 3, 5), sep = '_')
  p@id = gsub('\\..*', '', p@id)
  p@type = type
  p@option = rmNULL(optionList)
  p@xLevelsName = dataList@xLevelsName
  p@yLevelsName = dataList@yLevelsName
  p@seriesName = dataList@seriesName
  p@facetsName = dataList@facetsName
  p@plotOption = list(width = ifelse(!is.null(width), width, 0),
                      height = ifelse(!is.null(height), height, 0))
  if(type %in% c('line', 'bar', 'his', 'heatmap')){
    p@formatFunction_label = 'function(params){return params.data.label}'
    p@formatFunction_tooltip = 'function(params){return params.name + \':<br>\' + params.seriesName + \' : \' + params.data.label}'
  } else if(type == 'pie'){
    p@formatFunction_label = '\\"{b}: {c} ({d}%)\\"'
    p@formatFunction_tooltip = '\\"{a} <br/>{b}: {c} ({d}%)\\"'
  } else if(type %in% c('scatter', 'lines')){
    p@formatFunction_label = 'function(params){return params.data.label}'
    p@formatFunction_tooltip = 'function(params){return params.seriesName + \' : \' + params.data.label}'
    p@formatFunction_symbolSize = 'function (data){ return data[2]; }'
  } else if(type %in% c('graph')){
    p@formatFunction_label = 'function(params){return params.data.label}'
    # p@formatFunction_tooltip = 'function(params){return params.data.name + \' : \' + params.data.label}'
  }
  
  p
}
# p = setLayer(dat1, feed, weight, label = round(weight, 1))
# p = bar(dat2, wool, breaks, tension, label = breaks*10)




bar = function(dat, x, y, z = NULL, facets = NULL, label = NULL, 
               stack = F, label.position = NULL,
               barGap = '10%', legend.left = 'center', na.value = '(NA)', ...){
  
  if(is.null(label.position)) if(stack) label.position = 'inside' else label.position = 'right'
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dat = .fillNA(dat, c('x', 'z', 'facets'), na.value = na.value)
  dataList = .dataList(dat, type = 'bar')
  
  p = setLayer(dataList, type = 'bar', stack = stack, 
               xAxis.inverse = T, label.position = label.position,
               barGap = barGap, legend.left = legend.left, ...)

  coord_rotate(p)
}




his = function(dat, x, y, z = NULL, facets = NULL, label = NULL, 
               stack = F, label.position = NULL,
               barGap = '10%', legend.left = 'center', na.value = '(NA)', ...){
  
  if(is.null(label.position)) if(stack) label.position = 'inside' else label.position = 'top'
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dat = .fillNA(dat, c('x', 'z', 'facets'), na.value = na.value)
  dataList = .dataList(dat, type = 'bar')
  
  p = setLayer(dataList, type = 'bar', stack = stack, 
               label.position = label.position, 
               barGap = barGap, legend.left = legend.left, ...)
  p
}




line = function(dat, x, y, z = NULL, facets = NULL, label = NULL, label.position = 'top',
                legend.left = 'center', na.value = '(NA)', ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dat = .fillNA(dat, c('x', 'z', 'facets'), na.value = na.value)
  dataList = .dataList(dat, type = 'line')
  
  p = setLayer(dataList, type = 'line', label.position = label.position, legend.left = legend.left, ...)
  p
}




scatter = function(dat, x, y, z = NULL, facets = NULL, label = NULL, size = NULL,
                   legend.left = 'center', opacity = 0.7,
                   xAxis.name = as.character(substitute(x)),
                   yAxis.name = as.character(substitute(x)),
                   ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  expr[['type']] = 'scatter'
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'scatter')
  
  p = setLayer(dataList, type = 'scatter', legend.left = legend.left, 
               opacity = opacity, xAxis.name = xAxis.name, yAxis.name = yAxis.name,
               ...)
  p
}




pie = function(dat, x, y, facets = NULL, label = NULL, 
               label.show = T, label.position = 'outside', 
               chart.radius = NULL, chart.position = c('50%', '55%'),
               na.value = '(NA)',
               ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dat = .fillNA(dat, c('x', 'facets'), na.value = na.value)
  dataList = .dataList(dat, type = 'pie')
  
  # if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'pie', 
               label.show = label.show,
               label.position = label.position,
               radius = chart.radius,
               center = chart.position,
               ...)
  
  if(is.null(chart.radius)){
    if(!'facets' %in% dataList@var){ 
      chart.radius = '70%' 
    } else { 
      chart.radius = paste0(90/length(dataList@facetsName), '%')
    }
  }
  for(i in 1:length(p@option$series)){ 
    p@option$series[[i]]$radius = chart.radius
  }
  
  p
}




donut = function(dat, x, y, facets = NULL, label = NULL, 
                 label.show = T, label.position = 'outside', 
                 chart.radius = NULL, chart.position = c('50%', '55%'),
                 na.value = '(NA)',
                 ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dat = .fillNA(dat, c('x', 'facets'), na.value = na.value)
  dataList = .dataList(dat, type = 'pie')
  
  # if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'pie', 
               label.show = label.show,
               label.position = label.position,
               radius = chart.radius,
               center = chart.position,
               ...)
  
  if(is.null(chart.radius)){
    if(!'facets' %in% dataList@var){ 
      chart.radius = c('40%', '60%')
    } else { 
      chart.radius = paste0(c(60/length(dataList@facetsName), 90/length(dataList@facetsName)), '%')
    }
  }
  for(i in 1:length(p@option$series)){ 
    p@option$series[[i]]$radius = chart.radius
  }
  
  p
}




graph = function(dat, x, y, z = NULL, facets = NULL, label = NULL, category = NULL,
                 layout = 'force', color = .plotColor,
                 draggable = T, symbol = 'circle', symbolSize = 10, 
                 repulsion = 50, gravity = 0.1, edgeLength = 30, 
                 layoutAnimation = T,
                 focusNodeAdjacency = F,
                 ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  parList[['type']] = 'graph'
  dat = eval(expr, parent.frame())
  
  if(is.null(category)){
    idx = unique(c(.pickLevels(dat$x), .pickLevels(dat$y)))
  } else {
    idx = .pickLevels(category$name)
    idx2 = match(category$category[match(category$name, idx)], .pickLevels(category$category))  - 1
  }
  dat$x = match(dat$x, idx) - 1
  dat$y = match(dat$y, idx) - 1
  
  dataList = .dataList(dat, type = 'graph')
  p = setLayer(dataList, type = 'graph', layout = layout, 
               symbol = symbol, symbolSize = symbolSize, 
               draggable = draggable, focusNodeAdjacency = focusNodeAdjacency,
               force = list(repulsion = repulsion, gravity = gravity, 
                            edgeLength = edgeLength, layoutAnimation = layoutAnimation), 
               ...)
  
  if(is.null(category)){
    p@option$series[[1]]$data = lapply(idx, function(x) list(name = x))
    p@option$legend$data = NULL
  } else {
    if(is.null(category$label)){
      p@option$series[[1]]$data = mapply(function(x, y, z) list(name = x, category = y, label = '') , 
                                         idx, idx2, SIMPLIFY = F, USE.NAMES = F)
    } else {
      p@option$series[[1]]$data = mapply(function(x, y, z) list(name = x, category = y, label = z) , 
                                         idx, idx2, category$label, SIMPLIFY = F, USE.NAMES = F)
    }
    p@option$series[[1]]$categories = mapply(function(x, y){ 
      list(name = x, itemStyle = list(normal = list(color = y)))
    },
    .pickLevels(category$category), 
    color[1:length(unique(category$category))],
    SIMPLIFY = F, USE.NAMES = F)
    p@option$legend$data = .pickLevels(category$category)
  }
  p@option$tooltip$formatter = NULL
  p@option$series[[1]]$itemStyle = NULL
  p
  
}




mapLines = function(dat, x, y, z = NULL, label = NULL, 
                    center = NULL, zoom = 14, mapStyle = 'normal', 
                    line.width = 3,
                    label.show = F, legend.left = 'center', ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'lines')
  
  if(!is.null(expr$label) & is.null(expr$label)) label.show = T
  if(is.null(center)){
    cd = dataList@data[[1]]
    center = c(cd$x[!is.na(cd$x) & !is.na(cd$y)][1], cd$y[!is.na(cd$x) & !is.na(cd$y)][1])
  }
  p = setLayer(dataList, type = 'lines', label.show = label.show, legend.left = legend.left, 
               ..., 
               coordinateSystem = 'bmap', polyline = T, lineStyle = list(normal = list(width = line.width)))
  p@option$series = lapply(p@option$series, function(x){
    d = x$data[[1]]$coords
    if(length(d) == 1) x$data[[1]]$coords[2] = x$data[[1]]$coords[1]
    x
  })
  
  for(i in 1:length(p@option$series)){
    p@option$series[[i]]$data[[1]]$coords = lapply(p@option$series[[i]]$data[[1]]$coords, as.character)
  }
  p@option$bmap = .setBmap(center, zoom, mapStyle)
  p@type = 'mapLines'
  p
}




mapScatter = function(dat, x, y, z = NULL, label = NULL, 
                      center = NULL, zoom = 14, mapStyle = 'normal', 
                      legend.left = 'center', ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  expr[['type']] = 'mapScatter'
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'mapScatter')
  
  if(is.null(center)){
    cd = dataList@data[[1]]
    center = c(cd$x[!is.na(cd$x) & !is.na(cd$y)][1], cd$y[!is.na(cd$x) & !is.na(cd$y)][1])
  }
  p = setLayer(dataList, type = 'mapScatter', legend.left = legend.left, 
               ..., 
               coordinateSystem = 'bmap')
  p@option$series[[1]]$type = 'scatter'
  p@option$series[[1]]$label = NULL
  
  zt.vmmin = min(dataList@data[[1]]$z, na.rm = T)
  zt.vmmax = max(dataList@data[[1]]$z, na.rm = T)
  if(zt.vmmin == zt.vmmax){ 
    zt.vmmin = 0
    if(zt.vmmax == 0) zt.vmmax = 1
  }
  
  p@option$series[[1]]$data = lapply(p@option$series[[1]]$data, function(x) as.character(x$value))
  p@option$bmap = .setBmap(center, zoom, mapStyle = mapStyle)
  p@type = 'mapScatter'
  p
}




mapHeatmap = function(dat, x, y, z = NULL, label = NULL, 
                      center = NULL, zoom = 14, mapStyle = 'normal', 
                      splitNumber = 5,
                      legend.left = 'center', ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  expr[['type']] = 'mapHeatmap'
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'mapHeatmap')
  
  if(is.null(center)){
    cd = dataList@data[[1]]
    center = c(cd$x[!is.na(cd$x) & !is.na(cd$y)][1], cd$y[!is.na(cd$x) & !is.na(cd$y)][1])
  }
  p = setLayer(dataList, type = 'mapHeatmap', legend.left = legend.left, 
               ..., 
               coordinateSystem = 'bmap')
  p@option$series[[1]]$type = 'heatmap'
  p@option$series[[1]]$label = NULL
  
  zt.vmmin = min(dataList@data[[1]]$z, na.rm = T)
  zt.vmmax = max(dataList@data[[1]]$z, na.rm = T)
  if(zt.vmmin == zt.vmmax){ 
    zt.vmmin = 0
    if(zt.vmmax == 0) zt.vmmax = 1
  }
  
  p@option$visualMap = list(
    min = zt.vmmin, max = zt.vmmax, 
    splitNumber = splitNumber,
    inRange = list(
      color = list('#50a3ba', '#eac736', '#d94e5d')
    ),
    textStyle = list(
      color = list('#fff')
    )
  )
  
  p@option$series[[1]]$data = lapply(p@option$series[[1]]$data, function(x) as.character(x$value))
  p@option$bmap = .setBmap(center, zoom, mapStyle = mapStyle)
  p@type = 'mapHeatmap'
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



markPoint = function(p, dat, x, y, z, color = .plotColor[1], 
                     symbolSize = 20,
                     seriesIndex = 1){
  
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
    symbolSize = symbolSize,
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




addSecAxis = function(p, series, type, yAxis.max = NULL){
  zt.series = sapply(p@option$series, `[[`, 'name')
  zt.i = which(zt.series == series)
  p@option$series[[zt.i]]$type = type
  p@option$series[[zt.i]]$yAxisIndex = p@option$series[[zt.i]]$yAxisIndex + 1
  p@option$yAxis[[2]] = p@option$yAxis[[1]]
  if(!is.null(yAxis.max)) p@option$yAxis[[2]]$max = yAxis.max
  p
}


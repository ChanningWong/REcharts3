# type = 'scatter'
# ncol = NULL; nrow = NULL; facets.fontSize = 14; facets.top = 6;
# label = NULL; label.show = F; label.position = 'inside'; 
# tooltip.show = T; 
# type = 'bar'; stack = F; color = .plotColor;
# title = NULL; title.fontSize = 18; title.top = 0; title.left = 'left';
# legend = NULL; legend.show = T; legend.orient = c('horizontal', 'vertical');
# legend.left = 'center'; legend.top = '6%'; 
# legend.right = NULL; legend.bottom = NULL; legend.width = NULL; legend.height = NULL;
# grid.left = NULL; grid.top = NULL; grid.right = NULL; grid.bottom = NULL; grid.margin.x = 5; grid.margin.y = 5; 
# yAxis.max = 'auto';
# xAxis.inverse = F; axisLabel.interval.x = NULL; axisLabel.interval.y = NULL;
# width = NULL; height = NULL;
# chart.radius = '70%'; chart.position = c('50%', '55%')
# draggable = T;repulsion = 200;gravity = 0.1;edgeLength = 50;layoutAnimation = F;focusNodeAdjacency = F

setLayer = function(dataList, type = 'bar', 
                    ..., stack = F, color = .plotColor,
                    ncol = NULL, nrow = NULL, facets.fontSize = 14, facets.top = 6,
                    label = NULL, label.show = F, label.position = 'inside', 
                    tooltip.show = T, 
                    title = NULL, title.fontSize = 18, title.top = 0, title.left = 'left',
                    legend = NULL, legend.show = T, legend.orient = c('horizontal', 'vertical'),legend.left = 'center', legend.top = '6%', 
                    legend.right = NULL, legend.bottom = NULL, legend.width = NULL, legend.height = NULL,
                    grid.left = NULL, grid.top = NULL, grid.right = NULL, grid.bottom = NULL, grid.margin.x = 5, grid.margin.y = 5, 
                    yAxis.max = 'auto',
                    xAxis.inverse = F, axisLabel.interval.x = NULL, axisLabel.interval.y = NULL,
                    width = NULL, height = NULL){
  
  
  optionList = list()
  
  # seriesSet
  optionList$series = .setSeries(dataList, 
                                 type = type, color = color, stack = stack,
                                 label.show = label.show, label.position = label.position, 
                                 ...)
  
  # legendSet
  legendData = if(type != 'pie') dataList@seriesName else dataList@xLevelsName
  optionList$legend = .legendSet(data = legendData,
                                 legend.show = legend.show,
                                 legend.left = legend.left, legend.top = legend.top,
                                 legend.right = legend.right, legend.bottom = legend.bottom,
                                 legend.width = legend.width, legend.height = legend.height,
                                 legend.orient = legend.orient[1])
  
  
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
                                 top = title.top, left = title.left))
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
  
  optionList$tooltip = list(show = tooltip.show, formatter = 'formatFunction_tooltip')
  
  # Axis
  if(type %in% c('bar', 'his', 'line', 'scatter')){
    optionList$xAxis =  list()
    optionList$yAxis =  list()
    for(i in 1:length(dataList@ facetsName)){
      if(i < 1) next
      optionList$xAxis[[i]] = list(gridIndex = i - 1, 
                                   axisLabel = list(interval = axisLabel.interval.x),
                                   inverse = xAxis.inverse)
      if(type %in% c('line', 'bar', 'his')) optionList$xAxis[[i]]$data = dataList@xLevelsName
      optionList$yAxis[[i]] = list(gridIndex = i - 1, 
                                   axisLabel = list(interval = axisLabel.interval.y),
                                   max = yAxis.max)
    }
    names(optionList$xAxis) = NULL
    names(optionList$yAxis) = NULL
  }
  
  
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
  if(type %in% c('line', 'bar', 'his', 'graph')){
    p@formatFunction_label = 'function(params){return params.data.label}'
    p@formatFunction_tooltip = 'function(params){return params.name + \':<br>\' + params.seriesName + \' : \' + params.data.label}'
  } else if(type == 'pie'){
    p@formatFunction_label = '\\"{b}: {c} ({d}%)\\"'
    p@formatFunction_tooltip = '\\"{a} <br/>{b}: {c} ({d}%)\\"'
  } else if(type %in% c('scatter', 'lines')){
    p@formatFunction_label = 'function(params){return params.data.label}'
    p@formatFunction_tooltip = 'function(params){return params.seriesName + \' : \' + params.data.label}'
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
  
  if(!is.null(expr$label)) label.show = T
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
  
  if(!is.null(expr$label)) label.show = T
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
  
  if(!is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'line', label.show = label.show, legend.left = legend.left, ...)
  p
}




scatter = function(dat, x, y, z = NULL, facets = NULL, label = NULL, 
                   label.show = F, legend.left = 'center', ...){
  
  expr = match.call()
  expr[[1]] = as.name('.dataParse')
  expr[['type']] = 'scatter'
  parList = as.list(expr[-1])
  dat = eval(expr, parent.frame())
  dataList = .dataList(dat, type = 'scatter')
  
  if(!is.null(expr$label)) label.show = T
  p = setLayer(dataList, type = 'scatter', label.show = label.show, legend.left = legend.left, ...)
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
  
  # if(!is.null(expr$label)) label.show = T
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
  
  # if(!is.null(expr$label)) label.show = T
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
  
  # if(!is.null(expr$label)) label.show = T
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
  
  if(!is.null(expr$label)) label.show = T
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


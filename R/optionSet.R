
.titleSet = function(title = ''){
  title.show = F
  if(!is.null(title)) if(!is.na(title) | nchar(title) > 0) title.show = T 
  list(
    show = title.show,
    text = title
  )
}


.tooltipSet = function(tooltip.show = T){
  list(show = tooltip.show,
       formatter = 'formatFunction_tooltip'
  )
}


.seriesLabelSet = function(label.show = T, label.position = NULL){
  
  
}


.legendSet = function(data,
                      legend.show = T,
                      legend.left = 'auto', legend.top = 'auto',
                      legend.right = 'auto', legend.bottom = 'auto',
                      legend.width = 'auto', legend.height = 'auto',
                      legend.orient = c('horizontal', 'vertical')){
  list(data = data,
       show = legend.show,
       left = legend.left, top = legend.top,
       right = legend.right, bottom = legend.bottom,
       width = legend.width, height = legend.height,
       orient = legend.orient[1])
  
}



.gridSet = function(grid.left = NULL, grid.top = NULL, 
                    grid.right = NULL, grid.bottom = NULL){
  g = list()
  if(!is.null(grid.left)) g$left = paste0(grid.left, '%')
  if(!is.null(grid.top)) g$top = paste0(grid.top, '%')
  if(!is.null(grid.right)) g$right = paste0(grid.right, '%')
  if(!is.null(grid.bottom)) g$bottom = paste0(grid.bottom, '%')
  g
}



.gridSet_facets = function(n, ncol = NULL, nrow = NULL, 
                           grid.left = 7, grid.top = 7, grid.right = 5, grid.bottom = 5,
                           grid.margin.x = NULL, grid.margin.y = NULL,
                           containLabel = T){
  # ncol = ceiling(sqrt(n));grid.left = 7; grid.top = 8; grid.right = 5; grid.bottom = 6; grid.margin.x = 8; grid.margin.y = 9
  if(is.null(ncol)) ncol = ceiling(sqrt(n))
  if(is.null(nrow)) nrow = ceiling(n / ncol)
  if(is.null(grid.margin.x)) grid.margin.x = 5
  if(is.null(grid.margin.y)) grid.margin.y = 7
  margin.x = ifelse(ncol == 1, grid.margin.x, grid.margin.x / (ncol - 1))
  margin.y = ifelse(nrow == 1, grid.margin.y, grid.margin.y / (nrow - 1))
  
  width = (100 - grid.left - grid.right - margin.x*(ncol - 1)) / ncol
  height = (100 - grid.top - grid.bottom - margin.y* (nrow - 1)) / nrow
  i.x = grid.left + (1:ncol - 1) * width + (1:ncol - 1) * margin.x
  i.y = grid.top + (1:nrow - 1) * height + (1:nrow - 1) * margin.y
  i.grid = merge(i.x, i.y)[1:n, , drop = F]
  
  output = mapply(function(ir, it){ # ir = i.grid[1 ,1]; it = i.grid[1 ,2]
    o = list(left = ir, top = it, width = width, height = height, containLabel = containLabel)
    o = lapply(o, paste0, '%')
    o
  }, i.grid[ ,1], i.grid[ ,2], SIMPLIFY = F, USE.NAMES = F)
  attr(output, 'grid') = i.grid
  output
}



.setSeriesData = function(dat, xLevels,
                          xAxisIndex = 0,
                          yAxisIndex = 0,
                          type = 'bar', stack = NULL, name = 'name', color = .plotColor[1], 
                          label.show = T, label.position = 'top'){
  
  toList = function(d)(
    mapply(function(u, v){
      list(value = u, label = v)
    } , 
    d$y, d$label, 
    SIMPLIFY = F, USE.NAMES = F)
  )
  # xLevels = .rev(.pickLevels(d$x))
  if(label.show){
    normalList = list(show = label.show,
                      position = label.position[1],
                      formatter = 'formatFunction_label')
  } else {
    normalList = list(show = F)
  }
  
  if(!is.null(color)){
    itemStyle = list(normal = list(color = color))
  }
  
  list(name = name,
       type = type,
       xAxisIndex = xAxisIndex,
       yAxisIndex = yAxisIndex,
       stack = stack,
       data = toList(dat)[match(xLevels, .pickLevels(dat$x))],
       label = list(normal = normalList, emphasis = normalList),
       itemStyle = itemStyle 
  )
}


.setSeries = function(dat, xLevels, 
                      type = 'bar', stack = F, name = 'name', color = NULL, 
                      label.show = T, label.position = 'top'){
  
  if(is.null(dat$z)){
    series = list(.setSeriesData(dat, xLevels,
                                 type = type, 
                                 name = name, 
                                 label.show = label.show, 
                                 label.position = label.position,
                                 color = .plotColor[1]))
  } else {
    ff = split(dat, dat$z)
    series = mapply(.setSeriesData, 
                    ff, list(xLevels),
                    type = type, 
                    name = name, 
                    label.show = label.show, 
                    label.position = label.position, 
                    color = as.list(color[seq_along(ff)]),
                    SIMPLIFY = F, USE.NAMES = F)
  }
  names(series) = NULL
  series
}


.setSeriesData.pie = function(dat, type = 'pie', name = 'name', 
                              label.show = T, 
                              chart.radius = '50%', chart.position = c('50%', '60%'),
                              label.position = c('outside', 'inside', 'center')){
  
  toList_pie = function(d)(
    mapply(function(x, u, v){
      list(name = x, value = u, label = v)
    } , 
    as.character(d$x), d$y, d$label, 
    SIMPLIFY = F, USE.NAMES = F)
  )
  
  if(label.show){
    normalList = list(show = T,
                      position = label.position[1],
                      formatter = 'formatFunction_label')
  } else {
    normalList = list(show = T,
                      position = label.position[1],
                      formatter = '{b} : {c} ({d}%)')
  }
  
  list(name = name,
       type = type,
       radius = chart.radius,
       center = chart.position,
       data = toList_pie(dat),
       label = list(normal = normalList, emphasis = normalList)
  )
}





coord_rotate = function(p){
  
  i_x = names(p@option) == 'xAxis'
  i_y = names(p@option) == 'yAxis'
  names(p@option)[i_x] = 'yAxis'
  names(p@option)[i_y] = 'xAxis'
  
  for(i in 1:length(p@option$series)){
    p@option$yAxis$data = .rev(p@option$yAxis$data)
    p@option$series[[i]]$data = lapply(p@option$series[[i]]$data, .rev)
  }
  
  p
}







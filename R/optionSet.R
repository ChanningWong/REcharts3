
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
  if(is.null(grid.left)) grid.left = 5
  if(is.null(grid.top)) grid.top = 5
  if(is.null(grid.right)) grid.right = 5
  if(is.null(grid.bottom)) grid.bottom = 5
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





.dataParse = function(dat, x, y, z = NULL, facets = NULL, label = NULL, type = 'bar', ...){
  
  parList = as.list(match.call()[-1])
  if(is.character(parList$x)) parList$x = as.name(parList$x)
  if(is.character(parList$y)) parList$y = as.name(parList$y)
  if(is.character(parList$z)) parList$z = as.name(parList$z)
  if(is.character(parList$label)) parList$label = as.name(parList$label)
  if(is.character(parList$facets)) parList$facets = as.name(parList$facets)
  
  d = data.frame(x = eval(parList$x, dat), 
                 y = eval(parList$y, dat), 
                 stringsAsFactors = F)
  if(!is.null(parList$z)) d$z = eval(parList$z, dat)
  if(!is.null(parList$label)) d$label = eval(parList$label, dat)
  if(!is.null(parList$facets)) d$facets = eval(parList$facets, dat)
  
  if(type == 'scatter'){
    if(is.null(d$label)) d$label = paste0(d$x, ' , ', d$y)
  } else {
    if(is.null(d$label)) d$label = d$y
  }
  d
}


.dataList = function(dat, type = 'bar'){
  
  d = new("REcharts3Data")
  d@var = names(dat)
  d@type = type
  if(type %in% c('his', 'bar', 'line', 'pie')){ 
    d@xLevelsName = .pickLevels(dat$x)
  } else if(type == 'graph'){
    d@xLevelsName = unique(c(.pickLevels(dat$x), .pickLevels(dat$y)))
  }
  # d@yLevelsName = .pickLevels(dat$y)
  if(!is.null(dat$z)) d@seriesName = .pickLevels(dat$z)
  if(!is.null(dat$facets)) d@facetsName = .pickLevels(dat$facets)
  
  if(type == 'graph'){
    dat$x = match(dat$x, d@xLevelsName) - 1
    dat$y = match(dat$y, d@xLevelsName) - 1
  }
  
  if(is.null(dat$facets)){
    dataList = list(dat)
  } else {
    dataList = lapply(split(dat, dat$facets), `[`, setdiff(names(dat), 'facets'))
  }
  d@data = dataList
  d
}



# dat = dataList@data[[1]]
# xLevelsName = dataList@xLevelsName; type = 'lines'
.setDataSeries = function(dat, xLevelsName, type = 'bar'){
  
  toList.bar = function(d){
    mapply(function(u, v) list(value = u, label = v), d$y, d$label, SIMPLIFY = F, USE.NAMES = F)
  }
  # toList.bar(dat)
  
  toList.pie = function(d){
    if(is.factor(d$x)) d$x = as.character(d$x)
    mapply(function(x, y) list(name = x, value = y), d$x, d$y, SIMPLIFY = F, USE.NAMES = F)
  }
  # toList.pie(dat)
  
  toList.scatter = function(d)(
    mapply(function(x, y, v) list(value = c(x, y), label = v), 
    d$x, d$y, d$label, SIMPLIFY = F, USE.NAMES = F)
  )
  
  toList.lines = function(d)(
    mapply(function(x, y) c(x, y), d$x, d$y, SIMPLIFY = F, USE.NAMES = F)
  )
  
  toList.graph = function(d){
    mapply(function(x, y) list(`source` = x, target = y), d$x, d$y, SIMPLIFY = F, USE.NAMES = F)
  }
  
  if(type %in% c('bar', 'his', 'line')){
    toList = toList.bar
  } else if (type == 'pie') {
    toList = toList.pie
  } else if (type == 'scatter') {
    toList = toList.scatter
  } else if (type == 'lines') {
    toList = toList.lines
  } else if (type == 'graph') {
    toList = toList.graph
  }
  
  
  if(type %in% c('bar', 'his', 'line')){
    toList2 = function(d){
      y = toList(d)[match(xLevelsName, .pickLevels(d$x))]
      y[sapply(y, is.null)] = NA
      y
    }
  } else toList2 = toList  
  
  
  if(type != 'lines'){
    if(is.null(dat$z)){
      datSeries = list(toList2(dat))
      names(datSeries) = 'data'
    } else {
      datSeries = lapply(split(dat, dat$z), function(x) toList2(x))
    }
  } else {
    if(is.null(dat$z)){
      datSeries = list(list(coords = toList2(dat)))
      names(datSeries) = 'data'
    } else {
      datSeries = lapply(split(dat, dat$z), function(x) list(list(coords = toList2(x))))
    }
  }
    
  datSeries
}




# type = 'scatter'; label.show = T; label.position = 'top'; stack = T;color = .plotColor
.setSeries = function(dataList, type = 'bar', stack = F, color = .plotColor, 
                      label.show = T, label.position = 'top', ...){
  
  dataSeries = lapply(dataList@data, function(s){ # s = dataList@data[[1]]
    y = .setDataSeries(s, xLevelsName = dataList@xLevelsName, type = type)
    z = y[match(dataList@seriesName, names(y))]
    names(z) = NULL
    z
  })
  
  if('facets' %in% dataList@var){ 
    dataSeries = dataSeries[match(dataList@facetsName, names(dataSeries))]
    names(dataSeries) = NULL
  }
  
  if(label.show){
    normalList = list(show = label.show,
                      position = label.position[1],
                      formatter = 'formatFunction_label')
  } else {
    normalList = list(show = F)
  }
  
  if(type %in% c('bar', 'his', 'line', 'scatter', 'graph')){
    len = length(dataList@seriesName)
  } else if(type == 'pie'){
    len = length(dataList@xLevelsName)
  }
  plotColor = rep(.plotColor, ceiling(len/length(.plotColor)))[1:len]
  
  
  k = 1
  series = list()
  for(i in 1:length(dataList@facetsName)){ # i = 1
    for(j in 1:length(dataList@seriesName)){ # j = 1
      series[[k]] = list(name = dataList@seriesName[j],
                         xAxisIndex = i - 1,
                         yAxisIndex = i - 1, 
                         type = type,
                         label = list(normal = normalList, 
                                      emphasis = normalList),
                         ...)
      
      if(type %in% c('bar', 'his', 'line', 'scatter')){
        series[[k]]$data = dataSeries[[i]][[j]]
        series[[k]]$itemStyle = list(normal = list(color = plotColor[j]))
      } else if(type == 'pie'){
        series[[k]]$data = dataSeries[[i]][[j]]
        series[[k]]$data = mapply(function(x, y){
          x$itemStyle = list(normal = list(color = y))
          x
        }, series[[k]]$data, as.list(plotColor), SIMPLIFY = F, USE.NAMES = F)
      } else if(type == 'graph'){
        series[[k]]$data = dataList@xLevelsName
        series[[k]]$links = dataSeries[[i]][[j]]
        series[[k]]$itemStyle = list(normal = list(color = plotColor[j]))
      }
      
      if(stack) series[[k]]$stack = dataList@facetsName[i]
      
      k = k + 1
    }    
  }
  series
}






.setBmap = function(center, zoom){
  
  
  styleJson = list(
    list(
      'featureType' = 'water',
      'elementType' = 'all',
      'stylers' = list(
        'color' = '#d1d1d1'
      )
    ), list(
      'featureType' = 'land',
      'elementType' = 'all',
      'stylers' = list(
        'color' = '#f3f3f3'
      )
    ), list(
      'featureType' = 'railway',
      'elementType' = 'all',
      'stylers' = list(
        'visibility' = 'off'
      )
    ), list(
      'featureType' = 'highway',
      'elementType' = 'all',
      'stylers' = list(
        'color' = '#fdfdfd'
      )
    ), list(
      'featureType' = 'highway',
      'elementType' = 'labels',
      'stylers' = list(
        'visibility' = 'off'
      )
    ), list(
      'featureType' = 'arterial',
      'elementType' = 'geometry',
      'stylers' = list(
        'color' = '#fefefe'
      )
    ), list(
      'featureType' = 'arterial',
      'elementType' = 'geometry.fill',
      'stylers' = list(
        'color' = '#fefefe'
      )
    ), list(
      'featureType' = 'poi',
      'elementType' = 'all',
      'stylers' = list(
        'visibility' = 'on'
      )
    ), list(
      'featureType' = 'green',
      'elementType' = 'all',
      'stylers' = list(
        'visibility' = 'off'
      )
    ), list(
      'featureType' = 'subway',
      'elementType' = 'all',
      'stylers' = list(
        'visibility' = 'off'
      )
    ), list(
      'featureType' = 'manmade',
      'elementType' = 'all',
      'stylers' = list(
        'color' = '#d1d1d1'
      )
    ), list(
      'featureType' = 'local',
      'elementType' = 'all',
      'stylers' = list(
        'color' = '#d1d1d1'
      )
    ), list(
      'featureType' = 'arterial',
      'elementType' = 'labels',
      'stylers' = list(
        'visibility' = 'off'
      )
    ), list(
      'featureType' = 'boundary',
      'elementType' = 'all',
      'stylers' = list(
        'color' = '#fefefe'
      )
    ), list(
      'featureType' = 'building',
      'elementType' = 'all',
      'stylers' = list(
        'color' = '#d1d1d1'
      )
    ), list(
      'featureType' = 'label',
      'elementType' = 'labels.text.fill',
      'stylers' = list(
        'color' = '#999999'
      )
    )
  )
  
  
  mList = list(center = center,
               zoom = zoom,
               roam = T,
               mapStyle = list(styleJson = styleJson)
  )
  mList
}




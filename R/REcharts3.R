

setClass("REcharts3", 
         representation(id = "character", 
                        type = "character",
                        xLevelsName = "vector",
                        yLevelsName = "vector",
                        seriesName = "vector",
                        facetsName = "vector",
                        option = "list",
                        plotOption = "list", 
                        formatFunction_symbolSize = 'character',
                        formatFunction_label = 'character',
                        formatFunction_tooltip = 'character'),
         prototype = list(xLevelsName = NA,
                          yLevelsName = NA,
                          seriesName = 'data',
                          facetsName = 'total', 
                          formatFunction_symbolSize = '',
                          formatFunction_label = '',
                          formatFunction_tooltip = '')
)


setMethod("show",
          signature(object = "REcharts3"),
          definition = function(object) plot.REcharts3(object))


setMethod("summary",
          signature(object = "REcharts3"),
          definition = function(object){
            cat("\nID: ")
            cat(object@id)
            cat("\nPlot Type: ")
            cat(object@type)
            cat('\n')
          })



setClass("REcharts3Data", 
         representation(data = "list", 
                        var = 'character',
                        type = 'character',
                        xLevelsName = "vector",
                        yLevelsName = "vector",
                        seriesName = "vector",
                        facetsName = "vector"),
         prototype = list(
           xLevelsName = NA,
           yLevelsName = NA,
           seriesName = 'data',
           facetsName = 'total')
)




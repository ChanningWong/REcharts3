

setClass("REcharts3", 
         representation(id = "character", 
                        option = "list",
                        width = "numeric", 
                        height = "numeric", 
                        formatFunction_label = 'character',
                        formatFunction_tooltip = 'character')
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
            cat(object@option$series[[1]]$type)
            cat('\n')
          })






#' Summary of attributes and results
#' @name summary
#' @aliases summary,sp_range-method summary,sp_range_iucn-method
#' @aliases summary,sp_range_iucnextra-method
#' @param object object of class sp_range*.
#' @export
#' @return
#' A written summary.
#' @rdname summary

setMethod("summary", signature(object = "sp_range"),
          function(object) {
            # -----------
            # detecting potential errors
            if (!missing(object)) {
              clo <- class(object)[1]
              if (clo != "sp_range") {
                stop("Argument 'object' must be of class sp_range")
              }
            }else {
              stop("Argument 'object' is necessary.")
            }

            cat("\n                         Summary of sp_range object\n")
            cat("---------------------------------------------------------------------------\n\n")
            print(object@Summary, row.names = F)
            nameess <- slotNames(object)
            l <- slot(object, nameess[2])
            cat("\n\n\nSpecies range\n\n")
            sp::summary(l)
          }
)


#' @rdname summary
setMethod("summary", signature(object = "sp_range_iucn"),
          function(object) {
            # -----------
            # detecting potential errors
            if (!missing(object)) {
              clo <- class(object)[1]
              if (clo != "sp_range_iucn") {
                stop("Argument 'object' must be of class sp_range_iucn")
              }
            }else {
              stop("Argument 'object' is necessary to perform the analysis.")
            }

            cat("\n                      Summary of sp_range_iucn object\n")
            cat("---------------------------------------------------------------------------\n\n")
            print(object@Summary, row.names = F)
            nameess <- slotNames(object)[c(5, 1:3)]
            l <- lapply(1:length(nameess), function(x) {
              slot(object, nameess[x])
            })
            names(l) <- nameess
            cat("\n\n\nOther contents\n\n")
            summary(l)
          }
)


#' @rdname summary
setMethod("summary", signature(object = "sp_range_iucnextra"),
          function(object) {
            # -----------
            # detecting potential errors
            if (!missing(object)) {
              clo <- class(object)[1]
              if (clo != "sp_range_iucnextra") {
                stop("Argument 'object' must be of class sp_range_iucnextra")
              }
            }else {
              stop("Argument 'object' is necessary to perform the analysis.")
            }

            cat("\n                  Summary of sp_range_iucnextra object\n")
            cat("---------------------------------------------------------------------------\n\n")
            print(object@Summary, row.names = F)
            nameess <- slotNames(object)[c(5, 1:3)]
            l <- lapply(1:length(nameess), function(x) {
              slot(object, nameess[x])
            })
            names(l) <- nameess
            cat("\n\n\nOther contents\n\n")
            summary(l)
          }
)

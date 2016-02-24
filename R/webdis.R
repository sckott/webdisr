WebDis <- R6::R6Class("WebDis",
  lock_class = FALSE,
  lock_objects = FALSE,
  public = list(
    cmds = NULL,
    base = "http://127.0.0.1:7379",
    fxn = NULL,
    initialize = function(base) {
      if (!missing(base)) self$base <- base
      self$get_cmds()
      nms <- names(self$cmds)
      for (i in seq_along(nms)) {
        self[[nms[i]]] <- self$gen_fxn(self$cmds[[i]])
      }
      lockEnvironment(self)
    },
    get_cmds = function() {
      self$cmds <- jsonlite::fromJSON(system.file("examples", "commands.json", package = "webdisr"), FALSE)
    },
    gen_fxn = function(x) {
      function(key, ...) {
        url <- file.path(self$base, x$command)
        file.path(url, key)
        # res <- httr::GET(file.path(url, key), ...)
        # jsonlite::fromJSON(content(res, "text", encoding = "UTF-8"), FALSE)
      }
    }
))

f <- make_function2(make_alist(args))


# foo <- function(x) {
#   args <- pluck(x$arguments, "name", "")
#   ll <- alist()
#   for (i in seq_along(args)) {
#     ll[[args[i]]] <- ""
#   }
#   as.function(ll)
# }
# foo(x = 5)

make_alist <- function(args) {
  res <- replicate(length(args), substitute())
  setNames(res, args)
}

make_function2 <- function(args, env = parent.frame()) {
  f <- function() {
    url <- file.path(base, "APPEND")
    res <- httr::GET(file.path(url, ...))
    jsonlite::fromJSON(content(res, "text", encoding = "UTF-8"), FALSE)
  }
  formals(f) <- args
  environment(f) <- env
  return(f)
}

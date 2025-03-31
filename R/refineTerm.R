

#' @title refineTerm
#' 
#' @description ..
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @param model ..
#' 
#' @param ... ..
#' 
#' @examples
#' m1 = lm(mpg ~ cyl + am, data = mtc)
#' refineTerm(names(coef(m1)), model = m1)
#' summary(m2 <- lm(breaks ~ wool + tension, data = warpbreaks))
#' summary(m2a <- lm(breaks ~ -1 + wool + tension, data = warpbreaks))
#' rename_xlevels(m2)
#' rename_xlevels(m2a) # actually this is not ideal (but not wrong anyway) ..
#' refineTerm_factor(names(coef(m2)), model = m2)
#' @name refineTerm
#' @export
refineTerm <- function(x, ...) {
  x |> 
    refineTerm_multinom(...) |>
    refineTerm_logical(...) |>
    refineTerm_factor(...)
}

#' @rdname refineTerm
#' @export
refineTerm_multinom <- function(x, model, ...) {
  if (missing(model)) return(x)
  if (!inherits(model, what = 'multinom')) return(x)
  lev <- if (length(model$lab)) model$lab else if (length(model$lev)) model$lev else stop('wont come here')
  nlev <- length(lev)
  if (nlev <= 2L) return(x)
  ptrn <- sprintf(fmt = '^%s\\:', lev[-1L])
  rpl <- sprintf(fmt = '\u2e22%s vs %s\u2e25\t', lev[-1L], lev[1L])
  for (i in seq_along(ptrn)) x <- gsub(pattern = ptrn[i], replacement = rpl[i], x = x)
  return(x)
}


#' @rdname refineTerm
#' @export
refineTerm_logical <- function(x, model, ...) {
  # update 2025-03-06
  if (!is.character(x)) stop('input must be character')
  
  # 'xxxTRUE' is not guaranteed to be logical predictor; we need 'model' to tell!!
  if (missing(model)) return(x)
  cls <- tryCatch(dataClasses(model), error = identity)
  if (inherits(cls, what = 'error')) return(x)
  nm <- names(which(cls == 'logical'))
  if (!length(nm)) return(x)
  for (i in nm) {
    id <- (x == paste0(i, 'TRUE'))
    if (sum(id) == 1L) x[id] <- i
    # else do nothing
  }
  return(x)
}




#' @rdname refineTerm
#' @export
refineTerm_factor <- function(x, model, ...) {
  # update 2025-03-06
  if (!is.character(x)) stop('input must be character')
  if (missing(model)) return(x)
  tmp <- rename_xlevels(model)
  if (!length(tmp)) return(x)
  
  old <- names(tmp)
  id <- match(x, table = old, nomatch = NA_integer_)
  x[!is.na(id)] <- tmp[id[!is.na(id)]]
  for (j in seq_along(tmp)) { # when `x` is [refineTerm_multinom] return
    x <- gsub(paste0('\\t', old[j], '$'), replacement = paste0('\t', tmp[j]), x = x)
  }
  return(x)
}



#' @rdname refineTerm
#' @export
rename_xlevels <- function(model) {
  # update 2025-03-06
  l <- xlevels(model)
  if (!length(l)) return(invisible())
  
  trm <- terms(model)
  t0 <- trm |> attr(which = 'intercept', exact = TRUE)
  if (length(trm) && !t0) return(invisible()) # no-intercept model will be dangerous
  
  mapply(FUN = \(l, nm) {
    tmp <- paste0(nm, ' \u2e22', l[-1L], ' vs ', l[1L], '\u2e25')
    names(tmp) <- paste0(nm, l[-1L])
    return(tmp)
  }, l = l, nm = names(l), SIMPLIFY = FALSE) |>
    unname() |>
    unlist(use.names = TRUE)
  
}




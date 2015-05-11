#' @export
#' @rdname ls_events
ls_eventintervals <- function(pattern=NULL,match=TRUE) {
  symbols <- ls_events(pattern,match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = .event),silent=TRUE)
    if (inherits(tmp_instr, 'eventinterval') && inherits(tmp_instr, 'event')) {
      tmp_symbols <- c(tmp_symbols,instr)
    }
  }
  tmp_symbols
}

#' @export
#' @rdname ls_events
ls_concessions <- function(pattern=NULL,match=TRUE) {
  symbols <- ls_events(pattern,match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = .event),silent=TRUE)
    if (inherits(tmp_instr, 'concession') && inherits(tmp_instr, 'event')) {
      tmp_symbols <- c(tmp_symbols,instr)
    }
  }
  tmp_symbols
}

#' @export
#' @rdname ls_events
ls_breaks <- function(pattern=NULL,match=TRUE) {
  symbols <- ls_events(pattern,match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = .event),silent=TRUE)
    if (inherits(tmp_instr, 'break') && inherits(tmp_instr, 'event')) {
      tmp_symbols <- c(tmp_symbols,instr)
    }
  }
  tmp_symbols
}

#' @export
#' @rdname ls_events
ls_settle2settles <- function(pattern=NULL,match=TRUE) {
  symbols <- ls_events(pattern,match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = .event),silent=TRUE)
    if (inherits(tmp_instr, 'settle2settle') && inherits(tmp_instr, 'event')) {
      tmp_symbols <- c(tmp_symbols,instr)
    }
  }
  tmp_symbols
}

#' @export
#' @rdname ls_events
rm_eventintervals <- function(x) {
  if (missing(x)) {
    x <- ls_policies()
  }
  rm(list=x[x %in% ls_eventintervals()], pos=.event)
}

#' @export
#' @rdname ls_events
rm_concessions <- function(x) {
  if (missing(x)) {
    x <- ls_policies()
  }
  rm(list=x[x %in% ls_concessions()], pos=.event)
}

#' @export
#' @rdname ls_events
rm_breaks <- function(x) {
  if (missing(x)) {
    x <- ls_policies()
  }
  rm(list=x[x %in% ls_breaks()], pos=.event)
}


#' Capture side effects.
#'
#' These functions wrap functions so that instead of generating side effects
#' through printed output, messages, warnings, and errors, they return enhanced
#' output. They are all adverbs because they modify the action of a verb (a
#' function).
#'
#' @inheritParams map
#' @param quiet Hide errors (`TRUE`, the default), or display them
#'   as they occur?
#' @param otherwise Default value to use when an error occurs.
#'
#' @return `safely`: wrapped function instead returns a list with
#'   components `result` and `error`. One value is always `NULL`.
#'
#'   `quietly`: wrapped function instead returns a list with components
#'   `result`, `output`, `messages` and `warnings`.
#'
#'   `possibly`: wrapped function uses a default value (`otherwise`)
#'   whenever an error occurs.
#'
#' @export
#' @examples
#' safe_log <- safely(log)
#' safe_log(10)
#' safe_log("a")
#'
#' list("a", 10, 100) %>%
#'   map(safe_log) %>%
#'   transpose()
#'
#' # This is a bit easier to work with if you supply a default value
#' # of the same type and use the simplify argument to transpose():
#' safe_log <- safely(log, otherwise = NA_real_)
#' list("a", 10, 100) %>%
#'   map(safe_log) %>%
#'   transpose() %>%
#'   simplify_all()
#'
#' # To replace errors with a default value, use possibly().
#' list("a", 10, 100) %>%
#'   map_dbl(possibly(log, NA_real_))
#'
#' # persistently() makes a function repeatedly try to work
#' risky_runif <- function(lo = 0, hi = 1) {
#'   y <- runif(1, lo, hi)
#'   if(y < 0.9) {
#'     stop(y, " is too small")
#'   }
#'   y
#' }
#' persistent_risky_runif <- persistently(risky_runif, otherwise = -99, quiet = FALSE)
#' set.seed(1)
#' persistent_risky_runif()
#' set.seed(3)
#' persistent_risky_runif()
#'
#' # For interactive usage, auto_browse() is useful because it automatically
#' # starts a browser() in the right place.
#' f <- function(x) {
#'   y <- 20
#'   if (x > 5) {
#'     stop("!")
#'   } else {
#'     x
#'   }
#' }
#' if (interactive()) {
#'   map(1:6, auto_browse(f))
#' }
#'
#' # It doesn't make sense to use auto_browse with primitive functions,
#' # because they are implemented in C so there's no useful environment
#' # for you to interact with.
safely <- function(.f, otherwise = NULL, quiet = TRUE) {
  .f <- as_mapper(.f)
  function(...) capture_error(.f(...), otherwise, quiet)
}

#' @export
#' @rdname safely
quietly <- function(.f) {
  .f <- as_mapper(.f)
  function(...) capture_output(.f(...))
}

#' @export
#' @rdname safely
possibly <- function(.f, otherwise, quiet = TRUE) {
  .f <- as_mapper(.f)
  force(otherwise)

  function(...) {
    tryCatch(.f(...),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        otherwise
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
}

#' @export
#' @rdname safely
persistently <- function(.f, otherwise = NULL, quiet = TRUE, max_attempts = 5) {
  .f <- as_mapper(.f)
  force(max_attempts)
  function(...) {
    for(i in seq_len(max_attempts)) {
      answer <- capture_error(.f(...), quiet = quiet)
      if(is.null(answer$error)) {
        return(answer$result)
      }
    }
    if(!quiet) {
      msg <- sprintf(
        "%s failed after %d tries; returning %s",
        deparse(match.call()),
        max_attempts,
        format(otherwise)
      )
      message(msg)
    }
    otherwise
  }
}

#' @export
#' @rdname safely
auto_browse <- function(.f) {
  if (is_primitive(.f)) {
    abort("Can not auto_browse() primitive functions")
  }

  function(...) {
    withCallingHandlers(
      .f(...),
      error = function(e) {
        # 1: h(simpleError(msg, call))
        # 2: .handleSimpleError(function (e)  <...>
        # 3: stop(...)
        frame <- ctxt_frame(4)
        browse_in_frame(frame)
      },
      warning = function(e) {
        if (getOption("warn") >= 2) {
          frame <- ctxt_frame(7)
          browse_in_frame(frame)
        }
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
}

browse_in_frame <- function(frame) {
  if (.Platform$GUI == "ESS") {
    # Workaround ESS issue
    with_env(frame$env, on.exit({
      browser()
      NULL
    }))
    return_from(frame)
  } else {
    eval_bare(quote(browser()), env = frame$env)
  }
}

capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
  tryCatch(
    list(result = code, error = NULL),
    error = function(e) {
      if (!quiet)
        message("Error: ", e$message)

      list(result = otherwise, error = e)
    },
    interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    }
  )
}

capture_output <- function(code) {
  warnings <- character()
  wHandler <- function(w) {
    warnings <<- c(warnings, w$message)
    invokeRestart("muffleWarning")
  }

  messages <- character()
  mHandler <- function(m) {
    messages <<- c(messages, m$message)
    invokeRestart("muffleMessage")
  }

  temp <- file()
  sink(temp)
  on.exit({
    sink()
    close(temp)
  })

  result <- withCallingHandlers(
    code,
    warning = wHandler,
    message = mHandler
  )

  output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")

  list(
    result = result,
    output = output,
    warnings = warnings,
    messages = messages
  )
}

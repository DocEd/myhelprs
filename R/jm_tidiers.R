#' Tidy JM
#'
#'
#' @export
tidy_jm <- function(x) {

  lng <- summary(x)[["CoefTable-Long"]] %>%
    as_tibble(rownames = "term") %>%
    mutate(sub.Model = "Longitudinal")

  evt <- summary(x)[["CoefTable-Event"]] %>%
    as_tibble(rownames = "term") %>%
    mutate(sub.Model = "Event")

  bind_rows(lng, evt)
}

#' Augment JM
#'
#'
#' @export
augment_jm <- function(x, data = NULL, ...) {

  if (is.null(data))
    data <- x$data

  as_broom_tibble(data) %>%
    mutate(.cluster = as.factor(!!x$clustering))
}


#' Glance JM
#'
#' @export
glance_jm <- function(x) {
  tibble(
    logLik = logLik(x)[1],
    AIC = AIC(x),
    BIC = BIC(x)
  )
}

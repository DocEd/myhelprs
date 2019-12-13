#' Tidy JM
#'
#'
#' @export
tidy.JM <- function(x) {

  lng <- summary(jointFit.aids)[["CoefTable-Long"]] %>%
    as_tibble(rownames = "term") %>%
    mutate(sub.Model = "Longitudinal")

  evt <- summary(jointFit.aids)[["CoefTable-Event"]] %>%
    as_tibble(rownames = "term") %>%
    mutate(sub.Model = "Event")

  bind_rows(lng, evt)
}

#' Augment JM
#'
#'
#' @export
augment.JM <- function(x, data = NULL, ...) {

  if (is.null(data))
    data <- x$data

  as_broom_tibble(data) %>%
    mutate(.cluster = as.factor(!!x$clustering))
}


#' Glance JM
#'
#' @export
glance.jointModel <- function(x) {
  tibble(
    logLik = logLik(x)[1],
    AIC = AIC(jointFit.aids),
    BIC = BIC(jointFit.aids)
  )
}

#' Synchronise Project with IDHS
#'
#' Working within the IDHS necessitates moving between two computers where one
#' does not have access to git. For this reason, is it useful to automate a
#' syncronisation process when regularly having to move between the two. Please
#' use with EXTREME caution, as this function overwrites and deletes files as
#' specified
#'
#' MD5 hashes are used to check for file changes.
#'
#' Hidden files are ignored, which is useful as we don't want to break the .git
#' folder anyway. Please be ultra careful about taking out any data. This is
#' for code sync only.
#'
#' @param from the path to the folder that contains the source material to sync
#' @param to the path to the folder where you want to sync to take place
#'
#' @importFrom rlang inform
#' @importFrom tools md5sum
#' @importFrom glue glue
#'
#' @return returns nothing
#' @export
sync <- function(from, to) {

  stopifnot(!is.null(from), !is.null(to))

  from <- file.path(from)
  to <- file.path(to)

  arrivals <- list.files(from, recursive = TRUE)
  existing <- list.files(to, recursive = TRUE)

  arrival_dirs <- list.dirs(from, full.names = FALSE, recursive = TRUE)
  existing_dirs <- list.dirs(to, full.names = FALSE, recursive = TRUE)

  ## We need to take 3 actions:
  # 1) overwrite any files that have changed
  # 2) delete any files that have been deleted
  # 3) copy accross any new files

  overwrites <- 0L
  additions <- 0L
  deletes <- 0L
  new_dirs <- 0L
  removed_dirs <- 0L

  rlang::inform("The following changes will be made \n")

  # go through every directory in the arrivals area
  for (d in seq_along(arrival_dirs)) {
    if (!(arrival_dirs[d] %in% existing_dirs)) {
      rlang::inform(
        glue::glue("new folder detected: creating {arrival_dirs[d]}"))
    }
  }

  for (d in seq_along(existing_dirs)) {
    if (!(existing_dirs[d] %in% arrival_dirs)) {
      rlang::inform(
        glue::glue("folder deleted: removing {existing_dirs[d]}"))
    }
  }

  # go through every file in the arrivals area
  for (i in seq_along(arrivals)) {
    new_file <- file.path(from, arrivals[i])

    # check if arrival file already exists
    if (arrivals[i] %in% existing) {
      old_file <- file.path(to, arrivals[i])

      # check if the files differ.
      # We can use Hashes for this
      new_hash <- as.character(tools::md5sum(new_file))
      old_hash <- as.character(tools::md5sum(old_file))

      if (new_hash != old_hash) {
        rlang::inform(glue::glue("changes detected: overwriting {new_file}"))
      }

      # Otherwise if file is not in existance (i.e. it is a new file)
      # copy straight across
    } else {
      rlang::inform(glue::glue("new file: copying {new_file}"))
    }
  }

  # Now clean up any files that have been deleted
  for (j in seq_along(existing)) {
    existing_file <- file.path(to, existing[j])
    if (!(existing[j] %in% arrivals)) {
      rlang::inform(glue::glue("file removed: deleting {existing_file}"))
    }
  }

  x <- readline(prompt = "Are you happy to proceed? 1 = Yes, Any = No: ")

  if (x == "1") {

    # go through every directory in the arrivals area
    for (d in seq_along(arrival_dirs)) {
      if (!(arrival_dirs[d] %in% existing_dirs)) {
        rlang::inform(
          glue::glue("new folder detected: creating {arrival_dirs[d]}"))
        invisible(dir.create(file.path(to, arrival_dirs[d])))
        new_dirs <- new_dirs + 1L
      }
    }

    # go through every file in the arrivals area
    for (i in seq_along(arrivals)) {
      new_file <- file.path(from, arrivals[i])

      # check if arrival file already exists
      if (arrivals[i] %in% existing) {
        old_file <- file.path(to, arrivals[i])

        # check if the files differ.
        # We can use Hashes for this
        new_hash <- as.character(tools::md5sum(new_file))
        old_hash <- as.character(tools::md5sum(old_file))

        if (new_hash != old_hash) {
          rlang::inform(glue::glue("changes detected: overwriting {new_file}"))
          invisible(
            file.copy(
              from = new_file,
              to = old_file,
              overwrite = TRUE))
          overwrites <- overwrites + 1L
        }

      # Otherwise if file is not in existence
      # (i.e. it is a new file) copy straight across
      } else {
        rlang::inform(glue::glue("new file: copying {new_file}"))
        invisible(
          file.copy(
            from = new_file,
            to = file.path(to, arrivals[i])))
        additions <- additions + 1L
      }
    }

    # Now clean up any files that have been deleted
    for (j in seq_along(existing)) {
      existing_file <- file.path(to, existing[j])
      if (!(existing[j] %in% arrivals)) {
        rlang::inform(glue::glue("file removed: deleting {existing_file}"))
        invisible(file.remove(existing_file))
        deletes <- deletes + 1L
      }
    }

    for (d in seq_along(existing_dirs)) {
      if (!(existing_dirs[d] %in% arrival_dirs)) {
        rlang::inform(
          glue::glue("folder deleted: removing {existing_dirs[d]}"))
        invisible(unlink(file.path(to, existing_dirs[d]), recursive = TRUE))
        removed_dirs <- removed_dirs + 1L
      }
    }

    rlang::inform(
      glue::glue("summary
    additions: {additions}
    deletions: {deletes}
    overwrites: {overwrites}
    new dirs: {new_dirs}
    rm dirs: {removed_dirs}"))
  } else {
    rlang::inform("Aborting")
  }
}

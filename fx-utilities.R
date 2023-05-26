# README----
# utility functions

# register all font styles for use by ragg package
# https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/
# https://github.com/yjunechoe/junebug/
font_hoist <- function(family, silent = FALSE) {
  font_specs <- systemfonts::system_fonts() |>
    dplyr::filter(family == .env[["family"]]) |>
    dplyr::mutate(family = paste(.data[["family"]], .data[["style"]])) |>
    dplyr::select(plain = "path", name = "family")

  purrr::pwalk(as.list(font_specs), systemfonts::register_font)

  if (!silent) message(paste0("Hoisted ", nrow(font_specs), " variants:\n", paste(font_specs$name, collapse = "\n")))
}

# container for reactable tables with a title
react_tbl_div <- function(rtbl = tbl, selector = "rtbl", title = "Table title") {
  tagList(
    tags$div(
      class = paste0(selector),
      div(
        class = "rtbl-title",
        paste0(title),
      ), htmlwidgets::appendContent(
        rtbl,
        htmltools::tags$span(
          class = "rtbl-src",
          p("")
        )
      )
    )
  )
}

# container for reactable tables with a title
react_tbl_div_sans_title <- function(rtbl = tbl, selector = "rtbl") {
  tagList(
    tags$div(
      class = paste0(selector),
      htmlwidgets::appendContent(
        rtbl,
        htmltools::tags$span(
        class = "rtbl-src",
        p("")
        )
      )
    )
  )
}

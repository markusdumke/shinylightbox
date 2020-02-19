#' Lightbox Image Gallery
#'
#' A shiny module to be used inside a shiny app. Make sure to include
#' [use_lightbox()] in the UI to load the necessary javascript and css ressources.
#'
#' @param src A vector of paths to images.
#' @param gallery An identifier for an image gallery.
#' @param caption A vector of same length as `src` with corresponding image captions.
#' @param display CSS display property.
#'
#' @rdname lightbox
#' @export
#' @import shiny
#'
lightbox_ui <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("gallery"))
}

#' @export
#' @rdname lightbox
lightbox <- function(input, output, session, src, gallery, caption = "", display = "block") {
  output$gallery <- renderUI({
    lightbox_gallery(src = src, gallery = gallery, caption = caption, display = display)
  })
}


lightbox_gallery <- function(src, gallery, caption = "", display = 'block'){

  tags$div(
    style = sprintf('display: %s;', display),
    tags$div(
      class = 'shinylightbox-card-deck',
      lapply(seq_along(src), function(i){
        tags$div(
          `data-type` = "template", class = 'shinylightbox-card',
          tags$a(
            id = src[i],
            href = src[i],
            `data-lightbox` = gallery,
            `data-title` = caption[i],
            tags$img(
              class = 'shinylightbox-card-img-top',
              src = src[i],
              width = '80px',
              height = 'auto')
          )
        )
      })
    )
  )

}

#' Initialize lightbox gallery
#'
#' @export
#' @rdname lightbox
#'
use_lightbox <- function() {
  addResourcePath("lightbox", system.file("lightbox", package = "shinylightbox"))
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "lightbox/lightbox.min.css"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "lightbox/styles.css"
    ),
    tags$script(
      src = "lightbox/lightbox.min.js"
    )
  )
}

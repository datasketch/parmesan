library(shiny)
library(shinyjs)
library(dsAppLayout)
library(dsAppWidgets)
library(shinycustomloader)
library(yaml)
library(png)
library(brickr)
library(colourpicker)

styles <- "

.shiny-output-error {
  visibility: hidden;
}

.shiny-output-error:before {
  visibility: hidden;
}

.style_section {
  font-weight: 700;
  margin-bottom: 7px;
  letter-spacing: 1px;
}

.form-group label {
  font-size: 15px;
  font-weight: 500;
  letter-spacing: 1px;
}

.buttonDown {
  border: 1px solid;
  margin-left: 5px;
  display: inline-block;
}

.dn {
  font-size: 23pt;
  display: none !important;
  color: blue;
}

"

ui <- dsAppPanels(includeScript("www/add-remove_class.js"),
                  panel(title = "Imágen",
                        color = "chardonnay",
                        collapsed = FALSE,
                        width = "600px",
                        body = div(imageInputUI("imageIn",
                                                selected = "sampleData",
                                                # selected = "fileUpload",
                                                choices = list("Sample images" = "sampleData",
                                                               "Load" = "fileUpload",
                                                               "URL" = "url",
                                                               "My library" = "dsLibrary")),
                                   withLoader(imageOutput("image_preview"), type = "image", loader = "loading.svg"))),
                                   # withLoader(uiOutput("image_preview"), type = "image", loader = "loading.svg"))),
                  panel(title = "Editar mosaico",
                        color = "magenta", # azul
                        collapsed = FALSE,
                        body = uiOutput("layout_test")),
                  panel(title = "Mosaico",
                        color = "magenta",
                        collapsed = FALSE,
                        width = "700px",
                        body = withLoader(plotOutput("vizEnd"), type = "image", loader = "loading.svg")))


layout <- read_yaml("inst/examples/layout.yaml")
params <- read_yaml("inst/examples/params.yaml")
children <- names(params)[grep("child_info", params)]

random_name <- function(n = 10) {
  paste0(sample(c(LETTERS, letters, 0:9), n, TRUE), collapse = "")
}

server <- function(input, output, session) {

  # reactivo que almacena el plot
  plot_lego <- reactiveValues(img = NULL,
                              plt = NULL)

  # reactivo que almacena lo que se importa
  inputImage <- callModule(imageInput,
                           "imageIn",
                           # image = TRUE,
                           sampleFile = list("Tapete persa" = "www/99028399493-1.jpg",
                                             "Madera tejida" = "www/99028399493-1.png",
                                             "Rincón encontrado" = "www/pero.png"))

  # renderizando lo importado
  output$image_preview <- renderImage({
    if (is.null(inputImage())) {
      img <- list(src = "")
    } else {
      img <- inputImage()
      img$width <- "100%"
      img
    }
    img
  }, deleteFile = FALSE)

  # inicializando los sliders con el width y height de la imágen
  observeEvent(inputImage(), {
    lg <- inputImage()$src
    if (sum(is.null(lg) | nzchar(lg)) == 0) return()
    r0 <- tryCatch(png::readPNG(lg), error = function(e) e)
    r1 <- tryCatch(jpeg::readJPEG(lg), error = function(e) e)
    w0 <- which(c(!any(grepl("error", class(r0))), !any(grepl("error", class(r1))))) - 1
    r2 <- get(paste0("r", w0))
    plot_lego$img <- r2
    r3 <- dim(r2)[1:2]
    # escalar las dimensiones
    scl <- 78
    w1 <- which(r3 == max(r3))
    if (w1 == 1) {
      hg <- scl
      wh <- (r3[2] * scl) / r3[1]
    } else if (w1 == 2) {
      hg <- (r3[1] * scl) / r3[2]
      wh <- scl
    } else {
      hg <- scl
      wh <- scl
    }
    updateSliderInput(session, "height_img", value = hg)
    updateSliderInput(session, "width_img", value = wh)
  })

  # renderizando todos los parámetros, los hijos dentro de un div con id "shn-id"
  output$layout_test <- renderUI({
    map(names(layout), function(section) {
      div(div(id = section, class = 'style_section', section),
          map(layout[[section]], function(param_i) {
            p_inf <- params[[param_i]]
            if (is.null(p_inf)) return()
            if (p_inf$show) {
              if (param_i %in% children) {
                div(id = paste0("shn-", p_inf$input_info$input_params$inputId),
                    do.call(p_inf$input_info$input, p_inf$input_info$input_params))
              } else {
                do.call(p_inf$input_info$input, p_inf$input_info$input_params)
              }
            } else {
              return()
            }
          })
      )
    })
  })

  # dependencias de los parámetros contrast sólo si palette es "bw",
  # palette si color table es NULL
  # ¿El tamaño del lego debe inicial debe ser igual al de la imagen o algo así?...
  # ¿Tamaño default?

  # muestra o quita los widgets hijos si es el caso
  map(children, function(a) {
    chl_id <- params[[a]]$input_info$input_params$inputId
    chl_inf <- params[[a]]$child_info
    mofa <- chl_inf$motfat
    mofa_id <- params[[mofa]]$input_info$input_params$inputId
    fl <- chl_inf$filter
    observeEvent(input[[mofa_id]], {
      if (input[[mofa_id]] %in% fl) {
        session$sendCustomMessage("handlerS", paste0("shn-", chl_id))
      } else {
        session$sendCustomMessage("handlerN", paste0("shn-", chl_id))
      }
    })
  })

  observe(print(input$color_table_img))

  # si cambia la imagen, el mosaico se reinicializa
  observeEvent(inputImage(), {
    plot_lego$plt <- NULL
  })

  # gráfica de lego
  observeEvent(input$generate_img, {
    plt <- plot_lego$img %>%
      image_to_mosaic(img_size = c(input$width_img, input$height_img),
                      color_table = input$color_table_img,
                      method = input$method_img,
                      color_palette = input$color_palette_img,
                      dithering = input$dithering_img,
                      contrast = input$contrast_img,
                      brightness = input$brightness_img) %>%
      build_mosaic()
    plot_lego$plt <- plt
  })

  # renderizando mosaico ggplot
  output$vizEnd <- renderPlot({
    plot_lego$plt
  })

  # descargas
  output$downOptions <- renderUI({
    htmltools::tagList(div(downloadButton("img_png", "png", class = "buttonDown"),
                           downloadButton("img_jpeg", "jpeg", class = "buttonDown"),
                           downloadButton("img_svg", "svg", class = "buttonDown"),
                           downloadButton("img_pdf", "pdf", class = "buttonDown")))
  })

  tempDir <- reactive({
    last_ext <- input$last_btn
    if (is.null(last_ext)) return()
    dicTemp <- tempdir()
    n <- sample(1:10, 1)
    fileName <- random_name(n)
    x <-  list("Dir" = dicTemp,
               "viz_id" = fileName,
               "ext" = paste0(".", gsub(".+_", "", last_ext)))
    x
  })

  observe({
    map(c("img_png", "img_jpeg", "img_svg", "img_pdf"), function(z) {
      output[[z]] <- downloadHandler(filename = function() {paste0(tempDir()$Dir, tempDir()$viz_id, tempDir()$ext)},
                                     content = function(file) {ggmagic::save_ggmagic(plot_lego$plt, file, tempDir()$ext)})
    })
  })

}
shinyApp(ui, server)

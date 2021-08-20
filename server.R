
# Icons
neonIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28),
  Terrestrial = makeIcon("icons/mountain-icon.png", iconWidth =  28, iconHeight = 28)
)

# Slides for slickR
model_slides <- list.files("www/model_slides", full.names = TRUE)

# a table container with complex header
sketch1 = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan = 2, 'Slope (m)'),
      th(colspan = 2, 'Intercept (b)')
    ),
    tr(
      lapply(rep(c('Mean', 'Std. Dev.'), 2), th)
    )
  )
))

shinyServer(function(input, output, session) {

  # NEON Sites datatable ----
  output$table01 <- DT::renderDT(
    neon_sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE, dom = 't'), server = FALSE
  )

  # to keep track of previously selected row
  prev_row <- reactiveVal()
  siteID <- reactiveVal()

  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')


  # Select NEON DT rows ----
  observeEvent(input$table01_rows_selected, {
    row_selected = neon_sites[input$table01_rows_selected, ]
    siteID <<- neon_sites$siteID[input$table01_rows_selected]
    coords <- st_coordinates(row_selected)
    colnames(coords) <- c("long", "lat")
    row_selected = cbind(row_selected, coords)
    proxy <- leafletProxy('neonmap')
    proxy %>%
      addAwesomeMarkers(layerId = as.character(row_selected$uid),
                        lng=row_selected$long,
                        lat=row_selected$lat,
                        icon = my_icon)

    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy %>%
        addMarkers(data = prev_row(),
                   layerId = as.character(prev_row()$uid))
    }
    # set new value to reactiveVal
    prev_row(row_selected)

    # fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
    # fc_date <<- list.files(fpath)
    # fpath2 <- file.path(fpath, fc_date[1], "00")
    # fils <<- list.files(fpath2)
    # fils <<- fils[-c(grep("ens00", fils))]
    # fid <- nc_open(file.path(fpath2, fils[1]))
    # on.exit({
    #   nc_close(fid)
    # })
    # vars <- fid$var # Extract variable names for selection
    # fc_vars <<- names(vars)
    # membs <<- length(fils)
    #
    # # Update parameters & initial conditions
    # upd_parms <- as.vector(unlist(site_parms[site_parms$site == siteID, -1]))
    # upd_yin <- site_yini[site_yini$site == siteID, -1]
    # parms <<- upd_parms

  })

  # Neon map ----
  output$neonmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = neon_sites,
                 layerId = ~uid, clusterOptions = markerClusterOptions(),
                 label = ~locationDescription, icon = ~neonIcons[type])

  })

  # Download phenocam ----
  pheno_file <- reactiveValues(img = NULL)
  observeEvent(input$view_webcam, {

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Accessing and downloading phenocam image",
                 detail = "This may take a while. This window will disappear
                     when it is downloaded.", value = 0.5)

    p <- input$neonmap_marker_click
    idx <- which(neon_sites_df$siteID == siteID)
    url <- neon_sites_df$pheno_url[idx]
    pheno_file$img <<- download_phenocam(url)
    progress$set(value = 1)
  })

  # Show phenocam image ----
  output$pheno <- renderImage({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in the table.")
    )
    validate(
      need(!is.null(pheno_file$img), "Click 'View latest photo' to download the image.")
    )
    list(src = pheno_file$img,
         alt = "Image failed to render. Please click 'Save plot' again.",
         height = 320,
         width = 350)
  }, deleteFile = FALSE)

  observeEvent(input$view_webcam, {
    output$prompt1 <- renderText({
      "Hover your cursor above the image to enlarge."
    })
  })

  # Download html ----
  observeEvent(input$table01_rows_selected, {
    p <- input$neonmap_marker_click  # typo was on this line
    sid <- neon_sites$siteID[input$table01_rows_selected]
    idx <- which(neon_sites_df$siteID == sid)
    # output$site_name <- neon_sites$description[idx]
    output$site_html <- renderUI({
      return(get_html(site_id = neon_sites_df$siteID[idx]))
    })
  })
  #** Create hyperlink ----
  observeEvent(input$table01_rows_selected, {
    sid <- neon_sites$siteID[input$table01_rows_selected]
    url <- paste0("https://www.neonscience.org/field-sites/field-sites-map/", sid)

    output$site_link <- renderUI({
      tags$a(href = url, "Click here for more site info", target = "_blank")
    })
  })
  #** Create prompt ----
  observeEvent(input$table01_rows_selected, {
    output$prompt2 <- renderText({
      "Click on the link below to find out more information about your site."
    })
  })

  # Read in site data ----
  neon_DT <- reactive({ # view_var
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )

    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )
    df <- read.csv(file)
    df[, 1] <- as.POSIXct(df[, 1], tz = "UTC")
    df[, -1] <- signif(df[, -1], 4)
    names(df)[ncol(df)] <- read_var

    if(input$view_var == "Surface water temperature") {
      df <- df[df[, 2] == min(df[, 2], na.rm = TRUE), c(1, 3)] # subset to surface temperature
    }

    sel <- tryCatch(df[(selected$sel$pointNumber+1), , drop=FALSE] , error=function(e){NULL})


    return(list(data = df, sel = sel))
  })

  # NEON variable description table ----
  output$var_desc <- renderDT({
    var_desc <- neon_vars[!duplicated(neon_vars$Short_name), c("Short_name", "description")]
    colnames(var_desc) <- c("Name", "Description")
    datatable(var_desc, rownames = FALSE, options = list(pageLength = 4))
  })

  # Site data datatable ----
  output$neon_datatable <- DT::renderDT({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    df <- neon_DT()$data
    df[, -1] <- signif(df[, -1], 4)
    df[, 1] <- format(df[, 1], format = "%Y-%m-%d")
    names(df)[ncol(df)] <- read_var
    return(df)
  })

  # Variable description ----
  output$txt_out <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    out_txt <- neon_vars$description[which(neon_vars$Short_name == input$view_var)][1]
    return(out_txt)
  })

  # Site data plot ----
  output$var_plot <- renderPlotly({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )
    obj <- neon_DT()$sel

    p <- ggplot() +
      geom_point(data = neon_DT()$data, aes_string(names(neon_DT()$data)[1], names(neon_DT()$data)[2]), color = "black") +
      ylab(paste0(input$view_var, " (", units, ")")) +
      xlab("Time") +
      theme_minimal(base_size = 12)

    if(!is.null(obj)) {
      p <- p +
        geom_point(data = obj, aes_string(names(obj)[1], names(obj)[2]), color = cols[2])

    }
    return(ggplotly(p, dynamicTicks = TRUE, source = "A"))
  })

  # Slickr model output
  output$slck_model <- renderSlickR({
    slickR(model_slides)
  })

  # Stats 101 ----
  # Load airt & SWT
  airt_swt <- reactiveValues(df = NULL)
  observeEvent(input$plot_airt_swt, { # view_var

    req(input$table01_rows_selected != "")

    ref <- "Air temperature"
    x_var <- neon_vars$id[which(neon_vars$Short_name == ref)][1]
    x_units <- neon_vars$units[which(neon_vars$Short_name == ref)][1]
    x_file <- file.path("data", "neon", paste0(siteID, "_", x_var, "_", x_units, ".csv"))
    validate(
      need(file.exists(x_file), message = paste0(ref, " is not available at this site."))
    )
    xvar <- read.csv(x_file)
    xvar[, 1] <- as.POSIXct(xvar[, 1], tz = "UTC")
    xvar$Date <- as.Date(xvar[, 1])
    xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp


    ref2 <- "Surface water temperature"
    y_var <- neon_vars$id[which(neon_vars$Short_name == ref2)][1]
    y_units <- neon_vars$units[which(neon_vars$Short_name == ref2)][1]
    y_file <- file.path("data", "neon", paste0(siteID, "_", y_var, "_", y_units, ".csv"))
    validate(
      need(file.exists(y_file), message = paste0(ref2, " is not available at this site."))
    )
    yvar <- read.csv(y_file)
    yvar[, 1] <- as.POSIXct(yvar[, 1], tz = "UTC")
    yvar$Date <- as.Date(yvar[, 1])
    if(ref2 == "Surface water temperature") {
      yvar <- yvar[yvar[, 2] == min(yvar[, 2], na.rm = TRUE), c(1, 3)] # subset to Surface water temperature
    }
    yvar <- plyr::ddply(yvar, c("Date"), function(y) mean(y[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp

    df <- merge(xvar, yvar, by = "Date")

    validate(
      need(nrow(df) > 0, message = "No variables at matching timesteps.")
    )
    colnames(df)[-1] <- c("X", "Y")
    airt_swt$df <- df

  })

  # Plot airtemperature vs. surface water temperature
  output$airt_swt_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_airt_swt > 0,
           message = "Click 'Plot'")
    )
    p <- ggplot() +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(data = airt_swt$df, aes(X, Y), color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      theme_minimal(base_size = 18)
    return(p)
  })

  # Add dashed lines to build reg plot
  reg_line <- reactiveValues(m = 1, b = 0)
  sav_lines <- reactiveValues(m = 1, b = 0)
  observeEvent(input$draw_line, {
    reg_line$m <- input$m
    reg_line$b <- input$b
  })

  # Data table to store 10 lines
  lr_pars <- reactiveValues(dt = data.frame(m = rep(NA, 10), b = rep(NA, 10)))
  output$lr_DT <- renderDT(lr_pars$dt, selection = "single",
                           options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                          columnDefs = list(list(width = '10%', targets = "_all"))
                           ), colnames = c("Slope (m)", "Intercept (b)"),
                           server = FALSE, escape = FALSE)

  # Add red saved lines to plot & DT
  observeEvent(input$save_line, {
    if(input$save_line == 1) {
      sav_lines$m <- input$m
      sav_lines$b <- input$b
    } else {
      sav_lines$m <- c(sav_lines$m, input$m)
      sav_lines$b <- c(sav_lines$b, input$b)
    }
    if(!is.null(input$lr_DT_rows_selected)) {
      lr_pars$dt$m[input$lr_DT_rows_selected] <- input$m
      lr_pars$dt$b[input$lr_DT_rows_selected] <- input$b
    } else {
      idx <- which(is.na(lr_pars$dt$m))[1]
      lr_pars$dt$m[idx] <- input$m
      lr_pars$dt$b[idx] <- input$b
    }
  })

  # Plot with regression lines
  output$airt_swt_plot_lines <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_airt_swt > 0,
           message = "Click 'Plot'")
    )

    p <- ggplot() +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(data = airt_swt$df, aes(X, Y), color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      coord_cartesian(xlim = c(-5, 30), ylim = c(-5, 30)) +
      theme_minimal(base_size = 12)

    if(input$draw_line > 0) {
      p <- p +
        geom_abline(slope = reg_line$m, intercept = reg_line$b, color = "gray", linetype = "dashed")
    }
    if(input$save_line > 0) {
      p <- p +
        geom_abline(slope = lr_pars$dt$m, intercept = lr_pars$dt$b, color = "red", linetype = "solid")
    }
    return(ggplotly(p, dynamicTicks = TRUE))
  })

  # Calculate statistics from the lines drawn
  linr_stats <- reactiveValues(dt = data.frame("Mean (m)" = 0,
                                               "Std. Dev (m)" = 0,
                                               "Mean (b)" = 0,
                                               "Std. Dev (b)" = 0))
  observeEvent(input$calc_stats, {
    req(sum(!is.na(lr_pars$dt$m)) > 1)
    df <- data.frame("Mean (m)" = mean(lr_pars$dt$m, na.rm = TRUE),
                     "Std. Dev (m)" = sd(lr_pars$dt$m, na.rm = TRUE),
                     "Mean (b)" = mean(lr_pars$dt$b, na.rm = TRUE),
                     "Std. Dev (b)" = sd(lr_pars$dt$b, na.rm = TRUE))
    updateSliderInput(session, "m_std", value = df[1, 2])
    updateSliderInput(session, "b_std", value = df[1, 4])
    linr_stats$dt <- signif(df, 3)
  })

  output$lr_stats <- renderDT(linr_stats$dt, selection = "none",
                              options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                             columnDefs = list(list(width = '10%', targets = "_all"))
                              ),
                              colnames = c("Mean (m)", "Std. Dev (m)", "Mean (b)", "Std. Dev (b)"),
                              rownames = FALSE, container = sketch1,
                              server = FALSE, escape = FALSE)

  # Generate distribution plots
  lr_dist_plot <- reactiveValues(m = NA, b = NA)
  observeEvent(input$gen_lr_dist_plot, {
    lr_dist_plot$m <- rnorm(500, mean = linr_stats$dt[1, 1], sd = input$m_std)
    lr_dist_plot$b <- rnorm(500, mean = linr_stats$dt[1, 3], sd = input$b_std)
  })

  output$lr_m_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$gen_lr_dist_plot > 0, "Click 'Generate plot!'")
    )
    df <- data.frame(par = "Slope (m)", value = lr_dist_plot$m)

    p <- ggplot(df) +
      geom_vline(xintercept = linr_stats$dt[1, 1]) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      coord_cartesian(xlim = c(0, 2), ylim = c(0, 5)) +
      ggtitle("Slope (m)") +
      theme_minimal(base_size = 22)
    return(p)
  })
  output$lr_b_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$gen_lr_dist_plot > 0, "Click 'Generate plot!'")
    )
    df <- data.frame(par = "Intercept (b)", value = lr_dist_plot$b)

    p <- ggplot(df) +
      geom_vline(xintercept = linr_stats$dt[1, 3]) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      coord_cartesian(xlim = c(-2, 10), ylim = c(0, 5)) +
      ggtitle("Intercept (b)") +
      theme_minimal(base_size = 22)
    return(p)
  })

  # Sample m and b for plotting
  mb_samples <- reactiveValues(df = NULL)
  observeEvent(input$gen_lin_mods, {
    mb_samples$df <- signif(data.frame("m" = sample(lr_dist_plot$m, input$n_samp),
                                "b" = sample(lr_dist_plot$b, input$n_samp)), 3)
  })
  output$mb_samps <- renderDT(mb_samples$df, selection = "none",
                              # options = list(searching = FALSE, paging = TRUE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                              #                columnDefs = list(list(width = '10%', targets = "_all"))
                              # ),
                              colnames = c("Slope (m)", "Intercept (b)"),
                              rownames = FALSE,
                              server = FALSE, escape = FALSE)

  output$add_lin_mods <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_airt_swt > 0,
           message = "Click 'Plot'")
    )

    p <- ggplot() +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(data = airt_swt$df, aes(X, Y), color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      coord_cartesian(xlim = c(-5, 30), ylim = c(-5, 30)) +
      theme_minimal(base_size = 18)

    if(!is.null(mb_samples$df)) {
      p <- p +
        geom_abline(slope = mb_samples$df$m, intercept = mb_samples$df$b, color = "gray", linetype = "solid")
    }
    return(ggplotly(p, dynamicTicks = TRUE))
  })

})

# end

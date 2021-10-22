
# Icons
neonIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28),
  Terrestrial = makeIcon("icons/mountain-icon.png", iconWidth =  28, iconHeight = 28)
)

# Slides for slickR
model_slides <- list.files("www/model_slides", full.names = TRUE)

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")
met_pars <- read.csv("data/met_params.csv", fileEncoding = "UTF-8-BOM")

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
  siteID <- reactiveValues(lab = NULL)

  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')


  # Select NEON DT rows ----
  neon_chla <- reactiveValues(df = NULL)
  observeEvent(input$table01_rows_selected, {
    row_selected = neon_sites[input$table01_rows_selected, ]
    siteID$lab <- neon_sites$siteID[input$table01_rows_selected]
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

    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID$lab, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.POSIXct(chla[, 1], tz = "UTC")
    }
    neon_chla$df <- chla
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

    req(!is.null(siteID$lab))
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Accessing and downloading phenocam image",
                 detail = "This may take a while. This window will disappear
                     when it is downloaded.", value = 0.5)

    p <- input$neonmap_marker_click
    idx <- which(neon_sites_df$siteID == siteID$lab)
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

    ref <- "Air temperature"
    x_var <- neon_vars$id[which(neon_vars$Short_name == ref)][1]
    x_units <- neon_vars$units[which(neon_vars$Short_name == ref)][1]
    x_file <- file.path("data", "neon", paste0(siteID$lab, "_", x_var, "_", x_units, ".csv"))
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
    y_file <- file.path("data", "neon", paste0(siteID$lab, "_", y_var, "_", y_units, ".csv"))
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

  # Read in site data ----
  neon_DT <- reactive({ # view_var
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )

    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", "neon", paste0(siteID$lab, "_", read_var, "_", units, ".csv"))
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
    file <- file.path("data", "neon", paste0(siteID$lab, "_", read_var, "_", units, ".csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )
    obj <- neon_DT()$sel

    p <- ggplot() +
      geom_point(data = neon_DT()$data, aes_string(names(neon_DT()$data)[1], names(neon_DT()$data)[2]), color = "black") +
      ylab(paste0(input$view_var, " (", units, ")")) +
      xlab("Time") +
      theme_bw(base_size = 12)

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
  airt_swt <- reactiveValues(df = NULL, sub = NULL, sel = NULL)
  observeEvent(input$plot_airt_swt2, { # view_var

    req(!is.null(airt_swt$df))
    airt_swt$sub <- airt_swt$df[airt_swt$df$Date >= input$date1[1] & airt_swt$df$Date <= input$date1[2], ]
  })

  observe({
    airt_swt$sel <- tryCatch(airt_swt$df[(selected$sel$pointNumber+1),,drop=FALSE] , error=function(e){NULL})
  })

  # Plot air temperature vs. surface water temperature
  output$airt_swt_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$df),
           message = "Click 'Plot'")
    )
    validate(
      need(input$plot_airt_swt > 0,
           message = "Click 'Plot'")
    )
    df <- airt_swt$df
    df$X[is.na(df$Y)] <- NA
    df$Y[is.na(df$X)] <- NA
    p <- ggplot() +
      # geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_line(data = df, aes(Date, X, color = "Air temperature")) +
      geom_line(data = df, aes(Date, Y, color = "Water temperature")) +
      scale_color_manual(values = cols[5:6]) +
    # geom_point(data = airt_swt$df, aes(X, Y), color = "black") +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      theme_bw(base_size = 18)
    return(p)
  })

  output$date_slider1 <- renderUI({
    req(!is.null(airt_swt$df))
    sliderInput("date1", "Date", min = min(airt_swt$df$Date), max = max(airt_swt$df$Date), step = 1, value = c(min(airt_swt$df$Date), min(airt_swt$df$Date) + 100))
  })

  # Reset plot when adjusting dates
  observeEvent(input$date1, {
    airt_swt$sub <- NULL
    lm_fit$fit <- NULL
  })

  # Add dashed lines to build reg plot
  reg_line <- reactiveValues(m = 1, b = 0)
  sav_lines <- reactiveValues(m = 1, b = 0)
  observeEvent(input$draw_line, {
    reg_line$m <- input$m
    reg_line$b <- input$b
  })

  # Data table to store 10 lines
  lr_pars <- reactiveValues(dt = data.frame(m = rep(NA, 5), b = rep(NA, 5),
                                            start = rep(NA, 5), stop = rep(NA, 5),
                                            mean_err = rep(NA, 5),
                                            label = rep(NA, 5)))
  output$lr_DT <- renderDT(lr_pars$dt, selection = "single",
                           options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                          columnDefs = list(list(width = '10%', targets = "_all"))
                           ), colnames = c("m", "b", "Start", "Stop", "Cor.", "N"), rownames = TRUE,
                           server = FALSE, escape = FALSE)

  # Add red saved lines to plot & DT
  observeEvent(input$save_line, {
    req(!is.null(airt_swt$sub))
    req(!is.na(input$m))
    req(!is.na(input$b))
    if(input$save_line == 1) {
      sav_lines$m <- input$m
      sav_lines$b <- input$b
    } else {
      sav_lines$m <- c(sav_lines$m, input$m)
      sav_lines$b <- c(sav_lines$b, input$b)
    }
    mod <- (input$m * airt_swt$sub$X) + input$b
    df <- data.frame(obs = airt_swt$sub$Y, mod = mod)
    df <- na.exclude(df)
    # mean_err <- round(mean(mod - airt_swt$sub$Y, na.rm = TRUE), 2)
    mean_err <- round(cor(df$obs, df$mod), 2)
    if(!is.null(input$lr_DT_rows_selected)) {
      lr_pars$dt$m[input$lr_DT_rows_selected] <- input$m
      lr_pars$dt$b[input$lr_DT_rows_selected] <- input$b
      lr_pars$dt$start[input$lr_DT_rows_selected] <- as.character(input$date1[1])
      lr_pars$dt$stop[input$lr_DT_rows_selected] <- as.character(input$date1[2])
      lr_pars$dt$mean_err[input$lr_DT_rows_selected] <- mean_err
      lr_pars$dt$label[input$lr_DT_rows_selected] <- nrow(na.exclude(airt_swt$sub))
    } else {
      idx <- which(is.na(lr_pars$dt$m))[1]
      lr_pars$dt$m[idx] <- input$m
      lr_pars$dt$b[idx] <- input$b
      lr_pars$dt$start[idx] <- as.character(input$date1[1])
      lr_pars$dt$stop[idx] <- as.character(input$date1[2])
      lr_pars$dt$mean_err[idx] <- mean_err
      lr_pars$dt$label[idx] <- nrow(na.exclude(airt_swt$sub))
    }
  })

  # Plot with regression lines
  output$airt_swt_plot_lines <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.na(reg_line$m), "Ensure numeric values in the 'Slope (m)' box. (Check for spaces!)")
    )
    validate(
      need(!is.na(reg_line$b), "Ensure numeric values in the 'Intercept (b)' box. (Check for spaces!)")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    df <- na.exclude(airt_swt$sub)

    p <- ggplot() +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(data = df, aes(X, Y), color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      coord_cartesian(xlim = c(-5, 30), ylim = c(-5, 30)) +
      theme_bw(base_size = 12)

    if(input$draw_line > 0) {
      p <- p +
        geom_abline(slope = reg_line$m, intercept = reg_line$b, color = "gray", linetype = "dashed")
    }
    pars <- na.exclude(lr_pars$dt)
    if(nrow(pars) > 0) {
      pars$index <- factor(1:nrow(pars))
      p <- p +
        # geom_abline(slope = lr_pars$dt$m, intercept = lr_pars$dt$b, color = "red", linetype = "solid")
        geom_abline(data = pars, aes(slope = m, intercept = b, color = index),
                    linetype = "solid")
    }

    if(!is.null(lm_fit$fit)) {
      p <- p +
        geom_smooth(data = df, aes(X, Y), method = "lm", formula = "y ~ x")
    }

    return(ggplotly(p, dynamicTicks = TRUE))
  })

  # Plot water temp ts with model
  output$lm_ts_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$df),
           message = "Click 'Plot'")
    )
    validate(
      need(input$plot_airt_swt > 0,
           message = "Click 'Plot'")
    )
    df <- airt_swt$df
    df$X[is.na(df$Y)] <- NA
    df$Y[is.na(df$X)] <- NA

    pars <- na.exclude(lr_pars$dt)

    if(nrow(pars) > 0) {
      mod <- lapply(1:nrow(pars), function(x) {
        data.frame(Date = airt_swt$df$Date,
                   Model = pars$m[x] * airt_swt$df$X + pars$b[x])
      })
      # names(mod) <- pars$label
      mlt <- reshape::melt(mod, id.vars = "Date")
      colnames(mlt)[4] <- "index"
      mlt$index <- factor(mlt$index)
    }

    p <- ggplot() +
      # geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      # geom_line(data = df, aes(Date, X, color = "Air temperature")) +
      geom_point(data = df, aes(Date, Y, color = "Observed")) +
      # scale_color_manual(values = cols[5:6]) +
      # geom_point(data = airt_swt$df, aes(X, Y), color = "black") +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      theme_bw(base_size = 12)

    if(nrow(pars) > 0) {
      p <- p +
        geom_line(data = mlt, aes(Date, value, color = index)) +
        scale_color_manual(values = cols)
    }

    # return(p)
    return(ggplotly(p, dynamicTicks = TRUE))
  })

  # Add lm fit
  lm_fit <- reactiveValues(fit = NULL)
  observeEvent(input$add_lm, {
    req(!is.null(airt_swt$sub))
    df <- airt_swt$sub
    colnames(df)[2:3] <- c("airt", "wtemp")
    lm_fit$fit <- lm(wtemp ~ airt, data = df)
  })

  output$lm_out <- renderPrint({
    validate(
      need(!is.null(lm_fit$fit), "Click 'Fit linear model'")
    )
    summary(lm_fit$fit)
    # summary(lm_fit$fit)
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
  lr_dist_plot <- reactiveValues(m = NULL, b = NULL)
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
      ylab("Density") +
      xlab("Value") +
      ggtitle("Slope (m)") +
      theme_bw(base_size = 22)
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
      ylab("Density") +
      xlab("Value") +
      ggtitle("Intercept (b)") +
      theme_bw(base_size = 22)
    return(p)
  })

  # Sample m and b for plotting
  mb_samples <- reactiveValues(df = NULL)
  lm_dist <- reactiveValues(df = NULL)
  observeEvent(input$gen_lin_mods, {
    req(!is.null(input$n_samp))
    mb_samples$df <- signif(data.frame("m" = sample(lr_dist_plot$m, input$n_samp),
                                "b" = sample(lr_dist_plot$b, input$n_samp)), 3)
    # Create summary data frame
    x = c(-5, 35) # seq(-5, 35, 0.1)
    y = apply(mb_samples$df, 1, function(y) y[1]* x + y[2])

    lm_dist$df <- data.frame(x = x,
                             p025 = apply(y, 1, function(x) quantile(x, 0.025)),
                             p125 = apply(y, 1, function(x) quantile(x, 0.125)),
                             p875 = apply(y, 1, function(x) quantile(x, 0.875)),
                             p975 = apply(y, 1, function(x) quantile(x, 0.975)),
                             mean = apply(y, 1, function(x) mean(x)))
  })
  output$mb_samps <- renderDT(mb_samples$df, selection = "none",
                              # options = list(searching = FALSE, paging = TRUE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                              #                columnDefs = list(list(width = '10%', targets = "_all"))
                              # ),
                              colnames = c("Slope (m)", "Intercept (b)"),
                              rownames = FALSE,
                              server = FALSE, escape = FALSE)



  # Add linear models to plot ----
  output$add_lin_mods <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(input$n_samp), "Select number of samples")
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
      theme_bw(base_size = 16)

    if(!is.null(mb_samples$df) & !input$add_dist) {
      p <- p +
        geom_abline(slope = mb_samples$df$m, intercept = mb_samples$df$b, color = "gray", linetype = "solid")
    }
    if(input$add_dist) {
      p <- p +
        geom_ribbon(data = lm_dist$df, aes(x, ymin = p025, ymax = p975, fill = "95%"), alpha = 0.3) +
        geom_ribbon(data = lm_dist$df, aes(x, ymin = p125, ymax = p875, fill = "75%"), alpha = 0.3) +
        geom_line(data = lm_dist$df, aes(x, mean, color = "Mean")) +
        scale_fill_manual(values = l.cols)
    }
    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })

  # Investigate model error
  output$mod_err_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(input$n_samp), "Select number of samples")
    )
    validate(
      need(input$plot_airt_swt > 0,
           message = "Click 'Plot'")
    )
    validate(
      need(input$gen_lin_mods > 0,
           message = "Add lines")
    )


    p <- ggplot(data = airt_swt$df, aes(X, Y)) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      coord_cartesian(xlim = c(-5, 35), ylim = c(-5, 35)) +
      theme_bw(base_size = 16)

    p <- p +
      geom_line(data = lm_dist$df, aes(x, mean, color = "Linear regression")) +
      scale_color_manual(values = cols[2])

    return(p)
  })

  click_df <- reactiveValues(df = NULL)
  observe({
    req(!is.null(lm_dist$df))
    pnts <- nearPoints(airt_swt$df, input$mod_err_plot_click)[, -1]
    # class(pnts)
    brsh <- brushedPoints(airt_swt$df, brush = input$mod_err_plot_brush)[, -1]
    df <- rbind(pnts, brsh)
    df <- round(df, 1)
    # idx <- which(lm_dist$df$x %in% df$X)
    df$`Modelled Water Temp.` <- linr_stats$dt[1, 1] * df$X + linr_stats$dt[1, 3]
    colnames(df)[1:2] <- c("Air Temp.", "Observed Water Temp.")
    df$Error <- df[, 3] - df[, 2]
    click_df$df <- df
    mean_err$val <- NULL
  })


  output$click_dt <- renderDT(click_df$df, selection = "none",
                            colnames = c("Air Temp.", "Obs. Water Temp.", "Mod. Water Temp.", "Mod - Obs"),
                            options = list(pageLength = 5),
                              rownames = FALSE,
                              server = FALSE, escape = FALSE)

  # Calculate mean error
  mean_err <- reactiveValues(val = NULL)
  observeEvent(input$calc_err, {
    req(!is.null(click_df$df))
    mean_err$val <- mean(click_df$df$Error, na.rm = TRUE)
  })

  output$mean_err <- renderText({
    validate(
      need(!is.null(click_df$df), "Please select points on the plot.")
    )
    validate(
      need(!is.null(mean_err$val), "Click calculate.")
    )
    paste0("The mean model error is ", round(mean_err$val, 1), " \u00B0C.")
  })

  # Calculate error w/ UC
  output$mod_err_uc_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(input$n_samp), "Select number of samples")
    )
    validate(
      need(input$plot_airt_swt > 0,
           message = "Click 'Plot'")
    )
    validate(
      need(input$gen_lin_mods > 0,
           message = "Add lines")
    )

    p <- ggplot() +
      geom_ribbon(data = lm_dist$df, aes(x, ymin = p025, ymax = p975, fill = "95%"), alpha = 0.3) +
      geom_ribbon(data = lm_dist$df, aes(x, ymin = p125, ymax = p875, fill = "75%"), alpha = 0.3) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_point(data = airt_swt$df, aes(X, Y), color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      # coord_cartesian(xlim = c(-5, 35), ylim = c(-5, 35)) +
      theme_bw(base_size = 16)

    p <- p +
      geom_line(data = lm_dist$df, aes(x, mean, color = "Linear regression")) +
      scale_color_manual(values = cols[7]) +
      scale_fill_manual(values = l.cols)
    if(nrow(airt_swt$sel) != 0) {
      p <- p +
        geom_point(data = airt_swt$sel, aes(X, Y), color = cols[2])
    }

    gp <- ggplotly(p, dynamicTicks = TRUE, source = "ci_sel")
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })

  selected <- reactiveValues(sel = NULL)

  observeEvent(input$clear_sel1, {
    print("Test1")
    airt_swt$sel <- NULL
    selected$sel <- NULL
  })

  #selected
  observe({
    # suppress warnings
    storeWarn<- getOption("warn")
    options(warn = -1)
    selected$sel <- event_data(event = "plotly_selected", source = "ci_sel")

    #restore warnings, delayed so plot is completed
    shinyjs::delay(expr =({
      options(warn = storeWarn)
    }) ,ms = 100)
  })

  output$sel_points <- renderText({
    n <- 0
    if(!is.null(airt_swt$sel)) {
      n <- nrow(airt_swt$sel)
    }
    paste0("Number of selected points: ", n)
  })

  output$total_points <- renderText({
    validate(
      need(!is.null(airt_swt$df), "Need SOMETHING?")
    )
    n_pnts = nrow(na.exclude(airt_swt$df))
    paste0("Total number of points: ", n_pnts)
  })

  pct_msg <- reactiveValues(txt = NULL)
  observeEvent(input$calc_pct, {
    n_pnts = nrow(na.exclude(airt_swt$df))
    insid <- n_pnts - (input$points_above + input$points_below)
    pct_msg$txt <- paste0("Percentage points inside the CI: ", round((insid * 100 / n_pnts), 2), "%")
  })

  output$pct_inside <- renderText({
    validate(
      need(!is.null(pct_msg$txt), "Click calculate")
    )
    pct_msg$txt
  })

  # Build a forecast model ----
  output$mult_lin_reg_eqn <- renderUI({
    validate(
      need(!is.null(input$mult_lin_reg_vars),
           message = "Please select predictors.")
    )
    idx <- which(lin_reg_vars$Name %in% input$mult_lin_reg_vars)
    formula <- paste0("$$ wtemp =  ", paste0("\\beta_{", 1:length(idx), "} * ", lin_reg_vars$latex[idx], collapse = " + "), " + b $$")
    withMathJax(
      tags$p(formula)
    )
  })

  mlr_fit <- reactiveValues(lst = list())
  mlr_pred <- reactiveValues(lst = list())

  observeEvent(input$fit_mlr, {
    req(!is.null(input$mult_lin_reg_vars))

    inp_row <- which(is.na(mlr$dt$Equation))[1]

    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$Y,
                      airt = airt_swt$df$X,
                      wtemp_lag = NA, wtemp_mean = NA,
                      airt_lag = NA, airt_mean = NA)
    dat$wtemp_lag[-c(1:input$lag_t)] <- dat$wtemp[-c((nrow(dat)+1-input$lag_t):nrow(dat))]
    dat$wtemp_mean <- c(NA, zoo::rollmean(dat$wtemp[-nrow(dat)], input$mean_t, na.pad = TRUE, align = "right"))
    dat$airt_lag[-c(1:input$lag_t)] <- dat$airt[-c((nrow(dat)+1-input$lag_t):nrow(dat))]
    dat$airt_mean <- c(NA, zoo::rollmean(dat$airt[-nrow(dat)], input$mean_t, na.pad = TRUE, align = "right"))

    idx <- which(lin_reg_vars$Name %in% input$mult_lin_reg_vars)

    dat <- dat[, c("Date", "wtemp", "airt", lin_reg_vars$var[idx])]

    # Subset to training data
    train <- dat[dat$Date >= input$train_date[1] & dat$Date <= input$train_date[2], ]

    eval(parse(text = paste0("fit <- lm(wtemp ~ ", paste0(lin_reg_vars$var[idx], collapse = " + "), ", data = train)")))

    mlr_out$txt <- summary(fit)

    coeffs <- round(fit$coefficients, 2)
    if(coeffs[1] > 0) {
      b <- paste0("+", coeffs[1])
    } else {
      b <- coeffs[1]
    }
    # coeffs <- round(coeffs[c(2:(length(coeffs)), 1)], 2)
    mod <- predict(fit, dat)
    df2 <- data.frame(obs = dat$wtemp, mod = mod, diff = mod - dat$wtemp)
    df2 <- na.exclude(df2)
    r2 <- round(cor(df2$obs, df2$mod), 2)
    err <- mean(mod - dat$wtemp, na.rm = TRUE)

    if(!is.null(input$mlr_dt_rows_selected)) {
      mlr$dt$Equation[input$mlr_dt_rows_selected] <- paste0("$$ wtemp =  ", paste0(coeffs[-1], " * ", lin_reg_vars$latex[idx], collapse = " + "), b, " $$")
      mlr$dt$lag[input$mlr_dt_rows_selected] <- input$lag_t
      mlr$dt$mean_day[input$mlr_dt_rows_selected] <- input$mean_t
      mlr$dt$mean_err[input$mlr_dt_rows_selected] <- r2
      mlr_pred$lst[[input$mlr_dt_rows_selected]] <- data.frame(Date = dat$Date,
                                            Model = mod)
      mlr_fit$lst[[input$mlr_dt_rows_selected]] <- fit
    } else if(inp_row <= 5) {
      # idx <- which(is.na(lr_pars$dt$m))[1]
      mlr$dt$Equation[inp_row] <- paste0("$$ wtemp =  ", paste0(coeffs[-1], " * ", lin_reg_vars$latex[idx], collapse = " + "), b, " $$")
      mlr$dt$lag[inp_row] <- input$lag_t
      mlr$dt$mean_day[inp_row] <- input$mean_t
      mlr$dt$mean_err[inp_row] <- r2
      mlr_pred$lst[[inp_row]] <- data.frame(Date = dat$Date,
                                            Model = mod)
      mlr_fit$lst[[inp_row]] <- fit
    }

  })

  mlr_out <- reactiveValues(txt = NULL)

  output$mlr_out <- renderPrint({
    validate(
      need(!is.null(input$mult_lin_reg_vars),
           message = "Please select predictors.")
    )
    validate(
      need(!is.null(mlr_out$txt), "Click 'Fit model'")
    )
    mlr_out$txt
  })

  observeEvent(input$mult_lin_reg_vars, {
    mlr_out$txt <- NULL
  })



  mlr <- reactiveValues(dt = data.frame(Equation = rep(NA, 5),
                                        lag = rep(NA, 5),
                                        mean_day = rep(NA, 5),
                                        mean_err = rep(NA, 5))
                        )

  output$mlr_dt <- renderDT(mlr$dt, selection = "single",
                            options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                           columnDefs = list(list(width = '10%', targets = "_all")) #,
    #                                        drawcallback = JS("function( settings ) {
    #     // MathJax.Hub.Config({
    #     //    tex2jax: {inlineMath: [['$','$'],['\\(','\\)']]}
    #     // });
    #
    #     MathJax.Hub.Queue(['Typeset',MathJax.Hub]);
    # }")
                            ), colnames = c("Equation", "Lag (days)", "Mean (days)", "Cor."), rownames = TRUE,

                            server = FALSE, escape = FALSE)

  # Date slider for training & test data
  output$date_train <- renderUI({
    req(!is.null(airt_swt$df))
    bump <- round(nrow(airt_swt$df)/2)
    sliderInput("train_date", "Date", min = min(airt_swt$df$Date), max = max(airt_swt$df$Date), step = 1, value = c(min(airt_swt$df$Date), airt_swt$df$Date[bump]))
  })

  output$date_test <- renderUI({
    req(!is.null(airt_swt$df))
    bump <- round(nrow(airt_swt$df)/2)
    sliderInput("test_date", "Date", min = min(airt_swt$df$Date), max = max(airt_swt$df$Date), step = 1, value = c(airt_swt$df$Date[bump+1], max(airt_swt$df$Date)))
  })

  #** Plot water temp ts with MLR model----
  output$mlr_ts_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$df),
           message = "Click 'Plot'")
    )

    df <- airt_swt$df
    df$X[is.na(df$Y)] <- NA
    df$Y[is.na(df$X)] <- NA

    # separate into train & test
    df$per <- NA
    df$per[df$Date >= input$train_date[1] & df$Date <= input$train_date[2]] <- "Training"
    df$per[df$Date >= input$test_date[1] & df$Date <= input$test_date[2]] <- "Testing"
    df <- df[!is.na(df$per), ]
    df$per <- factor(df$per, levels = c("Training", "Testing"))


    if(nrow(na.exclude(mlr$dt)) > 0) {
      mlt <- reshape::melt(mlr_pred$lst, id.vars = "Date")
      colnames(mlt)[4] <- "Label"
      mlt$Label <- as.character(mlt$Label)

      # separate into train & test
      mlt$per <- NA
      mlt$per[mlt$Date >= input$train_date[1] & mlt$Date <= input$train_date[2]] <- "Training"
      mlt$per[mlt$Date >= input$test_date[1] & mlt$Date <= input$test_date[2]] <- "Testing"
      mlt <- mlt[!is.na(mlt$per), ]
      mlt$per <- factor(mlt$per, levels = c("Training", "Testing"))
    }

    p <- ggplot() +
      geom_point(data = df, aes(Date, Y, color = "Observed")) +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      facet_wrap(~per, scales = "free_x") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      theme_bw(base_size = 12)

    if(nrow(na.exclude(mlr$dt)) > 0) {
      p <- p +
        geom_line(data = mlt, aes(Date, value, color = Label)) +
        scale_color_manual(values = cols)
    }

    # return(p)
    return(ggplotly(p, dynamicTicks = TRUE))
  })


  # Activity A ----

  # Load NOAA airT
  noaa_df <- reactiveValues(airt = NULL, swr = NULL)
  observeEvent(input$load_noaa_at, {

    req(input$table01_rows_selected != "")

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = paste0("Loading NOAA forecast data"),
                 detail = "This may take a while. This window will disappear
                     when it is finished loading.", value = 0.1)

    fpath <- file.path("data", "NOAAGEFS_1hr", siteID$lab)
    fc_date <- list.files(fpath)[1]
    fpath2 <- file.path(fpath, fc_date[1], "00")
    fils <- list.files(fpath2, full.names = TRUE)
    fils <- fils[-c(grep("ens00", fils))]
    fid <- ncdf4::nc_open(file.path(fils[1]))
    vars <- fid$var # Extract variable names for selection
    fc_vars <- names(vars)[c(1, 5)] # Extract air temp
    membs <- length(fils)
    ncdf4::nc_close(fid)

    out <- lapply(fc_date, function(dat) {
      idx <- which(fc_date == dat)

      fpath2 <- file.path(fpath, dat, "00")
      fils <- list.files(fpath2)
      fils <- fils[-c(grep("ens00", fils))]

      for( i in seq_len(length(fils))) {

        fid <- ncdf4::nc_open(file.path("data", "NOAAGEFS_1hr", siteID$lab, dat,
                                        "00", fils[i]))
        tim = ncvar_get(fid, "time")
        tunits = ncatt_get(fid, "time")
        lnam = tunits$long_name
        tustr <- strsplit(tunits$units, " ")
        step = tustr[[1]][1]
        tdstr <- strsplit(unlist(tustr)[3], "-")
        tmonth <- as.integer(unlist(tdstr)[2])
        tday <- as.integer(unlist(tdstr)[3])
        tyear <- as.integer(unlist(tdstr)[1])
        tdstr <- strsplit(unlist(tustr)[4], ":")
        thour <- as.integer(unlist(tdstr)[1])
        tmin <- as.integer(unlist(tdstr)[2])
        origin <- as.POSIXct(paste0(tyear, "-", tmonth,
                                    "-", tday, " ", thour, ":", tmin),
                             format = "%Y-%m-%d %H:%M", tz = "UTC")
        if (step == "hours") {
          tim <- tim * 60 * 60
        }
        if (step == "minutes") {
          tim <- tim * 60
        }
        time = as.POSIXct(tim, origin = origin, tz = "UTC")
        var_list <- lapply(fc_vars, function(x) {
          if(x == "air_temperature") {
            data.frame(time = time, value = (ncdf4::ncvar_get(fid, x) -  273.15))
          } else {
            data.frame(time = time, value = (ncdf4::ncvar_get(fid, x)))
          }
        })

        ncdf4::nc_close(fid)
        names(var_list) <- fc_vars

        mlt1 <- reshape::melt(var_list, id.vars = "time")
        mlt1 <- mlt1[, c("time", "L1", "value")]

        # df <- get_vari(file.path("data", fils[i]), input$fc_var, print = F)
        cnam <- paste0("mem", formatC(i, width = 2, format = "d", flag = "0"))
        if(i == 1) {
          df2 <- mlt1
          colnames(df2)[3] <- cnam
        } else {
          df2 <- merge(df2, mlt1, by = c(1,2))
          colnames(df2)[ncol(df2)] <- cnam
        }

      }
      progress$set(value = i/length(fils))
      return(df2)
    })

    names(out) <- fc_date
    # out$`2020-09-25`$L1 <- "Air temperature"
    idx <- which(met_pars$Site == siteID$lab)

    noaa_df$airt <- reshape::melt(out[[1]][out[[1]]$L1 == "air_temperature", ], id.vars = c("time", "L1"))
    noaa_df$swr <- reshape::melt(out[[1]][out[[1]]$L1 == "surface_downwelling_shortwave_flux_in_air", ], id.vars = c("time", "L1"))
    noaa_df$swt <- noaa_df$airt
    noaa_df$swt$L1 <- "surface_water_temperature"
    noaa_df$swt$value <- met_pars$airt_m[idx] * noaa_df$swt$value + met_pars$airt_b[idx]
    noaa_df$upar <- noaa_df$swr
    noaa_df$upar$L1 <- "underwater_photosynthetically_active_radiation"
    noaa_df$upar$value <- met_pars$swr_m[idx] * noaa_df$upar$value + met_pars$swr_b[idx]
  })

  output$noaa_at_loaded <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt), "Please click 'Load forecast'")
    )
    return(paste0("Forecast loaded for ", siteID$lab))
  })

  # output$noaa_at_plot <- renderPlot({
  output$noaa_at_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt), "Please click 'Load forecast'")
    )
    validate(
      need(is.numeric(input$noaa_n_mems), "Please input a number for 'Number of forecasts'")
    )
    # validate(
    #   need(input$view_day0, "Please click 'Load observation'")
    # )
    set.seed(123)


    mlt <- noaa_df$airt

    if(input$noaa_timestep == "Daily mean") {
      mlt$date <- as.Date(mlt$time)
      mlt <- plyr::ddply(mlt, c("date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
      mlt$time <- as.POSIXct(mlt$date)
      fut_offset <- lubridate::days(6) #+ lubridate::hours(19)
    } else if(input$noaa_timestep == "Hourly") {
      fut_offset <- lubridate::days(6) + lubridate::hours(19)
    }

    idx1 <- which(mlt$time == mlt$time[1])
    dat <- mlt[idx1, ]
    dat$time2 <- dat$time + rnorm(nrow(dat), mean = 0, sd = 6000)
    idx2 <- which(mlt$time == (mlt$time[1] + fut_offset))
    dat2 <- mlt[idx2, ]
    dat2$time2 <- dat2$time + rnorm(nrow(dat2), mean = 0, sd = 6000)

    st <- data.frame(time = c(mlt$time[1], (mlt$time[1] + fut_offset)),
                     mean = c(mean(dat$value, na.rm = TRUE), mean(dat2$value, na.rm = TRUE)),
                     sd = c(sd(dat$value, na.rm = TRUE), sd(dat2$value, na.rm = TRUE)),
                     null = rep(mean(dat$value, na.rm = TRUE), 2),
                     L1 = dat$L1[1])

    if(input$view_day7) {
      ylims <- c(min(mlt$value, na.rm = TRUE) - 1, max(mlt$value, na.rm = TRUE) + 1)
      xlims <- c(mlt$time[1], (mlt$time[1] + fut_offset))
    } else {
      xlims <- c((mlt$time[1] - lubridate::hours(12)), (mlt$time[1] + lubridate::hours(12)))
      ylims <- c(min(dat$value, na.rm = TRUE) - 1, max(dat$value, na.rm = TRUE) + 1)
    }

    dat <- dat[dat$variable %in% paste0("mem", formatC(1:input$noaa_n_mems, width = 2, format = "d", flag = "0")), ]
    dat2 <- dat2[dat2$variable %in% paste0("mem", formatC(1:input$noaa_n_mems, width = 2, format = "d", flag = "0")), ]

    if(input$noaa_n_mems > 0 & input$view_day7) {
      cur_df <- data.frame(time = dat$time2, value = dat$value, variable = dat$variable, xend = dat2$time2, yend = dat2$value)
    }


    ylab <- "Temperature (\u00B0C)"
    xlab <- "Time"

    p <- ggplot() +
      # geom_point(data = dat2, aes(time2, value)) + # Day 7
      # geom_line(data = mlt[mlt$time <= xlims[2], ], aes(time, value, group = variable)) +
      # geom_errorbar(data = st, aes(time, ymin = mean - sd, ymax = mean + sd),
      #               color = "red", width = 12000, size = 2) + # Error bars - Std. Dev
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      # stat_ellipse(data = dat2, aes(time2, value), na.rm = TRUE, inherit.aes = FALSE) +
      # facet_wrap(~L1, scales = "free_y", ncol = 1) +
      ylab(ylab) +
      xlab(xlab) +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      theme_bw(base_size = 18)

    if(input$view_day0) {
      p <- p +
        geom_point(data = st[1, ], aes(time, mean), color = "red", size = 5) # Mean
    }

    if(input$add_obs_uc) {
      p <- p + geom_errorbar(data = st[1, ], aes(time, ymin = mean - sd, ymax = mean + sd),
                        color = "red", width = 12000, size = 2) # Error bars - Std. Dev

    }
    if(input$view_ic & input$noaa_n_mems > 0) {
      p <- p +
        geom_point(data = dat, aes(time2, value), size = 2) + # Day 1
        # geom_ellipse(aes(x0 = (st$time[1] - 6000), y0 = st$mean[1], a = (2 * 6000), b = (2 * st$sd[1]), angle = 0))
        stat_ellipse(data = dat, aes(time2, value), na.rm = TRUE,
                     inherit.aes = FALSE, type = "norm")
    }
    if(input$view_day7 & input$noaa_n_mems > 0) {
      p <- p +
        geom_point(data = dat2, aes(time2, value), size = 2) + # Day 1
        stat_ellipse(data = dat2, aes(time2, value), na.rm = TRUE,
                     inherit.aes = FALSE, type = "norm")
    }

    if(!is.null(input$add_to_plot)) {

      if(input$view_ic & input$noaa_n_mems > 0 & input$view_day7 & input$add_to_plot == "Line") {
        p <- p +
          geom_segment(data = cur_df, aes(time, value, group = variable, xend = xend, yend = yend),
                       color = "gray", alpha = 0.6)
        # geom_curve(data = cur_df, aes(time, value, group = variable, xend = xend, yend = yend),
        #            curvature = -0.2) # Connecting curves
      }
      if(input$view_ic & input$noaa_n_mems > 0 & input$view_day7 & input$add_to_plot == "Actual data") {

        mlt <- mlt[mlt$variable %in% paste0("mem", formatC(1:input$noaa_n_mems, width = 2, format = "d", flag = "0")), ]
        p <- p +
          geom_line(data = mlt[mlt$time <= xlims[2], ], aes(time, value, group = variable),
                    color = "gray", alpha = 0.6)
      }
      if(input$view_ic & input$noaa_n_mems > 0 & input$view_day7 & input$add_to_plot == "Distribution") {

        validate(
          need(input$noaa_n_mems >= 2, "Number of members must be greater than 1.")
        )

        wid <- tidyr::pivot_wider(mlt, c(time, L1), names_from = variable, values_from = value)
        wid <- wid[wid$time <= (mlt$time[1] + fut_offset), 1:(input$noaa_n_mems + 2)]
        df <- apply(wid[, -c(1, 2)], 1, function(x){
          quantile(x, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
        })
        df <- as.data.frame(t(df))
        colnames(df) <- paste0("p", gsub("%", "", colnames(df)))
        df$time <- wid$time
        p <- p +
          geom_ribbon(data = df, aes(time, ymin = p2.5, ymax = p97.5, fill = "95%"), alpha = 0.3)+
          geom_ribbon(data = df, aes(time, ymin = p12.5, ymax = p87.5, fill = "75%"), alpha = 0.3)+
          geom_line(data = df, aes(time, p50, color = "Median"))
      }
    }

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    # return(p)

    return(gp)
  })

  # Run forecasts with IC UC ----
  # Generate Initial condition distribution plots
  ic_dist_plot <- reactiveValues(phy = NULL, nut = NULL, phy_xlims = NULL, nut_xlims = NULL)
  observeEvent(input$gen_ic_dist, {
    req(!is.null(input$n_samp_ic))
    ic_dist_plot$phy <- rnorm(input$n_samp_ic, mean = input$phy_ic_value, sd = input$phy_ic_sd)
    ic_dist_plot$phy[ic_dist_plot$phy <= 0] <- 0.01
    ic_dist_plot$nut <- rnorm(input$n_samp_ic, mean = input$nut_ic_value, sd = input$nut_ic_sd)
    ic_dist_plot$nut[ic_dist_plot$nut <= 0] <- 0.01
    ic_dist_plot$phy_xlims <- c(input$phy_ic_value - 3 * input$phy_ic_sd, input$phy_ic_value + 3 * input$phy_ic_sd)
    ic_dist_plot$nut_xlims <- c(input$nut_ic_value - 3 * input$nut_ic_sd, input$nut_ic_value + 3 * input$nut_ic_sd)
    ic_dist_plot$phy_vline <- input$phy_ic_value
    ic_dist_plot$nut_vline <- input$nut_ic_value
  })

  output$ic_phy_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(input$n_samp_ic),
           message = "Please select a number of samples.")
    )
    validate(
      need(input$gen_ic_dist > 0, "Click 'Generate initial condition distributions'")
    )
    df <- data.frame(par = "Chlorophyll-a (μg/L)", value = ic_dist_plot$phy)

    p <- ggplot(df) +
      geom_vline(xintercept = ic_dist_plot$phy_vline) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      coord_cartesian(xlim = ic_dist_plot$phy_xlims, ylim = c(0, 5)) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Chlorophyll-a (μg/L)") +
      theme_bw(base_size = 22)
    return(p)
  })
  output$ic_nut_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(input$n_samp_ic),
           message = "Please select a number of samples.")
    )
    validate(
      need(input$gen_ic_dist > 0, "Click 'Generate initial condition distributions'")
    )
    df <- data.frame(par = "Nutrients (mg/L)", value = ic_dist_plot$nut)

    p <- ggplot(df) +
      geom_vline(xintercept = ic_dist_plot$nut_vline) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      coord_cartesian(xlim = ic_dist_plot$nut_xlims, ylim = c(0, 5)) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Nutrient (mg/L)") +
      theme_bw(base_size = 22)
    return(p)
  })

  # Run IC FC ----
  ic_fc_data <- reactiveValues(chla = NULL, nut = NULL)
  observeEvent(input$run_ic_fc, {

    req(!is.null(noaa_df$airt))

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running NP model with ", length(ic_dist_plot$phy), " different sets of initial conditions."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running", value = 0.01)

    swt <- noaa_df$swt
    swt$date <- as.Date(swt$time)
    swt_dly <- plyr::ddply(swt, "date", function(x) mean(x$value, na.rm = TRUE))

    upar <- noaa_df$upar
    upar$date <- as.Date(upar$time)
    upar_dly <- plyr::ddply(upar, "date", function(x) mean(x$value, na.rm = TRUE))

    np_inp <- merge(swt_dly, upar_dly, by = 1)
    np_inp[, 1] <- as.POSIXct(np_inp[, 1], tz = "UTC")
    times <- 1:nrow(np_inp)

    np_inputs <- create_np_inputs(time = np_inp[, 1], PAR = np_inp[, 3], temp = np_inp[, 2])

    # Parameters for NP model
    parms <- c(
      maxUptake = 0.2, #day-1
      kspar=120, #uEinst m-2 s-1
      ksdin=0.5, #mmol m-3
      maxGrazing=1.0, # day-1
      ksphyto=1, #mmol N m-3
      pFaeces=0.3, #unitless
      mortalityRate=0.8, #(mmmolN m-3)-1 day-1
      excretionRate=0.1, #day-1
      mineralizationRate=0.1, #day-1
      Chl_Nratio = 1, #mg chl (mmolN)-1
      Q10 = 2,  #unitless
      refTEMP = 20 # Reference temperature for q10
    )

    n_mem <- length(ic_dist_plot$phy)
    arr <- array(NA, dim = c(8, 3, n_mem))
    for(mem in 1:n_mem) {
      res <- matrix(NA, nrow = 8, ncol = 3, dimnames = list(rn = c(), cn = c("Phyto", "Nutrient", "Chla")))
      res[1, 1] <- ic_dist_plot$phy[mem] * 0.016129 # Convert from μg/L to mmolN/m3
      res[1, 2] <- ic_dist_plot$nut[mem] * 16.129 # Convert from mg/L to mmolN/m3
      res[1, 3] <- res[1, 1]  * 62
      for(i in 2:8) {

        out <- NP_model(time = i, states = res[i - 1, 1:2], parms = parms, inputs = np_inputs)
        res[i, ] <- c((res[i-1, 1] + out[[1]][1]),
                      (res[i-1, 2] + out[[1]][2]),
                      (res[i-1, 1] + out[[1]][1]) * 62)

      }
      arr[, , mem] <- res
      progress$set(value = mem/n_mem)
    }

    mlt <- reshape2::melt(arr[, 3, ])
    mlt$date <- np_inp$date[1:8]
    ic_fc_data$chla <- mlt

    mlt <- reshape2::melt(arr[, 2, ])
    mlt$date <- np_inp$date[1:8]
    mlt$value <- mlt$value / 16.129
    ic_fc_data$nut <- mlt
  })

  output$ic_fc_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(ic_dist_plot$phy),
           "Please generate distributions of initial conditions required")
    )
    validate(
      need(!is.null(input$ic_fc_type),
           "Please select a type of plot")
    )
    validate(
      need(!is.null(ic_fc_data$chla),
           message = "Click 'Run forecast'")
    )

    dat <- ic_fc_data$chla

    p <- ggplot()

    if(input$ic_fc_type == "Distribution") {
      wid <- tidyr::pivot_wider(dat, c(date), names_from = Var2, values_from = value)
      df <- apply(wid[, -c(1)], 1, function(x){
        quantile(x, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      df <- as.data.frame(t(df))
      colnames(df) <- paste0("p", gsub("%", "", colnames(df)))
      df$time <- wid$date
      p <- p +
        geom_ribbon(data = df, aes(time, ymin = p2.5, ymax = p97.5, fill = "95%"), alpha = 0.3)+
        geom_ribbon(data = df, aes(time, ymin = p12.5, ymax = p87.5, fill = "75%"), alpha = 0.3)+
        geom_line(data = df, aes(time, p50, color = "Median"))
      } else if(input$ic_fc_type == "Line") {
      p <- p +
        geom_line(data = dat, aes_string("date", "value", group = "Var2"),
                  color = "gray", alpha = 0.6)
      }
    p <- p +
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      theme_bw(base_size = 18)

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
        }
      }
    return(gp)
    })

  # Model Uncertainty ----
  #* Process Uncertainty ----
  mod0_runs <- reactiveValues(curr = NULL, prev = NULL,  prev2 = NULL,
                              none = NULL, low = NULL, med = NULL, high = NULL)
  observeEvent(input$run_mod0, {

    req(input$table01_rows_selected != "")
    req(!is.null(noaa_df$airt))

    swt <- noaa_df$swt
    swt$date <- as.Date(swt$time)
    swt_dly <- plyr::ddply(swt, "date", function(x) mean(x$value, na.rm = TRUE))

    upar <- noaa_df$upar
    upar$date <- as.Date(upar$time)
    upar_dly <- plyr::ddply(upar, "date", function(x) mean(x$value, na.rm = TRUE))

    np_inp <- merge(swt_dly, upar_dly, by = 1)
    np_inp[, 1] <- as.POSIXct(np_inp[, 1], tz = "UTC")
    times <- 1:nrow(np_inp)

    np_inputs <- create_np_inputs(time = np_inp[, 1], PAR = np_inp[, 3], temp = np_inp[, 2])

    # Parameters for NP model
    parms <- c(
      maxUptake = 0.2, #day-1
      kspar=120, #uEinst m-2 s-1
      ksdin=0.5, #mmol m-3
      maxGrazing=1.0, # day-1
      ksphyto=1, #mmol N m-3
      pFaeces=0.3, #unitless
      mortalityRate=0.8, #(mmmolN m-3)-1 day-1
      excretionRate=0.1, #day-1
      mineralizationRate=0.1, #day-1
      Chl_Nratio = 1, #mg chl (mmolN)-1
      Q10 = 2,  #unitless
      refTEMP = 20 # Reference temperature for q10
    )

    parms[1] <- 0.15
    parms[7] <- 0.85
    if(input$proc_uc0 == "None") {
      w_sd <- 0
    } else if(input$proc_uc0 == "Low") {
      w_sd <- 0.01
    } else if(input$proc_uc0 == "Medium") {
      w_sd <- 0.025
    } else if(input$proc_uc0 == "High") {
      w_sd <- 0.05
    }
    sig_w <- rnorm(8, mean = 0, sd = w_sd)

    res <- matrix(NA, nrow = 8, ncol = 3, dimnames = list(rn = c(), cn = c("Phyto", "Nutrient", "Chla")))
    res[1, 1] <- input$phy_ic_value * 0.016129 # Convert from μg/L to mmolN/m3
    res[1, 2] <- input$nut_ic_value * 16.129 # Convert from mg/L to mmolN/m3
    res[1, 3] <- res[1, 1]  * 62
    for(i in 2:8) {

      out <- NP_model(time = i, states = res[i - 1, 1:2], parms = parms, inputs = np_inputs)
      new_phy <- res[i-1, 1] + out[[1]][1] + sig_w[i]
      new_nut <- res[i-1, 2] + out[[1]][2]
      if(new_phy <= 0) {
        new_phy <- 0.01
      }
      if(new_nut <= 0) {
        new_nut <- 1
      }
      res[i, ] <- c(new_phy,
                    new_nut,
                    (new_phy * 62))
    }



    res <- as.data.frame(res)
    res$date <- np_inp[1:8, 1]
    if(input$proc_uc0 == "None") {
      mod0_runs$none <- res
    } else if(input$proc_uc0 == "Low") {
      mod0_runs$low <- res
    } else if(input$proc_uc0 == "Medium") {
      mod0_runs$med <- res
    } else if(input$proc_uc0 == "High") {
      mod0_runs$high <- res
    }

    mod0_runs$curr <- res
  })

  output$proc_uc_plot <- renderPlot({


    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    # validate(
    #   need(input$proc_uc0 != "None",
    #        message = "Add process uncertainty")
    # )

    xlims <- c((0 - 3*0.05), (0 + 3*0.05))
    ylims <- c(0, 40)
    df <- data.frame(Low = rnorm(1000, mean = 0, sd = 0.01),
                     Medium = rnorm(1000, mean = 0, sd = 0.025),
                     High = rnorm(1000, mean = 0, sd = 0.05))

    p <- ggplot(df) +
      geom_vline(xintercept = 0) +
      geom_density(aes(x = Low, fill = "Low"), alpha = 0.6) +
      geom_density(aes(x = Medium, fill = "Medium"), alpha = 0.6) +
      geom_density(aes(x = High, fill = "High"), alpha = 0.6) +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      scale_fill_manual(values = c("Low" = cols[2], "Medium" = cols[3], "High" = cols[4])) +
      guides(fill = guide_legend(override.aes = list(alpha = 0.6))) +
      labs(fill = "Level") +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Process Uncertainty") +
      theme_bw(base_size = 22)
    return(p)
    })

  output$run_mod0_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(mod0_runs$curr),
           message = "Click 'Run model'")
    )

    p <- ggplot()

    if(!is.null(mod0_runs$none)) {
      p <- p +
        geom_line(data = mod0_runs$none, aes(date, Chla, color = "None"),
                  linetype = "solid")
    }
    if(!is.null(mod0_runs$low)) {
      p <- p +
        geom_line(data = mod0_runs$low, aes(date, Chla, color = "Low"),
                  linetype = "twodash")
    }
    if(!is.null(mod0_runs$med)) {
      p <- p +
        geom_line(data = mod0_runs$med, aes(date, Chla, color = "Medium"),
                  linetype = "dotted")
    }
    if(!is.null(mod0_runs$high)) {
      p <- p +
        geom_line(data = mod0_runs$high, aes(date, Chla, color = "High"),
                  linetype = "dashed")
    }

    p <- p +
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      scale_color_manual(values = c("None" = cols[1], "Low" = cols[2], "Medium" = cols[3], "High" = cols[4])) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      labs(color = "Process uncertainty") +
      theme_bw(base_size = 18)

    return(ggplotly(p, dynamicTicks = TRUE))

  })


  #* Run model & explore parameters

  mod1_runs <- reactiveValues(curr = NULL, prev = NULL,  prev2 = NULL,
                              pars_df = data.frame(matrix(NA, nrow = 3, ncol = 3, dimnames = list(c("Current run", "Previous run", "Previous run2"), c("Mortality rate", "Nutrient uptake", "Reference temperature")))),
                              pars_curr = NULL, pars_prev = NULL, pars_prev2 = NULL)
  observeEvent(input$run_mod1, {

    req(input$table01_rows_selected != "")
    req(!is.null(noaa_df$airt))

    mod1_runs$prev2 <- mod1_runs$prev
    mod1_runs$prev <- mod1_runs$curr

    mod1_runs$pars_df[3, ] <- mod1_runs$pars_df[2, ]
    mod1_runs$pars_df[2, ] <- mod1_runs$pars_df[1, ]
    mod1_runs$pars_df[1, ] <- c(input$mort_rate1, input$nut_uptake1, input$refTEMP1)


    swt <- noaa_df$swt
    swt$date <- as.Date(swt$time)
    swt_dly <- plyr::ddply(swt, "date", function(x) mean(x$value, na.rm = TRUE))

    upar <- noaa_df$upar
    upar$date <- as.Date(upar$time)
    upar_dly <- plyr::ddply(upar, "date", function(x) mean(x$value, na.rm = TRUE))

    np_inp <- merge(swt_dly, upar_dly, by = 1)
    np_inp[, 1] <- as.POSIXct(np_inp[, 1], tz = "UTC")
    times <- 1:nrow(np_inp)

    np_inputs <- create_np_inputs(time = np_inp[, 1], PAR = np_inp[, 3], temp = np_inp[, 2])

    # Parameters for NP model
    parms <- c(
      maxUptake = 0.2, #day-1
      kspar=120, #uEinst m-2 s-1
      ksdin=0.5, #mmol m-3
      maxGrazing=1.0, # day-1
      ksphyto=1, #mmol N m-3
      pFaeces=0.3, #unitless
      mortalityRate=0.8, #(mmmolN m-3)-1 day-1
      excretionRate=0.1, #day-1
      mineralizationRate=0.1, #day-1
      Chl_Nratio = 1, #mg chl (mmolN)-1
      Q10 = 2,  #unitless
      refTEMP = 20 # Reference temperature for q10
    )

    parms[1] <- input$nut_uptake1
    parms[7] <- input$mort_rate1
    parms[12] <- input$refTEMP1

    res <- matrix(NA, nrow = 8, ncol = 3, dimnames = list(rn = c(), cn = c("Phyto", "Nutrient", "Chla")))
    res[1, 1] <- input$phy_ic_value * 0.016129 # Convert from μg/L to mmolN/m3
    res[1, 2] <- input$nut_ic_value * 16.129 # Convert from mg/L to mmolN/m3
    res[1, 3] <- res[1, 1]  * 62
    for(i in 2:8) {

      out <- NP_model(time = i, states = res[i - 1, 1:2], parms = parms, inputs = np_inputs)
      res[i, ] <- c((res[i-1, 1] + out[[1]][1]),
                    (res[i-1, 2] + out[[1]][2]),
                    (res[i-1, 1] + out[[1]][1]) * 62)

    }

    res <- as.data.frame(res)
    res$date <- np_inp[1:8, 1]

    mod1_runs$curr <- res
    mod1_runs$pars_curr <- c(parms[7], parms[1], parms[12])
  })

  output$run_mod1_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(mod1_runs$curr),
           message = "Click 'Run model'")
    )

    p <- ggplot()

    if(!is.null(mod1_runs$prev2)) {
      p <- p +
        geom_line(data = mod1_runs$prev2, aes(date, Chla, color = "Previous run2"),
                  linetype = "dotted")
    }
    if(!is.null(mod1_runs$prev)) {
      p <- p +
        geom_line(data = mod1_runs$prev, aes(date, Chla, color = "Previous run"),
                  linetype = "dashed")
    }

    p <- p +
      geom_line(data = mod1_runs$curr, aes(date, Chla, color = "Current run")) +
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      theme_bw(base_size = 18)

    return(ggplotly(p, dynamicTicks = TRUE))

  })

  #** Data table of used parameters ----
  output$run_mod1_pars <- renderDT(
    mod1_runs$pars_df, rownames = TRUE, options = list(ordering = FALSE, dom = 't'),
    colnames = c("Mortality rate", "Nutrient uptake", "Reference temperature")
  )

  #* Generate parameters ----
  pars_dist <- reactiveValues(mort_rate = NULL, nut_uptake = NULL, mort_xlims = NULL, nut_xlims = NULL)
  plot_switch <- reactiveValues(pars_UC = FALSE)
  observeEvent(input$gen_param_dist, {

    req(!is.null(input$n_samp_pars))
    if(input$add_nut_uc) {
      pars_dist$nut_uptake <- data.frame(value = rnorm(input$n_samp_pars, mean = input$nut_uptake2, sd = input$nut_uptake2_sd),
                                         par = "Nutrient uptake")
      pars_dist$nut_uptake[pars_dist$nut_uptake <= 0] <- 0.01
      pars_dist$nut_uptake[pars_dist$nut_uptake >= 1] <- 0.99
      pars_dist$nut_uptake_xlims <- c(input$nut_uptake2 - 3 * input$nut_uptake2_sd, input$nut_uptake2 + 3 * input$nut_uptake2_sd)
      pars_dist$nut_uptake_vline <- input$nut_uptake2
    }

    if(input$add_mort_uc) {
      pars_dist$mort_rate <- data.frame(value = rnorm(input$n_samp_pars, mean = input$mort_rate2, sd = input$mort_rate2_sd), par =  "Mortality rate")
      pars_dist$mort_rate[pars_dist$mort_rate <= 0] <- 0.01
      pars_dist$mort_rate[pars_dist$mort_rate >= 1] <- 0.99
      pars_dist$mort_rate_xlims <- c(input$mort_rate2 - 3 * input$mort_rate2_sd, input$mort_rate2 + 3 * input$mort_rate2_sd)
      pars_dist$mort_rate_vline <- input$mort_rate2
    }
    plot_switch$pars_UC <- FALSE
  })

  #** Mortality rate distribution plot ----
  output$mort_rate_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$add_mort_uc, "Check 'Add uncertainty for mortality rate'")
    )
    validate(
      need(!is.null(input$n_samp_pars), "Select the number of samples")
    )
    validate(
      need(!is.null(pars_dist$mort_rate), "Click 'Generate parameter distributions'")
    )

    df <- pars_dist$mort_rate

    p <- ggplot(df) +
      geom_vline(xintercept = pars_dist$mort_rate_vline) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      coord_cartesian(xlim = c(0, 1)) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Mortality rate") +
      theme_bw(base_size = 22)
    return(p)
  })

  #** Nutrient uptake distribution plot ----
  output$nut_uptake_dist_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$add_nut_uc, "Check 'Add uncertainty for nutrient uptake'")
    )
    validate(
      need(!is.null(pars_dist$nut_uptake), "Click 'Generate parameter distributions'")
    )

    df <- pars_dist$nut_uptake

    p <- ggplot(df) +
      geom_vline(xintercept = pars_dist$nut_uptake_vline) +
      geom_density(aes(x = value), fill = "gray", alpha = 0.6) +
      coord_cartesian(xlim = c(0, 1)) +
      ylab("Density") +
      xlab("Value") +
      ggtitle("Nutrient uptake") +
      theme_bw(base_size = 22)
    return(p)
  })


  # Run Param UC FC ----
  pars_fc_data <- reactiveValues(chla = NULL, nut = NULL)
  observeEvent(input$run_pars_fc, {

    req(!is.null(noaa_df$airt))
    req(!is.null(input$pars_fc_type))
    plot_switch$pars_UC <- TRUE

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running NP model with ", input$n_samp_pars, " different sets of parameters."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running", value = 0.01)

    swt <- noaa_df$swt
    swt$date <- as.Date(swt$time)
    swt_dly <- plyr::ddply(swt, "date", function(x) mean(x$value, na.rm = TRUE))

    upar <- noaa_df$upar
    upar$date <- as.Date(upar$time)
    upar_dly <- plyr::ddply(upar, "date", function(x) mean(x$value, na.rm = TRUE))

    np_inp <- merge(swt_dly, upar_dly, by = 1)
    np_inp[, 1] <- as.POSIXct(np_inp[, 1], tz = "UTC")
    times <- 1:nrow(np_inp)

    np_inputs <- create_np_inputs(time = np_inp[, 1], PAR = np_inp[, 3], temp = np_inp[, 2])

    # Parameters for NP model
    parms <- c(
      maxUptake = 0.2, #day-1
      kspar=120, #uEinst m-2 s-1
      ksdin=0.5, #mmol m-3
      maxGrazing=1.0, # day-1
      ksphyto=1, #mmol N m-3
      pFaeces=0.3, #unitless
      mortalityRate=0.8, #(mmmolN m-3)-1 day-1
      excretionRate=0.1, #day-1
      mineralizationRate=0.1, #day-1
      Chl_Nratio = 1, #mg chl (mmolN)-1
      Q10 = 2,  #unitless
      refTEMP = 20 # Reference temperature for q10
    )

    n_mem <- nrow(pars_dist$mort_rate)
    arr <- array(NA, dim = c(8, 3, n_mem))
    for(mem in 1:n_mem) {

      if(input$add_mort_uc) {
        parms[7] <- pars_dist$mort_rate$value[mem]
      }
      if(input$add_nut_uc) {
        parms[1] <- pars_dist$nut_uptake$value[mem]
      }

      res <- matrix(NA, nrow = 8, ncol = 3, dimnames = list(rn = c(), cn = c("Phyto", "Nutrient", "Chla")))
      res[1, 1] <- input$phy_ic_value * 0.016129 # Convert from μg/L to mmolN/m3
      res[1, 2] <- input$nut_ic_value * 16.129 # Convert from mg/L to mmolN/m3
      res[1, 3] <- res[1, 1]  * 62

      for(i in 2:8) {
        out <- NP_model(time = i, states = res[i - 1, 1:2], parms = parms, inputs = np_inputs)
        res[i, ] <- c((res[i-1, 1] + out[[1]][1]),
                      (res[i-1, 2] + out[[1]][2]),
                      (res[i-1, 1] + out[[1]][1]) * 62)
      }
      res[res[, 3] > 50 | res[, 3] < 0, 3] <- NA # Reset outlier values
      arr[, , mem] <- res
      progress$set(value = mem/n_mem)
    }

    mlt <- reshape2::melt(arr[, 3, ])
    mlt$date <- np_inp$date[1:8]
    pars_fc_data$chla <- mlt

    mlt <- reshape2::melt(arr[, 2, ])
    mlt$date <- np_inp$date[1:8]
    mlt$value <- mlt$value / 16.129
    pars_fc_data$nut <- mlt
  })

  output$pars_fc_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(pars_dist$mort_rate),
           "Please generate distributions of parameters required")
    )
    validate(
      need(!is.null(input$pars_fc_type),
           message = "Select a type of plot")
    )
    validate(
      need(plot_switch$pars_UC,
           message = "Click 'Run forecast'")
    )

    dat <- pars_fc_data$chla

    p <- ggplot()

    if(input$pars_fc_type == "Distribution") {
      wid <- tidyr::pivot_wider(dat, c(date), names_from = Var2, values_from = value)
      df <- apply(wid[, -c(1)], 1, function(x){
        quantile(x, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975), na.rm = TRUE)
      })
      df <- as.data.frame(t(df))
      colnames(df) <- paste0("p", gsub("%", "", colnames(df)))
      df$time <- wid$date
      p <- p +
        geom_ribbon(data = df, aes(time, ymin = p2.5, ymax = p97.5, fill = "95%"), alpha = 0.3)+
        geom_ribbon(data = df, aes(time, ymin = p12.5, ymax = p87.5, fill = "75%"), alpha = 0.3)+
        geom_line(data = df, aes(time, p50, color = "Median"))
    } else if(input$pars_fc_type == "Line") {
      p <- p +
        geom_line(data = dat, aes_string("date", "value", group = "Var2"),
                  color = "gray", alpha = 0.6)
    }
    p <- p +
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      theme_bw(base_size = 18)

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })

  # Run Driver UC FC ----
  driv_fc_data0 <- reactiveValues(chla = NULL, nut = NULL)
  rand_samp <- reactiveValues(val = NULL)
  observeEvent(input$run_driv_fc0, {

    req(!is.null(noaa_df$airt))
    # plot_switch$pars_UC <- TRUE

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running NP model."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running", value = 0.01)

    # Parameters for NP model
    parms <- c(
      maxUptake = 0.12, #day-1
      kspar=120, #uEinst m-2 s-1
      ksdin=0.5, #mmol m-3
      maxGrazing=1.0, # day-1
      ksphyto=1, #mmol N m-3
      pFaeces=0.3, #unitless
      mortalityRate=0.85, #(mmmolN m-3)-1 day-1
      excretionRate=0.1, #day-1
      mineralizationRate=0.1, #day-1
      Chl_Nratio = 1, #mg chl (mmolN)-1
      Q10 = 2,  #unitless
      refTEMP = 20 # Reference temperature for q10
    )

    n_mem <- 30
    arr <- array(NA, dim = c(8, 3, n_mem))
    for(mem in 1:n_mem) {

      sel_mem <- paste0("mem", formatC(mem, width = 2, format = "d", flag = "0"))

      swt <- noaa_df$swt
      swt <- swt[swt$variable == sel_mem, ]
      swt$date <- as.Date(swt$time)
      swt_dly <- plyr::ddply(swt, "date", function(x) mean(x$value, na.rm = TRUE))

      upar <- noaa_df$upar
      upar <- upar[upar$variable == sel_mem, ]
      upar$date <- as.Date(upar$time)
      upar_dly <- plyr::ddply(upar, "date", function(x) mean(x$value, na.rm = TRUE))

      np_inp <- merge(swt_dly, upar_dly, by = 1)
      np_inp[, 1] <- as.POSIXct(np_inp[, 1], tz = "UTC")
      times <- 1:nrow(np_inp)

      np_inputs <- create_np_inputs(time = np_inp[, 1], PAR = np_inp[, 3], temp = np_inp[, 2])

      res <- matrix(NA, nrow = 8, ncol = 3, dimnames = list(rn = c(), cn = c("Phyto", "Nutrient", "Chla")))
      res[1, 1] <- input$phy_ic_value * 0.016129 # Convert from μg/L to mmolN/m3
      res[1, 2] <- input$nut_ic_value * 16.129 # Convert from mg/L to mmolN/m3
      res[1, 3] <- res[1, 1]  * 62

      for(i in 2:8) {
        out <- NP_model(time = i, states = res[i - 1, 1:2], parms = parms, inputs = np_inputs)
        res[i, ] <- c((res[i-1, 1] + out[[1]][1]),
                      (res[i-1, 2] + out[[1]][2]),
                      (res[i-1, 1] + out[[1]][1]) * 62)
      }
      # res[res[, 3] > 50 | res[, 3] < 0, 3] <- NA # Reset outlier values
      arr[, , mem] <- res
    }

    mlt <- reshape2::melt(arr[, 3, ])
    mlt$date <- np_inp$date[1:8]
    driv_fc_data0$chla <- mlt

    mlt <- reshape2::melt(arr[, 2, ])
    mlt$date <- np_inp$date[1:8]
    mlt$value <- mlt$value / 16.129
    driv_fc_data0$nut <- mlt

    rand_samp$val <- sample(1:30, 1)

    progress$set(value = 1)
  })

  output$driv_fc_plot0 <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(driv_fc_data0$chla),
           message = "Click 'Run forecast'")
    )

    dat <- driv_fc_data0$chla
    dat$label <- paste0("mem", formatC((dat$Var2), width = 2, format = "d", flag = "0"))
    ylims <- range(dat$value, na.rm = TRUE)
    if(input$add_mem > 0) {
      add_dat <- dat[dat$Var2 %in% c(2:(input$add_mem + 1)), ]
    }
    dat <- dat[dat$Var2 == 1, ]
    col30 <- cols

    p <- ggplot() +
      geom_line(data = dat, aes(date, value, color = label)) +
      scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      coord_cartesian(ylim = ylims) +
      theme_bw(base_size = 18)

    if(input$add_mem > 0) {
      p <- p +
        geom_line(data = add_dat, aes(date, value, color = label))
    }
    if(input$add_mem > 7) {
      col30 <- c(rep("black", input$add_mem - 7), cols)
    }
    p <- p + scale_color_manual(values = col30) +
      guides(color = "none")

    return(p)
  })

  output$driv_fc_plot1 <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df$airt),
           "Load NOAA weather forecast")
    )
    validate(
      need(!is.null(driv_fc_data0$chla),
           message = "Click 'Run forecast'")
    )

    swt <- noaa_df$swt
    # swt <- swt[swt$variable == sel_mem, ]
    swt$date <- as.Date(swt$time)
    dat <- plyr::ddply(swt, c("date", "variable"), function(x) {
      data.frame(value = mean(x$value, na.rm = TRUE))
    })

    sub_dates <- unique(dat$date)[1:8]

    ylims <- range(dat$value, na.rm = TRUE)
    if(input$add_mem > 0) {
      add_dat <- dat[dat$variable %in% paste0("mem", formatC((2:(input$add_mem + 1)), width = 2, format = "d", flag = "0")) & dat$date %in% sub_dates, ]
      }
    dat <- dat[dat$variable == "mem01" & dat$date %in% sub_dates, ]
    col30 <- cols


    p <- ggplot() +
      geom_line(data = dat, aes(date, value, color = variable)) +
      scale_x_date(date_labels = "%a", date_breaks = "1 day") +
      ylab("Water temperature (\u00B0C)") +
      xlab("Time") +
      coord_cartesian(ylim = ylims) +
      theme_bw(base_size = 18)

    if(input$add_mem > 0) {
      p <- p +
        geom_line(data = add_dat, aes(date, value, color = variable))
    }
    if(input$add_mem > 7) {
      col30 <- c(rep("black", input$add_mem - 7), cols)
    }
    p <- p + scale_color_manual(values = col30) +
      guides(color = "none")

    return(p)
  })


})

# end

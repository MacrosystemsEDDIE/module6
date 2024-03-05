shinyServer(function(input, output, session) {

  #### Header ----
  
  # Help button
  observeEvent(input$help, {
    introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
  })
  
  #### Presentation ----
  
  # Slickr summary output
  output$slides <- renderSlickR({
    slickR(recap_slides) + settings(dots = TRUE, autoplay = FALSE)
  })
  
  #### Introduction ----
  
  # Hide download button until report is generated
  output$handoutbuilt <- reactive({
    return(file.exists("report.docx"))
  })
  outputOptions(output, 'handoutbuilt', suspendWhenHidden= FALSE)
  
  handout_file <- "Student_handout.docx"
  
  output$stud_dl <-  downloadHandler(
    filename = function() {
      handout_file
    },
    content = function(file) {
      file.copy("report.docx", file)
    }
  )
  
  #### Activity A ----
  
  ## Objective 1 - Select NEON site ----

  # NEON Sites datatable ----
  output$table01 <- DT::renderDT(
    neon_sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE, dom = 't'), server = FALSE
  )
  
  observe({
    if(input$row_num != "") {
      dt_proxy <- dataTableProxy("table01")
      selectRows(dt_proxy, input$row_num)
    }
  })

  # to keep track of previously selected row
  prev_row <- reactiveVal()
  siteID <- reactiveValues(lab = NULL)

  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  # Select NEON DT rows ----
  
  # air temperature forecast loads as soon as site is selected
  airt1_fc <- reactiveValues(df = NULL)
  
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

    # Load one airt fc
    fpath <- file.path("data", "NOAAGEFS_1hr", siteID$lab)
    fc_date <- list.files(fpath)[1]
    fpath2 <- file.path(fpath, fc_date[1], "00")
    fils <- list.files(fpath2, full.names = TRUE)
    fils <- fils[-c(grep("ens00", fils))]
    fid <- ncdf4::nc_open(file.path(fils[1]))
    vars <- fid$var # Extract variable names for selection
    fc_vars <- names(vars)[c(1)] # Extract air temp
    membs <- 1 #length(fils)
    ncdf4::nc_close(fid)

    out <- lapply(fc_date, function(dat) {
      idx <- which(fc_date == dat)

      fpath2 <- file.path(fpath, dat, "00")
      fils <- list.files(fpath2)
      fils <- fils[-c(grep("ens00", fils))]
      fils <- fils[1]

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

        cnam <- paste0("mem", formatC(i, width = 2, format = "d", flag = "0"))
          df2 <- mlt1
          colnames(df2)[3] <- cnam
        df3 <- data.frame(Date = as.character(as.Date(df2$time)),
                          value = df2$mem01)
        df4 <- plyr::ddply(df3, c("Date"), function(x) data.frame(value = mean(x[, 2], na.rm = TRUE)))
        df4$Date <- as.Date(df4$Date)
        df4 <- df4[df4$Date <= "2020-10-02", ]

      }
      return(df4)
    })
    airt1_fc$df <- out[[1]]
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
  
  # CREATING A BUNCH OF REACTIVE VALUES THAT WILL BE POPULATED WHEN SITE IS SELECTED
  # A df of air and water temperature data is also created when site is selected 
  airt_swt <- reactiveValues(df = NULL, sub = NULL, sel = NULL)
  
  # Also a df that will be used to plot the persistence model
  persist_df <- reactiveValues(df = NULL)
  
  #Also another data frame of water temperature forecast data that is referred to in subsequent objectives
  wtemp_fc_data <- reactiveValues(lst = as.list(rep(NA, 4)), hist = NULL, fut = NULL)
  
  # this code will run when site is selected
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
    df$month <- lubridate::month(df$Date)
    df <- df[(df$month %in% 5:10), 1:3]
    colnames(df)[-1] <- c("airt", "wtemp")
    airt_swt$df <- df
    df$Mod <- c(NA, df$wtemp[-nrow(df)])
    persist_df$df <- df


    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp, airt = airt_swt$df$airt)

    wtemp_fc_data$hist <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
    wtemp_fc_data$hist$wtemp[wtemp_fc_data$hist$Date > as.Date("2020-09-25")] <- NA
    wtemp_fc_data$hist$airt[wtemp_fc_data$hist$Date > as.Date("2020-09-25")] <- NA
    wtemp_fc_data$fut <- dat[dat$Date > as.Date("2020-09-25") & dat$Date <= "2020-10-02", ]
    wtemp_fc_data$fut$airt[wtemp_fc_data$fut$Date > as.Date("2020-09-25")] <- NA


  })

  ### Objective 2 - View water temperature data ----
  
  # Plot surface water temperature
  plot.airt_swt <- reactiveValues(main=NULL)
  
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    validate(
      need(input$plot_airt_swt > 0,
           message = "Click 'Plot water temperature'")
    )
    
    df <- airt_swt$df
    df$airt[is.na(df$wtemp)] <- NA
    df$wtemp[is.na(df$airt)] <- NA
    
    p <- ggplot() +
      geom_point(data = df, aes(Date, wtemp, color = "Water temperature")) +
      scale_color_manual(values = c("Water temperature" = cols[5], "Air temperature" = cols[6])) +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      labs(color = NULL) +
      theme_bw(base_size = 12)
    
    plot.airt_swt$main <- ggplotly(p, dynamicTicks = TRUE)
  })
  
  observe({
  
  output$airt_swt_plot <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_airt_swt > 0 | exists("up_answers"),
           message = "Click 'Plot water temperature'")
    )
    
    plot.airt_swt$main })
  
  })
  
  # Plot air temperature and water temperature
  plot.airt_swt1 <- reactiveValues(main=NULL)
    
  output$airt_swt_plot1 <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(airt_swt$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(input$plot_airt_swt2 > 0,
             message = "Click 'Plot air temperature'")
      )
      
      df <- airt_swt$df
      df$airt[is.na(df$wtemp)] <- NA
      df$wtemp[is.na(df$airt)] <- NA
      
      p <- ggplot() +
        geom_point(data = df, aes(Date, wtemp, color = "Water temperature")) +
        geom_point(data = df, aes(Date, airt, color = "Air temperature")) +
        scale_color_manual(values = c("Water temperature" = cols[5], "Air temperature" = cols[6])) +
        ylab("Temperature (\u00B0C)") +
        xlab("Time") +
        labs(color = NULL) +
        theme_bw(base_size = 12)
      
      plot.airt_swt1$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
       })
    
  # Download plot of air and water temperature
  output$save_at_swt_plot <- downloadHandler(
    filename = function() {
      paste("Q8a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.airt_swt1$main, device = device)
    }
  )
  
  # Output stats ----
  output$out_stats <- renderText({
    
    validate(
      need(!is.null(airt_swt$df),
           message = "Please select a site in Objective 1.")
    )
    
    sum_stat <- summary(airt_swt$df$wtemp)
    ridx <- grep(input$stat_calc, names(sum_stat))
    out_stat <- paste(input$stat_calc,":",round(sum_stat[ridx],2), sep = " ")
    
    return(out_stat)
  })
  
  output$out_stats1 <- renderText({
    
    validate(
      need(!is.null(airt_swt$df),
           message = "Please select a site in Objective 1.")
    )
    
    sum_stat <- summary(airt_swt$df$airt)
    ridx <- grep(input$stat_calc1, names(sum_stat))
    out_stat <- paste(input$stat_calc1,":",round(sum_stat[ridx],2), sep = " ")
    
    return(out_stat)
  })
  
  # )
  
  q6_ans <- reactiveValues(dt = q6_table) # %>% formatStyle(c(1:3), border = '1px solid #ddd'))
  
  output$q6_tab <- DT::renderDT(
    q6_ans$dt, 
    selection = "none", class = "cell-border stripe",
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"),
    server = FALSE, escape = FALSE, rownames= c("Water temperature"), colnames=c("Mean", "Min.", "Max."), editable = FALSE
  )
  
  q8_ans <- reactiveValues(dt = q8_table) # %>% formatStyle(c(1:3), border = '1px solid #ddd'))
  
  output$q8_tab <- DT::renderDT(
    q8_ans$dt, 
    selection = "none", class = "cell-border stripe",
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"),
    server = FALSE, escape = FALSE, rownames= c("Air temperature"), colnames=c("Mean", "Min.", "Max."), editable = FALSE
  )
  
  
  ## Objective 3 - Build models ----
  
  # Slickr model output
  output$model_slides <- renderSlickR({
    slickR(model_slides) + settings(dots = TRUE, autoplay = FALSE)
  })
  
  # Create reactive value for model selection table
  mod_selec_tab <- reactiveValues(dt = data.frame(eqn = rep(NA, 4),
                                                  mean = rep(0, 4),
                                                  lag = rep(0, 4),
                                                  r2 = rep(NA, 4),
                                                  rmse = rep(NA, 4)))
  
  # Persistence model
  
  persist_plot <- reactiveValues(main = NULL, layer1 = NULL)
  
  observe({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    df <- persist_df$df
    df <- df[df$Date > "2020-01-01", ]
    
    persist_plot$main <- ggplot() +
      geom_point(data = df, aes(Date, wtemp, color = "Obs")) +
      ylab("Water temperature (\u00B0C)") +
      xlab("Time") +
      scale_color_manual(values = c("Mod" = cols[3], "Obs" = "black"), name = "") +
      theme_bw(base_size = 16)
    
  })
  
  observe({
    validate(
      need(input$plot_persist > 0,
           message = "Click 'Plot'.")
    )
    df <- persist_df$df
    df <- df[df$Date > "2020-01-01", ]
    persist_plot$layer1 <- geom_line(data = df, aes(Date, Mod, color = "Mod"))
    mod_selec_tab$dt$eqn[1] <- "$$wtemp_{t+1} = wtemp_{t}$$"
    
  })
  
    output$persist_plot <- renderPlotly({
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      if(!is.null(persist_plot$layer1)){
        return(ggplotly(persist_plot$main + persist_plot$layer1, dynamicTicks = TRUE))
      } else {
        return(ggplotly(persist_plot$main, dynamicTicks = TRUE))
      }
    })
  
  # download plot of persistence model predictions
  output$save_pers_model_pred_plot <- downloadHandler(
    filename = function() {
      paste("Q11a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 6, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = persist_plot$main + persist_plot$layer1, device = device)
    }
  )
  
  observe({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_persist > 0, "Click 'Plot'.")
    )
    
    df <- persist_df$df
    df <- na.exclude(df)
  
    fit <- lm(wtemp ~ Mod, data = df)
    out <- summary(fit)
    
    r2 <- round(out$r.squared, 2) # round(cor(df$wtemp, df$Mod), 2)
    rmse <- round(sqrt(mean((df$Mod - df$wtemp)^2, na.rm = TRUE)), 2)
    mod_selec_tab$dt$eqn[1] <- "$$wtemp_{t+1} = wtemp_{t}$$"
    mod_selec_tab$dt$r2[1] <- r2
    mod_selec_tab$dt$rmse[1] <- rmse
    withMathJax(
      tags$p(paste0("$$RMSE = ", rmse, "\u00B0C$$"))
    )
  })
  
  # Simple linear regression 1: Plot water temp today vs water temp yesterday
  
  observe({ 
    
    validate(
      need(input$plot_airt_swt3 > 0,
           message = "Click 'Plot'")
    )
    
    req(!is.null(airt_swt$df))
    
    idx_dates <- seq.Date(airt_swt$df$Date[1], to = airt_swt$df$Date[nrow(airt_swt$df)], by = "1 day")
    
    airt_swt$sub <- airt_swt$df[airt_swt$df$Date %in% idx_dates, ]
  })

  # Fitting linear regression
  lr_pars <- reactiveValues(dt = data.frame(m_est = rep(NA, 1), m_se = rep(NA, 1),
                                            b_est = rep(NA, 1), b_se = rep(NA, 1),
                                            r2 = rep(NA, 1), N = rep(NA, 1), rmse = rep(NA, 1)))
 
  lr_eqn <- reactiveValues(dt = data.frame(m = rep(NA, 1), b = rep(NA, 1),
                                            r2 = rep(NA, 1), N = rep(NA, 1)))
  
  output$lr_DT <- renderDT(lr_eqn$dt[, c(1,2, 4)], selection = "none",
                           options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                          columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                           ), colnames = c("Slope (m)","Intercept (b)", "N"),
                           rownames = c("Model parameters"),
                           server = FALSE, escape = FALSE)
  output$lr_DT2 <- renderDT(lr_pars$dt[, c("m_est", "m_se", "b_est", "b_se", "rmse", "N")], selection = "single",
                           options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                          columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                           ),
                           # colnames = c("m", "b", "R-squared", "N"),
                           colnames = c("m", "m (Std. Dev.)", "b", "b (Std. Dev.)", "RMSE", "N"),
                           rownames = c("Monthly", "Fortnightly", "Weekly", "Daily"),
                           server = FALSE, escape = FALSE)

  output$r2_tab <- renderDT(data.frame(lr_pars$dt$rmse), selection = "none",
                               options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                              columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                               ), colnames = c("RMSE"),
                               rownames = c("Monthly", "Fortnightly", "Weekly", "Daily"),
                               server = FALSE, escape = FALSE)


  # Plot with regression lines
  swt_swt_plot_lines <- reactiveValues(main = NULL)
  
  lm_fit <- reactiveValues(fit = NULL, df_lst = list(), eqn = NULL)
  
  wt_reg_ts_plot <- reactiveValues(main = NULL)
  
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot data'")
    )
    
    validate(
      need(input$plot_airt_swt3 > 0,
           message = "Click 'Plot data'")
    )
    
    req(!is.null(airt_swt$sub))
    tot_rows <- nrow(na.exclude(airt_swt$df))
    df <- airt_swt$sub
    df <- na.exclude(df)
    colnames(df)[2:3] <- c("airt", "wtemp")
    df <- df %>%
      mutate(wtemp_yday = dplyr::lag(wtemp)) %>%
      filter(year(Date) == 2020)
    fit <- lm(df$wtemp ~ df$wtemp_yday)
    out <- summary(fit)
    
    
    idx <- 1
    
    lr_pars$dt$m_est[idx] <- round(out$coefficients[2, 1], 2)
    lr_pars$dt$b_est[idx] <- round(out$coefficients[1, 1], 1)
    lr_pars$dt$m_se[idx] <- round(out$coefficients[2, 2], 2)
    lr_pars$dt$b_se[idx] <- round(out$coefficients[1, 2], 1)
    lr_pars$dt$r2[idx] <- round(out$r.squared, 2)
    lr_pars$dt$N[idx] <- nrow(df)
    lr_pars$dt$rmse[idx] <- 0
    
    # }
    
    df$N <- nrow(df)
    
    lm_fit$df_lst[[idx]] <- df
    
    lm_fit$fit <- fit
    
    
    # For model selection table
    mod_selec_tab$dt$eqn[2] <- paste0("$$wtemp_{t+1} =  ", round(out$coefficients[2, 1], 2), " \\times wtemp_{t} + ", round(out$coefficients[1, 1], 2), "$$")
    mod_selec_tab$dt$r2[2] <- round(out$r.squared, 2)
    
    
    mx <- max(df$wtemp_yday, df$wtemp, na.rm = TRUE) + 2
    mn <- min(df$wtemp_yday, df$wtemp, na.rm = TRUE) - 2
    
    sgmnt <- data.frame(x = mn, xend = mx, y = mn, yend = mx)
    
    mlt <- do.call(rbind, lm_fit$df_lst)
    
    p <- ggplot() +
      geom_segment(data = sgmnt, aes(x, y, xend = xend, yend = yend), linetype = "dashed") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Yesterday's water temperature (\u00B0C)") +
      geom_point(data = df, aes(wtemp_yday, wtemp, color = "Obs")) +
      scale_color_manual(values = c("Mod" = cols[4], "Obs" = "black"), name = "") +
      theme_bw(base_size = 12)+
      ggtitle("Note: the dashed line (- - -) is the 1:1 line")
    
    pars <- na.exclude(lr_pars$dt)
    
    df1 <- df %>%
      filter(Date > "2020-01-01")
    
    p1 <- ggplot() +
      geom_point(data = df1, aes(Date, wtemp, color = "Obs")) +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      scale_color_manual(values = c("Mod" = cols[4],
                                    "Obs" = "black"), name = "") +
      theme_bw(base_size = 12)
    
    if(input$add_lm > 0) {
      
      p <- p + geom_smooth(data = df, aes(wtemp_yday, wtemp, color = "Mod"), method = "lm", formula = "y ~ x",
                           se = FALSE) 
      
      lm_fit$eqn <- paste0("$$wtemp_{t} =  ", round(out$coefficients[2, 1], 2), "\\times wtemp_{t-1} + ", round(out$coefficients[1, 1], 2), "$$")
      
      lr_eqn$dt$m[idx] <- round(out$coefficients[2, 1], 2)
      lr_eqn$dt$b[idx] <- round(out$coefficients[1, 1], 2)
      lr_eqn$dt$r2[idx] <- round(out$r.squared, 2)
      lr_eqn$dt$N[idx] <- nrow(df)
      
      mod <- predict(fit, df)
      pred <- data.frame(Date = df$Date,
                         model = mod) 
      
      p1 <- p1 + geom_line(data = pred, aes(Date, model, color = "Mod"))
      
    }
    
    swt_swt_plot_lines$main <- p 
    
    wt_reg_ts_plot$main <- p1 
    
    
  })
  
  output$swt_swt_plot_lines <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot data'")
    )
    return(ggplotly(swt_swt_plot_lines$main, dynamicTicks = TRUE))
  })
  
  # save scatterplot of wtemp vs wtemp yday
  output$save_swt_swt_plot <- downloadHandler(
    filename = function() {
      paste("Q13a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 6, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = swt_swt_plot_lines$main, device = device)
    }
  )
  
  output$wt_reg_ts_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot data'")
    )
    return(ggplotly(wt_reg_ts_plot$main, dynamicTicks = TRUE))
  })


  output$lm_mod <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    validate(
      need(!is.null(lm_fit$eqn),
           message = "Click 'Get model parameters'.")
    )
    withMathJax(
      tags$p(lm_fit$eqn)
    )
  })
  
  
  # Simple linear regression 2: Plot water temp vs air temp
  
  observe({ # view_var
    
    validate(
      need(input$plot_airt_swt4 > 0,
           message = "Click 'Plot'")
    )
    
    req(!is.null(airt_swt$df))
    
    idx_dates <- seq.Date(airt_swt$df$Date[1], to = airt_swt$df$Date[nrow(airt_swt$df)], by = "1 day")
    
    airt_swt$sub <- airt_swt$df[airt_swt$df$Date %in% idx_dates, ]
  })
  
  # Fitting linear regression
  lr_pars1 <- reactiveValues(dt = data.frame(m_est = rep(NA, 1), m_se = rep(NA, 1),
                                            b_est = rep(NA, 1), b_se = rep(NA, 1),
                                            r2 = rep(NA, 1), N = rep(NA, 1), rmse = rep(NA, 1)))
  
  lr_eqn1 <- reactiveValues(dt = data.frame(m = rep(NA, 1), b = rep(NA, 1),
                                           r2 = rep(NA, 1), N = rep(NA, 1)))
  
  output$lr_DT1 <- renderDT(lr_eqn1$dt[, c(1,2, 4)], selection = "none",
                           options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                          columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                           ), colnames = c("Slope (m)","Intercept (b)", "N"),
                           rownames = c("Model parameters"),
                           server = FALSE, escape = FALSE)
  output$lr_DT2 <- renderDT(lr_pars1$dt[, c("m_est", "m_se", "b_est", "b_se", "rmse", "N")], selection = "single",
                            options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                           columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                            ),
                            # colnames = c("m", "b", "R-squared", "N"),
                            colnames = c("m", "m (Std. Dev.)", "b", "b (Std. Dev.)", "RMSE", "N"),
                            rownames = c("Monthly", "Fortnightly", "Weekly", "Daily"),
                            server = FALSE, escape = FALSE)
  
  output$r2_tab1 <- renderDT(data.frame(lr_pars1$dt$rmse), selection = "none",
                            options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                           columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                            ), colnames = c("RMSE"),
                            rownames = c("Monthly", "Fortnightly", "Weekly", "Daily"),
                            server = FALSE, escape = FALSE)
  
  
  # Plot with regression lines
  airt_swt_plot_lines <- reactiveValues(main = NULL)
  
  lm_fit1 <- reactiveValues(fit = NULL, df_lst = list(), eqn = NULL)
  
  at_reg_ts_plot <- reactiveValues(main = NULL)
  
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    
    validate(
      need(input$plot_airt_swt4 > 0,
           message = "Click 'Plot'")
    )
    
    req(!is.null(airt_swt$sub))
    
    tot_rows <- nrow(na.exclude(airt_swt$df))
    df <- airt_swt$sub
    df <- na.exclude(df)
    colnames(df)[2:3] <- c("airt", "wtemp")
    fit <- lm(wtemp ~ airt, data = df)
    out <- summary(fit)
    
    
    idx <- 1
    
    lr_pars1$dt$m_est[idx] <- round(out$coefficients[2, 1], 2)
    lr_pars1$dt$b_est[idx] <- round(out$coefficients[1, 1], 1)
    lr_pars1$dt$m_se[idx] <- round(out$coefficients[2, 2], 2)
    lr_pars1$dt$b_se[idx] <- round(out$coefficients[1, 2], 1)
    lr_pars1$dt$r2[idx] <- round(out$r.squared, 2)
    lr_pars1$dt$N[idx] <- nrow(df)
    lr_pars1$dt$rmse[idx] <- 0
    
    # }
    
    df$N <- nrow(df)
    
    lm_fit1$df_lst[[idx]] <- df
    
    lm_fit1$fit <- fit
    
    
    # For model selection table
    mod_selec_tab$dt$eqn[3] <- paste0("$$wtemp_{t+1} =  ", round(out$coefficients[2, 1], 2), " \\times atemp_{t+1} + ", round(out$coefficients[1, 1], 2), "$$")
    mod_selec_tab$dt$r2[3] <- round(out$r.squared, 2)
    
    
    mx <- max(df$airt, df$wtemp, na.rm = TRUE) + 2
    mn <- min(df$airt, df$wtemp, na.rm = TRUE) - 2
    
    sgmnt <- data.frame(x = mn, xend = mx, y = mn, yend = mx)
    
    mlt <- do.call(rbind, lm_fit$df_lst)
    
    p <- ggplot() +
      geom_segment(data = sgmnt, aes(x, y, xend = xend, yend = yend), linetype = "dashed") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      geom_point(data = df, aes(airt, wtemp, color = "Obs")) +
      scale_color_manual(values = c("Mod" = cols[5], "Obs" = "black"), name = "") +
      theme_bw(base_size = 12)+
      ggtitle("Note: the dashed line (- - -) is the 1:1 line")
    
    pars <- na.exclude(lr_pars$dt)
    
    df1 <- df %>%
      filter(Date > "2020-01-01")
    
    p1 <- ggplot() +
      geom_point(data = df1, aes(Date, wtemp, color = "Obs")) +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      scale_color_manual(values = c("Mod" = cols[5],
                                    "Obs" = "black"), name = "") +
      theme_bw(base_size = 12)
    
    if(input$add_lm1 > 0) {
      
      p <- p + geom_smooth(data = df, aes(airt, wtemp, color = "Mod"), method = "lm", formula = "y ~ x",
                           se = FALSE) 
      
      lm_fit1$eqn <- paste0("$$wtemp_{t} =  ", round(out$coefficients[2, 1], 2), "\\times atemp_{t} + ", round(out$coefficients[1, 1], 2), "$$")
      
      lr_eqn1$dt$m[idx] <- round(out$coefficients[2, 1], 2)
      lr_eqn1$dt$b[idx] <- round(out$coefficients[1, 1], 2)
      lr_eqn1$dt$r2[idx] <- round(out$r.squared, 2)
      lr_eqn1$dt$N[idx] <- nrow(df)
      
      mod <- predict(fit, df)
      pred <- data.frame(Date = df$Date,
                         model = mod) %>%
        filter(Date > "2020-01-01")
      
      p1 <- p1 + geom_line(data = pred, aes(Date, model, color = "Mod"))
      
    }
    
    airt_swt_plot_lines$main <- p
    
    at_reg_ts_plot$main <- ggplotly(p1, dynamicTicks = TRUE) 
    
    
  })
  
  output$airt_swt_plot_lines <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot data'")
    )
    validate(
      need(input$plot_airt_swt4 > 0,
           message = "Click 'Plot data'")
    )
    return(ggplotly(airt_swt_plot_lines$main, dynamicTicks = TRUE)) 
  })
  
  # save scatterplot of wtemp vs airtemp
  output$save_airt_swt_plot <- downloadHandler(
    filename = function() {
      paste("Q14a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 6, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = airt_swt_plot_lines$main, device = device)
    }
  )
  
  output$at_reg_ts_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot data'")
    )
    at_reg_ts_plot$main
  })
  
  
  output$lm_mod1 <- renderUI({
    input$add_lm1
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    validate(
      need(!is.null(lm_fit1$eqn),
           message = "Click 'Get model parameters'.")
    )
    withMathJax(
      tags$p(lm_fit1$eqn)
    )
  })
  
  # Multiple linear regression model
  
  observe({ 
    
    validate(
      need(input$plot_mlr > 0,
           message = "Click 'Plot'")
    )
    
    req(!is.null(airt_swt$df))
   
    idx_dates <- seq.Date(airt_swt$df$Date[1], to = airt_swt$df$Date[nrow(airt_swt$df)], by = "1 day")
    
    airt_swt$sub <- airt_swt$df[airt_swt$df$Date %in% idx_dates, ]
  })
  
  # Fitting multiple linear regression
  mlr_pars <- reactiveValues(dt = data.frame(b0_est = rep(NA, 1), b0_se = rep(NA, 1),
                                             b1_est = rep(NA, 1), b1_se = rep(NA, 1),
                                             b2_est = rep(NA, 1), b2_se = rep(NA,1), N = rep(NA, 1), rmse = rep(NA, 1)))
  
  mlr_eqn <- reactiveValues(dt = data.frame(b0 = rep(NA, 1), b1 = rep(NA, 1),
                                            b2 = rep(NA, 1), N = rep(NA, 1)))
  
  output$mlr_DT <- renderDT(mlr_eqn$dt[, c(1,2,3, 4)], selection = "none",
                            options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                           columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                            ), colnames = c("β0","β1", "β2","N"),
                            rownames = c("Model parameters"),
                            server = FALSE, escape = FALSE)
  output$mlr_DT2 <- renderDT(mlr_pars$dt[, c("b0_est", "b0_se", "b1_est", "b1_se","b2_est", "b2_se", "rmse", "N")], selection = "single",
                            options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                           columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                            ),
                            #colnames = c("m", "m (Std. Dev.)", "b", "b (Std. Dev.)", "RMSE", "N"),
                            rownames = c("Monthly", "Fortnightly", "Weekly", "Daily"),
                            server = FALSE, escape = FALSE)
  
  output$r2_tab1 <- renderDT(data.frame(mlr_pars$dt$rmse), selection = "none",
                             options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                            columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                             ), colnames = c("RMSE"),
                             rownames = c("Monthly", "Fortnightly", "Weekly", "Daily"),
                             server = FALSE, escape = FALSE)
  
  
  mlr_fit1 <- reactiveValues(fit = NULL, df_lst = list(), eqn = NULL)
  
  mlr_ts_plot1 <- reactiveValues(main = NULL)
  

  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    
    validate(
      need(input$plot_mlr > 0,
           message = "Click 'Plot'")
    )
    
    req(!is.null(airt_swt$sub))
    tot_rows <- nrow(na.exclude(airt_swt$df))
    df <- airt_swt$sub
    df <- na.exclude(df)
    colnames(df)[2:3] <- c("airt", "wtemp")
    df <- df %>%
      mutate(wtemp_yday = dplyr::lag(wtemp)) %>%
      filter(year(Date) == 2020)
    fit <- lm(wtemp ~ wtemp_yday + airt, data = df)
    out <- summary(fit)
    
    
    idx <- 1
    
    mlr_pars$dt$b0_est[idx] <- round(out$coefficients[1, 1], 2)
    mlr_pars$dt$b1_est[idx] <- round(out$coefficients[2, 1], 2)
    mlr_pars$dt$b2_est[idx] <- round(out$coefficients[3, 1], 2)
    mlr_pars$dt$b0_se[idx] <- round(out$coefficients[1, 2], 2)
    mlr_pars$dt$b1_se[idx] <- round(out$coefficients[2, 2], 2)
    mlr_pars$dt$b2_se[idx] <- round(out$coefficients[3, 2], 2)
    mlr_pars$dt$r2[idx] <- round(out$r.squared, 2)
    mlr_pars$dt$N[idx] <- nrow(df)
    mlr_pars$dt$rmse[idx] <- 0
    
    # }
    
    df$N <- nrow(df)
    
    mlr_fit1$df_lst[[idx]] <- df
    
    mlr_fit1$fit <- fit
    
    
    # For model selection table
    mod_selec_tab$dt$eqn[4] <- paste0("$$wtemp_{t+1} =  ",round(out$coefficients[1, 1], 2)," + ", round(out$coefficients[2, 1], 2), " \\times wtemp_{t} + ", round(out$coefficients[3, 1], 2), "\\times atemp_{t+1}$$")
    mod_selec_tab$dt$r2[4] <- round(out$r.squared, 2)
    
    df1 <- df %>%
      filter(Date > "2020-01-01")
    
    mod <- predict(fit, df)
    pred <- data.frame(Date = df$Date,
                       model = mod) %>%
      filter(Date > "2020-01-01")
    
    p1 <- ggplot() +
      geom_point(data = df1, aes(Date, wtemp, color = "Obs")) +
      geom_line(data = pred, aes(Date, model, color = "Mod")) +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      scale_color_manual(values = c("Mod" = cols[6],
                                    "Obs" = "black"), name = "") +
      theme_bw(base_size = 12)
    
   
      mlr_fit1$eqn <- paste0("$$wtemp_{t} =  ",round(out$coefficients[1, 1], 2)," + ", round(out$coefficients[2, 1], 2), " \\times wtemp_{t-1} + ", round(out$coefficients[3, 1], 2), "\\times atemp_{t}$$")
      
      mlr_eqn$dt$b0[idx] <- round(out$coefficients[1, 1], 2)
      mlr_eqn$dt$b1[idx] <- round(out$coefficients[2, 1], 2)
      mlr_eqn$dt$b2[idx] <- round(out$coefficients[3, 1], 2)
      mlr_eqn$dt$N[idx] <- nrow(df)
    
    mlr_ts_plot1$main <- ggplotly(p1, dynamicTicks = TRUE) 
    
    
  })

  output$mlr_ts_plot1 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    mlr_ts_plot1$main
  })
  
  
  output$mlr_mod1 <- renderUI({
    input$plot_mlr
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    validate(
      need(!is.null(mlr_fit1$eqn),
           message = "Click 'Plot'.")
    )
    withMathJax(
      tags$p(mlr_fit1$eqn)
    )
  })
  
  
  # Final plot with all four model fits
  
  all_mods_plot <- reactiveValues(main = NULL)
  all_mods_df <- reactiveValues(df = NULL)
  
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    validate(
      need(!is.null(lm_fit$fit),
           message = "Be sure to fit all four models above!")
    )
    validate(
      need(!is.null(lm_fit1$fit),
           message = "Be sure to fit all four models above!")
    )
    validate(
      need(!is.null(mlr_fit1$fit),
           message = "Be sure to fit all four models above!")
    )
    
    validate(
      need(input$plot_mlr > 0,
           message = "Click 'Plot'")
    )
    
    df <- airt_swt$sub
    df <- na.exclude(df)
    colnames(df)[2:3] <- c("airt", "wtemp")
    df1 <- df %>%
      mutate(wtemp_yday = dplyr::lag(wtemp)) %>%
      filter(year(Date) == 2020) 
    
    mod1 <- df1$wtemp_yday
    mod2 <- predict(lm_fit$fit, df1)
    mod3 <- predict(lm_fit1$fit, df1)
    mod4 <- predict(mlr_fit1$fit, df1)
    pred <- data.frame(Date = df1$Date,
                       model1 = mod1,
                       model2 = mod2,
                       model3 = mod3,
                       model4 = mod4,
                       wtemp = df1$wtemp) 
    
    all_mods_df$df <- pred
    
    p1 <- ggplot() +
      geom_point(data = df1, aes(Date, wtemp, color = "Obs")) +
      geom_line(data = pred, aes(Date, model1, color = "Pers")) +
      geom_line(data = pred, aes(Date, model2, color = "Wtemp")) +
      geom_line(data = pred, aes(Date, model3, color = "Atemp")) +
      geom_line(data = pred, aes(Date, model4, color = "Both")) +
      ylab("Temperature (\u00B0C)") +
      xlab("Time") +
      scale_color_manual(values = c("Pers" = cols[3],
                                    "Wtemp" = cols[4],
                                    "Atemp" = cols[5],
                                    "Both" = cols[6],
                                    "Obs" = "black"), name = "") +
      theme_bw(base_size = 12)
    
    all_mods_plot$main <- p1
    
    
  })
  
  output$all_mods_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(airt_swt$sub),
           message = "Click 'Plot'")
    )
    validate(
      need(!is.null(lm_fit$fit),
           message = "Be sure to fit all four models above!")
    )
    validate(
      need(!is.null(lm_fit1$fit),
           message = "Be sure to fit all four models above!")
    )
    validate(
      need(!is.null(mlr_fit1$fit),
           message = "Be sure to fit all four models above!")
    )
    return(ggplotly(all_mods_plot$main, dynamicTicks = TRUE))
  })
  
  # save plot of all model predictions
  output$save_all_mods_plot <- downloadHandler(
    filename = function() {
      paste("Q18a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = all_mods_plot$main, device = device)
    }
  )
  
  # end Objective 3
  

  ## Objective 4 - Generate deterministic forecasts ----
  
  # air temperature forecast plot
  output$airt1_fc_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$view_at_fc > 0,
           message = "Please click 'View forecast'")
    )
    
    p <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp. - Observed")) +
      geom_line(data = airt1_fc$df, aes(Date, value, color = "Air temp. - Forecast")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      labs(color = NULL)+
      theme_bw(base_size = 12)
    
    return(ggplotly(p, dynamicTicks = TRUE))
  })
  
  #create UI outputs for model equations
  output$persUI_1 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[1])
    )
  })
  
  output$wtempUI_1 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[2])
    )
  })
  
  output$atempUI_1 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[3])
    )
  })
  
  output$bothUI_1 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[4])
    )
  })
  
  # create reactive value for output of deterministic forecasts
  wtemp_fc_out1a <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))
  
  # code to run forecasts when students press action button
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    validate(
      need(input$fc1_Pers > 0,
           message = "Click 'Run forecast.'")
    )
    
    df <- run_deterministic_forecast(model = 1,
                               data = airt_swt$df,
                               airtemp_forecast = airt1_fc$df,
                               lr_pars3 = lr_eqn1$dt,
                               lr_pars2 = lr_eqn$dt,
                               mlr_pars = mlr_pars$dt,
                               model_table = mod_selec_tab$dt)

    wtemp_fc_out1a$lst[[1]] <- df[, c("Date", "forecast")]
  })
  
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    validate(
      need(input$fc1_Wtemp > 0,
           message = "Click 'Run forecast.'")
    )
    
    df <- run_deterministic_forecast(model = 2, 
                                     data = airt_swt$df,
                                     airtemp_forecast = airt1_fc$df, 
                                     lr_pars3 = lr_eqn1$dt,
                                     lr_pars2 = lr_eqn$dt,
                                     mlr_pars = mlr_pars$dt,
                                     model_table = mod_selec_tab$dt)
    
    wtemp_fc_out1a$lst[[2]] <- df[, c("Date", "forecast")]
  })
  
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    validate(
      need(input$fc1_Atemp > 0,
           message = "Click 'Run forecast.'")
    )
    
    df <- run_deterministic_forecast(model = 3, 
                                     data = airt_swt$df,
                                     airtemp_forecast = airt1_fc$df, 
                                     lr_pars3 = lr_eqn1$dt,
                                     lr_pars2 = lr_eqn$dt,
                                     mlr_pars = mlr_pars$dt,
                                     model_table = mod_selec_tab$dt)
    
    wtemp_fc_out1a$lst[[3]] <- df[, c("Date", "forecast")]
  })
  
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    validate(
      need(input$fc1_Both > 0,
           message = "Click 'Run forecast.'")
    )
    
    df <- run_deterministic_forecast(model = 4, 
                                     data = airt_swt$df,
                                     airtemp_forecast = airt1_fc$df, 
                                     lr_pars3 = lr_eqn1$dt,
                                     lr_pars2 = lr_eqn$dt,
                                     mlr_pars = mlr_pars$dt,
                                     model_table = mod_selec_tab$dt)

    wtemp_fc_out1a$lst[[4]] <- df[, c("Date", "forecast")]
  })
  
  
  # plot of deterministic forecasts
  wtemp_fc1a <- reactiveValues(main = NULL)
  
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    
    if(any(!is.na(wtemp_fc_out1a$lst))) {
      sub_lst <- wtemp_fc_out1a$lst[!is.null(wtemp_fc_out1a$lst)]
      mlt <- reshape::melt(sub_lst, id.vars = "Date")
      colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
      for(num in 1:4) {
        if(num %in% mlt$Label) {
          mlt$Label[mlt$Label == num] <- mod_names[num]
        }
      }
      mlt$Label <- factor(mlt$Label, levels = mod_names)
    }
    
    p <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp. - obs")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12) +
      labs(color = NULL)
    
    if(any(!is.na(wtemp_fc_out1a$lst))) {
      p <- p +
        geom_line(data = mlt, aes(Date, value, color = Label)) +
        labs(color = NULL)
    }
    
    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp. - obs" = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      labs(color = NULL)
    
    wtemp_fc1a$main <- p
  })
  
  observe({
    
    output$wtemp_fc1a <- renderPlotly({
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.na(mod_selec_tab$dt[1,1]),
             message = "Please fit models in Objective 3.")
      )
      validate(
        need(!is.na(mod_selec_tab$dt[2,1]),
             message = "Please fit models in Objective 3.")
      )
      validate(
        need(!is.na(mod_selec_tab$dt[3,1]),
             message = "Please fit models in Objective 3.")
      )
      validate(
        need(!is.na(mod_selec_tab$dt[4,1]),
             message = "Please fit models in Objective 3.")
      )
      validate(
        need(any(!is.na(wtemp_fc_out1a$lst)),
             message = "Click 'Run forecast'.")
      )
      
      return(ggplotly(wtemp_fc1a$main, dynamicTicks = TRUE))
    })
    
  })
  
  # save deterministic forecast plot
  output$save_deter_fc_plot <- downloadHandler(
    filename = function() {
      paste("Q19a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 6, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = wtemp_fc1a$main, device = device)
    }
  )
  
  # end Objective 4 and Activity A
  
  #### Activity B ----
  
  ## Objective 5 - Process Uncertainty ----
  
  # Slickr Process UC slides
  output$proc_uc_slides <- renderSlickR({
    slickR(proc_uc_slides) + settings(dots = TRUE, autoplay = FALSE)
  })
  
  # create reactive value for process uncertainty distribution plot
  proc_dist_plot <- reactiveValues(main = NULL)
  
  # create reactive value for table with sd of residuals
  sigma_table <- reactiveValues(df = NULL)
  
  # when you click 'Generate distributions', this will run
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    validate(
      need(input$gen_proc_dist > 0,
           message = "Click 'Generate distributions'.")
    )
    
    df <- all_mods_df$df 
    
    pers_residuals = df$model1 - df$wtemp
    pers_sigma = sd(pers_residuals, na.rm = TRUE)
    
    lr1_residuals = df$model2 - df$wtemp
    lr1_sigma = sd(lr1_residuals, na.rm = TRUE)
    
    lr2_residuals = df$model3 - df$wtemp
    lr2_sigma = sd(lr2_residuals, na.rm = TRUE)
    
    mlr_residuals = df$model4 - df$wtemp
    mlr_sigma = sd(mlr_residuals, na.rm = TRUE)
    
    proc_dist_data <- data.frame(W = c(rnorm(1000, 0, pers_sigma),
                                       rnorm(1000, 0, lr1_sigma),
                                       rnorm(1000, 0, lr2_sigma),
                                       rnorm(1000, 0, mlr_sigma)),
                                 Model = rep(c("Pers.","Wtemp","Atemp","Both"), each = 1000)) %>%
      mutate(Model = factor(Model, levels = c("Pers.","Wtemp","Atemp","Both")))
    
    proc_dist_plot$main <- ggplot(data = proc_dist_data, aes(x = W, fill = Model))+
      geom_density(alpha = 0.5)+
      facet_wrap(facets = vars(Model), nrow = 2)+
      scale_fill_manual(values = c(cols[3],cols[4],cols[5],cols[6]))+
      theme_bw(base_size = 16)
    
    sigma_table$df <- data.frame(Model = c("Pers.","Wtemp","Atemp","Both"),
                              Sigma = round(c(pers_sigma, lr1_sigma, lr2_sigma, mlr_sigma),2))
    
    
  })
  
  # output object for process uncertainty distribution plot
  output$proc_dist_plot <- renderPlot({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(input$gen_proc_dist > 0,
           message = "Click 'Generate distributions'.")
    )
    
    return(proc_dist_plot$main)
  })
  
  # save plot of process uncertainty distributions
  output$save_proc_uc_distrib_plot <- downloadHandler(
    filename = function() {
      paste("Q24a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 6, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = proc_dist_plot$main, device = device)
    }
  )
  
  # output object for table with sd of residuals for each model
  output$sigma_table <- renderDT({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(input$gen_proc_dist > 0,
           message = "Click 'Generate distributions'.")
    )
    
    sigma_table$df
  }, selection = "single",
  options = list(searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t", autoWidth = TRUE,
                 columnDefs = list(list(width = '10%', targets = "_all")),
                 scrollX = TRUE),
  colnames = c("Model", "SD of residuals"), 
  server = FALSE, escape = FALSE)
  
  
  #create UI outputs for model equations
  output$persUI_2 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[1])
    )
  })
  
  output$wtempUI_2 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[2])
    )
  })
  
  output$atempUI_2 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[3])
    )
  })
  
  output$bothUI_2 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[4])
    )
  })
  
  
  # create reactive value for output of process uncertainty forecast
  wtemp_fc_out2 <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))
  
  # this will run when the user clicks 'Run forecast' in Objective 5
  wtemp_fc_out2_Pers <- reactive({
    
    if(input$fc2_Pers){
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )

    out <- run_process_forecast(model = 1, 
                               data = airt_swt$df,
                               airtemp_forecast = airt1_fc$df, 
                               lr_pars3 = lr_eqn1$dt,
                               lr_pars2 = lr_eqn$dt,
                               mlr_pars = mlr_pars$dt,
                               model_table = mod_selec_tab$dt,
                               sigmas = sigma_table$df)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out2_Pers()))){
      wtemp_fc_out2$dist[[1]] <- wtemp_fc_out2_Pers()$dist

      wtemp_fc_out2$mlt[[1]] <- wtemp_fc_out2_Pers()$mlt

      wtemp_fc_out2$lst[[1]] <- wtemp_fc_out2_Pers()$lst

    }
  })
  
  wtemp_fc_out2_Wtemp <- reactive({
    
    if(input$fc2_Wtemp){
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    
    out <- run_process_forecast(model = 2, 
                                data = airt_swt$df,
                                airtemp_forecast = airt1_fc$df, 
                                lr_pars3 = lr_eqn1$dt,
                                lr_pars2 = lr_eqn$dt,
                                mlr_pars = mlr_pars$dt,
                                model_table = mod_selec_tab$dt,
                                sigmas = sigma_table$df)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out2_Wtemp()))){
      wtemp_fc_out2$dist[[2]] <- wtemp_fc_out2_Wtemp()$dist
      
      wtemp_fc_out2$mlt[[2]] <- wtemp_fc_out2_Wtemp()$mlt
      
      wtemp_fc_out2$lst[[2]] <- wtemp_fc_out2_Wtemp()$lst
      
    }
  })
  
  wtemp_fc_out2_Atemp <- reactive({
    
    if(input$fc2_Atemp){
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    
    out <- run_process_forecast(model = 3, 
                                data = airt_swt$df,
                                airtemp_forecast = airt1_fc$df, 
                                lr_pars3 = lr_eqn1$dt,
                                lr_pars2 = lr_eqn$dt,
                                mlr_pars = mlr_pars$dt,
                                model_table = mod_selec_tab$dt,
                                sigmas = sigma_table$df)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out2_Atemp()))){
      wtemp_fc_out2$dist[[3]] <- wtemp_fc_out2_Atemp()$dist
      
      wtemp_fc_out2$mlt[[3]] <- wtemp_fc_out2_Atemp()$mlt
      
      wtemp_fc_out2$lst[[3]] <- wtemp_fc_out2_Atemp()$lst
      
    }
  })
  
  wtemp_fc_out2_Both <- reactive({
    
    if(input$fc2_Both){
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    
    out <- run_process_forecast(model = 4, 
                                data = airt_swt$df,
                                airtemp_forecast = airt1_fc$df, 
                                lr_pars3 = lr_eqn1$dt,
                                lr_pars2 = lr_eqn$dt,
                                mlr_pars = mlr_pars$dt,
                                model_table = mod_selec_tab$dt,
                                sigmas = sigma_table$df)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out2_Both()))){
      wtemp_fc_out2$dist[[4]] <- wtemp_fc_out2_Both()$dist
      
      wtemp_fc_out2$mlt[[4]] <- wtemp_fc_out2_Both()$mlt
      
      wtemp_fc_out2$lst[[4]] <- wtemp_fc_out2_Both()$lst
      
    }
  })
  
  # plot process uncertainty forecast output
  proc_fc_plot <- reactiveValues(main = NULL)
  
  output$wtemp_fc2 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(input$gen_proc_dist > 0,
           message = "Click 'Generate distributions' above.")
    )
    validate(
      need(any(!is.na(wtemp_fc_out2$mlt)),
           message = "Click 'Run forecast'.")
    )
    
    p0 <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp. - obs")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12)
    
    
    if(any(!is.na(wtemp_fc_out2$mlt))) {
      sub_lst <- wtemp_fc_out2$mlt[!is.null(wtemp_fc_out2$mlt)]
      mlt <- do.call(rbind, sub_lst)
      mlt <- na.exclude(mlt)
      for(num in 1:4) {
        if(num %in% mlt$Level) {
          mlt$Level[mlt$Level == num] <- mod_names[num]
        }
      }
      mlt$Level <- factor(mlt$Level, levels = mod_names)
    }
    
    if(any(!is.na(wtemp_fc_out2$dist))) {
      sub_lst1 <- wtemp_fc_out2$dist[!is.null(wtemp_fc_out2$dist)]
      mlt1 <- do.call(rbind, sub_lst1)
      mlt1 <- na.exclude(mlt1)
      for(num in 1:4) {
        if(num %in% mlt1$Level) {
          mlt1$Level[mlt1$Level == num] <- mod_names[num]
        }
      }
      mlt1$Level <- factor(mlt1$Level, levels = mod_names)
    }
    
    if(input$plot_type2 == "Line") {
      
      p <- p0 +
        geom_line(data = mlt, aes(Date, value, color = Level, group = variable), alpha = 0.6)+
        labs(color = NULL)
      
    } else if(input$plot_type2 == "Distribution") {
      
      p <- p0 +
        geom_ribbon(data = mlt1, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
        geom_line(data = mlt1, aes(Date, p50, color = Level))+
        labs(color = NULL, fill = NULL)
      
    }
    
    p1 <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp. - obs" = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4]))+
      labs(color = NULL, fill = NULL)
    
    proc_fc_plot$main <- p0 +
      geom_ribbon(data = mlt1, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
      geom_line(data = mlt1, aes(Date, p50, color = Level))+
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp. - obs" = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4]))+
      labs(color = NULL, fill = NULL)
    
    gp <- ggplotly(p1, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
  })
  
  # save plot of forecasts with process uncertainty
  output$save_proc_fc_plot <- downloadHandler(
    filename = function() {
      paste("Q25a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 6, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = proc_fc_plot$main, device = device)
    }
  )
  
  # end Objective 5
  
  ## Objective 6 - Parameter Uncertainty ----
  
  # Slickr Parameter UC slides
  output$param_uc_slides <- renderSlickR({
    slickR(param_uc_slides) + settings(dots = TRUE, autoplay = FALSE)
  })
  
  # create reactive value for plot of model fit from year 1 and 2
  airt_swt_plot_year1 <- reactiveValues(main = NULL)
  
  airt_swt_plot_year2 <- reactiveValues(main = NULL)
  
  # create reactive value for table with model fit parameters
  param_uc_example_table <- reactiveValues(df = data.frame(m = c(NA,NA),
                                                           b = c(NA,NA)))
  
  # calculate model params for first year and put in table and make plot
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(input$fit_model_year_1 > 0,
           message = "Click 'Fit model to one year of data'.")
    )
    
    df <- airt_swt$sub
    df <- na.exclude(df)
    colnames(df)[2:3] <- c("airt", "wtemp")
    df <- df %>%
      filter(year(Date) == 2020)
    fit <- lm(df$wtemp ~ df$airt)
    out <- summary(fit)
    
    param_uc_example_table$df[1,] <- c(round(out$coefficients[2, 1], 2),round(out$coefficients[1, 1], 2))
    
    mx <- max(df$airt, df$wtemp, na.rm = TRUE) + 2
    mn <- min(df$airt, df$wtemp, na.rm = TRUE) - 2
    
    sgmnt <- data.frame(x = mn, xend = mx, y = mn, yend = mx)
    
    p <- ggplot() +
      geom_segment(data = sgmnt, aes(x, y, xend = xend, yend = yend), linetype = "dashed") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      geom_point(data = df, aes(airt, wtemp, color = "Obs")) +
      scale_color_manual(values = c("Mod" = cols[5], "Obs" = "black"), name = "") +
      theme_bw(base_size = 12) + 
      geom_smooth(data = df, aes(airt, wtemp, color = "Mod"), method = "lm", formula = "y ~ x",
                  se = FALSE) 
    
    airt_swt_plot_year1$main <- p
    
  })
  
  # plot model fit from one year of data
  output$model_fit_year_1 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(input$fit_model_year_1 > 0,
           message = "Click 'Fit model to one year of data'.")
    )
    airt_swt_plot_year1$main
  })
  
  # calculate model fit for second year and write plot
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(input$fit_model_year_2 > 0,
           message = "Click 'Fit model to one year of data'.")
    )
    
    df <- airt_swt$sub
    df <- na.exclude(df)
    colnames(df)[2:3] <- c("airt", "wtemp")
    df <- df %>%
      filter(year(Date) %in% c(2019,2020))
    fit <- lm(df$wtemp ~ df$airt)
    out <- summary(fit)
    
    param_uc_example_table$df[2,] <- c(round(out$coefficients[2, 1], 2),round(out$coefficients[1, 1], 2))
                                                                          
    mx <- max(df$airt, df$wtemp, na.rm = TRUE) + 2
    mn <- min(df$airt, df$wtemp, na.rm = TRUE) - 2
    
    sgmnt <- data.frame(x = mn, xend = mx, y = mn, yend = mx)
    
    p <- ggplot() +
      geom_segment(data = sgmnt, aes(x, y, xend = xend, yend = yend), linetype = "dashed") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      geom_point(data = df, aes(airt, wtemp, color = "Obs")) +
      scale_color_manual(values = c("Mod" = cols[5], "Obs" = "black"), name = "") +
      theme_bw(base_size = 12) + 
      geom_smooth(data = df, aes(airt, wtemp, color = "Mod"), method = "lm", formula = "y ~ x",
                  se = FALSE) 
    
    airt_swt_plot_year2$main <- p
  })
  
  # output object for plot model fit from Objective 3 (two years of data)
  output$model_fit_year_2 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(input$fit_model_year_2 > 0,
           message = "Click 'Fit model to one year of data'.")
    )
    airt_swt_plot_year2$main 
  })
  
  # table showing parameters from models fit to year 1 and year 2
  output$param_uc_example_table <- renderDT(param_uc_example_table$df, selection = "none",
                             options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                            columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                             ), colnames = c("Slope (m)","Intercept (b)"), rownames = c("1 year model","2 year model"),
                             server = FALSE, escape = FALSE)
  
  #create UI outputs for model equations
  output$persUI_3 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[1])
    )
  })
  
  output$wtempUI_3 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[2])
    )
  })
  
  output$atempUI_3 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[3])
    )
  })
  
  output$bothUI_3 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[4])
    )
  })
  
  # create reactive value to hold parameter distributions
  param_dist3b <- reactiveValues(dist = as.list(rep(NA, 4)),
                                 main = NULL)
  
  # this code will run when the user clicks 'Generate parameter distributions'
  param_dist3b_Pers <- reactive({
    if(input$param_Pers){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      
      df <- "None"
      return(list(dist = df))
    }
  })
  
  observe({
    if(any(!is.na(param_dist3b_Pers()))){
      param_dist3b$dist[[1]] <- param_dist3b_Pers()$dist
    }
  })
  
  observeEvent(input$param_Pers, {
    
    req(!is.na(param_dist3b$dist[[1]]))
    
    p <- ggplot() +
      annotate("text", x = 10,  y = 10,
               size = 6,
               label = "Looks like the model you selected has no parameters.\nBut you can still generate a forecast with it!") + theme_void()
    
    param_dist3b$main <- p
  })
  
  param_dist3b_Wtemp <- reactive({
    if(input$param_Wtemp){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      
      df <- data.frame(m = rnorm(5000, lr_pars$dt$m_est[1], lr_pars$dt$m_se[1]),
                       b = rnorm(5000, lr_pars$dt$b_est[1], lr_pars$dt$b_se[1]))
      
      return(list(dist = df))
    }
  })
  
  observe({
    if(any(!is.na(param_dist3b_Wtemp()))){
      param_dist3b$dist[[2]] <- param_dist3b_Wtemp()$dist
    }
  })
  
  observeEvent(input$param_Wtemp, {

    req(!is.na(param_dist3b$dist[[2]]))
    
    idx <- 2
    
    mlt <- reshape::melt(param_dist3b$dist[[idx]])
    
    p <- ggplot(mlt) +
      geom_density(aes(value), fill = l.cols[idx], alpha = 0.5) +
      facet_wrap(~variable, nrow = 1, scales = "free_x") +
      theme_bw(base_size = 16)
    
    param_dist3b$main <- p
  })
  
  param_dist3b_Atemp <- reactive({
    if(input$param_Atemp){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      
      df <- data.frame(m = rnorm(5000, lr_pars1$dt$m_est[1], lr_pars1$dt$m_se[1]),
                       b = rnorm(5000, lr_pars1$dt$b_est[1], lr_pars1$dt$b_se[1]))
      
      return(list(dist = df))
    }
  })
  
  observe({
    if(any(!is.na(param_dist3b_Atemp()))){
      param_dist3b$dist[[3]] <- param_dist3b_Atemp()$dist
    }
  })
  
  observeEvent(input$param_Atemp, {
    
    req(!is.na(param_dist3b$dist[[3]]))

    idx <- 3
    
    mlt <- reshape::melt(param_dist3b$dist[[idx]])
    
    p <- ggplot(mlt) +
      geom_density(aes(value), fill = l.cols[idx], alpha = 0.5) +
      facet_wrap(~variable, nrow = 1, scales = "free_x") +
      theme_bw(base_size = 16)
    
    param_dist3b$main <- p
  })
  
  param_dist3b_Both <- reactive({
    if(input$param_Both){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      
      df <- data.frame(beta0 = rnorm(5000, mlr_pars$dt$b0_est[1], mlr_pars$dt$b0_se[1]),
                       beta1 = rnorm(5000, mlr_pars$dt$b1_est[1], mlr_pars$dt$b1_se[1]),
                       beta2 = rnorm(5000, mlr_pars$dt$b2_est[1], mlr_pars$dt$b2_se[1]))
      
      return(list(dist = df))
    }
  })
  
  observe({
    if(any(!is.na(param_dist3b_Both()))){
      param_dist3b$dist[[4]] <- param_dist3b_Both()$dist
    }
  })
  
  observeEvent(input$param_Both, {
    
    req(!is.na(param_dist3b$dist[[4]]))

    idx <- 4
    
    mlt <- reshape::melt(param_dist3b$dist[[idx]])
    
    p <- ggplot(mlt) +
      geom_density(aes(value), fill = l.cols[idx], alpha = 0.5) +
      facet_wrap(~variable, nrow = 1, scales = "free_x") +
      theme_bw(base_size = 16)
    
    param_dist3b$main <- p
  })
  
  # write plot output for parameter distributions
  output$param_dist3b <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(!is.null(param_dist3b$main),
           message = "Please select a model and generate parameter distributions.")
    )

      param_dist3b$main
    
  })
  
  # create reactive value to hold forecast output
  wtemp_fc_out3b <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))
  
  # this code will run when user clicks 'Run forecast'
  wtemp_fc_out3b_Pers <- reactive({
    
    if(input$fc3_Pers){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(any(!is.na(param_dist3b$dist)),
             message = "Click 'Generate parameter distributions.'")
      )

    out <- run_param_forecast(model = 1, 
                                          data = airt_swt$df,
                                          airtemp_forecast = airt1_fc$df, 
                                          param_dist = param_dist3b$dist,
                                          model_table = mod_selec_tab$dt)
    
    return(list(dist = out$dat, mlt = out$mlt))
    
    } else {
      return(list(dist = NA, mlt = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out3b_Pers()))){
      
      wtemp_fc_out3b$dist[[1]] <- wtemp_fc_out3b_Pers()$dist
      
      wtemp_fc_out3b$mlt[[1]] <- wtemp_fc_out3b_Pers()$mlt
      
    }
  })
  
  wtemp_fc_out3b_Wtemp <- reactive({
    
    if(input$fc3_Wtemp){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(any(!is.na(param_dist3b$dist)),
             message = "Click 'Generate parameter distributions.'")
      )
    
    out <- run_param_forecast(model = 2, 
                                          data = airt_swt$df,
                                          airtemp_forecast = airt1_fc$df, 
                                          param_dist = param_dist3b$dist,
                                          model_table = mod_selec_tab$dt)
    return(list(dist = out$dat, mlt = out$mlt))
    
    } else {
      return(list(dist = NA, mlt = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out3b_Wtemp()))){
      
      wtemp_fc_out3b$dist[[2]] <- wtemp_fc_out3b_Wtemp()$dist
      
      wtemp_fc_out3b$mlt[[2]] <- wtemp_fc_out3b_Wtemp()$mlt
      
    }
  })
  
  wtemp_fc_out3b_Atemp <- reactive({
    
    if(input$fc3_Atemp){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(any(!is.na(param_dist3b$dist)),
             message = "Click 'Generate parameter distributions.'")
      )
    
    out <- run_param_forecast(model = 3, 
                                          data = airt_swt$df,
                                          airtemp_forecast = airt1_fc$df, 
                                          param_dist = param_dist3b$dist,
                                          model_table = mod_selec_tab$dt)
    return(list(dist = out$dat, mlt = out$mlt))
    
    } else {
      return(list(dist = NA, mlt = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out3b_Atemp()))){
      
      wtemp_fc_out3b$dist[[3]] <- wtemp_fc_out3b_Atemp()$dist
      
      wtemp_fc_out3b$mlt[[3]] <- wtemp_fc_out3b_Atemp()$mlt
      
    }
  })
  
  wtemp_fc_out3b_Both <- reactive({
    
    if(input$fc3_Both){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(any(!is.na(param_dist3b$dist)),
             message = "Click 'Generate parameter distributions.'")
      )
    
    out <- run_param_forecast(model = 4, 
                                          data = airt_swt$df,
                                          airtemp_forecast = airt1_fc$df, 
                                          param_dist = param_dist3b$dist,
                                          model_table = mod_selec_tab$dt)
    return(list(dist = out$dat, mlt = out$mlt))
    
    } else {
      return(list(dist = NA, mlt = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out3b_Both()))){
      
      wtemp_fc_out3b$dist[[4]] <- wtemp_fc_out3b_Both()$dist
      
      wtemp_fc_out3b$mlt[[4]] <- wtemp_fc_out3b_Both()$mlt
      
    }
  })
  
  # code to render plot
  param_fc_plot <- reactiveValues(main = NULL)
  
  output$wtemp_fc3b <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )

    validate(
      need(any(!is.na(param_dist3b$dist)),
           message = "Please select a model and generate parameter distributions.")
    )

    validate(
      need(any(!is.na(wtemp_fc_out3b$mlt)),
           message = "Click 'Run forecast'.")
    )
    
    p0 <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp. - obs")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12) +
      labs(color = NULL)
    
    if(any(!is.na(wtemp_fc_out3b$mlt))) {
      sub_lst1 <- wtemp_fc_out3b$mlt[!is.null(wtemp_fc_out3b$mlt)]
      mlt1 <- do.call(rbind, sub_lst1)
      mlt1 <- na.exclude(mlt1)
      for(num in 1:4) {
        if(num %in% mlt1$Level) {
          mlt1$Level[mlt1$Level == num] <- mod_names[num]
        }
      }
      mlt1$Level <- factor(mlt1$Level, levels = mod_names)
    }
      
      if(any(!is.na(wtemp_fc_out3b$dist))) {
        sub_lst2 <- wtemp_fc_out3b$dist[!is.null(wtemp_fc_out3b$dist)]
        mlt2 <- do.call(rbind, sub_lst2)
        mlt2 <- na.exclude(mlt2)
        for(num in 1:4) {
          if(num %in% mlt2$Level) {
            mlt2$Level[mlt2$Level == num] <- mod_names[num]
          }
        }
        mlt2$Level <- factor(mlt2$Level, levels = mod_names)
      }
    
    if(input$plot_type3b == "Line") {
        p <- p0 +
          geom_line(data = mlt1, aes(Date, value, color = Level, group = variable), alpha = 0.6) +
          labs(color = NULL)
    } else if(input$plot_type3b == "Distribution") {
        p <- p0 +
          geom_ribbon(data = mlt2, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt2, aes(Date, p50, color = Level)) +
          labs(color = NULL, fill = NULL)
    }
    
    p1 <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp. - obs" = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      labs(color = NULL, fill = NULL)
    
    param_fc_plot$main <- p0 +
      geom_ribbon(data = mlt2, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
      geom_line(data = mlt2, aes(Date, p50, color = Level)) +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp. - obs" = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      labs(color = NULL, fill = NULL)

    
    gp <- ggplotly(p1, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
  })
  
  # save parameter uncertainty forecast plot
  output$save_param_fc_plot <- downloadHandler(
    filename = function() {
      paste("Q28a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 6, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = param_fc_plot$main, device = device)
    }
  )
  
  # end Objective 6
  
  ## Objective 7 - Initial Conditions Uncertainty ----
  
  # Slickr Initial conditions UC slides
  output$ic_uc_slides <- renderSlickR({
    slickR(ic_uc_slides) + settings(dots = TRUE, autoplay = FALSE)
  })
  
  # set up reactive value for distribution around initial condition
  ic_dist <- reactiveValues(df = NULL)
  
  # generate IC distribution 
  observe({
    
    req(input$table01_rows_selected != "")
    req(!is.null(wtemp_fc_data$hist))
    
    validate(
      need(input$gen_ic > 0,
           message = "")
    )
    
    mn_wtemp <- wtemp_fc_data$hist$wtemp[wtemp_fc_data$hist$Date == fc_date]
    ic_dist$df <- data.frame(value = rnorm(1000, mn_wtemp, 0.1)) #0.1 is assumed sensor observation error
  })
  
  # output code for plot of recent observations to inform IC
  output$ic_obs_plot <- renderPlotly({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    df <- wtemp_fc_data$hist[2:5, ]
    
    p <- ggplot()
    
    if(!is.null(ic_dist$df)) {
      quants <- quantile(ic_dist$df$value, c(0.25, 0.75))
      
      err_bar <- data.frame(x = as.Date(fc_date), ymin = quants[1], ymax = quants[2])
      p <- p +
        geom_errorbar(data = err_bar, aes(x, ymin = ymin, ymax = ymax, width = 0.5), )
    }
    
    p <- p +
      geom_point(data = df, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      xlab("Date") +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      theme_bw(base_size = 12) +
      theme(legend.position = "none")
    
    
    
    return(ggplotly(p, dynamicTicks = TRUE))
  })
  
  # plot IC distribution
  output$ic_uc_plot <- renderPlot({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(!is.null(ic_dist$df), "Click 'Generate distribution'.")
    )
    
    df <- data.frame(x = wtemp_fc_data$hist$wtemp[wtemp_fc_data$hist$Date == fc_date],
                     label = "Observed")
    
    xlims <- c(df$x -1.5, df$x + 1.5)
    ylims <- c(0,7)
    
    p <- ggplot() +
      geom_vline(xintercept = df$x) +
      geom_density(data = ic_dist$df, aes(value), fill = l.cols[2], alpha = 0.3) +
      xlab("Temperature (\u00B0C)") +
      ylab("Density") +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      theme_bw(base_size = 18)
    
    return(p)
    
  })
  
  #create UI outputs for model equations
  output$persUI_4 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[1])
    )
  })
  
  output$wtempUI_4 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[2])
    )
  })
  
  output$atempUI_4 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[3])
    )
  })
  
  output$bothUI_4 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[4])
    )
  })
  
  # create reactive value for output of initial conditions uncertainty forecast
  wtemp_fc_out4 <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))

  # this code will run when user clicks "Run forecast"
  wtemp_fc_out4_Pers <- reactive({
    
    if(input$fc4_Pers){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(!is.null(ic_dist$df), "Click 'Generate distribution'.")
      )
    
    out <- run_ic_forecast(model = 1, 
                                data = airt_swt$df,
                                airtemp_forecast = airt1_fc$df, 
                                lr_pars3 = lr_eqn1$dt,
                                lr_pars2 = lr_eqn$dt,
                                mlr_pars = mlr_pars$dt,
                                model_table = mod_selec_tab$dt)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out4_Pers()))){
      
      wtemp_fc_out4$dist[[1]] <- wtemp_fc_out4_Pers()$dist
      
      wtemp_fc_out4$mlt[[1]] <- wtemp_fc_out4_Pers()$mlt
      
      wtemp_fc_out4$lst[[1]] <- wtemp_fc_out4_Pers()$lst
      
    }
  })
  
  wtemp_fc_out4_Wtemp <- reactive({
    
    if(input$fc4_Wtemp){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(!is.null(ic_dist$df), "Click 'Generate distribution'.")
      )
    
    out <- run_ic_forecast(model = 2, 
                           data = airt_swt$df,
                           airtemp_forecast = airt1_fc$df, 
                           lr_pars3 = lr_eqn1$dt,
                           lr_pars2 = lr_eqn$dt,
                           mlr_pars = mlr_pars$dt,
                           model_table = mod_selec_tab$dt)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out4_Wtemp()))){
      
      wtemp_fc_out4$dist[[2]] <- wtemp_fc_out4_Wtemp()$dist
      
      wtemp_fc_out4$mlt[[2]] <- wtemp_fc_out4_Wtemp()$mlt
      
      wtemp_fc_out4$lst[[2]] <- wtemp_fc_out4_Wtemp()$lst
      
    }
  })
  
  wtemp_fc_out4_Atemp <- reactive({
    
    if(input$fc4_Atemp){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(!is.null(ic_dist$df), "Click 'Generate distribution'.")
      )
    
    out <- run_ic_forecast(model = 3, 
                           data = airt_swt$df,
                           airtemp_forecast = airt1_fc$df, 
                           lr_pars3 = lr_eqn1$dt,
                           lr_pars2 = lr_eqn$dt,
                           mlr_pars = mlr_pars$dt,
                           model_table = mod_selec_tab$dt)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out4_Atemp()))){
      
      wtemp_fc_out4$dist[[3]] <- wtemp_fc_out4_Atemp()$dist
      
      wtemp_fc_out4$mlt[[3]] <- wtemp_fc_out4_Atemp()$mlt
      
      wtemp_fc_out4$lst[[3]] <- wtemp_fc_out4_Atemp()$lst
      
    }
  })
  
  wtemp_fc_out4_Both <- reactive({
    
    if(input$fc4_Both){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(!is.null(ic_dist$df), "Click 'Generate distribution'.")
      )
    
    out <- run_ic_forecast(model = 4, 
                           data = airt_swt$df,
                           airtemp_forecast = airt1_fc$df, 
                           lr_pars3 = lr_eqn1$dt,
                           lr_pars2 = lr_eqn$dt,
                           mlr_pars = mlr_pars$dt,
                           model_table = mod_selec_tab$dt)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out4_Both()))){
      
      wtemp_fc_out4$dist[[4]] <- wtemp_fc_out4_Both()$dist
      
      wtemp_fc_out4$mlt[[4]] <- wtemp_fc_out4_Both()$mlt
      
      wtemp_fc_out4$lst[[4]] <- wtemp_fc_out4_Both()$lst
      
    }
  })
  
  
  # create output object for plot of initial conditions uncertainty forecast
  ic_fc_plot <- reactiveValues(main = NULL)
  
  output$wtemp_fc4 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(!is.null(ic_dist$df), "Click 'Generate distribution' above.")
    )
    validate(
      need(any(!is.na(wtemp_fc_out4$mlt)),
           message = "Please select a model and click 'Run forecast'.")
    )
    
    p0 <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12) +
      labs(color = NULL)
    
    if(any(!is.na(wtemp_fc_out4$mlt))) {
      sub_lst <- wtemp_fc_out4$mlt[!is.null(wtemp_fc_out4$mlt)]
      mlt <- do.call(rbind, sub_lst)
      mlt <- na.exclude(mlt)
      for(num in 1:4) {
        if(num %in% mlt$Level) {
          mlt$Level[mlt$Level == num] <- mod_names[num]
        }
      }
      mlt$Level <- factor(mlt$Level, levels = mod_names)
    }
    
    if(any(!is.na(wtemp_fc_out4$dist))) {
      sub_lst1 <- wtemp_fc_out4$dist[!is.null(wtemp_fc_out4$dist)]
      mlt1 <- do.call(rbind, sub_lst1)
      mlt1 <- na.exclude(mlt1)
      for(num in 1:4) {
        if(num %in% mlt1$Level) {
          mlt1$Level[mlt1$Level == num] <- mod_names[num]
        }
      }
      mlt1$Level <- factor(mlt1$Level, levels = mod_names)
    }
    
    if(input$plot_type4 == "Line") {
      
        p <- p0 +
          geom_line(data = mlt, aes(Date, value, color = Level, group = variable), alpha = 0.6) +
          labs(color = NULL)

    } else if(input$plot_type4 == "Distribution") {
      
        p <- p0 +
          geom_ribbon(data = mlt1, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt1, aes(Date, p50, color = Level)) +
          labs(color = NULL, fill = NULL)
      
    }
    
    p1 <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      labs(color = NULL, fill = NULL)
    
    ic_fc_plot$main <- p0 +
      geom_ribbon(data = mlt1, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
      geom_line(data = mlt1, aes(Date, p50, color = Level)) +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      labs(color = NULL, fill = NULL)

    gp <- ggplotly(p1, dynamicTicks = TRUE)
    
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })
  
  # save initial conditions uncertainty forecast
  output$save_ic_fc_plot <- downloadHandler(
    filename = function() {
      paste("Q32a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 6, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = ic_fc_plot$main, device = device)
    }
  )
  
  # end Objective 7
  
  #### Objective 8 - Driver Uncertainty
  
  # Slickr Driver UC slides
  output$driver_uc_slides <- renderSlickR({
    slickR(driver_uc_slides) + settings(dots = TRUE, autoplay = FALSE)
  })
  
  # this code will run when the user clicks "Load forecast"
  noaa_df <- reactive({
    
    if(input$load_noaa_at > 0){
    
    req(input$table01_rows_selected != "")
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = paste0("Loading NOAA forecast data"),
                 detail = "This may take a while. This window will disappear
                     when it is finished loading.", value = 0.1)
    
    noaa_data <- list()
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
    
    noaa_data$airt <- reshape::melt(out[[1]][out[[1]]$L1 == "air_temperature", ], id.vars = c("time", "L1"))
    noaa_data$swr <- reshape::melt(out[[1]][out[[1]]$L1 == "surface_downwelling_shortwave_flux_in_air", ], id.vars = c("time", "L1"))
    noaa_data$swt <- noaa_data$airt
    noaa_data$swt$L1 <- "surface_water_temperature"
    noaa_data$swt$value <- met_pars$airt_m[idx] * noaa_data$swt$value + met_pars$airt_b[idx]
    noaa_data$upar <- noaa_data$swr
    noaa_data$upar$L1 <- "underwater_photosynthetically_active_radiation"
    noaa_data$upar$value <- met_pars$swr_m[idx] * noaa_data$upar$value + met_pars$swr_b[idx]
    
    mlt <- noaa_data$airt
    mlt$Date <- as.Date(mlt$time)
    mlt <- plyr::ddply(mlt, c("Date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
    mlt <- mlt[mlt$Date <= "2020-10-02", ]
    
    wid <- tidyr::pivot_wider(mlt, c(Date, L1), names_from = variable, values_from = value)
    wid <- as.data.frame(wid)
    
    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp,
                      airt = airt_swt$df$airt,
                      wtemp_yday = NA,
                      airt_yday = NA)
    
    dat$wtemp_yday[-c(1:1)] <- dat$wtemp[-c((nrow(dat)+1-1):nrow(dat))]
    dat$airt_yday[-c(1:1)] <- dat$airt[-c((nrow(dat)+1-1):nrow(dat))]
    
    lag_date <- (as.Date(fc_date) + 1)
    mn_date <- (as.Date(fc_date) + 1)
    
    noaa_data$lst <- lapply(1:30, function(x) {
      dat <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
      dat$wtemp[dat$Date > fc_date] <- NA
      dat$forecast <- NA
      dat$forecast[dat$Date == fc_date] <- dat$wtemp[dat$Date == fc_date]
      dat$airt[dat$Date > fc_date] <- wid[2:8, x+2]
      dat$wtemp_yday[dat$Date > lag_date] <- NA
      dat$airt_yday[dat$Date > mn_date] <- NA
      
      df <- data.frame(Date = seq.Date(as.Date("2020-09-22"), as.Date("2020-10-02"), by = 1))
      df <- merge(dat, df, by = "Date", all.y = TRUE)
    })
    
    return(list(airt = noaa_data$airt, swr = noaa_data$swr, lst = noaa_data$lst))
    
    }
    
  })
  
  # this text lets the user know the forecast has loaded
  output$noaa_at_loaded <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df()$airt), "Please click 'Load forecast'")
    )
    return(paste0("Forecast loaded for ", siteID$lab))
  })
  
  # create output object for NOAA ensemble air temperature forecast plot
  output$airt_fc5 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(noaa_df()$airt), "Please click 'Load forecast'")
    )
    
    mlt <- noaa_df()$airt
    
    mlt$Date <- as.Date(mlt$time)
    mlt <- plyr::ddply(mlt, c("Date", "L1", "variable"), function(x) data.frame(value = mean(x$value, na.rm = TRUE)))
    mlt <- mlt[mlt$Date <= "2020-10-02", ]
    mlt$time <- as.POSIXct(mlt$Date)
    fut_offset <- lubridate::days(6) 
    mlt <- mlt[mlt$variable %in% paste0("mem", formatC(1:30, width = 2, format = "d", flag = "0")), ]
    
    
    p <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, airt, color = "Air temp. - Observed")) +
      geom_line(data = mlt, aes(Date, value, group = variable, color = "Air temp. - Forecast"), alpha = 0.6) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      scale_color_manual(values = c("Air temp. - Observed" = cols[1], "Air temp. - Forecast" = "gray")) +
      ylab("Temperature (\u00B0C)") +
      labs(color = NULL) +
      theme_bw(base_size = 12)
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
  })
  
  #create UI outputs for model equations
  output$persUI_5 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[1])
    )
  })
  
  output$wtempUI_5 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[2])
    )
  })
  
  output$atempUI_5 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[3])
    )
  })
  
  output$bothUI_5 <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Select site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Fit models in Objective 3.")
    )
    withMathJax(
      tags$p(mod_selec_tab$dt$eqn[4])
    )
  })
  
  # create reactive value to hold driver uncertainty forecast output
  wtemp_fc_out5 <- reactiveValues(mlt = as.list(rep(NA, 4)), dist = as.list(rep(NA, 4)), lst = as.list(rep(NA, 4)))
  
  # this code will run when user clicks "Run forecast"
  wtemp_fc_out5_Pers <- reactive({
    
    if(input$fc5_Pers){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(!is.null(noaa_df()$airt), "Please click 'Load forecast'")
      )
    
    out <- run_driver_forecast(model = 1, 
                               data = airt_swt$df,
                               airtemp_forecast = noaa_df()$airt, 
                               lr_pars3 = lr_eqn1$dt,
                               lr_pars2 = lr_eqn$dt,
                               mlr_pars = mlr_pars$dt,
                               model_table = mod_selec_tab$dt,
                               airtemp_forecast_data = noaa_df()$lst)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out5_Pers()))){
      
      wtemp_fc_out5$dist[[1]] <- wtemp_fc_out5_Pers()$dist
      
      wtemp_fc_out5$mlt[[1]] <- wtemp_fc_out5_Pers()$mlt
      
      wtemp_fc_out5$lst[[1]] <- wtemp_fc_out5_Pers()$lst
      
    }
  })
  
  wtemp_fc_out5_Wtemp <- reactive({
    
    if(input$fc5_Wtemp){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(!is.null(noaa_df()$airt), "Please click 'Load forecast'")
      )
    
    out <- run_driver_forecast(model = 2, 
                               data = airt_swt$df,
                               airtemp_forecast = noaa_df()$airt, 
                               lr_pars3 = lr_eqn1$dt,
                               lr_pars2 = lr_eqn$dt,
                               mlr_pars = mlr_pars$dt,
                               model_table = mod_selec_tab$dt,
                               airtemp_forecast_data = noaa_df()$lst)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out5_Wtemp()))){
      
      wtemp_fc_out5$dist[[2]] <- wtemp_fc_out5_Wtemp()$dist
      
      wtemp_fc_out5$mlt[[2]] <- wtemp_fc_out5_Wtemp()$mlt
      
      wtemp_fc_out5$lst[[2]] <- wtemp_fc_out5_Wtemp()$lst
      
    }
  })
  
  wtemp_fc_out5_Atemp <- reactive({
    
    if(input$fc5_Atemp){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(!is.null(noaa_df()$airt), "Please click 'Load forecast'")
      )
    
    out <- run_driver_forecast(model = 3, 
                               data = airt_swt$df,
                               airtemp_forecast = noaa_df()$airt, 
                               lr_pars3 = lr_eqn1$dt,
                               lr_pars2 = lr_eqn$dt,
                               mlr_pars = mlr_pars$dt,
                               model_table = mod_selec_tab$dt,
                               airtemp_forecast_data = noaa_df()$lst)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out5_Atemp()))){
      
      wtemp_fc_out5$dist[[3]] <- wtemp_fc_out5_Atemp()$dist
      
      wtemp_fc_out5$mlt[[3]] <- wtemp_fc_out5_Atemp()$mlt
      
      wtemp_fc_out5$lst[[3]] <- wtemp_fc_out5_Atemp()$lst
      
    }
  })
  
  wtemp_fc_out5_Both <- reactive({
    
    if(input$fc5_Both){
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Select site in Objective 1.")
      )
      validate(
        need(!is.null(all_mods_df$df),
             message = "Fit models in Objective 3.")
      )
      validate(
        need(!is.null(noaa_df()$airt), "Please click 'Load forecast'")
      )
    
    out <- run_driver_forecast(model = 4, 
                               data = airt_swt$df,
                               airtemp_forecast = noaa_df()$airt, 
                               lr_pars3 = lr_eqn1$dt,
                               lr_pars2 = lr_eqn$dt,
                               mlr_pars = mlr_pars$dt,
                               model_table = mod_selec_tab$dt,
                               airtemp_forecast_data = noaa_df()$lst)
    
    return(list(dist = out$dat, mlt = out$mlt, lst = out$df[, c("Date", "forecast")]))
    
    } else {
      return(list(dist = NA, mlt = NA, lst = NA))
      
    }
    
  })
  
  observe({
    if(any(!is.na(wtemp_fc_out5_Both()))){
      
      wtemp_fc_out5$dist[[4]] <- wtemp_fc_out5_Both()$dist
      
      wtemp_fc_out5$mlt[[4]] <- wtemp_fc_out5_Both()$mlt
      
      wtemp_fc_out5$lst[[4]] <- wtemp_fc_out5_Both()$lst
      
    }
  })
  
  # create output object for driver uncertainty forecast plot
  driver_fc_plot <- reactiveValues(main = NULL)
  output$wtemp_fc5 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(!is.null(noaa_df()$airt), "Please click 'Load forecast' above.")
    )
    validate(
      need(any(!is.na(wtemp_fc_out5$mlt)),
           message = "Click 'Run forecast'.")
    )
    
    if(any(!is.na(wtemp_fc_out5$lst))) {
      sub_lst <- wtemp_fc_out5$lst[!is.na(wtemp_fc_out5$lst)]
      mlt <- reshape::melt(sub_lst, id.vars = "Date")
      colnames(mlt)[which(colnames(mlt) == "L1")] <- "Label"
      mlt$Label <- as.character(mlt$Label)
    }
    
    p0 <- ggplot() +
      geom_point(data = wtemp_fc_data$hist, aes(Date, wtemp, color = "Water temp.")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12) +
      labs(color = NULL)
    
    if(any(!is.na(wtemp_fc_out5$mlt))) {
      sub_lst <- wtemp_fc_out5$mlt[!is.null(wtemp_fc_out5$mlt)]
      mlt <- do.call(rbind, sub_lst)
      mlt <- na.exclude(mlt)
      for(num in 1:4) {
        if(num %in% mlt$Level) {
          mlt$Level[mlt$Level == num] <- mod_names[num]
        }
      }
      mlt$Level <- factor(mlt$Level, levels = mod_names)
    }
    
    if(any(!is.na(wtemp_fc_out5$dist))) {
      sub_lst1 <- wtemp_fc_out5$dist[!is.null(wtemp_fc_out5$dist)]
      mlt1 <- do.call(rbind, sub_lst1)
      mlt1 <- na.exclude(mlt1)
      for(num in 1:4) {
        if(num %in% mlt1$Level) {
          mlt1$Level[mlt1$Level == num] <- mod_names[num]
        }
      }
      mlt1$Level <- factor(mlt1$Level, levels = mod_names)
    }
    
    if(input$plot_type5 == "Line") {
        
        p <- p0 +
          geom_line(data = mlt, aes(Date, value, color = Level, group = variable), alpha = 0.6) +
          labs(color = NULL)
      
    } else if(input$plot_type5 == "Distribution") {
        
        p <- p0 +
          geom_ribbon(data = mlt1, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
          geom_line(data = mlt1, aes(Date, p50, color = Level)) +
          labs(color = NULL, fill = NULL)
      
    }
    
    p1 <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      labs(color = NULL, fill = NULL)
    
    driver_fc_plot$main <- p0 +
      geom_ribbon(data = mlt1, aes(Date, ymin = p5, ymax = p95, fill = Level), alpha = 0.3) +
      geom_line(data = mlt1, aes(Date, p50, color = Level)) +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      labs(color = NULL, fill = NULL)

    gp <- ggplotly(p1, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })
  
  # save forecast plot with driver uncertainty
  output$save_driver_fc_plot <- downloadHandler(
    filename = function() {
      paste("Q34a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 6, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = driver_fc_plot$main, device = device)
    }
  )
  
  
  # end Objective 8
  
  #### Objective 9 - Quantifying Uncertainty
  
  # output showing text of model A 
  output$modA_txt <- renderText({
    validate(
      need(input$mod_selec_tot_fc != "", "")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "")
    )
    
    if(input$mod_selec_tot_fc[1] == "Pers") {
      txt <- "We will use the persistence model. This model predicts that tomorrow's water temperature will be the same as today."
    } else if (input$mod_selec_tot_fc[1] == "Wtemp") {
      txt <- "We will use the water temperature linear regression model. This model uses today's water temperature as a explanatory variable to predict tomorrow's water temperature."
    } else if (input$mod_selec_tot_fc[1] == "Atemp") {
      txt <- "We will use the air temperature linear regression model. This model uses tomorrow's air temperature as a explanatory variable to predict tomorrow's water temperature."
    } else if (input$mod_selec_tot_fc[1] == "Both") {
      txt <- "We will use the multiple linear regression model with air and water temperature. This model uses today's water temperature and tomorrow's air temperature as explanatory variables in a multiple linear regression."
    }
    return(txt)
  })
  
  # output showing equation of model A
  output$modA_eqn <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )
    
    if(input$mod_selec_tot_fc[1] == "Pers") {
      eqn <- mod_selec_tab$dt[1, 1]
    } else if (input$mod_selec_tot_fc[1] == "Wtemp") {
      eqn <- mod_selec_tab$dt[2, 1]
    } else if (input$mod_selec_tot_fc[1] == "Atemp") {
      eqn <- mod_selec_tab$dt[3, 1]
    } else if (input$mod_selec_tot_fc[1] == "Both") {
      eqn <- mod_selec_tab$dt[4, 1]
    }
    eqn <- gsub("[$$]+", "", eqn)
    eqn <- paste0("$$", eqn, " + W_{t}$$")
    
    withMathJax(
      div(eqn)
    )
  })
  
  # this code will run when user pushes run forecast button for Model A
  tot_fc_dataA <- reactive({
    
    if(input$run_tot_fcA){
    
    req(input$table01_rows_selected != "")
    req(length(input$mod_selec_tot_fc) == 2)
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(!is.null(sigma_table$df),
      message = "Need to generate process uncertainty distributions in Objective 5.")
  )
  validate(
    need(any(!is.na(param_dist3b$dist)),
         message = "Need to generate parameter uncertainty distributions in Objective 6.")
  )
  validate(
    need(!is.null(ic_dist$df), "Need to generate initial conditions uncertainty distribution in Objective 7.")
  )
  validate(
    need(!is.null(noaa_df()$airt), "Need to load NOAA air temperature forecast in Objective 8.")
  )
    
  tot_fc_dataA <- list(mlt = NULL, dist = NULL, mat = NULL, lab = NULL, quant_df = NULL)
  
    idx <- which(mod_names == input$mod_selec_tot_fc[1])
    
    req(!is.null(noaa_df()$lst))
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = paste0("Forecasting water temperature"),
                 detail = "This may take a while. This window will disappear
                     when it is finished loading.", value = 0.05)
    
    df <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp)
    tot_fc_dataA$lab <- input$mod_selec_tot_fc[1]
    
    for(fc_uncertA in uc_sources) {
      
      pidx <- which(uc_sources == fc_uncertA)
      mat <- matrix(NA, 8, 500)
      
      if("Driver" %in% fc_uncertA | "Total" %in% fc_uncertA) {
        driv_mat <- sapply(1:30, function(x) noaa_df()$lst[[x]]$airt[noaa_df()$lst[[x]]$Date >= fc_date])
        tmes <- ceiling(500 / 30)
        M <- do.call(cbind, replicate(tmes, driv_mat, simplify = FALSE))
        driv_mat <- M[, 1:500]
      } else {
        driv_mat <- sapply(1, function(x) noaa_df()$lst[[x]]$airt[noaa_df()$lst[[x]]$Date >= fc_date])
      }
      if("Parameter" %in% fc_uncertA | "Total" %in% fc_uncertA) {
        
        idx <- sample(1:5000, 500)
        
        if(input$mod_selec_tot_fc[1] == "Wtemp") {
          pars <- param_dist3b$dist[[2]]
          params <- data.frame(m = pars$m[idx],
                               b = pars$b[idx])
        } else if (input$mod_selec_tot_fc[1] == "Both") {
          pars <- param_dist3b$dist[[4]]
          params <- data.frame(beta1 = pars$beta1[idx],
                               beta2 = pars$beta2[idx],
                               beta0 = pars$beta0[idx])
        } else if(input$mod_selec_tot_fc[1] == "Atemp") {
          pars <- param_dist3b$dist[[3]]
          params <- data.frame(m = pars$m[idx],
                               b = pars$b[idx])
        } else {
          params <- "NULL"
        }
      } else {
        if(input$mod_selec_tot_fc[1] == "Wtemp") {
          pars <- param_dist3b$dist[[2]]
          params <- data.frame(m = mean(pars$m),
                               b = mean(pars$b))
        } else if (input$mod_selec_tot_fc[1] == "Both") {
          pars <- param_dist3b$dist[[4]]
          params <- data.frame(beta1 = mean(pars$beta1),
                               beta2 = mean(pars$beta2),
                               beta0 = mean(pars$beta0))
        } else if(input$mod_selec_tot_fc[1] == "Atemp") {
          pars <- param_dist3b$dist[[3]]
          params <- data.frame(m = mean(pars$m),
                               b = mean(pars$b))
        } else {
          params <- "NULL"
        }
      }
      if("Initial Conditions" %in% fc_uncertA | "Total" %in% fc_uncertA) {
        if(input$mod_selec_tot_fc[1] %in% c("Wtemp","Pers","Both")){
          mat[1, ] <- rnorm(500, df$wtemp[which(df$Date == fc_date)], sd = 0.1)
        } else {
          mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
        }
      } else {
        mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
      }
      
      for(mem in 2:nrow(mat)) {
        
        # Calculate process noise each time step
        if("Process" %in% fc_uncertA | "Total" %in% fc_uncertA) {
          if(input$mod_selec_tot_fc[1] == "Wtemp") {
            Wt <- rnorm(500, 0, sigma_table$df[2,2])
          } else if (input$mod_selec_tot_fc[1] == "Both") {
            Wt <- rnorm(500, 0, sigma_table$df[4,2])
          } else if(input$mod_selec_tot_fc[1] == "Atemp") {
            Wt <- rnorm(500, 0, sigma_table$df[3,2])
          } else if(input$mod_selec_tot_fc[1] == "Pers") {
            Wt <- rnorm(500, 0, sigma_table$df[1,2])
          }
        } else {
          Wt <- 0
        }
        
        if(input$mod_selec_tot_fc[1] == "Atemp") {
          mat[mem, ] <- params$m * driv_mat[mem, ] + params$b + Wt
        } else if (input$mod_selec_tot_fc[1] == "Pers") {
          mat[mem, ] <- mat[mem-1, ] + Wt
        } else if (input$mod_selec_tot_fc[1] == "Wtemp") {
          mat[mem, ] <- params$m * mat[mem-1, ] + params$b + Wt
        } else if (input$mod_selec_tot_fc[1] == "Both") {
          mat[mem, ] <- mat[mem-1, ] * params$beta1 + driv_mat[mem, ] * params$beta2 + params$beta0 + Wt
        }
      }
      
      # Calculate distributions
      tot_fc_dataA$mat <- mat
      
      if(fc_uncertA == "Total") {
        dat <- apply(mat, 1, function(x) {
          quantile(x, c(0.05, 0.5, 0.95))
        })
        dat <- as.data.frame(t(dat))
        colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
        dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
        tot_fc_dataA$dist <- dat
        df2 <- as.data.frame(mat)
        df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
        mlt <- reshape::melt(df2, id.vars = "Date")
        tot_fc_dataA$mlt <- mlt
      }
      
      if(fc_uncertA != "Total") {
        # Quantify UC
        std <- apply(tot_fc_dataA$mat, 1, sd)
        
        df2 <- data.frame(Date = seq.Date(from = as.Date(fc_date), length.out = 8, by = 1),
                          sd = std, label = fc_uncertA)
        
        if(is.null(tot_fc_dataA$quant_df)) {
          tot_fc_dataA$quant_df <- df2
        } else {
          # Overwrite previous Std Dev.
          if((df2$label[1] %in% tot_fc_dataA$quant_df$label)) {
            idx <- which(tot_fc_dataA$quant_df$label %in% df2$label[1])
            tot_fc_dataA$quant_df[idx, ] <- df2
          } else {
            tot_fc_dataA$quant_df <- rbind(tot_fc_dataA$quant_df, df2)
          }
        }
      }
      
      progress$set(value = (pidx / length(uc_sources)))
    }
    return(tot_fc_dataA)
    }
  })
  
  # create output for forecast with total uncertainty plot for Model A
  output$tot_fc_uncertA <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(!is.null(sigma_table$df),
      message = "Need to generate process uncertainty distributions in Objective 5.")
  )
  validate(
    need(any(!is.na(param_dist3b$dist)),
         message = "Need to generate parameter uncertainty distributions in Objective 6.")
  )
  validate(
    need(!is.null(ic_dist$df), "Need to generate initial conditions uncertainty distribution in Objective 7.")
  )
  validate(
    need(!is.null(noaa_df()$airt), "Need to load NOAA air temperature forecast in Objective 8.")
  )
  validate(
    need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
  )
    
    idx <- which(mod_names == input$mod_selec_tot_fc[1])
    sel_col <- cols[idx]
  
    validate(
      need(!is.null(tot_fc_dataA()$dist), "Click 'Run forecast'")
    )
    
    dat <- wtemp_fc_data$hist[wtemp_fc_data$hist$Date >= (as.Date(fc_date) - 1), ]
    
    
    p <- ggplot() +
      geom_point(data = dat, aes(Date, wtemp, color = "Water temp. - observed")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12) +
      labs(color = NULL)
    
    if(input$plot_type_totA == "Line") {
      if(!is.null(tot_fc_dataA()$mlt)) {
        
        mlt <- tot_fc_dataA()$mlt
        
        p <- p +
          geom_line(data = mlt, aes(Date, value, group = variable, color = input$mod_selec_tot_fc[1]), alpha = 0.6) +
          labs(color = NULL)
      }
    } else if(input$plot_type_totA == "Distribution") {
      if(!is.null(tot_fc_dataA()$dist)) {
        mlt <- tot_fc_dataA()$dist
        
        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = input$mod_selec_tot_fc[1]), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = input$mod_selec_tot_fc[1])) +
          labs(color = NULL, fill = NULL)
      }
    }
    
    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      labs(color = NULL, fill = NULL)
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })
  
  
  # create output for partitioned uncertainty plot for Model A
  fc_quantA_plot <- reactiveValues(main = NULL)
  output$fc_quantA <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(!is.null(sigma_table$df),
           message = "Need to generate process uncertainty distributions in Objective 5.")
    )
    validate(
      need(any(!is.na(param_dist3b$dist)),
           message = "Need to generate parameter uncertainty distributions in Objective 6.")
    )
    validate(
      need(!is.null(ic_dist$df), "Need to generate initial conditions uncertainty distribution in Objective 7.")
    )
    validate(
      need(!is.null(noaa_df()$airt), "Need to load NOAA air temperature forecast in Objective 8.")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )
    validate(
      need(!is.null(tot_fc_dataA()$quant_df), "Click 'Run forecast' above.")
    )
    validate(
      need(input$quant_ucA > 0, "Click 'Quantify uncertainty'")
    )
    
    p <- ggplot() +
      geom_bar(data = tot_fc_dataA()$quant_df, aes(Date, sd, fill = label), stat = "identity", position = "stack") +
      ylab("Standard Deviation (\u00B0C)") +
      scale_fill_manual(values = c("Process" = cols2[1], "Parameter" = cols2[2], "Initial Conditions" = cols2[3],
                                   "Driver" = cols2[4], "Total" = cols2[5])) +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      labs(fill = "Uncertainty") +
      theme_bw(base_size = 12)
    
    fc_quantA_plot$main <- p
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    return(gp)
  })
  
  # save uncertainty partitioning plot for model A
  output$save_fc_quantA_plot <- downloadHandler(
    filename = function() {
      paste("Q36a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = fc_quantA_plot$main, device = device)
    }
  )
  
  # text describing Model B
  output$modB_txt <- renderText({
    validate(
      need(input$mod_selec_tot_fc != "", "")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "")
    )
    
    if(input$mod_selec_tot_fc[2] == "Pers") {
      txt <- "We will use the persistence model. This model predicts that tomorrow's water temperature will be the same as today."
    } else if (input$mod_selec_tot_fc[2] == "Wtemp") {
      txt <- "We will use the water temperature linear regression model. This model uses today's water temperature as a explanatory variable to predict tomorrow's water temperature."
    } else if (input$mod_selec_tot_fc[2] == "Atemp") {
      txt <- "We will use the air temperature linear regression model. This model uses tomorrow's air temperature as a explanatory variable to predict tomorrow's water temperature."
    } else if (input$mod_selec_tot_fc[2] == "Both") {
      txt <- "We will use the multiple linear regression model with air and water temperature. This model uses today's water temperature and tomorrow's air temperature as explanatory variables in a multiple linear regression."
    }
    return(txt)
  })
  
  # equation output for Model B selected by user
  output$modB_eqn <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )
    
    if(input$mod_selec_tot_fc[2] == "Pers") {
      eqn <- mod_selec_tab$dt[1, 1]
    } else if (input$mod_selec_tot_fc[2] == "Wtemp") {
      eqn <- mod_selec_tab$dt[2, 1]
    } else if (input$mod_selec_tot_fc[2] == "Atemp") {
      eqn <- mod_selec_tab$dt[3, 1]
    } else if (input$mod_selec_tot_fc[2] == "Both") {
      eqn <- mod_selec_tab$dt[4, 1]
    }
    
    eqn <- gsub("[$$]+", "", eqn)
    eqn <- paste0("$$", eqn, " + W_{t}$$")
    
    withMathJax(
      div(eqn)
    )
  })
  
  # this code will run when user clicks "Run forecast" for Model B
  tot_fc_dataB <- reactive({
    
    if(input$run_tot_fcB){
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(!is.null(sigma_table$df),
           message = "Need to generate process uncertainty distributions in Objective 5.")
    )
    validate(
      need(any(!is.na(param_dist3b$dist)),
           message = "Need to generate parameter uncertainty distributions in Objective 6.")
    )
    validate(
      need(!is.null(ic_dist$df), "Need to generate initial conditions uncertainty distribution in Objective 7.")
    )
    validate(
      need(!is.null(noaa_df()$airt), "Need to load NOAA air temperature forecast in Objective 8.")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = paste0("Forecasting water temperature"),
                 detail = "This may take a while. This window will disappear
                     when it is finished loading.", value = 0.05)
    
    tot_fc_dataB <- list(mlt = NULL, dist = NULL, mat = NULL, lab = NULL, quant_df = NULL)
    
    df <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp)
    
    tot_fc_dataB$lab <- input$mod_selec_tot_fc[2]
    
    for(fc_uncertA in uc_sources) {
      
      mat <- matrix(NA, 8, 500)
      pidx <- which(uc_sources == fc_uncertA)
      
      
      if("Driver" %in% fc_uncertA | "Total" %in% fc_uncertA) {
        driv_mat <- sapply(1:30, function(x) noaa_df()$lst[[x]]$airt[noaa_df()$lst[[x]]$Date >= fc_date])
        tmes <- ceiling(500 / 30)
        M <- do.call(cbind, replicate(tmes, driv_mat, simplify = FALSE))
        driv_mat <- M[, 1:500]
      } else {
        driv_mat <- sapply(1, function(x) noaa_df()$lst[[x]]$airt[noaa_df()$lst[[x]]$Date >= fc_date])
      }
      if("Parameter" %in% fc_uncertA | "Total" %in% fc_uncertA) {
        
        idx <- sample(1:5000, 500)
        
        if(input$mod_selec_tot_fc[2] == "Wtemp") {
          pars <- param_dist3b$dist[[2]]
          params <- data.frame(m = pars$m[idx],
                               b = pars$b[idx])
        } else if (input$mod_selec_tot_fc[2] == "Both") {
          pars <- param_dist3b$dist[[4]]
          params <- data.frame(beta1 = pars$beta1[idx],
                               beta2 = pars$beta2[idx],
                               beta0 = pars$beta0[idx])
        } else if(input$mod_selec_tot_fc[2] == "Atemp") {
          pars <- param_dist3b$dist[[3]]
          params <- data.frame(m = pars$m[idx],
                               b = pars$b[idx])
        } else {
          params <- "NULL"
        }
      } else {
        if(input$mod_selec_tot_fc[2] == "Wtemp") {
          pars <- param_dist3b$dist[[2]]
          params <- data.frame(m = mean(pars$m),
                               b = mean(pars$b))
        } else if (input$mod_selec_tot_fc[2] == "Both") {
          pars <- param_dist3b$dist[[4]]
          params <- data.frame(beta1 = mean(pars$beta1),
                               beta2 = mean(pars$beta2),
                               beta0 = mean(pars$beta0))
        } else if(input$mod_selec_tot_fc[2] == "Atemp") {
          pars <- param_dist3b$dist[[3]]
          params <- data.frame(m = mean(pars$m),
                               b = mean(pars$b))
        } else {
          params <- "NULL"
        }
      }
      if("Initial Conditions" %in% fc_uncertA | "Total" %in% fc_uncertA) {
        if(input$mod_selec_tot_fc[2] %in% c("Wtemp","Both","Pers")){
          mat[1, ] <- rnorm(500, df$wtemp[which(df$Date == fc_date)], sd = 0.1)
        } else {
          mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
        }
      } else {
        mat[1, ] <- df$wtemp[which(df$Date == fc_date)]
      }
      
      for(mem in 2:nrow(mat)) {
        # Calculate process noise each step
        if("Process" %in% fc_uncertA | "Total" %in% fc_uncertA) {
          if(input$mod_selec_tot_fc[2] == "Wtemp") {
            Wt <- rnorm(500, 0, sigma_table$df[2,2])
          } else if (input$mod_selec_tot_fc[2] == "Both") {
            Wt <- rnorm(500, 0, sigma_table$df[4,2])
          } else if(input$mod_selec_tot_fc[2] == "Atemp") {
            Wt <- rnorm(500, 0, sigma_table$df[3,2])
          } else if(input$mod_selec_tot_fc[2] == "Pers") {
            Wt <- rnorm(500, 0, sigma_table$df[1,2])
          }
        } else {
          Wt <- 0
        }
        
        if(input$mod_selec_tot_fc[2] == "Atemp") {
          mat[mem, ] <- params$m * driv_mat[mem, ] + params$b + Wt
        } else if (input$mod_selec_tot_fc[2] == "Pers") {
          mat[mem, ] <- mat[mem-1, ] + Wt
        } else if (input$mod_selec_tot_fc[2] == "Wtemp") {
          mat[mem, ] <- params$m * mat[mem-1, ] + params$b + Wt
        } else if (input$mod_selec_tot_fc[2] == "Both") {
          mat[mem, ] <- mat[mem-1, ] * params$beta1 + driv_mat[mem, ] * params$beta2 + params$beta0 + Wt
        }
      }
      
      # Calculate distributions
      tot_fc_dataB$mat <- mat
      
      if(fc_uncertA == "Total") {
        dat <- apply(mat, 1, function(x) {
          quantile(x, c(0.05, 0.5, 0.95))
        })
        dat <- as.data.frame(t(dat))
        colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
        dat$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
        tot_fc_dataB$dist <- dat
        df2 <- as.data.frame(mat)
        df2$Date <- seq.Date(from = as.Date(fc_date), length.out = 8, by = 1)
        mlt <- reshape::melt(df2, id.vars = "Date")
        tot_fc_dataB$mlt <- mlt
      }
      
      if(fc_uncertA != "Total") {
        # Quantify UC
        std <- apply(tot_fc_dataB$mat, 1, sd)
        
        df2 <- data.frame(Date = seq.Date(from = as.Date(fc_date), length.out = 8, by = 1),
                          sd = std, label = fc_uncertA)
        
        if(is.null(tot_fc_dataB$quant_df)) {
          tot_fc_dataB$quant_df <- df2
        } else {
          # Overwrite previous Std Dev.
          if((df2$label[1] %in% tot_fc_dataB$quant_df$label)) {
            idx <- which(tot_fc_dataB$quant_df$label %in% df2$label[1])
            tot_fc_dataB$quant_df[idx, ] <- df2
          } else {
            tot_fc_dataB$quant_df <- rbind(tot_fc_dataB$quant_df, df2)
          }
        }
      }
      progress$set(value = (pidx / length(uc_sources)))
    }
    return(tot_fc_dataB)
    }
  })
  
  # create output object for total uncertainty forecast plot for Model B
  output$tot_fc_uncertB <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(!is.null(sigma_table$df),
           message = "Need to generate process uncertainty distributions in Objective 5.")
    )
    validate(
      need(any(!is.na(param_dist3b$dist)),
           message = "Need to generate parameter uncertainty distributions in Objective 6.")
    )
    validate(
      need(!is.null(ic_dist$df), "Need to generate initial conditions uncertainty distribution in Objective 7.")
    )
    validate(
      need(!is.null(noaa_df()$airt), "Need to load NOAA air temperature forecast in Objective 8.")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )
    
    idx <- which(mod_names == input$mod_selec_tot_fc[2])
    sel_col <- cols[idx]
    
    if(idx != 1) {
      validate(
        need(!is.na(param_dist3b$dist[[idx]]), "Need to generate parameters in Objective 6.")
      )
    }
    validate(
      need(!is.null(noaa_df()$airt), "Please click 'Load forecast' in Objective 8.")
    )
    validate(
      need(!is.null(tot_fc_dataB()$dist), "Click 'Run forecast'")
    )
    
    dat <- wtemp_fc_data$hist[wtemp_fc_data$hist$Date >= (as.Date(fc_date) - 1), ]
    
    p <- ggplot() +
      geom_point(data = dat, aes(Date, wtemp, color = "Water temp. - observed")) +
      geom_vline(xintercept = as.Date(fc_date), linetype = "dashed") +
      ylab("Temperature (\u00B0C)") +
      theme_bw(base_size = 12) +
      labs(color = NULL)
    
    if(input$plot_type_totB == "Line") {
      if(!is.null(tot_fc_dataB()$mlt)) {
        
        mlt <- tot_fc_dataB()$mlt
        
        p <- p +
          geom_line(data = mlt, aes(Date, value, group = variable, color = input$mod_selec_tot_fc[2]), alpha = 0.6) +
          labs(color = NULL)
      }
    } else if(input$plot_type_totB == "Distribution") {
      if(!is.null(tot_fc_dataB()$dist)) {
        mlt <- tot_fc_dataB()$dist
        
        p <- p +
          geom_ribbon(data = mlt, aes(Date, ymin = p5, ymax = p95, fill = input$mod_selec_tot_fc[2]), alpha = 0.3) +
          geom_line(data = mlt, aes(Date, p50, color = input$mod_selec_tot_fc[2])) +
          labs(color = NULL, fill = NULL)
      }
    }
    
    p <- p +
      scale_color_manual(values = c("Air temp." = cols[1], "Water temp." = cols[2],
                                    "Pers" = cols[3], "Wtemp" = cols[4],
                                    "Atemp" = cols[5], "Both" = cols[6])) +
      scale_fill_manual(values = c("Pers" = l.cols[1], "Wtemp" = l.cols[2],
                                   "Atemp" = l.cols[3], "Both" = l.cols[4])) +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      labs(color = NULL, fill = NULL)
    
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  })
  
  # create plot output for partitioned uncertainty for Model B
  fc_quantB_plot <- reactiveValues(main = NULL)
  
  output$fc_quantB <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(all_mods_df$df),
           message = "Please fit models in Objective 3.")
    )
    validate(
      need(!is.null(sigma_table$df),
           message = "Need to generate process uncertainty distributions in Objective 5.")
    )
    validate(
      need(any(!is.na(param_dist3b$dist)),
           message = "Need to generate parameter uncertainty distributions in Objective 6.")
    )
    validate(
      need(!is.null(ic_dist$df), "Need to generate initial conditions uncertainty distribution in Objective 7.")
    )
    validate(
      need(!is.null(noaa_df()$airt), "Need to load NOAA air temperature forecast in Objective 8.")
    )
    validate(
      need(length(input$mod_selec_tot_fc) == 2, "Select two models above.")
    )
    validate(
      need(!is.null(tot_fc_dataB()$quant_df), "Click 'Run forecast' above.")
    )
    validate(
      need(input$quant_ucB > 0, "Click 'Quantify uncertainty'")
    )
    
    p <- ggplot() +
      geom_bar(data = tot_fc_dataB()$quant_df, aes(Date, sd, fill = label), stat = "identity", position = "stack") +
      ylab("Standard Deviation (\u00B0C)") +
      scale_fill_manual(values = c("Process" = cols2[1], "Parameter" = cols2[2], "Initial Conditions" = cols2[3],
                                   "Driver" = cols2[4], "Total" = cols2[5])) +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      labs(fill = "Uncertainty") +
      theme_bw(base_size = 12)
    
    fc_quantB_plot$main <- p
    
    gp <- ggplotly(p) #, dynamicTicks = TRUE)
    return(gp)
  })
  
  # save plot of quantified uncertainty for model B
  output$save_fc_quantB_plot <- downloadHandler(
    filename = function() {
      paste("Q37a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = fc_quantB_plot$main, device = device)
    }
  )
  
  # end Objective 9
  
  
  #### Objective 10 - Management Scenario
  
  # create output plot for scenario 1
  output$scen1_plot <- renderPlot({
    
    p <- ggplot(scen_fc1) +
      geom_hline(yintercept = 12, linetype = "dashed") +
      geom_ribbon(aes(Date, ymin = surf_lci, ymax = surf_uci, fill = "Surface"), alpha = 0.4) +
      geom_ribbon(aes(Date, ymin = bot_lci, ymax = bot_uci, fill = "Bottom"), alpha = 0.4) +
      geom_line(aes(Date, surftemp, color = "Surface")) +
      geom_line(aes(Date, bottemp, color = "Bottom")) +
      ylab("Temperature (\u00B0C)") +
      xlab("Day") +
      guides(color = "none") +
      labs(fill = "Location") +
      scale_x_date(breaks = "1 day", date_labels = "%a") +
      scale_color_manual(values = c(p.cols[c(6, 2)]), breaks = c("Surface", "Bottom")) +
      scale_fill_manual(values = c(p.cols[c(5, 1)]), breaks = c("Surface", "Bottom")) +
      # scale_fill_discrete(breaks = c("Surface", "Bottom")) +
      coord_cartesian(ylim = c(8, 14)) +
      theme_bw(base_size = 22)
    return(p)
    
  })
  
  # create output plot for scenario 2
  output$scen2_plot <- renderPlot({
    
    validate(
      need(input$dec_scen1 %in% c("Surface","Bottom"), "Complete Decision #1 above.")
    )
    
    p <- ggplot(scen_fc2) +
      geom_hline(yintercept = 12, linetype = "dashed") +
      geom_ribbon(aes(Date, ymin = surf_lci, ymax = surf_uci, fill = "Surface"), alpha = 0.4) +
      geom_ribbon(aes(Date, ymin = bot_lci, ymax = bot_uci, fill = "Bottom"), alpha = 0.4) +
      geom_line(aes(Date, surftemp, color = "Surface")) +
      geom_line(aes(Date, bottemp, color = "Bottom")) +
      ylab("Temperature (\u00B0C)") +
      xlab("Day") +
      guides(color = "none") +
      labs(fill = "Location") +
      scale_x_date(breaks = "1 day", date_labels = "%a") +
      scale_color_manual(values = c(p.cols[c(6, 2)]), breaks = c("Surface", "Bottom")) +
      scale_fill_manual(values = c(p.cols[c(5, 1)]), breaks = c("Surface", "Bottom")) +
      coord_cartesian(ylim = c(8, 14)) +
      theme_bw(base_size = 22)
    return(p)
    
  })
  
  # end Objective 10
  
  
  #### Navigating Tabs ----
  
  # Navigating Tabs ----
  #* Main Tab ====
  rv1 <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$maintab, {
    curr_tab1 <- input$maintab
    rv1$prev <- readr::parse_number(curr_tab1) - 1
    rv1$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  observe({
    toggleState(id = "prevBtn1", condition = rv1$prev > 0)
    if(rv1$nxt > 6 & rv3a$nxt > 10) {
      shinyjs::disable("nextBtn1")
    } else {
      shinyjs::enable("nextBtn1")
    }
    hide(selector = ".page")
  })
  
  
  # Next button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx + 1]
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if(curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]    } 
    if(curr_tab1 == "mtab6" & rv3a$nxt > 10) {
      updateActionButton(session, inputId = "nextBtn1", label = paste("End of module"))
    } else {
      updateActionButton(session, inputId = "nextBtn1", label = paste(new_nam, ">"))
    }
  })
  
  # Previous button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx - 1]
    
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj1") idx2 <- idx2 - 1 # Move off Activty A label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj5") idx2 <- idx2 - 1 # Move off Activty B label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj9") idx2 <- idx2 - 1 # Move off Activty C label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if(curr_tab1 == "mtab1") {
      updateActionButton(session, inputId = "prevBtn1", label = paste("Module begins"))
    } else {
      # shinyjs::show(id = "prevBtn1")
      updateActionButton(session, inputId = "prevBtn1", label = paste("<", new_nam))
    }
  })
  
  
  # Advancing Tabs
  observeEvent(input$nextBtn1, {
    
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab4" & rv1a$nxt < 5) {
      curr_obj <- input$tabseries1
      
      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("obj", rv1a$nxt))
      
    } else if (curr_tab1 == "mtab5" & rv2a$nxt < 9) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("obj", rv2a$nxt))
    } else if (curr_tab1 == "mtab6" & rv3a$nxt < 11) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("obj", rv3a$nxt))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj1")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj5")
      updateTabsetPanel(session, "tabseries3",
                        selected = "obj9")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$nxt))
    }
    shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
  })
  
  # Moving back through tabs
  observeEvent(input$prevBtn1, {
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab4" & rv1a$prev > 0) {
      curr_obj <- input$tabseries1
      
      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("obj", rv1a$prev))
      
    } else if (curr_tab1 == "mtab5" & rv2a$prev > 4) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("obj", rv2a$prev))
    } else if (curr_tab1 == "mtab6" & rv3a$prev > 8) {
      curr_obj <- input$tabseries3
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("obj", rv3a$prev))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj4")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj8")
      updateTabsetPanel(session, "tabseries3",
                        selected = "obj10")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$prev))
    }
    shinyjs::runjs("window.scrollTo(0, 0)")
    
  })
  
  #* Tab 1a ----
  rv1a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries1, {
    curr_tab1 <- input$tabseries1
    rv1a$prev <- readr::parse_number(curr_tab1) - 1
    rv1a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  #* Tab 2a ----
  rv2a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries2, {
    curr_tab1 <- input$tabseries2
    rv2a$prev <- readr::parse_number(curr_tab1) - 1
    rv2a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  #* Tab 3a ----
  rv3a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries3, {
    curr_tab1 <- input$tabseries3
    rv3a$prev <- readr::parse_number(curr_tab1) - 1
    rv3a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  # Bookmarking
  bookmarkingWhitelist <- c("plot_airt_swt","plot_airt_swt2","plot_persist", "plot_airt_swt3",
  "plot_airt_swt4","add_lm","add_lm1","plot_mlr","view_at_fc","fc1_Pers","fc1_Wtemp",
  "fc1_Atemp","fc1_Both","gen_proc_dist","fc2_Pers","fc2_Wtemp","fc2_Atemp","fc2_Both",
  "fit_model_year_1","fit_model_year_2","param_Pers","fc3_Pers","param_Wtemp","fc3_Wtemp",
  "param_Atemp","fc3_Atemp","param_Both","fc3_Both","gen_ic","fc4_Pers","fc4_Wtemp","fc4_Atemp",
  "fc4_Both","load_noaa_at","fc5_Pers","fc5_Wtemp","fc5_Atemp","fc5_Both",
  "run_tot_fcA","quant_ucA","run_tot_fcB","quant_ucB","dec_scen1","dec_scen2")
  
  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })

  ExcludedIDs <- reactiveVal(value = NULL)

  observe({
    toExclude <- setdiff(names(input), bookmarkingWhitelist)
    setBookmarkExclude(toExclude)
    ExcludedIDs(toExclude)
  })

  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$sel_row <- input$table01_rows_selected
    state$values$sel_mods <- input$mod_selec_tot_fc
  })

  # Read values from state$values when we restore
  onRestore(function(state) {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab4")
    updateTabsetPanel(session, "tabseries1",
                      selected = "obj1")
  })

  onRestored(function(state) {
    updateSelectizeInput(session, "row_num", selected = state$values$sel_row)
    updateSelectizeInput(session, "mod_selec_tot_fc", selected = state$values$sel_mods)
  })


})

# end

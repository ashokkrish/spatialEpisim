#---------------------------
#     Server Components
#---------------------------
server <- function(input, output, session) {
  
  iv <- InputValidator$new()
  iv_alpha <- InputValidator$new()
  iv_seeddataupload <- InputValidator$new() 
  
  # Non-spatial Modelling
  
  # muValue
  # iv$add_rule("muBirth", sv_required())
  # iv$add_rule("muBirth", sv_gte(0))
  #iv$add_rule("muBirth", sv_lte(0.1))
  
  # iv$add_rule("muDeath", sv_required())
  # iv$add_rule("muDeath", sv_gte(0))
  #iv$add_rule("muDeath", sv_lte(0.1))
  
  # Spatial Modelling
  
  iv$add_rule("selectedCountry", sv_required())
  
  iv_alpha$add_rule("alpha", sv_required())
  iv_alpha$add_rule("alpha", sv_gte(0))
  
  iv$add_rule("beta", sv_required())
  iv$add_rule("beta", sv_gte(0))
  
  iv$add_rule("gamma", sv_required())
  iv$add_rule("gamma", sv_gte(0))
  
  iv$add_rule("sigma", sv_required())
  iv$add_rule("sigma", sv_gte(0))
  
  iv$add_rule("delta", sv_required())
  iv$add_rule("delta", sv_gte(0))
  
  iv$add_rule("lambda", sv_required())
  iv$add_rule("lambda", sv_integer())
  iv$add_rule("lambda", sv_gt(0))
  
  iv$add_rule("date", sv_required())
  
  iv$add_rule("timestep", sv_required())
  iv$add_rule("timestep", sv_integer())
  iv$add_rule("timestep", sv_gt(0))
  
  iv_seeddataupload$add_rule("seedData", sv_required())
  iv_seeddataupload$add_rule("seedData", ~ if(is.null(fileInputs$smStatus) || fileInputs$smStatus == 'reset') "Required")
  
  iv_alpha$condition(~ isTRUE(input$modelSelect == "SVEIRD"))
  
  iv$add_validator(iv_alpha)
  iv$add_validator(iv_seeddataupload)
  
  iv$enable()
  iv_alpha$enable()
  iv_seeddataupload$enable()
  
  observe({
    if(iv$is_valid()){
      shinyjs::enable(id = "go")
    } else {
      shinyjs::disable(id = "go")
    }
  })
  
  # observeEvent(iv$is_valid(), {
  #   shinyjs::enable(id = "go")
  # })
  # Reset vital dynamics when not checked off
  observe({
    input$muValue
    updateNumericInput(session, "muBirth", value = 0)
  })
  observe({
    input$muValue
    updateNumericInput(session, "muDeath", value = 0)
  })
  
  values <- reactiveValues()
  values$allow_simulation_run <- TRUE
  
  fileInputs <- reactiveValues(
    smStatus = NULL
  )

  # output$table <- renderTable(values$df)
  
  susceptible <- reactive({
    req(!is.null(input$selectedCountry) && input$selectedCountry != "")
    
    createSusceptibleLayer(input$selectedCountry, 0, FALSE, level1Names = NULL)
  })
  
  ############################################################################    
  # Create a country plot cropped by level1Identifier and output to UI       #
  ############################################################################ 
  # observeEvent(input$go, {
  #   if(input$cropLev1 == TRUE){
  #     output$croppedOutputImage <- renderImage({
  #
  #       outfile <- tempfile(fileext = '.png')
  #       
  #       png(outfile, width = 800, height = 600)
  #       createCroppedRaster(selectedCountry = input$selectedCountry, level1Region = input$level1List, rasterAgg = input$agg, directOutput = T)
  #       dev.off()
  #       
  #       list(src = outfile, contentType = 'image/png', width = 600, height = 400, alt = "Base plot image not found")
  #     }, deleteFile = TRUE)
  #   }
  # })
  
  ############################################################################    
  # Output population base plot image to the app UI                          #
  ############################################################################ 
  observeEvent(input$go, {
    req(iv$is_valid())
    output$outputImage <- renderImage({
      
      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 1068, height = 768)
      
      if(input$cropLev1) {
        req(input$level1List != "")
        isolate(createCroppedRaster(selectedCountry = input$selectedCountry, level1Region = input$level1List, susceptible()$Susceptible, directOutput = TRUE))
      } else {
        isolate(createBasePlot(selectedCountry = input$selectedCountry, susceptible()$Susceptible, directOutput = TRUE))  # print the susceptible plot direct to UI
      }
      
      dev.off()
      
      list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Base plot image not found")
      # The above line adjusts the dimensions of the base plot rendered in UI
    }, deleteFile = TRUE)
  })
  
  ############################################################################    
  # Output IDE equations image to the app UI                                 #
  ############################################################################ 
  observeEvent(input$go, {
    req(iv$is_valid())
    output$modelImg <- renderImage({
      return(list(src= "www/ModelEquations.png",
                  contentType = "image/png"))
    }, deleteFile = FALSE)
  })
  
  ############################################################################    
  # Output flowchart image to the app UI                                     #
  ############################################################################ 
  observeEvent(input$go, {
    req(iv$is_valid())
    output$flowchartImg <- renderImage({
      if (input$modelSelect == "SEIRD"){
        return(list(src= "www/SEIRD.png",
                    contentType = "image/png"))
      }
      else if (input$modelSelect == "SVEIRD"){
        return(list(src = "www/SVEIRD.png",
                    contentType = "image/png"))
      }
    }, deleteFile = FALSE)
  })
  
  ############################################################################    
  # Reset all parameter sliders, country selection, etc.                     #
  ############################################################################ 
  observeEvent(input$resetAll, {
    shinyjs::reset("dashboard")
    
    shinyjs::disable(id = "go")
    values$allow_simulation_run <- FALSE
  })
  
  ############################################################################    
  # Checks to see that a new file has been uploaded (helper func)            #
  ############################################################################ 
  observeEvent(input$seedData, {
    values$allow_simulation_run <- TRUE
    fileInputs$smStatus <- 'uploaded'
  })
  
  ############################################################################    
  # Check if all mandatory fields have a value                               #
  ############################################################################   
  # observe({
  #       mandatoryFilled <-
  #       vapply(fieldsMandatory,
  #              function(x) {
  #                !is.null(input[[x]]) && input[[x]] != ""
  #              },
  #              logical(1))
  #     
  #       mandatoryFilled <- all(mandatoryFilled)
  # 
  #     # enable/disable the submit button
  #     if (isolate(values$allow_simulation_run) == TRUE){
  #       shinyjs::toggleState(id = "go", condition = mandatoryFilled)
  #   }
  # })
  
  ##############################################################################
  #highlight drop down item when hovering                                      #
  ##############################################################################
  # observe({
  #   hoverDrop <-
  #     vapply(hoverDrop,
  #            function(x) {
  #              !is.null(input[[x]]) && input[[x]] != ""
  #            },
  #            logical(1))
  #   hoverDrop <- all(hoverDrop)
  #   # enable/disable the submit button
  #   if (isolate(values$allow_simulation_run) == TRUE){
  #     shinyjs::toggleClass(class = hoverDrop)
  #   }
  # })
  
  ############################################################################    
  # This static ui field is in server since other dynamic ui elements need it#
  ############################################################################
  output$countryDropdown <- renderUI({
    pickerInput(
      inputId = "selectedCountry",
      label = (strong("Country")), 
      choices = shortlist$Country,
      multiple = FALSE,
      selected = NULL, # "Democratic Republic of Congo", #
      options = pickerOptions(
        actionsBox = TRUE,
        title = "Please select a country")
    )
  })
  
  ############################################################################    
  # Dynamically display the checkbox option to select for states/provinces   #
  ############################################################################
  output$cropStateCheckbox <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(
        inputId = "cropLev1", 
        label = strong("Crop State(s)/Province(s)"), 
        value = FALSE)
    }
  })
  
  ############################################################################     
  # Checkbox for Data Assimilation                                           #
  ############################################################################ 
  output$dataAssimCheckbox <- renderUI({
    validate(need(!is.null(input$selectedCountry), ""))
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(inputId = "dataAssim", label = strong("Include data assimilation?"), value = FALSE)
    }
  })
  
  ############################################################################    
  # Create select box for choosing input country                             #
  ############################################################################      
  output$Level1Ui <- renderUI({
    validate(need(input$cropLev1 == TRUE, "")) # catches UI warning
    
    isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c")
    
    if (file.exists(paste0("gadm/", "gadm36_", toupper(isoCode), "_1_sp.rds"))){
      level1Options <<- readRDS(paste0("gadm/", "gadm36_", toupper(isoCode), "_1_sp.rds"))$NAME_1 
    } else {
      level1Options <<- getData("GADM", download = TRUE, level = 1, country = toupper(isoCode))$NAME_1 
    }
    
    selectizeInput(inputId = "level1List", "",
                   choices = level1Options,
                   # selected = c("Kwara", "Oyo"),
                   multiple = TRUE,
                   options = list(placeholder = "Select state(s)/province(s)"))
    
  })
  
  ############################################################################     
  # Radio button for SEIRD vs SVEIRD Model                                   #
  ############################################################################   
  output$modelRadio <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      radioButtons(inputId = "modelSelect",
                   label = strong("Epidemic Model"),
                   choiceValues = list("SEIRD","SVEIRD"),
                   choiceNames = list("SEIRD","SVEIRD"),
                   selected = "SEIRD", #character(0), # 
                   inline = TRUE,
                   width = "1000px")
    }
  })
  
  ############################################################################     
  # Radio button for Deterministic vs Stochastic Model                       #
  ############################################################################  
  output$stochasticRadio <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      radioButtons(inputId = "stochasticSelect",
                   label = strong("Model Stochasticity"),
                   choiceValues = list("Deterministic", "Stochastic"),
                   choiceNames = list("Deterministic", "Stochastic"),
                   selected = "Deterministic", #character(0), #
                   inline = TRUE,
                   width = "1000px")
    }
  })
  
  ############################################################################    
  # TODO: refactor numericInputs into single function                        #
  ############################################################################ 
  output$alphaInput <- renderUI({
    alphaValue <- 0.00015 # 0.2100
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"alpha"])
        } else if (input$selectedCountry == "Nigeria"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"alpha"])
        } else if (input$selectedCountry == "Uganda"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"alpha"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"alpha"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"alpha"])
        } else if (input$selectedCountry == "Nigeria"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"alpha"])
        }else if (input$selectedCountry == "Uganda"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"alpha"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"alpha"])}
      }
      
      numericInput(inputId = "alpha",
                   label = HTML(paste("Daily Vaccination Rate (&#945)")),
                   value = alphaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$betaInput <- renderUI({
    req(!is.null(input$modelSelect))
    betaValue <- 0.055 # 0.00001
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"beta"])
        } else if (input$selectedCountry == "Nigeria"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"beta"])}
        else if (input$selectedCountry == "Uganda"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"beta"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"beta"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"beta"])
        } else if (input$selectedCountry == "Nigeria"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"beta"])}
        else if (input$selectedCountry == "Uganda"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"beta"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"beta"])}
      }
      
      numericInput(inputId = "beta",
                   label = HTML(paste("Daily Exposure Rate (&#946)")), 
                   value = betaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$gammaInput <- renderUI({
    req(!is.null(input$modelSelect))
    gammaValue <- 0.009 #0.008
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"gamma"])
        } else if (input$selectedCountry == "Nigeria"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"gamma"])}
        else if (input$selectedCountry == "Uganda"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"gamma"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"gamma"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"gamma"])
        } else if (input$selectedCountry == "Nigeria"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"gamma"])}
        else if (input$selectedCountry == "Uganda"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"gamma"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"gamma"])}
      }
      
      numericInput(inputId = "gamma",
                   label = HTML(paste("Daily Infection Rate (&#947)")), 
                   value = gammaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$sigmaInput <- renderUI({
    req(!is.null(input$modelSelect))
    sigmaValue <- 0.065
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"sigma"])
        } else if (input$selectedCountry == "Nigeria"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Uganda"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"sigma"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"sigma"])
        } else if (input$selectedCountry == "Nigeria"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Uganda"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"sigma"])}
      }
      
      numericInput(inputId = "sigma",
                   label = HTML(paste("Daily Recovery Rate (&#963)")), 
                   value = sigmaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$deltaInput <- renderUI({
    req(!is.null(input$modelSelect))
    deltaValue <- 0.0015
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"delta"])
        } else if (input$selectedCountry == "Nigeria"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Uganda"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"delta"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"delta"])
        } else if (input$selectedCountry == "Nigeria"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Uganda"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"delta"])}
      }
      
      numericInput(inputId = "delta",
                   label = HTML(paste("Daily Death Rate (&#948)")),
                   value = deltaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  
  #helpText('NOTE: Radius of 1 is called the Moore neighbourhood.'),
  #HTML("<p>NOTE: Radius of 1 is called the <a href='https://en.wikipedia.org/wiki/Moore_neighborhood'>Moore neighbourhood</a></p>", target = "_blank"),
  #p("NOTE:Radius of 1 is called the",a("Moore neighbourhood", href="https://en.wikipedia.org/wiki/Moore_neighborhood", target="_blank")),
  
  output$lambdaInput <- renderUI({
    req(!is.null(input$modelSelect))
    lambdaValue <- 15
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"lambda"])
        } else if (input$selectedCountry == "Nigeria"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"lambda"])}
        else if (input$selectedCountry == "Uganda"){
          lambdaValue <- 5}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"lambda"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"lambda"])
        } else if (input$selectedCountry == "Nigeria"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"lambda"])
        }
        else if (input$selectedCountry == "Uganda"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"lambda"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"lambda"])}
      }
      
      numericInput(inputId = "lambda",
                   label = HTML(paste("Distance Parameter (&#955)")),
                   value = lambdaValue,min = 1, max = 50, step = 1)
    }
  })
  
  ############################################################################    
  #                     Upload Seed Data                                     #
  ############################################################################ 
  output$seedUpload <- renderUI({
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      fileInput(inputId = "seedData", 
                label = "Upload Seed Data:", 
                placeholder = "Upload seed data (.csv or .xls or .xlsx)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"),
      )
      
      #p("Click ", a("here", href="https://docs.google.com/spreadsheets/d/1aEfioSNVVDwwTt6ky7MrOQj5uGO7QQ1NTB2TdwOBhrM/edit?usp=sharing", target="_blank"), "for a template of initial seed data")
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$startDateInput <- renderUI({
    req(!is.null(input$modelSelect))
    startDateInput <- Sys.Date() # NULL
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          startDateInput <- "2020-09-01" #filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"startDate"]
        } else if (input$selectedCountry == "Nigeria"){
          startDateInput <- "2020-09-01" #filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"startDate"]
        }
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          startDateInput <- "2021-09-01" #filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"startDate"]
        } else if (input$selectedCountry == "Nigeria"){
          startDateInput <- "2021-09-01" #filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"startDate"]
        }
      }
      if (input$selectedCountry == "Uganda") {
        startDateInput <- "2022-10-20"
      }
      else if (input$selectedCountry == "Democratic Republic of Congo") {
        startDateInput <- "2018-08-01"}
      
      dateInput('date', "Choose simulation start date:", value = startDateInput, max = Sys.Date(),
                format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                language = "en", width = NULL)
    }
  })
  
  ############################################################################     
  # numeric input for number of iterations                                   #
  ############################################################################  
  output$timestepInput <- renderUI({
    timestepValue <- 10
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (input$selectedCountry == "Czech Republic" || input$selectedCountry == "Nigeria"){timestepValue = 120}
    else if (input$selectedCountry == "Democratic Republic of Congo") {timestepValue = 440}
    else if (input$selectedCountry == "Uganda") {timestepValue = 63}
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "timestep",
                   label = "Number of Iterations (days)",
                   min = 1, max = 3650, value = timestepValue, step = 1)}
  }
  )
  ############################################################################     
  # Data Assimilation settings                                               #
  ############################################################################
  
  output$dataAssimCmpts <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
    
    checkboxGroupInput(inputId = "selectedCompartments", 
                       "Select observable compartment(s)",
                       choices = c("V", "E", "I", "R", "D"),
                       selected = c("I"), 
                       inline = TRUE,
    )
  })
  showI <- reactive({
    "I" %in% input$selectedCompartments
  })
  
  showD <- reactive({
    "D" %in% input$selectedCompartments
  })
  
  output$dataAssimZones <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
    if (!is.null(input$selectedCountry) && input$selectedCountry != "") {
      fileInput(inputId = "dataAssimZones", 
                label = ("Upload the lat/lon coordinates of reporting health zones (.csv or .xls or .xlsx)"),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"),)
    }
  })
  
  observeEvent(input$dataAssimZones, {
    print(read.csv(input$dataAssimZones$datapath))
    print(as.character(input$dataAssimZones[1]))})
  
  output$dataAssimFileI <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
    if (showI()) {
      fileInput(inputId = "assimIData", 
                label = ("Upload infection data to be assimilated with the model (.csv or .xls or .xlsx)"),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"), )
    }
  })
  output$dataAssimFileD <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
    if (showD()) {
      fileInput(inputId = "assimDData", 
                label = ("Upload death data to be assimilated with the model (.csv or .xls or .xlsx)"),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"),
      )
    }
  })
  # output$dataAssimCmpts <- renderUI({
  #   validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
  #   
  #   selectizeInput(inputId = "level1List", "Select observable compartments",
  #                  choices = c("V", "E", "I", "R", "D"),
  #                  selected = "", multiple = TRUE,
  #                  options = list(placeholder = ""))
  #})
  
  ############################################################################    
  # Change the function which generates the Q matrix     #
  ############################################################################  
  output$varCovarFunc <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      selectInput(inputId = "covarianceSelect",
                  label = HTML("<span class='label-text'>Choose variance-covariance function:</span>"),
                  choices = list("DBD", "Balgovind", "Exponential", "Gaussian", "Spherical"),
                  # HTML("<span class='option-text'>Distance-Based Decay</span>"),
                  # HTML("<span class='option-text'>Balgovind</span>"),
                  # HTML("<span class='option-text'>Exponential</span>"),
                  # HTML("<span class='option-text'>Gaussian</span>"),
                  # HTML("<span class='option-text'>Spherical</span>")
                  #),
                  selected = "DBD", #character(0), #
                  width = "1000px",
                  multiple = FALSE)
    }
  })
  
  ############################################################################    
  # Adjust parameter values for the variance=covariance function     #
  ############################################################################  
  
  output$selectRho <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "QCorrLength",
                   label = "Choose correlation length parameter for generating Q:",
                   value = 0.675,
                   step = 0.001,
                   min = 0)
    }
  })
  
  output$selectSigma <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "QVar",
                   label = "Choose variance parameter for generating Q:",
                   value = 0.55,
                   step = 0.01,
                   min = 0)
    }
  })
  
  output$selectNbhd <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "nbhd",
                   label = "Choose neighborhood parameter for generating Q:",
                   value = 3,
                   step = 1,
                   min = 0)
    }
  })
  
  output$selectPsiDiag <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "psidiag",
                   label = HTML(paste("Choose a value for the zero elements of", TeX("&#936"), "to be set to:")),
                   value = 0.001,
                   step = 0.001,
                   min = 0)
    }
  })
  
  ############################################################################    
  # Change the recommended aggregation factor for slider dynamically         #
  ############################################################################  
  output$aggInput <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      sliderInput(inputId = "agg",
                  label = "Aggregation Factor",
                  min = 0, max = 100, step = 1, value = population$reco_rasterAgg[match(input$selectedCountry, population$Country)])
    }
  })
  
  ############################################################################    
  # Output the .mp4 video from www/ to the app UI                            #
  ############################################################################  
  output$outputVideo <- renderUI({
    tags$video(
      id = "video", 
      type = "video/mp4",
      src = "MP4/Infected_MP4.mp4",  # TODO: dynamically change which mp4 is printed
      controls = "controls"
    )
  })
  
  ############################################################################    
  # Output bubble plot with initial seed data directly to the app UI         #
  ############################################################################ 
  # observeEvent(input$go, {
  #   output$seededOutputImage <- renderImage({
  #     source("R/plotSeedData.R", encoding="UTF-8")
  #     outfile <- tempfile(fileext = '.png')
  #     
  #     # print the seed plot direct to UI
  #     png(outfile, width = 1024, height = 768)
  #     plot(c(1,3,6,9,12), c(1.5,2,7,8,15), main = "Bubble Plot Placeholder") # TODO: example plot, below lines don't work due to "Regions defined for each Polygons" warning
  #     # createSeedPlot(countryName = "Czech Republic", seedData = "seeddata/CZE_InitialSeedData.csv", startDate = "2021-07-01", source = "testSource") 
  #     dev.off()
  #     
  #     list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Seed image not found")
  #   }, deleteFile = TRUE)
  # })
  
  lineThickness <- 1.5
  
  observeEvent(input$go, {
    req(iv$is_valid())
    
    output$infectedExposedPlot <- makePlot(
                                    compartments = c("E", "I"), 
                                    input = input, 
                                    plotTitle = paste0("Time-series plot of Exposed and Infectious compartments in ", input$selectedCountry), 
                                    xTitle = paste0("Day (from ", input$date, ")"), 
                                    yTitle = "Compartment Value", 
                                    lineThickness = lineThickness)
    
    output$cumulativePlot <- makePlot(
                              compartments = c("D"), 
                              input = input, 
                              plotTitle = paste0("Estimated Cumulative COVID-19 Deaths in ", input$selectedCountry), 
                              xTitle = paste0("Day (from ", input$date, ")"), 
                              yTitle = "Cumulative Deaths", 
                              lineThickness = lineThickness)
    
    if (input$modelSelect == "SVEIRD"){
      output$fullPlot <- makePlot(
                           compartments = c("S", "V", "E", "I", "R", "D"), 
                           input = input, 
                           plotTitle = paste0("Time-series plot of epidemic compartments in ", input$selectedCountry), 
                           xTitle = paste0("Day (from ", input$date, ")"), 
                           yTitle = "Compartment Value", 
                           lineThickness = lineThickness)
    } else {
      output$fullPlot <- makePlot(
                           compartments = c("S", "E", "I", "R", "D"), 
                           input = input, 
                           plotTitle = paste0("Time-series plot of epidemic compartments in ", input$selectedCountry), 
                           xTitle = paste0("Day (from ", input$date, ")"), 
                           yTitle = "Compartment Value", 
                           lineThickness = lineThickness)
    }
    
    # output$fracSusPlot <- renderImage({
    #   outfile <- tempfile(fileext = '.png')
    #   
    #   png(outfile, width = 1024, height = 768)
    #   df <- read.xlsx(paste0("www/MP4/", countrycode(input$selectedCountry, "country.name", "iso3c"), "_summary.xlsx"), sheetIndex = 1)
    #   plotData = data.frame(X = df[,"S"]/df[,"N"], Y = df[,"I"]/df[,"N"])
    #   p = ggplot(plotData, mapping = aes(X, Y, group = 1)) +
    #     geom_line(aes(X, Y), size=lineThickness, color="black") +
    #     labs(title = paste0(input$selectedCountry, " SI Phase Plane (", input$date, ", ", input$timestep, " timesteps)"),
    #          x = "Fraction susceptible", y = "Fraction Infected")
    #   plot(p)
    #   dev.off()
    #   
    #   list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Image not found")
    # }, deleteFile = TRUE)
  })
  
  ##########################################################################    
  # Allow the user to download the time-series plots from UI               #
  ########################################################################## 
  # observeEvent(input$go, {
  #   # TODO: implement downloading of files
  # })
  
  ############################################################################    
  # Generate seed data and have an option to download the file locally       #
  ############################################################################ 
  
  output$seedDataButton <- renderUI({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      downloadButton('downloadData', label = "Generate Seed Data Template", 
                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                     style = "length:800px")
    }
  })
  
  observeEvent(input$selectedCountry, {
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      inputISO <- countrycode(input$selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha
      
      gadmFileName <- paste0("gadm36_", inputISO, "_1_sp.rds")  # name of the .rds file
      
      gadmFolder <- "gadm/" # .rds files should be stored in local gadm/ folder
      
      # if (file.exists(paste0(gadmFolder, gadmFileName)))
      # {
      Level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))
      # }
      # else
      # {
      #   Level1Identifier <- getData("GADM", level = 1, country = inputISOLower)
      # }
      #print(coordinates(Level1Identifier)) # coords of the region
      #print(Level1Identifier$NAME_1) # List of all states/provinces/regions
      
      seedNames <- Level1Identifier$NAME_1
      seedCoords <- coordinates(Level1Identifier)
      #print(seedCoords)
      seedVaxx <- c(0)
      seedExpo <- c(0)
      seedInfect <- c(0)
      seedRec <- c(0)
      seedDead <- c(0)
      seedCombine <- cbind(seedNames, seedCoords, seedVaxx, seedExpo, seedInfect, seedRec, seedDead)
      frameCombine <- data.frame(seedCombine)
      
      frameCombine <- frameCombine[c("seedNames", "V3", "V2", "seedVaxx", "seedExpo", "seedInfect", "seedRec", "seedDead")]
      
      colnames(frameCombine) <- c("Location", "lat", "lon", "InitialVaccinated", "InitialExposed", "InitialInfections", "InitialRecovered", "InitialDead")
      #print(frameCombine)
      
      isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c")
      sheetName <- sprintf("%s_initialSeedData", isoCode)
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(sheetName, Sys.Date(), ".csv",  sep = "")
        },
        content = function(sheetName) {
          write.csv(frameCombine, sheetName, row.names = FALSE)
        }
      )
    }
    
    fileInputs$smStatus <- 'reset'
  })
  
  ############################################################################    
  # Multiple functionality when 'Run Simulation' is pressed                  #
  ############################################################################ 
  observeEvent(input$go, {
    req(iv$is_valid())
    
    isCropped <- FALSE
    
    if(input$cropLev1 == TRUE)
    {
      isCropped <- TRUE
    }
    else
    {
      isCropped <- FALSE
    }
    
    print(paste0(c("isCropped", isCropped)))
    
    rs <- createRasterStack(input$selectedCountry, input$agg, isCropped, level1Names = input$level1List)
    
    # ============= TAB TO SHOW SEED DATA IN TABLE ===========
    data <- reactive({               # read seed data from .csv or .xlsx
      req(iv$is_valid())
      req(input$seedData)
      ext <- tools::file_ext(input$seedData$datapath)
      seedData <- input$seedData
      if(ext == 'xlsx'){
        readxl::read_excel(input$seedData$datapath)
      } else {
        read.csv(input$seedData$datapath)
      }
    })
    
    output$tableSeed <- renderDT({ # print initial seed data to UI
      req(input$seedData)
      if(is.null(data())){return ()}
      data()
    })
    
    output$outputSummary <- renderDT({ # print output summary to UI
      outputSummaryTable <- read_excel(paste0("www/MP4/", countrycode(input$selectedCountry, "country.name", "iso3c"), "_summary.xlsx"))
      datatable(outputSummaryTable,
                options = list(
                  autoWidth = FALSE,
                  scrollX = TRUE))
    })
    
    output$dataPlot <- renderPlot({
      buildPlot()
    })
    
    # # Allow user to download the raster plot
    # output$downloadPlot <- downloadHandler(
    #     filename = function() {
    #         "susceptibleDataPlot.pdf"
    #     },
    #     
    #     content = function(file) {
    #         pdf(file = file, width = 12, height = 12)
    #         print(buildPlot())
    #         dev.off()
    #     }
    # )
    
    # #Allow user to download the simulation summary data, simply save as csv
    # output$downloadData <- downloadHandler(
    #     filename = function() {
    #         "simulationSummary.csv"
    #     },
    # 
    #     content = function(file) {
    #         write.table(x = cDatTable(),
    #                     file = file,
    #                     quote = FALSE, sep = ",", row.names = FALSE)
    #     }
    #  )
    
    # validate(need(!is.null(data()),'No csv uploaded.'))
    # 
    # if(nrow(data())>1)
    # {
    #     return('Your csv has enough rows!')
    # }
    # else
    # {
    #     return('Your csv has not enough rows!')
    # }
    
    #---------------------------------------#
    # Compartmental model simulation begins #
    #---------------------------------------#
    
    #print(data())          # Prints the seed data
    
    #print(names(data()))   # Prints the column names of the seed data
    
    alpha <- ifelse(input$modelSelect == "SVEIRD", input$alpha, 0) # DO NOT DELETE
    beta  <- input$beta  # DO NOT DELETE
    gamma <- input$gamma # DO NOT DELETE
    sigma <- input$sigma # DO NOT DELETE
    delta <- input$delta # ifelse(input$modelSelect == "SEIR", 0, input$delta) # DO NOT DELETE
    
    eps <- 0.0000000000000001
    
    radius <- ifelse(input$lambda <= input$agg, 1, round(((input$lambda - input$agg)/input$agg) + eps) + 1)
    
    isDeterministic <- TRUE
    
    if(input$stochasticSelect == "Deterministic")
    {
      isDeterministic <- TRUE
    }
    else
    {
      isDeterministic <- FALSE
    }
    
    SpatialCompartmentalModelWithDA(model = input$modelSelect, startDate = input$date, selectedCountry = input$selectedCountry, directOutput = FALSE, rasterAgg = input$agg, 
                                    alpha, beta, gamma, sigma, delta, radius = radius, lambda = input$lambda, timestep = input$timestep, seedFile = input$seedData$datapath, seedRadius = 0,
                                    deterministic = isDeterministic, isCropped = input$cropLev1, level1Names = input$level1List, DA = input$dataAssim, sitRepData = input$dataAssimZones$datapath, 
                                    dataI = input$assimIData$datapath, dataD = input$assimDData$datapath, varCovarFunc = input$covarianceSelect, QVar = input$QVar, 
                                    QCorrLength = input$QCorrLength, nbhd = input$nbhd, psiDiag = input$psidiag)
    
    row1  <- data.frame(Variable = "Country", Value = input$selectedCountry)
    row2  <- data.frame(Variable = "WorldPop Raster Dimension", Value = paste0(rs$nRows, " rows x ", rs$nCols, " columns = ", rs$nCells, " grid cells"))
    row3  <- data.frame(Variable = "Aggregation Factor", Value = input$agg)
    row4  <- data.frame(Variable = "Aggregated Raster Dimension", Value = paste0(nrow(rs$rasterStack), " rows x ", ncol(rs$rasterStack), " columns = ", ncell(rs$rasterStack), " grid cells"))
    row5  <- data.frame(Variable = "Compartmental Model", Value = input$modelSelect)
    row6  <- data.frame(Variable = "Model Parameters", Value = paste("Alpha:", alpha,"Beta:", beta,"Gamma:", gamma, "Sigma:", sigma,"Delta:", delta))
    row7  <- data.frame(Variable = "Average Distance Travelled/Day (in km)", Value = input$lambda)
    row8  <- data.frame(Variable = "Radius (1 = Moore neighbourhood)", Value = radius)
    row9  <- data.frame(Variable = "Uploaded Seed Data", Value = input$seedData$name)
    row10 <- data.frame(Variable = "Number of iterations (days)", Value = input$timestep)

    values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10)
    
    output$summaryTable <- renderDT({
      datatable(values$df,
                rownames = FALSE,
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE))
    })

    #########################################    
    # Output seed plot image to the app UI  #
    #########################################
    
    output$seedPlot <- renderImage({
      
      outfile <- tempfile(fileext = '.png')
      
      png(outfile, width = 1024, height = 768)
      createCroppedSeedPlot(selectedCountry = input$selectedCountry, rasterAgg = input$agg, isCropped, level1Names = input$level1List, seedData = input$seedData$datapath, seedNeighbourhood = 0)  # print the seed plot direct to UI
      dev.off()
      
      list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Seed plot image not found")
      # The above line adjusts the dimensions of the base plot rendered in UI
    }, deleteFile = TRUE)
  })
  
  # observeEvent(input$filterLMIC,{
  #   updateCheckboxInput(session, inputId = "cropLev1", value = FALSE) # uncheck the crop box first
  #   if(input$filterLMIC){
  #     population <- population[population$LMIC == 'TRUE',]
  #   } else {
  #     population <- population #[population$LMIC == 'TRUE' || population$LMIC == 'FALSE']
  #   }
  #   updatePickerInput(session, inputId = 'selectedCountry', choices = population$Country, selected = "Nigeria")
  # })
  
  observeEvent(input$modellingApproach, {
    
    updatePickerInput(
      session, 
      inputId = 'selectedCountry', 
      choices = shortlist$Country, 
      selected = "Nigeria")
  })
  
  ################
  # Tabset Panel #
  ################
  
  observeEvent(input$resetAll,{
    shinyjs::hide(id = "tabsetContainer")
    fileInputs$smStatus <- 'reset'
  })
  
  observeEvent(!iv$is_valid(),{
    shinyjs::hide(id = "tabsetContainer")
  })
  
  observeEvent(input$go,{
    shinyjs::show(id = "tabsetContainer")
    updateTabsetPanel(inputId = 'tabSet', selected = 'Input Summary')
  })
  
  # output$downloadOutputSummary <- downloadHandler(
  #   filename = function() {"output.csv"},
  #   content = function(file){
  #     write.csv(data(), file, row.names = FALSE)
  #   }
  #   
  # )
}
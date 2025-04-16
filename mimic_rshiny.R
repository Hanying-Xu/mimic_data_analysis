library(shiny)
library(DBI)
library(bigrquery)
library(ggplot2)
library(dplyr)
library(forcats)
library(lubridate)
library(gtsummary)
library(tidyr)
library(stringr)
library(dbplyr)
library(gt)
library(tidyverse)

file.exists("2025-winter-4e58ec6e5579.json")
satoken <- "2025-winter-4e58ec6e5579.json"
bq_auth(path = satoken)
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "2025-winter",
  dataset = "mimiciv_3_1",
  billing = "2025-winter"
)
con_bq
if (!file.exists("mimiciv_shiny/mimic_icu_cohort.rds")) {
  stop("Error: mimic_icu_cohort.rds not found in mimiciv_shiny folder.")
}


mimic_icu_cohort <- read_rds("mimiciv_shiny/mimic_icu_cohort.rds")
mimic_icu_cohort_shiny <- mimic_icu_cohort %>%
  select(
    race, insurance, language, marital_status, gender, age_at_intime,
    hospital_expire_flag, first_careunit, last_careunit, los, admission_type, 
    admission_location, discharge_location, dod, sodium, chloride, 
    creatinine, potassium, glucose, hematocrit, wbc, bicarbonate, 
    temperature_fahrenheit, non_invasive_blood_pressure_diastolic, 
    respiratory_rate, non_invasive_blood_pressure_systolic, heart_rate)

# UI
ui <- fluidPage(
  titlePanel("MIMIC-IV ICU Cohort Explorer"),
  tabsetPanel(
    
    # Patient Characteristics Tab
    tabPanel("Patient Characteristics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_variable", "Choose a Variable:", 
                             choices = names(mimic_icu_cohort_shiny)),
               ),
               mainPanel(
                 plotOutput("plot_variable_distribution"),
                 tableOutput("table_variable_summary") 
               )
             )
    ),
    
    # Patient's ADT and ICU Stay Tab
    tabPanel("Patient's ADT and ICU Stay Information",
             sidebarLayout(
               sidebarPanel(
                 numericInput("patient_id", "Input Patient ID:", 
                              value = 10063848, min = 1),
                 actionButton("lookup", "Retrieve Data")
               ),
               mainPanel(
                 h3("Patient's ADT and ICU Stay Information"),
                 tableOutput("table_patient_info"),
                 plotOutput("plot_patient_ADT_timeline"),
                 plotOutput("plot_patient_vitals")
               )
             )
    )
  )
)


# Server Logic
server <- function(input, output, session) {
  
  # Patient Summary Plots
  output$plot_variable_distribution <- renderPlot({
    chosen_var <- input$selected_variable
    
    if (chosen_var == "first_careunit") {
      mimic_icu_cohort_shiny[[chosen_var]] <- 
        as.factor(mimic_icu_cohort_shiny[[chosen_var]])
    }
    
    if (is.numeric(mimic_icu_cohort_shiny[[chosen_var]])) {
      ggplot(mimic_icu_cohort_shiny, aes(x = .data[[chosen_var]])) +
        geom_histogram(binwidth = 
                         diff(range(mimic_icu_cohort_shiny[[chosen_var]], 
                                    na.rm = TRUE)) / 40, 
                       fill = "pink", color = "black", alpha = 0.6) +
        labs(title = paste("Distribution of", chosen_var),
             x = chosen_var, y = "Frequency") +
        theme_light(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 14))
    } else {
      ggplot(mimic_icu_cohort_shiny, aes(x = .data[[chosen_var]])) +
        geom_bar(fill = "orange", color = "black", alpha = 0.6) +
        labs(title = paste("Distribution of", chosen_var),
             x = chosen_var, y = "Count") +
        theme_light(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 14)) +
        coord_flip()
    }
  })
  
  # Patient Characteristics Table
  output$table_variable_summary <- renderTable({
    chosen_var <- input$selected_variable
    
    summary_table <- mimic_icu_cohort_shiny %>%
      mutate(across(where(is.character), as.factor)) %>% 
      select(all_of(chosen_var)) %>%
      tbl_summary()
    
  })
  
  patient_info <- eventReactive(input$lookup, {
    subject_id <- input$patient_id
    
    transfers_info <- dbGetQuery(con_bq, paste0(
      "SELECT subject_id, intime, outtime, 
      careunit FROM `biostat-203b-2025-winter.mimiciv_3_1.transfers`
       WHERE subject_id = ", subject_id
    ))
    
    procedures_info <- dbGetQuery(con_bq, paste0(
      "SELECT subject_id, chartdate, 
      icd_code FROM `biostat-203b-2025-winter.mimiciv_3_1.procedures_icd`
       WHERE subject_id = ", subject_id
    ))
    
    vitals_info <- dbGetQuery(con_bq, paste0(
      "SELECT subject_id, stay_id, charttime, 
      itemid, valuenum FROM `biostat-203b-2025-winter.mimiciv_3_1.chartevents`
       WHERE subject_id = ", subject_id, 
      " AND itemid IN (220045, 220179, 220180, 220210, 223761)"
    ))
    
    list(
      transfers = transfers_info,
      procedures = procedures_info,
      vitals = vitals_info
    )
  })
  
  careunit_colors <- c(
    "Emergency Department" = "red",
    "Medicine" = "green",
    "Surgical Intensive Care Unit (SICU)" = "purple",
    "Med/Surg/Trauma" = "cyan",
    "Med/Surg" = "yellow",
    "UNKNOWN" = "blue"
  )
  
  
  
  output$plot_patient_ADT_timeline <- renderPlot({
    req(patient_info())
    
    transfers_info <- patient_info()$transfers
    procedures_info <- patient_info()$procedures
    labevents_info <- patient_info()$labevents
    
    if (is.null(transfers_info) || nrow(transfers_info) == 0) {
      showNotification("No ICU stay data found for this patient.", 
                       type = "warning")
      return(NULL)
    }
    
    transfers_info <- transfers_info %>%
      mutate(
        intime = as.POSIXct(intime),
        outtime = ifelse(is.na(outtime), intime + 3600, outtime),
        outtime = as.POSIXct(outtime),
        event_type = "ADT",
        linewidth = case_when(
          careunit == "Surgical Intensive Care Unit (SICU)" ~ "Wide",
          TRUE ~ "Narrow"
        )
      )
    
    if (!is.null(labevents_info) && nrow(labevents_info) > 0) {
      labevents_info <- labevents_info %>%
        mutate(charttime = as.POSIXct(charttime), event_type = "Lab")
    } else {
      labevents_info <- tibble(charttime = as.POSIXct(character()), 
                               event_type = character()) 
    }
    
    d_icd_procedures <- dbGetQuery(con_bq, "
    SELECT icd_code, 
    long_title FROM `biostat-203b-2025-winter.mimiciv_3_1.d_icd_procedures`
  ")
    
    if (!is.null(procedures_info) && nrow(procedures_info) > 0) {
      procedures_info <- procedures_info %>%
        left_join(d_icd_procedures, by = "icd_code") %>%
        mutate(
          charttime = as.POSIXct(chartdate),
          event_type = "Procedure",
          short_title = trimws(sub(",.*", "", long_title))
        )
    } else {
      procedures_info <- tibble(charttime = as.POSIXct(character()), 
                                event_type = character(), 
                                short_title = character())
    }
    
    unique_procedures <- unique(procedures_info$short_title)
    procedure_shapes <- setNames(17:(16 + length(unique_procedures)), 
                                 unique_procedures)
    
    ggplot() +
      geom_point(
        data = procedures_info,
        aes(x = charttime, y = factor("Procedure", levels = c("ADT", "Lab", 
                                                              "Procedure")), 
            shape = short_title),
        size = 3, color = "black"
      ) +
      
      geom_point(
        data = labevents_info,
        aes(x = charttime, y = factor(event_type, levels = c("Lab", "ADT", 
                                                             "Procedure"))),
        shape = 3, size = 2, color = "black"
      ) +
      
      geom_segment(
        data = transfers_info,
        aes(x = intime, xend = outtime, 
            y = factor("ADT", levels = c("ADT", "Lab", "Procedure")), 
            yend = factor("ADT", levels = c("ADT", "Lab", "Procedure")), 
            color = careunit, linewidth = linewidth)
      ) +
      
      scale_linewidth_manual(values = c("Narrow" = 2.5, "Wide" = 5), 
                             guide = "none") +
      
      scale_shape_manual(values = procedure_shapes, 
                         labels = str_wrap(names(procedure_shapes), 
                                           width = 18)) +
      
      scale_color_manual(values = careunit_colors) +
      
      labs(
        title = paste("Patient", input$patient_id, "ICU Stay Timeline"),
        x = "Calendar Time"
      ) + 
      
      scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
      
      theme_minimal() +
      theme(
        plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 8)
      ) + 
      
      guides(
        shape = guide_legend(title = "Procedure", order = 2),
        color = guide_legend(title = "Care Unit", order = 1)
      )
  })
  
  
  
  output$plot_patient_vitals <- renderPlot({
    req(patient_info())
    vitals_info <- patient_info()$vitals
    
    
    vital_mapping <- tibble(
      itemid = c(220045, 220179, 220180, 220210, 223761),
      vital_name = c("HR", "NBPs", "NBPd", "RR", "Temperature")
    )
    
    vitals_info <- vitals_info %>%
      left_join(vital_mapping, by = "itemid") %>%
      mutate(charttime = as.POSIXct(charttime)) %>%
      filter(!is.na(valuenum)) %>%
      mutate(vital_name = factor(vital_name, levels = c("HR", "NBPd", "NBPs", 
                                                        "RR", "Temperature")))
    
    vital_colors <- setNames(
      c("red", "green", "darkgreen", "skyblue", "purple"), 
      c("HR", "NBPd", "NBPs", "RR", "Temperature")
    )
    
    ggplot(vitals_info, aes(x = charttime, y = valuenum, color = vital_name, 
                            group = interaction(stay_id, vital_name))) +
      geom_line() + 
      geom_point() + 
      
      facet_grid(vital_name ~ stay_id, scales = "free_y") + 
      
      labs(
        title = paste("Patient", input$patient_id, "ICU stays - Vitals"),
        x = NULL,
        y = NULL 
      ) +
      
      scale_x_datetime(
        date_breaks = "12 hours",
        date_labels = "%b %d %H:%M",
        guide = guide_axis(n.dodge = 2)
      ) +
      
      scale_color_manual(values = vital_colors) +
      
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12),
        strip.background.y = element_rect(fill = "gray88", color = NA), 
        strip.background.x = element_rect(fill = "gray88", color = NA), 
        panel.border  = element_rect(color = "gray88", fill = NA, size = 0.3), 
        axis.text.x = element_text(hjust = 0.5), 
        panel.spacing = unit(0.5, "lines"), 
        strip.text.x = element_text(size = 9, color = "white"),
        strip.text.y = element_text(size = 9, color = "white", angle = 270), 
        strip.placement = "outside", 
        legend.position = "none"
      )
  })
}

shinyApp(ui, server)
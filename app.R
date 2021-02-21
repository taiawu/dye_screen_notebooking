# editable tables 
# https://www.r-bloggers.com/2019/04/edit-datatables-in-r-shiny-app/
library(shiny)
library(dplyr)
library(DT)
library(tidyverse)
library(rmarkdown)
library(shiny)

volume_from_layout <- function( .layout,
                                .variable,
                                .final_vol = 10,
                                .fold_conc = 1,
                                .excess_vol = 1.1) {

    .layout %>%
        select(all_of(.variable)) %>%
        pivot_longer(cols = names(.),# take everything
                     names_to = "category",
                     values_to = "component") %>%
        arrange(category, component) %>%

        table() %>%
        as_tibble() %>%
        filter(n != 0) %>%
        set_names(c("category","component", "num_wells")) %>%
        mutate(exact_1X_vol = num_wells*.final_vol,
               exact_conc_vol = exact_1X_vol/.fold_conc) %>%
        mutate(fold_conc = .fold_conc,
               excess_vol = .excess_vol,
               final_conc_vol = exact_conc_vol*excess_vol,
               stock_conc = NaN,
               final_conc = NaN,
               stock_vol = final_conc_vol*(fold_conc*final_conc)/stock_conc,
               buffer_vol = final_conc_vol - stock_vol)

}

update_volume_from_layout <- function( vol_from_layout ) {
    vol_from_layout %>%
        mutate(exact_conc_vol = exact_1X_vol/fold_conc) %>%
        mutate(fold_conc = fold_conc,
               excess_vol = excess_vol,
               final_conc_vol = exact_conc_vol*excess_vol,
               stock_vol = final_conc_vol*(fold_conc*final_conc)/stock_conc,
               buffer_vol = final_conc_vol - stock_vol) %>%

        mutate(across(where(is.numeric), round, 1)) %>%
        mutate_each(funs(replace(., .<0, NA)))



}



demo_layout <- readRDS("demo_layout.rds")
demo_table <- volume_from_layout(demo_layout, .variable =  c("protein", "ligand"))

input_data <- data.frame(Brand = c("Brand1", "Brand2","Brand3"),
                         ratio = c (.5, .5, .5),
                         cost = c(2000, 3000, 4000),
                         stringsAsFactors = FALSE) %>%
    mutate(updated_price = cost * ratio)

modFunction <- function(input, output, session, data, reset) {

    v <- reactiveValues(data = data)

    proxy = dataTableProxy("mod_table")

    observeEvent(input$mod_table_cell_edit, {
        print(names(v$data))
        info = input$mod_table_cell_edit
        str(info)
        i = info$row
        j = info$col
        k = info$value
        str(info)

        isolate(
            {
                v$data[i, j] <<- as.numeric(k)
                v$data <<- update_volume_from_layout(v$data)
            }
        #v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])

        )

        # isolate(
        #     if (j %in% match(c("ratio","cost","updated_price"), names(v$data))) {
        #         print(match(c("ratio","cost", "updated_price"), names(v$data)))
        #         v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
        #         print(v$data)
        #
        #         if (j %in% match("cost", names(v$data))) {
        #             v$data$updated_price <<- v$data$cost * v$data$ratio
        #         }
        #         if (j %in% match("ratio", names(v$data))) {
        #             v$data$updated_price <<- v$data$cost * v$data$ratio
        #         }
        #     } else {
        #         v$data$updated_price <<- v$data$cost * v$data$ratio
        #         #stop("You cannot change this column.") # check to stop the user from editing only few columns
        #     }
        # )
        replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
    })
    ### Reset Table
    observeEvent(reset(), {
        v$data <- data # your default data
    })

    print(isolate(colnames(v$data)))
    output$mod_table <- DT::renderDataTable({
        DT::datatable(v$data, editable = TRUE)

    })
}

modFunctionUI <- function(id) {
    ns <- NS(id)
    DT::dataTableOutput(ns("mod_table"))

}

    ui = basicPage(
        mainPanel(

            actionButton("reset", "Reset"),
            tags$hr(),
            modFunctionUI("editable")
        )
    )
    server = function(input, output) {
        demodata <- demo_table
        callModule(modFunction,"editable", demodata,
                   reset = reactive(input$reset))

    }


# ### scaffolded from: https://shiny.rstudio.com/gallery/download-knitr-reports.html
# ui = fluidPage(
#     title = 'Download a PDF report',
#     sidebarLayout(
#         sidebarPanel(
#             helpText(),
#             selectInput('x', 'Build a regression model of mpg against:',
#                         choices = names(mtcars)[-1]),
#             radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
#                          inline = TRUE),
#             downloadButton('downloadReport')
#         ),
#         mainPanel(
#             plotOutput('regPlot')
#         )
#     )
# )
#     
#  server = function(input, output) {
#         
#         regFormula <- reactive({
#             as.formula(paste('mpg ~', input$x))
#         })
#         
#         output$regPlot <- renderPlot({
#             par(mar = c(4, 4, .1, .1))
#             plot(regFormula(), data = mtcars, pch = 19)
#         })
#         
#         output$downloadReport <- downloadHandler(
#             filename = function() {
#                 paste('my-report', sep = '.', switch(
#                     input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
#                 ))
#             },
#             
#             content = function(file) {
#                 src <- normalizePath('report.Rmd')
#                 
#                 # temporarily switch to the temp dir, in case you do not have write
#                 # permission to the current working directory
#                 owd <- setwd(tempdir())
#                 on.exit(setwd(owd))
#                 file.copy(src, 'report.Rmd', overwrite = TRUE)
#                 
#                 library(rmarkdown)
#                 out <- render('report.Rmd', switch(
#                     input$format,
#                     PDF = pdf_document(), HTML = html_document(), Word = word_document()
#                 ))
#                 file.rename(out, file)
#             }
#         )
#         
#     }


# library(shiny)
# library(data.table)
# library(DT)
# 
#     ui = fluidPage(
#         mainPanel(
#             DT::dataTableOutput("testProxy"),
#             actionButton(inputId = "addrowDF", label = "add row with data.frame"),
#             actionButton(inputId = "addrowDT", label = "add row with data.table")
#         )
#     )
#     server = function(input, output) {
#         
#         mydata = data.table(
#             "col1" = 1:4,
#             "col2" = LETTERS[1:4]
#         )
#         
#         print(mydata)
#         
#         output$testProxy <- DT::renderDataTable(
#             mydata[1:3,], server = FALSE
#         )
#         
#         myproxy = DT::dataTableProxy(outputId = "testProxy")
#         
#         observeEvent(input$addrowDF, {
#             print("addrow")
#             
#             # Good with data.frame
#             myproxy %>% addRow(data.frame(mydata)[1,])
#         })
#         
#         observeEvent(input$addrowDT, {
#             print("addrow")
#             
#             # Not good with data.table
#             myproxy %>% addRow(mydata[1,])
#         })
#     }



# Run the application 
shinyApp(ui = ui, server = server)

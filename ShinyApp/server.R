shinyServer(function(session, input, output) {
        
        reval <- reactive(predictWord(input$text))

        # Fill the buttons with the predicted values
        output$wordOneButton <- renderUI({
                actionButton("action", label = reval()[1], width = '26.5%', style = "font-size: 20px")
                })
                
        observe(toggle("wordOneButton", condition = !is.null(reval()[1])))
        
        output$wordTwoButton <- renderUI({
                actionButton("action2", label = reval()[2], width = '26.5%', style = "font-size: 20px")})

        observe(toggle("wordTwoButton", condition = !is.null(reval()[2])))
        
        output$wordThreeButton <- renderUI({
                actionButton("action3", label = reval()[3], width = '26.5%', style = "font-size: 20px")})
        
        observe(toggle("wordThreeButton", condition = !is.null(reval()[3])))
        
        # Include predicted word in the text after click event on button
        observeEvent(input$action, {
                name <- paste0(input$text, reval()[1], sep = " ")
                updateTextInput(session = session, "text", value = name)
        })
        
        observeEvent(input$action2, {
                name <- paste0(input$text, reval()[2], sep = " ")
                updateTextInput(session = session, "text", value = name)
        })
        
        observeEvent(input$action3, {
                name <- paste0(input$text, reval()[3], sep = " ")
                updateTextInput(session = session, "text", value = name)
        })
        
})
  

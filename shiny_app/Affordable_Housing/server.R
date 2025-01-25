function(input, output, session) {
  
  project <-  reactive({
    project <- get_project_data(input$hudid)
    
  })
  
  model_data <- reactive({
    model_data <-  get_model_data(project())
    
  })
  
  did_logged_tpost <- reactive({
    did_logged_tpost <- lm(glue('log(amount) ~ treatment  + I(sale_year - {project()$YR_PIS}) + I((sale_year - {project()$YR_PIS})^2) + treatment:after + treatment:after:(I(sale_year - {project()$YR_PIS}) + I((sale_year - {project()$YR_PIS})^2)) + square_footage + age + building_condition'), data = model_data())
  })
  
  estimate_df <- reactive({
    estimate_df <- get_estimates(did_logged_tpost(), model_data(), project())
  })
  
  output$distPlot <- renderPlot({
    
    model_data() |> 
      mutate(treatment = factor(treatment)) |> 
      group_by(treatment, sale_year) |> 
      summarize(log_amount = mean(log(amount))) |> 
      ggplot(aes(x = sale_year, y = log_amount, group = treatment, color = treatment)) +
      geom_line() + geom_point() +
      geom_line(data = estimate_df() |> mutate(treatment = factor(treatment)), 
                aes(x = sale_year, y = fit, group = treatment, color = treatment), linetype = 'dashed') +
      geom_vline(xintercept = project()$YR_PIS) + geom_vline(xintercept = project()$YR_ALLOC)
    
  })
  
  output$distPlot2 <- renderPlot({
    
    estimate_df() |> 
      ggplot(aes(x = sale_year, y = fit, group = group, color = group)) +
      geom_line() + geom_point() + 
      geom_ribbon(data = estimate_df() |> filter(group == "treatment", sale_year > project()$YR_PIS), aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") + 
      geom_line(data = estimate_df() |> 
                  filter(group == "control", sale_year >= project()$YR_PIS) |> 
                  mutate(fit = fit + did_logged_tpost()$coefficients['treatment']),
                aes(x = sale_year, y = fit), linetype = 'dashed', color = "black") +
      geom_vline(xintercept = project()$YR_PIS) + geom_vline(xintercept = project()$YR_ALLOC)
    
  })
}

library(shiny)
data(mtcars)

shinyServer(function(input, output) {
    modelFit <- reactive({
        strF <- "mpg ~ am"
        if (input$cyl) strF <- paste(strF, "cyl", sep = " + ")
        if (input$disp) strF <- paste(strF, "disp", sep = " + ")
        if (input$hp) strF <- paste(strF, "hp", sep = " + ")
        if (input$drat) strF <- paste(strF, "drat", sep = " + ")
        if (input$wt) strF <- paste(strF, "wt", sep = " + ")
        if (input$qsec) strF <- paste(strF, "qsec", sep = " + ")
        if (input$vs) strF <- paste(strF, "vs", sep = " + ")
        if (input$gear) strF <- paste(strF, "gear", sep = " + ")
        if (input$carb) strF <- paste(strF, "carb", sep = " + ")
        lm(as.formula(strF), data = mtcars)
    })
    output$FormulaTitle <- renderText({
        if (input$showFormula) {print("Regression formula")}
    })
    output$formula <- renderText({
        if (input$showFormula) {
            tm <- as.character(modelFit()$terms)
            paste(tm[[2]], tm[[1]], tm[[3]])
        }
    })
    output$ResultsTitle <- renderText({
        if (input$showResults) {print("Regression results")}
    })
    output$regressionResults <- renderText({
        if (input$showResults) {
            r.squared <- paste( "R-squared =", round(summary(modelFit())$r.squared, 2))
            adj.r.squared <- paste( "Adj. R-squared =", round(summary(modelFit())$adj.r.squared, 2))
            rse <- paste( "RSE =", round(summary(modelFit())$sigma, 2))
            paste(rse, r.squared, adj.r.squared, sep = ", ")
        }
    })
    output$ShapiroTestTitle <- renderText({
        if (input$showShapiroTest) {print("Shapiro test")}
    })
    
    output$ShapiroTest <- renderText({
        if (input$showShapiroTest) {
            st <- shapiro.test(modelFit()$residuals)
            method <- paste("Method =", st$method)
            W <- paste("W =", round(st$statistic, 2))
            p.value <- paste("p-value =", round(st$p.value, 4))
            paste(method, W, p.value, sep = ", ")
        }
    })
    
    output$ammanualTitle <- renderText({
        if (input$showImpact) {print("Impact of am on mpg")}
    })
    output$ammanual <- renderText({
        if (input$showImpact) round(modelFit()$coefficients["am"], 2)
    })
    
    output$ResidualsPlotTitle <- renderText({
        if (input$showResidualsPlot) {print("Residuals plots")}
    })
    output$ResidualsPlot <- renderPlot({
        if (input$showResidualsPlot) {
            resid <- residuals(modelFit())
            fitted <- fitted.values(modelFit())
            par(mfrow = c(3,2), mar = c(2, 2, 2, 2))
            plot(density(resid), xlab = "Residuals", ylab = "Density", main = "")
            plot(fitted, resid, xlab = "Predicted values", ylab = "Residuals")
            abline(h = 0, col = "red", lty = "dashed")
            plot(modelFit(), cex = 0.8)
        }
    })

})


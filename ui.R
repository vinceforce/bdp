library(shiny)
library(car)
data(mtcars)
outcome <- "mpg"
firstpredictor = "am"
c <- mtcars
c$am <- as.factor(c$am)
levels(c$am) <- c("auto", "manual")
c$cyl <- as.factor(c$cyl)
c$gear <- as.factor(c$gear) 
c$carb <- as.factor(c$carb)
c$vs <- as.factor(c$vs)
mdls <- list()
factors <- c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "gear", "carb")
v <- list()
for (f in factors) {
    mdls[[f]] <- lm(as.formula(paste("mpg ~ am", f, sep = " + ")), data = c)
    v[[f]] <- round(vif(mdls[[f]]), 2)
}
ammdl <- lm(mpg ~ am, data = c)
a <- anova(ammdl, mdls[["cyl"]], mdls[["disp"]], mdls[["hp"]], mdls[["drat"]], mdls[["wt"]],
           mdls[["qsec"]], mdls[["vs"]], mdls[["gear"]], mdls[["carb"]])
rss <- round(a$RSS, 2)
rsslist <- list()
for (i in 2:length(rss)) {
    rsslist[[factors[i-1]]] <- rss[i]
}
lib <- list()
for (f in factors) {
    lib[[f]] <- paste(f, " (vif=", v[[f]], ", rss=", rsslist[[f]], ")", sep = "")
}


shinyUI(navbarPage("Select factors for predicting with LM",
    tabPanel("Application",
        sidebarLayout(
            sidebarPanel(
                checkboxInput("cyl", lib[["cyl"]][1], value = FALSE),
                checkboxInput("disp", lib[["disp"]][1], value = FALSE),
                checkboxInput("hp", lib[["hp"]][1], value = FALSE),
                checkboxInput("drat", lib[["drat"]][1], value = FALSE),
                checkboxInput("wt", lib[["wt"]][1], value = FALSE),
                checkboxInput("qsec", lib[["qsec"]][1], value = FALSE),
                checkboxInput("vs", lib[["vs"]][1], value = FALSE),
                checkboxInput("gear", lib[["gear"]][1], value = FALSE),
                checkboxInput("carb", lib[["carb"]][1], value = FALSE),
                hr(height = 10),
                checkboxInput("showFormula", "Show Formula", value = TRUE),
                checkboxInput("showResults", "Show Results", value = TRUE),
                checkboxInput("showShapiroTest", "Show Shapiro test", value = TRUE),
                checkboxInput("showImpact", "Show impact", value = TRUE),
                checkboxInput("showResidualsPlot", "Show residuals plots", value = TRUE)
            ),
            mainPanel(
                tags$h4(
                    textOutput("FormulaTitle")
                ),
                textOutput("formula"),
                tags$h4(
                    textOutput("ResultsTitle")
                ),
                textOutput("regressionResults"),
                tags$h4(
                    textOutput("ShapiroTestTitle")
                ),
                textOutput("ShapiroTest"),
                tags$h4(
                    textOutput("ammanualTitle")
                ),
                textOutput("ammanual"),
                tags$h4(
                    textOutput("ResidualsPlotTitle")
                ),
                plotOutput("ResidualsPlot", height = "600px")
            )
        )
    ),
    tabPanel("Documentation",
         h3("How to use application"),
         img(src = "LMFactors.png", style = "margin:auto; display:block"),
         p("This tool helps to select factors to include in the formula applied in Linear Regression."),
         p("The dataset used is mtcars."),
         p("The context is the one of the final assigment for the Linear Regression course."),
         p("The purpose is to select the relevant factors to add to am as predictors of mpg as outcome."),
         h3("Selecting factors / options for results"),
         tags$table(
             tags$tr(
                 tags$td(
                     img(src = "LMFactorsleft.png", height = 250, hspace = 15)
                 ),
                 tags$td(
                     tags$p("On the left side, you can select factors to add to the formula, by clicking on the corresponding checkbox."),
                     tags$p("The list gives, for each factor"),
                     tags$ul(
                         tags$li("the name of the factor"),
                         tags$li("the variance importance factor (in a regression with all columns as factors), noted vif"),
                         tags$li("the Residual Sum of Squares of the model with am and the considered factor, noted rss")
                     ),
                     tags$p("You can also choose to display (or not) the results items (formula, regression results, Shapiro test, residuals plots).")
                 )
             )
         ),
         h3("Display results"),
         tags$table(
             tags$tr(
                 tags$td(
                     img(src = "LMFactorsright.png", hspace = 15, height = 300)
                 ),
                 tags$td(
                     tags$p("The right part of the screen is updated, showing:"),
                     tags$ul(
                         tags$li("The resulting formula"),
                         tags$li("The regression results (RSE - Residual Standard Error, R-squared, Adjusted R-squared)"),
                         tags$li("A Shapiro test"),
                         tags$li("The impact of am on mpg (all other factors left unchanged) following this regression"),
                         tags$li("Residuals plots (for checking)"),
                         tags$ul(
                             tags$li("Density curve of the residuals (normality)"),
                             tags$li("Distribution of the predictions (symetry around 0)"),
                             tags$li("Residuals versus fitted (symetri around 0)"),
                             tags$li("Normal Q-Q (normality of residuals)"),
                             tags$li("Scale location (no heteroscedacity)"),
                             tags$li("Residuals versus leverage (no outliers)")
                         )
                     )
                 )
             )
         )
    ),
    tabPanel("Code",
             h3("Github repository"),
             p("Code of the application (ui.R, server.R) is available on the link below"),
             a("https://github.com/vinceforce/bdp", href = "https://github.com/vinceforce/bdp", target = "_blank")
    )
))


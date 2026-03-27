### Excessive switching in OCD and paranoia arises from different deficits in belief updating
### Freeland et al. 2026

## Function to create model diagnostic plots
create_diagnostic_plots <- function(model) 
{
  # Residuals vs Fitted Values
  plot1 <- ggplot(model, aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")
  
  # Normal Q-Q Plot
  plot2 <- ggplot(model, aes(sample = .stdresid)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", 
         y = "Standardized Residuals")
  
  # Scale-Location Plot
  plot3 <- ggplot(model, aes(.fitted, sqrt(abs(.stdresid)))) +
    geom_point() +
    geom_smooth(se = FALSE, method = "loess", color = "red") +
    labs(title = "Scale-Location (Spread-Location) Plot", x = "Fitted Values", 
         y = "√|Standardized Residuals|")
  
  # Residuals vs Leverage Plot
  plot4 <- ggplot(model, aes(.hat, .stdresid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Residuals vs Leverage", x = "Leverage", y = "Standardized Residuals")
  
  # Arrange the plots in a grid
  grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
}

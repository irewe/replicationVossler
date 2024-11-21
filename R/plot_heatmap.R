plot_heatmap <- function(data, main_title = "Test"){
    p <- ggplot(data, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                            midpoint = 0, limit = c(-1, 1), space = "Lab", 
                            name = "Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        labs(title = main_title, x = "", y = "")
    return(p)
}
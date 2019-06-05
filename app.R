## app.R ##
library(shinydashboard)
library(factoextra)
library(here)
library(R.utils)
library(tidyverse)
library(janitor)
library(DT)
library(colorblindr)


# Read in pokemon data ---------------------------------------------------

pokemon_data <- clean_names(read.csv("pokemon.csv"))  %>% 
    select(-percentage_male, -type2) %>%  
    filter(type1 %in% c("ghost", "fairy", "dragon")) %>%  # Let's limit this to a few pokemon
    mutate(type1 = droplevels(type1)) %>% # Few poke have data for these
    drop_na()
pokemon_type <- pokemon_data$type1
pokemon_data <- pokemon_data %>% 
    select(starts_with("against"), hp) %>% 
    scale() %>% as.data.frame()


# Load output from k-means clustering -------------------------------------

get_filenames <- function(folder) {
    files <- list.files(here(glue::glue("{folder}/")))
    map_chr(files, ~word(.x, sep = "\\."))
}

load_clustdata <- function(file) {
    temp <- loadObject(here(glue::glue("kmeans/{file}.Rda")))
    assign(file, temp, envir = .GlobalEnv)
    }

map(get_filenames("kmeans"), ~load_clustdata(.x))

# Custom functinos --------------------------------------------------------

# function to create data for silhouette table
get_summary_data <- function(clust){
    
    k <- clust$nbclust  # number of clusters 
    clust_num <- map_chr(seq(1:k), ~paste("Cluster", .x))  # cluster number
    wss <- round(clust$withinss, 2)  # ss-within
    bss <- round(rep(clust$betweenss, k), 2)  # ss-between
    nobs <- clust$size  # n observations per cluster
    neg_sil <- rep(0, k)  # number of observations with negative sil value (misclassified)
    neg_sil_clust <- clust$silinfo$widths[, c("cluster","sil_width")] %>% 
        filter(sil_width < 0) %>% 
        group_by(cluster) %>% 
        summarize(neg_sil = n())
    neg_sil[neg_sil_clust$cluster] <- neg_sil_clust$neg_sil 
    data.frame(clust_num, nobs, wss, bss, neg_sil)  #bind elements to data frame
}

# function to create sil table
make_summary_table <- function(clust) {
    table <- get_summary_data(clust) %>% 
        datatable(rownames = FALSE, 
                  colnames = c("Cluster", "N", "Within SS", "Between SS", "Neg. Silhouette"),
                  caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: left;',
                      htmltools::em('N = number of observations per cluster; SS = sum of squares; Neg. Silhouette = misclassified observations')))
    
    return(table)
}

# Scatterplot function

scatplot <- function(data){
    pca_data <- prcomp(pokemon_data)
    plot_data <- data.frame(pokemon_type, data$cluster, pca_data$x[, 1:3]) %>% 
        rename(cluster = 2) %>% 
        gather(starts_with("PC"), value = value, key = principal_component)  
    
    # Plot clusters by pokemon type
    facet_labels <- c(dragon = "Dragon", fairy = "Fairy", ghost = "Ghost")
    
    plot_data %>% 
        ggplot(aes(x = principal_component, y = value, color = factor(cluster))) +
        geom_point(position = position_jitter(width = 0.5), alpha = 0.6, size = 5) + 
        coord_flip() +
        scale_color_OkabeIto(name = "cluster") +
        labs(x = "Principal Component \n", y = "") + 
        facet_wrap(~pokemon_type, labeller = labeller(pokemon_type = facet_labels)) +
        theme_minimal(base_size = 17) + 
        theme(panel.grid.minor = element_blank())
}


# Dashboard header --------------------------------------------------------

header <-
    dashboardHeader(title = "K-means Clustering of Pokemon Data",
                    titleWidth = 450)


# Dashboard sidebar -------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Intro to clustering", tabName = "intro", icon = icon("info-circle")),
        menuItem("Cluster Plot", tabName = "clustplot", icon = icon("cookie")),
        menuItem("Silhuoette Plot", tabName = "silplot", icon = icon("chart-area")),
        menuItem("Scatter Plot", tabName = "scatplot", icon = icon("braille")),
        selectInput(inputId = "clusters",
                    label = "Number of centroids:",
                    c("2" = "clust2", # turn this into a function!
                      "3" = "clust3", 
                      "4" = "clust4", 
                      "5" = "clust5",
                      "6" = "clust6"),
                    selected = "clust2")
    ))


# Dashboard body ----------------------------------------------------------

body <- dashboardBody(
    tabItems(
        # Intro tab content
        tabItem(tabName = "intro",
                    box(tags$div(
                        tags$p(tags$strong("Introduction to this project")),
                        tags$p("This dashboard is the final project for an R functional programming class.
                        We use the Kaggle Pokemon dataset available", tags$a(href="https://www.kaggle.com/rounakbanik/pokemon", "here"),  
                        "demonstrate how different visualization of k-means clustering can provide help determine how well 
                        various clustering solutions fit the data."), 
                        tags$p("Clustering algorithms are designed to group data based on their similarity or dissimilarity (e.g. distance in 
                        euclidian space). K-means clustering is an unsupervised learning approach to grouping observations in a data set
                        based on the compactness of the observations. It is best suited for data in which there is a priori reason to select
                        a given number of clusters, though it can also be useful as a way to explore a data set visually."), 
                        tags$p("Visualizations in this project include a cluster plot, silhouette plot, and scatterplot, 
                        as well as a table that provides information about cluster size (number of observations), the number of observations
                        that may be incorrectly included in a cluster (negative silhouette,  cluster density (within cluster sum of squares) 
                        and cluster separation (between cluster sum of squares)."),  
                        tags$p(tags$strong("For additional information about unsupervised clustering algorithms, see the following resources:"))),
                        
                        tags$ol(
                            tags$li("- Cichosz, P. (2015). Data mining algorithms: explained using R. Chichester, West Sussex, UK.; Malden, MA, USA: John Wiley & Sons Inc."),  
                            tags$li("- Ding, C., & He, X. (2004). K-means Clustering via Principal Component Analysis. In Twenty-first international conference on Machine learning  - ICML ’04 (p. 29). Banff, Alberta, Canada: ACM Press. https://doi.org/10.1145/1015330.1015408"),
                            tags$li("- Karatzoglou, A., Smola, A., Hornik, K., & Zeileis, A. (2004). kernlab-an S4 package for kernel methods in R. Journal of statistical software, 11(9), 1-20."),
                            tags$li("- Nerurkar, P., Shirke, A., Chandane, M., & Bhirud, S. (2018). Empirical Analysis of Data Clustering Algorithms. Procedia Computer Science, 125, 770–779. https://doi.org/10.1016/j.procs.2017.12.099"),
                            tags$li("- Andrew Y. Ng, Michael I. Jordan, Yair Weiss On Spectral Clustering: Analysis and an Algorithm Neural Information Processing Symposium 2001 http://papers.nips.cc/paper/2092-on-spectral-clustering-analysis-and-an-algorithm.pdf"),
                            tags$li("- von Luxburg, U. (2007). A Tutorial on Spectral Clustering. Statistics and Computing, 17(4), 395–416. https://doi.org/10.1007/s11222-007-9033-z"),
                            tags$li("- Zha, H., Ding, C., Gu, M., He, X., & Simon, H. (2002). Spectral relaxation for K-means clustering. Advances in Neural Information Processing Systems 14 (NIPS’01), 1057–1064."), 
                
                            tags$li(tags$a(href="https://www.datacamp.com/community/tutorials/k-means-clustering-r", "Datacamp tutorial: Describes basics of clustering & provides a tutorial of k-means clustering, including interpretation of output in R.")),
                            tags$li(tags$a(href="https://stats.stackexchange.com/questions/183236/what-is-the-relation-between-k-means-clustering-and-pca", "A discussion of the relationship betweek K-means and PCA")),
                            tags$li(tags$a(href="https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/clustering-algorithms-evaluation-r/tutorial/", "Practical Guide to Clustering Algorithms & Evaluation in R")),
                            tags$li(tags$a(href="https://www.datascience.com/blog/k-means-clustering", "Introduction to K-means Clustering")),
                            tags$li(tags$a(href="https://stats.stackexchange.com/questions/133656/how-to-understand-the-drawbacks-of-k-means", "Drawbacks of K mean clustering")),
                            tags$li(tags$a(href="https://www.datascience.com/blog/k-means-alternatives", "Alternatives for segmenting noisy data"))),
                        width = 12)),
        
        # clustplot tab content
        tabItem(tabName = "clustplot",
                fluidRow(
                    box(plotOutput("clustplot"), width = 6),
                    box(DTOutput("summarytable1"), width = 6)),
                fluidRow(
                    box("This plot shows the cluster results on the first two principal components of the data that were used to create them. 
                        We typically want to see results in which observations group together closely within the cluster (a small within cluster sum of squares)
                        and in which the different clusers do not overlap (a larger between clusters sum of squares).  

                        The table displays information about cluster size (number of observations), the number of observations
                        that may be incorrectly included in a cluster (negative silhouette,  cluster density (within cluster sum of squares) 
                        and cluster separation (between cluster sum of squares).", width = 12))
                ),

                
        # silplot tab content
        tabItem(tabName = "silplot",
                fluidRow(
                    box(plotOutput("silplot"), width = 6),
                    box(DTOutput("summarytable2"), width = 6)),
                fluidRow(
                    box("A silhouette plot shows cluster distance, a combination of within cluster compactness and of between cluster separation. 
                        A silhouette coefficient closer to 1 means that the data are well classified, whereas a coefficient near 0 
                        means observations are between clusters. A negative silhouette coefficient means observations are likely misclassified 
                        and that the data do not group well with any of the identified clusters. The height of each cluster in this plot 
                        represents the number of observations per cluster. Generally, we want clusters to be of roughly the same size, which we can gain 
                        information about by examining the silhouette plot.", width = 12))
        ),
        
        # scatplot tab content
        tabItem(tabName = "scatplot",
                fluidRow(
                    box(plotOutput("scatplot"), width = 12)),
                fluidRow(
                    box(width = 12, "K-means clustering is a form of unsupervised learning, meaning that it is intended to find grouping structure in unlabeled data.
                        However, we know that the pokemon in this dataset already 'grouped' by type of pokemon. 
                        So, we might want to ask how well the clusters we have identified in the data set map onto this pre-existing grouping variable, pokemon type.
                        Here we show a scatterplot that is faceted by 3 popular types of pokemon: dragon, fairy, and ghost. Rather than showing the raw data from the original variables that were fed into the 
                        k-means clustering algorithm, we used principal components analysis (PCA) to reduce the data for simplicity of visualization. Here we show the 
                        first 3 principal components. You will notice that with a 3-cluster solution, the clusters perfectly map onto the 3 types of pokemon. This is apparent from the fact that 
                        each pokemon type only contains a single color. However, with alternative clustering solutions, we see a mix of colors within each pokemon type, meaning that 
                        cluster membership is not completely corresponding to pokemon type. In general, greater mixing of colors across pokemon types corresponds to a weaker relationship between 
                        the identified clusters in the data and pokemon type.")))
        )
    )



# User interface ----------------------------------------------------------

ui <- dashboardPage(header, sidebar, body)


# Server ------------------------------------------------------------------

server <- function(input, output) {
    
    # Silhuoette plot
    output$silplot <-
        renderPlot({
            data <- get(input$clusters)
            data <- data$silinfo$widths

            ggplot(data, aes(x = seq_along(cluster), y = sil_width, fill = cluster, color = cluster)) +
                geom_col() +
                coord_flip() +
                geom_hline(yintercept = mean(data$sil_width, na.rm = TRUE), linetype = 2, size = .7) +
                scale_fill_OkabeIto() +
                scale_color_OkabeIto() +
                theme_minimal(base_size = 17) + 
                labs(title = paste0("Average Silhouette Width = ", round(mean(data$sil_width, na.rm = TRUE), 2)),
                     x = NULL,
                     y = "Silhouette width") 
        })
    
    
    # Cluster plot
    output$clustplot <-
        renderPlot({
            data <- get(input$clusters)
            clust_plot <- data$clust_plot
            clust_plot + 
                theme_minimal(base_size = 17) + 
                theme(panel.grid.minor = element_blank()) + 
                labs(title = "")
        })
    
    # Scatterplot
    output$scatplot <- 
        renderPlot({
            data <- get(input$clusters)
            scatplot(data)
})
    
    output$summarytable1 <- 
        renderDT({
            data <- get(input$clusters)
            make_summary_table(data)
        })
    
    output$summarytable2 <- 
        renderDT({
            data <- get(input$clusters)
            make_summary_table(data)
        })
}

shinyApp(ui, server)
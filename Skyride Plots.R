library(magick)
library(patchwork)
library(cowplot)

# Define the PDF paths and titles
pdf_paths_with_titles <- list(
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Main/analysis2.ww.skyrideplot.pdf", title = "Wastewater Sequences (n = 81)"),
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Main/analysis2.clinical.skyrideplot.pdf", title = "Clinical Sequences (n = 633)"),
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Main/analysis1.skyrideplot.pdf", title = "Combined Sequences (n = 713"),
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Main/analysis3.subsample1.skyrideplot.pdf", title = "DTA - Replicate 1 (n = 632)"),
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Main/analysis3.subsample2.skyrideplot.pdf", title = "DTA - Replicate 2 (n = 632)"),
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Main/analysis3.subsample3.skyrideplot.pdf", title = "DTA - Replicate 3 (n = 632)"),
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Main/analysis3.subsample4.skyrideplot.pdf", title = "DTA - Replicate 4 (n = 632)"),
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Main/analysis3.subsample5.skyrideplot.pdf", title = "DTA - Replicate 5 (n = 632)")
)

# Initialize an empty list for storing the plot objects
all_plots <- list()

# Loop through each PDF, add titles, and create plots
for (item in pdf_paths_with_titles) {
  # Read the PDF file into images (one per page)
  images <- image_read_pdf(item$path)
  
  # Loop through each image (page of the PDF)
  for (i in seq_along(images)) {
    img <- images[[i]]  # Get the current image
    
    # Convert the image to ggplot format using patchwork and magick (no grid or raster!)
    plot <- ggdraw() + draw_image(img) + 
      ggtitle(item$title) +  # Add the title
      theme_void() +  # Remove gridlines and axes
      theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5))  # Title formatting
    
    # Add the plot to the list
    all_plots <- append(all_plots, list(plot))
  }
}

# Combine the plots vertically into one figure using patchwork
# combined_plot <- wrap_plots(all_plots, ncol = 1)

# Create the first plot with plots [1:3]
plot1 <- wrap_plots(all_plots[1:3], ncol = 1)

# Create the second plot with plots [4:8]
plot2 <- wrap_plots(all_plots[4:8], ncol = 1)

# Combine the two plots horizontally (side-by-side)
combined_plot <- wrap_plots(plot1, plot2, ncol = 2, heights = c(1,1), widths = c(1, 1), nrow = 1) 

# Save the final combined plot with titles as a PDF
ggsave("C:/Users/gev25289/Desktop/wastewater/Figures/combined_all_plots_with_titles.pdf", combined_plot, width = 5, height = 6, limitsize = FALSE)

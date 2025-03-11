library(magick)
library(patchwork)
library(cowplot)
library(ggplot2)

# Define the PDF paths and titles
pdf_paths_with_titles <- list(
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Analysis 2 Clinical Root to Tip.pdf", title = "Clinical Sequences (n = 633)"),
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Analysis 2 WW Root to Tip.pdf", title = "Wastewater Sequences (n = 81)"),
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Analysis 1 Root to Tip.pdf", title = "Combined Sequences (n = 713)"),
  list(path = "C:/Users/gev25289/Desktop/wastewater/Figures/Analysis 3 Subsample 1 Root to Tip.pdf", title = "DTA - Replicate 1 (n = 632)")
)

# Initialize an empty list for storing the plot objects
all_plots <- list()

# Loop through each PDF, add titles, and create plots
for (item in pdf_paths_with_titles) {
  # Read the PDF file into images (one per page) & trim whitespace
  images <- image_read_pdf(item$path) |> image_trim()
  
  # Loop through each image (page of the PDF)
  for (i in seq_along(images)) {
    img <- images[[i]]  # Get the current image
    
    # Convert the image to ggplot format using patchwork and magick
    plot <- ggdraw() + 
      draw_image(img) + 
      ggtitle(item$title) +  # Add the title
      theme_void() +  # Remove gridlines and axes
      theme(plot.title = element_text(size = 8, face = "bold", hjust = 0.5, vjust = -0.3))  # Increase title size
    
    # Add the plot to the list
    all_plots <- append(all_plots, list(plot))
  }
}

# Combine plots into a multi-panel layout
final_plot <- wrap_plots(all_plots, ncol = 2)  # Adjust columns if needed

# Save the final combined plot as a high-quality PDF
ggsave("C:/Users/gev25289/Desktop/wastewater/Figures/tempest_root_to_tip.pdf", 
       final_plot, width = 8, height = 6, dpi = 300, limitsize = FALSE)

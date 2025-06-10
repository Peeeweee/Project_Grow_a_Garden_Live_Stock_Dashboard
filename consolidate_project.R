# --- Script to Consolidate Project Files for LLM Context ---
#
# Description:
# This script scans an R project directory, gathers all relevant source code and
# text files, and consolidates them into a single text file.
# It intelligently ignores temporary files, version control directories,
# large data files, and other "noise," making it ideal for providing
# context to a Large Language Model (LLM).
#
# How to Use:
# 1. Place this script in the root directory of your R project.
# 2. Customize the 'Configuration' section below, especially the
#    'ignore_patterns' to exclude any project-specific files or folders.
# 3. Run the script from your R console using: source("consolidate_project.R")
# 4. A file named "project_snapshot.txt" (or your custom name) will be created.
# 5. Copy the entire content of that file and paste it into your LLM prompt.
#
# --------------------------------------------------------------------------

# 1. Configuration
# --------------------------------------------------------------------------
# Output file name (will be created in the project root)
output_filename <- "project_snapshot.txt"

# Files/directories/extensions to ignore (uses regular expressions)
# Add patterns for things you want to exclude.
# Common examples:
# - .git/ : Git repository internal files
# - .Rproj.user/ : RStudio temporary files
# - .Rhistory : R command history
# - .RData : R workspace data
# - .rds / .rda : Saved R objects (often large/binary)
# - renv/ : renv library cache
# - rsconnect/ : Shinyapps.io deployment files
# - Images/Binary files: .png, .jpg, .jpeg, .gif, .ico, .pdf, .xlsx, .csv (if large/data)
# - The output file itself!
ignore_patterns <- c(
  "\\.Rproj$",              # RStudio project file itself (usually not needed for code context)
  "\\.Rproj\\.user/",       # RStudio temporary directory
  "\\.Rhistory$",
  "\\.RData$",
  "\\.Ruserdata/",
  "renv/",                 # renv directory
  "rsconnect/",            # Deployment files
  "\\.git/",               # Git directory
  "packrat/",              # Packrat directory
  output_filename,         # Ignore the output file itself!
  "\\.png$", "\\.jpg$", "\\.jpeg$", "\\.gif$", "\\.ico$", # Common image formats
  "\\.pdf$", "\\.xlsx$", "\\.zip$", # Other binary/document formats
  "\\.rds$", "\\.rda$",         # Saved R objects (can be large/binary)
  "\\.csv$",               # Often data, not code. Remove this if you want to include CSVs.
  # Add any other specific files or directories you want to ignore:
  # "my_secret_file.R",
  # "data_large/",
  "~$",                    # Temporary Word/Excel files
  "\\.DS_Store$"           # MacOS specific
)

# Combine ignore patterns into a single regex pattern
ignore_regex <- paste(ignore_patterns, collapse = "|")

# Set max file size (in bytes) to include (e.g., 1MB = 1 * 1024 * 1024)
# Set to Inf to include all sizes (be careful with large files!)
max_file_size_bytes <- 1 * 1024 * 1024 # 1 MB limit

# --------------------------------------------------------------------------
# 2. Setup - Load Required Packages
# --------------------------------------------------------------------------
if (!requireNamespace("fs", quietly = TRUE)) {
  install.packages("fs")
}
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(fs)
library(here)

# --------------------------------------------------------------------------
# 3. Define Project Root and Output Path
# --------------------------------------------------------------------------
project_dir <- here::here()
output_file_path <- file.path(project_dir, output_filename)

cat("Project Directory:", project_dir, "\n")
cat("Output File:", output_file_path, "\n")

# --------------------------------------------------------------------------
# 4. Generate Directory Tree (Filtered)
# --------------------------------------------------------------------------
cat("Generating filtered directory tree...\n")

tree_lines <- tryCatch({
  # Use fs::dir_tree to get a visual representation of the project structure.
  # The 'all = FALSE' argument conveniently hides most dotfiles.
  # We will manually filter the final list of files later with our more
  # comprehensive 'ignore_regex', so this tree is primarily for visual aid.
  capture.output(
    fs::dir_tree(
      path = project_dir,
      recurse = TRUE,
      all = FALSE # Hides dotfiles like .git, .Rproj.user, etc. by default
    )
  )
}, error = function(e) {
  paste("Error generating directory tree:", e$message)
})

# Add a header explaining the tree view
tree_output <- c(
  "PROJECT DIRECTORY TREE (Filtered View)",
  "(Common config/temporary files like .Rproj.user, .git, .Rhistory are hidden for clarity)",
  "======================================================================",
  tree_lines,
  "\n\n" # Add some spacing before file contents
)

# --------------------------------------------------------------------------
# 5. List, Filter, and Read Files
# --------------------------------------------------------------------------
cat("Listing all files recursively...\n")
# Get all entities, respecting .gitignore if it exists, but also including other dotfiles for our manual filter
all_files <- fs::dir_ls(project_dir, recurse = TRUE, all = TRUE, type = "any")

cat("Filtering files based on ignore patterns and size...\n")

# Filter files based on the comprehensive ignore_regex
files_to_include <- grep(ignore_regex, all_files, value = TRUE, invert = TRUE)

# Further filter: Ensure they are actually files (not directories) and check size
final_file_list <- character()
for (f in files_to_include) {
  # Need to handle potential errors if file disappears between list and info
  file_info <- tryCatch(fs::file_info(f), error = function(e) NULL)
  if (!is.null(file_info) && file_info$type == "file") {
    if (is.na(file_info$size) || file_info$size <= max_file_size_bytes) {
      final_file_list <- c(final_file_list, f)
    } else {
      cat("  Skipping (too large):", fs::path_rel(f, project_dir), "(Size:", format(file_info$size, units="auto"), ")\n")
    }
  }
}

cat("Found", length(final_file_list), "files to include.\n")

# Prepare list to hold file contents
file_contents_list <- list()

cat("Reading and formatting file contents...\n")
for (file_path in final_file_list) {
  # Make path relative to project root for cleaner output markers
  relative_path <- fs::path_rel(file_path, start = project_dir)
  cat("  Processing:", as.character(relative_path), "\n")
  
  tryCatch({
    # Read lines, suppressing warning for files not ending with newline
    # Specify UTF-8 for broad compatibility, but allow fallback
    file_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
    
    # Create start/end markers
    start_marker <- paste0("--- START FILE: ", relative_path, " ---")
    end_marker <- paste0("--- END FILE: ", relative_path, " ---")
    
    # Add to list
    file_contents_list[[file_path]] <- c(start_marker, file_lines, end_marker, "") # Add blank line for spacing
    
  }, error = function(e) {
    # If UTF-8 fails, try reading with the default system encoding
    tryCatch({
      cat("  Warning: UTF-8 read failed for", as.character(relative_path), ". Retrying with default encoding...\n")
      file_lines <- readLines(file_path, warn = FALSE)
      start_marker <- paste0("--- START FILE: ", relative_path, " ---")
      end_marker <- paste0("--- END FILE: ", relative_path, " ---")
      file_contents_list[[file_path]] <- c(start_marker, file_lines, end_marker, "")
    }, error = function(e2) {
      cat("  ERROR: Could not read file:", as.character(relative_path), "- Final Error:", e2$message, "\n")
      # Add a note about the error in the output
      error_marker <- paste0("--- ERROR READING FILE: ", relative_path, " ---")
      error_details <- paste0("--- Error message: ", e2$message, " ---")
      file_contents_list[[file_path]] <- c(error_marker, error_details, "")
    })
  })
}

# --------------------------------------------------------------------------
# 6. Combine and Write Output
# --------------------------------------------------------------------------
cat("Combining tree and file contents...\n")

# Flatten the list of file contents
all_formatted_content <- unlist(file_contents_list, use.names = FALSE)

# Combine tree and content
final_output <- c(tree_output, all_formatted_content)

cat("Writing output to:", output_file_path, "\n")
tryCatch({
  # Write the file using a connection to specify UTF-8 encoding for the output
  con <- file(output_file_path, open = "w", encoding = "UTF-8")
  writeLines(final_output, con, useBytes = FALSE)
  close(con)
  
  cat("----------------------------------------------------------------------\n")
  cat("Success! Project snapshot written to:", output_filename, "\n")
  cat("You can now copy the contents of this file.\n")
  cat("----------------------------------------------------------------------\n")
}, error = function(e) {
  cat("----------------------------------------------------------------------\n")
  cat("Error writing output file:", e$message, "\n")
  cat("----------------------------------------------------------------------\n")
})
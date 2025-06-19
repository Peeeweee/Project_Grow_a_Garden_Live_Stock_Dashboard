# Grow A Garden - Live Stock Dashboard

**By Kent Paulo R. Delgado**


This project is a comprehensive R Shiny web application designed for players of the Roblox game "Grow a Garden". It provides a real-time, interactive dashboard to monitor in-game data, helping players make informed decisions about buying, selling, and trading.

The dashboard pulls live information from various APIs and community-run websites to display current shop inventories, restock timers, and active weather events. It also features powerful tools like a fruit value calculator and a detailed in-game encyclopedia.

---

## 🛠️ Technologies Used

*   **Programming Language:** R
*   **Core Frameworks:**
    *   `Shiny` & `shinydashboard` for the interactive web application structure.
*   **Key R Packages:**
    *   `httr` & `jsonlite`: For interacting with live game APIs.
    *   `rvest` & `xml2`: For web scraping data from community wikis.
    *   `dplyr`, `purrr`, `stringr`: For efficient and modern data manipulation.
    *   `DT`: For creating beautiful, interactive data tables.
    *   `shinyjs`: For enhancing user experience with JavaScript actions without writing JS.
    *   `lubridate`: For handling date-time data.
*   **Front-End:**
    *   HTML & CSS for custom styling and a unique visual theme.

---

## ✨ Features

The application is divided into four main modules:

### 1. Live Dashboard
The central hub for real-time game information.
*   **Real-time Stock Tracking:** Displays the current items and quantities available in all major in-game shops (Seeds, Gear, Eggs, Honey, Cosmetics, Night).
*   **Dynamic Countdown Timers:** Shows live countdowns for when each shop is due to restock.
*   **Smart Auto-Refresh:** The app automatically refreshes data when timers hit zero and at strategic mid-cycle points to catch early stock changes.
*   **Interactive UI:** Click on a timer to automatically scroll down to its corresponding stock section.
*   **Weather Monitoring:** Reports the current in-game weather, which affects crop growth, with custom icons and colors for each event.

### 2. Stock History Viewer
A tool to look back at past shop inventories.
*   **Data Logging:** Logs every fetched stock update to a local CSV file for historical analysis.
*   **Interactive Viewer:** Allows users to select any previously recorded timestamp from a dropdown and see a complete snapshot of the shops at that moment.

### 3. Fruit Value Calculator
A powerful tool to calculate the final sell price of any crop.
*   **Dynamic Calculation:** Users can select a base crop and apply various in-game mutations (Growth, Temperature, Environmental) to see the final value based on the official formula.
*   **Live Data Fetching:** Includes a "Fetch Latest" button to update crop base values from community sources.
*   **Transparent Formula:** Shows the breakdown of the calculation for clarity.

### 4. Comprehensive Encyclopedia
A one-stop reference for all in-game items, scraped from community wikis.
*   **Categorized Information:** Organized into searchable and sortable tabs for:
    *   Seeds & Crops
    *   Gear & Tools
    *   Eggs & Hatchable Pets
    *   Weather & Mutations
*   **Robust & Resilient:** Features a fallback system that uses a cached HTML file to display data if the primary live API is down, ensuring the app remains functional.

---

## 📸 Screenshots

Here are some previews of the dashboard in action.

**Live Dashboard View:**
*(Shows the main page with timers, weather, and current stock.)*


**Fruit Calculator in Action:**
*(Shows a crop and mutations selected with the final value calculated.)*


**Stock History Page:**
*(Shows a historical timestamp selected and the corresponding stock.)*


**In-Game Encyclopedia:**
*(Shows one of the detailed tables, like the Gear or Pets section.)*


*(To add your own screenshots, replace the `https://i.imgur.com/your-screenshot-name.png` links with your image URLs.)*

---

## 🚀 Setup and Installation

To run this project on your local machine, follow these steps:

1.  **Clone the Repository:**
    ```bash
    git clone https://github.com/your-username/Project_Grow_a_Garden_Live_Stock_Dashboard.git
    cd Project_Grow_a_Garden_Live_Stock_Dashboard
    ```
    *(Replace the URL with your actual repository URL)*

2.  **Install R and RStudio:**
    *   Ensure you have a recent version of [R](https://cran.r-project.org/) and [RStudio Desktop](https://posit.co/download/rstudio-desktop/) installed.

3.  **Install Required Packages:**
    *   Open `app.R` in RStudio. RStudio may automatically prompt you to install the missing packages.
    *   Alternatively, run the following command in the R console to install all necessary packages at once:
    ```R
    install.packages(c("shiny", "shinydashboard", "httr", "jsonlite", "dplyr", "stringr", "lubridate", "shinyjs", "DT", "purrr", "rvest", "xml2"))
    ```

4.  **Run the Application:**
    *   Open the `app.R` file in RStudio.
    *   Click the **"Run App"** button that appears at the top of the script editor, or use the keyboard shortcut `Ctrl+Shift+Enter`.
    *   Alternatively, you can run `shiny::runApp()` from the console within the project's root directory.

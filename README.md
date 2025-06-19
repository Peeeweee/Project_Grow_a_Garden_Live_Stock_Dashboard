# Grow A Garden - Live Stock Dashboard

**By Kent Paulo R. Delgado**
![image](https://github.com/user-attachments/assets/b90c2680-1748-4191-a403-e47b2e6af19a)

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
![image](https://github.com/user-attachments/assets/31bab383-7e39-44fa-adf8-d9d80feaf7cd)
![image](https://github.com/user-attachments/assets/db436778-92f0-4fe2-8dde-b1c44de96c48)


**Fruit Calculator in Action:**
![image](https://github.com/user-attachments/assets/03c700f4-de33-41aa-b3f6-46e9e5365563)
![image](https://github.com/user-attachments/assets/0fa32580-2f1b-4dba-bffe-485e19df982a)


**Stock History Page:**
![image](https://github.com/user-attachments/assets/b7b94251-b324-42ab-bc76-999714efc6a0)


**In-Game Encyclopedia:**
![image](https://github.com/user-attachments/assets/cd4fc1df-69f9-426d-bd03-f49fa4c976d2)
![image](https://github.com/user-attachments/assets/c0be8dc8-f311-4828-bcde-a0dc3af30198)
![image](https://github.com/user-attachments/assets/c3f7e385-e0eb-4737-84a5-db41d4f11f08)
![image](https://github.com/user-attachments/assets/080b1706-f73f-4c91-95ab-b745ee44ecda)
![image](https://github.com/user-attachments/assets/0b64409b-1770-4635-aa0d-0d75a973234a)


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

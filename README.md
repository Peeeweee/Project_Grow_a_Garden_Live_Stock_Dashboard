# Grow a Garden: Live Stock & Event Dashboard

[![Built with R | Shiny](https://img.shields.io/badge/Built%20with-R%20%7C%20Shiny-blue?logo=r)](https://shiny.posit.co/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Status](https://img.shields.io/badge/status-live-brightgreen)](https://delgadoearlpaulo.shinyapps.io/project_grow_a_garden_live_stock_dashboard/)

This project is a real-time, interactive web dashboard for the game "Grow a Garden". Built entirely in **R** and the **Shiny** framework, this application pulls live data to help players track valuable in-game assets, monitor time-sensitive events, and make informed decisions without needing to be actively in the game.

### **[View the Live Dashboard Here!](https://delgadoearlpaulo.shinyapps.io/project_grow_a_garden_live_stock_dashboard/)**

---

## ✨ Key Features

-   **Live Dashboard:** A central hub that displays real-time information on item stocks, weather, and crucial event timers.
-   **Real-Time Item Stock Tracking:** Dynamic panels show the current count of Seeds, Gear, Eggs, and Cosmetics available in the game's public stock.
-   **Event Timers:** Keep track of critical in-game cycles, including Seed, Gear, Cosmetics, and Night stock refresh timers.
-   **Stock History:** A dedicated tab to view historical data on item stock changes (powered by `history_module.R`).
-   **Fruit Calculator:** An in-game utility to perform calculations related to fruit values (powered by `calculator_module.R`).
-   **In-Game Encyclopedia:** A reference guide for in-game items and information (powered by `encyclopedia_module.R`).
-   **Manual Refresh:** A control panel allows users to manually refresh the data at any time.
-   **Mobile-Friendly Design:** The UI is designed to be flexible and usable on both desktop and mobile devices.

## 📸 Screenshot

The main dashboard, providing an at-a-glance overview of all critical in-game timers and item stocks.
![image](https://github.com/user-attachments/assets/65967d03-5607-4861-9809-45bd60a448ea)
![image](https://github.com/user-attachments/assets/fcd6d2b2-fd44-4cb1-bee5-d269dc1211e3)
![image](https://github.com/user-attachments/assets/e2fad68f-a97f-477d-b2d8-f8e359322032)
![image](https://github.com/user-attachments/assets/430d5296-79c5-441a-beae-301a3916948b)
![image](https://github.com/user-attachments/assets/16b4001f-00a4-4bbc-86c3-a931bdf7a84f)
![image](https://github.com/user-attachments/assets/ffb911f6-b3a0-4f27-914a-af4a828167b9)
![image](https://github.com/user-attachments/assets/998c06af-636a-44c2-bc6f-ca0369ba7423)
![image](https://github.com/user-attachments/assets/f9e4e39c-2849-4405-bf69-a8fbdda12d97)
![image](https://github.com/user-attachments/assets/8ef45a86-542c-49af-a10c-de4175a8b00c)
![image](https://github.com/user-attachments/assets/f4b725aa-cf43-4a45-88fb-1e0551c4b3c1)


## 🛠️ Tech Stack

This application is powered by the R ecosystem, demonstrating a modern, modularized approach to Shiny development.

-   **Core Framework:** **R** & **Shiny**
-   **UI / Layout:** **shinydashboard** combined with custom HTML/CSS located in the `www/` directory for unique styling and mobile responsiveness.
-   **Data Handling:**
    -   **API Interaction:** `{httr}` or `{httr2}` for fetching live game data.
    -   **Data Caching:** Caches API data to an `.rds` file (`api_data_cache.rds`) to minimize requests and improve performance.
    -   **Data Manipulation:** `{dplyr}`, `{readr}` for cleaning and processing data from both the API and static `.csv` files.
    -   **Time/Date:** `{lubridate}` for handling timers and timestamps.
-   **Modularity:** The application is broken into **Shiny Modules** (`calculator_module.R`, `encyclopedia_module.R`, `history_module.R`) for better organization, scalability, and code management.
-   **Deployment:** Hosted on **shinyapps.io**.


## 🚀 Getting Started

To get a local copy up and running, follow these simple steps.

### Prerequisites

Make sure you have a recent version of **R** and **RStudio Desktop** installed.
*   [Download R](https://cran.r-project.org/)
*   [Download RStudio Desktop](https://posit.co/download/rstudio-desktop/)

### Installation

1.  Clone the repository:
    ```sh
    git clone https://github.com/Peeweeee/Project_Grow_a_Garden_Live_Stock_Dashboard.git
    ```
2.  Open the project in RStudio by clicking on the `.Rproj` file.
3.  Install the required R packages. Open the R console in RStudio and run the following command. **(Please add any other packages you used to this list)**
    ```r
    install.packages(c(
      "shiny", "shinydashboard", "httr", "dplyr", 
      "readr", "lubridate", "purrr"
      # See the modules for adding any other required packages here
    ))
    ```
4.  Run the application by opening the `app.R` file and clicking the **"Run App"** button in RStudio, or by running `shiny::runApp()` in the console.

Developed by: Kent Paulo Delgado
Project Link: [https://github.com/Peeweeee/Project_Grow_a_Garden_Live_Stock_Dashboard](https://github.com/Peeweeee/Project_Grow_a_Garden_Live_Stock_Dashboard)

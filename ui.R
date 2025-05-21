library(shiny)
library(shinyjs)       # Für JS-basierte Steuerung
library(shinyBS)       # Für Bootstrap Komponenten (Legacy)
library(bslib)
library(jsonlite)
library(data.table)
library(visNetwork)
library(wordcloud2)
library(stringr)
library(syuzhet)
library(igraph)
library(udpipe)
library(text2vec)      # Placeholder für semantische Ähnlichkeit
library(reticulate)    # Placeholder für BERT-Integration
library(tidyr)
library(ggplot2)
library(bsicons)
library(DT)
library(plotly)

### Gemeinsame Einstellungen: Theme, Custom CSS und JavaScript

custom_theme <- bs_theme(
  version = 5,
  primary = "#0063A6",    # WU primary blue
  secondary = "#009A93",  # WU teal
  info = "#4c4c4c",       # Medium gray
  success = "#f0f0f0",    # Light gray
  warning = "#F6A800",    # WU yellow/orange
  danger = "#E4003A"      # WU red
)

custom_css <- tags$style(HTML("
  /* Import fonts */
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=Montserrat:wght@500;600;700&display=swap');

/* Root variables for consistent theming */
:root {
  /* Brand colors */
  --primary: #000000;
  --primary-dark: #000000;
  --primary-light: #e6f2f9;
  --secondary: #FFFFFF;
  --secondary-light: #e6f5f4;
  --accent: #8B0000;
  --danger: #E4003A;
  
  /* Neutral colors */
  --dark: #333333;
  --medium: #4c4c4c;
  --light: #f8f9fa;
  --lighter: #f0f0f4;
  --border: #dee2e6;
  
  /* Shadows */
  --shadow-sm: 0 2px 4px rgba(0,0,0,0.05);
  --shadow-md: 0 4px 8px rgba(0,0,0,0.1);
  --shadow-lg: 0 8px 16px rgba(0,0,0,0.15);
  
  /* Border radius */
  --radius-sm: 4px;
  --radius-md: 8px;
  --radius-lg: 12px;
  
  /* Animation */
  --transition-fast: all 0.15s ease;
  --transition: all 0.3s ease;
  
  /* Spacing */
  --spacing-xs: 0.5rem;  
  --spacing-sm: 1rem;    
  --spacing-md: 1.5rem; 
  --spacing-lg: 2rem;    
  --spacing-xl: 3rem;    
  
  /* Typography */
  --font-primary: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  --font-headings: 'Montserrat', sans-serif;
  --title-size: 2.5rem;     
  --subtitle-size: 1.5rem;  
  --h1-size: 1.875rem;      
  --h2-size: 1.625rem;      
  --h3-size: 1.375rem;     
  --h4-size: 1.25rem;       
  --body-size: 1rem;        
  --small-size: 0.875rem;   
  
  /* Component sizes */
  --control-height: 3rem;      
  --card-padding: 1.25rem;     
  --card-header-height: 3.75rem; 
  --nav-height: 3.75rem;       
  --sidebar-width: 21.875rem;  
  --header-height: 9rem;     
}

/* Reset and base styles */
html, body {
  min-height: 100vh;
  width: 100vw;
  margin: 0;
  padding: 0;
  overflow-x: hidden;
  font-family: var(--font-primary);
  color: var(--dark);
  line-height: 1.5;
  background-color: #f5f7f9;
}

/* Scrollbar styling */
::-webkit-scrollbar {
  width: 8px;
  height: 8px;
}

::-webkit-scrollbar-track {
  background: var(--lighter);
  border-radius: 10px;
}

::-webkit-scrollbar-thumb {
  background: #BBB;
  border-radius: 10px;
}

::-webkit-scrollbar-thumb:hover {
  background: #999;
}

/* Layout components */
#app {
  min-height: 100vh;
  display: flex;
  flex-direction: column;
}

.app-header {
  height: var(--header-height);
  width: 100%;
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%);
  padding: var(--spacing-md) var(--spacing-lg);
  box-shadow: var(--shadow-md);
  position: relative;
  z-index: 10;
  display: flex;
  flex-wrap: wrap;
  align-items: center;
}

.app-header::after {
  content: '';
  position: absolute;
  bottom: 0;
  left: 0;
  right: 0;
  height: 8px;
  background: linear-gradient(90deg, var(--accent) 0%, var(--secondary) 100%);
}

.app-header-section { 
  flex: 1; 
}

.app-header-section.left { 
  text-align: left; 
}

.app-header-section.right { 
  text-align: right; 
}

.app-title-container {
  flex: 1;
  text-align: center;
  padding: 0 var(--spacing-lg);
}

.app-title {
  font-family: var(--font-headings);
  color: white;
  font-weight: 700;
  font-size: var(--title-size);
  margin-bottom: var(--spacing-xs);
  text-align: center;
  letter-spacing: -0.5px;
  text-shadow: 0 3px 10px rgba(0,0,0,0.3);
}

.app-subtitle {
  font-family: var(--font-primary);
  color: rgba(255, 255, 255, 0.9);
  font-weight: 400;
  font-size: var(--subtitle-size);
  text-align: center;
  text-shadow: 0 2px 6px rgba(0,0,0,0.2);
  margin-bottom: var(--spacing-md);
}

.app-logo {
  height: 80px;
  filter: drop-shadow(0 2px 4px rgba(0,0,0,0.2));
}

.inst-logo{
  height: 150px;
  filter: drop-shadow(0 2px 4px rgba(0,0,0,0.2));
}

/* Main content and layout */
.main-content {
  height: calc(100vh - var(--header-height));
  overflow: hidden;
  flex: 1;
  display: flex;
  flex-direction: column;
  padding: var(--spacing-md);
  gap: var(--spacing-md);
}

/* Row that takes full height */
.row.flex-fill {
  flex: 1 0 auto;
  height: calc(100vh - 180px) !important;
  min-height: 0;
}

/* Column containers */
.col-3.d-flex.flex-column, 
.col-4.d-flex.flex-column,
.col-5.d-flex.flex-column,
.col-6.d-flex.flex-column,
.col-md-6.d-flex.flex-column {
  height: 100%;
  min-height: 0;
  padding: 0 10px;
  position: relative;
}

 /* limit accordion‑body to vertical scroll only */
    .accordion .accordion-body {
      max-height: 250px;
      overflow-y: auto;
      overflow-x: hidden;
    }
    /* ensure sidebar never scrolls horizontally */
    .sidebar-panel {
      overflow-x: hidden !important;
    }


/* Navigation tabs */
.nav-tabs {
  border-bottom: none;
  overflow-x: auto;
  white-space: nowrap;
  -webkit-overflow-scrolling: touch;
  background-color: white;
  box-shadow: var(--shadow-sm);
  padding: 0 var(--spacing-md);
  border-radius: var(--radius-md) var(--radius-md) 0 0;
  gap: 4px;
  height: var(--nav-height);
  scrollbar-width: thin;
}

.nav-tabs .nav-link {
  padding: 0 var(--spacing-md);
  font-size: 16px;
  font-weight: 600;
  transition: var(--transition);
  margin-right: 4px;
  border: none;
  color: var(--medium) !important;
  height: var(--nav-height);
  display: flex;
  align-items: center;
  text-transform: uppercase;
  position: relative;
}

.nav-tabs .nav-link.active {
  color: var(--primary) !important;
  font-weight: 700;
  background-color: transparent;
}

.nav-tabs .nav-link.active::after {
  content: '';
  position: absolute;
  bottom: 0;
  left: 0;
  right: 0;
  height: 4px;
  background-color: var(--primary);
  border-radius: 4px 4px 0 0;
}

.nav-tabs .nav-link:hover:not(.active) {
  background-color: var(--lighter);
  color: var(--primary) !important;
}

/* Cards */
.card {
  display: flex;
  flex-direction: column;
  margin-bottom: var(--spacing-md);
  overflow: hidden;
  border-radius: var(--radius-md);
  box-shadow: var(--shadow-md);
  border: none;
  background-color: white;
  transition: var(--transition);
}

.card-hover, .card:hover {
  box-shadow: var(--shadow-lg);
  transform: translateY(-2px);
}

/* Card that fills its container */
.card.flex-fill {
  display: flex;
  flex-direction: column;
  height: 100%;
  min-height: 0;
  margin-bottom: 0;
}

/* Network card specific fix */
.col-6.d-flex.flex-column .card.flex-fill {
  position: absolute;
  top: 0;
  left: 10px;
  right: 10px;
  bottom: 0;
  height: auto !important;
}

/* Right column cards in User Perspective - accordion style */
.perspective-right-column .card {
  margin-bottom: 15px !important;
  flex-shrink: 0;
}

/* Style for collapsed cards */
.perspective-right-column .card .collapsible.collapsed + .card-header {
  border-bottom: none;
}

.perspective-right-column .card .card-header {
  cursor: pointer;
  min-height: var(--card-header-height);
}

/* Default collapsed state for all except the first one */
.perspective-right-column .card .collapsible {
  max-height: 0;
  padding: 0;
  overflow: hidden;
  transition: max-height 0.3s ease, padding 0.3s ease;
}

/* First card expanded by default */
.perspective-right-column .card:first-child .collapsible {
  max-height: 500px;
  padding: var(--card-padding);
  overflow: auto;
}

/* Toggle button icon states */
.perspective-right-column .card:first-child .toggle-btn i {
  transform: rotate(180deg);
}

.perspective-right-column .card .toggle-btn i {
  transition: transform 0.3s ease;
}

/* Active state - when expanded */
.perspective-right-column .card .collapsible:not(.collapsed) {
  max-height: 500px;
  padding: var(--card-padding);
  overflow: auto;
}

.perspective-right-column .card .collapsible:not(.collapsed) ~ .card-header .toggle-btn i {
  transform: rotate(180deg);
}

.card-header {
  padding: var(--spacing-sm) var(--spacing-md);
  background-color: white;
  border-bottom: 3px solid var(--primary-light);
  display: flex;
  align-items: center;
  min-height: var(--card-header-height);
}

.card-body {
  flex: 1;
  overflow: auto;
  padding: var(--card-padding);
}

/* Card body that can scroll and fills its container */
.card-body.flex-fill {
  flex: 1 1 auto;
  overflow: auto;
  padding: var(--card-padding);
  display: flex;
  flex-direction: column;
  min-height: 0;
}

/* Network card body specific fix */
.col-6.d-flex.flex-column .card.flex-fill .card-body {
  padding: 0 !important;
  position: absolute;
  top: var(--card-header-height);
  left: 0;
  right: 0;
  bottom: 0;
  height: auto !important;
}

/* Reset some paddings for network container */
.card-body.p-0 {
  padding: 0 !important;
}

/* Collapsible sections */
.collapsible {
  transition: max-height 0.3s ease, padding 0.3s ease;
}

.collapsible.collapsed {
  max-height: 0 !important;
  padding-top: 0 !important;
  padding-bottom: 0 !important;
  overflow: hidden !important;
  border-top: none;
  border-bottom: none;
}

/* Sidebar */
.sidebar-panel {
  background-color: white;
  border-radius: var(--radius-md);
  padding: var(--spacing-md);
  max-height: calc(100vh - 180px);
  overflow-y: auto;
  box-shadow: var(--shadow-md);
  width: var(--sidebar-width);
}

/* Form controls */
.form-group {
  margin-bottom: var(--spacing-md);
}

select.form-select,
input.form-control {
  font-family: var(--font-primary);
  font-size: var(--body-size);
  padding: 0.5rem 0.75rem;
  height: var(--control-height);
  border-radius: var(--radius-sm);
  border: 1px solid var(--border);
  background-color: white;
  box-shadow: var(--shadow-sm);
  transition: var(--transition-fast);
}

select.form-select:focus,
input.form-control:focus {
  border-color: var(--primary);
  box-shadow: 0 0 0 3px rgba(0, 99, 166, 0.15);
  outline: none;
}

/* Buttons */
.btn {
  font-family: var(--font-primary);
  font-weight: 500;
  font-size: var(--body-size);
  padding: 0.5rem 1rem;
  border-radius: var(--radius-sm);
  transition: var(--transition-fast);
  box-shadow: var(--shadow-sm);
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 0.5rem;
}

.btn-primary {
  background-color: var(--primary) !important;
  border-color: var(--primary) !important;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  color: white;
}

.btn-primary:hover {
  background-color: var(--primary-dark) !important;
  border-color: var(--primary-dark) !important;
  transform: translateY(-2px);
  box-shadow: var(--shadow-md);
}

.btn-primary:active {
  transform: translateY(0);
}

.btn-sm {
  padding: 0.25rem 0.5rem;
  font-size: 0.875rem;
}

.toggle-btn {
  background: transparent;
  border: none;
  cursor: pointer;
  color: var(--medium);
  z-index: 5;
}

.toggle-btn:hover {
  color: var(--primary);
}

/* Switches and checkboxes */
.form-switch {
  display: inline-flex;
  align-items: center;
  gap: 0.5rem;
  position: relative;
  user-select: none;
}

.form-switch .form-check-input {
  height: 1.25rem;
  width: 2.5rem;
  border-radius: 1.25rem;
  background-color: var(--lighter);
  border: 1px solid var(--border);
  cursor: pointer;
  appearance: none;
  transition: var(--transition-fast);
  margin-right: 0;
}

.form-switch .form-check-input:checked {
  background-color: var(--primary);
  border-color: var(--primary);
}

.form-switch .form-check-input:focus {
  box-shadow: 0 0 0 3px rgba(0, 99, 166, 0.15);
  outline: none;
}

.switch-slider {
  position: absolute;
  width: 1rem;
  height: 1rem;
  left: 0.2rem;
  top: 0.2rem;
  border-radius: 50%;
  background-color: white;
  transition: var(--transition-fast);
  pointer-events: none;
}

.form-switch .form-check-input:checked + .switch-slider {
  transform: translateX(1.25rem);
}

/* Special components */
.verbatim-text {
  background-color: var(--lighter);
  border-radius: var(--radius-sm);
  padding: var(--spacing-sm);
  max-height: 300px;
  overflow-y: auto;
  font-family: monospace;
  white-space: pre-wrap;
  word-wrap: break-word;
  font-size: 0.9rem;
  line-height: 1.4;
}

.emotion-legend {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
  gap: 0.5rem;
  margin-top: 0.5rem;
}

.emotion-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.875rem;
}

.emotion-color {
  display: inline-block;
  width: 16px;
  height: 16px;
  border-radius: 50%;
}

/* Network and plot containers */
.network-container,
.small-network-container,
.plot-container,
.small-plot-container,
.medium-plot-container,
.large-plot-container {
  position: absolute !important;
  top: 0 !important;
  left: 0 !important;
  right: 0 !important;
  bottom: 0 !important;
  height: 100% !important;
  width: 100% !important;
  border-radius: var(--radius-sm);
  overflow: visible !important;
}

/* Minimum heights for different container sizes */
.small-network-container,
.small-plot-container {
  min-height: 200px;
}

.medium-plot-container {
  min-height: 300px;
}

.large-plot-container {
  min-height: 400px;
}

/* Make network and plot outputs fill their containers */
.network-container .visNetwork,
.small-network-container .visNetwork,
.visNetwork,
.visNetwork > canvas,
.visNetwork > div {
  position: absolute !important;
  top: 0 !important;
  left: 0 !important;
  right: 0 !important;
  bottom: 0 !important;
  height: 100% !important;
  width: 100% !important;
}

/* Fix for plots */
.plot-container .shiny-plot-output,
.small-plot-container .shiny-plot-output,
.medium-plot-container .shiny-plot-output,
.large-plot-container .shiny-plot-output {
  position: absolute !important;
  top: 0 !important;
  left: 0 !important;
  right: 0 !important;
  bottom: 0 !important;
  height: 100% !important;
  width: 100% !important;
}

/* Perspective right column layout */
.perspective-right-column {
  display: flex;
  flex-direction: column;
  height: 100%;
  min-height: 0;
  gap: 15px;
  overflow-y: auto;
  padding-right: 5px;
}

/* Help with flexbox proper nesting */
.d-flex.flex-column.gap-3 {
  height: 100%;
  min-height: 0;
}

/* Metrics grid */
.metrics-grid {
  display: grid;
  grid-template-columns: 1fr;      /* one column only */
  grid-auto-rows: minmax(80px, auto); /* adjust min‑height */
  gap: 1rem;
}


.metrics-item {
  background-color: var(--lighter);
  border-radius: var(--radius-sm);
  padding: 1rem;
  text-align: center;
  transition: var(--transition-fast);
}

.metrics-item:hover {
  background-color: var(--primary-light);
  transform: translateY(-2px);
}

.metrics-value {
  font-size: 1.5rem;
  font-weight: 600;
  color: var(--primary);
  margin-bottom: 0.25rem;
}

.metrics-label {
  font-size: 0.875rem;
  color: var(--medium);
}

/* Animation classes */
.fade-in {
  animation: fadeIn 0.5s ease forwards;
}

.fade-in-up {
  animation: fadeInUp 0.5s ease forwards;
}

@keyframes fadeIn {
  from { opacity: 0; }
  to { opacity: 1; }
}

@keyframes fadeInUp {
  from {
    opacity: 0;
    transform: translateY(20px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

/* Fix issues with DataTables */
.dataTables_wrapper {
  width: 100%;
}

/* Discussion metrics styling */
.metrics-container {
  padding: 10px;
}

/* Thread statistics tabs */
#thread_stats_tabs > .tab-content {
  padding: 0;
  height: calc(100% - 50px);
}

#thread_stats_tabs > .tab-content > .tab-pane {
  height: 100%;
}

/* Responsive adjustments */
@media (max-width: 992px) {
  :root {
    --sidebar-width: 100%;
    --title-size: 1.75rem;
    --subtitle-size: 1.25rem;
  }
  
  .app-header {
    height: auto;
    padding: var(--spacing-sm);
  }
  
  .main-content {
    height: auto;
    min-height: calc(100vh - 180px);
  }
  
  .sidebar-panel {
    max-height: none;
  }
  
  .metrics-grid {
    grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
  }
  
  .col-3.d-flex.flex-column, 
  .col-4.d-flex.flex-column,
  .col-5.d-flex.flex-column,
  .col-6.d-flex.flex-column {
    padding: 0 5px;
  }
}

@media (max-width: 768px) {
  .app-title-container {
    padding: 0;
  }
  
  .card-header {
    padding: var(--spacing-sm);
  }
  
  .nav-tabs {
    padding: 0 var(--spacing-sm);
  }
  
  .nav-tabs .nav-link {
    padding: 0 var(--spacing-sm);
    font-size: 14px;
  }
  
  .row.flex-fill {
    flex-direction: column;
  }
  
  .col-3.d-flex.flex-column, 
  .col-4.d-flex.flex-column,
  .col-5.d-flex.flex-column,
  .col-6.d-flex.flex-column,
  .col-md-6.d-flex.flex-column {
    width: 100%;
    padding: 0;
    margin-bottom: 15px;
  }
  
  .card.flex-fill {
    height: auto;
    min-height: 400px;
  }
  
  /* Mobile adjustment for perspective right column */
  .perspective-right-column {
    height: auto;
    overflow-y: visible;
  }
  
  .perspective-right-column .card {
    max-height: none !important;
  }
}
.app-title-container p.app-copyright {
  color: rgba(255,255,255,0.7);
  font-size: var(--small-size);
  margin-top: var(--spacing-sm);
  margin-bottom: 0;  /* keep it tight */
}
@media (min-width: 1200px) {
  :root {
    --header-height: 12rem;
  }
}

/* Add or update these styles in your custom_css */

/* Global perspective specific styles */
#global-perspective .card {
  margin-bottom: 15px;
}

#global-perspective .card-body {
  padding: 15px;
}

/* Remove the flex-fill classes that were causing excessive height */
#global-perspective .plot-container,
#global-perspective .medium-plot-container {
  height: 400px !important;
  position: relative;
  overflow: hidden;
}

#global-perspective .shiny-plot-output {
  height: 100% !important;
  width: 100% !important;
}

/* Allow scrolling for the entire content area */
#global-perspective .sidebar-layout > div:last-child {
  overflow-y: auto;
}

/* Ensure cards don't stretch to full height */
#global-perspective .card {
  height: auto !important;
}

/* Adjust spacing between cards */
#global-perspective .row {
  margin-bottom: 20px;
}

/* --- Argument Structure Tab Layout Fix --- */
#tabsetpanel-argument-structure .row.flex-fill {
  height: 100%;
  min-height: 0;
}
#tabsetpanel-argument-structure .col-3.d-flex.flex-column {
  height: 100%;
  min-height: 0;
  display: flex;
  flex-direction: column;
}
#tabsetpanel-argument-structure .col-9.d-flex.flex-column {
  height: 100%;
  min-height: 0;
  display: flex;
  flex-direction: column;
  gap: 20px;
  background: #f8f9fa;
  padding: 10px 0 10px 0;
}
#tabsetpanel-argument-structure .col-9.d-flex.flex-column > .d-flex.flex-column {
  height: 100%;
  min-height: 0;
  gap: 20px;
}
#tabsetpanel-argument-structure .col-9.d-flex.flex-column > .d-flex.flex-column > div {
  flex: 1 1 0;
  min-height: 0;
  max-height: 50%;
  display: flex;
  flex-direction: column;
}
#tabsetpanel-argument-structure .card {
  height: 100%;
  min-height: 0;
  display: flex;
  flex-direction: column;
  margin-bottom: 0;
  box-shadow: var(--shadow-md);
  border-radius: var(--radius-md);
  background: #fff;
}
#tabsetpanel-argument-structure .card-body {
  flex: 1 1 auto;
  min-height: 0;
  overflow-y: auto;
  padding: var(--card-padding);
}
#tabsetpanel-argument-structure .card-header {
  background: #f5f7f9;
  border-bottom: 2px solid var(--primary-light);
  font-weight: 600;
  font-size: 1.1rem;
  min-height: 48px;
}
#tabsetpanel-argument-structure .alert-info {
  margin-bottom: 10px;
  border-radius: 8px;
  font-size: 1.08rem;
}
/* Subtle background for main area */
#tabsetpanel-argument-structure .col-9.d-flex.flex-column {
  background: #f8f9fa;
}
/* Remove extra margin from cards */
#tabsetpanel-argument-structure .card + .card {
  margin-top: 20px;
}
/* Responsive: stack cards on mobile */
@media (max-width: 992px) {
  #tabsetpanel-argument-structure .col-9.d-flex.flex-column > .d-flex.flex-column > div {
    max-height: none;
    min-height: 300px;
  }
  #tabsetpanel-argument-structure .col-9.d-flex.flex-column {
    padding: 0;
  }
}
#tabsetpanel-argument-structure .sidebar-panel {
  /* override the fixed 350px width */
  width: auto !important;
}

"))

custom_js <- tags$script(HTML("
  $(document).ready(function() {
  // Initialize UI enhancements
  function enhanceUI() {
    // Card hover effects
    $('.card').hover(
      function() { $(this).addClass('card-hover'); },
      function() { $(this).removeClass('card-hover'); }
    );
    
    // Initial tab indicator
    animateTabIndicator($('.nav-link.active'));
    
    // Add animation classes to elements
    addAnimationClasses();
    
    // Enhance form controls
    enhanceFormControls();
  }
  
  // Animate the tab indicator for the active tab
  function animateTabIndicator($activeTab) {
    $('.nav-link').removeClass('tab-animating');
    $activeTab.addClass('tab-animating');
    setTimeout(function() { 
      $('.nav-link').removeClass('tab-animating'); 
    }, 500);
  }
  
  // Add animation classes to various elements
  function addAnimationClasses() {
    $('.card').each(function(index) {
      var $card = $(this);
      setTimeout(function() { 
        $card.addClass('fade-in-up'); 
      }, index * 100);
    });
    
    $('.network-container, .small-network-container, .plot-container, .small-plot-container, .medium-plot-container, .large-plot-container')
      .addClass('fade-in');
  }
  
  // Enhance form controls with visual feedback
  function enhanceFormControls() {
    $('select.form-select').each(function() {
      var $select = $(this);
      if ($select.val() && $select.val() !== '') { 
        $select.addClass('has-value'); 
      }
      
      $select.on('change', function() {
        $select.toggleClass('has-value', $select.val() && $select.val() !== '');
      });
    });
    
    $('.form-switch .form-check-input').after('<span class=\"switch-slider\"></span>');
  }
  
  // Initialize collapsible panels in the User Perspective tab
  function initializeCollapsiblePanels() {
    var allPanels = $('.perspective-right-column .collapsible');
    
    // By default, expand the first two panels and collapse the rest
    allPanels.each(function(index) {
      var $panel = $(this);
      var $toggleBtn = $panel.closest('.card').find('.toggle-btn i);
      
      if (index >= 2) {
        $panel.addClass('collapsed');
        $toggleBtn.removeClass('fa-chevron-up').addClass('fa-chevron-down');
      } else {
        $panel.removeClass('collapsed');
        $toggleBtn.removeClass('fa-chevron-down').addClass('fa-chevron-up');
      }
    });
    
    // Trigger resize to update any visualizations
    setTimeout(function() { 
      $(window).trigger('resize'); 
    }, 300);
  }
  
  // Initialize UI enhancements
  enhanceUI();
  
  // Handle tab changes
  $('a[data-bs-toggle=\"tab\"]').on('shown.bs.tab', function (e) {
    var $target = $(e.target);
    
    // Animate the tab indicator
    animateTabIndicator($target);
    
    // If User Perspective tab is shown, initialize its panels
    if ($target.text().trim() === 'User Perspective') {
      initializeCollapsiblePanels();
      Shiny.setInputValue('userPerspectiveTabShown', true, {priority: 'event'});
    }
    
    // Redraw all network visualizations after a short delay
    setTimeout(function() {
      $('.visNetwork').each(function() {
        if ($(this).data('vis-network')) { 
          $(this).data('vis-network').redraw(); 
        }
      });
    }, 300);
  });
  
  // Handle panel toggle button clicks
  $(document).on('click', '.toggle-btn', function() {
    var $card = $(this).closest('.card');
    var $panel = $card.find('.collapsible');
    var $icon = $(this).find('i');
    var isCollapsed = $panel.hasClass('collapsed');
    
    // Toggle the collapsed state
    $panel.toggleClass('collapsed', !isCollapsed);
    
    // Update the icon
    $icon.toggleClass('fa-chevron-up', !isCollapsed)
         .toggleClass('fa-chevron-down', isCollapsed);
    
    // Notify Shiny of the change
    Shiny.setInputValue('togglePanel', {
      panelId: $panel.attr('id'),
      buttonId: $(this).attr('id')
    }, {priority: 'event'});
    
    // Trigger resize to update any visualizations
    setTimeout(function() {
      $(window).trigger('resize');
    }, 300);
    
    return false; // Prevent event bubbling
  });
  
  // Handle custom messages from the server to update icons
  Shiny.addCustomMessageHandler('updateIcon', function(message) {
    var button = document.getElementById(message.id);
    if (button) {
      var icon = button.querySelector('i');
      if (icon) {
        icon.classList.remove('fa-chevron-up', 'fa-chevron-down');
        icon.classList.add('fa-' + message.icon);
      }
    }
  });
  
  // Check if the User Perspective tab is initially active
  if ($('.nav-link.active').text().trim() === 'User Perspective') {
    initializeCollapsiblePanels();
    Shiny.setInputValue('userPerspectiveTabShown', true, {priority: 'event'});
  }
});
"))

### UI Module Definition

# Header-Bereich
appHeader <- div(class = "app-header",
                 div(class = "app-header-section left",
                     img(src = "complex_nw.png", class = "inst-logo", alt = "Institue Logo")
                 ),
                 div(class = "app-title-container",
                     h1(class = "app-title",    "Shiny for Emotions"),
                     h3(class = "app-subtitle", "A Reddit r/ChangeMyView Analysis"),
                     tags$p(
                       class = "app-copyright",
                       "© Benjamin Scepka BSc."
                     )
                 ),
                 div(class = "app-header-section right",
                     img(src = "wu-white.png", class = "app-logo", alt = "WU Logo")
                 )
)


# Home Tab


homeTab <- nav_panel(
  "Home",
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      class = "sidebar-panel",
      h3("Shiny for Emotions"),
      p("Interactive analysis of narrative evolution and persuasion on r/ChangeMyView."),
      hr(),
      tags$small("© Benjamin Scepka BSc.")
    ),
    card(
      class = "flex-fill",
      card_header(h3("Welcome & Overview")),
      card_body(
        accordion(
          id   = "home_accordion",
          open = "Introduction",
          
          # 1. Introduction
          accordion_panel(
            title = "Introduction",
            icon  = bs_icon("info-circle"),
            tags$p(
              "This application explores how arguments unfold and persuade on Reddit's ",
              tags$em("r/ChangeMyView"), " community. ",
              "By tracking shifts in emotional tone, argument style, and narrative structure, ",
              "it aims to shed light on what makes online debates successful."
            )
          ),
          
          # 2. Dataset Overview
          accordion_panel(
            title = "Dataset Overview",
            icon  = bs_icon("database"),
            tags$ul(
              tags$li(strong("Source:"), "Reddit's r/ChangeMyView subreddit"),
              tags$li(strong("Posts analyzed:"), "Initial posts and all replies in 79 threads"),
              tags$li(strong("Total delta posts:"), "19 (~24%)"),
              tags$li(strong("Total comments (incl Replies):"), "1134"),
              tags$li(strong("Total (unique) users:"), "953"),
              tags$li(strong("Time frame:"), " 11/2024 – 03/2024"),
              tags$li(strong("General topics:"), "Politics, social issues, technology, etc."),
              tags$li(strong("Data collection:"), " Web scraping using Python and Reddit API"),
              tags$li(strong("Features:"), " Sentiment (NRC + valence), argument components, network structure, Δ awards")
            )
          ),
          
          # 3. How CMV Works
          accordion_panel(
            title = "How r/ChangeMyView Works",
            icon  = bs_icon("chat-square-text"),
            tags$p(
              "r/ChangeMyView is a unique Reddit community where users post opinions they're open to changing. ",
              "Other users respond with counterarguments, and if the original poster (OP) changes their view, ",
              "they award a delta (Δ) to acknowledge this shift in perspective."
            ),
            tags$p(
              "The community follows specific rules to maintain productive discussions:",
              tags$ul(
                tags$li("OPs must be genuinely open to changing their view"),
                tags$li("Responses must challenge some aspect of the OP's view"),
                tags$li("Deltas are awarded when a comment changes the OP's view, even partially"),
                tags$li("Comments must meet quality standards (no low-effort responses)")
              )
            ),
            tags$p(
              "This structure creates a natural experiment for studying persuasion dynamics, ",
              "as delta awards provide clear markers of successful persuasion."
            )
          ),
          
          # 5. Delta System Explained
          accordion_panel(
            title = "Delta System Explained",
            icon  = bs_icon("award"),
            tags$p(
              "The delta (Δ) is the core mechanism of r/ChangeMyView that signals successful persuasion."
            ),
            tags$div(
              class = "delta-explanation",
              tags$h4("How Deltas Work:"),
              tags$ul(
                tags$li(
                  tags$strong("Who awards:"), " The original poster (OP) awards deltas to users who changed their view"
                ),
                tags$li(
                  tags$strong("How to award:"), " By typing '!delta' or the Δ symbol with an explanation"
                ),
                tags$li(
                  tags$strong("Partial changes:"), " Deltas can be awarded for partially changing a view, not just complete reversals"
                ),
              ),
              tags$p(
                "In this analysis, deltas serve as clear markers of persuasion success, ",
                "helping us identify effective persuasion strategies."
              )
            )
          ),
          
          # 6. Using This Dashboard
          accordion_panel(
            title = "Using This Dashboard",
            icon  = bs_icon("info-square"),
            tags$div(
              class = "dashboard-guide",
              tags$h4("Main Views:"),
              tags$ul(
                tags$li(
                  tags$strong("Network:"), " Visualize discussion structures and flows"
                ),
                tags$li(
                  tags$strong("Time:"), " Analyze how discussions evolve over time"
                ),
                tags$li(
                  tags$strong("User:"), " Examine participant behaviors and patterns"
                ),
                tags$li(
                  tags$strong("Arguments:"), " Explore argumentation styles and components"
                ),
                tags$li(
                  tags$strong("Global:"), " View aggregate patterns across all discussions"
                )
              ),
              tags$p(
                "Begin by selecting a discussion from the dropdown menu in the Network tab. ",
                "You can then explore different perspectives using the tab navigation, ",
                "or switch to the Global view for dataset-wide patterns."
              ),
              tags$p(
                "Pay special attention to nodes marked with a triangle shape - ",
                "these represent posts that received a delta, indicating successful persuasion."
              )
            )
          ),
          
          # 7. Definitions
          accordion_panel(
            title = "Definitions",
            icon  = bs_icon("book"),
            tags$ul(
              tags$li(
                tags$strong("Ethos:"), 
                " Appeals to credibility or character. It persuades by establishing the speaker's authority or trustworthiness."
              ),
              tags$li(
                tags$strong("Pathos:"), 
                " Appeals to emotion. It persuades by evoking an emotional response from the audience."
              ),
              tags$li(
                tags$strong("Logos:"), 
                " Appeals to logic or reason. It persuades by using evidence, facts, or logical arguments."
              ),
              tags$li(
                tags$strong("Claims:"), 
                " Statements or assertions that are open to challenge and require justification."
              ),
              tags$li(
                tags$strong("Premises:"), 
                " Supporting statements that provide reasons or evidence for a claim."
              ),
              tags$li(
                tags$strong("Rebuttals:"), 
                " Counterarguments that challenge or refute a claim or premise."
              ),
              tags$li(
                tags$strong("Concessions:"), 
                " Acknowledgments of opposing viewpoints or arguments, often used to strengthen one's own position."
              ),
              tags$li(
                tags$strong("Evidence:"), 
                " Information or data used to support claims, premises, or arguments."
              )
            ),
            tags$h4("How the BART Model Classifies Text:"),
            tags$p(
              "The BART (Bidirectional and Auto-Regressive Transformers) model is fine-tuned for zero-shot classification tasks. ",
              "It uses a pre-trained language model to predict the likelihood of a given text belonging to specific categories, such as ",
              tags$em("Ethos, Pathos, Logos, Claims, Premises, Rebuttals, Concessions, and Evidence."),
              " The model is provided with candidate labels and evaluates the text against these labels using contextual embeddings."
            ),
            tags$p(
              "For example, the model is asked to classify a text by comparing it to the labels ",
              tags$strong("Ethos, Pathos, Logos"), 
              " and assigning scores based on how well the text aligns with each label. ",
              "This process is repeated for other categories like Claims, Premises, and Evidence."
            ),
            tags$p(
              "The classification is achieved by leveraging the model's understanding of language semantics, ",
              "which has been learned from large-scale datasets during pre-training. This allows the model to identify patterns, ",
              "such as emotional tone (Pathos), logical reasoning (Logos), or credibility (Ethos), in the text."
            )
          )
        )
      )
    )
  )
)



viewingViewsTab <- nav_panel(
  "Conversation Network Views",
  div(
    class = "container-fluid flex-fill d-flex flex-column p-0",
    style = "height: 100%;",
    
    # Dropdown ganz oben
    div(class = "p-3",
        selectInput(
          "viewPerspective",
          "Viewing Perspective:",
          choices = c(
            "Posting Perspective" = "posting",
            "User Perspective"    = "user",
            "Temporal Perspective"    = "time",
            "Global Perspective" = "global"
          ),
          selected = "posting"
        )
    ),
    
    # Posting Perspective UI
    conditionalPanel(
      "input.viewPerspective == 'posting'",
      div(
        class = "row flex-fill m-0",
        div(
          class = "container-fluid flex-fill d-flex flex-column p-0",
          style = "height: 100%; min-height: 0;",
          
          # Zeile, die die volle Höhe nutzt
          div(
            class = "row flex-fill m-0",
            style = "height: 100%; min-height: 0;",
            
            # Linke Spalte (30%)
            div(
              class = "col-3 d-flex flex-column p-0",
              style = "height: 100%; min-height: 0;",
              card(
                class = "flex-fill d-flex flex-column",
                card_header(h3("Controls")),
                card_body(
                  class = "flex-fill overflow-auto",
                  input_switch("highlight_path", "Highlight path to Delta", value = FALSE),
                  hr(),
                  selectInput("selected_post", "Select Discussion*:", choices = NULL),
                  tags$small("*Discussion titles classified and displayed using SVO analysis", style = "color: #6c757d; font-style: italic;"),
                  input_switch("filter_delta", "Show only threads with Delta (Δ)", value = TRUE),
                  hr(),
                  accordion(
                    id = "acc",
                    open = "Original Post",
                    accordion_panel(
                      title = "Original Post",
                      icon  = bs_icon("menu-app"),
                      uiOutput("original_post_text", placeholder = TRUE)
                    ),
                    accordion_panel(
                      title = "Selected Node Text",
                      icon  = bs_icon("sliders"),
                      uiOutput("selected_node_text", placeholder = TRUE)
                    )
                  ),
                  hr(),
                  h4(class = "legend-title", "Legend"),
                  p(strong("Shapes:"), "Triangle: Post/comment with Delta (Δ) • Dot: Regular post/comment"),
                  div(
                    class = "emotion-legend",
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#000000;"), "Original Poster"),
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#F6A800;"), "Joy"),
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#0063A6;"), "Sadness"),
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#E4003A;"), "Anger"),
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#009A93;"), "Trust"),
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#9B26B6;"), "Fear"),
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#8B572A;"), "Disgust"),
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#FF6B00;"), "Anticipation"),
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#EC86D0;"), "Surprise"),
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#A4D3EE;"), "Neutral"),
                    div(class = "emotion-item", span(class = "emotion-color", style = "background-color:#888888;"), "Unclassified")
                  )
                )
              )
            ),
            
            # Mittlere Spalte (60%) - Network Visualisierung
            div(
              class = "col-6 d-flex flex-column p-0",
              style = "height: 100%; min-height: 0;",
              card(
                class = "flex-fill d-flex flex-column",
                card_body(
                  class = "flex-fill p-0",
                  div(
                    class = "network-container flex-fill",
                    style = "height: 100%; min-height: 0;",
                    visNetworkOutput("network", height = "100%", width = "100%")
                  )
                )
              )
            ),
            
            # Rechte Spalte (30%) - Discussion Details
            div(
              class = "col-3 d-flex flex-column p-0",
              style = "height: 100%; min-height: 0;",
              card(
                class = "flex-fill d-flex flex-column",
                card_header(h4("Discussion Details")),
                card_body(
                  class = "flex-fill overflow-auto",
                  div(class = "metrics-container", uiOutput("discussion_metrics"))
                )
              )
            )
          )
        )
      )
    ),
    
    # User Perspective UI
    conditionalPanel(
      "input.viewPerspective == 'user'",
      div(
        class = "row flex-fill m-0",
        div(
          class = "container-fluid flex-fill d-flex flex-column p-0",
          style = "height: 100%; min-height: 0;",
          div(
            class = "row flex-fill m-0",
            style = "height: 100%; min-height: 0;",
            
            # Left column: Controls
            div(
              class = "col-3 d-flex flex-column p-0",
              style = "height: 100%; min-height: 0;",
              card(
                class = "flex-fill d-flex flex-column",
                card_header(h3("Controls")),
                card_body(
                  class = "flex-fill overflow-auto",
                  input_switch("remove_op", "Remove OP", value = TRUE),
                  sliderInput("min_component_size", "Min Interactions", min = 0, max = 10, value = 0, step = 1),
                  hr(),
                  h4("Adjust which graphs appear on the right"),
                  selectInput(
                    "right_graph1", "Graph in Card 1:",
                    choices = c(
                      "Timeline"            = "user_timeline",
                      "Delta Participation" = "user_delta",
                      "Network Metrics"     = "nw_metrics"
                    ), selected = "nw_metrics"
                  ),
                  # Add description for card 1
                  div(class = "mb-3", style = "font-size: 0.9rem; font-style: italic; color: #6c757d;",
                      uiOutput("card1_desc")),
                  selectInput(
                    "right_graph2", "Graph in Card 2:",
                    choices = c(
                      "Timeline"            = "user_timeline",
                      "Delta Participation" = "user_delta",
                      "Network Metrics"     = "nw_metrics"
                    ), selected = "user_timeline"
                  ),
                  # Add description for card 2
                  div(class = "mb-3", style = "font-size: 0.9rem; font-style: italic; color: #6c757d;",
                      uiOutput("card2_desc"))
                )
              )
            ),
            
            # Middle column: User Interaction Network
            div(
              class = "col-6 d-flex flex-column p-0",
              style = "height: 100%; min-height: 0;",
              card(
                class = "flex-fill d-flex flex-column",
                card_body(
                  class = "flex-fill p-0",
                  div(
                    class = "network-container flex-fill",
                    visNetworkOutput("user_interaction_network", height = "100%", width = "100%")
                  )
                )
              )
            ),
            
            # Right column: Two dynamic graph cards
            div(
              class = "col-3 d-flex flex-column p-0",
              style = "height: 100%; min-height: 0; gap: 15px;",
              # Card 1
              card(
                class = "flex-fill d-flex flex-column",
                card_header(h4(textOutput("card1_title"))),
                card_body(
                  class = "flex-fill p-0",
                  uiOutput("card1_ui")
                )
              ),
              # Card 2
              card(
                class = "flex-fill d-flex flex-column",
                card_header(h4(textOutput("card2_title"))),
                card_body(
                  class = "flex-fill p-0",
                  uiOutput("card2_ui")
                )
              )
            )
          )
        )
      )
    ),
    
    # Temporal Perspective UI
    conditionalPanel(
      "input.viewPerspective == 'time'",
      div(
        class = "row flex-fill m-0",
        div(
          class = "container-fluid flex-fill d-flex flex-column p-0",
          style = "height:100%; min-height:0;",
          
          div(
            class = "row flex-fill m-0",
            style = "height:100%; min-height:0;",
            
            # Left sidebar - enhanced controls
            div(
              class = "col-3 d-flex flex-column p-0",
              style = "height:100%; min-height:0;",
              card(
                class = "flex-fill d-flex flex-column",
                card_header(h3("Time Controls")),
                card_body(
                  class = "flex-fill overflow-auto",
                  uiOutput("enhanced_time_slider"),
                  hr(),
                  uiOutput("current_post_preview"),
                  hr(),
                  # Add chart selector for middle card
                  selectInput(
                    "time_chart_type", 
                    "Chart to Display:",
                    choices = c(
                      "Emotion Trajectory" = "emotion_trajectory",
                      "Argument Tactics" = "argument_tactics"
                    ), 
                    selected = "emotion_trajectory"
                  ),
                  uiOutput("time_chart_description"),
                  hr(),
                  uiOutput("comparative_metrics"),
                  hr(),
                  uiOutput("key_events")
                )
              )
            ),
            
            # Main content area - reorganized
            div(
              class = "col-9 d-flex flex-column p-0",
              style = "height:100%; min-height:0;",
              
              # Top row: Timeline visualization
              div(
                class = "row flex-fill m-0 mb-3",
                style = "height:30%; min-height:0;",
                div(
                  class = "col-12 d-flex flex-column p-0",
                  style = "height:100%; min-height:0;",
                  card(
                    class = "flex-fill d-flex flex-column",
                    card_header(h4("Discussion Timeline")),
                    card_body(
                      class = "flex-fill p-0",
                      div(
                        class = "small-plot-container",
                        plotOutput("post_timeline", height = "100%", width = "100%")
                      )
                    )
                  )
                )
              ),
              
              # Middle row: Switchable charts (emotion or tactics)
              div(
                class = "row flex-fill m-0 mb-3",
                style = "height:30%; min-height:0;",
                div(
                  class = "col-12 d-flex flex-column p-0",
                  style = "height:100%; min-height:0;",
                  card(
                    class = "flex-fill d-flex flex-column",
                    card_header(
                      # Dynamic title based on selected chart
                      textOutput("time_chart_title")
                    ),
                    card_body(
                      class = "flex-fill p-0",
                      div(
                        class = "medium-plot-container flex-fill",
                        uiOutput("time_selected_chart")
                      )
                    )
                  )
                )
              ),
              
              # Bottom row: Network visualizations side by side
              div(
                class = "row flex-fill m-0",
                style = "height:40%; min-height:0;",
                div(
                  class = "col-6 d-flex flex-column p-0 pe-2",
                  style = "height:100%; min-height:0;",
                  card(
                    class = "flex-fill d-flex flex-column",
                    card_header(h4("Argument Flow Network")),
                    card_body(
                      class = "flex-fill p-0",
                      div(
                        class = "small-network-container",
                        visNetworkOutput("time_argument_network", height = "100%", width = "100%")
                      )
                    )
                  )
                ),
                div(
                  class = "col-6 d-flex flex-column p-0 ps-2",
                  style = "height:100%; min-height:0;",
                  card(
                    class = "flex-fill d-flex flex-column",
                    card_header(h4("User Interaction Network")),
                    card_body(
                      class = "flex-fill p-0",
                      div(
                        class = "small-network-container",
                        visNetworkOutput("time_user_network", height = "100%", width = "100%")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    ,
    
    # Global Perspective UI
    conditionalPanel(
      "input.viewPerspective == 'global'",
      div(
        id = "global-perspective",
        class = "container-fluid flex-fill d-flex flex-column p-0",
        style = "height: 100%; min-height: 0;",
        layout_sidebar(
          sidebar = sidebar(
            width = 300,
            class = "sidebar-panel",
            h3("Why People Change Their Mind"),
            p("This analysis shows patterns in successful persuasion on r/ChangeMyView."),
            h4("Key Insights"),
            uiOutput("key_insights_summary"),
            hr(),
            # Add chart descriptions section
            uiOutput("global_chart_descriptions")
          ),
          div(
            class = "container-fluid p-0",
            style = "overflow-y: auto; height: 100%;",
            
            # Delta Success Funnel (Chart 1)
            div(class = "row m-0 mb-3",
                div(class = "col-12 p-2",
                    card(
                      div(class = "card-header",
                          h4("Discussion Progression to Delta", class = "m-0")
                      ),
                      div(class = "card-body p-2",
                          div(style = "height: 350px;",
                              plotOutput("word_usage_analysis", height = "100%")
                          )
                      )
                    )
                )
            ),
            
            # Narrative Evolution Chart (Chart 2)
            div(class = "row m-0 mb-3",
                div(class = "col-12 p-2",
                    card(
                      div(class = "card-header",
                          h4("Narrative Evolution Patterns", class = "m-0")
                      ),
                      div(class = "card-body p-2",
                          div(style = "height: 350px;",
                              plotOutput("narrative_evolution_analysis", height = "100%")
                          )
                      )
                    )
                )
            ),
            
            # Argument Structure Analysis (Chart 3)
            # div(class = "row m-0 mb-3",
            #     div(class = "col-12 p-2",
            #         card(
            #           div(class = "card-header",
            #               h4("Argument Structure Analysis", class = "m-0")
            #           ),
            #           div(class = "card-body p-2",
            #               div(style = "height: 350px;",
            #                   plotOutput("argument_structure_analysis", height = "100%")
            #               )
            #           )
            #         )
            #     )
            # ),
            
            # Discussion Length Impact (Chart 4)
            div(class = "row m-0 mb-3",
                div(class = "col-12 p-2",
                    card(
                      div(class = "card-header",
                          h4("Discussion Length Impact", class = "m-0")
                      ),
                      div(class = "card-body p-2",
                          div(style = "height: 350px;",
                              plotOutput("discussion_metrics_analysis", height = "100%")
                          )
                      )
                    )
                )
            )
          )
        )
      )
    )
  )
)







# Argument Structure Tab
argumentStructureTab <- nav_panel(
  "Argument Structure",
  div(
    class = "container-fluid flex-fill d-flex flex-column p-0",
    style = "height:100%; min-height:0;",
    # Tab-level description
    div(
      class = "row m-0 mb-3",
      div(
        class = "col-12",
        div(
          class = "alert alert-info",
          style = "font-size: 1.1rem; margin-bottom: 0.5rem;",
          strong("Argument Structure Analysis: "),
          "Explore how arguments unfold in a selected Reddit discussion. Use the sidebar to choose which charts to display in the two cards."
        )
      )
    ),
    div(
      class = "row flex-fill m-0",
      style = "height:100%; min-height:0;",
      # Sidebar (left)
      div(
        class = "col-3 d-flex flex-column p-0",
        style = "height:100%; min-height:0;",
        card(
          class = "flex-fill d-flex flex-column sidebar-panel",
          style = "height:100%; min-height:0;",
          card_header(h3("Chart Selection")),
          card_body(
            class = "flex-fill overflow-auto",
            h4("Choose which charts to display:"),
            selectInput(
              "argument_chart1", "Selected Network:",
              choices = c(
                "Argument Component Balance" = "argument_components_radar",
                "Flow Between Components"    = "argument_flow_chord",
                "Appeal Types Distribution"  = "appeal_types_plot",
                "Temporal Evolution"         = "argument_temporal_evolution"
              ),
              selected = "argument_components_radar"
            ),
            # Description for chart 1
            uiOutput("argument_card1_desc"),
            hr(),
            selectInput(
              "argument_chart2", "Global Discussion:",
              choices = c(
                "Argument Component Balance" = "global_argument_components_radar",
                "Flow Between Components" = "global_argument_flow_chord",
                "Appeal Types Distribution" = "global_appeal_types_plot",
                "Temporal Evolution Mean" = "global_argument_temporal_evolution"
              ),
              selected = "appeal_types_plot"
            ),
            # Description for chart 2
            uiOutput("argument_card2_desc"),
          )
        )
      ),
      # Main content (right): stack both cards vertically, each 50% height, scrollable
      div(
        class = "col-9 d-flex flex-column p-0",
        style = "height:100%; min-height:0;",
        div(
          class = "d-flex flex-column gap-3",
          style = "height:100%; min-height:0;",
          # Card 1
          div(
            style = "flex: 1 1 0; min-height:0; max-height:50%;",
            uiOutput("argument_card1")
          ),
          # Card 2
          div(
            style = "flex: 1 1 0; min-height:0; max-height:50%;",
            uiOutput("argument_card2")
          )
        )
      )
    )
  )
)


## ────────────────────────────────────────────────────────────────────────────
## UI: Persuasion Analysis Tab
## ────────────────────────────────────────────────────────────────────────────
persuasionAnalysisTab <- nav_panel(
  "Persuasion Analysis",
  div(
    class = "container-fluid flex-fill d-flex flex-column p-0",
    style = "height:100%; min-height:0;",
    
    # Tab-level description
    div(
      class = "row m-0 mb-3",
      div(
        class = "col-12",
        div(
          class = "alert alert-info",
          style = "font-size: 1.1rem; margin-bottom: 0.5rem;",
          strong("Persuasion Analysis: "),
          "Explore how narrative evolution and different persuasion factors contribute to changing views. The visualizations analyze BERT-detected argument components, evidence usage, and emotional shifts throughout discussions."
        )
      )
    ),
    
    div(
      class = "row flex-fill m-0",
      style = "height:100%; min-height:0;",
      
      # Sidebar (left)
      div(
        class = "col-3 d-flex flex-column p-0",
        style = "height:100%; min-height:0;",
        card(
          class = "flex-fill d-flex flex-column sidebar-panel",
          style = "height:100%; min-height:0;",
          card_header(h3("Persuasion Metrics")),
          card_body(
            class = "flex-fill overflow-auto",
            h4("Choose which metrics to display:"),
            selectInput(
              "persuasion_chart1", "Selected Network:",
              choices = c(
                "Feature Effects on Δ Odds" = "persuasion_coef_plot",
                "Sentiment Shift Patterns" = "sentiment_shift_patterns",
                "Narrative Arc Components" = "narrative_arc",
                "Evidence Type Impact" = "evidence_impact"
              ),
              selected = "persuasion_coef_plot"
            ),
            # Description for chart 1
            uiOutput("persuasion_card1_desc"),
            hr(),
            selectInput(
              "persuasion_chart2", "Global Discussion:",
              choices = c(
                "Global Feature Effects on Δ Odds" = "persuasion_coef_plot",
                "Global Sentiment Shift Patterns" = "sentiment_shift_patterns",
                "Global Evidence Type Impact" = "evidence_impact"
              ),
              selected = "persuasion_effectiveness"
            ),
            # Description for chart 2
            uiOutput("persuasion_card2_desc"),
          )
        )
      ),
      
      # Main content (right): stack both cards vertically, each 50% height, scrollable
      div(
        class = "col-9 d-flex flex-column p-0",
        style = "height:100%; min-height:0;",
        div(
          class = "d-flex flex-column gap-3",
          style = "height:100%; min-height:0;",
          
          # Card 1
          div(
            style = "flex: 1 1 0; min-height:0; max-height:50%;",
            uiOutput("persuasion_card1")
          ),
          
          # Card 2
          div(
            style = "flex: 1 1 0; min-height:0; max-height:50%;",
            uiOutput("persuasion_card2")
          )
        )
      )
    )
  )
)


### 2) navset_tab aktualisieren
ui <- fluidPage(
  theme = custom_theme,
  useShinyjs(),
  tags$head(custom_css, custom_js),
  div(
    id = "app",
    appHeader,
    div(
      class = "main-content",
      navset_tab(
        id = "tabsetpanel",
        homeTab,
        viewingViewsTab,        # statt Posting/User/Time
        argumentStructureTab,
        persuasionAnalysisTab
      )
    )
  )
)

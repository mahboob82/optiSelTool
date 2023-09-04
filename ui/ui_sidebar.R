appSidebar <- dashboardSidebar(
  width="300px",
  sidebarMenu(
    menuItem(tabName="Workspace", text="Workspace",  icon=icon("home")),
    div(style="padding-left:20px;",
        hr(),
        fileInput("RawPedUpload", "Upload Pedigree file:",accept=c(".csv", ".txt", ".rds")),
        fileInput("RawPhenUpload", "Upload Phenotype file:",accept=c(".csv", ".txt", ".rds")),
        actionButton("clearDB", "Clear database", width='100px', class='floating-button'),
    ),
  
    menuItem(tabName="BreedingPlan", text="Breeding Plans", icon=icon("th")),
    div(
        hr(),
        style="padding-left:20px",
        actionButton("btnPlanA", "Plan A", width='100px', class='floating-button'),
        actionButton("btnPlanB", "Plan B", width='100px', class='floating-button'),
        actionButton("btnPlanC", "Plan C", width='100px', class='floating-button')
    ),

    hr(),
    div(
      style="padding-left:20px",
      h4("OPTISEL official doc."),
      actionButton(inputId='PedOCSdoc',label="1. Pedigree OCS",icon=icon("book"), onclick=PedOCSdoc)),
    div(style="padding-left:20px",
      actionButton(inputId='MarkerOCSdoc',label="2. Marker OCS",icon=icon("book"), onclick=MarkerOCSdoc)),
    div(style="padding-left:20px",
        actionButton(inputId='OCSdoc',label="3. OCS",icon=icon("book"), onclick=OCSdoc))


  # actionButton(inputId='ab2', label='Upload'),
  # actionButton(
  #   inputId='Work',
  #   label="Learn More",
  #   icon=icon("th"),
  #   onclick=helplink
  # )
  ))



# grid2cols = "
# display:grid; 
# grid-template-columns: 50% 50%; 
# grid-gap: 0px; 
# background-color=#ccc; 
# padding: 0px;
# margin: 0px;
# text-align: left;
# "

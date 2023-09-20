
fluidRow(
  column(
    width=5,
    h1("Step 1"),
    h2("OCS with pedigree and phenotypes only- Plan 1"),
    h4("Pedigree file options:"),
    h4("  1. Use all individuals"),
    h4("  2. Use animals within year range: [inputbox1] [inputbox2]"),
    h4("  3. Breed name: [input box]"),
    h4("  4. Treat animals as natives if born before: [year input]"),
    h4("  5. Add numeric columns to Preped object: [year input]"),
    h2("Phenotype file options:"),
    h4("  * Use phenotypes indiv only <yes/no>"),
    h2("Calculate Breed composition"),
    h4("  yes/no?"),
    h2("Work flow")
  ),
  column(
    width=7,
  imageOutput("ocsPedigree1")

  )
)
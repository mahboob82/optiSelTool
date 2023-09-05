
renderDT15 <- function(df, row=15){
  renderDT(
    df,
    filter = 'top',
    options = list(
      scrollX = TRUE,
      paging = TRUE,
      pageLength = row,
      lengthMenu = ShowEntryRowsList,
      #dom = 'Blfrtip' # 'tip'
      dom='Blfrtip'
    )
  )
}

renderDTminimal <- function(df, row=15){
  renderDT(
    df,
    filter = 'top',
    options = list(
      scrollX = TRUE,
      paging = TRUE,
      pageLength = row,
      lengthMenu = ShowEntryRowsList,
      dom = 'tp' # 'tip'
    )
  )
}

renderDT15special <- function(df, row=15){
  renderDataTable(
    df,
    filter = 'top',
    options = list(
      searchCols = list(NULL, NULL, NULL,NULL, NULL, NULL,NULL), 
      # scrollX = TRUE,
      # paging = TRUE,
      # pageLength = row,
      # lengthMenu = ShowEntryRowsList,
      # dom = 't', # 'tip',Blfrtip
      initComplete = JS(
          "function(settings, json) {",
          "  var table = this.api();",
          "  table.columns().every(function() {",
          "    var that = this;",
          "    $('input', this.header()).on('keyup', function() {",
          "      if (that.search() !== this.value) {",
          "        that.search(this.value).draw();",
          "      }",
          "    });",
          "  });",
          "}"
      ),
      #dom = 'Blfrtip',
      rowCallback = JS(
        "function(row, data, index) {",
        "  var cells = $('td', row);",
        "  cells.eq(0).addClass('highlight');",  # Highlight the first column
        "}"
      ),
      class = "display cell-border stripe"
    )
  )
}



renderDT15_Pkin_dblClick <- function(df, rows=15){
  renderDT(
    df,
    filter = 'top',
    options = list(
      scrollX = TRUE,
      paging = TRUE,
      pageLength = rows,
      lengthMenu = ShowEntryRowsList,
      dom = 'Blfrtip' # 'tip'
    ),
    callback = htmlwidgets::JS(
      "table.on('dblclick', 'td',",
      "  function() {",
      "    var row = table.row(this).index();",
      "    var col = table.column(this).index();",
      "    var info = table.page.info()",
      "    var pagelen = table.page.len()",
      "    Shiny.setInputValue('Pkin_dt_dblclick', {dt_row: row+1, dt_col: col+1, dt_page: info.page, dt_len: pagelen} );",
      "  }",
      ");"
    )
  )
}

renderDT15_PkinatN_dblClick <- function(df, rows=15){
  renderDT(
    df,
    filter = 'top',
    options = list(
      scrollX = TRUE,
      paging = TRUE,
      pageLength = rows,
      lengthMenu = ShowEntryRowsList,
      dom = 'Blfrtip' # 'tip'
    ),
    callback = htmlwidgets::JS(
      "table.on('dblclick', 'td',",
      "  function() {",
      "    var row = table.row(this).index();",
      "    var col = table.column(this).index();",
      "    var info = table.page.info()",
      "    var pagelen = table.page.len()",
      "    Shiny.setInputValue('PkinatN_dt_dblclick', {dt_row: row+1, dt_col: col+1, dt_page: info.page, dt_len: pagelen} );",
      "  }",
      ");"
    )
  )
}


# 
# renderDTPkinatNdblClick <- function(df, rows=15){
#   renderDT(
#     df,
#     filter = 'top',
#     options = list(
#       scrollX = TRUE,
#       paging = TRUE,
#       pageLength = rows,
#       lengthMenu = ShowEntryRowsList,
#       dom = 'Blfrtip' # 'tip'
#     ),
#     callback = htmlwidgets::JS(
#       "table.on('dblclick', 'td',",
#       "  function() {",
#       "    var tbl_name = 'PKinatN;'",
#       "    var row = table.row(this).index();",
#       "    var col = table.column(this).index();",
#       "    var info = table.page.info()",
#       "    var pagelen = table.page.len()",
#       "    Shiny.setInputValue('dt_dblclick', {dt_tbl_name: tbl_name, dt_row: row+1, dt_col: col+1, dt_page: info.page, dt_len: pagelen} );",
#       "  }",
#       ");"
#     )
#   )
# }

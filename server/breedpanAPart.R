 observe({
        #if (is.null("docs/OCS-types-1.png")) return()
        # myimg <- paste0("img/OCS-types-", seq_len(2), ".png")
        output$ocsPedigree1 <- renderImage({
          list(src = "./img/OCS-types-1.png", contentType = 'image/png',width = 800, height = 600,
               alt = "This is alternate text")
        }, deleteFile = FALSE)
      })
 
 
# Importing libraries.
  library("shiny")
  library("queueing")

  # Shiny server that receives input, output and session values.
    shinyServer(function(input,output,session){

      # Listening to the click event where "$click" is the id of the button.
      observeEvent(input$click,{

        # Values that arrive from Javascript. You can see them through the server console.
          print(input$click)

        # Values from the mm1k model:  λ, μ, severs and k.
          
          lambda <- input$click$lambda
          mean <- input$click$mean
          n<- input$click$n

        # I send the values to build the model.
          i_mm1 <- NewInput.MM1(lambda = lambda, mu = mean, n = n)
        # I check that the conditions are met in order to use this model ✔.
          CheckInput(i_mm1)
        # building the model ⚒.
          o_mm1 <- QueueingModel(i_mm1)
        # Extracting the values.
          # [Values that will be sent to Javascript]
            # The probability (p0, p1, ..., pk) of the clients in the system.
              pn <- Pn(o_mm1)
            # The traffic intensity.
              traffic_intensity <- (L(o_mm1) - Lq(o_mm1))
            # The server use.
              server_use <- RO(o_mm1)
            # The mean number of clients in the system.
              mnc_system <- L(o_mm1)
            # The mean number of clients in the queue.
              mnc_queue <- Lq(o_mm1)
            # The mean number of clients in the server.
              mnc_server <- (L(o_mm1) - Lq(o_mm1))
            # The mean time spend in the system.
              mts_system <- W(o_mm1)
            # The mean time spend in the queue.
              mts_queue <- Wq(o_mm1)
            # The mean time spend in the server.
              mts_server <- W(o_mm1) - Wq(o_mm1)
            # The mean time spend in the queue when there is queue.
              mtsq_queue <- Wqq(o_mm1)
            # The throughput.
              throughput <- Throughput(o_mm1)
            # probability that the system is unused.
              lazy <- (1 - RO(o_mm1))
            #The sum of the proababilities
              probas = 0
              for (i in Pn(o_mm1)) {
                 probas = probas + i
              }
              probs <- probas
            #Probability of having more than n clients  
              complement <- 1-probas

          # I create a list that will save all the previous values ​​to send them to Javascript and it receives them as a vector.
          # Unfortunately I didn't know how to send it as an associative vector.
            info <- list(pn, traffic_intensity,server_use, mnc_system, mnc_queue, mnc_server, mts_system, mts_queue, mts_server, mtsq_queue, throughput, lazy,probs,complement)

          # I send the values to Javascript.
          session$sendCustomMessage("handler_stable_state_chart", info)
      })
    })

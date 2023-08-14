
# Set theme
tcTheme()

# Define plot values
.GlobalEnv$exponent<-5
.GlobalEnv$data_frame<-data.frame(x=c(1:10),
                            y=NA)

# Create a function to render a plot
.GlobalEnv$render_plot<-function(exponent){
  # Create a plot
  data_frame$y<-data_frame$x^exponent
  .GlobalEnv$testplot<-ggplot(data_frame,aes(x=x,y=y))+
    geom_point()+
    geom_line()
}
render_plot(exponent)

#Define widgets
frame<-tcFrame("app",
               tcGrid(1,1,pady=5,padx=30),
               "frame1")

# Define scrollbar function
.GlobalEnv$get_value<-function(...){

  .GlobalEnv$exponent<-as.numeric(list(...))

  # Update the plot data
  render_plot(exponent)

  #Modify the UI
  tcModify("plot1",
           widget_list,
           ggplot="testplot")

}

# Define scrollbar
scroll<-tcScrollbar("frame1",
                    tcGrid(1,1),
                    "scroll1",
                    from=1,
                    to=10,
                    resolution=0.1,
                    variable="5",
                    command="get_value",
                    height=250,
                    color="red",
                    weight="bold",
                    orient="horizontal",
                    showvalue=TRUE,
                    label="y=x^(m)")

dispplot<-tcPlot("frame1",
                 tcGrid(1,2),
                 "plot1",
                 ggplot="testplot")

widget_list<-list()
widget_list[[1]]<-frame
widget_list[[2]]<-scroll
widget_list[[3]]<-dispplot

test_that("test tcScrollbar",{
  expect_no_error(
    tcBuild("app",
            widget_list,
            geometry="320x340+400+100",self_destruct = TRUE)
  )
})



#Set theme
tcTheme("DEFAULT")

#Define exponent variable
.GlobalEnv$exponent<-1

# Create a plot
.GlobalEnv$generate_plot<-function(){

  x_vals<-1:100
  y_vals<-(x_vals)^exponent
  plot_data<-data.frame(x=x_vals,
                        y=y_vals)

  .GlobalEnv$gplot<-ggplot(plot_data,aes(x=x,y=y))+
    geom_point()

}
generate_plot()

# Add 1 to exponent
.GlobalEnv$add_exp<-function(){

  # Add 1 to exponent
  .GlobalEnv$exponent<-exponent+1

  # Overwrite the current plot
  generate_plot()

  # Display the new plot
  update_ui()
}

# Subtract 1 from exponent
.GlobalEnv$sub_exp<-function(){

  # Subtract from the exponent
  .GlobalEnv$exponent<-exponent-1

  # Overwrite the current plot
  generate_plot()

  # Display the new plot
  update_ui()

}

# Update the UI
.GlobalEnv$update_ui<-function(){

  # Update entry display
  tcConfigure("entry1",
              textvariable=tclVar(exponent))

  # Update plot display
  tcModify(widget_name="aplot",
           widget_list=widget_list,
           ggplot="gplot")

}

#Create widget list
widget_list<-list()
widget_list[[1]]<-tcButton(parent="plotdisplay",
                           position=tcGrid(3,1,padx=10),
                           name="add1",
                           text="[+]",
                           command="add_exp")
widget_list[[2]]<-tcEntry(parent="plotdisplay",
                          position=tcGrid(2,1,pady=10),
                          name="entry1",
                          text=exponent)
widget_list[[3]]<-tcButton(parent="plotdisplay",
                          position=tcGrid(1,1,padx=10),
                          name="sub1",
                          text="[-]",
                          command="sub_exp")
widget_list[[4]]<-tcPlot(parent="plotdisplay",
                         position=tcGrid(1,2,xspan=3),
                         name="aplot",
                         ggplot="gplot")

test_that(
  "test plot display",
  {expect_no_error(
    tcBuild(ui_name="plotdisplay",
            widget_list,self_destruct = TRUE)
  )}
)



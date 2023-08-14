
# Set the theme
tcTheme()

# Create a function and button widget that will replace label1 with label2
# Will work in an R script without saving to the global environment
.GlobalEnv$change_the_label<-function(){

  # Label 2 must be saved to the global environment
  .GlobalEnv$label_2<-tcLabel(parent="test",
                              position=tcGrid(1,1),
                              name="label2",
                              text="Goodbye World!")

  # Replace the label!
  tcReplace(old_widget_name="label1",
            widget_list=widget_list,
            new_widget_object=label_2)

}

# Define widgets to include in the UI
label_1<-tcLabel(parent="test",
                 position=tcGrid(1,1),
                 name="label1",
                 text="Hello World!")

button_1<-tcButton(parent="test",
                   position=tcGrid(1,2,padx=10,pady=10),
                   name="button1",
                   text="Change the label!",
                   command="change_the_label")

# Add label1 and button to the widget list
widget_list<-list()
widget_list[[1]]<-label_1
widget_list[[2]]<-button_1

# Build the UI
test_that("test tcReplace",{

  expect_no_error(
    tcBuild(ui_name="test",
            widget_list=widget_list,self_destruct = TRUE)
  )

})

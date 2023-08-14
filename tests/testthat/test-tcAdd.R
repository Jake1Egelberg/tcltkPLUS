
# Set theme
tcTheme()

# Define function to print hello
.GlobalEnv$print_hi<-function(){
  tk_messageBox(message='Hi!')
}

# Define function to add a new button
.GlobalEnv$add_button<-function(){

  new_button<-tcButton("app",
                       tcGrid(1,2,padx=10,pady=10),
                       name="button1",
                       text="Im a second button!",
                       command="print_hi")

  tcAdd(new_button,widget_list)

}

# Create the original button widget
button<-tcButton(parent="app",
                 position=tcGrid(1,1,padx=20,10,sticky="w"),
                 name="button1",
                 text="Im a button",
                 command="add_button")

# Create the widget list
widget_list<-list()
widget_list[[1]]<-button

# Build the UI
test_that("test tcAdd",{
  expect_no_error(
    tcBuild("app",
            widget_list,geometry = "150x100",self_destruct = TRUE)
  )
})



# Set the theme
tcTheme()

# Create a function to kill the app
.GlobalEnv$kill_function<-function(){
  tcDestroy("app")
}

# Create a button
button<-tcButton("app",
                 tcGrid(1,1,padx=20),
                 "button1",
                 text="Kill the app!",
                 command="kill_function")

widget_list<-list()
widget_list[[1]]<-button

test_that("test tcDestroy",{
  expect_no_error(
    tcBuild("app",
            widget_list,
            "+400+100",self_destruct = TRUE)
  )
})

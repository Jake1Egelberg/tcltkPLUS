
# Set theme
tcTheme()

# Modify with tcConfigure
.GlobalEnv$tc_config<-function(){

  tcConfigure("label1",
              text="Now try tcModify again!")

}

# Modify with tcModify
.GlobalEnv$tc_mod<-function(){

  tcModify("label1",
           widget_list,
           text="Now try tcConfigure!")

}

# Create widget list
widget_list<-list()
widget_list[[1]]<-tcLabel("app",
                          tcGrid(1,1,pady=15,padx=20),
                          "label1",
                          text="Modify me with tcModify!")
widget_list[[2]]<-tcButton("app",
                          tcGrid(1,3,pady=15),
                          "button1",
                          text="Modify with tcConfigure",
                          command="tc_config")
widget_list[[3]]<-tcButton("app",
                           tcGrid(1,2,pady=15),
                           "button2",
                           text="Modify with tcModify",
                           command="tc_mod")

# Build the UI
test_that("test tcConfigure",{
  expect_no_error(
    tcBuild("app",
            widget_list,self_destruct = TRUE)
  )
})



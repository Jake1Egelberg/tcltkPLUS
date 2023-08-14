# Set theme
tcTheme()

.GlobalEnv$say_hi<-function(){
  tk_messageBox(message="Hello!")
}

widget_list<-list()
widget_list[[1]]<-tcButton("app",
                           tcGrid(1,1,padx=30),
                           "button",
                           text="Click me!",
                           command="say_hi")

test_that("test tcButton",
          {
            expect_no_error(
              tcBuild("app",
                      widget_list,
                      "+400+100",self_destruct = TRUE)
            )
          })

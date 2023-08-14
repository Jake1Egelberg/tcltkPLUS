# Set theme
tcTheme()

.GlobalEnv$bound_function<-function(){
  tk_messageBox(message="Your curser has entered the blue box!")
}

widget_list<-list()
widget_list[[1]]<-tcFrame("app",
                          tcGrid(1,1),
                          "frame1",
                          border=tcBorder(5,"red"),
                          height=300,
                          width=300)
widget_list[[2]]<-tcFrame("frame1",
                          tcGrid(1,1,padx=100,pady=100),
                          "frame2",
                          border=tcBorder(5,"blue"),
                          height=100,
                          width=100,
                          binding=tcBind("<Enter>","bound_function"))
widget_list[[3]]<-tcLabel("frame2",
                          tcGrid(1,1,pady=30),
                          "label",
                          text="Hover over me!",
                          color="blue")

test_that("test tcBind",{
  expect_no_error(
    tcBuild("app",
            widget_list,
            "+400+100",self_destruct = TRUE)
  )
})

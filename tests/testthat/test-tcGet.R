
#Set the theme
tcTheme("DEFAULT")

# Use a user input to generate a plot
.GlobalEnv$update_label<-function(){

  # Get entry data
  input<-tcGet("entry1",widget_list)

  #Randomize size and color
  size<-sample(seq(10,20,2),1)
  color<-sample(c("blue","green","yellow4","firebrick"),1)

  # Update the GUI
  tcConfigure("label2",
              text=input,
              fg=color,
              font=tkfont.create(size=size,weight="bold",family="Comic Sans MS"))

}

# Create widget list
widget_list<-list()
widget_list[[1]]<-tcFrame(parent="test",
                          position=tcGrid(1,1,padx=10),
                          name="frame1")
widget_list[[2]]<-tcLabel(parent="frame1",
                          position=tcGrid(1,1,pady=10),
                          name="label1",
                          text="Write something!",
                          size=15,
                          weight="bold")
widget_list[[3]]<-tcEntry(parent="frame1",
                          position=tcGrid(1,2),
                          name="entry1",
                          width=20,
                          justify="center",
                          binding=tcBind("<Return>","update_label"))
widget_list[[4]]<-tcLabel(parent="frame1",
                          position=tcGrid(1,3),
                          name="label3",
                          text="(then press enter)")
widget_list[[5]]<-tcLabel(parent="frame1",
                          position=tcGrid(1,4,pady=10),
                          name="label2",
                          text="")

# Build the UI
test_that("check tcGet",{
  expect_no_error({
    tcBuild("test",
            widget_list,"200x160",self_destruct = TRUE)
  })
})


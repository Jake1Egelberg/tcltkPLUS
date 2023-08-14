
# Set theme
tcTheme("DEFAULT")

# Create function to modify label
.GlobalEnv$modify_label<-function(){

  tcModify("label1",
           widget_list,
           text="new text!")

  tcModify("listbox1",
           widget_list,
           values=c("hi","bye"))

  tcModify("listbox1",
           widget_list,
           weight="bold")

  tcModify("combo1",
            widget_list,
            values=c("my","name", "is","jake"))

  tcModify("combo1",
            widget_list,
            selected="my")

  tcModify("combo1",
           widget_list,
           weight="bold")

}

# Create widgets
frame<-tcFrame("app",
               tcGrid(1,1,padx=20,pady=10),
               "frame1")
label<-tcLabel("frame1",
               tcGrid(1,1),
               "label1",
               text="Im a label!")
button<-tcButton("frame1",
                 tcGrid(1,2,sticky="w"),
                 "button1",
                 text="Click me!",
                 command="modify_label")
listbox<-tcListbox("frame1",
                   tcGrid(2,1,yspan=2,padx=10),
                   name="listbox1",
                   values=c("Value 1",
                            "Value 2"))
combobox<-tcCombobox("frame1",
                     tcGrid(3,1,yspan=2),
                     name="combo1",
                     values=c("test 1", "test 2"),
                     selected="test 1")

widget_list<-list()
widget_list[[1]]<-frame
widget_list[[2]]<-label
widget_list[[3]]<-button
widget_list[[4]]<-listbox
widget_list[[5]]<-combobox


test_that("test tcModify",{
  expect_no_error(
    tcBuild("app",
            widget_list,
            "350x120",self_destruct = TRUE)
  )
})

# Set theme
tcTheme()

#Get checkbox input
.GlobalEnv$get_check<-function(){

  # Subtract from 1 to get the new value, not the value prior to selecting the checkbox
  .GlobalEnv$checked<-1-as.numeric(tcGet("check",widget_list))

  if(checked==1){
    label<-tcLabel("frame",
                   tcGrid(1,2,pady=10),
                   "label",
                   "Im checked!")
    tcAdd(label,widget_list)
  } else{
    tcDestroy("label")
  }

}

widget_list<-list()
widget_list[[1]]<-tcFrame("app",
                          tcGrid(1,1,padx=15,pady=10),
                          "frame")
widget_list[[2]]<-tcCheckbox("frame",
                             tcGrid(1,1),
                             "check",
                             text="Check me!",
                             value=FALSE,
                             binding=tcBind("<Button>","get_check"))

test_that("test tcCheck",{
  expect_no_error(
    tcBuild("app",
            widget_list,
            "100x100+400+100",self_destruct = TRUE)
  )
})

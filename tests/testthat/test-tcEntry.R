# Set theme
tcTheme()

# Define function to reset
.GlobalEnv$reset_fun<-function(){
  lapply(1:entered_number, function(x){
    tcDestroy(paste("label",x,sep=""))
  })
  tryCatch(tcDestroy("resetbutton"),error=function(e){NULL})
}

# Define function when enter is pressed
.GlobalEnv$enter_fun<-function(){

  .GlobalEnv$entered_number<-as.numeric(tcGet("entry",widget_list))

  lapply(1:entered_number,function(x){

    new_entry<-tcLabel("frame",
                       tcGrid(1,2+x,pady=1),
                       paste("label",x,sep=""),
                       text=x,
                       size=x)
    tcAdd(new_entry,
          widget_list)


  })

  tryCatch(tcDestroy("resetbutton"),error=function(e){NULL})

  reset_button<-tcButton("frame",
                         tcGrid(1,3+entered_number,pady=5),
                         "resetbutton",
                         text="Reset",
                         command="reset_fun")
  tcAdd(reset_button,
        widget_list)

}

#Create a frame
frame<-tcFrame("app",
               tcGrid(1,1,padx=10,pady=10),
               "frame")

# Create a label
label<-tcLabel("frame",
               tcGrid(1,1),
               "label",
               text="Enter a number!")

# Create the entry
entry<-tcEntry("frame",
               tcGrid(1,2),
               "entry",
               binding=tcBind("<Return>","enter_fun"))

widget_list<-list()
widget_list[[1]]<-frame
widget_list[[2]]<-label
widget_list[[3]]<-entry

test_that("tst tcEntry",{
  expect_no_error(
    tcBuild("app",widget_list,"+500+100",self_destruct = TRUE)
  )
})

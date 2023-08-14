
# Set theme
tcTheme("DEFAULT")

# Define values to work with
.GlobalEnv$values<-c(5,3)

# Create a function to do math
.GlobalEnv$update_math<-function(){

  .GlobalEnv$selected_fun<-tcGet("combo1",widget_list)
  .GlobalEnv$val1<-as.numeric(tcGet("value1",widget_list))
  .GlobalEnv$val2<-as.numeric(tcGet("value2",widget_list))

  if(selected_fun=="Addition"){

    newresult<-val1+val2

  } else if(selected_fun=="Subtraction"){

    newresult<-val1-val2


  } else if(selected_fun=="Multiplication"){

    newresult<-val1*val2

  } else if(selected_fun=="Division"){

    newresult<-round(val1/val2,3)

  } else{
    newresult<-""
  }

  tcConfigure("result1",
              text=paste("= ",newresult,sep=""))


}

# Create widgets
frame<-tcFrame("app",
               tcGrid(1,1,padx=20,pady=10),
               "frame1")
label<-tcLabel("frame1",
               tcGrid(1,1,padx=0,pady=10,sticky="w"),
               "label1",
               text="Given")
entry1<-tcEntry("frame1",
                tcGrid(2,1,sticky="w"),
                "value1",
                text="5",
                width=3,
                binding=tcBind("<Return>","update_math"))
label2<-tcLabel("frame1",
               tcGrid(2,1,padx=30,sticky="w"),
               "label2",
               text="&")
entry2<-tcEntry("frame1",
                tcGrid(2,1,sticky="e"),
                "value2",
                text="3",
                width=3,
                binding=tcBind("<Return>","update_math"))
combobox<-tcCombobox("frame1",
                     tcGrid(1,2,xspan=3),
                     name="combo1",
                     values=c("Addition", "Subtraction","Multiplication","Division"),
                     selected="Select what to do!",
                     width=15,
                     binding=tcBind("<<ComboboxSelected>>","update_math"))
result<-tcLabel("frame1",
                tcGrid(1,3,xspan=3,pady=10),
                name="result1",
                text="...")

widget_list<-list()
widget_list[[1]]<-frame
widget_list[[2]]<-label
widget_list[[3]]<-combobox
widget_list[[4]]<-result
widget_list[[5]]<-entry1
widget_list[[6]]<-label2
widget_list[[7]]<-entry2

test_that("test tcCombobox",{
  expect_no_error(
    tcBuild("app",
            widget_list,
            "170x140+400+100",self_destruct = TRUE)
  )
})

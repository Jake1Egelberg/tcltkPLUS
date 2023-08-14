# Set theme
tcTheme()

.GlobalEnv$border_width<-5
.GlobalEnv$iterations<-19

.GlobalEnv$generate_image<-function(){

  .GlobalEnv$border_width<-as.numeric(tcGet("entry1",parent_list))

  #Create frames with different borders
  widget_list<-lapply(1:iterations,function(x){

    if(x==1){
      parent<-"app"
    } else{
      parent<-paste("frame",(x-1),sep="")
    }

    frame<-tcFrame(parent=parent,
                   tcGrid(1,1),
                   paste("frame",x,sep=""),
                   width=200-(x*10),
                   height=200-(x*10),
                   border=tcBorder(border_width,colors()[x]),
                   background="black")

  })

  tcBuild("app",widget_list,"+400+100")
}

.GlobalEnv$set_iter_num<-function(...){

  .GlobalEnv$iterations<-as.numeric(list(...))

}

parent_list<-list()
parent_list[[1]]<-tcScrollbar("control",
                              tcGrid(1,1,padx=10),
                              "scroll",
                              from=2,to=19,
                              resolution=1,
                              command="set_iter_num",
                              variable=iterations,
                              orient="horizontal",
                              label=" Iteration Number",
                              height=130,
                              weight="bold")
parent_list[[2]]<-tcLabel("control",
                          tcGrid(1,2),
                          "label1",
                          text="Border Width",
                          weight="bold")
parent_list[[3]]<-tcEntry("control",
                          tcGrid(1,3),
                          "entry1",
                          text=border_width,
                          width=10,
                          binding=tcBind("<Return>","generate_image"))
parent_list[[4]]<-tcButton("control",
                           tcGrid(1,4,pady=10),
                           "button1",
                           text="RENDER",
                           command="generate_image")

test_that("test tcBorder",{
  expect_no_error(
    tcBuild("control",
            parent_list,
            "150x160+400+100",self_destruct = TRUE)
  )
})

#Run the app

.GlobalEnv$getlistdata<-function(){
  print(tcGet("testlist",widget_list))
}
.GlobalEnv$modify_test<-function(){

  tcModify(widget_name = "plot1",
           widget_list=widget_list,
           ggplot="anewplot")


}

tcTheme("RETRO")

frame<-tcFrame(parent="test",
               tcGrid(1,1,0,20),
               "frame1")
label<-tcLabel(parent="frame1",
               name="label1",
               position=tcGrid(1,1,0,0),
               text="testing",family="Comic Sans MS")
button<-tcButton(parent="frame1",
                 name="testbutton",
                 position=tcGrid(1,2,0,10),
                 text="yay",
                 command="modify_test")
check<-tcCheckbox(parent="frame1",
                  position=tcGrid(1,3,0,0),
                  name="testcheck",
                  text="check me!")
list<-tcListbox(parent="frame1",
                position=tcGrid(1,4,40,10),
                name="testlist",
                values=c("testing1","testing2","testing3","testing4"),
                binding=tcBind("<<ListboxSelect>>","getlistdata"),
                weight="bold",
                family="Comic Sans MS",
                add_scrollbar=TRUE)
entry<-tcEntry(parent="frame1",
               position=tcGrid(2,1,10,10),
               name="entry1",
               text="entry test")
combo<-tcCombobox(parent="frame1",
                  position=tcGrid(2,2,10,10,sticky="w"),
                  name="combo1",
                  values=c("hi","bye"),
                  selected="hi")

.GlobalEnv$testplot<-ggplot(data=data.frame(x=1:10,y=1:10),aes(x=x,y=y))+
  geom_point()
.GlobalEnv$anewplot<-ggplot(data=data.frame(x=1:10,y=(1:10)^3),aes(x=x,y=y))+
  geom_point()

tkplot<-tcPlot(parent="frame1",
               position=tcGrid(2,3,0,0,yspan=10,xspan=1),
               name="plot1",
               ggplot="testplot",own_frame=TRUE)

widget_list<-list()
widget_list[[1]]<-frame
widget_list[[2]]<-label
widget_list[[3]]<-button
widget_list[[4]]<-check
widget_list[[5]]<-list
widget_list[[6]]<-entry
widget_list[[7]]<-combo
widget_list[[8]]<-tkplot

test_that("tcBuild",
          {
            expect_no_error(
              tcBuild("test",widget_list,"600x550+500+50",self_destruct = TRUE)
            )
          })



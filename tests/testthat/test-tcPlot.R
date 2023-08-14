
tcTheme("DEFAULT")

.GlobalEnv$testplot<-ggplot(data=data.frame(x=1:100,y=1:100),aes(x=x,y=y))+
  geom_point()

frame<-tcFrame(parent="test",
               tcGrid(1,1,0,0),
               "frame1")
tkplot<-tcPlot(parent="test",
               position=tcGrid(1,1),
               name="plot1",
               ggplot="testplot",
               hscale=2,
               vscale=1.6,
               own_frame=TRUE)

widget_list<-list()
widget_list[[1]]<-tkplot

#Run test
test_that("check tcPlot",{
  expect_no_error(
    tcBuild("test",
            widget_list,self_destruct = TRUE)
  )
})











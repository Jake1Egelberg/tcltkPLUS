\name{tcApp}
\alias{tcApp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert a widget list to an externally launchable app
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcApp() to create launchable applications from your R scrips. tcApp() will save all of the objects in your R environment to an app directory, the R script containing your app's functions, and a .bat file that will echo the R script off of an installed version of R. This allows apps to be launched externally, outside of R or R Studio.
}
\usage{
tcApp(
  app_name,
  ui_name,
  widget_list,
  geometry,
  app_directory,
  r_directory,
  run_app,
  prompts
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{app_name}{
%%     ~~Describe \code{x} here~~
A string that defines the app name.
} \item{ui_name}{
A string that defines the UI name.
}
\item{widget_list}{
The widget list to create the primary UI from.
}
\item{geometry}{
(optional) A geometry string such as "50x60+70+80", where 50 is width, 60 is height, 70 is x-offset, and 80 is y-offset, all in units of pixel.
}
\item{app_directory}{
(optional) A string specifying the parent directory for your app. This function will create a new subdirectory for your app within it. If not specified as an argument, you will be prompted to select it. This is a folder, not a file.
}
\item{r_directory}{
(optional) A string specifying the R.exe file that will be used to run your app. If not specified as an argument, you will be prompted to select it. This is a file, not a folder.
}
\item{run_app}{
(optional) A boolean that determines if your app will be automatically launched. Defaults to FALSE.
}
\item{prompts}{
(optional) A boolean that determines if the user will be prompted during app creation. If an app directory or R .exe are not specified as arguments, the user will be prompted regardless of this value.
}
}
\examples{
# Load only necessary packages
library(tcltkPLUS)

# Remove excess objects from R environment
rm(list = ls())

# Set the theme
tcTheme()

# Get a seed
time_stamp<-paste(unlist(str_extract_all(Sys.time(),"[:digit:]")),collapse="")
.GlobalEnv$current_seed<-as.numeric(str_sub(time_stamp,start=-5))

# Create the count number, remembers how many rounds the user is on
.GlobalEnv$count_num<-1

# Create a vector to store user-selected frames
.GlobalEnv$selects<-c()

# Create a boolean to activate flashing new frames
.GlobalEnv$flash_status<-FALSE

# Create a dataframe matching each frame widget to a color
.GlobalEnv$frames<-data.frame(Frame=c("frame1","frame2","frame3","frame4"),
                              Color=c("red","blue","yellow","green"))

# Store which dificulty mode the user is on
.GlobalEnv$modeval<-"1"

# A function to adjust difficult with user input
.GlobalEnv$get_mode<-function(...){
  .GlobalEnv$modeval<-unlist(list(...))
}

# Function to create a dynamic widget list with alternating bind events
.GlobalEnv$define_list<-function(binding){

  if(binding=="flash"){
    event="<Enter>"
  } else if(binding=="click"){
    event="<Button>"
  }

  # Create a frame
  frame1<-tcFrame("Rsimon",
                  tcGrid(1,3),
                  "frame1",
                  background="red",
                  border=tcBorder(1,"black"),
                  width=100,
                  height=100,
                  binding=tcBind(event,ifelse(binding=="flash","flash_frame","getinred")))
  # Create a frame
  frame2<-tcFrame("Rsimon",
                  tcGrid(2,3),
                  "frame2",
                  background="blue",
                  border=tcBorder(1,"black"),
                  width=100,
                  height=100,
                  binding=tcBind(event,ifelse(binding=="flash","flash_frame","getinblue")))

  # Create a frame
  frame3<-tcFrame("Rsimon",
                  tcGrid(1,4),
                  "frame3",
                  background="yellow",
                  border=tcBorder(1,"black"),
                  width=100,
                  height=100,
                  binding=tcBind(event,ifelse(binding=="flash","flash_frame","getinyellow")))

  # Create a frame
  frame4<-tcFrame("Rsimon",
                  tcGrid(2,4),
                  "frame4",
                  background="green",
                  border=tcBorder(1,"black"),
                  width=100,
                  height=100,
                  binding=tcBind(event,ifelse(binding=="flash","flash_frame","getingreen")))

  # Create the scrollbar to set your mode/speed
  mode<-tcScrollbar("Rsimon",
                    tcGrid(1,2,xspan=1),
                    "modescroll",
                    from=1,to=3,resolution=1,variable=modeval,command="get_mode",
                    orient="horizontal",label="Difficulty",weight="bold",height=80)

  # Display which round the user is on
  status<-tcLabel("Rsimon",
                  tcGrid(2,2),
                  "statusdisplay",
                  text=paste("n = ",count_num,sep=""),
                  weight="bold")

  # Display the title
  title<-tcLabel("Rsimon",
                 tcGrid(1,1,xspan=2),
                 name="title1",
                 text="Rsimon",
                 size=16,
                 weight="bold")

  # Create the widget list
  widget_list<-list()
  widget_list[[1]]<-frame1
  widget_list[[2]]<-frame2
  widget_list[[3]]<-frame3
  widget_list[[4]]<-frame4
  widget_list[[5]]<-mode
  widget_list[[6]]<-status
  widget_list[[7]]<-title

  # Save to global environment outside of function
  .GlobalEnv$widget_list<-widget_list

}

# Create the list
define_list("flash")

# A function to build UIs with either widget list
.GlobalEnv$switch_ui<-function(type){

  if(type=="flash"){
    tryCatch(tcDestroy("Rsimon"),error=function(e){NULL})
    define_list(type)
    tcBuild("Rsimon",
            widget_list,
            "+400+100")
  } else if(type=="click"){
    .GlobalEnv$flash_status<-FALSE
    tcDestroy("Rsimon")
    define_list(type)
    tcBuild("Rsimon",
            widget_list,
            "+400+100")
  }

}

# Check if user inputs match the correct values
.GlobalEnv$check_ins<-function(){

  #Check that selects and
  vals<-unlist(lapply(seq_along(selects),function(x){
    if(selects[x]!=toflash[x]){
      return(1)
    } else{
      return(0)
    }
  }))

  if(sum(vals)>0){

    tryCatch(tcDestroy("Rsimon"),error=function(e){NULL})
    tk_messageBox(message=paste("You lost after ",
    count_num," iterations on seed ",current_seed,"!",sep=""))

  } else{

    if(length(selects)==count_num){

      .GlobalEnv$count_num<-count_num+1
      .GlobalEnv$selects<-c()

      tryCatch(switch_ui("flash"),error=function(e){NULL})
      tryCatch(flash_frame(),error=function(e){NULL})

    }
  }

}

#A function for when the user activates frame flashing
.GlobalEnv$update_frame<-function(frame){

  #Highlight border
  tcConfigure(frame$Frame,
              background="cyan")

  tcl('update')
  Sys.sleep(0.1)

  #Highlight border
  tcConfigure(frame$Frame,
              background=frame$Color)

  tcl('update')

}

# Functions to retrieve where the user has clicked, check if its correct, and move on
.GlobalEnv$getinred<-function(){
  .GlobalEnv$selects<-c(selects,1)
  update_frame(frames[1,])
  check_ins()
}
.GlobalEnv$getinblue<-function(){
  .GlobalEnv$selects<-c(selects,2)
  update_frame(frames[2,])
  check_ins()
}
.GlobalEnv$getinyellow<-function(){
  .GlobalEnv$selects<-c(selects,3)
  update_frame(frames[3,])
  check_ins()
}
.GlobalEnv$getingreen<-function(){
  .GlobalEnv$selects<-c(selects,4)
  update_frame(frames[4,])
  check_ins()
}

# A function to flash different frames according to a random pattern
.GlobalEnv$flash_frame<-function(){

  # Set the flash speed based on the mode chosen
  if(modeval=="1"){
    flash_time<-0.75
  } else if(modeval=="2"){
    flash_time<-0.5
  } else if(modeval=="3"){
    flash_time<-0.3
  }

  # Update the GUI
  tcl("update")

  # Sleep for half a second prior to starting flashes
  Sys.sleep(0.5)

  # If the flash hasn't started yet (prevents triggering multiple times)
  if(flash_status==FALSE){

    # Set that flash ha sstarted
    .GlobalEnv$flash_status<-TRUE

    #Flash a random frame equal to the number of counts
    set.seed(current_seed)
    .GlobalEnv$toflash<-sample(1:nrow(frames),count_num,replace=TRUE)

    # Cycle through each frame and highlight in purple
    lapply(toflash,function(i){

      # Get the current frame
      cur_frame<-frames[i,]

      # Change the frame to purple
      tcConfigure(cur_frame$Frame,
                  background="purple")

      # Update the gui before sleeping
      tcl("update")

      # Sleep
      Sys.sleep(flash_time)

      # Change the frame to normal background color
      tcConfigure(cur_frame$Frame,
                  background=cur_frame$Color)

      # Update the gui again
      tcl("update")

      # Sleep
      Sys.sleep(flash_time)

    })

    # Switch to user selection mode
    switch_ui("click")

  }


}

# A function to start the game
.GlobalEnv$start_game<-function(){
  tcDestroy("startapp")
  switch_ui("flash")
}

startlist<-list()
startlist[[1]]<-tcFrame("startapp",
                        tcGrid(1,1,padx=10,pady=10),
                        "startframe")
startlist[[2]]<-tcLabel("startframe",
                        tcGrid(1,1),
                        "starttitle",
                        text="Rsimon",
                        weight="bold",
                        size=18)
startlist[[3]]<-tcButton("startframe",
                         tcGrid(1,2,pady=10),
                         "startbutton",
                         text="START GAME",
                         command="start_game",
                         weight="bold")

# Build the app
 tcApp(app_name="Rsimon-1.0.0",
       ui_name="startapp",
       geometry="100x100+500+200",
       startlist,
       run_app = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

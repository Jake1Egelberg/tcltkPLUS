\name{tcGet}
\alias{tcGet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get user inputs from any widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcGet() to extract user-inputted data from any tc widget.
}
\usage{
tcGet(widget_name, widget_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{widget_name}{
%%     ~~Describe \code{x} here~~
The name of the widget you would like to retrieve data from as a string.
}
\item{widget_list}{
The widget list that the widget is stored in.
}
}
\examples{

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
tcBuild("test",
        widget_list,"200x160")


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

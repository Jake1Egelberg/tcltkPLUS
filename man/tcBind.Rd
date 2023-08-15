\name{tcBind}
\alias{tcBind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bind events to tcWidgets
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcBind() in conjunction with individual widget's "binding" argument to link widgets with binding events.
}
\usage{
tcBind(event, fun)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{event}{
%%     ~~Describe \code{x} here~~
A valid tcltk binding event. For a list of all binding events, see \code{binding_events}.
}
\item{fun}{
The function name, as a string, that you would like to call when the binding event is detected.
}
}
\examples{
# Set theme
tcTheme()

.GlobalEnv$bound_function<-function(){
  tk_messageBox(message="Your curser has entered the blue box!")
}

widget_list<-list()
widget_list[[1]]<-tcFrame("app",
                          tcGrid(1,1),
                          "frame1",
                          border=tcBorder(5,"red"),
                          height=300,
                          width=300)
widget_list[[2]]<-tcFrame("frame1",
                          tcGrid(1,1,padx=100,pady=100),
                          "frame2",
                          border=tcBorder(5,"blue"),
                          height=100,
                          width=100,
                          binding=tcBind("<Enter>","bound_function"))
widget_list[[3]]<-tcLabel("frame2",
                          tcGrid(1,1,pady=30),
                          "label",
                          text="Hover over me!",
                          color="blue")

tcBuild("app",
        widget_list,
        "+400+100")

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

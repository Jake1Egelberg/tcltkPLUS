\name{tcAdd}
\alias{tcAdd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add a new widget to an existing widget list
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcAdd() to add new widgets to an existing UI.
}
\usage{
tcAdd(new_widget_object, widget_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{new_widget_object}{
%%     ~~Describe \code{x} here~~
A new widget object to add to the UI. This should be a widget object, not a string.
}
\item{widget_list}{
The widget list to add the new widget to.
}
}
\examples{

# Set theme
tcTheme()

# Define function to print hello
.GlobalEnv$print_hi<-function(){
  tk_messageBox(message='Hi!')
}

# Define function to add a new button
.GlobalEnv$add_button<-function(){

  new_button<-tcButton("app",
                       tcGrid(1,2,padx=10,pady=10),
                       name="button1",
                       text="Im a second button!",
                       command="print_hi")

  tcAdd(new_button,widget_list)

}

# Create the original button widget
button<-tcButton(parent="app",
                 position=tcGrid(1,1,padx=20,10,sticky="w"),
                 name="button1",
                 text="Im a button",
                 command="add_button")

# Create the widget list
widget_list<-list()
widget_list[[1]]<-button

# Build the UI
tcBuild("app",widget_list,geometry = "150x100")


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

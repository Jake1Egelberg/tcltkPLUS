\name{tcListbox}
\alias{tcListbox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a listbox widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcListbox() to display a in interactive list of values on your UI.
}
\usage{
tcListbox(
  parent,
  position,
  name,
  values,
  height,
  width,
  selectmode,
  color,
  background,
  size,
  weight,
  family,
  add_scrollbar,
  binding
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parent}{
%%     ~~Describe \code{x} here~~
The parent's name as a string.
}
\item{position}{
The return of \code{\link{tcGrid}()}.
}
\item{name}{
The name of your widget as a string.
}
\item{values}{
A vector of options to be listed.
}
\item{height}{
(optional) An integer value for the number of options to display.
}
\item{width}{
(optional) An integer value for the width of the displayed list.
}
\item{selectmode}{
A string of "single" or "multiple" specifying if single or multiple selections should (optional) be allowed. Defaults to single.
}
\item{color}{
(optional) A string specifying text color.
}
\item{background}{
(optional) A string specifying background color.
}
\item{size}{
(optional) An integer specifying text size.
}
\item{weight}{
(optional) An integer specifying text weight.
}
\item{family}{
(optional) A string specifying text family.
}
\item{add_scrollbar}{
(optional) A boolean specifying if the list includes a scrollbar. Defaults to FALSE.
}
\item{binding}{
(optional) The return of \code{\link{tcBind}()}.
}
}
\examples{

# Set the theme
tcTheme("DEFAULT")

# Define function to print the selected list value
.GlobalEnv$getlistdata<-function(){

  tk_messageBox(message=tcGet("testlist",widget_list))

}

# Create the list widget
listwidget<-tcListbox(parent="test",
                position=tcGrid(1,4,40,10),
                name="testlist",
                values=c("testing1","testing2","testing3","testing4"),
                weight="bold",
                family="Comic Sans MS",
                height=3,
                width=7,
                binding=tcBind("<<ListboxSelect>>","getlistdata"),
                add_scrollbar=TRUE)

# Create the widget list
widget_list<-list()
widget_list[[1]]<-listwidget

# Build the UI
tcBuild("test",widget_list)


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

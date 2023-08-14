\name{tcLabel}
\alias{tcLabel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a label widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcLabel() to display a string of text on your UI.
}
\usage{
tcLabel(parent, position, name, text, color, background, size, weight, family, binding)
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
\item{text}{
A string of text to be displayed.
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
\item{binding}{
(optional) The return of \code{\link{tcBind}()}.
}
}
\examples{

# Set the theme
tcTheme("DEFAULT")

# Create a function to print hi
.GlobalEnv$print_hello<-function(){
tk_messageBox(message="Hello there!")
}

# Create the list widget
labelwidget<-tcLabel(parent="test",
                     position=tcGrid(1,4,40,10),
                     name="testlist",
                     text="Hover over me!",
                     weight="bold",
                     family="Comic Sans MS",
                     binding=tcBind("<Enter>","print_hello"))

# Create the widget list
widget_list<-list()
widget_list[[1]]<-labelwidget

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

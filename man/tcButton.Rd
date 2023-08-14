\name{tcButton}
\alias{tcButton}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a button widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcButton() to add interactive buttons to your UI.
}
\usage{
tcButton(
  parent,
  position,
  name,
  text,
  command,
  color,
  background,
  size,
  weight,
  family,
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
\item{text}{
A string of text to display within the button.
}
\item{command}{
The name of the function, as a string, to be called when the button is clicked.
}
\item{color}{
(optional) A string specifying text color.
}
\item{background}{
(optional) A string specifying background color.
}
\item{size}{
(optional) A string specifying text size.
}
\item{weight}{
(optional) A string specifying text weight. Can be "normal" or "bold".
}
\item{family}{
(optional) A string specifying text font family.
}
\item{binding}{
(optional) The return of \code{\link{tcBind}()}.
}
}
\examples{
# Set theme
tcTheme()

.GlobalEnv$say_hi<-function(){
  tk_messageBox(message="Hello!")
}

widget_list<-list()
widget_list[[1]]<-tcButton("app",
                           tcGrid(1,1,padx=30),
                           "button",
                           text="Click me!",
                           command="say_hi")

tcBuild("app",widget_list,"+400+100")

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

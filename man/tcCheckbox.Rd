\name{tcCheckbox}
\alias{tcCheckbox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a checkbox widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcCheckbox() to add interactive checkboxes to your UI.
}
\usage{
tcCheckbox(
  parent,
  position,
  name,
  text,
  value,
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
(optional) A string of text to display alongside the checkbox.
}
\item{value}{
(optional) A boolean of TRUE or FALSE. Defaults to FALSE.
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

#Get checkbox input
.GlobalEnv$get_check<-function(){

  # Subtract from 1 to get the new value, not the value prior to selecting the checkbox
  .GlobalEnv$checked<-1-as.numeric(tcGet("check",widget_list))

  if(checked==1){
    label<-tcLabel("frame",
                   tcGrid(1,2,pady=10),
                   "label",
                   "Im checked!")
    tcAdd(label,widget_list)
  } else{
    tcDestroy("label")
  }

}

widget_list<-list()
widget_list[[1]]<-tcFrame("app",
                          tcGrid(1,1,padx=15,pady=10),
                          "frame")
widget_list[[2]]<-tcCheckbox("frame",
                             tcGrid(1,1),
                             "check",
                             text="Check me!",
                             value=FALSE,
                             binding=tcBind("<Button>","get_check"))

tcBuild("app",widget_list,"100x100+400+100")

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

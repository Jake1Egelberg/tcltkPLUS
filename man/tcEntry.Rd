\name{tcEntry}
\alias{tcEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create an entry widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcEntry() to create entry boxes that allows users to type text.
}
\usage{
tcEntry(
  parent,
  position,
  name,
  text,
  width,
  justify,
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
(optional) Default text to display. Defaults to "".
}
\item{width}{
(optional) An integer specifying the entry's width.
}
\item{justify}{
(optional) A string specifying text justification in the entry. Can be "left", "right", or "center".
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

# Define function to reset
.GlobalEnv$reset_fun<-function(){
  lapply(1:entered_number, function(x){
    tcDestroy(paste("label",x,sep=""))
  })
  tryCatch(tcDestroy("resetbutton"),error=function(e){NULL})
}

# Define function when enter is pressed
.GlobalEnv$enter_fun<-function(){

  .GlobalEnv$entered_number<-as.numeric(tcGet("entry",widget_list))

  lapply(1:entered_number,function(x){

    new_entry<-tcLabel("frame",
                       tcGrid(1,2+x,pady=1),
                       paste("label",x,sep=""),
                       text=x,
                       size=x)
    tcAdd(new_entry,
          widget_list)


  })

  tryCatch(tcDestroy("resetbutton"),error=function(e){NULL})

  reset_button<-tcButton("frame",
                         tcGrid(1,3+entered_number,pady=5),
                         "resetbutton",
                         text="Reset",
                         command="reset_fun")
  tcAdd(reset_button,
        widget_list)

}

#Create a frame
frame<-tcFrame("app",
               tcGrid(1,1,padx=10,pady=10),
               "frame")

# Create a label
label<-tcLabel("frame",
               tcGrid(1,1),
               "label",
               text="Enter a number!")

# Create the entry
entry<-tcEntry("frame",
               tcGrid(1,2),
               "entry",
               binding=tcBind("<Return>","enter_fun"))

widget_list<-list()
widget_list[[1]]<-frame
widget_list[[2]]<-label
widget_list[[3]]<-entry

tcBuild("app",widget_list,"+500+100")

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

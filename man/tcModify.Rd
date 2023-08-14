\name{tcModify}
\alias{tcModify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Modify an existing widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcModify() to modify the displayed values of an existing widget.
}
\usage{
tcModify(widget_name, widget_list, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{widget_name}{
%%     ~~Describe \code{x} here~~
A string specifying the widget that you would like to modify.
}
\item{widget_list}{
The widget list containing the widget to modify.
}
\item{...}{,
A new argument. For example: \code{text="new text"}.
}
}
\examples{


# Set theme
tcTheme("DEFAULT")

# Create function to modify label
.GlobalEnv$modify_label<-function(){

  tcModify("label1",
           widget_list,
           text="new text!")

  tcModify("listbox1",
           widget_list,
           values=c("hi","bye"))

  tcModify("listbox1",
           widget_list,
           weight="bold")

  tcModify("combo1",
            widget_list,
            values=c("my","name", "is","jake"))

  tcModify("combo1",
            widget_list,
            selected="my")

  tcModify("combo1",
           widget_list,
           weight="bold")

}

# Create widgets
frame<-tcFrame("app",
               tcGrid(1,1,padx=20,pady=10),
               "frame1")
label<-tcLabel("frame1",
               tcGrid(1,1),
               "label1",
               text="Im a label!")
button<-tcButton("frame1",
                 tcGrid(1,2,sticky="w"),
                 "button1",
                 text="Click me!",
                 command="modify_label")
listbox<-tcListbox("frame1",
                   tcGrid(2,1,yspan=2,padx=10),
                   name="listbox1",
                   values=c("Value 1",
                            "Value 2"))
combobox<-tcCombobox("frame1",
                     tcGrid(3,1,yspan=2),
                     name="combo1",
                     values=c("test 1", "test 2"),
                     selected="test 1")

widget_list<-list()
widget_list[[1]]<-frame
widget_list[[2]]<-label
widget_list[[3]]<-button
widget_list[[4]]<-listbox
widget_list[[5]]<-combobox

tcBuild("app",
            widget_list,
            "350x120")


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

\name{tcCombobox}
\alias{tcCombobox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a combobox widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcCombobox() to allow the user to select one value from a list of provided values.
}
\usage{
  tcCombobox(
  parent,
  position,
  name,
  values,
  selected,
  width,
  justify,
  color,
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
\item{values}{
A vector of options to be listed.
}
\item{selected}{
(optional) A string or numeric value (or variable) to define what the combobox will select on default.
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
tcTheme("DEFAULT")

# Define values to work with
.GlobalEnv$values<-c(5,3)

# Create a function to do math
.GlobalEnv$update_math<-function(){

  .GlobalEnv$selected_fun<-tcGet("combo1",widget_list)
  .GlobalEnv$val1<-as.numeric(tcGet("value1",widget_list))
  .GlobalEnv$val2<-as.numeric(tcGet("value2",widget_list))

  if(selected_fun=="Addition"){

    newresult<-val1+val2

  } else if(selected_fun=="Subtraction"){

    newresult<-val1-val2


  } else if(selected_fun=="Multiplication"){

    newresult<-val1*val2

  } else if(selected_fun=="Division"){

    newresult<-round(val1/val2,3)

  } else{
    newresult<-""
  }

  tcConfigure("result1",
              text=paste("= ",newresult,sep=""))


}

# Create widgets
frame<-tcFrame("app",
               tcGrid(1,1,padx=20,pady=10),
               "frame1")
label<-tcLabel("frame1",
               tcGrid(1,1,padx=0,pady=10,sticky="w"),
               "label1",
               text="Given")
entry1<-tcEntry("frame1",
                tcGrid(2,1,sticky="w"),
                "value1",
                text="5",
                width=3,
                binding=tcBind("<Return>","update_math"))
label2<-tcLabel("frame1",
               tcGrid(2,1,padx=30,sticky="w"),
               "label2",
               text="&")
entry2<-tcEntry("frame1",
                tcGrid(2,1,sticky="e"),
                "value2",
                text="3",
                width=3,
                binding=tcBind("<Return>","update_math"))
combobox<-tcCombobox("frame1",
                     tcGrid(1,2,xspan=3),
                     name="combo1",
                     values=c("Addition", "Subtraction","Multiplication","Division"),
                     selected="Select what to do!",
                     width=15,
                     binding=tcBind("<<ComboboxSelected>>","update_math"))
result<-tcLabel("frame1",
                tcGrid(1,3,xspan=3,pady=10),
                name="result1",
                text="...")

widget_list<-list()
widget_list[[1]]<-frame
widget_list[[2]]<-label
widget_list[[3]]<-combobox
widget_list[[4]]<-result
widget_list[[5]]<-entry1
widget_list[[6]]<-label2
widget_list[[7]]<-entry2

tcBuild("app",
            widget_list,
            "170x140+400+100")

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

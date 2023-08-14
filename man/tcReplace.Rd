\name{tcReplace}
\alias{tcReplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Replace one widget with another
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcReplace() to replace an existing widget a new one.
}
\usage{
tcReplace(old_widget_name, widget_list, new_widget_object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{old_widget_name}{
The name of the widget to replace as a string. This is specified by the "name" attribute when you create the widget.
}
\item{widget_list}{
The list of widgets containing the old widget.
}
\item{new_widget_object}{
The name new widget to replace the old one. This is the object you have saved the new widget to, not a string.
}
}
\examples{


# Set the theme
tcTheme()

# Create a function and button widget that will replace label1 with label2
# Will work in an R script without saving to the global environment
.GlobalEnv$change_the_label<-function(){

  # Label 2 must be saved to the global environment
  .GlobalEnv$label_2<-tcLabel(parent="test",
                              position=tcGrid(1,1),
                              name="label2",
                              text="Goodbye World!")

  # Replace the label!
  tcReplace(old_widget_name="label1",
            widget_list=widget_list,
            new_widget_object=label_2)

}

# Define widgets to include in the UI
label_1<-tcLabel(parent="test",
                 position=tcGrid(1,1),
                 name="label1",
                 text="Hello World!")

button_1<-tcButton(parent="test",
                   position=tcGrid(1,2,padx=10,pady=10),
                   name="button1",
                   text="Change the label!",
                   command="change_the_label")

# Add label1 and button to the widget list
widget_list<-list()
widget_list[[1]]<-label_1
widget_list[[2]]<-button_1

# Build the UI
tcBuild(ui_name="test",
        widget_list=widget_list)


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

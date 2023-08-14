\name{tcGrid}
\alias{tcGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Define widget positions
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
UIs are constructed from columns and rows of widgets layed out in a grid format. Therefore, each widget has a corresponding column (x) and row (y) value.

Use tcGrid() to specify a widget's positioning within the UI grid.
}
\usage{
tcGrid(x, y, padx, pady, xspan, yspan, sticky)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
An integer for the horizontal positioning of a widget.
}
\item{y}{
An integer for the vertical positioning of a widget.
}
\item{padx}{
An integer for the horizontal offset of a widget from its x coordinate.
}
\item{pady}{
An integer for the vertical offset of a widget from its y coordinate.
}
\item{xspan}{
An integer for the number of discrete horizontal positions a widget should cover. This allows a widget to span multiple columns of other widgets.
}
\item{yspan}{
An integer for the number of discrete vertical positions a widget should cover. This allows a widget to span multiple rows of other widgets.
}
\item{sticky}{
A string of "n", "s", "e", "w", or any combination, that specifies which region your widget should originate from within its (x,y) positioning.
}
}
\examples{

#Set theme
tcTheme("DEFAULT")

#Define exponent variable
.GlobalEnv$exponent<-1

# Create a plot
.GlobalEnv$generate_plot<-function(){

  x_vals<-1:100
  y_vals<-(x_vals)^exponent
  plot_data<-data.frame(x=x_vals,
                        y=y_vals)

  .GlobalEnv$gplot<-ggplot(plot_data,aes(x=x,y=y))+
    geom_point()

}
generate_plot()

# Add 1 to exponent
.GlobalEnv$add_exp<-function(){

  # Add 1 to exponent
  .GlobalEnv$exponent<-exponent+1

  # Overwrite the current plot
  generate_plot()

  # Display the new plot
  update_ui()
}

# Subtract 1 from exponent
.GlobalEnv$sub_exp<-function(){

  # Subtract from the exponent
  .GlobalEnv$exponent<-exponent-1

  # Overwrite the current plot
  generate_plot()

  # Display the new plot
  update_ui()

}

# Update the UI
.GlobalEnv$update_ui<-function(){

  # Update entry display
  tcConfigure("entry1",
              textvariable=tclVar(exponent))

  # Update plot display
  tcModify(widget_name="aplot",
           widget_list=widget_list,
           ggplot="gplot")

}

#Create widget list
widget_list<-list()
widget_list[[1]]<-tcButton(parent="plotdisplay",
                           position=tcGrid(3,1,padx=10),
                           name="add1",
                           text="[+]",
                           command="add_exp")
widget_list[[2]]<-tcEntry(parent="plotdisplay",
                          position=tcGrid(2,1,pady=10),
                          name="entry1",
                          text=exponent)
widget_list[[3]]<-tcButton(parent="plotdisplay",
                          position=tcGrid(1,1,padx=10),
                          name="sub1",
                          text="[-]",
                          command="sub_exp")
widget_list[[4]]<-tcPlot(parent="plotdisplay",
                         position=tcGrid(1,2,xspan=3),
                         name="aplot",
                         ggplot="gplot")

tcBuild(ui_name="plotdisplay",
            widget_list)



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

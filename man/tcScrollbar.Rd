\name{tcScrollbar}
\alias{tcScrollbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a scrollbar widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcScrollbar() to create scrollbar widgets.
}
\usage{
tcScrollbar(
  parent,
  position,
  name,
  from,
  to,
  resolution,
  command,
  variable,
  showvalue,
  sliderlength,
  height,
  width,
  label,
  color,
  size,
  weight,
  family,
  orient,
  background,
  troughcolor,
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
\item{from}{
A numeric value setting the minimum scroll value.
}
\item{to}{
A numeric value setting the maximum scroll value.
}
\item{resolution}{
A numeric value setting the scroll resolution.
}
\item{command}{
A function passed the argument ... that reacts to changes in the scroll value and returns the set value. For example, scroll_function<-function(...){print(...)} will print the current scroll value each time it is changed.
}
\item{variable}{
The name of an R object as a string used to store the scroll value.
}
\item{showvalue}{
(optional) A boolean indicating if the current scroll value should be displayed on the UI. Defaults to TRUE.
}
\item{sliderlength}{
(optional) An integer specifying the length of the slider, in pixels.
}
\item{height}{
(optional) An integer value for the height of the scrollbar.
}
\item{width}{
(optional) An integer value for the width of the scrollbar.
}
\item{label}{
(optional) A string to be displayed with the scrollbar. The label appears in the top left corner if the scale is horizontal, or the top right corner if vertical.
}
\item{color}{
(optional) A string specifying text color.
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
\item{orient}{
(optional) The orientation of the scrollbar. Can be "horizontal" or "vertical".
}
\item{background}{
(optional) A string specifying background color.
}
\item{troughcolor}{
(optional) A string specifying trough color.
}
\item{binding}{
(optional) The return of \code{\link{tcBind}()}.
}
}
\examples{

# Set theme
tcTheme()

# Define plot values
.GlobalEnv$data<-data.frame(x=1:10,
                            y=(1:10)^2)

# Create a function to render a plot
.GlobalEnv$render_plot<-function(exponent){
  # Create a plot
  data$y<-data$x^exponent
  .GlobalEnv$testplot<-ggplot(data,aes(x=x,y=y))+
    geom_point()+
    geom_line()
}
render_plot(5)

#Define widgets
frame<-tcFrame("app",
               tcGrid(1,1,pady=5,padx=30),
               "frame1")

# Define scrollbar function
.GlobalEnv$get_value<-function(...){

  exponent<-as.numeric(list(...))

  # Update the plot data
  render_plot(exponent)

  #Modify the UI
  tcModify("plot1",
           widget_list,
           ggplot="testplot")

}

# Define scrollbar
scroll<-tcScrollbar("frame1",
                    tcGrid(1,1),
                    "scroll1",
                    from=1,
                    to=10,
                    resolution=0.1,
                    variable="5",
                    command="get_value",
                    height=250,
                    color="red",
                    weight="bold",
                    orient="horizontal",
                    showvalue=TRUE,
                    label="y=x^(m)")

dispplot<-tcPlot("frame1",
                 tcGrid(1,2),
                 "plot1",
                 ggplot="testplot")

widget_list<-list()
widget_list[[1]]<-frame
widget_list[[2]]<-scroll
widget_list[[3]]<-dispplot

tcBuild("app",
        widget_list,
        geometry="320x340+400+100")


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

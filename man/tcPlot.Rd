\name{tcPlot}
\alias{tcPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a plot widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcPlot() to generate plot widgets.
}
\usage{
tcPlot(parent, position, name, ggplot, hscale, vscale, binding, own_frame)
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
\item{ggplot}{
The name of your ggplot to display as a string.
}
\item{hscale}{
(optional) A horizontal scaling factor.
}
\item{vscale}{
(optional) A vertical scaling factor.
}
\item{binding}{
(optional) The return of \code{\link{tcBind}()}.
}
\item{own_frame}{
(optional) Specifies if the plot should be displayed within its own frame. This defaults to TRUE and avoids oversized margins around the plot in the UI.
}
}
\examples{

# Set the theme
tcTheme()

# Create the plot, must be a ggplot2 object
  # Does not need to be saved to global environment in an R script.
.GlobalEnv$testplot<-ggplot(data=data.frame(x=1:100,y=1:100),aes(x=x,y=y))+
  geom_point()

# Create the plot widget
tkplot<-tcPlot(parent="test",
               position=tcGrid(1,1),
               name="plot1",
               ggplot="testplot",
               hscale=2,
               vscale=1.6,
               own_frame=TRUE)

# Add to the widget list
widget_list<-list()
widget_list[[1]]<-tkplot

# Build the UI
tcBuild("test",
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

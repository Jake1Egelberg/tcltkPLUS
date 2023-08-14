\name{tcBuild}
\alias{tcBuild}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Build a UI from a widget list
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcBuild() to compile a list of widget objects into a fully functional UI.
}
\usage{
tcBuild(ui_name, widget_list, geometry, self_destruct, destruct_delay, pos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ui_name}{
%%     ~~Describe \code{x} here~~
Your user interface's name as a string.
  }
  \item{widget_list}{
A list of widgets created with tc<Widget> functions such as tcFrame or tcEntry.
  }
  \item{geometry}{
(optional) A geometry string such as "50x60+70+80", where 50 is width, 60 is height, 70 is x-offset, and 80 is y-offset, all in units of pixel.
  }
\item{self_destruct}{
(optional) A boolean that controls whether UI immediately deletes itself. Used for bugfixing. Defaults to FALSE.
}
\item{destruct_delay}{
(optional) An integer specifying the time to wait (in seconds) to destroy the window after creating it. Defaults to 100 ms. Useed for bugfixing.
}
\item{pos}{
Do not change.
}
}
\examples{

# Set the theme
tcTheme()

# Create a widget list
widget_list<-list()

# Add a label to the widget list
widget_list[[1]]<-tcLabel(parent="first_ui",position=tcGrid(1,1),name="label",text="A label!")

# Compile the widget list into a UI
tcBuild(ui_name="first_ui",widget_list=widget_list,geometry="100x100")

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

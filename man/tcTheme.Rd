\name{tcTheme}
\alias{tcTheme}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Define the UI theme
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcTheme() to set a global theme for your UI.
}
\usage{
tcTheme(theme_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theme_name}{
%%     ~~Describe \code{theme_name} here~~
A theme name as a string.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
A theme is a list with components Background, WidgetBackground, TextColor, TextSize, and TextFamily. You can create your own theme by creating a list with these values.

For example:
RETRO<-list(Background="violet", WidgetBackground="purple", TextColor="yellow3", TextSize=12, TextFamily="Comic Sans MS")

Default themes include:
DEFAULT, LARGE, SMALL, RETRO, and DARK

}
\examples{

#Set the theme
tcTheme("RETRO")

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

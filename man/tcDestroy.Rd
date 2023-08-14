\name{tcDestroy}
\alias{tcDestroy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Destroy a widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcDestroy() to remove unwanted UI elements or to kill the app.
}
\usage{
tcDestroy(widget_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{widget_name}{
%%     ~~Describe \code{x} here~~
A string specifying the widget or UI to destroy.
}
}
\examples{

# Set the theme
tcTheme()

# Create a function to kill the app
.GlobalEnv$kill_function<-function(){
  tcDestroy("app")
}

# Create a button
button<-tcButton("app",
                 tcGrid(1,1,padx=20),
                 "button1",
                 text="Kill the app!",
                 command="kill_function")

widget_list<-list()
widget_list[[1]]<-button

tcBuild("app",
            widget_list,
            "+400+100")

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

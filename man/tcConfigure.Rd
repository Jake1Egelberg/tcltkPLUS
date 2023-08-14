\name{tcConfigure}
\alias{tcConfigure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Configure widgets with tcltk syntax
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcConfigure to configure or modify tctlkPLUS widgets with traditional tcltk syntax. This may be easier than \code{\link{tcModify}()} in some cases.

tcConfigure will not modify widget lists where tcModify will.
}
\usage{
tcConfigure(widget_name, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{widget_name}{
%%     ~~Describe \code{x} here~~
A string of the widget to configure.
}
\item{...}{
An argument with traditional tcltk syntax. For example, tcConfigure("label",text="New text!").
}
}
\examples{

# Set theme
tcTheme()

# Modify with tcConfigure
.GlobalEnv$tc_config<-function(){

  tcConfigure("label1",
              text="Now try tcModify again!")

}

# Modify with tcModify
.GlobalEnv$tc_mod<-function(){

  tcModify("label1",
           widget_list,
           text="Now try tcConfigure!")

}

# Create widget list
widget_list<-list()
widget_list[[1]]<-tcLabel("app",
                          tcGrid(1,1,pady=15,padx=20),
                          "label1",
                          text="Modify me with tcModify!")
widget_list[[2]]<-tcButton("app",
                          tcGrid(1,3,pady=15),
                          "button1",
                          text="Modify with tcConfigure",
                          command="tc_config")
widget_list[[3]]<-tcButton("app",
                           tcGrid(1,2,pady=15),
                           "button2",
                           text="Modify with tcModify",
                           command="tc_mod")

# Build the UI
tcBuild("app",widget_list)

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

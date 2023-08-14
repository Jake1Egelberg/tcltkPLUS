\name{tcSubmissionbox}
\alias{tcSubmissionbox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a submissionbox
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A stand-alone submission box that returns the user input.
}
\usage{
tcSubmissionbox(
  label_text,
  button_text,
  entry_text,
  entry_width,
  geometry,
  self_destruct,
  destruct_delay
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{label_text}{
%%     ~~Describe \code{x} here~~
A string of text to display about the user entry box.
}
\item{button_text}{
A string of text to display on the submission button.
}
\item{entry_text}{
(optional) A string of text to display in the user entry box.
}
\item{entry_width}{
(optional) The width of the user entry box
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
}
\examples{

# Set the theme
tcTheme()

#Create the submission box
tcSubmissionbox(label_text="Enter your name",button_text="Submit")

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

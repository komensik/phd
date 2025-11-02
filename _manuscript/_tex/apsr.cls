% APSR.cls -- LaTeX class for APSR articles
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{apsr}[2015/08/30 APSR class based on article class]

\LoadClass[12pt]{article}

\RequirePackage{geometry}
\geometry{margin=1in}

\RequirePackage{setspace}
\doublespacing

\RequirePackage{times}
\RequirePackage{natbib}

\renewcommand{\bibname}{References}

\RequirePackage{titlesec}
\titleformat{\section}
  {\normalfont\bfseries\centering}{\thesection.}{1em}{}
\titleformat{\subsection}
  {\normalfont\itshape}{\thesubsection.}{1em}{}
\titleformat{\subsubsection}
  {\normalfont\itshape}{\thesubsubsection.}{1em}{}

\RequirePackage{fancyhdr}
\pagestyle{fancy}
\fancyhf{}
\rhead{\thepage}
\renewcommand{\headrulewidth}{0pt}

\RequirePackage{caption}
\captionsetup{font=small, labelfont=bf}

\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt}

\endinput

# tidyToPān 1.0.0 "Account of Monte Cristo"

tidyToPān 1.0.0 is a trimmed down version of <a href="https://zenodo.org/record/1289084#.XhqUty17Hys" target="_blank">(Meletē)ToPān v.0.5</a>. I wrote (Meletē)ToPān v.0.5 with Ancient languages in mind. In tidyToPān I have simplified the user interface, so that it is easier to use for other languages too. I also made sure that there are fewer dependencies: for instance, I am not expecting the data to be in a specific citation format and one has to install fewer external libraries to make tidyToPān work. Additionally, I have updated the source code, so it incorporates recent advances of the 'tidyverse' package. 

The name (Meletē)ToPān v.0.5 is based on the Greek principle μελέτη τὸ πᾶν which roughly translate to "take into care everything". I decided for the name because Topic-Modelling performs well on large amounts of logically structured chunks of texts and it helps selecting the interesting bits in a large corpus of text by technically having looked at everything. The butterfly in the logo is of the species Melete. The original photograph is by Didier Descouens and he has licensed it under CC BY-SA 4.0. I changed the image for the logo slightly. I'd strongly suggest to start with the <a href="https://commons.wikimedia.org/wiki/File:Melete_leucadia_MHNT_dos.jpg" target="_blank">original</a> if you want to use it, but you can also use this now slightly modified logo under CC BY-SA 4.0 license as I am required to share it under the same license as the original image.

tidyToPān is Topic-Modelling for everyone: from people without programming knowledge to people that want to build teaching and text-reuse tools and apps based on Topic-Modelling data without having to develop their own tool or having to majorly restructure their textual data. tidyToPān is made to be shared and used. That is why I tried to modularise tidyToPān in a way that in each step you could ingest your own data. While I have decoupled tidyToPān from a specific citation infrastructure, I still insist that all you documents should have a unique idea. If you don't know how to generate IDs for your documens have a look at the <a href="http://cite-architecture.github.io" target="_blank">CTS/CITE architecture</a>.

tidyToPān is also still under active development, but should be stable. Please let me know if you have any issues.

## Installing it

You need to install R and the libraries mentioned under "Trying it". I also recommend to install RStudio. Then just clone the repository, modify it as you like and run it as an RShiny app.

1. Install [R](https://www.r-project.org) and optionally [RStudio](https://www.rstudio.com) or jump to [rstudio.cloud](https://rstudio.cloud) to test it there.
2. In RStudio/R install some packages you will need: `install.packages(c("shiny", "tidyverse", "lda", "LDAvis", "data.table", "Rtsne", "DT"))`
3. `library(shiny)`
4. `runApp('GithubProjects/tidyToPan')` ("GithubProjects" or whatever folder you have cloned it to)
5. Enjoy!

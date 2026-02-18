Minimum Structure-Related and Content-Related Project Requirements
In terms of the structure of questions, the specific requirements are as follows:
1) There are at least three primary questions (at least three questions in the first layer),
2) There are at least three secondary questions (at least three questions not in the first layer),
3) At least two primary questions had follow-up questions.
   
In terms of content-related requirements, the specific requirements are as follows:
1) At least four of your questions involve scraping data from the web, using an API to collect
the data, or automating the collection of data across different webpages or over time,
2) Each of your six questions were answered with statistical or decision-analytic tools learned
in this class, in another class, or on your own,
3) At least four of your questions involve you building some kind of a regression model and
at least one of your questions involves providing an actionable recommendation for your
audience (resulting from a statistical or decision-analytical model),
4) At least three of your questions have data visualizations to support your investigation (not
counting regression diagnostic plots).

Data Skill #1: Data Collection
For this skill, you must either scrape data from the web or call an API from within R to collect the
data. You cannot just use packages/examples from class (you can use code/packages from class to
supplement your data, but you must have data in your project collected on your own not seen in
the lecture slides). You can also satisfy this data collection requirement by automating the
collection of csv or JSON files across many different pages or over time to collect your data. In
essence, this skill should demonstrate some data collection skills that you did not have prior to
taking the class. Simply downloading a csv file from the web and importing it into R does not
count (since you may already have had that skill prior to taking the class). However, if you
automate the process in some way (via a for-loop or using a task scheduling tool to collect data
over time in an automated fashion), then that would satisfy the data collection requirement. You
can feel free supplement the data you obtain from this with data that you simply download from
other publicly available sources (such as csv files, JSON files, XML files, etc.). Packages that
might be useful are rvest, httr, XML, and RSelenium, and the PhantomJS tool might be useful too.
Examples of various data collection techniques are covered in the Automated Data Collection
textbook, and more examples are provided in the Web-Scraping subfolder on D2L.


Data Skill #2: Data Visualization
For this skill, you must first ask questions for which a data visualization would be very helpful in
providing insights and then visually investigate your questions by creating at least three data
visualizations (preferably, at least one of the visualizations would be interactive). These
visualizations should be clearly relevant to the question(s) you are asking (i.e., you are not creating
plots just to create more plots), and they should be made for a general audience (i.e., creating
regression diagnostic plots do not count towards data visualization). Although I am fairly open to
any data visualization tool, I would recommend against making plots using base R or Excel. Use
more modern R packages (ggplot2, plotly, ggmaps, leaflet, sp, etc.) if creating visualizations in R.
Similarly, I would also recommend using Power BI instead of Excel.


Data Skill #3: Data Analysis
For this skill, you must first ask questions that can be framed as hypothesis tests, statistical models,
or decision-analysis problems. Ignore from consideration questions that cannot be answered with
one of those approaches. If you ask a question that can be formulated as a hypothesis test (including
regression models), then conduct the appropriate test or create an appropriate decision/statistical
model. If you formulate a decision-analytic model, then parameters of that model should be
estimated from real-world data. You can use any analytical techniques we have covered or any
techniques you may have learned on your own or in another class. More details on the requirements
are provided in the next section.


Structure of Questions
Throughout your projects, questions should be structured in a way that can be answered through
data (publicly available data, so that we can replicate your findings). Some questions you may be
interested in are not answerable with data at all (philosophical questions or thought experiments),
so you need to make sure the questions you ask for your projects can be investigated with data.
Moreover, you want to frame your questions in a way that cannot be interpreted as being
potentially biased or loaded (asking a question that assumes something that you are trying to show
or need to show).

In terms of your questions, at bare minimum, your Final Project needs to answer six questions of
your choice based on your group’s discussions, and these questions should be structured in one of
the two following ways. The first approach is “three sets of questions, each two levels deep.” The
second approach is structured so that you explore one set of questions three levels deep, another
set of questions two levels deep, and one question that did not come out of the answer to either of
the other two lines of questioning. The two approaches are discussed in more detail below.

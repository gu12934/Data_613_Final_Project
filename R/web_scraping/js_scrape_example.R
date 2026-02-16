
# The below is from: http://www.rladiesnyc.org/post/scraping-javascript-websites-in-r/

# writeLines() can be used to create and save new text files in your working directory.
# You tell it what to write, and setting the connection (con) specifies what the name of the file is.

writeLines("var url = 'http://therapboard.com';
var page = new WebPage();
var fs = require('fs');

page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('1.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}
", con = "scrape.js")


# This function below allows us to change the scrape.js file above to use whatever URL we specify.

js_scrape <- function(url = "http://therapboard.com", 
                      js_path = "scrape.js",
                      phantompath = "C:/Users/duy.dao/Dropbox/Files to share between computers/Teaching/Fall 2021/Slides for Fall 2021/Supplemental Slides/More on Scraping/PhantomJS example"){
  
  # Set the default phantompath above to your working directory
  
  # This section will replace the url in scrape.js to whatever you want 
  lines <- readLines(js_path)
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, js_path)
  
  command = paste(phantompath, js_path, sep = " ")
  system(command)
  
}

js_scrape(url = "http://www.planecrashinfo.com/2021/2021.htm")

# Now check your scrape.js file.
# It now has a new url.


# You can imagine how you would use this in a for-loop to scrape across pages, ...
# updating the url for each cycle of the for-loop.

# In the for-loop, you would update your URL using the js_scrape() function above, ...
# then use system("./phantomjs scrape.js") ...
# (and use Sys.sleep() to pause R to give your computer more time to save the HTML files).

# Doing this would save the HTML pages to your working directory.

# For practice, you can try to use this approach to get the data from ...
# http://www.planecrashinfo.com/database.htm by modifying the above


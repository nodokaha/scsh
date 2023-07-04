#!/bin/sh
# An example CGI program outputing the current date
echo content-type: text/html
echo status: 200 OK
echo 
echo "<html><body>"
echo "<h2> This is the cgi script. </h2>"
echo "<br> Current date: "
echo `date`
echo "</body></html>"

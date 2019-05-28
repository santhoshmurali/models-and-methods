from flask import Flask
from flask import render_template
from datetime import datetime
import re

app = Flask(__name__)

#@app.route("/")
##def home():
##    return("Hello, Flask!")

@app.route("/hello/<name>")
def hell_there(name):
    now = datetime.now()
    formatted_now = now.strftime("%A, %d %B. %Y at %X")
    # Filter the name argument to letters only using regular expressions. URL arguments
    # can contain arbitrary text, so we restrict to safe characters only.
    match_object = re.match("[a-zA-Z]+",name)

    if match_object:
        clean_name = match_object.group(0)
    else:
        clean_name = "Friend"
    content = "Hello there, " + clean_name + "! It's " + formatted_now

    return(content)

@app.route("/hello/render/<name>")
def hell0_there_r(name=None):
    return(render_template("hello_there.html",name=name,date=datetime.now()))

@app.route("/hello/data")
def returnData():
    return(app.send_static_file("data.json"))

@app.route("/")
def renderHome():
    return(render_template("home.html"))

@app.route("/about/")
def renderAbout():
    return(render_template("about.html"))

@app.route("/contact/")
def renderContact():
    return(render_template("contact.html"))
from flask import Flask,jsonify

app = Flask(__name__)

@app.route("/")
def helloFlask():
    return(jsonify({"message":"Hello Flask"}))

if __name__ == "__main__":
    app.run()
    

function Countdown() {

    var _self = this;
    var timer = null;

    var width = 150;
    var docid = null;

    this.start = function(id) {
        docid = id;
        document.getElementById(docid).style.width = width+"px";
        document.getElementById(docid).style.display = "block";
        timer = setTimeout(function() { _self.update(120); }, 250);
    }

    this.update = function(secs) {
        document.getElementById(docid).style.width = (1.25*secs)+"px";

        if(secs != 0) {
           timer = setTimeout(function() { _self.update(secs-1); }, 250);
        } else {
            this.stop()
        }
    }

    this.stop = function() {
       document.getElementById(docid).style.display = "none";
        clearTimeout(timer);
    }

}
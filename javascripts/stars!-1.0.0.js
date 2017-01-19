var stdInput = unescape(encodeURIComponent(""));
$("lisp").livequery(function() {
   stdInput += unescape(encodeURIComponent( $(this).text() ));
});

var Module = {
  preRun: function() {
    console.log("preRun");
    function stdin() {
      if (stdInput.length == 0) {
        return undefined;
      }

      var chr = stdInput.charCodeAt(0);
      stdInput = stdInput.substring(1);
      return chr;
    }
    var stdout = null;
    var stderr = null;
    FS.init(stdin, stdout, stderr);
  },
   postRun: function() {
      console.log("postRun");
   },

   print: function(text) {
      document.write(text);
   },
   printErr: function(text) {
      alert(text);
   },

  setStatus: function(text) {
    console.log(text);
  },
  totalDependencies: 0,
  monitorRunDependencies: function(left) {
    console.log(left);
    this.totalDependencies = Math.max(this.totalDependencies, left);
    Module.setStatus(left ? 'Preparing... (' + (this.totalDependencies-left) + '/' + this.totalDependencies + ')' : 'All downloads complete.');
  }
};
Module.setStatus('Downloading...');
window.onerror = function(event) {
  // TODO: do not warn on ok events like simulating an infinite loop or exitStatus
  Module.setStatus('Exception thrown, see JavaScript console');
  //spinnerElement.style.display = 'none';
  Module.setStatus = function(text) {
    if (text) Module.printErr('[post-exception status] ' + text);
  };
};

// need to do this at runtime!
$(function() {
   var script = document.createElement('script');
   script.src = "javascripts/olvm-1.0.js"; // 1.0-154-g1dfdc2f
   document.body.appendChild(script);
});
// load Otus Lisp handler:

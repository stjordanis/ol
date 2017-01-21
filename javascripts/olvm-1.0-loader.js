var stdInput = unescape(encodeURIComponent( "" ));
$("#lisp").livequery(function() {
   //console.log($(this).html());
   stdInput += unescape(encodeURIComponent( $(this).html() ));
});
function tolisp(text)
{
   stdInput += unescape(encodeURIComponent( text ));
}

var Module;
Module = {
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
      document.write("<html>");
      document.write(Module.preHead);
      document.write("<body>");
      Module.preHead;

      // head restored, let's change print to simple version
      (Module.print = function(text) {
         //console.log('write:' + text);
         document.write(text);
         //$("body").append(text); // этот метод не работает с частичными, только с целыми кусками
      })(text);
   },
   printErr: function(text) {
      alert(text);
   },
   preHead: $('head').html(),
   setStatus: function(text) {
      console.log("status: " + text);
   },
   totalDependencies: 0,
   monitorRunDependencies: function(left) {
      console.log("left: " + left);
      this.totalDependencies = Math.max(this.totalDependencies, left);
      Module.setStatus(left ? 'Preparing... (' + (this.totalDependencies-left) + '/' + this.totalDependencies + ')' : 'All downloads complete.');
  }
};
//Module.setStatus('Downloading...');
window.onerror = function(event) {
   // TODO: do not warn on ok events like simulating an infinite loop or exitStatus
   Module.setStatus('Exception thrown, see JavaScript console');
   //spinnerElement.style.display = 'none';
   Module.setStatus = function(text) {
      if (text) Module.printErr('[post-exception status] ' + text);
   };
};

// shall do this at runtime!
//$(function() {
   var script = document.createElement('script');
   script.src = "javascripts/olvm-1.0.js"; // 1.0-154-g1dfdc2f
   document.body.appendChild(script);
//});

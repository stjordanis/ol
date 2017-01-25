function tolisp( text )
{
   Module.send(text);
}

var Ol, Module;  // global "Ol" variable
Ol = Module = {
   oltext: unescape(encodeURIComponent( "" )),
   preRun: function() {
      console.log("preRun");
      function stdin() {
         if (Module.oltext.length == 0) {
            return undefined;
         }

         var chr = Module.oltext.charCodeAt(0);
         Module.oltext = Module.oltext.substring(1);
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
      var m = Module;
      // the first print must restore the head
      console.log("Restoring the 'head'...");
      document.write("<html>");
      document.write(Module.savedHead);
      document.write("<body>");

      // restore the global "Module" variable
      window.Module = m;

      // head restored, let's change print to simple version
      (Module.print = function(text) {
         //console.log('write:' + text);
         document.write(text);
         //$("body").append(text); // этот метод не работает с частичными, только с целыми кусками
      })(text);
   },
   // this function sends the text to the lisp machine
   send: function(text) {
      Module.oltext += unescape(encodeURIComponent( text ));
   },
   printErr: function(text) {
      alert(text);
   },
   savedHead: $('head').html(),
   setStatus: function(text) {
      console.log("status: " + text);
   },

   totalDependencies: 0,
   monitorRunDependencies: function(left) {
      console.log("left: " + left);
      Module.totalDependencies = Math.max(Module.totalDependencies, left);
      Module.setStatus(left ? 'Preparing... (' + (Module.totalDependencies-left) + '/' + Module.totalDependencies + ')' : 'All downloads complete.');
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
// please, start virtual machine only at runtime!
$(function() {
   var script = document.createElement('script');
   script.src = "javascripts/olvm-1.0.js"; // 1.0-154-g1dfdc2f
   document.body.appendChild(script);
});

// subscribe to "lisp" code
$("#lisp").livequery( function() {
   console.log( $(this).html() );
   Module.send( $(this).html() );
});

function OL(text) {
   Ol.send( text );
}

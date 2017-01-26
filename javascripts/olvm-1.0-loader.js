function tolisp(text)
{
   Module.send(text);
}

// global variable
var Module;
$(function() {
   console.log("x");

Module = window.Module || (function() {
   // do something.
   console.log("Module creation");

   //Module.setStatus('Downloading...');
   /*window.onerror = function(event) {
      // to do: do not warn on ok events like simulating an infinite loop or exitStatus
      Module.setStatus('Exception thrown, see JavaScript console');
      //spinnerElement.style.display = 'none';
      Module.setStatus = function(text) {
         if (text) Module.printErr('[post-exception status] ' + text);
      };
   };*/
   
   var head = $('head').html();
   var body = $('body').html();

   // shall do this at runtime!
   // please, start virtual machine only at runtime!
   // let's start loading the olvm
   var script = document.createElement('script');
   script.src = "javascripts/olvm-1.0.js"; // 1.0-154-g1dfdc2f
   document.body.appendChild(script);

   return {
      oltext: unescape(encodeURIComponent( "(print)" )),
   preRun: function() {
      console.log("preRun");
      // redefine stdin:
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

      // subscribe to "lisp" code
      $("#lisp").livequery( function() {
         //console.log( $(this).html() );
         Module.send( $(this).html() );
      });
   },
   print: function(text) {
      // the first print must restore the head
      var me = Module;
      // prepare the document to olvm
      document.write("<html>");
      console.log("Restoring the 'head'...");
      document.write(head);
      document.write("<body>");

      console.log("Restoring the 'body'...");
      document.write(body);
      console.log("Cleanup...");

      // cleanup
      head = body = null;
      window.Module = me;

      /*window.OL = function(something) {
         console.log("ol: " + something);
         Module.send( something );
      }*/

      // preparations done, simplifying
      //Module.print = document.write;
      (Module.print = function(text) {
       //console.log('write:' + text);
         document.write(text);
         //$("body").append(text); // этот метод не работает с частичными кусками, только с целым текстом
      })(text);
   },
   // this function sends the text to the lisp machine
   send: function(text) {
      Module.oltext += unescape(encodeURIComponent( text ));
   },
   printErr: function(text) {
      alert(text);
   },
   setStatus: function(text) {
      console.log("status: " + text);
   },

   totalDependencies: 0,
   monitorRunDependencies: function(left) {
      console.log("left: " + left);
      Module.totalDependencies = Math.max(Module.totalDependencies, left);
      Module.setStatus(left ? 'Preparing... (' + (Module.totalDependencies-left) + '/' + Module.totalDependencies + ')' : 'All downloads complete.');
   }}
})();

});
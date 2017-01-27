function OL(text)
{
// console.log("ol: " + text);
   Module.send(text);
}

var timestamp = 0;
// global variable
var Module;
$(function() {
   console.log("--x--");

// window.Module || 
Module = (function() {
   console.log("Module creation");

return {
   oltext: unescape(encodeURIComponent( "(print)" )), // сразу сбросим буфер
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
      //var ts2 = 0;
      $("#lisp").livequery(function() {
      // console.log("lisp: " + ++ts2);
      // console.log( $(this).html() );
         Module.send( $(this).html() );
      });
   },
   printS: "",
   print: function(text) {
      console.log(++timestamp + " print: " + text);
      $("body").append(text); // этот метод не работает с частичными кусками, только с целым текстом
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

   // shall do this at runtime!
   // please, start virtual machine only at runtime!
   // let's start loading the olvm
   var script = document.createElement('script');
   script.src = "javascripts/olvm-1.0.js"; // 1.0-154-g1dfdc2f
   document.body.appendChild(script);
});
// -=( some usefull/weird staff )=----------------------------

function goto(url, ok, fail) {
   console.log('trying to go to ' + url);
   var address = url[0];
   for (var i = 1; i < url.length; i++) {
      address += '/';
      var arg = url[i];
      if (typeof arg === 'string' || arg instanceof String)
         address += "'" + escape(arg);
      else
         address += arg;
   }

   //was: document.location.href = address;
   console.log(url);
   $.ajax({
      url: address,
      context: document.body
   }).done(function(text) {
      console.log("trying to call " + '('+ok+' '+ text +')');
      tolisp('('+ok+' '+ text +')\n');
   }).error(function(text) {
      tolisp('('+fail+' '+ text +')\n');
   });
}

function redirect() {
	setTimeout(function(){goto($('#redirect').attr('href'))}, 2000);
}

// -=( some usefull/weird staff )=----------------------------

function goto(url, ok, fail) {
   console.log('trying to go to ' + url);
   var address = url;
   for (var i = 1; i < arguments.length; i++) {
      address += '/';
      var arg = arguments[i];
      if (typeof arg === 'string' || arg instanceof String)
         address += "'" + escape(arg);
      else
         address += arg;
   }

   //was: document.location.href = address;
   $.ajax({
      url: address,
      context: document.body
   }).done(function() {
      tolisp('(print "ok")');
   }).error(function() {
      tolisp('(print "fail")');
   });
}

function redirect() {
	setTimeout(function(){goto($('#redirect').attr('href'))}, 2000);
}

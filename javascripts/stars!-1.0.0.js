// http://stackoverflow.com/questions/332872/encode-url-in-javascript
// escape() will not encode: @*/+
// encodeURI() will not encode: ~!@#$&*()=:/,;?+'
// encodeURIComponent() will not encode: ~!*()'
function goto(url) {
   var address = url;
   for (var i = 1; i < arguments.length; i++) {
      address += '/';
      var arg = arguments[i];
      if (typeof arg === 'string' || arg instanceof String)
         address += "'" + escape(arg);
      else
         address += arg;
   }

   document.location.href = address;
}

function redirect() {
	setTimeout(function(){goto($('#redirect').attr('href'))}, 2000);
}

/*function load(context) {

    var defaults = {
        parameter1: defaultValue1,
        parameter2: defaultValue2,
        ...
    };

    var context = extend(defaults, context);

    // do stuff
}
function extend() {
    for (var i = 1; i < arguments.length; i++)
        for (var key in arguments[i])
            if (arguments[i].hasOwnProperty(key))
                arguments[0][key] = arguments[i][key];
    return arguments[0];
}

function load(context) {
   var parameter1 = context.parameter1 || defaultValue1,
       parameter2 = context.parameter2 || defaultValue2;

   // do stuff
}

log() {
    let args = Array.prototype.slice.call(arguments);
    args = ['MyObjectName', this.id_].concat(args);
    console.log.apply(console, args);
}
*/

function radio(name, v) {
	var i = 0;
	var s = 0;
	$('input[name='+name+']').each(function() {
		if (i == v)
			this.checked = true;
		if (this.checked)
			s = i;
		i += 1;
	});
	return s;
}
function check(name, v) {
	var i = 1;
	var s = 0;
	$('input[name='+name+']').each(function() {
		if (i == (v & i))
			this.checked = true;
		if (this.checked)
			s = i | s;
		i <<= 1;
	});
	return s;
}

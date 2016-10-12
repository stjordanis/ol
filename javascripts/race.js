function prt(v)
{
	return radio('prt', v);
/*	var i = 0;
	var s = 0;
	$('input[name=prt]').each(function() {
		if (i == v)
			this.checked = true;
		if (this.checked)
			s = i;
		i += 1;
	});
	return s;*/
}

function lrts(v)
{
	return check('lrts', v);
/*
	var i = 1;
	var s = 0;
	$('input[name=lrts]').each(function() {
		if ((v & i) == i)
			this.checked = true;
		if (this.checked)
			s = s | i;
		i <<= 1;
	});
	return s;*/
}
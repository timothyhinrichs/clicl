
function preg_replace (fromregstring, to, string) {
	var reg = new RegExp(fromregstring, "g")
	return string.replace(reg, to);
}

function preg_match(regstring, string) {
	var reg  = new RegExp(regstring);
	return string.match(reg);
}


#!/usr/bin/perl
# symbols.pl performs various operations on logical html symbols

# process command-line
my $htmlmode = "-html";
my $imagemode = "-images";
my $unicodemode = "-unicode";

my $mode = $ARGV[0];
   
print_help() 
	if (!(($mode eq $htmlmode) || 
		  ($mode eq $imagemode) || 
		  ($mode eq $unicodemode)));




my @symbols = (
"Alpha", "Alpha",
"Beta", "Beta",
"Gamma", "Gamma",
"Delta", "Delta",
"Epsilon", "Epsilon",
"Zeta", "Zeta",
"Eta", "Eta",
"Theta", "Theta",
"Iota", "Iota",
"Kappa", "Kappa",
"Lambda", "Lambda",
"Mu", "Mu",
"Nu", "Nu",
"Omicron", "Omicron",
"Pi", "Pi",
"Rho", "Rho",
"Sigma", "Sigma",
"Tau", "Tau",
"Phi", "Phi",
"Psi", "Psi",
"Chi", "Chi",
"Omega", "Omega",
"alpha", "alpha",
"beta", "beta",
"gamma", "gamma",
"delta", "delta",
"epsilon", "epsilon",
"zeta", "zeta",
"eta", "eta",
"theta", "theta",
"iota", "iota",
"kappa", "kappa",
"lambda", "lambda",
"mu", "mu",
"nu", "nu",
"omicron", "omicron",
"pi", "pi",
"rho", "rho",
"sigma", "sigma",
"tau", "tau",
"phi", "phi",
"psi", "psi",
"chi", "chi",
"omega", "omega",
"Leftarrow", "lArr",
"Rightarrow", "rArr",
"Leftrightarrow", "hArr",
"forall", "forall",
"exists", "exist",
"cap", "cap",
"cup", "cup",

"neq", "#8800",
"wedge", "#x22C0",    # #8743
"vee", "#x22C1",      # #8744
"neg", "#x00AC",      #172 
"not in", "#x2209",  
"not ni", "#x220C",  
"not proves", "#x22AC", 
"not models", "#x22AD", 
"qed", "#x220E", 
"in", "isin",
"ni", "#x220B",
"proves", "#x22AC",  
"models", "#x22AD"
);



# output the appropriate information
if ($mode eq $htmlmode) {
	output_html();
} elsif ($mode eq $imagemode) {
	output_images();
} elsif ($mode eq $unicodemode) {
	output_unicode();
}

exit;


sub output_html  {

	print "<table border=\"1\">\n";
	for (my $i=0; $i<$#symbols; $i=$i+2) {
		print "<tr><td>$symbols[$i]</td><td>&$symbols[$i+1];</td></tr>\n";
	}
	print "</table>\n";
}

sub output_unicode  {
	#s/\\forall/&forall;/g;

	for (my $i=0; $i<$#symbols; $i=$i+2) {
		print 's/\\\\' . space2back($symbols[$i]) . "/&$symbols[$i+1];/g;\n";
	}
}

sub output_images  {
	#s/\\forall/&forall;/g;

	for (my $i=0; $i<$#symbols; $i=$i+2) {
		print 's/\\\\' . space2back($symbols[$i]) . 
			"/<img src=\"images\\/" . spacegone($symbols[$i]) . ".gif\">/g;\n";
	}
}

sub space2back () {
	# turns any spaces into double back slashes.
	join('\\\\', split(" ", $_[0]));
}
sub spacegone () {
	# removes all spaces {
	join("", split(" ", $_[0]));
}

sub print_help {
	print "Usage: symbols.pl (-html|-images|-unicode) \n";
	print "   $htmlmode outputs an html page for testing a browser's support of  symbols.\n";
	print "   $imagemode outputs perl code for substituting LaTeX commands with images.\n";
	print "   $unicodemode outputs perl code for substituting LaTeX commands with unicode.\n";
	exit;

}

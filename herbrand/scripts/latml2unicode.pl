#!/usr/bin/perl
# Convert an html file with embedded LaTeX commands into html.

my $content_string = "<!-- content here -->";


my $file = $ARGV[0];
my $template_file = $ARGV[1];
if ((!(-e $file)) || (!(-e $template_file))) {
	print "Usage: latml2html.pl <file> <template-file>\n Converts latml file to HTML.\n";
	exit;
}

# grab the contents of the file
open(FILE, $file) || die "Error: Could not open $file\n";
@contents = <FILE>;
$_ = join("", @contents);
close(FILE);

# then perform the translation
#print $_;

# Greek letters

s/\\Alpha/&Alpha;/g;
s/\\Beta/&Beta;/g;
s/\\Gamma/&Gamma;/g;
s/\\Delta/&Delta;/g;
s/\\Epsilon/&Epsilon;/g;
s/\\Zeta/&Zeta;/g;
s/\\Eta/&Eta;/g;
s/\\Theta/&Theta;/g;
s/\\Iota/&Iota;/g;
s/\\Kappa/&Kappa;/g;
s/\\Lambda/&Lambda;/g;
s/\\Mu/&Mu;/g;
s/\\Nu/&Nu;/g;
s/\\Omicron/&Omicron;/g;
s/\\Pi/&Pi;/g;
s/\\Rho/&Rho;/g;
s/\\Sigma/&Sigma;/g;
s/\\Tau/&Tau;/g;
s/\\Phi/&Phi;/g;
s/\\Psi/&Psi;/g;
s/\\Chi/&Chi;/g;
s/\\Omega/&Omega;/g;

s/\\alpha/&alpha;/g;
s/\\beta/&beta;/g;
s/\\gamma/&gamma;/g;
s/\\delta/&delta;/g;
s/\\epsilon/&epsilon;/g;
s/\\zeta/&zeta;/g;
s/\\eta/&eta;/g;
s/\\theta/&theta;/g;
s/\\iota/&iota;/g;
s/\\kappa/&kappa;/g;
s/\\lambda/&lambda;/g;
s/\\mu/&mu;/g;
s/\\nu/&nu;/g;
s/\\omicron/&omicron;/g;
s/\\pi/&pi;/g;
s/\\rho/&rho;/g;
s/\\sigma/&sigma;/g;
s/\\tau/&tau;/g;
s/\\phi/&phi;/g;
s/\\psi/&psi;/g;
s/\\chi/&chi;/g;
s/\\omega/&omega;/g;

# Mathematics
s/\\Leftarrow/&lArr;/g;
s/\\Rightarrow/&rArr;/g;
s/\\Leftrightarrow/&hArr;/g;

s/\\forall/&forall;/g;
s/\\exists/&exist;/g;
s/\\wedge/&#8743;/g;
s/\\vee/&#8744;/g;
s/\\neg/&#x00AC;/g;

s/\\ltstruct/&lt;/g;
s/\\gtstruct/&gt;/g;


s/\\not\\in/&#x2209;/g;
s/\\not\\ni/&#x220C;/g;
s/\\not\\proves/|/-/g;         #&#x22AC;/g;
s/\\not\\models/|#/g;         #&#x22AD;/g;


s/\\qed/&#x220E/ig;
s/\\cap/&cap;/g;
s/\\cup/&cup;/g;
s/\\in/&isin;/g;   # &#2208;/g;
s/\\ni/&#220B;/g;

s/\\proves/|-/g;   #&#x22A2;/g;
s/\\models/|=/g;   #&#x22A8;/g;


# superscript, subscript: order is important
s/_{(.*)}/<sub>$1<\/sub>/g;
s/_(.)/<sub>$1<\/sub>/g;

s/\^{(.*)}/<sup>$1<\/sup>/g;
s/\^(.)/<sup>$1<\/sup>/g;


# indentation
$_ = rewriteIndent($_);

# nobr indentation
$_ = rewriteNoBrIndent($_);


# output the file
#my $newfile = dropExt($file) . ".html";
#if ($newfile eq $file)  {
#	print "Going to overwrite input file.  Exiting...\n";
#	exit;
#}

# Stick it in the template.
open(TEMPLATE, $template_file) || die "Error: Could not open template file.\n";
@contents = <TEMPLATE>;
$contents = join("", @contents);
close(TEMPLATE);

$contents =~ s/$content_string/$_/;

# Output it to the file
#open(FILE, ">$newfile");
#print FILE $contents;
#close(FILE);

print $contents;
exit;

# do the indentation transformation
# WTF: why is the $3 being given the right info below?
sub rewriteIndent () {
	my @lines = split("<indent>", $_[0]);
	my @output;
	for ($i=0; $i<=$#lines; $i++) {
		if ($lines[$i] =~ /^((.|\s)*)<\/indent>((\s|.)*)$/) {
			$output[$i] = "<ul>". htmlbreak($1) . "</ul>" . $3;
		} else {
			$output[$i] = $lines[$i];
		}
	}
	#print @output, "\n";
	return join("", @output);
}

sub rewriteNoBrIndent () {
	my @lines = split("<nobrindent>", $_[0]);
	my @output;
	for ($i=0; $i<=$#lines; $i++) {
		if ($lines[$i] =~ /^((.|\s)*)<\/nobrindent>((\s|.)*)$/) {
			$output[$i] = "<br><span style=\"margin-left: 35px;\">". htmlbreak($1) . "</span>" . $3;
		} else {
			$output[$i] = $lines[$i];
		}
	}
	#print @output, "\n";
	return join("", @output);
}

# turn normal line breaks into br line breaks
sub htmlbreak() {
	my $line = $_[0];
	#print $line;
	$line =~ s/\n/<br>\n/g;
	return $line;
}


# drop the extension (.xxx) off a file name
sub dropExt() {
   $f = $_[0];
   my $i = rindex($f, ".");
   return $f if ($i==-1);
   return substr($f,0,$i);
}


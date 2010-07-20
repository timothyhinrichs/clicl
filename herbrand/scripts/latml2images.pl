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


s/\\Alpha/<img src="images\/Alpha.gif">/g;
s/\\Beta/<img src="images\/Beta.gif">/g;
s/\\Gamma/<img src="images\/Gamma.gif">/g;
s/\\Delta/<img src="images\/Delta.gif">/g;
s/\\Epsilon/<img src="images\/Epsilon.gif">/g;
s/\\Zeta/<img src="images\/Zeta.gif">/g;
s/\\Eta/<img src="images\/Eta.gif">/g;
s/\\Theta/<img src="images\/Theta.gif">/g;
s/\\Iota/<img src="images\/Iota.gif">/g;
s/\\Kappa/<img src="images\/Kappa.gif">/g;
s/\\Lambda/<img src="images\/Lambda.gif">/g;
s/\\Mu/<img src="images\/Mu.gif">/g;
s/\\Nu/<img src="images\/Nu.gif">/g;
s/\\Omicron/<img src="images\/Omicron.gif">/g;
s/\\Pi/<img src="images\/Pi.gif">/g;
s/\\Rho/<img src="images\/Rho.gif">/g;
s/\\Sigma/<img src="images\/Sigma.gif">/g;
s/\\Tau/<img src="images\/Tau.gif">/g;
s/\\Phi/<img src="images\/Phi.gif">/g;
s/\\Psi/<img src="images\/Psi.gif">/g;
s/\\Chi/<img src="images\/Chi.gif">/g;
s/\\Omega/<img src="images\/Omega.gif">/g;
s/\\alpha/<img src="images\/alpha.gif">/g;
s/\\beta/<img src="images\/beta.gif">/g;
s/\\gamma/<img src="images\/gamma.gif">/g;
s/\\delta/<img src="images\/delta.gif">/g;
s/\\epsilon/<img src="images\/epsilon.gif">/g;
s/\\zeta/<img src="images\/zeta.gif">/g;
s/\\eta/<img src="images\/eta.gif">/g;
s/\\theta/<img src="images\/theta.gif">/g;
s/\\iota/<img src="images\/iota.gif">/g;
s/\\kappa/<img src="images\/kappa.gif">/g;
s/\\lambda/<img src="images\/lambda.gif">/g;
s/\\mu/<img src="images\/mu.gif">/g;
s/\\nu/<img src="images\/nu.gif">/g;
s/\\omicron/<img src="images\/omicron.gif">/g;
s/\\pi/<img src="images\/pi.gif">/g;
s/\\rho/<img src="images\/rho.gif">/g;
s/\\sigma/<img src="images\/sigma.gif">/g;
s/\\tau/<img src="images\/tau.gif">/g;
s/\\phi/<img src="images\/phi.gif">/g;
s/\\psi/<img src="images\/psi.gif">/g;
s/\\chi/<img src="images\/chi.gif">/g;
s/\\omega/<img src="images\/omega.gif">/g;
s/\\Leftarrow/<img src="images\/Leftarrow.gif">/g;
s/\\Rightarrow/<img src="images\/Rightarrow.gif">/g;
s/\\Leftrightarrow/<img src="images\/Leftrightarrow.gif">/g;
s/\\forall/<img src="images\/forall.gif">/g;
s/\\exists/<img src="images\/exists.gif">/g;
s/\\cap/<img src="images\/cap.gif">/g;
s/\\cup/<img src="images\/cup.gif">/g;
s/\\wedge/<img src="images\/wedge.gif">/g;
s/\\vee/<img src="images\/vee.gif">/g;
s/\\neg/<img src="images\/neg.gif">/g;
s/\\not\\in/<img src="images\/notin.gif">/g;
s/\\not\\ni/<img src="images\/notni.gif">/g;
s/\\not\\proves/<img src="images\/notproves.gif">/g;
s/\\not\\models/<img src="images\/notmodels.gif">/g;
s/\\qed/<img src="images\/qed.gif">/g;
s/\\in/<img src="images\/in.gif">/g;
s/\\ni/<img src="images\/ni.gif">/g;
s/\\proves/<img src="images\/proves.gif">/g;
s/\\models/<img src="images\/models.gif">/g;


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


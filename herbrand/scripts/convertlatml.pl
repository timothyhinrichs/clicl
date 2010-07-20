#!/usr/bin/perl
## This script converts a list of latml filenames with latml2unicode.pl script.
##   If no files are specified on the command line, the contents of 
##   $latml_dir is used.

# global variables
my $unicode_output_dir = "../html/";   # directory for html w/ unicode
my $image_output_dir = "../htmlimg/";      # directory for html w/ images 
my $latml_dir = "../latml/";              # latml directory
my $template_file = "../template.html";   # template file
my @files = ();                           # xml files to process
my $force_all = 0;                        # disregard file modification times



# process command-line
if ($#ARGV >= 0) {
   if ($ARGV[0] =~ /^-/) {
      # help
      if($ARGV[0] =~ /^-h/) {
         print "Usage: convertlatml.pl [-all] [<filepaths>]\n";
         print "   Use contents of $latml_dir if no files specified.\n";
         print "   Updates only those html files older than latml counterparts,\n";
         print "     unless -all is specified.\n\n";
         exit;
      }
      if($ARGV[0] =~ /^-all/) {
         $force_all = 1;
         shift(@ARGV);
      }
   } 

   # drop any unused switches
   while ($ARGV[0] =~ /^-/) { shift(@ARGV); }
   @files = @ARGV;
}

# ensure directories end with /
$unicode_output_dir = $unicode_output_dir."/" if (!($unicode_output_dir =~ /\/$/));
$latml_dir = $latml_dir."/" if (!($latml_dir =~ /\/$/));

# if no files, read in those in the latml directory and prepend the $latml_dir to
#   ensure all items in @files are path-names
if ($#files == -1) {
   opendir(LATML, $latml_dir) || 
          die("Error: No filenames specified and latml directory could not be opened.\n");
   my @contents = readdir(LATML);
   closedir(LATML);
   foreach my $f (@contents) {
      # if $f is a file, with extension latml, and it is newer than the html version, add
      #   it to the list of files to process.
      if ((-f "$latml_dir$f") && ($f =~ /\.latml$/) && 
            newerLATMLorTemplate("$latml_dir$f", $template_file, $unicode_output_dir))  {
         push(@files, "$latml_dir$f") 
      }
   }
}

# check if any files need to be processed
if ($#files == -1) {
   print("All files up to date.\n");
   exit;
}

############ Conversion ##########

# convert @files to unicode
print "-- Converting latml to html with Unicode with latml2unicode.pl\n";
my @tfiles, @xfiles;
foreach my $f (@files) {
   
   my $target_file = dropExt(findFile($f)).".html";   # new filename
   my $target_path = "$unicode_output_dir$target_file";       # new filepath
   my $latml_file = dropExt(findFile($f)).".latml";   # latml filename (drop path)
   push(@tfiles, $target_file);                       # tfiles is a list of new filenames
   push(@xfiles, $latml_file);                        # xfiles is a list of the associated xml 

   print "\t$f to $target_path...\n";
   # convert latml to html
   my $call = "./latml2unicode.pl $f $template_file > $target_path";
   system($call);
}

# convert @files to images
#print "-- Converting latml to html with images with latml2images.pl\n";
#my @tfiles, @xfiles;
#foreach my $f (@files) {
   
#   my $target_file = dropExt(findFile($f)).".html";   # new filename
#   my $target_path = "$image_output_dir$target_file";       # new filepath
#   my $latml_file = dropExt(findFile($f)).".latml";   # latml filename (drop path)
#   push(@tfiles, $target_file);                       # tfiles is a list of new filenames
#   push(@xfiles, $latml_file);                        # xfiles is a list of the associated xml 

#   print "\t$f to $target_path...\n";
   # convert latml to html
#   my $call = "./latml2images.pl $f $template_file > $target_path";
#   system($call);
#}


print "-- Processing complete.\n";

exit;

#########################
## Utility subroutines ##


# determine if the template or a latml file is newer than its html counterpart
sub newerLATMLorTemplate() {
   my $latml = $_[0];   # latml file
   my $template = $_[1];  # template
   my $html = $_[2].dropExt(findFile($latml)).".html";   # html file

   # if $force_all is true, return true.
   return 1 if ($force_all);

   # if there is no html file, return true.
   return 1 if (!(-f $html)); 

   # find info and modification times 
   my @ops = stat($latml);
   my $latmltime = $ops[9];
   @ops = stat($html);
   my $htmltime = $ops[9];
   @ops = stat($template);
   my $templatetime = $ops[9];

   return (($latmltime > $htmltime) || ($templatetime > $htmltime));
   #print "xml: $xml, html: $html\n";
   #print " -- xmltime: $xmltime, htmltime: $htmltime \n";
}

# drop the extension (.xxx) off a file name
sub dropExt() {
   $f = $_[0];
   my $i = rindex($f, ".");
   return $f if ($i==-1);
   return substr($f,0,$i);
}  

# find just the filename given a path
sub findFile() {
   $path = $_[0];
   my $i = rindex($path, "/");
   return $path if ($i == -1);
   return substr($path,$i+1);
}





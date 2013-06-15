package Data::Path::DZIL;

# VERSION
# ABSTRACT: dist.ini-style paths for DZIL

#############################################################################
# Modules

use sanity;
use Moo;

use String::Escape;

use namespace::clean;
no warnings 'uninitialized';

#############################################################################
# Required Methods

with 'Data::Path::Role::Path';

sub blueprint { {
   hash_step_regexp => qr/
      # Standard character (or a zero-length with a delimiter)
      (?<key>\w+|(?=\.))|

      # Quoted key
      (?<quote>['"])(?<key> (?:

         # The (?!) is a fancy way of saying ([^\"\\]*) with a variable quote character
         (?>(?: (?! \\|\g{quote}). )*) |  # Most stuff (no backtracking)
         \\ \g{quote}                  |  # Escaped quotes
         \\ (?! \g{quote})                # Any other escaped character

      )* )\g{quote}|

      # Zero-length step (with a single blank key)
      (?<key>^$)
   /x,

   array_step_regexp   => qr/\[(?<key>\d{1,5})\]/,
   delimiter_regexp    => qr/(?:\.|(?=\[))/,

   unescape_sub          => \&String::Escape::unbackslash,
   unescape_quote_regexp => qr/\"/,

   delimiter_placement => {
      HH => '.',
      AH => '.',
   },

   depth_translation => {
      '#DEFAULT#' => 'X+1',
   },

   array_step_sprintf       => '[%u]',
   hash_step_sprintf        => '%s',
   hash_step_sprintf_quoted => '"%s"',
   quote_on_regexp          => qr/\W|^$/,

   escape_sub       => \&String::Escape::backslash,
   escape_on_regexp => qr/\W|^$/,
} }

42;

__END__

=begin wikidoc

= SYNOPSIS

   # code

= DESCRIPTION

### Ruler ##################################################################################################################################12345

Insert description here...

= CAVEATS

### Ruler ##################################################################################################################################12345

Bad stuff...

= SEE ALSO

### Ruler ##################################################################################################################################12345

Other modules...

= ACKNOWLEDGEMENTS

### Ruler ##################################################################################################################################12345

Thanks and stuff...

=end wikidoc

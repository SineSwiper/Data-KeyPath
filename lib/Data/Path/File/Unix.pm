package Data::Path::File::Unix;

# VERSION
# ABSTRACT: UNIX file path support

#############################################################################
# Modules

use sanity;
use Moo;

use namespace::clean;
no warnings 'uninitialized';

#############################################################################
# Required Methods

with 'Data::Path::Role::Path';

sub blueprint { {
   hash_step_regexp => qr{
      # Illegal characters are a mere \0 and /
      (?<key>[^/\0]*)
   }x,

   array_step_regexp   => qr/\Z.\A/,  # no-op; arrays not supported
   delimiter_regexp    => qr{/+},     # + to capture repetitive slashes, like foo////bar

   # no support for escapes
   unescape_sub          => undef,
   unescape_quote_regexp => qr/\Z.\A/,

   delimiter_placement => {
      '0R' => '/',
      HH   => '/',
   },

   depth_translation => {
      qr{^/+$}     => 0,
      qr{^\.\./*$} => 'X-1',
      qr{^\./*$}   => 'X-0',
      '#DEFAULT#'  => 'X+1',
   },

   array_step_sprintf       => '',
   hash_step_sprintf        => '%s',
   hash_step_sprintf_quoted => '%s',
   quote_on_regexp          => qr/\Z.\A/,  # no-op; quoting not supported

   escape_sub       => undef,
   escape_on_regexp => qr/\Z.\A/,
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

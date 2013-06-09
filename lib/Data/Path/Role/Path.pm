package Data::Path::Role::Path;

# VERSION
# ABSTRACT: Role for paths

#############################################################################
# Modules

use sanity;
use Moo;
use MooX::Types::MooseLike 0.18;  # AnyOf support
use MooX::Types::MooseLike::Base qw(Str ArrayRef HashRef CodeRef RegexpRef InstanceOf AnyOf Maybe);  ### FIXME: Clean this up ###

use String::Escape qw( qqbackslash unbackslash );
use Scalar::Util qw( blessed );
use Storable qw( dclone );

use namespace::clean;
no warnings 'uninitialized';

#############################################################################
# Overloading

use overload
   # assign

   ### FIXME: assign subs shouldn't modify its assignment :(
   #'.='   => sub {
   #   my ($self, $thing) = @_;
   #   $self->push($thing);
   #},

   # 3way_comparison
   #'cmp'  ### TODO

   # conversion
   'bool' => sub { !!$_[0]->depth },
   '""'   => sub { $_[0]->as_string },
   '0+'   => sub { $_[0]->depth },
   #'qr'  ### TODO

   # dereferencing
   '@{}'  => sub { @{ $_[0]->_path } },

   # special
   '='    => sub { $_[0]->clone; }
;


#############################################################################
# Attributes

### TODO: Quote handling needs its own attr sub or switch ###

### FIXME: Really, ::Path should be the role with all of the properties ###
has keypath_obj => (
   is       => 'ro',
   isa      => InstanceOf['Data::KeyPath'],
   required => 1,
   weak_ref => 1,
   handles => [ qw(
      hash_step_regexp array_step_regexp delimiter_regexp
      delimiter delimiter_placement

      error _set_error
   ) ],
);

has _path => (
   is       => 'rw',
   isa      => ArrayRef[HashRef[Str]],
   predicate => 1,
);

has _tmp_path_thing => (
   is       => 'ro',
   init_arg => 'path',
   required => 1,
   clearer  => 1,
);

#############################################################################
# Pre/post-BUILD

sub BUILD {
   my $self = $_[0];

   # Post-build coercion of path
   unless ($self->_has_path) {
      my $path_array = $self->_coerce_step( $self->_tmp_path_thing ) || return;
      $self->_path( $path_array );
   }
   $self->_clear_tmp_path_thing;  # ...and may it never return...

   return $self;
}

#############################################################################
# Methods

sub depth   { scalar @{$_[0]->_path}; }

# XXX: This makes internal CORE calls ambiguous

sub shift   {   CORE::shift @{$_[0]->_path}; }
sub pop     {     CORE::pop @{$_[0]->_path}; }
sub unshift {
   my ($self, $thing) = @_;
   my $hash_steps = $self->_coerce_step($thing) || do {
      # Add path to error
      $self->_set_error( sprintf "While unshifting path '%s', %s", $self->as_string, $self->error );
      return;
   };
   CORE::unshift @{$_[0]->_path}, @$hash_steps;
}
sub push {
   my ($self, $thing) = @_;
   my $hash_steps = $self->_coerce_step($thing) || do {
      # Add path to error
      $self->_set_error( sprintf "While pushing path '%s', %s", $self->as_string, $self->error );
      return;
   };
   CORE::push    @{$_[0]->_path}, @$hash_steps;
}

sub clone {
   my $self = $_[0];
   $self->new(
      keypath_obj => $self->keypath_obj,
      _path => dclone($self->_path),
      path  => '',  # ignored
   );
}

sub _coerce_step {
   my ($self, $step) = @_;

   # A string step/path to be converted to a HASH step
   unless (ref $step) {
      return $self->path_str2array($step)
   }

   ### FIXME: Add check for blessed objects ###

   # A potential HASH step
   elsif (ref $step eq 'HASH') {
      if (grep { ref $_ } values %$step) {
         $self->_set_error('found incoercible HASH step with ref values');
         return;
      }

      unless ( grep { not exists $step->{$_} } qw(key type step) ) {
         # We have no idea what data is in $step, so we just soft clone it into
         # something else.  Our own methods will bypass the validation if we
         # pass the right thing, by accessing _path directly.
         return [{
            type => $step->{type},
            key  => $step->{key},
            step => $step->{step},
         }];
      }

      # It's better to have a key/type pair than a step
      if (exists $step->{key} and exists $step->{type}) {
         my $hash_step = $self->key2hash( @$step{qw(key type)} ) || return;  # error already exists
         return [ $hash_step ];
      }

      return $self->path_str2array( $step->{step} ) if (exists $step->{step});

      $self->_set_error('found incoercible HASH step with wrong keys/data');
      return;
   }

   # A collection of HASH steps?
   elsif (ref $step eq 'ARRAY') {
      my $path_array = [];
      foreach my $item (@$step) {
         my $hash_step = $self->_coerce_step($item) || return;  # error already exists
         CORE::push @$path_array, $hash_step;
      }

      return $path_array;
   }

   # WTF is this?
   else {
      $self->_set_error( sprintf "found incoercible %s step", ref $step );
      return;
   }
}

sub key2hash {
   my ($self, $key, $type) = @_;

   # Sanity checks
   unless ($type =~ /^HASH$|^ARRAY$/) {
      $self->_set_error('Type must be HASH or ARRAY!');
      return;
   }

   my $hash_re  = $self->hash_step_regexp;
   my $array_re = $self->array_step_regexp;

   # Transform the key to a string step
   my $step = $key;
   ### FIXME: Add hash/array rules attr ###
   if ($type eq 'HASH') {
      $step = qqbackslash($step) if ($step =~ /\W/ or not length $step);  ### FIXME: Add quoting RE attr ###
      ### TODO: Sprintf step for hashes, if needed ###
   }
   else {
      $step = sprintf '[%u]', $step;
   }

   # Validate the new step
   if (
      $type eq 'HASH'  and $step !~ /^$hash_re$/ ||
      $type eq 'ARRAY' and $step !~ /^$array_re$/
   ) {
      $self->_set_error( sprintf "found %s key than didn't validate against regexp: '%s' --> '%s' (depth %u)", $type, $key, $step, $self->depth );
      return;
   }

   return {
      type => $type,
      key  => $key,
      step => $step,
   };
}

sub path_str2array {
   my ($self, $path) = @_;
   my $path_array = [];

   while (length $path) {
      my $hash_steps = $self->shift_path_str(\$path, scalar @$path_array) || do {
         # Add path to error
         $self->_set_error( sprintf "In path '%s', %s", $_[1], $self->error );
         return;
      };

      CORE::push(@$path_array, $hash_steps);
      if (@$path_array > 255) {
         $self->_set_error( sprintf "In path '%s', too deep down the rabbit hole, stopped at '%s'", $_[1], $path );
         return;
      }
   };

   return $path_array;
}

sub shift_path_str {
   my ($self, $pathref, $depth) = @_;

   my $orig_path = $$pathref;

   my $hash_re  = $self->hash_step_regexp;
   my $array_re = $self->array_step_regexp;
   my $delim_re = $self->delimiter_regexp;

   ### FIXME: There's something inaccurate about this... ###
   #$$pathref =~ s/^$delim_re//;

   my $hash_step;
   # Array first because hash could have zero-length string
   if ($$pathref =~ s/^(?<step>$array_re)//) {
      $hash_step = {
         type => 'ARRAY',
         key  => $+{key},
         step => $+{step},
      };
   }
   elsif ($$pathref =~ s/^(?<step>$hash_re)//) {
      $hash_step = {
         type => 'HASH',
         key  => $+{key},
         step => $+{step},
      };

      # Support escaping via double quotes
      $hash_step->{key} = unbackslash($hash_step->{key}) if ($+{quote} eq '"');
   }
   else {
      $self->_set_error( sprintf "found unparsable step: '%s' (depth %u)", $_[1], $$pathref, $depth );
      return;
   }

   $$pathref =~ s/^$delim_re//;

   # If the path is not shifting at all, then something is wrong with REs
   if (length $$pathref == length $orig_path) {
      $self->_set_error( sprintf "found unshiftable step: '%s' (depth %u)", $$pathref, $depth );
      return;
   }

   return $hash_step;
}

### NOTE: basically the opposite of path_str2array ###
sub as_string {
   my $self = $_[0];

   my $d   = $self->delimiter;
   my $dlp = $self->delimiter_placement;

   my $str = '';
   for my $i (0 .. $self->depth - 1) {
      my $hash_step = $self->_path->[$i];
      my $next_step = ($i == $self->depth - 1) ? undef : $self->_path->[$i+1];

      # First delimiter placement
      ### FIXME: Need first_delimiter? ###
      $str .= $d if ($dlp =~ /PL/ and !$i);

      # Add the step
      $str .= $hash_step->{step};

      if ($next_step) {
         my ($L, $R) = (
            substr($hash_step->{type}, 0, 1),
            substr($next_step->{type}, 0, 1),
         );

         $str .= $d if ($dlp =~ /$L$R/);
      }
      # Ending delimiter
      else {
         $str .= $d if ($dlp =~ /PR/);
      }
   }

   return $str;
}

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

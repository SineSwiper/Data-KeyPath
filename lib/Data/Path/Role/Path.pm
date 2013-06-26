package Data::Path::Role::Path;

# VERSION
# ABSTRACT: Role for paths

#############################################################################
# Modules

use sanity;

use Moo::Role;
use MooX::Types::MooseLike 0.18;  # AnyOf support
use MooX::Types::MooseLike::Base qw(Bool Str ArrayRef HashRef InstanceOf);

use String::Escape qw( qqbackslash unbackslash );
use Scalar::Util qw( blessed );
use Storable qw( dclone );
use List::AllUtils qw( first all any );

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
   'bool' => sub { !!shift->step_count },
   '""'   => sub { shift->as_string    },
   '0+'   => sub { shift->step_count   },
   #'qr'  ### TODO

   # dereferencing
   '@{}'  => sub { @{ shift->_path } },

   # special
   '='    => sub { shift->clone }
;

#############################################################################
# Requirements

requires 'blueprint';

# hash_step_regexp
# array_step_regexp
# delimiter_regexp
#
# unescape_sub
# unescape_quote_regexp
#
# delimiter_placement
# depth_translation
#
# array_step_sprintf
# hash_step_sprintf
# hash_step_sprintf_quoted
# quote_on_regexp
#
# escape_sub
# escape_on_regexp

#############################################################################
# Attributes

has stash_obj => (
   is        => 'ro',
   isa       => InstanceOf['Data::Stash'],
   weak_ref  => 1,
   predicate => 1,
);

has _path => (
   is        => 'rw',
   isa       => ArrayRef[HashRef[Str]],
   predicate => 1,
);

has _tmp_path_thing => (
   is       => 'ro',
   init_arg => 'path',
   required => 1,
   clearer  => 1,
);

has auto_normalize => (
   is        => 'ro',
   isa       => Bool,
   dfeault   => sub { 0 },
);

has auto_cleanup => (
   is        => 'ro',
   isa       => Bool,
   dfeault   => sub { 0 },
);

### XXX: Should there be individual Path::error attrs, or should it just die? ###
has _error => (
   is      => 'rwp',
   isa     => Str,
   predicate => 1,
   clearer   => 1,
);

#############################################################################
# Pre/post-BUILD

sub BUILD {
   my $self = $_[0];

   # Post-build coercion of path
   unless ($self->_has_path) {
      my $path_array = $self->_coerce_step( $self->_tmp_path_thing ) || return;
      $self->_path( $path_array );

      if ($self->auto_cleanup and @$path_array) { $self->cleanup || return; }
   }
   $self->_clear_tmp_path_thing;  # ...and may it never return...

   return $self;
}

#############################################################################
# Methods

# XXX: The array-based methods makes internal CORE calls ambiguous
no warnings 'ambiguous';

sub depth {
   my $self = shift;
   $self->step_count ? $self->_path->[-1]{depth} : undef;
}
sub step_count { scalar @{shift->_path}; }

sub is_absolute {
   my $self = shift;
   $self->_path->[0]{depth} !~ /^X/;
}

sub shift   {   shift @{shift->_path}; }
sub pop     {     pop @{shift->_path}; }
sub unshift {
   my $self = shift;
   my $hash_steps = $self->_coerce_step([@_]) || do {
      # Add path to error
      $self->_set_error( sprintf "While unshifting path '%s', %s", $self->as_string, $self->error );
      return;
   };

   my $return = unshift @{$self->_path}, @$hash_steps;
   if ($self->auto_cleanup and @$hash_steps) { $self->cleanup || return; }
   return $return;
}
sub push {
   my $self = shift;
   my $hash_steps = $self->_coerce_step([@_]) || do {
      # Add path to error
      $self->_set_error( sprintf "While pushing path '%s', %s", $self->as_string, $self->error );
      return;
   };

   my $return = push @{$self->_path}, @$hash_steps;
   if ($self->auto_cleanup and @$hash_steps) { $self->cleanup || return; }
   return $return;
}
sub splice {
   my ($self, $offset, $length) = (shift, shift, shift);
   my $hash_steps = $self->_coerce_step([@_]) || do {
      # Add path to error
      $self->_set_error( sprintf "While splicing path '%s', %s", $self->as_string, $self->error );
      return;
   };

   # Perl syntax getting retardo here...
   my @params = ( $offset, defined $length ? ($length, @$hash_steps) : () );
   my @return = splice( @{$self->_path}, @params );
   #my $return = splice( @{$self->_path}, $offset, (defined $length ? ($length, @$hash_steps) : ()) );

   if ($self->auto_cleanup and defined $length and @$hash_steps) { $self->cleanup || return; }
   return \@return;
}

sub clone {
   my $self = $_[0];

   $self->new(
      stash_obj => $self->stash_obj,
      _path => dclone($self->_path),
      path  => '',  # ignored

      auto_normalize => $self->auto_normalize,
      auto_cleanup   => $self->auto_cleanup,
   );
}

sub normalize {
   my $self = $_[0];
   $self->_normalize( $self->_path );
   return $self;
}

sub _normalize {
   my ($self, $path_array) = @_;

   # For normalization, can't trust the original step, so we make new ones
   my $new_array = [];
   foreach my $item (@$path_array) {
      my $hash_step = $self->key2hash( @$item{qw(key type depth)} ) || return;  # error already exists
      push @$new_array, (ref $hash_step eq 'ARRAY') ? @$hash_step : $hash_step;
   }

   return $new_array;
}

sub cleanup {
   my $self = $_[0];
   my $path = $self->_path;
   my $new_path = [];

### FIXME: Rename depth to index or pos ###

   my ($old_depth, $old_type);
   foreach my $hash_step (@$path) {
      my $full_depth = $hash_step->{depth};

      # Process depth
      my ($depth, $type);
      if    ($full_depth =~ /^(\d+)$/)       { ($depth, $type) = ($1, 'A'); }  # absolute
      elsif ($full_depth =~ /^X([+\-]\d+)$/) { ($depth, $type) = ($1, 'R'); }  # relative
      else {                                                                   # WTF is this?
         $self->_set_error( sprintf "During path cleanup, found unparsable depth: %s (step: %s)", $full_depth, $hash_step->{step} );
         return;
      }
      $depth = int($depth);

      ### FIXME: Revisit this after plotting all of the path classes...
      ### We may not need this level of complexity if we are only using 0, 1, X-1, X-0, X+1

      my $new_hash_step = { %$hash_step };

      # The most important depth is the first one
      unless (defined $old_depth) {
         $old_depth = $depth;
         $old_type  = $type;

         push(@$new_path, $new_hash_step);
         $new_hash_step->{depth} = $hash_step->{depth};
         next;
      }

      # Relative is going to continue the status quo
      if ($type eq 'R') {
         $old_depth += $depth;
         $new_hash_step->{depth} = $old_type eq 'A' ? $old_depth : sprintf 'X%+d', $depth;

         # Don't use the depth for placement.  Follow the chain of the index, using the array offset.
         # IOW, if it started out with something like X+3, we won't end up with a bunch of starter blanks.
         my $array_index = $#$new_path + $depth;

         # If the index ends up in the negative, we can't clean it up yet.
         if ($array_index < 0) {
            if ($old_type eq 'A') {
               # FIXME: Solve for C:\.. (which should error sooner)

               # An absolute path should never go into the negative index (ie: /..)
               $self->_set_error( sprintf "During path cleanup, an absolute path dropped into a negative depth (full path: %s)", $self->as_string );
               return;
            }

            push(@$new_path, $new_hash_step);
         }
         # Backtracking
         elsif ($depth <= 0) {
            # If the slicing would carve off past the end, just append and move on...
            if (@$new_path < abs($depth)) {
               push(@$new_path, $new_hash_step);
               next;
            }

            # Just ignore zero-depth (ie: /./)
            next unless $depth;

            # Carve off a slice of the $new_path
            my @back_path = splice(@$new_path, $depth);

            # If any of the steps in the path are a relative negative, we have to keep all of them.
            if (any { $_->{depth} =~ /^X-/ } @back_path) { push(@$new_path, @back_path, $new_hash_step); }

            # Otherwise, we won't save this virtual step, and trash the slice.
         }
         # Moving ahead
         else {
            $new_path->[$array_index] = $new_hash_step;
         }
      }
      # Absolute is a bit more error prone...
      elsif ($type eq 'A') {
         if ($old_type eq 'R') {
            # What the hell is ..\C:\ ?
            $self->_set_error( sprintf "During path cleanup, a relative path found an illegal absolute step (full path: %s)", $self->as_string );
            return;
         }

         # Now this is just A/A, which is rarer, but may happen with volumes
         $new_hash_step->{depth} = $old_depth = $depth;
         $new_path->[$depth] = $new_hash_step;
      }
   }

   # Replace
   $self->_path( $new_path );

   return $self;
}

sub _coerce_step {
   my ($self, $thing) = @_;

   # A string step/path to be converted to a HASH step
   unless (ref $thing) {
      my $path_array = $self->path_str2array($thing);
      return $path_array unless $self->auto_normalize;
      return $self->_normalize($path_array);
   }

   # Another DP path object
   elsif (blessed $thing and $thing->does('Data::Path::Role::Path')) {
      # At the very least, we need to make sure our depths are cleaned up.
      $self ->cleanup || return;
      $thing->cleanup || return;

      # If the class is the same, it's the same type of path and we can do a
      # direct transfer.  And only if the path is normalized, or we don't care
      # about it.
      return dclone($thing->_path) if (
         $thing->isa($self) and
         $thing->auto_normalize || not $self->auto_normalize
      );

      return $self->_normalize($thing->_path);
   }

   # WTF is this?
   elsif (blessed $thing) {
      $self->_set_error( sprintf "found incoercible %s step (blessed)", blessed $thing );
      return;
   }

   # A potential HASH step
   elsif (ref $thing eq 'HASH') {
      if (grep { ref $_ } values %$thing) {
         $self->_set_error('found incoercible HASH step with ref values');
         return;
      }

      if ( all { exists $thing->{$_} } qw(key type step depth) ) {
         # We have no idea what data is in $thing, so we just soft clone it into
         # something else.  Our own methods will bypass the validation if we
         # pass the right thing, by accessing _path directly.
         return [{
            type  => $thing->{type},
            key   => $thing->{key},
            step  => $thing->{step},
            depth => $thing->{depth},
         }];
      }

      # It's better to have a key/type pair than a step
      if (exists $thing->{key} and exists $thing->{type}) {
         my $hash_step = $self->key2hash( @$thing{qw(key type depth)} ) || return;  # error already exists
         return [ $hash_step ];
      }

      return $self->path_str2array( $thing->{step} ) if (exists $thing->{step});

      $self->_set_error('found incoercible HASH step with wrong keys/data');
      return;
   }

   # A collection of HASH steps?
   elsif (ref $thing eq 'ARRAY') {
      my $path_array = [];
      foreach my $item (@$thing) {
         my $hash_step = $self->_coerce_step($item) || return;  # error already exists
         push @$path_array, (ref $hash_step eq 'ARRAY') ? @$hash_step : $hash_step;
      }

      return $path_array;
   }

   # WTF is this?
   else {
      $self->_set_error( sprintf "found incoercible %s step", ref $thing );
      return;
   }
}

sub key2hash {
   my ($self, $key, $type, $depth) = @_;

   # Sanity checks
   unless ($type =~ /^HASH$|^ARRAY$/) {
      $self->_set_error('Type must be HASH or ARRAY!');
      return;
   }

   my $bp = $self->blueprint;
   my $hash_re  = $bp->{hash_step_regexp};
   my $array_re = $bp->{array_step_regexp};

   # Transform the key to a string step
   my $step = $key;
   if ($type eq 'HASH') {
      $step = $bp->{escape_sub}->($step) if ($bp->{escape_sub} and $step =~ $bp->{escape_on_regexp});
      $step = sprintf ($bp->{
         'hash_step_sprintf'.($step =~ $bp->{quote_on_regexp} ? '_quoted' : '')
      }, $step);
   }
   else {
      $step = sprintf ($bp->{array_step_sprintf}, $step);
   }

   # Validate the new step
   if (
      $type eq 'HASH'  and $step !~ /^$hash_re$/ ||
      $type eq 'ARRAY' and $step !~ /^$array_re$/
   ) {
      $self->_set_error( sprintf "found %s key than didn't validate against regexp: '%s' --> '%s' (depth: %s)", $type, $key, $step, $depth // '???' );
      return;
   }

   return {
      type  => $type,
      key   => $key,
      step  => $step,
      ### XXX: No +delimiter in latter case.  Not our fault; doing the best we can with the data we've got! ###
      depth => $depth // $self->_find_depth($step),
   };
}

sub path_str2array {
   my ($self, $path) = @_;
   my $path_array = [];

   while (length $path) {
      my $hash_step = $self->shift_path_str(\$path, scalar @$path_array) || do {
         # Add path to error
         $self->_set_error( sprintf "In path '%s', %s", $_[1], $self->error );
         return;
      };

      push(@$path_array, $hash_step);
      if (@$path_array > 255) {
         $self->_set_error( sprintf "In path '%s', too deep down the rabbit hole, stopped at '%s'", $_[1], $path );
         return;
      }
   };

   return $path_array;
}

sub _find_depth {
   my ($self, $step_plus_delimiter) = @_;

   # Find a matching depth key
   my $dt = $self->blueprint->{depth_translation};
   my $re = first { $_ ne '#DEFAULT#' && $step_plus_delimiter =~ /$_/; } (keys %$dt);
   $re //= '#DEFAULT#';

   return $dt->{$re};
}

sub shift_path_str {
   my ($self, $pathref, $depth) = @_;

   my $orig_path = $$pathref;

   my $bp = $self->blueprint;
   my $hash_re  = $bp->{hash_step_regexp};
   my $array_re = $bp->{array_step_regexp};
   my $delim_re = $bp->{delimiter_regexp};

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
      $hash_step->{key} = $bp->{unescape_sub}->($hash_step->{key})
         if ($bp->{unescape_sub} and $+{quote} =~ $bp->{unescape_quote_regexp});
   }
   else {
      $self->_set_error( sprintf "found unparsable step: '%s'", $_[1], $$pathref );
      return;
   }

   $$pathref =~ s/^($delim_re)//;

   # Re-piece the step + delimiter to use with _find_depth
   $hash_step->{depth} = $self->_find_depth( $hash_step->{step}.$1 );

   # If the path is not shifting at all, then something is wrong with REs
   if (length $$pathref == length $orig_path) {
      $self->_set_error( sprintf "found unshiftable step: '%s'", $$pathref );
      return;
   }

   return $hash_step;
}

### NOTE: basically the opposite of path_str2array ###
sub as_string {
   my $self = $_[0];

   my $dlp = $self->blueprint->{delimiter_placement};

   my $str = '';
   for my $i (0 .. $self->step_count - 1) {
      my $hash_step = $self->_path->[$i];
      my $next_step = ($i == $self->step_count - 1) ? undef : $self->_path->[$i+1];

      my $d = $hash_step->{depth};

      ### Left side delimiter placement
      if    (                   exists $dlp->{$d.'L'}) { $str .= $dlp->{$d.'L'};  }  # depth-specific
      elsif (not $next_step and exists $dlp->{'-1L'} ) { $str .= $dlp->{'-1L'};   }  # ending depth

      # Add the step
      $str .= $hash_step->{step};

      ### Right side delimiter placement
      my $L = substr($hash_step->{type}, 0, 1);
      if (exists $dlp->{$d.'R'}) {  # depth-specific (supercedes other right side options)
         $str .= $dlp->{$d.'R'};
      }
      elsif ($next_step) {          # ref-specific
         my $R = substr($next_step->{type}, 0, 1);
         $str .= $dlp->{$L.$R} if (exists $dlp->{$L.$R});
      }
      else {                        # ending depth
         if    (exists $dlp->{'-1R'}) { $str .= $dlp->{'-1R'}; }  # depth-specific
         elsif (exists $dlp->{$L})    { $str .= $dlp->{$L};    }  # ref-specific
      }
   }

   return $str;
}

#############################################################################
# Error methods

sub error      { $_[0]->has_stash_obj ? $_[0]->stash_obj->error             : $_[0]->error;             }
sub _set_error { $_[0]->has_stash_obj ? $_[0]->stash_obj->_set_error($_[1]) : $_[0]->_set_error($_[1]); }

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

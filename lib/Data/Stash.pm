package Data::KeyPath;

# VERSION
# ABSTRACT: Expand/flatten/search paths

#############################################################################
# Modules

use sanity;
use Moo;
use MooX::Types::MooseLike 0.18;  # AnyOf support
use MooX::Types::MooseLike::Base qw(Str ArrayRef HashRef CodeRef RegexpRef InstanceOf AnyOf Maybe);

use Hash::Merge;
use Try::Tiny;
use Scalar::Util qw( blessed );

use Data::KeyPath::Path;

use namespace::clean;
no warnings 'uninitialized';

#############################################################################
# Custom Hash::Merge behaviors

my $default_behavior = 'LEFT_PRECEDENT_STRICT_ARRAY_INDEX';

Hash::Merge::specify_behavior(
   {
      # NOTE: Undef is still considered 'SCALAR'.  Furthermore, Hash::Merge seems to convert it to ''.
      SCALAR => {
         SCALAR => sub { $_[1] },
         ARRAY  => sub {
            return $_[1] unless length $_[0];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'SCALAR', 'ARRAY', $_[0]);
         },
         HASH   => sub {
            return $_[1] unless length $_[0];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'SCALAR', 'HASH',  $_[0]);
         },
      },
      ARRAY => {
         SCALAR => sub {
            return $_[0] unless length $_[1];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'ARRAY', 'SCALAR', $_[1]);
         },
         ARRAY  => sub {
            # Handle arrays by index, not by combining
            my ($l, $r) = @_;
            $l->[$_] = $r->[$_] for (
               grep { defined $r->[$_] }
               (0 .. $#{$_[1]})
            );
            return $l;
         },
         HASH   => sub { die sprintf('mismatched type (%s vs. %s) found during merge', 'ARRAY', 'HASH'); },
      },
      HASH => {
         SCALAR => sub {
            return $_[0] unless length $_[1];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'HASH', 'SCALAR', $_[1]);
         },
         ARRAY  => sub { die sprintf('mismatched type (%s vs. %s) found during merge', 'HASH', 'ARRAY'); },
         HASH   => sub { Hash::Merge::_merge_hashes( $_[0], $_[1] ) },
      },
   },
   $default_behavior,
);

#############################################################################
# Attributes

#### TERMINOLOGY ####

# Path = A full string path to a destination within a tree
# Step = A path fragment, which indicates both the key and type
# Key  = Either a hash key or an array index
# Type = A ref type: either HASH or ARRAY (for now)

### TODO: Quote handling needs its own attr sub or switch ###

### FIXME: Make most of these required with no default ###

has hash_step_regexp => (
   is      => 'ro',
   isa     => RegexpRef,
   default => sub { qr/
      (?<key>\w+|(?=\.))|
      (?<quote>['"])(?<key> (?:
         # The (?!) is a fancy way of saying ([^\"\\]*) with a variable quote character
         (?>(?: (?! \\|\g{quote}). )*) |  # Most stuff (no backtracking)
         \\ \g{quote}                  |  # Escaped quotes
         \\ (?! \g{quote})                # Any other escaped character
      )* )\g{quote}|
      (?<key>^$)
   /x },
);

has array_step_regexp => (
   is      => 'ro',
   isa     => RegexpRef,
   default => sub { qr/\[(?<key>\d{1,5})\]/ },
);

has delimiter => (
   is      => 'ro',
   isa     => Str,
   default => sub { '.' },
);

has delimiter_regexp => (
   is      => 'ro',
   isa     => RegexpRef,
   default => sub { qr/(?:\.|(?=\[))/ },
);

has delimiter_placement => (
   is      => 'ro',
   isa     => Str,
   default => sub { 'HH|AH' },
);

#my $rule_coercion = sub {
#   my $rule = $_[0];
#   return sub {
#      my ($path, $key, $depth) = @_;
#      sprintf $rule, $_[0];
#   } unless (ref $rule);
#   $rule;
#};

has _merge_obj => (
   is      => 'rw',
   isa     => InstanceOf['Hash::Merge'],
   default => sub { Hash::Merge->new($default_behavior); },
   handles => { qw(
      merge             merge
      specify_behavior  specify_merge_behavior
      set_behavior      set_merge_behavior
   ) },
);

has error => (
   is      => 'rwp',
   isa     => Str,
   predicate => 1,
   clearer   => 1,
);

#############################################################################
# Methods

### EXPANSION ###

sub expand_hash {
   my ($self, $hash) = @_;
   $self->clear_error;

   my $root;  # not sure if it's a hash or array yet
   foreach my $path (sort keys %$hash) {
      my $branch = $self->expand_pathval($path, $hash->{$path}) || return;  # error already set

      # New root?
      unless (defined $root) {
         $root = $branch;
         next;
      }

      # Our merge behavior might die on us (or Hash::Merge itself)
      my $err;
      try   { $root = $self->merge($root, $branch); }
      catch { $err = $_; };

      if ($err) {
         # Add path to error
         $self->_set_error( sprintf "In path '%s', %s", $path, $err );
         return;
      }
   }

   return $root;
}

sub expand_pathval {
   my ($self, $path, $val) = @_;
   $self->clear_error;

   my ($root, $leaf, $hash_steps);
   $path = Data::KeyPath::Path->new(
      keypath_obj => $self,
      path => $path,
   ) // return;

   for my $i (0 .. $path->depth - 1) {
      my $hash_step = $path->_path->[$i];
      my $next_step = ($i == $path->depth - 1) ? undef : $path->_path->[$i+1];

      # Construct $root if we need to
      $root = $leaf = ( $hash_step->{type} eq 'HASH' ? {} : [] ) unless ($i);

      # Add in the key, construct the next ref, and move the leaf forward
      my $type_str = substr($hash_step->{type}, 0, 1);
      $type_str   .= substr($next_step->{type}, 0, 1) if $next_step;

      my $key = $hash_step->{key};

      # (RIP for/when)
      if    ($type_str eq 'HH') { $leaf = $leaf->{$key} = {};   }
      elsif ($type_str eq 'HA') { $leaf = $leaf->{$key} = [];   }
      elsif ($type_str eq 'AH') { $leaf = $leaf->[$key] = {};   }
      elsif ($type_str eq 'AA') { $leaf = $leaf->[$key] = [];   }
      elsif ($type_str eq 'H')  {         $leaf->{$key} = $val; }
      elsif ($type_str eq 'A')  {         $leaf->[$key] = $val; }
   }

   return $root;
}

### FLATTENING ###

sub flatten_ref {
   my ($self, $ref) = @_;
   $self->clear_error;

   my $type = ref $ref;
   unless (defined $ref && !blessed $ref && $type =~ /HASH|ARRAY/) {
      $self->_set_error('Reference must be an unblessed HASH or ARRAY!');
      return;
   }

   return $self->flatten_refpath('', $ref, 0);
}

sub flatten_refpath {
   my ($self, $path, $ref) = @_;
   $path //= '';
   $self->clear_error;

   my $prh = { $path => $ref };  # single row answer

   return $prh if blessed $ref;  # down that path leads madness...
   my $type = ref $ref || return $prh;        # that covers SCALARs...
   return $prh unless $type =~ /HASH|ARRAY/;  # ...and all other endpoints

   # Bless the path
   unless (blessed $path) {
      $path = Data::KeyPath::Path->new(
         keypath_obj => $self,
         path => $path,
      ) // return;
   }

   if ($path->depth > 255) {  # XXX: Might already be checked with Path object
      $self->_set_error( sprintf "Too deep down the rabbit hole, stopped at '%s'", $path );
      return;
   }

   my $hash = {};
   my @keys = $type eq 'HASH' ? (keys %$ref) : (0 .. $#$ref);
   foreach my $key (@keys) {
      my $val = $type eq 'HASH' ? $ref->{$key} : $ref->[$key];

      # Add on to $path
      my $newpath = $path->clone;
      $newpath->push({
         type => $type,
         key  => $key,
      }) || return;  # error already defined

      # Recurse back to give us a full set of $path => $val pairs
      my $newhash = $self->flatten_refpath($newpath, $val) || return;  # error already defined

      # Merge (shallowly)
      ### FIXME: We are removing undef here, but we might want a switch for that ###
      $hash->{$_} = $newhash->{$_} for (grep { defined $newhash->{$_} } keys %$newhash);
   }

   return $hash;
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

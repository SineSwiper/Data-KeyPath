package Data::Stash;

# VERSION
# ABSTRACT: Expand/flatten/search paths

#############################################################################
# Modules

use sanity;
use Moo;
use MooX::Types::MooseLike 0.18;  # *Of support
use MooX::Types::MooseLike::Base qw(Str HashRef InstanceOf HasMethods);

use Class::Load 0.17 ('load_class');  # 0.17 = wheezy's version
use Hash::Merge;
use Try::Tiny;
use Scalar::Util qw( blessed );

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

has cache_obj => (
   is      => 'ro',
   isa     => HasMethods[qw(get set remove clear dump_as_hash)],
   default => sub {
      use Data::Stash::HashCache;
      Data::Stash::HashCache->new;
   },
   handles => [qw(get remove clear dump_as_hash)],
);

has path_class => (
   is      => 'ro',
   isa     => Str,
   default => sub { 'DZIL' },
   coerce  => sub {
      'Data::Path::'.$_[0] unless ($_[0] =~ s/^\=//);  # NOTE: kill two birds with one stone
   },
);

has path_options => (
   is      => 'ro',
   isa     => HashRef,
   default => sub { {
      auto_normalize => 1,
      auto_cleanup   => 1,
   } },
);

has error => (
   is      => 'rwp',
   isa     => Str,
   predicate => 1,
   clearer   => 1,
);

#############################################################################
# Pre/post-BUILD

sub BUILD {
   my $self = $_[0];

   # Load the path class
   load_class $self->path_class;

   return $self;
}

#############################################################################
# Methods

### CACHING ###

sub set {
   my ($self, $thing) = (shift, shift);

   # Key/data pair
   unless (ref $thing) {
      return $self->cache_obj->set($thing, @_);
   }

   # Flatten hash (hopefully)
   elsif (ref $thing eq 'HASH') {
      $self->cache_obj->set($_, $thing->{$_}, @_) for (keys %$thing);
      return 1;
   }

   # Blessed?!  Maybe somebody forgot to stringify it?
   elsif (blessed $thing and $_[0]) {
      return $self->cache_obj->set("$thing", @_);
   }

   # WTF is this?
   else {
      $self->_set_error( sprintf "Set can't process a %s", ref $thing );
      return;
   }
}

sub flatten_and_set {
   my ($self, $ref) = (shift, shift);
   my $hash = $self->flatten_ref($ref) || return;  # error already set
   return $self->set($hash, @_);
}

sub get_and_expand {
   my ($self, $key) = (shift, shift);
   my $data = $self->get($key, @_);
   return $self->expand_pathval($key, $data);
}

sub get_all_and_expand {
   my ($self) = (shift);
   my $hash = $self->dump_as_hash(@_);
   return $self->expand_hash($hash);
}

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
   $path = $self->path_class->new(
      %{ $self->path_options },
      stash_obj => $self,
      path => $path,
   ) // return;

   for my $i (0 .. $path->step_count - 1) {
      my $hash_step = $path->_path->[$i];
      my $next_step = ($i == $path->step_count - 1) ? undef : $path->_path->[$i+1];

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

   # Blessed is the path
   unless (blessed $path) {
      $path = $self->path_class->new(
         %{ $self->path_options },
         stash_obj => $self,
         path => $path,
      ) // return;
   }

   if ($path->step_count > 255) {  # XXX: Might already be checked with Path object
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
         type  => $type,
         key   => $key,
         depth => $path->step_count ? 'X+1' : 0,
      }) || return;  # error already defined

      # Recurse back to give us a full set of $path => $val pairs
      my $newhash = $self->flatten_refpath($newpath, $val) || return;  # error already defined

      # Merge (shallowly)
      ### XXX: We are removing undef here, but we might want a switch for that ###
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

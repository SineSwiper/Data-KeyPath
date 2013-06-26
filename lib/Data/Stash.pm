package Data::Stash;

# VERSION
# ABSTRACT: Stash objects with "split serialization" capabities

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
      # NOTE: Undef is still considered 'SCALAR'.
      SCALAR => {
         SCALAR => sub { $_[1] },
         ARRAY  => sub {
            return $_[1] unless defined $_[0];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'SCALAR', 'ARRAY', $_[0]);
         },
         HASH   => sub {
            return $_[1] unless defined $_[0];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'SCALAR', 'HASH',  $_[0]);
         },
      },
      ARRAY => {
         SCALAR => sub {
            return $_[0] unless defined $_[1];
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
            return $_[0] unless defined $_[1];
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
   isa     => HasMethods[qw(get set remove clear dump_as_hash get_keys)],
   default => sub {
      use Data::Stash::HashCache;
      Data::Stash::HashCache->new;
   },
   handles => { qw(
      clear         remove_all
      get_keys      get_paths
      dump_as_hash  get_all
   ) },
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

   # Path/data pair
   unless (ref $thing) {
      return $self->cache_obj->set($thing, @_);
   }

   # Flatten hash (hopefully)
   elsif (ref $thing eq 'HASH') {
      if ($self->cache_obj->can('set_multi')) {
         $self->cache_obj->set_multi($thing, @_);
      }
      else {
         $self->cache_obj->set($_, $thing->{$_}, @_) for (keys %$thing);
      }
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

sub get {
   my ($self, $thing) = (shift, shift);

   my $paths = $self->_parse_path_arg($thing);

   unless (defined $paths) {
      $self->_set_error( sprintf "Get can't process a %s", ref $thing );
      return;
   }
   elsif (@$paths == 1) {
      return $self->cache_obj->get($paths->[0], @_);
   }

   # If this cache is CHI-like, use the (potentially) more efficient method
   my $hash = $self->cache_obj->can('get_multi_hashref') ?
      $self->cache_obj->get_multi_hashref(@$paths) :
      { map { $_ => $self->get($_, @_) } @$paths }
   ;

   return $hash;
}

sub _parse_path_arg {
   my ($self, $thing) = (shift, shift);

   my @paths;

   # Path
   unless (ref $thing)            { return [ $thing ]; }
   # Array of keys
   elsif (ref $thing eq 'ARRAY')  { return [ @$thing ]; }
   # RE to search against
   elsif (ref $thing eq 'Regexp') { return [ grep { $thing } ($self->get_paths) ]; }
   # Blessed?!  Maybe somebody forgot to stringify it?
   elsif (blessed $thing)         { return [ "$thing" ]; }
   # WTF is this?
   else                           { return; }
}

sub get_and_expand {
   my ($self, $thing) = (shift, shift);
   my $data = $self->get($thing, @_);

   if (not ref $thing or blessed $thing) {  # single
      return $self->expand_pathval($thing, $data);
   }
   else {                                   # multi
      return $self->expand_hash($data);
   }
}

sub get_all_and_expand {
   my ($self) = (shift);
   my $hash = $self->get_all(@_);
   return $self->expand_hash($hash);
}

sub remove {
   my ($self, $thing) = (shift, shift);

   my $paths = $self->_parse_path_arg($thing);

   unless (defined $paths) {
      $self->_set_error( sprintf "Remove can't process a %s", ref $thing );
      return;
   }
   elsif (@$paths == 1) {
      return $self->cache_obj->remove($paths->[0], @_);
   }

   # If this cache is CHI-like, use the (potentially) more efficient method
   if ($self->cache_obj->can('remove_multi')) { $self->cache_obj->remove_multi(@$paths); }
   else                                       { $self->cache_obj->remove($_, @_) for (@$paths); }

   return 1;
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

   return $self->flatten_refpath('', $ref);
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

   use Data::Stash;

   # Store a gopher stash
   my $stash = Data::Stash->new( path_class => 'DZIL' );
   my $serialized = {
      'gophers[0].holes'      => 3,
      'gophers[0].food.type'  => 'grubs',
      'gophers[0].food.count' => 7,

      'gophers[1].holes'      => 1,
      'gophers[1].food.type'  => 'fruit',
      'gophers[1].food.count' => 5,
   };
   $stash->set($serialized);

   my $more_gophers = [];
   $more_gophers->[2] = {
      holes => 1,
      food  => {
         type  => 'earthworms',
         count => 15,
      },
   };

   $stash->flatten_and_set( { gophers => $more_gophers } ) // die $stash->error;
   $stash->set( 'gophers[2].holes' => 2 );  # just added a hole

        $serialized = $stash->get_all            // die $stash->error;
   my $deserialized = $stash->get_all_and_expand // die $stash->error;

   $stash->remove_all;

   # Store into CHI caches
   use CHI;

   my $cache = CHI->new( driver => 'Memory', global => 1 );
   my $stash = Data::Stash->new(
      path_class => 'DZIL',
      cache_obj  => $cache,
   );
   $stash->flatten_and_set($deserialized) // die $stash->error;

= DESCRIPTION

Data::Stash is a module designed to store path/value pairs.  Unlike other similar modules, Data::Stash can handle the flattening of
data structures into path/value pairs using "split serialization", as well as turning these pairs back into data structures.
Data::Stash comes with its own set of path parsers to handle these transformations.

== What is split serialization?

Split serialization is a unique form of serialization that only serializes part of the data structure (as a path on the left side) and
leaves the rest of the data, typically a scalar, untouched (as a value on the right side).  Consider the gopher example above:

   my $deserialized = {
      gophers => [
         {
            holes => 3,
            food  => {
               type  => 'grubs',
               count => 7,
            },
         },
         {
            holes => 1,
            food  => {
               type  => 'fruit',
               count => 5,
            },
         },
         {
            holes => 2,
            food  => {
               type  => 'earthworms',
               count => 15,
            },
         }
      ],
   };

A full serializer, like [Data::Serializer] or [Data::Dumper], would turn the entire object into a string, much like the real code
above.  Or into JSON, XML, BerkleyDB, etc.  But, the end values would be lost in the stream.  If you were given an object like this,
how would you be able to store the data in an easy-to-access form for a caching module like [CHI]?  It requires key/value pairs.

Data::Stash uses split serialization to turn the data into a path like this:

   my $serialized = {
      'gophers[0].holes'      => 3,
      'gophers[0].food.type'  => 'grubs',
      'gophers[0].food.count' => 7,

      'gophers[1].holes'      => 1,
      'gophers[1].food.type'  => 'fruit',
      'gophers[1].food.count' => 5,

      'gophers[2].holes'      => 2,
      'gophers[2].food.type'  => 'earthworms',
      'gophers[2].food.count' => 15,
   };

Now, you can either stash the data into Data::Stash's own simple hash stash, or leverage a caching module like [CHI] to store the
data in different formats.

== How is this different than...?

* *[CHI]* - While CHI can be used with this module, CHI only supports storing key/value pairs, with the ability to tie a serializer to
keys and/or values.  Hopefully, split serialization support could be added into CHI, but it would require a different set of get/set
commands.  (So, maybe Data::Stash should continue to be the topmost module...)

* *[Data::DPath]* - DPath works in similar avenues as Data::Stash, but it's limited to its own DPath language, and is exclusively for
searching.

= CONSTRUCTOR

   # Defaults shown
   my $stash = Data::Stash->new(
      cache_obj    => Data::Stash::HashCache->new,
      path_class   => 'DZIL',
      path_options => {
         auto_normalize => 1,
         auto_cleanup   => 1,
      },
   );

Creates a new stash object.  Accepts the following arguments:

== cache_obj

   cache_obj => Data::Stash::HashCache->new
   cache_obj => CHI->new( driver => 'Memory', global => 1 )

A caching object of some sort.  This object is used to store the data that gets split into a set of path/value pairs.  Ideally, it
would be a [CHI] cache, but it can be anything that supports the following methods: {get set remove clear dump_as_hash get_keys}.

Default is a fresh [Data::Stash::HashCache] object.

== path_class

   path_class => 'File::Unix'
   path_class => '=MyApp::Data::Path::Foobar'

Class used to create new [path objects|Data::Path::Role::Path] for path parsing.  With a {=} prefix, it will use that as the full
class.  Otherwise, the class will be intepreted as {Data::Path::$class}.

Default is [DZIL|Data::Path::DZIL].

== path_options

   path_options => {
      auto_normalize => 1,
      auto_cleanup   => 1,
   }

Hash of options to pass to new path objects.  Typically, the default set of options are recommended to ensure a more commutative
path to be able to reuse in a get request.

= METHODS

### FIXME: Needs definition of terms, including $ref ###
### FIXME: Needs MOAR examples! ###

== Caching

These methods cover the getting and setting of stash data.  Ideally, you won't have to use anything beyond this section.

=== get

   my $data       = $stash->get($path, @options);
   my $serialized = $stash->get(qr/^\Q$path\E/, @options);
   my $serialized = $stash->get(\@paths, @options);

Get single or multiple values.  For single values, requires a path as a scalar, and returns the value for that path.

For multiple values, will accept either a Regexp to search the paths against, or an arrayref of paths.  (Admittedly, searching by
Regexp is not as flashy as a XPath-like string, but it's efficient and gets the job done.)  Returns a hashref of serialized
path/data pairs.

If your cache object has {get_multi_hashref} (like [CHI]), it will use the method to its advantage.  Note that it won't pass the extra
arguments in that case.

=== get_all

   my $serialized = $stash->get_all(@options_if_any);

Handle to the {dump_as_hash} method in your caching object, working exactly like it would there.

In the context of Data::Stash, this would be used to acquire the entire stash, in serialized form.  If you need it deserialized,
use [/get_all_and_expand] instead.

=== get_paths

   my @paths = $stash->get_paths(@options_if_any);

Handle to the {get_keys} method in your caching object, working exactly like it would there.

=== get_and_expand

   my $deserialized_item  = $stash->get_and_expand($path, @options);
   my $deserialized_items = $stash->get_and_expand(qr/^\Q$path\E/, @options);
   my $deserialized_items = $stash->get_and_expand(\@paths, @options);

Similar to the [/get] method, but expands the result into its deserialized form.  If using the single path, you will only get a
single "tree" from this.  For example:

   # Single path
   my $hash = $stash->get_and_expand('gophers[1].food.count');

   # Returns:
   $hash = {
      gophers => [
         undef,
         {
            food  => {
               count => 5,
            },
         }
      ],
   };

   # Regexp search
   my $hash = $stash->get_and_expand(qr/^gophers\[\d+\]\.food/);

   # Returns:
   my $deserialized = {
      gophers => [
         {
            food  => {
               type  => 'grubs',
               count => 7,
            },
         },
         {
            food  => {
               type  => 'fruit',
               count => 5,
            },
         },
         {
            food  => {
               type  => 'earthworms',
               count => 15,
            },
         }
      ],
   };

=== get_all_and_expand

   my $deserialized = $stash->get_all_and_expand;

Similar to the [/get_all] method, but expands the path/data hash into its deserialized form.

=== set

   $stash->set($path, $value, @options);
   $stash->set($serialized, @options);

Set single or multiple path/value pairs.  For multiple values, requires a hashref with path/value pairs.

If your cache object has {set_multi} (like [CHI]), it will use the method to its advantage.

=== flatten_and_set

   $stash->flatten_and_set($deserialized, @options);

Similar to the [/set] method, but flattens the result into its serialized form prior to the set operation.  Either single or multiple
path/value pairs could come from the flatten operation, depending on how the hash tree (deserialized argument) is populated.

=== remove

   $stash->remove($path, @options_if_any);
   $stash->remove(qr/^\Q$path\E/, @options_if_any);
   $stash->remove(\@paths, @options_if_any);

Remove single or multiple values.  For single values, requires a path as a scalar.

For multiple values, will accept either a Regexp to search the paths against, or an arrayref of paths.  (Admittedly, searching by
Regexp is not as flashy as a XPath-like string, but it's efficient and gets the job done.)

If your cache object has {remove_multi} (like [CHI]), it will use the method to its advantage.  Note that it won't pass the extra
arguments in that case.

=== remove_all

   $stash->remove_all(@options_if_any);

Handle to the {clear} method in your caching object, working exactly like it would there (ie: clean out the entire cache).

== Expansion and Flattening

These methods are mostly utilitarian, used by the above caching functions to serialize/deserialize the data sets.  Unless you are
going to forgo the main stash functionality, you probably don't need to use these methods.

=== expand_hash

   my $deserialized = $stash->expand_hash($serialized);

Expands/deserializes a hash of path/data pairs.  Returns the expanded object, which it usually a hashref, but might be an arrayref.
For example:

   # Starts with an array
   my $serialized = {
      '[0].thingy' => 1,
      '[1].thingy' => 2,
   };
   my $deserialized = $stash->expand_hash($serialized);

   # Returns:
   $deserialized = [
      { thingy => 1 },
      { thingy => 2 },
   ];

=== expand_pathval

   my $deserialized = $stash->expand_pathval($path, $value);

Expands/deserializes a single path/data pair.  Returns the expanded object.

=== flatten_ref

   my $serialized = $self->flatten_ref($deserialized);

Flattens/serializes a ref.  Returns a serialized hashref of path/value pairs.

=== flatten_refpath

   my $serialized = $self->flatten_refpath($path_prefix, $deserialized);

   # flatten_ref is basically this with some extra sanity checks
   my $serialized = $self->flatten_refpath('', $deserialized);

The real workhorse for {flatten_ref}.  Recursively dives down the different pieces of the deserialized tree and eventually comes back
with the serialized hashref.  The path prefix can be used for prepending all of the paths returned in the serialized hashref.

=== merge

   my $newhash = $stash->merge($hash1, $hash2);

Merges two hashes.  This is a direct handle to {merge} from an (internal) [Hash::Merge] object, and is used by [/expand_hash] to
combine individual expanded objects.

=== set_merge_behavior

Handle to {set_behavior} from the (internal) [Hash::Merge] object.  *Advanced usage only!*

Data::Stash uses a special custom type called {LEFT_PRECEDENT_STRICT_ARRAY_INDEX}, which properly handles array indexes and dies
on any non-array-or-hash refs.

=== specify_merge_behavior

Handle to {specify_behavior} from the (internal) [Hash::Merge] object.  *Advanced usage only!*

=== error

= CAVEATS

== Undefined values

Flattening will remove path/values if the value is undefined.  This is to clean up unused array values that appeared as holes in a
sparse array.  For example:

   # From one of the basic tests
   my $round_trip = $stash->flatten_ref( $stash->expand_pathval(
      'a[0][1][1][1][1][2].too' => 'long'
   ) );

   # Without undef removal, this returns:
   $round_trip = {
      'a[0][0]'                 => undef,
      'a[0][1][0]'              => undef,
      'a[0][1][1][0]'           => undef,
      'a[0][1][1][1][0]'        => undef,
      'a[0][1][1][1][1][0]'     => undef,
      'a[0][1][1][1][1][1]'     => undef,
      'a[0][1][1][1][1][2].too' => 'long',
   };

== Refs in split serialization

Split serialization works by looking for HASH or ARRAY refs and diving further into them, adding path prefixes as it goes down.  If
it encounters some other ref (like a SCALAR), it will stop and consider that to be the value for that path.  In terms of ref parsing,
this means two things:

0 Only HASH and ARRAYs can be explained deeper.
0 If you have a HASH or ARRAY as a "value", serialization cannot tell the difference and it will be included in the path.

The former isn't that big of a problem, since deeper dives with other kinds of refs are either not possible or dangerous (like CODE).

The latter could be a problem if you started with a hashref with a path/data pair, expanded it, and tried to flatten it again.  This
can be solved by protecting the hash with a REF.  Consider this example:

   my $round_trip = $stash->flatten_ref( $stash->expand_pathval(
      'a[0]' => { your => 'hash' }
   ) );

   # Returns:
   $round_trip = {
      'a[0].your' => 'hash',
   };

   # Now protect the hash
   my $round_trip = $stash->flatten_ref( $stash->expand_pathval(
      'a[0]' => \{ your => 'hash' }
   ) );

   # Returns:
   $round_trip = {
      'a[0]' => \{ your => 'hash' }
   };

== Sparse arrays and memory usage

= TODO

Better searching.  The expanding, flattening, caching, and the pathing is done, and Regexp mode in {get} does the trick in most cases.
But, this module still needs a good XPath-style wildcard search, which would make it a lot cooler.  This requires a lot of work on the
pathing to get it to work right, though.

= SEE ALSO

### Ruler #######################################################################################################################12345

Other modules...

= ACKNOWLEDGEMENTS

Kent Fredric for getting me started on the basic idea.

=end wikidoc

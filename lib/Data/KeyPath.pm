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
            return $_[1] unless $_[0];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'SCALAR', 'ARRAY', $_[0]);
         },
         HASH   => sub {
            return $_[1] unless $_[0];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'SCALAR', 'HASH',  $_[0]);
         },
      },
      ARRAY => {
         SCALAR => sub {
            return $_[0] unless $_[1];
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
            return $_[0] unless $_[1];
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

my $rule_coercion = sub {
   my $rule = $_[0];
   return sub { ($_[0] =~ $rule) ? $1 : undef; } if (ref $_[0] eq 'Regexp');
   $rule;
};

has key_split_expand_rule => (
   is      => 'ro',
   isa     => AnyOf[RegexpRef, CodeRef],
   default => sub { qr/\.|(?<=\]|\w)\.?(?=\[)/ },
   coerce  => sub {
      my $rule = $_[0];
      return sub { split $rule, $_[0]; } if (ref $_[0] eq 'Regexp');
      $rule;
   },
);

has hash_expand_rule => (
   is      => 'ro',
   isa     => AnyOf[RegexpRef, CodeRef],
   default => sub { qr/^(\w*)$/ },
   coerce  => $rule_coercion,
);

has array_expand_rule => (
   is      => 'ro',
   isa     => AnyOf[RegexpRef, CodeRef],
   default => sub { qr/^\[(\d+)\]$/ },
   coerce  => $rule_coercion,
);

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
   isa     => Maybe[Str],
   predicate => 1,
   clearer   => 1,
);

#############################################################################
# Pre/post-BUILD

#around BUILDARGS => sub {
#   my ($orig, $self) = (shift, shift);
#   my $hash = shift;
#   $hash = { $hash, @_ } unless ref $hash;
#
#   ### INSERT CODE HERE ###
#   
#   $orig->($self, $hash);
#};

#############################################################################
# Methods
   use Devel::Dwarn;

sub expand_hash {
   my ($self, $hash) = @_;
   $self->clear_error;
   
   Dwarn $hash;
   
   my $root;  # not sure if it's a hash or array yet
   foreach my $path (sort keys %$hash) {
      my $path_obj = $self->expand_path($path, $hash->{$path}) || return;  # error already set
      Dwarn { $path => $path_obj };
      
      # New root?
      unless (defined $root) {
         $root = $path_obj;
         next;
      }
      
      # Our merge behavior might die on us (or Hash::Merge itself)
      my $err;
      try   { $root = $self->merge($root, $path_obj); }
      catch { $err = $_; };

      if ($err) {
         $self->_set_error( sprintf "In path '%s', %s", $path, $err );
         return;
      }
   }
   
   return $root;
}

sub expand_path {
   my ($self, $path, $val) = @_;
   $self->clear_error;

   my $root;
   my @path_steps = $self->key_split_expand_rule->($path);
   
   my $leaf;
   for my $i (0 .. $#path_steps) {
      my $step = [ @path_steps[$i,$i+1] ];
      pop @$step unless defined $step->[1];
      
      # Step analysis
      for my $j (0 .. $#$step) {
         my $str = $step->[$j];

         # NOTE: re-defining $step->[$j]
         $step->[$j] = $self->step_type($str) || do {
            $self->_set_error( sprintf "In path '%s', found unparsable step: '%s' (depth %u)", $path, $str, $i+$j );
            return;
         };
         $step->[$j]{str} = $str;
      }
      
      # Construct $root if we need to
      $root = $leaf = ( $step->[0]{type} eq 'HASH' ? {} : [] ) unless ($i);

      # Add in the key, construct the next ref, and move the leaf forward
      my $type_str = join('|', map { $_->{type} } @$step);
      my $key = $step->[0]{val};

      #Dwarn { before => {
      #   'path'.$i => $step,
      #   root => $root,
      #   leaf => $leaf,
      #   type => $type_str,
      #   key  => $key,
      #}};
      
      # (RIP for/when)
      if    ($type_str eq 'HASH|HASH')   { $leaf = $leaf->{$key} = {};   }
      elsif ($type_str eq 'HASH|ARRAY')  { $leaf = $leaf->{$key} = [];   }
      elsif ($type_str eq 'ARRAY|HASH')  { $leaf = $leaf->[$key] = {};   }
      elsif ($type_str eq 'ARRAY|ARRAY') { $leaf = $leaf->[$key] = [];   }
      elsif ($type_str eq 'HASH')        {         $leaf->{$key} = $val; }
      elsif ($type_str eq 'ARRAY')       {         $leaf->[$key] = $val; }

      #Dwarn { after => {
      #   root => $root,
      #   leaf => $leaf,
      #}};
      
   }

   return $root;
}

sub step_type {
   my ($self, $step) = @_;
   
   my $val;
   my $type =
      defined($val = $self->hash_expand_rule ->($step)) ? 'HASH'  :
      defined($val = $self->array_expand_rule->($step)) ? 'ARRAY' : return;

   return {
      type => $type,
      val  => $val,
   };
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

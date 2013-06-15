package Data::Stash::HashCache;

# VERSION
# ABSTRACT: Very simple default hash cache for Data::Stash

#############################################################################
# Modules

use sanity;
use Moo;
use MooX::Types::MooseLike 0.18;  # *Of support
use MooX::Types::MooseLike::Base qw(Str HashRef InstanceOf HasMethods);

use Storable qw( dclone );

use namespace::clean;
no warnings 'uninitialized';

#############################################################################
# Attributes

has _cache => (
   is       => 'rw',
   isa      => HashRef,
   default  => sub { { } },
   init_arg => undef,
);

#############################################################################
# Methods

sub set {
   my ($self, $key, $data) = @_;
   $self->_cache->{$key} = $data;
}

sub get {
   my ($self, $key) = @_;
   $self->_cache->{$key};
}

sub remove {
   my ($self, $key) = @_;
   delete $self->_cache->{$key};
}

sub clear {
   my ($self) = @_;
   $self->_cache({});
}

sub dump_as_hash {
   my ($self) = @_;
   dclone($self->_cache);
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

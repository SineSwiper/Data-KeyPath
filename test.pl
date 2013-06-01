use Data::KeyPath;
use Devel::Dwarn;

my $hash = {
   'a[4]' => 2,
   'a[1].b.c.d' => 3,
   'a[3].turnip' => 4,
};

my $dkp = Data::KeyPath->new;
my $tree = $dkp->expand_hash($hash) || die $dkp->error;
Dwarn $tree;

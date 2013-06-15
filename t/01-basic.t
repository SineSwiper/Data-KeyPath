use Data::Stash;
use Test::More tests => 25;

my ($hash, $tree);
my $dso = new_ok('Data::Stash');

test_both_ways(
   {
      'a[4]' => 2,
      'a[1].b.c.d' => 3,
      'a[3].turnip' => 4,
      'a[0][1][1][1][1].[2]' => { too => 'long' },
   },
   {
      a => [
         [ undef, [ undef, [ undef, [ undef, [ undef, undef, { too => "long" } ] ] ] ] ],
         {
            b => {
               c => { d => 3 }
            }
         },
         undef,
         { turnip => 4 },
         2
      ]
   },
   {
      'a[0][1][1][1][1][2].too' => 'long',
      'a[1].b.c.d' => 3,
      'a[3].turnip' => 4,
      'a[4]' => 2,
   },
   'Basic hash',
);

test_both_ways(
   {
      '[4]' => 2,
      '[1].b.c.d' => 3,
      '[3].turnip' => 4,
      '[0][1][1][1][1].[2]' => { too => 'long' },
   },
   [
      [ undef, [ undef, [ undef, [ undef, [ undef, undef, { too => "long" } ] ] ] ] ],
      {
         b => {
            c => { d => 3 }
         }
      },
      undef,
      { turnip => 4 },
      2
   ],
   {
      '[4]' => 2,
      '[1].b.c.d' => 3,
      '[3].turnip' => 4,
      '[0][1][1][1][1][2].too' => 'long',
   },
   'Array-is-first',
);

test_both_ways(
   {
      q{"This can't be a terrible mistake"[0].value} => 2,
      q{'"Oh, but it can..." said the spider'.[0].value} => 3,
   },
   {
      "This can't be a terrible mistake"    => [ { value => 2 } ],
      '"Oh, but it can..." said the spider' => [ { value => 3 } ],
   },
   {
      q{"This can't be a terrible mistake"[0].value} => 2,
      q{"\"Oh, but it can...\" said the spider"[0].value} => 3,
   },
   'Quoted hash',
);

test_both_ways(
   {
      'a.b...c[0].""."".' . "''" => 2,
   },
   {
      a => { b => { '' => { '' => { c => [ { '' => { '' => { '' => 2 } } } ] } } } }
   },
   {
      'a.b.""."".c[0].""."".""' => 2,
   },
   'Zero-length keys',
);

sub test_both_ways {
   my ($hash_start, $expect_tree, $expect_hash, $name) = @_;

   # expand_hash
   my $tree = $dso->expand_hash($hash_start);
   ok(!$dso->has_error, 'No error') || diag $dso->error;
   is_deeply($tree, $expect_tree, $name.' expanded correctly') || diag explain $tree;

   # flatten_ref
   my $hash = $dso->flatten_ref($tree);
   ok(!$dso->has_error, 'No error') || diag $dso->error;
   is_deeply($hash, $expect_hash, $name.' flattened correctly') || diag explain $hash;

   # expand_hash
   $tree = $dso->expand_hash($hash);
   ok(!$dso->has_error, 'No error') || diag $dkp->error;
   is_deeply($tree, $expect_tree, $name.' re-expanded correctly') || diag explain $tree;
}

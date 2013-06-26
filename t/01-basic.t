use Data::Stash;
use Test::More tests => 25;

use lib 't/lib';
use StashTest;

my ($hash, $tree);
my $dso = new_ok('Data::Stash');

test_both_ways($dso,
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

test_both_ways($dso,
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

test_both_ways($dso,
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

test_both_ways($dso,
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

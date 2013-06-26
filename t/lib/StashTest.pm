package StashTest;

use Test::More;

use base 'Exporter';

our @EXPORT = qw(test_both_ways test_pathing);

sub test_both_ways {
   my ($dso, $hash_start, $expect_tree, $expect_hash, $name) = @_;

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
   ok(!$dso->has_error, 'No error') || diag $dso->error;
   is_deeply($tree, $expect_tree, $name.' re-expanded correctly') || diag explain $tree;
}

sub test_pathing {
   my ($dso, $list, $expect_list, $name) = @_;

   for (my $i = 0; $i < @$list; $i++) {
      my ($path_str, $expect_str) = ($list->[$i], $expect_list->[$i]);

      my $path = $dso->path_class->new(
         %{ $dso->path_options },
         stash_obj => $dso,
         path => $path_str,
      ) // do {
         diag $dso->error;
         fail;
      };

      cmp_ok($path->as_string, 'eq', $expect_str, $name.' --> '.$path_str.' compare correctly');
   }
}

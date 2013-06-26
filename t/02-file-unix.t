use Data::Stash;
use Test::More tests => 11;

use lib 't/lib';
use StashTest;

use utf8;

my ($hash, $tree);
my $dso = new_ok('Data::Stash', [ path_class => 'File::Unix' ]);

test_pathing($dso,
   [qw(
      /
      ..
      .
      /etc/foobar.conf
      ../..///.././aaa/.///bbb/ccc/../ddd
      /home/bbyrd///foo/bar.txt
      foo/////bar
      ////root
      var/log/turnip.log
   ),
      '/root/FILENäME NIGHTMäRE…/…/ﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝ.conf',
   ],
   [qw(
      /
      ..
      .
      /etc/foobar.conf
      ../../../aaa/bbb/ddd
      /home/bbyrd/foo/bar.txt
      foo/bar
      /root
      var/log/turnip.log
   ),
      '/root/FILENäME NIGHTMäRE…/…/ﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝ.conf',
   ],
   'Basic UNIX path set',
);

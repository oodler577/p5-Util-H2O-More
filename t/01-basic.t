use strict;
use warnings;

use lib './lib';
use Foo;
require Data::Dumper;

my $foo = Foo->new(some => q{thing});
print $foo->some . qq{\n};
$foo->bar(1);
print qq{Print accessor\n};
print $foo->bar . qq{\n};
print $foo->fart . qq{\n};

print Data::Dumper::Dumper($foo);

my $foo2 = Foo->new(some => q{thang});
print $foo2->some . qq{\n};
$foo2->bar(2);
print qq{Print accessor\n};
print $foo2->bar . qq{\n};
print $foo2->burp . qq{\n};

my $foo_deeply = Foo->new_deeply(some => q{thing}, more => { other => q{things} });

print Data::Dumper::Dumper($foo_deeply);

print qq{Print accessor\n};

print $foo_deeply->more->other . qq{\n};

$foo_deeply->bar(1);

print $foo_deeply->bar . qq{\n};

1;

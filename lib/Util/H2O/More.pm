use strict;
use warnings;

package Util::H2O::More;
use parent q/Exporter/;
our @EXPORT_OK = (qw/baptise baptise_deeply/);

use Util::H2O qw/h2o/;
use feature 'state';

# maintains basically a count to create non-colliding
# unique $pkg names (basically what Util::H2O::h2o does
# if $pkg is not specified using -class

sub _uuid {
    state $uuid;
    return $uuid++;
}

# non-recursive option
sub baptise ($$@) {
    my ( $ref, $pkg, @default_accessors ) = @_;

    my $self;
    my $real_pkg = sprintf( qq{%s::_%s}, $pkg, _uuid );

    # uses -isa to inherit from $pkg; -class to bless with a package name
    # derived from $pkg
    $self = h2o -isa => $pkg, -class => $real_pkg, $ref, @default_accessors;

    return $self;
}

# recursive option
sub baptise_deeply ($$@) {
    my ( $ref, $pkg, @default_accessors ) = @_;

    my $self;
    my $real_pkg = sprintf( qq{%s::_%s}, $pkg, _uuid );

    # uses -isa to inherit from $pkg; -class to bless with a package name
    # derived from $pkg
    $self = h2o -recurse, -isa => $pkg, -class => $real_pkg, $ref, @default_accessors;

    return $self;
}

1;

=head1 NAME

Util::H2O::More - collection of handy Perl objects utilities
using C<Util::H2O>'s C<h2o> utility. Chiefly, the idea is to
provide a I<better> C<bless>, plus some other handy methods
for performing precise surgery on blessed hash references.

=head1 SYNOPSIS

Creating a new module using C<baptise> instead of C<bless>,
which means it includes accessors (thanks to C<Util::H2O::h2o>.

    use strict;
    use warnings;
    
    package Foo::Bar;

    use Util::H2O::More qw/baptise/;
     
    sub new {
      my $pkg    = shift;
      my %opts   = @_;
      my $self = baptise \%opts, $pkg, qw/foo haz herp derpes/;
      return $self;
    }
    
    1;

Then on a client script,

    use strict;
    use warnings;
     
    use Foo::Bar;
     
    my $foo = Foo->new(some => q{thing});
     
    print $foo->some . qq{\n};
     
    # set bar via default accessor
    $foo->bar(1);
     
    print $foo->bar . qq{\n};

=head1 DESCRIPTION

The primary method, C<baptise>, essentially provides the same
interface as the core keyword C<bless> with an additional third
parameter where one may specify a list of default accessors.

=head1 METHODS

=over 4

=item C<baptise $href, $pkg, LIST>

Takes the same first 2 parameters as C<bless>; with the addition
of a list that defines a set of default accessors that do not
rely on the top level keys of the provided hash reference.

=item C<baptise_deeply $href, $pkg, LIST>

Like C<baptise>, but creates accessors recursively for a nested
hash reference. Uses C<h2o>'s C<-recurse> flag.

=back

=head1 DEPENDENCIES

Requires C<Util::H2O> because this module is effectively a wrapper
around C<h2o>.

=head1 LICENSE AND COPYRIGHT 

Perl/perl

=head1 AUTHOR

Oodler 577 L<< <oodler@cpan.org> >> 

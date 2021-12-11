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

Util::H2O::More - like if C<bless> created accessors for you.
Intended for I<hash reference>-based Perl OOP only.

=head1 SYNOPSIS

Creating a new module using C<baptise> instead of C<bless>,
which means it includes accessors (thanks to C<Util::H2O::h2o>).

    use strict;
    use warnings;
    
    package Foo::Bar;

    # exports 'h2o' also
    use Util::H2O::More qw/baptise/;
     
    sub new {
      my $pkg    = shift;
      my %opts   = @_;
      my $self = baptise \%opts, $pkg, qw/bar haz herp derpes/;
      return $self;
    }
    
    1;

Then on a client script,

    use strict;
    use warnings;
     
    use Foo::Bar;
     
    my $foo = Foo::Bar->new(some => q{thing});
     
    print $foo->some . qq{\n};
     
    # set bar via default accessor
    $foo->bar(1);
     
    print $foo->bar . qq{\n};

=head1 DESCRIPTION

The primary method, C<baptise>, essentially provides the same
interface as the core keyword C<bless> with an additional I<slurpy>
third parameter where one may specify a list of default accessors.

=head2 Why Was This Created?

The really short answer: because C<h2o> doesn't play nice
inside of the traditional Perl OOP constructor (C<new>) idiom.
This is not C<h2o>'s fault. This is my fault for wanting to use
it to do something it was never meant to do.

Implied above is that I wanted to maintain the usage pattern of
C<bless>, but extend it to include the generation of accessors.
I wanted a I<better bless>.

The long answer...

C<h2o> is an deceptively powerful tool that, above all, makes
it I<easy> and I<fun> to add accessors to ad hoc hash references
that many Perl developers like to use and that get emitted,
I<unblessed> by many popular modules. For example, C<HTTP::Tiny>,
C<Web::Scraper>, and the more common I<select%> methods C<DBI>
flavors implement. 

The usage pattern of C<h2o> begs it to be able to support being
used as a I<drop in> replacement for C<bless>. However, this is
not C<h2o>'s original intent and it will not work as a I<better
bless>. But is does a fine job as serving as the I<basis> for a
I<better bless>.

=head1 METHODS

=over 4

=item C<baptise $hash_ref, $pkg, LIST>

Takes the same first 2 parameters as C<bless>; with the addition
of a list that defines a set of default accessors that do not
rely on the top level keys of the provided hash reference.

=item C<baptise_deeply $hash_ref, $pkg, LIST>

Like C<baptise>, but creates accessors recursively for a nested
hash reference. Uses C<h2o>'s C<-recurse> flag.

Note: The accessors created in the nested hashes are handled
directly by C<h2o> by utilizing the C<-recurse> flag. This means
that they will necessarily be blessed using the unchangable
behavior of C<h2o>, which maintains the name space of C<Util::H2O::_$hash>
even if C<h2o> is passed with the C<-isa> and C<-class> flags,
which are both utilized to achieve the effective outcome of
C<baptise> and C<bastise_deeply>.


=back

=head1 EXTERNAL METHODS

=over 4

=item C<h2o>

Because C<Util::H2O::More> exports C<h2o> as the basis for its
operations, C<h2o> is also available without needing to qualify
its full name space.

=back

=head1 DEPENDENCIES

Requires C<Util::H2O> because this module is effectively a wrapper
around C<h2o>.

=head1 BUGS

Yes, buyer beware.

=head1 LICENSE AND COPYRIGHT 

Perl/perl

=head1 AUTHOR

Oodler 577 L<< <oodler@cpan.org> >> 

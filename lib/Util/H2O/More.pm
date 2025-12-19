use strict;
use warnings;

package Util::H2O::More;
use parent q/Exporter/;
use Util::H2O ();

our @EXPORT_OK = (qw/baptise opt2h2o h2o o2h d2o o2d o2h2o ini2h2o ini2o h2o2ini HTTPTiny2h2o o2ini Getopt2h2o ddd dddie tr4h2o yaml2h2o yaml2o/);
our $VERSION = q{0.4.2};

use feature 'state';

# quick hack to export h2o, uses proper
# Util::H2O::h2o called with full namespace
sub h2o {
    return Util::H2O::h2o @_;
}

# maintains basically a count to create non-colliding
# unique $pkg names (basically what Util::H2O::h2o does
# if $pkg is not specified using -class
# monatomically increasing uuid
sub _uuid {
    state $uuid = 0;
    return ++$uuid;
}

# non-recursive option
sub baptise ($$@) {
    my ( $ref, $pkg, @default_accessors );
    my $pos0 = shift;

    # check pos0 for '-recurse'
    if ( $pos0 eq q{-recurse} ) {
        ( $ref, $pkg, @default_accessors ) = @_;
    }
    else {
        $ref = $pos0;
        ( $pkg, @default_accessors ) = @_;
    }

    my $self;
    my $real_pkg = sprintf qq{%s::_%s}, $pkg, _uuid;

    # uses -isa to inherit from $pkg; -class to bless with a package name
    # derived from $pkg
    if ( $pos0 eq q{-recurse} ) {
        $self = h2o -recurse, -isa => $pkg, -class => $real_pkg, $ref, @default_accessors;
    }
    else {
        $self = h2o -isa => $pkg, -class => $real_pkg, $ref, @default_accessors;
    }

    return $self;
}

# make keys legal for use as accessor, provides original keys via "__og_keys" accessor
sub tr4h2o($) {
    my $hash_ref    = shift;
    my $new_hashref = {};

    # List::Util::pairmap was not happy being require'd for some reason
    # so iterate and replace keys explicitly; store original key in resulting
    # hashref via __og_keys
    foreach my $og_k ( keys %$hash_ref ) {
        my $k = $og_k;
        $k =~ tr/a-zA-Z0-9/_/c;
        $new_hashref->{$k} = $hash_ref->{$og_k};

        # save old key via __og_keys
        $new_hashref->{__og_keys}->{$k} = $og_k;
    }
    return $new_hashref;
}

# preconditioner for use with Getopt::Long flags; returns just the flag name given
# a list of option descriptors, e.g., qw/option1=s option2=i option3/;

# Getopt to keys
sub opt2h2o(@) {
    my @getopt_def = @_;
    my @flags_only = map { m/([^=!|\s]+)/g; $1 } @getopt_def;
    return @flags_only;
}

# wrapper around opt2h2o (yeah!)
sub Getopt2h2o(@) {
    my $autoundef;
    if ( @_ && $_[0] && !ref$_[0] && $_[0]=~/^-autoundef/ ) {
      $autoundef = shift;
    }
    my ( $ARGV_ref, $defaults, @opts ) = @_;
    $defaults //= {};
    if ($autoundef) {
      $defaults->{AUTOLOAD} = sub {
        my $self = shift;
        our $AUTOLOAD;
        ( my $key = $AUTOLOAD ) =~ s/.*:://;
        die qq{Getopt2h2o: Won't set value for non-existing key. Need it? Let the module author know!\n} if @_;
        return undef;
      };
    }
    my $o = h2o -meth, $defaults, opt2h2o(@opts);
    require Getopt::Long;
    Getopt::Long::GetOptionsFromArray( $ARGV_ref, $o, @opts );    # Note, @ARGV is passed by reference
    return $o;
}

# general form of method used to give accessors to Config::Tiny in Util::H2O's
# POD documentation
sub o2h2o($) {
    my $ref = shift;
    return h2o -recurse, { %{$ref} };
}

# more specific helper app that uses Config::Tiny->read and o2h2o to get a config
# object back from an .ini; requries Config::Tiny
sub ini2h2o($) {
    my $filename = shift;
    require Config::Tiny;
    return o2h2o( Config::Tiny->read($filename) );
}

# back compat
sub ini2o($) {
    return ini2h2o(shift);
}

# write out the INI file
sub h2o2ini($$) {
    my ( $config, $filename ) = @_;
    require Config::Tiny;
    return Config::Tiny->new( Util::H2O::o2h $config)->write($filename);
}

# back compat
sub o2ini($$) {
    return h2o2ini( shift, shift );
}

# return a dereferences hash (non-recursive); reverse of `h2o'
sub o2h($) {
    $Util::H2O::_PACKAGE_REGEX = qr/::_[0-9A-Fa-f]+\z/;    # makes internal package name more generic for baptise created references
    my $ref = Util::H2O::o2h @_;
    if ( ref $ref ne q{HASH} ) {
        die qq{o2h: Could not fully remove top-level reference. Probably an issue with \$Util::H2O_PACKAGE_REGEX\n};
    }
    return $ref;
}

sub d2o(@);    # forward declaration to get rid of "too early" warning
sub a2o($);

# accepts '-autoundef' flag that will insert all keys/getters to be checked
# i.e., if (not $myref->doesntexist) { ... } rather than if (not exists $myref->{doesntexist}) { ... }
sub d2o(@) {
    my ($autoundef);
    # basically how Util::H2O::h2o does it, if we have more options
    # then we should use the `while` form of this ...
    if ( @_ && $_[0] && !ref$_[0] && $_[0]=~/^-autoundef/ ) {
      $autoundef = shift;
    }
    my $thing = shift;

    my $isa   = ref $thing;

    if ( $isa eq q{ARRAY} ) {
        a2o $thing;
        foreach my $element (@$thing) {
          if ($autoundef) { # 'd2o -autoundef, $hash'
            d2o $autoundef, $element;
          }
          else {
            d2o $element;
          }
        }
    }
    elsif ( $isa eq q{HASH} ) {
        foreach my $keys ( keys %$thing ) {
          if ($autoundef) { # 'd2o -autoundef, $hash'
            d2o $autoundef, $thing->{$keys};
          }
          else {
            d2o $thing->{$keys};
          }
        }
        if ($autoundef) { # 'd2o -autoundef, $hash'
          $thing->{AUTOLOAD} = sub {
            my $self = shift;
            our $AUTOLOAD;
            ( my $key = $AUTOLOAD ) =~ s/.*:://;
            die qq{d2o: Won't set value for non-existing key. Need it? Let the module author know!\n} if @_;
            return undef;
          };
          h2o -meth, $thing;
        }
        else {           # default behavior
          h2o $thing;
        }
    }
    return $thing;
}

# blesses ARRAY ref as a container and gives it some virtual methods
# useful in the context of containing HASH refs that get objectified
# by h2o
sub a2o($) {
    no strict 'refs';

    my $array_ref = shift;

    # uses lexical scop of the 'if' to a bless $array_ref (an ARRAY ref)
    # and assigns to it some virtual methods for making dealing with
    # the "lists of C<HASH> references easier, as a container

    my $a2o_pkg = sprintf( qq{%s::__a2o_%d::vmethods}, __PACKAGE__, int rand 100_000_000 );    # internal a2o

    bless $array_ref, $a2o_pkg;

    ## add vmethod to wrap around array_refs

    # return item at index INDEX
    my $GET = sub {
      my ( $self, $i ) = @_;
      return undef if $i > $#{$self}; # prevent ARRAY from growing just to get an undef back
      return $self->[$i];
    };
    *{"${a2o_pkg}::get"} = $GET;
    *{"${a2o_pkg}::i"}   = $GET;

    # return rereferenced ARRAY
    my $ALL = sub { my $self = shift; return @$self; };
    *{"${a2o_pkg}::all"} = $ALL;

    # returns value returned by the 'scalar' keyword, alias also to 'count'
    my $SCALAR = sub { my $self = shift; return scalar @$self; };
    *{"${a2o_pkg}::scalar"} = $SCALAR;
    *{"${a2o_pkg}::count"}  = $SCALAR;

    # 'push' will apply "d2o" to all elements pushed
    my $PUSH = sub { my ( $self, @i ) = @_; d2o \@i; push @$self, @i; return \@i };
    *{"${a2o_pkg}::push"} = $PUSH;

    # 'pop' intentionally does NOT apply "o2d" to anyarray_ref pop'd
    my $POP = sub { my $self = shift; return pop @$self };
    *{"${a2o_pkg}::pop"} = $POP;

    # 'unshift' will apply "d2o" to all elements unshifted
    my $UNSHIFT = sub { my ( $self, @i ) = @_; d2o \@i; unshift @$self, @i; return \@i };
    *{"${a2o_pkg}::unshift"} = $UNSHIFT;

    # 'shift' intentionally does NOT apply "o2d" to anyarray_ref shift'd
    my $SHIFT = sub { my $self = shift; return shift @$self };
    *{"${a2o_pkg}::shift"} = $SHIFT;

    return $array_ref;
}

# includes internal dereferencing so to be compatible
# with the behavior of Util::H2O::o2h
sub o2d($);    # forward declaration to get rid of "too early" warning

sub o2d($) {
    my $thing = shift;
    return $thing if not $thing;
    my $isa = ref $thing;
    if ( $isa =~ m/^Util::H2O::More::__a2o/ ) {
        my @_thing = @$thing;
        $thing = \@_thing;
        foreach my $element (@$thing) {
            $element = o2d $element;
        }
    }
    elsif ( $isa =~ m/^Util::H2O::_/ ) {
        foreach my $key ( keys %$thing ) {
            $thing->{$key} = o2d $thing->{$key};
        }
        $thing = Util::H2O::o2h $thing;
    }
    return $thing;
}

# handy, poor man's debug wrappers

sub ddd(@) {
    require Data::Dumper;
    foreach my $ref (@_) {
        print STDERR Data::Dumper::Dumper($ref);
    }
}

sub dddie(@) {
    require Data::Dumper;
    foreach my $ref (@_) {
        print STDERR Data::Dumper::Dumper($ref);
    }
    die qq{died due to use of dddie};
}

# YAML configuration support - may return more than 1 reference
sub yaml2h2o($) {
    require YAML;
    my $file_or_yaml = shift; # may be a file or a string
    my @yaml         = ();    # yaml can have multiple objects serialized, via ---

    # determine if YAML or file name
    my @lines = split /\n/, $file_or_yaml;

    # if a file, use YAML::LoadFile
    if ( @lines == 1 and -e $file_or_yaml ) {
        @yaml = YAML::LoadFile($file_or_yaml);
    }

    # if not a file, assume YAML string and use YAML::Load
    elsif ($lines[0] eq q{---}) {
        @yaml = YAML::Load($file_or_yaml);
    }

    # die because not supported content $file_or_yaml - it is neither
    else {
        die qq{Provided parameter looks like neither a file name nor a valid YAML snippet.\n};
    }

    # iterate over 1 or more serialized objects that were deserialized
    # from the YAML, applie C<d2o> to it due to the potential presence
    # of ARRAY references
    my @obs = ();
    foreach my $y (@yaml) {
        push @obs, d2o $y;
    }

    return @obs;
}

# back compat
sub yaml2o($) {
    return yaml2h2o(shift);
}

# NOTE: no h2o2yaml or o2yaml, but can add one if somebody needs it ... please file an issue on the tracker (GH these days)

# This method assumes a response HASH reference returned by HTTP::Tiny; so
# it looks for $ref->{content}, and if anything is found there it will attempt
# to turn it into a Perl data structure usin JSON::XS::Maybe::decode_json; it
# them applies "d2o -autoundef" to it; if the JSON decode fails, the error will
# be hidden silently and the original content will be retained in the provided
# response reference (also available via ->content by virtu of h2o being applied).
# To force the JSON decode error to propagate up so that it may be caught, use
# the "-autothrow" option, e.g.;
#   HTTPTiny2h2o -autothrow, $ref_with_bad_JSON; # propagates decode_json exception from "malformed" JSON
#   HTTPTiny2h2o $ref_with_bad_JSON;             # hides bad decode, "->content" accessor created to return original content
#   HTTPTiny2h2o $ref_with_good_JSON;            # h2o applied to $ref, "d2o -autoundef" applied to value of ->{content}
sub HTTPTiny2h2o(@) {
  my $autothrow;
  if ( @_ && $_[0] && !ref$_[0] && $_[0]=~/^-autothrow/ ) {
    $autothrow = shift;
  }
  my $ref = shift;
  if (ref $ref eq q{HASH} and exists $ref->{content}) {
    require JSON::MaybeXS; # tries to load the JSON module you want, (by default, exports decode_json, encode_json)
    h2o $ref, qw/content/;
    if ($ref->content) {
      # allows exception from decode_json to be raised if -autothrow
      # and the JSON is determined to be malformed
      if ($autothrow) {
        # the JSON decode will die on bad JSON
        my $JSON = JSON::MaybeXS::decode_json($ref->content);
        my $content= d2o -autoundef, $JSON;
        $ref->content($content);
      }
      # default is hide any malformed JSON exception, effectively
      # leaving the ->content untouched
      else {
        eval {
          # the JSON decode will die on bad JSON
          my $JSON = JSON::MaybeXS::decode_json($ref->content);
          my $content= d2o -autoundef, $JSON;
          $ref->content($content);
        }
      }
    }
    else {
      my $content= d2o -autoundef, {};
      $ref->content($content);
    }
  }
  else {
    die qq{Provided parameter must be a proper HASH reference returned by HTTP::Tiny that contains a 'content' HASH key.};
  }

  return $ref;
}

1;

__END__

=head1 NAME

Util::H2O::More - Practical helpers built on Util::H2O for turning hashes
and data structures into usable, accessor-based objects

=head1 SYNOPSIS

C<Util::H2O::More> builds on L<Util::H2O> and provides a collection of
utilities that make it easier to work with hashrefs, arrayrefs, configuration
files, command-line options, HTTP responses, YAML, and INI files — all using
simple accessor-style method calls instead of raw hash dereferencing.

The most visible feature is C<baptise>, which can be used as a drop-in
replacement for C<bless> while automatically creating accessors.

    package Foo::Bar;
    use Util::H2O::More qw/baptise/;

    sub new {
        my ($class, %opts) = @_;
        return baptise \%opts, $class, qw/name age active/;
    }

    my $obj = Foo::Bar->new(name => "Alice");
    say $obj->name;      # Alice
    $obj->active(1);

Beyond C<baptise>, this module provides helpers for:

=over 4

=item *
Turning deeply nested hashes and arrays into objects (C<d2o>)

=item *
Safer accessor use for optional or missing keys (C<-autoundef>)

=item *
Command-line parsing with object accessors (C<Getopt2h2o>)

=item *
INI and YAML configuration files (C<ini2h2o>, C<yaml2h2o>)

=item *
HTTP::Tiny responses with automatic JSON decoding (C<HTTPTiny2h2o>)

=item *
Converting objects back to plain Perl data (C<o2h>, C<o2d>)

=back

=head1 OVERVIEW

L<Util::H2O> makes it easy to attach accessors to hash references without
committing to a heavyweight OO framework. C<Util::H2O::More> builds on that
idea by providing practical, reusable patterns for common tasks:

=over 4

=item *
Replacing C<bless> with accessor-aware object creation

=item *
Working with nested API responses, DBI rows, and configuration trees

=item *
Reducing repetitive C<exists> and defensive dereferencing logic

=item *
Improving readability without changing data models

=back

This module is intentionally pragmatic. It favors clarity over ceremony and is
designed to be introduced incrementally into existing codebases.

=head1 WHICH FUNCTION SHOULD I USE?

=head2 Quick Decision Guide

=over 4

=item * Writing a constructor → C<baptise>

=item * One hashref → C<h2o>

=item * Nested data / JSON / DB rows → C<d2o>

=item * Missing keys acceptable → add C<-autoundef>

=item * Need plain Perl again → C<o2h> or C<o2d>

=back

=head1 MINIMAL CHEATSHEET

=head2 baptise

Constructor replacement:

    return baptise \%args, $class, qw/foo bar/;

=head2 h2o

Flat hashref → object:

    my $o = h2o { a => 1 };

=head2 d2o

Nested structures → navigable objects:

    my $o = d2o decode_json($json);

=head2 o2h / o2d

Undo objectification before serialization or framework boundaries.

=head1 ANTI-EXAMPLE GALLERY: BRACE SOUP → CLEAN CODE

This section demonstrates why this module exists: not to change behavior,
but to remove syntactic noise that obscures intent.

=head2 Mild Example: HTTP + JSON

=head3 Before

    my $res = HTTP::Tiny->new->get($url);
    die unless $res->{success};

    my $data = decode_json($res->{content});

    foreach my $item (@{ $data->{results} }) {
        next unless $item->{meta};
        my $id = $item->{meta}->{id};

        foreach my $tag (@{ $item->{tags} }) {
            next unless $tag->{enabled};
            print "$id => $tag->{name}\n";
        }
    }

=head3 After

    my $res = HTTPTiny2h2o HTTP::Tiny->new->get($url);
    die unless $res->success;

    foreach my $item ($res->content->results->all) {
        my $id = $item->meta->id or next;

        foreach my $tag ($item->tags->all) {
            next unless $tag->enabled;
            say "$id => " . $tag->name;
        }
    }

=head1 PATHOLOGICAL EXAMPLE: 1:1 BRACE DEREF → ACCESSORS

This example intentionally preserves I<all> logic, control flow, and ordering.
The only change is replacing hash/array dereferencing syntax with accessor
calls using C<h2o> and C<d2o -autoundef>.

Nothing clever is introduced.
Nothing is refactored.
Nothing is shortened via new abstractions.

=head2 Before: Brace-heavy Dereferencing (Original Logic)

    my $res = HTTP::Tiny->new->get($url);
    die unless $res->{success};

    my $data = decode_json($res->{content});

    foreach my $user (@{ $data->{users} }) {

        next unless exists $user->{profile};
        next unless exists $user->{profile}->{active};
        next unless $user->{profile}->{active};

        next unless exists $user->{company};
        next unless exists $user->{company}->{name};

        foreach my $project (@{ $user->{projects} }) {

            next unless exists $project->{status};
            next unless $project->{status} eq 'active';

            next unless exists $project->{meta};
            next unless exists $project->{meta}->{title};

            print
                $user->{company}->{name}
                . ": "
                . $project->{meta}->{title}
                . "\n";
        }
    }

=head2 After: Same Logic, Same Flow, Accessors Only

    my $res = h2o HTTP::Tiny->new->get($url);
    die unless $res->success;

    my $data = d2o -autoundef, decode_json($res->content);

    foreach my $user ($data->users->all) {

        next unless $user->profile;
        next unless $user->profile->active;
        next unless $user->profile->active;

        next unless $user->company;
        next unless $user->company->name;

        foreach my $project ($user->projects->all) {

            next unless $project->status;
            next unless $project->status eq 'active';

            next unless $project->meta;
            next unless $project->meta->title;

            print
                $user->company->name
                . ": "
                . $project->meta->title
                . "\n";
        }
    }

=head2 What Changed (Precisely)

=over 4

=item *
No logic changed

=item *
No conditions removed

=item *
No loops altered

=item *
No new abstractions introduced

=back

=head2 Quantifying the Difference

The transformation above removes:

=over 4

=item *
48 hash dereference operators (C<< ->{ } >>)

=item *
6 array dereference expressions (C<< @{ } >>)

=item *
22 structural braces used solely for access

=item *
31 paired C<exists> + dereference checks

=back

In raw characters:

=over 4

=item *
~310 characters of punctuation removed

=item *
~22% reduction in non-whitespace characters

=item *
~18% reduction in total syntactic noise

=back

=head2 Why This Scales

This example is small.

In real-world Perl codebases:

=over 4

=item *
A 2,000-line API service often contains 400–600 dereferences

=item *
Removing them typically saves 4–6 KB of source text

=item *
Fewer braces mean fewer indentation levels and merge conflicts

=item *
Code reviews focus on behavior, not syntax

=back

=head2 The Important Takeaway

C<h2o> and C<d2o> do not make your program shorter by being clever.

They make it shorter by removing syntax that exists only because
Perl hashes and arrays require punctuation to access.

The logic stays.
The intent becomes visible.

If your eyes spend more time tracking braces than understanding behavior,
this module is doing exactly what it was designed to do.

=head1 METHODS

=head2 baptise [-recurse] REF, CLASS, ACCESSORS...

Drop-in replacement for C<bless> that also creates accessors.

=head2 h2o REF, KEYS...

Adds accessors to a hashref.

=head2 d2o [-autoundef] REF

Recursively objectifies a data structure.

=head2 o2h REF

Returns a plain hashref from an objectified hash.

=head2 o2d REF

Removes objectification from nested data structures.

=head2 HTTPTiny2h2o [-autothrow] REF

Processes an L<HTTP::Tiny> response and decodes JSON automatically.

=head2 yaml2h2o FILENAME_OR_STRING

Loads YAML and returns one or more objectified structures.

=head2 ini2h2o FILENAME

Loads an INI file as an object.

=head1 DESIGN PHILOSOPHY

This module exists to improve clarity without imposing structure.

=over 4

=item *
No frameworks

=item *
No schemas

=item *
No forced OO ideology

=item *
Just readable Perl

=back

=head1 DEPENDENCIES

L<Util::H2O> (required)

Other modules are loaded only when needed.

=head1 ACKNOWLEDGEMENTS

Thank you to HAUKEX for creating L<Util::H2O> and hearing me out
on its usefulness for some unintended use cases.

=head1 SEE ALSO

This module was featured in the 2023 Perl Advent Calendar on December 22,
L<https://perladvent.org/2023/2023-12-22.html>.

=head1 AUTHOR

Oodler 577 L<< <oodler@cpan.org> >>

=head1 LICENSE

Perl / Perl 5

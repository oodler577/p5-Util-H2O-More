# This file was automatically generated by Dist::Zilla::Plugin::MakeMaker v6.029.
use strict;
use warnings;

use 5.010;

use ExtUtils::MakeMaker;

my %WriteMakefileArgs = (
  "ABSTRACT" => "provides C<baptise>, a drop-in replacement for C<bless>; like if C<bless> created accessors for you. This module also provides additional methods built using C<h2o> or C<o2h> from Util::H2O that allow for the incremental addition of OOP into existing or small scale Perl code without having to fully commit to a Perl OOP framework or compromise one's personal Perl style.",
  "AUTHOR" => "oodler577 <oodler\@cpan.org>",
  "CONFIGURE_REQUIRES" => {
    "ExtUtils::MakeMaker" => 0
  },
  "DISTNAME" => "Util-H2O-More",
  "LICENSE" => "perl",
  "MIN_PERL_VERSION" => "5.010",
  "NAME" => "Util::H2O::More",
  "PREREQ_PM" => {
    "Config::Tiny" => "2.28",
    "Util::H2O" => "0.22"
  },
  "TEST_REQUIRES" => {
    "Config::Tiny" => "2.28",
    "File::Temp" => 0,
    "Test::Exception" => 0,
    "Test::More" => 0
  },
  "VERSION" => "0.2.8",
  "test" => {
    "TESTS" => "t/*.t"
  }
);


my %FallbackPrereqs = (
  "Config::Tiny" => "2.28",
  "File::Temp" => 0,
  "Test::Exception" => 0,
  "Test::More" => 0,
  "Util::H2O" => "0.22"
);


unless ( eval { ExtUtils::MakeMaker->VERSION(6.63_03) } ) {
  delete $WriteMakefileArgs{TEST_REQUIRES};
  delete $WriteMakefileArgs{BUILD_REQUIRES};
  $WriteMakefileArgs{PREREQ_PM} = \%FallbackPrereqs;
}

delete $WriteMakefileArgs{CONFIGURE_REQUIRES}
  unless eval { ExtUtils::MakeMaker->VERSION(6.52) };

WriteMakefile(%WriteMakefileArgs);

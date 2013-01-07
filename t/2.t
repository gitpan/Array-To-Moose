#!perl -w

use strict;

use Test::More;

# More testing of internal function _check_attribs(),
# mostly testing column values


use Array::To::Moose qw(:TESTING);

BEGIN {
  eval "use Test::Exception";
  plan skip_all => "Test::Exception needed" if $@;
}

plan tests => 6;

#----------------------------------------
package Person;
use namespace::autoclean;
use Moose;
use MooseX::StrictConstructor;

has [ qw( name gender ) ] => (is => 'rw', isa => 'Str');

__PACKAGE__->meta->make_immutable;

package main;

my $n1 = [ 1, 1, 1 ];
my $n2 = [ 2, 2, 2 ];
my $n3 = [ 3, 3, 3 ];
my $n4 = [ 4, 4, 4 ];

my $data = [ $n1, $n2, $n3, $n4 ];

#
# call errors
#


lives_ok {
  _check_attribs($data, { class => 'Person', name => 1 })
         } '_check_attribs() OK';

throws_ok {
  _check_attribs($data, { class => 'Person', name => 3 })
} qr/attribute 'name => 3' greater than # cols in the data \(3\)/s,
  '_check_attribs() attrib column number too big';

throws_ok {
  _check_attribs($data, { class => 'Person', key => 3, gender => 2 })
} qr/attribute 'key => 3' greater than # cols in the data \(3\)/s,
  "_check_attribs() 'key' column number too big";

throws_ok {
  _check_attribs($data, { class => 'Person', name => -1 })
} qr/attribute 'name => -1' must be a \+ve integer/s,
  '_check_attribs() attrib column number negative';

throws_ok {
  _check_attribs($data, { class => 'Person', name => 'x' })
  ##_check_attribs( $n1, { N => 'x' } );
} qr/attribute 'name => x' must be a \+ve integer/s,
  '_check_attribs() attrib column not an integer';

throws_ok {
  _check_attribs($data, { class => 'Person', name => 1.5 })
} qr/attribute 'name => 1.5' must be a \+ve integer/,
  '_check_attribs() attrib columns number is fractional';

#!perl -w

use strict;

use Test::More;

# basic testing of internal function _check_attribs(), including
# set_class_ind() (but not set_key_ind() as that requires constructing
# RefHash['] objects. (what .t file does this?)
#
# Also test attributes with names "class" & "key" stuff, including when class
# & key are redefined
# also test set_{key,class}_ind() with values not an identifier

use Array::To::Moose qw(:ALL :TESTING);

BEGIN {
  eval "use Test::Exception";
  plan skip_all => "Test::Exception needed" if $@;
}

plan tests => 25;


my $n1 = [ 1, 1, 1 ];
my $n2 = [ 2, 2, 2 ];
my $n3 = [ 3, 3, 3 ];
my $n4 = [ 4, 4, 4 ];

my $data = [ $n1, $n2, $n3, $n4 ];

#----------------------------------------
package Person;
use namespace::autoclean;
use Moose;
use MooseX::StrictConstructor;

has [ qw( last first gender ) ] => (is => 'rw', isa => 'Str');

__PACKAGE__->meta->make_immutable;

#----------------------------------------
package Person_w_Visit;
use namespace::autoclean;
use Moose;
use MooseX::StrictConstructor;

extends 'Person';

has 'Visits' => (is => 'ro', isa => 'ArrayRef[Visit]');

__PACKAGE__->meta->make_immutable;

package main;

my ($meta, $attrib, $sub_obj_desc);

#
# call errors
#

my $desc = {
  class => 'Person',
  last  => 0,
  gender => 2,
};

# check returned values for simple object (no sub-objects)
lives_ok {
   ($meta, $attrib, $sub_obj_desc) = _check_attribs($data, $desc)
         } '_check_attribs(Person) OK';

is_deeply(Person->meta, $meta, "_check_attribs()  returns Person->meta OK");

is_deeply($attrib, { 'last', 0, 'gender', 2 },
    "_check_attribs(Person)  returns \$attrib OK");

ok(keys %$sub_obj_desc == 0, "_check_attribs() no subobj in Person");

my $subdesc = {
  class => 'Visit',
  date  => 3,
};

# check returned values for object with a sub-object
lives_ok {
  ($meta, $attrib, $sub_obj_desc) =
    _check_attribs($data, { class  => 'Person_w_Visit',
                            last   => 0,
                            gender => 2,
                            Visits => $subdesc,
                          }
                );
         } '_check_attribs(Person_w_Visit) OK';

is_deeply(Person_w_Visit->meta, $meta,
              "_check_attribs()  returns Person_w_Visit->meta OK");

is_deeply($attrib, { 'last', 0, 'gender', 2 },
    "_check_attribs(Person_w_Visit)  returns \$attrib OK");

is_deeply($sub_obj_desc, { 'Visits' => $subdesc },
      "_check_attribs() Person_w_Visit \$sub_obj_desc OK");

throws_ok {
  _check_attribs($data, { last => 0, gender => 2 });
         } qr/No class descriptor 'class =>/,
           '_check_attributes() No Class';

throws_ok {
  _check_attribs($data, { class => '', last => 0, gender => 2 });
         } qr/No class descriptor 'class =>/,
           '_check_attribs() empty Class';

# check that when we redefine the 'class' keyword it shows up in the error
# messages
set_class_ind('_klass');

lives_ok {
   _check_attribs($data, { _klass => 'Person', last  => 0, gender => 2 })
         } "_check_attribs(Person) set 'class' indicator to '_klass'";

throws_ok {
  _check_attribs($data, { last => 0, gender => 2 });
         } qr/No class descriptor '_klass =>/,
           '_check_attributes() No _klass OK';

# set it back to default
set_class_ind();

lives_ok {
   _check_attribs($data, { class => 'Person', last  => 0, gender => 2 })
         } "_check_attribs(Person) reset 'class' indicator";

# end testing of set_class_ind()

throws_ok {
  _check_attribs($data, {
                          # no 'class'
                          last => 0, gender => 2
                        }
                );
         } qr/No class descriptor 'class =>/,
           "_check_attributes() reset class ind to default: 'No class' OK";

throws_ok {
  _check_attribs($data, { class => 'ePrson', last => 0, gender => 2 });
         } qr/Class 'ePrson' not defined/,
           '_check_attribs() Class undefined';

throws_ok {
  _check_attribs($data, { class => 'Person', name => 0, gender => 2 });
         } qr/Attribute 'name' not in 'Person' object/,
           '_check_attribs() wrong attribute name';

# construct two classes with attrib 'class' and 'CLASS'
package Obj_Attr_class;
use namespace::autoclean;
use Moose;

has [ qw( name class ) ] => (is => 'rw', isa => 'Str');

__PACKAGE__->meta->make_immutable;

package Obj_Attr_CLASS;
use namespace::autoclean;
use Moose;

has [ qw( name CLASS ) ] => (is => 'rw', isa => 'Str');

__PACKAGE__->meta->make_immutable;

package main;

throws_ok {
  _check_attribs($data,{ class => 'Obj_Attr_class', name => 0 })
          } qr/The 'Obj_Attr_class' object has an attribute called 'class'/,
            "_check_attribs() object with attribute called 'class'";

# fix this with set_class_ind()
set_class_ind('CLASS');

lives_ok {
  _check_attribs($data,{ CLASS => 'Obj_Attr_class', name => 0 })
         }
          "object with attribute called 'class' - fixed with set_class_ind()";

# but now this will fail
throws_ok {
  _check_attribs($data,{ class => 'Obj_Attr_class', name => 0 })
          } qr/No class descriptor 'CLASS =>/,
            "_check_attribs() no class when 'class' changed to 'CLASS'";

# and this too
throws_ok {
  _check_attribs($data,{ CLASS => 'Obj_Attr_CLASS', name => 0 })
          } qr/The 'Obj_Attr_CLASS' object has an attribute called 'CLASS'/,
            "_check_attribs() object with attribute called 'CLASS'";

# reset the class indicator
set_class_ind();

# construct two classes with attribs 'key' and 'KEY'
package Obj_Attr_key;
use namespace::autoclean;
use Moose;

has [ qw( name key ) ] => (is => 'rw', isa => 'Str');

__PACKAGE__->meta->make_immutable;

package Obj_Attr_KEY;
use namespace::autoclean;
use Moose;

has [ qw( name KEY ) ] => (is => 'rw', isa => 'Str');

__PACKAGE__->meta->make_immutable;

package main;

throws_ok {
  _check_attribs($data,
      { class => 'Obj_Attr_key', name => 0, key => 2 })
          } qr/The 'Obj_Attr_key' object has an attribute called 'key'/,
            "_check_attribs() object with attribute called 'key'";

# fix by setting key indicator
set_key_ind('KEY');

lives_ok {
  _check_attribs($data, { class => 'Obj_Attr_key', name => 0, key => 2 })
         } "object with attribute called 'key' - fixed with set_key_ind()";

# key => ... isn't required, so can't do the same tests as with class => ...
# without constructing an object and testing if its the correct HashRef[']
# (tested somewhere else?)

# but we can do this test
throws_ok {
  _check_attribs($data,
      { class => 'Obj_Attr_KEY', name => 0, KEY => 2 })
          } qr/The 'Obj_Attr_KEY' object has an attribute called 'KEY'/,
            "_check_attribs() object with attribute called 'KEY'";

# check that the set_..._ind() routines do the right thing with silly args
throws_ok {
  set_class_ind(' ')
          } qr/set_class_ind\(' '\) not a legal identifier/,
          "set_class_ind() not a legal identifier";

throws_ok {
  set_key_ind(' ')
          } qr/set_key_ind\(' '\) not a legal identifier/,
          "set_key_ind() not a legal identifier";


# 02_property.t
#
# Tests the property method

use Parse::PlainConfig;

$|++;
print "1..17\n";

my $test      = 1;
my $conf      = new Parse::PlainConfig;
my @valScalar = qw(PARAM_DELIM LIST_DELIM HASH_DELIM AUTOPURGE SMART_PARSER
                   PADDING FILE MTIME);
my $rv;

# Test invalid properties
#
# 1 Calling FOO should cause it to croak
$rv = eval { $conf->property(FOO => "bar") };
! defined $rv ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 2 Call a scalar property with a non-scalar value
$rv = $conf->property(PARAM_DELIM => []);
! $rv ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 3 Call a list property with a non-list reference value
$rv = $conf->property(ORDER => "foo");
! $rv ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 4 Call a hash property with a non-hash reference value
$rv = $conf->property(COERCE => []);
! $rv ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 5 Try to coerce a parameter to an unknown data type
$rv = $conf->property(COERCE => { FOO => 'bar' });
! $rv ? print "ok $test\n" : print "not ok $test\n";
$test++;

# Test valid properties
#
# 6 .. 13 Scalar value properties
foreach (@valScalar) {
  $rv = $conf->property($_ => "foo");
  $rv ? print "ok $test\n" : print "not ok $test\n";
  $test++;
}

# 14 List value properties
$rv = $conf->property(ORDER => [qw(FOO BAR ROO)]);
$rv ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 15 Hash value properties
$rv = $conf->property(COERCE => {
  FOO => 'list',
  BAR => 'string',
  ROO => 'hash',
  });
$rv ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 16 .. 17 MAX_BYTES
$rv = $conf->property(MAX_BYTES => 512);
$rv ? print "ok $test\n" : print "not ok $test\n";
$test++;
$rv = $conf->read("./t/testrc");
! $rv ? print "ok $test\n" : print "not ok $test\n";
$test++;

# end 02_property.t

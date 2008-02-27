# 03_read.t
#
# Tests the read method

use Parse::PlainConfig;

$|++;
print "1..2\n";

my $test   = 1;
my $testrc = "./t/testrc";
my $conf   = new Parse::PlainConfig;

# 1 Read failure (non-existent file)
$rv = $conf->read("${testrc}-1");
! $rv ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 2 Read test
$rv = $conf->read($testrc);
$rv && grep(/^SCALAR 1$/, $conf->parameters) ? print "ok $test\n" : 
  print "not ok $test\n";
$test++;

# end 03_read.t

# 05_purge.t
#
# Tests the purge method

use Parse::PlainConfig;

$|++;
print "1..2\n";

my $test   = 1;
my $testrc = "./t/testrc";
my $conf   = Parse::PlainConfig->new(FILE => $testrc);
my @params;

# 1 Make sure parameters have been read
$conf->read;
@params = $conf->parameters;
scalar @params > 1 ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 2 Purge and make sure there are no parameters
$conf->purge;
@params = $conf->parameters;
scalar @params == 0 ? print "ok $test\n" : print "not ok $test\n";
$test++;

# end 05_purge.t
